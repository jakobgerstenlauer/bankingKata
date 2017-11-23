package bank
import scala.collection.mutable.ListBuffer

case class Date(val day: Int, val month: Int, val year: Int, val hour: Int, val minute: Int, val second: Int){
        assert(day>=0 && day<=31, "Invalid day!")
        assert(month>=0 && month<=12, "Invalid month!")
        assert(year>=2017, "Invalid year!")
        assert(hour>=0 && hour<=24, "Invalid hour!")
        assert(minute>=0 && minute<=59, "Invalid minute!")
        assert(second>=0 && second<=59, "Invalid second!")

        def print() : String = {
                day.toString + "." + month.toString + "." + year.toString
        }
}

class Amount(val euro: Int, val cent: Int){
        def this(amount: Double){
                this( amount.toInt, (100*(amount-amount.toInt)).toInt)
        }

        def print() : String = {
                euro.toString + "." + cent.toString + 0x20AC.toChar
        }

        //Multiplies the integer amount with -1.
        //This is NOT a multiplication of the amount with -1!
        def invert() : Amount = {
                new Amount(-euro, cent)
        }

        def toDouble() : Double = euro.toDouble + 0.01 * cent.toDouble
}

case class Transaction(val date: Date, val amount: Amount, val newBalance: Amount){
        def print() : String ={
                date.print() + "\t" + amount.print() + "\t" + newBalance.print() + "\n"
        }
}

object Transaction{
        def printHeader() : String={
                "Date\t\tAmount("+ 0x20AC.toChar +")\tBalance\n"
        }
}

class Account{
	/**
	* ID should be unique for each new account.
	*/
	val id: Int = Account.getCounter()

        private var balance : Double = 0.0
        private var transactions = ListBuffer[Transaction]()

        def deposit(date: Date, amount: Amount) : Unit = {
                balance = balance + amount.toDouble
                transactions += Transaction(date, amount, new Amount(balance))
        }

  	/**
    	*
    	* @param quantity amount of money to withdrawal
    	* @return money withdrawed. 0 if none
    	*/
        def withdraw(date: Date, amount: Amount) : Unit = {
                if(balance - amount.toDouble > 0){
                    balance = balance - amount.toDouble
                    transactions += Transaction(date, amount.invert(), new Amount(balance))
                }else{
                    println("Withdrawal of "+amount.print()+" was rejected!")
                }
        }
	
	/**
	* Prints all the history starting with the recent one.
	* An example statement would be:
	* Date        Amount  Balance
	* 23.8.2016    -100      400
	* 24.12.2015   +500      500
	*/
        def printStatement() : Unit = {
                println(Transaction.printHeader())
                println("-------------------------------------")
                val ledger = transactions.toList
                for(i <- Range(0,ledger.size)){
                      println(ledger.apply(i).print())
                }
        }
}

object Account {
	private var counter : Int = 0
	def getCounter() : Int = {
		counter = counter + 1
		return counter - 1
        }
} 

object UseBankAccount extends App {
	val account = new Account()
	account.deposit(Date(day=1, month=2, year=2017, hour=11, minute=12, second=13), new Amount(euro=300,cent=50))
	account.withdraw(Date(day=1, month=2, year=2017, hour=12, minute=13, second=14), new Amount(euro=200,cent=10))
	account.withdraw(Date(day=1, month=2, year=2017, hour=12, minute=13, second=14), new Amount(euro=100,cent=1))
	account.withdraw(Date(day=1, month=2, year=2017, hour=12, minute=13, second=14), new Amount(euro=400,cent=70))
	account.printStatement()
}
