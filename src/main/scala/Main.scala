object Main {
  def main(args: Array[String]): Unit = {

    hello("Hello, Scala!")

    val salaries = salary_sorted(salary_correct(salary_variance(
      month_salary(2200, 3, 200),
      List(100, 150, 200, 80, 120, 75)
    )))

    print_middle(salaries, 100, 150)
    indexing(salaries)
  }

  def hello(text: String): Unit = {

    println("Задача a:")
    // первая программа!
    println("Hello, Scala!")

    // печатаем строку справа налево
    for (char <- text.length - 1 to 0 by -1) {
      print(text(char))
    }
    println()

    // перевод строки в нижний регистр
    val text_low_case = text
      .toLowerCase()
      // удаление символа "!" - он крайний справа
      .dropRight(1)

    // добавим текcт и напечатаем
    println(text_low_case + " and goodbye python!\n")
  }

  def month_salary(year_income: Int, premium_amt: Int, nutrition_comp: Int): Int = {

    val salary = (
      year_income * premium_amt / 100 + year_income + nutrition_comp
      ) / 12
    // оклад минус налог
    val salary_without_tax = math.round(salary * 0.87).toInt
    println(s"Задача b:\nОклад за вычетом налогов: $salary_without_tax\n")
    salary_without_tax
  }

  def salary_variance(salary: Int, other_salaries: List[Int]): (List[Int], Int) = {

    println("Задача c:")

    // Посчитаем средний размер зарплаты с учётом нашего сотрудника
    val all_salaries = other_salaries :+ salary
    val cnt_person = all_salaries.size
    val avg_salary = all_salaries.sum / cnt_person
    println(s"Средний размер зарплаты: $avg_salary")

    // теперь рассчитаем отклонения и запишем в список
    val variance = all_salaries.map(x => math.round(
      x.toFloat / avg_salary * 100 - 100)
    )
    for (x <- 0 until cnt_person) {
      if (x < cnt_person - 1) {
        println(
          s"Сотрудник отдела ${x + 1}: Зарплата - ${all_salaries(x)}, "
            + s"Отклонение от средней зп в % - ${variance(x)}"
        )
      } else {
        println(
          s"НАШ СОТРУДНИК: Зарплата - ${all_salaries(x)}, "
            + s"Отклонение от средней зп в % - ${variance(x)}\n"
        )
      }
    }
    val result = (all_salaries, variance(cnt_person - 1))
    result
  }

  def salary_correct(tuple: (List[Int], Int)): List[Int] = {

    println("Задача d:")
    val (all_salaries, variance) = tuple
    val idx_employee = all_salaries.size - 1
    val result = all_salaries.updated(
      idx_employee,
      math.round(all_salaries(idx_employee).toFloat / (100 + variance) * 100)
    )
    println(s"Старый массив зарплат:      $all_salaries")
    println(s"Обновлённый массив зарплат: $result")

    val sorted_result = result.sorted
    println(s"Минимальная зарплата в отделе: ${sorted_result.head}")
    println(s"Максимальная зарплата в отделе: ${sorted_result.last}\n")
    sorted_result
  }

  def salary_sorted(salaries: List[Int]): List[Int] = {

    println("Задача e:")
    val result_e = (salaries :+ 350 :+ 90).sorted
    println(s"Добавили 2-х сотрудников и отсортировали список: $result_e\n")

    println("Задача f")
    val text = "Добавили ещё одного сотрудника и его оклад с сохранением сортировки:"
    for (i <- result_e.indices) {
      if (130 <= result_e(i)) {
        val result_f = (
          result_e.slice(0, i) ++ List(130)
            ++ result_e.slice(i, result_e.size + 1)
          )
        println(s"$text $result_f\n")
        return result_f
      }
    }
    val result_f = result_e :+ 130
    println(s"$text $result_f\n")
    result_f
  }

  def print_middle(salaries: List[Int], min_of_range: Int, max_of_range: Int): Unit = {

    println("Задача g:")

    print("Номер(-а) сотрудника(-ов) с зарплатой уровня middle: ")
    val result = salaries.filter(num => num >= min_of_range && num <= max_of_range)
    println(s"${result.mkString(", ")}\n")
  }

  def indexing(salaries: List[Int]): Unit = {

    println("Задача h:")
    println(s"Зарплаты до индексации:    ${salaries.mkString(", ")}")
    println("Зарплаты после индексации: " +
      s"${salaries.map(num => math.round(num * 1.07)).mkString(", ")}")
  }
}