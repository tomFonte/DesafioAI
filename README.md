# DesafioAI
Repositorio para los documentos del desafio tecnico.

# Detalle
Para el desafío fue necesario calcular el score crediticio para un set de datos. Para el set se dio una observación importante, y es que cada clasificado como Falso Positivo tenía un valuación 5 veces mayor que un Falso Negativo. Es decir, la precisión para el ajuste debía ser de un orden superior, y por esto ajusté varios modelos buscando uno que redujera la tasa de Falsos Positivos.

## Modelos Estimados
Para la prueba, utilicé modelos supervisados clásicos y otros un poco más sofisticados. Los modelos ajustados fueron los siguientes:

* Regresión Logistica
* Random Forest
* SVM
* Adaboost

No utilicé modelos de redes neuronales debido a restricciones de tiempo y que 2/3 de las variables eran categóricas. Asímismo, tampoco utilicé Redes Neuronales Bayesianas que permitirían dar cuenta de la incertidumbre por motivos de tiempo.

# Resultados
Buscando el mejor modelo, al optimizar las variables a utilizar quedé con el modelo de regresión logística, el cual predice un 85% para la muestra de entrenamiento y un 78.7% para la de validación, con un 17.78% de tasa de Falsos Positivos, la cual era la más baja dentro de los modelos ajustados.

## Siguientes Pasos
Para efectos de mejorar el modelo, se sugiere incrementar la capacidad de cálculo y realizar un modelo multiplicativo. Considerando la gran cantidad de variables categóricas sugiero considerar la conversión a numérica de muchas de ellas y volver a ejecutar, o bien, considerar los valores medios de las categorías asociadas a numérica y evaluar el comportamiento de los modelos contra una red neuronal. 
