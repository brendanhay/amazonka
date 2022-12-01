{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.AutoMLJobObjective
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobObjective where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLMetricEnum

-- | Specifies a metric to minimize or maximize as the objective of a job.
--
-- /See:/ 'newAutoMLJobObjective' smart constructor.
data AutoMLJobObjective = AutoMLJobObjective'
  { -- | The name of the objective metric used to measure the predictive quality
    -- of a machine learning system. This metric is optimized during training
    -- to provide the best estimate for model parameter values from data.
    --
    -- Here are the options:
    --
    -- -   @MSE@: The mean squared error (MSE) is the average of the squared
    --     differences between the predicted and actual values. It is used for
    --     regression. MSE values are always positive: the better a model is at
    --     predicting the actual values, the smaller the MSE value is. When the
    --     data contains outliers, they tend to dominate the MSE, which might
    --     cause subpar prediction performance.
    --
    -- -   @Accuracy@: The ratio of the number of correctly classified items to
    --     the total number of (correctly and incorrectly) classified items. It
    --     is used for binary and multiclass classification. It measures how
    --     close the predicted class values are to the actual values. Accuracy
    --     values vary between zero and one: one indicates perfect accuracy and
    --     zero indicates perfect inaccuracy.
    --
    -- -   @F1@: The F1 score is the harmonic mean of the precision and recall.
    --     It is used for binary classification into classes traditionally
    --     referred to as positive and negative. Predictions are said to be
    --     true when they match their actual (correct) class and false when
    --     they do not. Precision is the ratio of the true positive predictions
    --     to all positive predictions (including the false positives) in a
    --     data set and measures the quality of the prediction when it predicts
    --     the positive class. Recall (or sensitivity) is the ratio of the true
    --     positive predictions to all actual positive instances and measures
    --     how completely a model predicts the actual class members in a data
    --     set. The standard F1 score weighs precision and recall equally. But
    --     which metric is paramount typically depends on specific aspects of a
    --     problem. F1 scores vary between zero and one: one indicates the best
    --     possible performance and zero the worst.
    --
    -- -   @AUC@: The area under the curve (AUC) metric is used to compare and
    --     evaluate binary classification by algorithms such as logistic
    --     regression that return probabilities. A threshold is needed to map
    --     the probabilities into classifications. The relevant curve is the
    --     receiver operating characteristic curve that plots the true positive
    --     rate (TPR) of predictions (or recall) against the false positive
    --     rate (FPR) as a function of the threshold value, above which a
    --     prediction is considered positive. Increasing the threshold results
    --     in fewer false positives but more false negatives. AUC is the area
    --     under this receiver operating characteristic curve and so provides
    --     an aggregated measure of the model performance across all possible
    --     classification thresholds. The AUC score can also be interpreted as
    --     the probability that a randomly selected positive data point is more
    --     likely to be predicted positive than a randomly selected negative
    --     example. AUC scores vary between zero and one: a score of one
    --     indicates perfect accuracy and a score of one half indicates that
    --     the prediction is not better than a random classifier. Values under
    --     one half predict less accurately than a random predictor. But such
    --     consistently bad predictors can simply be inverted to obtain better
    --     than random predictors.
    --
    -- -   @F1macro@: The F1macro score applies F1 scoring to multiclass
    --     classification. In this context, you have multiple classes to
    --     predict. You just calculate the precision and recall for each class
    --     as you did for the positive class in binary classification. Then,
    --     use these values to calculate the F1 score for each class and
    --     average them to obtain the F1macro score. F1macro scores vary
    --     between zero and one: one indicates the best possible performance
    --     and zero the worst.
    --
    -- If you do not specify a metric explicitly, the default behavior is to
    -- automatically use:
    --
    -- -   @MSE@: for regression.
    --
    -- -   @F1@: for binary classification
    --
    -- -   @Accuracy@: for multiclass classification.
    metricName :: AutoMLMetricEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLJobObjective' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'autoMLJobObjective_metricName' - The name of the objective metric used to measure the predictive quality
-- of a machine learning system. This metric is optimized during training
-- to provide the best estimate for model parameter values from data.
--
-- Here are the options:
--
-- -   @MSE@: The mean squared error (MSE) is the average of the squared
--     differences between the predicted and actual values. It is used for
--     regression. MSE values are always positive: the better a model is at
--     predicting the actual values, the smaller the MSE value is. When the
--     data contains outliers, they tend to dominate the MSE, which might
--     cause subpar prediction performance.
--
-- -   @Accuracy@: The ratio of the number of correctly classified items to
--     the total number of (correctly and incorrectly) classified items. It
--     is used for binary and multiclass classification. It measures how
--     close the predicted class values are to the actual values. Accuracy
--     values vary between zero and one: one indicates perfect accuracy and
--     zero indicates perfect inaccuracy.
--
-- -   @F1@: The F1 score is the harmonic mean of the precision and recall.
--     It is used for binary classification into classes traditionally
--     referred to as positive and negative. Predictions are said to be
--     true when they match their actual (correct) class and false when
--     they do not. Precision is the ratio of the true positive predictions
--     to all positive predictions (including the false positives) in a
--     data set and measures the quality of the prediction when it predicts
--     the positive class. Recall (or sensitivity) is the ratio of the true
--     positive predictions to all actual positive instances and measures
--     how completely a model predicts the actual class members in a data
--     set. The standard F1 score weighs precision and recall equally. But
--     which metric is paramount typically depends on specific aspects of a
--     problem. F1 scores vary between zero and one: one indicates the best
--     possible performance and zero the worst.
--
-- -   @AUC@: The area under the curve (AUC) metric is used to compare and
--     evaluate binary classification by algorithms such as logistic
--     regression that return probabilities. A threshold is needed to map
--     the probabilities into classifications. The relevant curve is the
--     receiver operating characteristic curve that plots the true positive
--     rate (TPR) of predictions (or recall) against the false positive
--     rate (FPR) as a function of the threshold value, above which a
--     prediction is considered positive. Increasing the threshold results
--     in fewer false positives but more false negatives. AUC is the area
--     under this receiver operating characteristic curve and so provides
--     an aggregated measure of the model performance across all possible
--     classification thresholds. The AUC score can also be interpreted as
--     the probability that a randomly selected positive data point is more
--     likely to be predicted positive than a randomly selected negative
--     example. AUC scores vary between zero and one: a score of one
--     indicates perfect accuracy and a score of one half indicates that
--     the prediction is not better than a random classifier. Values under
--     one half predict less accurately than a random predictor. But such
--     consistently bad predictors can simply be inverted to obtain better
--     than random predictors.
--
-- -   @F1macro@: The F1macro score applies F1 scoring to multiclass
--     classification. In this context, you have multiple classes to
--     predict. You just calculate the precision and recall for each class
--     as you did for the positive class in binary classification. Then,
--     use these values to calculate the F1 score for each class and
--     average them to obtain the F1macro score. F1macro scores vary
--     between zero and one: one indicates the best possible performance
--     and zero the worst.
--
-- If you do not specify a metric explicitly, the default behavior is to
-- automatically use:
--
-- -   @MSE@: for regression.
--
-- -   @F1@: for binary classification
--
-- -   @Accuracy@: for multiclass classification.
newAutoMLJobObjective ::
  -- | 'metricName'
  AutoMLMetricEnum ->
  AutoMLJobObjective
newAutoMLJobObjective pMetricName_ =
  AutoMLJobObjective' {metricName = pMetricName_}

-- | The name of the objective metric used to measure the predictive quality
-- of a machine learning system. This metric is optimized during training
-- to provide the best estimate for model parameter values from data.
--
-- Here are the options:
--
-- -   @MSE@: The mean squared error (MSE) is the average of the squared
--     differences between the predicted and actual values. It is used for
--     regression. MSE values are always positive: the better a model is at
--     predicting the actual values, the smaller the MSE value is. When the
--     data contains outliers, they tend to dominate the MSE, which might
--     cause subpar prediction performance.
--
-- -   @Accuracy@: The ratio of the number of correctly classified items to
--     the total number of (correctly and incorrectly) classified items. It
--     is used for binary and multiclass classification. It measures how
--     close the predicted class values are to the actual values. Accuracy
--     values vary between zero and one: one indicates perfect accuracy and
--     zero indicates perfect inaccuracy.
--
-- -   @F1@: The F1 score is the harmonic mean of the precision and recall.
--     It is used for binary classification into classes traditionally
--     referred to as positive and negative. Predictions are said to be
--     true when they match their actual (correct) class and false when
--     they do not. Precision is the ratio of the true positive predictions
--     to all positive predictions (including the false positives) in a
--     data set and measures the quality of the prediction when it predicts
--     the positive class. Recall (or sensitivity) is the ratio of the true
--     positive predictions to all actual positive instances and measures
--     how completely a model predicts the actual class members in a data
--     set. The standard F1 score weighs precision and recall equally. But
--     which metric is paramount typically depends on specific aspects of a
--     problem. F1 scores vary between zero and one: one indicates the best
--     possible performance and zero the worst.
--
-- -   @AUC@: The area under the curve (AUC) metric is used to compare and
--     evaluate binary classification by algorithms such as logistic
--     regression that return probabilities. A threshold is needed to map
--     the probabilities into classifications. The relevant curve is the
--     receiver operating characteristic curve that plots the true positive
--     rate (TPR) of predictions (or recall) against the false positive
--     rate (FPR) as a function of the threshold value, above which a
--     prediction is considered positive. Increasing the threshold results
--     in fewer false positives but more false negatives. AUC is the area
--     under this receiver operating characteristic curve and so provides
--     an aggregated measure of the model performance across all possible
--     classification thresholds. The AUC score can also be interpreted as
--     the probability that a randomly selected positive data point is more
--     likely to be predicted positive than a randomly selected negative
--     example. AUC scores vary between zero and one: a score of one
--     indicates perfect accuracy and a score of one half indicates that
--     the prediction is not better than a random classifier. Values under
--     one half predict less accurately than a random predictor. But such
--     consistently bad predictors can simply be inverted to obtain better
--     than random predictors.
--
-- -   @F1macro@: The F1macro score applies F1 scoring to multiclass
--     classification. In this context, you have multiple classes to
--     predict. You just calculate the precision and recall for each class
--     as you did for the positive class in binary classification. Then,
--     use these values to calculate the F1 score for each class and
--     average them to obtain the F1macro score. F1macro scores vary
--     between zero and one: one indicates the best possible performance
--     and zero the worst.
--
-- If you do not specify a metric explicitly, the default behavior is to
-- automatically use:
--
-- -   @MSE@: for regression.
--
-- -   @F1@: for binary classification
--
-- -   @Accuracy@: for multiclass classification.
autoMLJobObjective_metricName :: Lens.Lens' AutoMLJobObjective AutoMLMetricEnum
autoMLJobObjective_metricName = Lens.lens (\AutoMLJobObjective' {metricName} -> metricName) (\s@AutoMLJobObjective' {} a -> s {metricName = a} :: AutoMLJobObjective)

instance Core.FromJSON AutoMLJobObjective where
  parseJSON =
    Core.withObject
      "AutoMLJobObjective"
      ( \x ->
          AutoMLJobObjective'
            Prelude.<$> (x Core..: "MetricName")
      )

instance Prelude.Hashable AutoMLJobObjective where
  hashWithSalt _salt AutoMLJobObjective' {..} =
    _salt `Prelude.hashWithSalt` metricName

instance Prelude.NFData AutoMLJobObjective where
  rnf AutoMLJobObjective' {..} = Prelude.rnf metricName

instance Core.ToJSON AutoMLJobObjective where
  toJSON AutoMLJobObjective' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("MetricName" Core..= metricName)]
      )
