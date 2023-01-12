{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.AutoMLMetricExtendedEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLMetricExtendedEnum
  ( AutoMLMetricExtendedEnum
      ( ..,
        AutoMLMetricExtendedEnum_AUC,
        AutoMLMetricExtendedEnum_Accuracy,
        AutoMLMetricExtendedEnum_BalancedAccuracy,
        AutoMLMetricExtendedEnum_F1,
        AutoMLMetricExtendedEnum_F1macro,
        AutoMLMetricExtendedEnum_InferenceLatency,
        AutoMLMetricExtendedEnum_LogLoss,
        AutoMLMetricExtendedEnum_MAE,
        AutoMLMetricExtendedEnum_MSE,
        AutoMLMetricExtendedEnum_Precision,
        AutoMLMetricExtendedEnum_PrecisionMacro,
        AutoMLMetricExtendedEnum_R2,
        AutoMLMetricExtendedEnum_RMSE,
        AutoMLMetricExtendedEnum_Recall,
        AutoMLMetricExtendedEnum_RecallMacro
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoMLMetricExtendedEnum = AutoMLMetricExtendedEnum'
  { fromAutoMLMetricExtendedEnum ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern AutoMLMetricExtendedEnum_AUC :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_AUC = AutoMLMetricExtendedEnum' "AUC"

pattern AutoMLMetricExtendedEnum_Accuracy :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_Accuracy = AutoMLMetricExtendedEnum' "Accuracy"

pattern AutoMLMetricExtendedEnum_BalancedAccuracy :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_BalancedAccuracy = AutoMLMetricExtendedEnum' "BalancedAccuracy"

pattern AutoMLMetricExtendedEnum_F1 :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_F1 = AutoMLMetricExtendedEnum' "F1"

pattern AutoMLMetricExtendedEnum_F1macro :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_F1macro = AutoMLMetricExtendedEnum' "F1macro"

pattern AutoMLMetricExtendedEnum_InferenceLatency :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_InferenceLatency = AutoMLMetricExtendedEnum' "InferenceLatency"

pattern AutoMLMetricExtendedEnum_LogLoss :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_LogLoss = AutoMLMetricExtendedEnum' "LogLoss"

pattern AutoMLMetricExtendedEnum_MAE :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_MAE = AutoMLMetricExtendedEnum' "MAE"

pattern AutoMLMetricExtendedEnum_MSE :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_MSE = AutoMLMetricExtendedEnum' "MSE"

pattern AutoMLMetricExtendedEnum_Precision :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_Precision = AutoMLMetricExtendedEnum' "Precision"

pattern AutoMLMetricExtendedEnum_PrecisionMacro :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_PrecisionMacro = AutoMLMetricExtendedEnum' "PrecisionMacro"

pattern AutoMLMetricExtendedEnum_R2 :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_R2 = AutoMLMetricExtendedEnum' "R2"

pattern AutoMLMetricExtendedEnum_RMSE :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_RMSE = AutoMLMetricExtendedEnum' "RMSE"

pattern AutoMLMetricExtendedEnum_Recall :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_Recall = AutoMLMetricExtendedEnum' "Recall"

pattern AutoMLMetricExtendedEnum_RecallMacro :: AutoMLMetricExtendedEnum
pattern AutoMLMetricExtendedEnum_RecallMacro = AutoMLMetricExtendedEnum' "RecallMacro"

{-# COMPLETE
  AutoMLMetricExtendedEnum_AUC,
  AutoMLMetricExtendedEnum_Accuracy,
  AutoMLMetricExtendedEnum_BalancedAccuracy,
  AutoMLMetricExtendedEnum_F1,
  AutoMLMetricExtendedEnum_F1macro,
  AutoMLMetricExtendedEnum_InferenceLatency,
  AutoMLMetricExtendedEnum_LogLoss,
  AutoMLMetricExtendedEnum_MAE,
  AutoMLMetricExtendedEnum_MSE,
  AutoMLMetricExtendedEnum_Precision,
  AutoMLMetricExtendedEnum_PrecisionMacro,
  AutoMLMetricExtendedEnum_R2,
  AutoMLMetricExtendedEnum_RMSE,
  AutoMLMetricExtendedEnum_Recall,
  AutoMLMetricExtendedEnum_RecallMacro,
  AutoMLMetricExtendedEnum'
  #-}
