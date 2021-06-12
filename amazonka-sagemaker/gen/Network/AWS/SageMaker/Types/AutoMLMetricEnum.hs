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
-- Module      : Network.AWS.SageMaker.Types.AutoMLMetricEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLMetricEnum
  ( AutoMLMetricEnum
      ( ..,
        AutoMLMetricEnum_AUC,
        AutoMLMetricEnum_Accuracy,
        AutoMLMetricEnum_F1,
        AutoMLMetricEnum_F1macro,
        AutoMLMetricEnum_MSE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AutoMLMetricEnum = AutoMLMetricEnum'
  { fromAutoMLMetricEnum ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AutoMLMetricEnum_AUC :: AutoMLMetricEnum
pattern AutoMLMetricEnum_AUC = AutoMLMetricEnum' "AUC"

pattern AutoMLMetricEnum_Accuracy :: AutoMLMetricEnum
pattern AutoMLMetricEnum_Accuracy = AutoMLMetricEnum' "Accuracy"

pattern AutoMLMetricEnum_F1 :: AutoMLMetricEnum
pattern AutoMLMetricEnum_F1 = AutoMLMetricEnum' "F1"

pattern AutoMLMetricEnum_F1macro :: AutoMLMetricEnum
pattern AutoMLMetricEnum_F1macro = AutoMLMetricEnum' "F1macro"

pattern AutoMLMetricEnum_MSE :: AutoMLMetricEnum
pattern AutoMLMetricEnum_MSE = AutoMLMetricEnum' "MSE"

{-# COMPLETE
  AutoMLMetricEnum_AUC,
  AutoMLMetricEnum_Accuracy,
  AutoMLMetricEnum_F1,
  AutoMLMetricEnum_F1macro,
  AutoMLMetricEnum_MSE,
  AutoMLMetricEnum'
  #-}
