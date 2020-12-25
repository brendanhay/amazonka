{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLMetricEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLMetricEnum
  ( AutoMLMetricEnum
      ( AutoMLMetricEnum',
        AutoMLMetricEnumAccuracy,
        AutoMLMetricEnumMse,
        AutoMLMetricEnumF1,
        AutoMLMetricEnumF1macro,
        AutoMLMetricEnumAuc,
        fromAutoMLMetricEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AutoMLMetricEnum = AutoMLMetricEnum'
  { fromAutoMLMetricEnum ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AutoMLMetricEnumAccuracy :: AutoMLMetricEnum
pattern AutoMLMetricEnumAccuracy = AutoMLMetricEnum' "Accuracy"

pattern AutoMLMetricEnumMse :: AutoMLMetricEnum
pattern AutoMLMetricEnumMse = AutoMLMetricEnum' "MSE"

pattern AutoMLMetricEnumF1 :: AutoMLMetricEnum
pattern AutoMLMetricEnumF1 = AutoMLMetricEnum' "F1"

pattern AutoMLMetricEnumF1macro :: AutoMLMetricEnum
pattern AutoMLMetricEnumF1macro = AutoMLMetricEnum' "F1macro"

pattern AutoMLMetricEnumAuc :: AutoMLMetricEnum
pattern AutoMLMetricEnumAuc = AutoMLMetricEnum' "AUC"

{-# COMPLETE
  AutoMLMetricEnumAccuracy,
  AutoMLMetricEnumMse,
  AutoMLMetricEnumF1,
  AutoMLMetricEnumF1macro,
  AutoMLMetricEnumAuc,
  AutoMLMetricEnum'
  #-}
