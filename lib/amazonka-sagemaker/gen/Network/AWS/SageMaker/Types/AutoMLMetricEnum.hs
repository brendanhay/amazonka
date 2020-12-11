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
        Accuracy,
        Auc,
        F1,
        F1macro,
        Mse
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutoMLMetricEnum = AutoMLMetricEnum' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Accuracy :: AutoMLMetricEnum
pattern Accuracy = AutoMLMetricEnum' "Accuracy"

pattern Auc :: AutoMLMetricEnum
pattern Auc = AutoMLMetricEnum' "AUC"

pattern F1 :: AutoMLMetricEnum
pattern F1 = AutoMLMetricEnum' "F1"

pattern F1macro :: AutoMLMetricEnum
pattern F1macro = AutoMLMetricEnum' "F1macro"

pattern Mse :: AutoMLMetricEnum
pattern Mse = AutoMLMetricEnum' "MSE"

{-# COMPLETE
  Accuracy,
  Auc,
  F1,
  F1macro,
  Mse,
  AutoMLMetricEnum'
  #-}
