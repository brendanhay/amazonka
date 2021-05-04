{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype AutoMLMetricEnum = AutoMLMetricEnum'
  { fromAutoMLMetricEnum ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
