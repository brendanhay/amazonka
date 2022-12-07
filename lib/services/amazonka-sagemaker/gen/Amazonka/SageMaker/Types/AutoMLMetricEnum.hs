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
-- Module      : Amazonka.SageMaker.Types.AutoMLMetricEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLMetricEnum
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoMLMetricEnum = AutoMLMetricEnum'
  { fromAutoMLMetricEnum ::
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
