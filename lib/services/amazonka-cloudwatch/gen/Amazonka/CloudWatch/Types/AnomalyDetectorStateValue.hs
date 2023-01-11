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
-- Module      : Amazonka.CloudWatch.Types.AnomalyDetectorStateValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.AnomalyDetectorStateValue
  ( AnomalyDetectorStateValue
      ( ..,
        AnomalyDetectorStateValue_PENDING_TRAINING,
        AnomalyDetectorStateValue_TRAINED,
        AnomalyDetectorStateValue_TRAINED_INSUFFICIENT_DATA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AnomalyDetectorStateValue = AnomalyDetectorStateValue'
  { fromAnomalyDetectorStateValue ::
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

pattern AnomalyDetectorStateValue_PENDING_TRAINING :: AnomalyDetectorStateValue
pattern AnomalyDetectorStateValue_PENDING_TRAINING = AnomalyDetectorStateValue' "PENDING_TRAINING"

pattern AnomalyDetectorStateValue_TRAINED :: AnomalyDetectorStateValue
pattern AnomalyDetectorStateValue_TRAINED = AnomalyDetectorStateValue' "TRAINED"

pattern AnomalyDetectorStateValue_TRAINED_INSUFFICIENT_DATA :: AnomalyDetectorStateValue
pattern AnomalyDetectorStateValue_TRAINED_INSUFFICIENT_DATA = AnomalyDetectorStateValue' "TRAINED_INSUFFICIENT_DATA"

{-# COMPLETE
  AnomalyDetectorStateValue_PENDING_TRAINING,
  AnomalyDetectorStateValue_TRAINED,
  AnomalyDetectorStateValue_TRAINED_INSUFFICIENT_DATA,
  AnomalyDetectorStateValue'
  #-}
