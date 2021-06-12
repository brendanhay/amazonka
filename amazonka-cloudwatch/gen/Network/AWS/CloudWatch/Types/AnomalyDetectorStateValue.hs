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
-- Module      : Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
  ( AnomalyDetectorStateValue
      ( ..,
        AnomalyDetectorStateValue_PENDING_TRAINING,
        AnomalyDetectorStateValue_TRAINED,
        AnomalyDetectorStateValue_TRAINED_INSUFFICIENT_DATA
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AnomalyDetectorStateValue = AnomalyDetectorStateValue'
  { fromAnomalyDetectorStateValue ::
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
