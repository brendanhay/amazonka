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

import qualified Network.AWS.Prelude as Prelude

newtype AnomalyDetectorStateValue = AnomalyDetectorStateValue'
  { fromAnomalyDetectorStateValue ::
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
