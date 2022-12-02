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
-- Module      : Amazonka.LookoutMetrics.Types.AnomalyDetectorStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyDetectorStatus
  ( AnomalyDetectorStatus
      ( ..,
        AnomalyDetectorStatus_ACTIVATING,
        AnomalyDetectorStatus_ACTIVE,
        AnomalyDetectorStatus_BACK_TEST_ACTIVATING,
        AnomalyDetectorStatus_BACK_TEST_ACTIVE,
        AnomalyDetectorStatus_BACK_TEST_COMPLETE,
        AnomalyDetectorStatus_DEACTIVATED,
        AnomalyDetectorStatus_DEACTIVATING,
        AnomalyDetectorStatus_DELETING,
        AnomalyDetectorStatus_FAILED,
        AnomalyDetectorStatus_INACTIVE,
        AnomalyDetectorStatus_LEARNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AnomalyDetectorStatus = AnomalyDetectorStatus'
  { fromAnomalyDetectorStatus ::
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

pattern AnomalyDetectorStatus_ACTIVATING :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_ACTIVATING = AnomalyDetectorStatus' "ACTIVATING"

pattern AnomalyDetectorStatus_ACTIVE :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_ACTIVE = AnomalyDetectorStatus' "ACTIVE"

pattern AnomalyDetectorStatus_BACK_TEST_ACTIVATING :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_BACK_TEST_ACTIVATING = AnomalyDetectorStatus' "BACK_TEST_ACTIVATING"

pattern AnomalyDetectorStatus_BACK_TEST_ACTIVE :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_BACK_TEST_ACTIVE = AnomalyDetectorStatus' "BACK_TEST_ACTIVE"

pattern AnomalyDetectorStatus_BACK_TEST_COMPLETE :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_BACK_TEST_COMPLETE = AnomalyDetectorStatus' "BACK_TEST_COMPLETE"

pattern AnomalyDetectorStatus_DEACTIVATED :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_DEACTIVATED = AnomalyDetectorStatus' "DEACTIVATED"

pattern AnomalyDetectorStatus_DEACTIVATING :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_DEACTIVATING = AnomalyDetectorStatus' "DEACTIVATING"

pattern AnomalyDetectorStatus_DELETING :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_DELETING = AnomalyDetectorStatus' "DELETING"

pattern AnomalyDetectorStatus_FAILED :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_FAILED = AnomalyDetectorStatus' "FAILED"

pattern AnomalyDetectorStatus_INACTIVE :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_INACTIVE = AnomalyDetectorStatus' "INACTIVE"

pattern AnomalyDetectorStatus_LEARNING :: AnomalyDetectorStatus
pattern AnomalyDetectorStatus_LEARNING = AnomalyDetectorStatus' "LEARNING"

{-# COMPLETE
  AnomalyDetectorStatus_ACTIVATING,
  AnomalyDetectorStatus_ACTIVE,
  AnomalyDetectorStatus_BACK_TEST_ACTIVATING,
  AnomalyDetectorStatus_BACK_TEST_ACTIVE,
  AnomalyDetectorStatus_BACK_TEST_COMPLETE,
  AnomalyDetectorStatus_DEACTIVATED,
  AnomalyDetectorStatus_DEACTIVATING,
  AnomalyDetectorStatus_DELETING,
  AnomalyDetectorStatus_FAILED,
  AnomalyDetectorStatus_INACTIVE,
  AnomalyDetectorStatus_LEARNING,
  AnomalyDetectorStatus'
  #-}
