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
-- Module      : Amazonka.LookoutMetrics.Types.AnomalyDetectorFailureType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyDetectorFailureType
  ( AnomalyDetectorFailureType
      ( ..,
        AnomalyDetectorFailureType_ACTIVATION_FAILURE,
        AnomalyDetectorFailureType_BACK_TEST_ACTIVATION_FAILURE,
        AnomalyDetectorFailureType_DEACTIVATION_FAILURE,
        AnomalyDetectorFailureType_DELETION_FAILURE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AnomalyDetectorFailureType = AnomalyDetectorFailureType'
  { fromAnomalyDetectorFailureType ::
      Core.Text
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

pattern AnomalyDetectorFailureType_ACTIVATION_FAILURE :: AnomalyDetectorFailureType
pattern AnomalyDetectorFailureType_ACTIVATION_FAILURE = AnomalyDetectorFailureType' "ACTIVATION_FAILURE"

pattern AnomalyDetectorFailureType_BACK_TEST_ACTIVATION_FAILURE :: AnomalyDetectorFailureType
pattern AnomalyDetectorFailureType_BACK_TEST_ACTIVATION_FAILURE = AnomalyDetectorFailureType' "BACK_TEST_ACTIVATION_FAILURE"

pattern AnomalyDetectorFailureType_DEACTIVATION_FAILURE :: AnomalyDetectorFailureType
pattern AnomalyDetectorFailureType_DEACTIVATION_FAILURE = AnomalyDetectorFailureType' "DEACTIVATION_FAILURE"

pattern AnomalyDetectorFailureType_DELETION_FAILURE :: AnomalyDetectorFailureType
pattern AnomalyDetectorFailureType_DELETION_FAILURE = AnomalyDetectorFailureType' "DELETION_FAILURE"

{-# COMPLETE
  AnomalyDetectorFailureType_ACTIVATION_FAILURE,
  AnomalyDetectorFailureType_BACK_TEST_ACTIVATION_FAILURE,
  AnomalyDetectorFailureType_DEACTIVATION_FAILURE,
  AnomalyDetectorFailureType_DELETION_FAILURE,
  AnomalyDetectorFailureType'
  #-}
