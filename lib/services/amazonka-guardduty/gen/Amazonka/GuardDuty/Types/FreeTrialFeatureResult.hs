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
-- Module      : Amazonka.GuardDuty.Types.FreeTrialFeatureResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.FreeTrialFeatureResult
  ( FreeTrialFeatureResult
      ( ..,
        FreeTrialFeatureResult_CLOUD_TRAIL,
        FreeTrialFeatureResult_DNS_LOGS,
        FreeTrialFeatureResult_EBS_MALWARE_PROTECTION,
        FreeTrialFeatureResult_EKS_AUDIT_LOGS,
        FreeTrialFeatureResult_EKS_RUNTIME_MONITORING,
        FreeTrialFeatureResult_FLOW_LOGS,
        FreeTrialFeatureResult_LAMBDA_NETWORK_LOGS,
        FreeTrialFeatureResult_RDS_LOGIN_EVENTS,
        FreeTrialFeatureResult_S3_DATA_EVENTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FreeTrialFeatureResult = FreeTrialFeatureResult'
  { fromFreeTrialFeatureResult ::
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

pattern FreeTrialFeatureResult_CLOUD_TRAIL :: FreeTrialFeatureResult
pattern FreeTrialFeatureResult_CLOUD_TRAIL = FreeTrialFeatureResult' "CLOUD_TRAIL"

pattern FreeTrialFeatureResult_DNS_LOGS :: FreeTrialFeatureResult
pattern FreeTrialFeatureResult_DNS_LOGS = FreeTrialFeatureResult' "DNS_LOGS"

pattern FreeTrialFeatureResult_EBS_MALWARE_PROTECTION :: FreeTrialFeatureResult
pattern FreeTrialFeatureResult_EBS_MALWARE_PROTECTION = FreeTrialFeatureResult' "EBS_MALWARE_PROTECTION"

pattern FreeTrialFeatureResult_EKS_AUDIT_LOGS :: FreeTrialFeatureResult
pattern FreeTrialFeatureResult_EKS_AUDIT_LOGS = FreeTrialFeatureResult' "EKS_AUDIT_LOGS"

pattern FreeTrialFeatureResult_EKS_RUNTIME_MONITORING :: FreeTrialFeatureResult
pattern FreeTrialFeatureResult_EKS_RUNTIME_MONITORING = FreeTrialFeatureResult' "EKS_RUNTIME_MONITORING"

pattern FreeTrialFeatureResult_FLOW_LOGS :: FreeTrialFeatureResult
pattern FreeTrialFeatureResult_FLOW_LOGS = FreeTrialFeatureResult' "FLOW_LOGS"

pattern FreeTrialFeatureResult_LAMBDA_NETWORK_LOGS :: FreeTrialFeatureResult
pattern FreeTrialFeatureResult_LAMBDA_NETWORK_LOGS = FreeTrialFeatureResult' "LAMBDA_NETWORK_LOGS"

pattern FreeTrialFeatureResult_RDS_LOGIN_EVENTS :: FreeTrialFeatureResult
pattern FreeTrialFeatureResult_RDS_LOGIN_EVENTS = FreeTrialFeatureResult' "RDS_LOGIN_EVENTS"

pattern FreeTrialFeatureResult_S3_DATA_EVENTS :: FreeTrialFeatureResult
pattern FreeTrialFeatureResult_S3_DATA_EVENTS = FreeTrialFeatureResult' "S3_DATA_EVENTS"

{-# COMPLETE
  FreeTrialFeatureResult_CLOUD_TRAIL,
  FreeTrialFeatureResult_DNS_LOGS,
  FreeTrialFeatureResult_EBS_MALWARE_PROTECTION,
  FreeTrialFeatureResult_EKS_AUDIT_LOGS,
  FreeTrialFeatureResult_EKS_RUNTIME_MONITORING,
  FreeTrialFeatureResult_FLOW_LOGS,
  FreeTrialFeatureResult_LAMBDA_NETWORK_LOGS,
  FreeTrialFeatureResult_RDS_LOGIN_EVENTS,
  FreeTrialFeatureResult_S3_DATA_EVENTS,
  FreeTrialFeatureResult'
  #-}
