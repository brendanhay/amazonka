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
-- Module      : Amazonka.GuardDuty.Types.DetectorFeatureResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DetectorFeatureResult
  ( DetectorFeatureResult
      ( ..,
        DetectorFeatureResult_CLOUD_TRAIL,
        DetectorFeatureResult_DNS_LOGS,
        DetectorFeatureResult_EBS_MALWARE_PROTECTION,
        DetectorFeatureResult_EKS_AUDIT_LOGS,
        DetectorFeatureResult_EKS_RUNTIME_MONITORING,
        DetectorFeatureResult_FLOW_LOGS,
        DetectorFeatureResult_LAMBDA_NETWORK_LOGS,
        DetectorFeatureResult_RDS_LOGIN_EVENTS,
        DetectorFeatureResult_S3_DATA_EVENTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DetectorFeatureResult = DetectorFeatureResult'
  { fromDetectorFeatureResult ::
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

pattern DetectorFeatureResult_CLOUD_TRAIL :: DetectorFeatureResult
pattern DetectorFeatureResult_CLOUD_TRAIL = DetectorFeatureResult' "CLOUD_TRAIL"

pattern DetectorFeatureResult_DNS_LOGS :: DetectorFeatureResult
pattern DetectorFeatureResult_DNS_LOGS = DetectorFeatureResult' "DNS_LOGS"

pattern DetectorFeatureResult_EBS_MALWARE_PROTECTION :: DetectorFeatureResult
pattern DetectorFeatureResult_EBS_MALWARE_PROTECTION = DetectorFeatureResult' "EBS_MALWARE_PROTECTION"

pattern DetectorFeatureResult_EKS_AUDIT_LOGS :: DetectorFeatureResult
pattern DetectorFeatureResult_EKS_AUDIT_LOGS = DetectorFeatureResult' "EKS_AUDIT_LOGS"

pattern DetectorFeatureResult_EKS_RUNTIME_MONITORING :: DetectorFeatureResult
pattern DetectorFeatureResult_EKS_RUNTIME_MONITORING = DetectorFeatureResult' "EKS_RUNTIME_MONITORING"

pattern DetectorFeatureResult_FLOW_LOGS :: DetectorFeatureResult
pattern DetectorFeatureResult_FLOW_LOGS = DetectorFeatureResult' "FLOW_LOGS"

pattern DetectorFeatureResult_LAMBDA_NETWORK_LOGS :: DetectorFeatureResult
pattern DetectorFeatureResult_LAMBDA_NETWORK_LOGS = DetectorFeatureResult' "LAMBDA_NETWORK_LOGS"

pattern DetectorFeatureResult_RDS_LOGIN_EVENTS :: DetectorFeatureResult
pattern DetectorFeatureResult_RDS_LOGIN_EVENTS = DetectorFeatureResult' "RDS_LOGIN_EVENTS"

pattern DetectorFeatureResult_S3_DATA_EVENTS :: DetectorFeatureResult
pattern DetectorFeatureResult_S3_DATA_EVENTS = DetectorFeatureResult' "S3_DATA_EVENTS"

{-# COMPLETE
  DetectorFeatureResult_CLOUD_TRAIL,
  DetectorFeatureResult_DNS_LOGS,
  DetectorFeatureResult_EBS_MALWARE_PROTECTION,
  DetectorFeatureResult_EKS_AUDIT_LOGS,
  DetectorFeatureResult_EKS_RUNTIME_MONITORING,
  DetectorFeatureResult_FLOW_LOGS,
  DetectorFeatureResult_LAMBDA_NETWORK_LOGS,
  DetectorFeatureResult_RDS_LOGIN_EVENTS,
  DetectorFeatureResult_S3_DATA_EVENTS,
  DetectorFeatureResult'
  #-}
