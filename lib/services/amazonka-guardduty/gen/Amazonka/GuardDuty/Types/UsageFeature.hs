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
-- Module      : Amazonka.GuardDuty.Types.UsageFeature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.UsageFeature
  ( UsageFeature
      ( ..,
        UsageFeature_CLOUD_TRAIL,
        UsageFeature_DNS_LOGS,
        UsageFeature_EBS_MALWARE_PROTECTION,
        UsageFeature_EKS_AUDIT_LOGS,
        UsageFeature_EKS_RUNTIME_MONITORING,
        UsageFeature_FLOW_LOGS,
        UsageFeature_LAMBDA_NETWORK_LOGS,
        UsageFeature_RDS_LOGIN_EVENTS,
        UsageFeature_S3_DATA_EVENTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UsageFeature = UsageFeature'
  { fromUsageFeature ::
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

pattern UsageFeature_CLOUD_TRAIL :: UsageFeature
pattern UsageFeature_CLOUD_TRAIL = UsageFeature' "CLOUD_TRAIL"

pattern UsageFeature_DNS_LOGS :: UsageFeature
pattern UsageFeature_DNS_LOGS = UsageFeature' "DNS_LOGS"

pattern UsageFeature_EBS_MALWARE_PROTECTION :: UsageFeature
pattern UsageFeature_EBS_MALWARE_PROTECTION = UsageFeature' "EBS_MALWARE_PROTECTION"

pattern UsageFeature_EKS_AUDIT_LOGS :: UsageFeature
pattern UsageFeature_EKS_AUDIT_LOGS = UsageFeature' "EKS_AUDIT_LOGS"

pattern UsageFeature_EKS_RUNTIME_MONITORING :: UsageFeature
pattern UsageFeature_EKS_RUNTIME_MONITORING = UsageFeature' "EKS_RUNTIME_MONITORING"

pattern UsageFeature_FLOW_LOGS :: UsageFeature
pattern UsageFeature_FLOW_LOGS = UsageFeature' "FLOW_LOGS"

pattern UsageFeature_LAMBDA_NETWORK_LOGS :: UsageFeature
pattern UsageFeature_LAMBDA_NETWORK_LOGS = UsageFeature' "LAMBDA_NETWORK_LOGS"

pattern UsageFeature_RDS_LOGIN_EVENTS :: UsageFeature
pattern UsageFeature_RDS_LOGIN_EVENTS = UsageFeature' "RDS_LOGIN_EVENTS"

pattern UsageFeature_S3_DATA_EVENTS :: UsageFeature
pattern UsageFeature_S3_DATA_EVENTS = UsageFeature' "S3_DATA_EVENTS"

{-# COMPLETE
  UsageFeature_CLOUD_TRAIL,
  UsageFeature_DNS_LOGS,
  UsageFeature_EBS_MALWARE_PROTECTION,
  UsageFeature_EKS_AUDIT_LOGS,
  UsageFeature_EKS_RUNTIME_MONITORING,
  UsageFeature_FLOW_LOGS,
  UsageFeature_LAMBDA_NETWORK_LOGS,
  UsageFeature_RDS_LOGIN_EVENTS,
  UsageFeature_S3_DATA_EVENTS,
  UsageFeature'
  #-}
