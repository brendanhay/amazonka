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
-- Module      : Amazonka.GuardDuty.Types.OrgFeature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrgFeature
  ( OrgFeature
      ( ..,
        OrgFeature_EBS_MALWARE_PROTECTION,
        OrgFeature_EKS_AUDIT_LOGS,
        OrgFeature_EKS_RUNTIME_MONITORING,
        OrgFeature_LAMBDA_NETWORK_LOGS,
        OrgFeature_RDS_LOGIN_EVENTS,
        OrgFeature_S3_DATA_EVENTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OrgFeature = OrgFeature'
  { fromOrgFeature ::
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

pattern OrgFeature_EBS_MALWARE_PROTECTION :: OrgFeature
pattern OrgFeature_EBS_MALWARE_PROTECTION = OrgFeature' "EBS_MALWARE_PROTECTION"

pattern OrgFeature_EKS_AUDIT_LOGS :: OrgFeature
pattern OrgFeature_EKS_AUDIT_LOGS = OrgFeature' "EKS_AUDIT_LOGS"

pattern OrgFeature_EKS_RUNTIME_MONITORING :: OrgFeature
pattern OrgFeature_EKS_RUNTIME_MONITORING = OrgFeature' "EKS_RUNTIME_MONITORING"

pattern OrgFeature_LAMBDA_NETWORK_LOGS :: OrgFeature
pattern OrgFeature_LAMBDA_NETWORK_LOGS = OrgFeature' "LAMBDA_NETWORK_LOGS"

pattern OrgFeature_RDS_LOGIN_EVENTS :: OrgFeature
pattern OrgFeature_RDS_LOGIN_EVENTS = OrgFeature' "RDS_LOGIN_EVENTS"

pattern OrgFeature_S3_DATA_EVENTS :: OrgFeature
pattern OrgFeature_S3_DATA_EVENTS = OrgFeature' "S3_DATA_EVENTS"

{-# COMPLETE
  OrgFeature_EBS_MALWARE_PROTECTION,
  OrgFeature_EKS_AUDIT_LOGS,
  OrgFeature_EKS_RUNTIME_MONITORING,
  OrgFeature_LAMBDA_NETWORK_LOGS,
  OrgFeature_RDS_LOGIN_EVENTS,
  OrgFeature_S3_DATA_EVENTS,
  OrgFeature'
  #-}
