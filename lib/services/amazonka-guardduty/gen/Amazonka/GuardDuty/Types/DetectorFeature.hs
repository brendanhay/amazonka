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
-- Module      : Amazonka.GuardDuty.Types.DetectorFeature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DetectorFeature
  ( DetectorFeature
      ( ..,
        DetectorFeature_EBS_MALWARE_PROTECTION,
        DetectorFeature_EKS_AUDIT_LOGS,
        DetectorFeature_EKS_RUNTIME_MONITORING,
        DetectorFeature_LAMBDA_NETWORK_LOGS,
        DetectorFeature_RDS_LOGIN_EVENTS,
        DetectorFeature_S3_DATA_EVENTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DetectorFeature = DetectorFeature'
  { fromDetectorFeature ::
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

pattern DetectorFeature_EBS_MALWARE_PROTECTION :: DetectorFeature
pattern DetectorFeature_EBS_MALWARE_PROTECTION = DetectorFeature' "EBS_MALWARE_PROTECTION"

pattern DetectorFeature_EKS_AUDIT_LOGS :: DetectorFeature
pattern DetectorFeature_EKS_AUDIT_LOGS = DetectorFeature' "EKS_AUDIT_LOGS"

pattern DetectorFeature_EKS_RUNTIME_MONITORING :: DetectorFeature
pattern DetectorFeature_EKS_RUNTIME_MONITORING = DetectorFeature' "EKS_RUNTIME_MONITORING"

pattern DetectorFeature_LAMBDA_NETWORK_LOGS :: DetectorFeature
pattern DetectorFeature_LAMBDA_NETWORK_LOGS = DetectorFeature' "LAMBDA_NETWORK_LOGS"

pattern DetectorFeature_RDS_LOGIN_EVENTS :: DetectorFeature
pattern DetectorFeature_RDS_LOGIN_EVENTS = DetectorFeature' "RDS_LOGIN_EVENTS"

pattern DetectorFeature_S3_DATA_EVENTS :: DetectorFeature
pattern DetectorFeature_S3_DATA_EVENTS = DetectorFeature' "S3_DATA_EVENTS"

{-# COMPLETE
  DetectorFeature_EBS_MALWARE_PROTECTION,
  DetectorFeature_EKS_AUDIT_LOGS,
  DetectorFeature_EKS_RUNTIME_MONITORING,
  DetectorFeature_LAMBDA_NETWORK_LOGS,
  DetectorFeature_RDS_LOGIN_EVENTS,
  DetectorFeature_S3_DATA_EVENTS,
  DetectorFeature'
  #-}
