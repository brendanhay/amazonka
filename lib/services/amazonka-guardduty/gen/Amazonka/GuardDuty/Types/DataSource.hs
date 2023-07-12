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
-- Module      : Amazonka.GuardDuty.Types.DataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DataSource
  ( DataSource
      ( ..,
        DataSource_CLOUD_TRAIL,
        DataSource_DNS_LOGS,
        DataSource_EC2_MALWARE_SCAN,
        DataSource_FLOW_LOGS,
        DataSource_KUBERNETES_AUDIT_LOGS,
        DataSource_S3_LOGS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataSource = DataSource'
  { fromDataSource ::
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

pattern DataSource_CLOUD_TRAIL :: DataSource
pattern DataSource_CLOUD_TRAIL = DataSource' "CLOUD_TRAIL"

pattern DataSource_DNS_LOGS :: DataSource
pattern DataSource_DNS_LOGS = DataSource' "DNS_LOGS"

pattern DataSource_EC2_MALWARE_SCAN :: DataSource
pattern DataSource_EC2_MALWARE_SCAN = DataSource' "EC2_MALWARE_SCAN"

pattern DataSource_FLOW_LOGS :: DataSource
pattern DataSource_FLOW_LOGS = DataSource' "FLOW_LOGS"

pattern DataSource_KUBERNETES_AUDIT_LOGS :: DataSource
pattern DataSource_KUBERNETES_AUDIT_LOGS = DataSource' "KUBERNETES_AUDIT_LOGS"

pattern DataSource_S3_LOGS :: DataSource
pattern DataSource_S3_LOGS = DataSource' "S3_LOGS"

{-# COMPLETE
  DataSource_CLOUD_TRAIL,
  DataSource_DNS_LOGS,
  DataSource_EC2_MALWARE_SCAN,
  DataSource_FLOW_LOGS,
  DataSource_KUBERNETES_AUDIT_LOGS,
  DataSource_S3_LOGS,
  DataSource'
  #-}
