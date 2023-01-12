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
-- Module      : Amazonka.DrS.Types.FailbackReplicationError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.FailbackReplicationError
  ( FailbackReplicationError
      ( ..,
        FailbackReplicationError_AGENT_NOT_SEEN,
        FailbackReplicationError_FAILBACK_CLIENT_NOT_SEEN,
        FailbackReplicationError_FAILED_GETTING_REPLICATION_STATE,
        FailbackReplicationError_FAILED_TO_ATTACH_STAGING_DISKS,
        FailbackReplicationError_FAILED_TO_AUTHENTICATE_WITH_SERVICE,
        FailbackReplicationError_FAILED_TO_BOOT_REPLICATION_SERVER,
        FailbackReplicationError_FAILED_TO_CONFIGURE_REPLICATION_SOFTWARE,
        FailbackReplicationError_FAILED_TO_CONNECT_AGENT_TO_REPLICATION_SERVER,
        FailbackReplicationError_FAILED_TO_CREATE_SECURITY_GROUP,
        FailbackReplicationError_FAILED_TO_CREATE_STAGING_DISKS,
        FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE,
        FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT,
        FailbackReplicationError_FAILED_TO_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION,
        FailbackReplicationError_FAILED_TO_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION,
        FailbackReplicationError_FAILED_TO_LAUNCH_REPLICATION_SERVER,
        FailbackReplicationError_FAILED_TO_PAIR_AGENT_WITH_REPLICATION_SOFTWARE,
        FailbackReplicationError_FAILED_TO_PAIR_REPLICATION_SERVER_WITH_AGENT,
        FailbackReplicationError_FAILED_TO_START_DATA_TRANSFER,
        FailbackReplicationError_NOT_CONVERGING,
        FailbackReplicationError_SNAPSHOTS_FAILURE,
        FailbackReplicationError_UNSTABLE_NETWORK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FailbackReplicationError = FailbackReplicationError'
  { fromFailbackReplicationError ::
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

pattern FailbackReplicationError_AGENT_NOT_SEEN :: FailbackReplicationError
pattern FailbackReplicationError_AGENT_NOT_SEEN = FailbackReplicationError' "AGENT_NOT_SEEN"

pattern FailbackReplicationError_FAILBACK_CLIENT_NOT_SEEN :: FailbackReplicationError
pattern FailbackReplicationError_FAILBACK_CLIENT_NOT_SEEN = FailbackReplicationError' "FAILBACK_CLIENT_NOT_SEEN"

pattern FailbackReplicationError_FAILED_GETTING_REPLICATION_STATE :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_GETTING_REPLICATION_STATE = FailbackReplicationError' "FAILED_GETTING_REPLICATION_STATE"

pattern FailbackReplicationError_FAILED_TO_ATTACH_STAGING_DISKS :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_ATTACH_STAGING_DISKS = FailbackReplicationError' "FAILED_TO_ATTACH_STAGING_DISKS"

pattern FailbackReplicationError_FAILED_TO_AUTHENTICATE_WITH_SERVICE :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_AUTHENTICATE_WITH_SERVICE = FailbackReplicationError' "FAILED_TO_AUTHENTICATE_WITH_SERVICE"

pattern FailbackReplicationError_FAILED_TO_BOOT_REPLICATION_SERVER :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_BOOT_REPLICATION_SERVER = FailbackReplicationError' "FAILED_TO_BOOT_REPLICATION_SERVER"

pattern FailbackReplicationError_FAILED_TO_CONFIGURE_REPLICATION_SOFTWARE :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_CONFIGURE_REPLICATION_SOFTWARE = FailbackReplicationError' "FAILED_TO_CONFIGURE_REPLICATION_SOFTWARE"

pattern FailbackReplicationError_FAILED_TO_CONNECT_AGENT_TO_REPLICATION_SERVER :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_CONNECT_AGENT_TO_REPLICATION_SERVER = FailbackReplicationError' "FAILED_TO_CONNECT_AGENT_TO_REPLICATION_SERVER"

pattern FailbackReplicationError_FAILED_TO_CREATE_SECURITY_GROUP :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_CREATE_SECURITY_GROUP = FailbackReplicationError' "FAILED_TO_CREATE_SECURITY_GROUP"

pattern FailbackReplicationError_FAILED_TO_CREATE_STAGING_DISKS :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_CREATE_STAGING_DISKS = FailbackReplicationError' "FAILED_TO_CREATE_STAGING_DISKS"

pattern FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE = FailbackReplicationError' "FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE"

pattern FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT = FailbackReplicationError' "FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT"

pattern FailbackReplicationError_FAILED_TO_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION = FailbackReplicationError' "FAILED_TO_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION"

pattern FailbackReplicationError_FAILED_TO_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION = FailbackReplicationError' "FAILED_TO_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION"

pattern FailbackReplicationError_FAILED_TO_LAUNCH_REPLICATION_SERVER :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_LAUNCH_REPLICATION_SERVER = FailbackReplicationError' "FAILED_TO_LAUNCH_REPLICATION_SERVER"

pattern FailbackReplicationError_FAILED_TO_PAIR_AGENT_WITH_REPLICATION_SOFTWARE :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_PAIR_AGENT_WITH_REPLICATION_SOFTWARE = FailbackReplicationError' "FAILED_TO_PAIR_AGENT_WITH_REPLICATION_SOFTWARE"

pattern FailbackReplicationError_FAILED_TO_PAIR_REPLICATION_SERVER_WITH_AGENT :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_PAIR_REPLICATION_SERVER_WITH_AGENT = FailbackReplicationError' "FAILED_TO_PAIR_REPLICATION_SERVER_WITH_AGENT"

pattern FailbackReplicationError_FAILED_TO_START_DATA_TRANSFER :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_START_DATA_TRANSFER = FailbackReplicationError' "FAILED_TO_START_DATA_TRANSFER"

pattern FailbackReplicationError_NOT_CONVERGING :: FailbackReplicationError
pattern FailbackReplicationError_NOT_CONVERGING = FailbackReplicationError' "NOT_CONVERGING"

pattern FailbackReplicationError_SNAPSHOTS_FAILURE :: FailbackReplicationError
pattern FailbackReplicationError_SNAPSHOTS_FAILURE = FailbackReplicationError' "SNAPSHOTS_FAILURE"

pattern FailbackReplicationError_UNSTABLE_NETWORK :: FailbackReplicationError
pattern FailbackReplicationError_UNSTABLE_NETWORK = FailbackReplicationError' "UNSTABLE_NETWORK"

{-# COMPLETE
  FailbackReplicationError_AGENT_NOT_SEEN,
  FailbackReplicationError_FAILBACK_CLIENT_NOT_SEEN,
  FailbackReplicationError_FAILED_GETTING_REPLICATION_STATE,
  FailbackReplicationError_FAILED_TO_ATTACH_STAGING_DISKS,
  FailbackReplicationError_FAILED_TO_AUTHENTICATE_WITH_SERVICE,
  FailbackReplicationError_FAILED_TO_BOOT_REPLICATION_SERVER,
  FailbackReplicationError_FAILED_TO_CONFIGURE_REPLICATION_SOFTWARE,
  FailbackReplicationError_FAILED_TO_CONNECT_AGENT_TO_REPLICATION_SERVER,
  FailbackReplicationError_FAILED_TO_CREATE_SECURITY_GROUP,
  FailbackReplicationError_FAILED_TO_CREATE_STAGING_DISKS,
  FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE,
  FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT,
  FailbackReplicationError_FAILED_TO_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION,
  FailbackReplicationError_FAILED_TO_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION,
  FailbackReplicationError_FAILED_TO_LAUNCH_REPLICATION_SERVER,
  FailbackReplicationError_FAILED_TO_PAIR_AGENT_WITH_REPLICATION_SOFTWARE,
  FailbackReplicationError_FAILED_TO_PAIR_REPLICATION_SERVER_WITH_AGENT,
  FailbackReplicationError_FAILED_TO_START_DATA_TRANSFER,
  FailbackReplicationError_NOT_CONVERGING,
  FailbackReplicationError_SNAPSHOTS_FAILURE,
  FailbackReplicationError_UNSTABLE_NETWORK,
  FailbackReplicationError'
  #-}
