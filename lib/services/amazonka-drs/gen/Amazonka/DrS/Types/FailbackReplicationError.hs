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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.FailbackReplicationError
  ( FailbackReplicationError
      ( ..,
        FailbackReplicationError_AGENT_NOT_SEEN,
        FailbackReplicationError_FAILBACK_CLIENT_NOT_SEEN,
        FailbackReplicationError_FAILED_TO_CONFIGURE_REPLICATION_SOFTWARE,
        FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT,
        FailbackReplicationError_FAILED_TO_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION,
        FailbackReplicationError_FAILED_TO_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION,
        FailbackReplicationError_FAILED_TO_PAIR_AGENT_WITH_REPLICATION_SOFTWARE,
        FailbackReplicationError_NOT_CONVERGING,
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

pattern FailbackReplicationError_FAILED_TO_CONFIGURE_REPLICATION_SOFTWARE :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_CONFIGURE_REPLICATION_SOFTWARE = FailbackReplicationError' "FAILED_TO_CONFIGURE_REPLICATION_SOFTWARE"

pattern FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT = FailbackReplicationError' "FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT"

pattern FailbackReplicationError_FAILED_TO_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION = FailbackReplicationError' "FAILED_TO_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION"

pattern FailbackReplicationError_FAILED_TO_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION = FailbackReplicationError' "FAILED_TO_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION"

pattern FailbackReplicationError_FAILED_TO_PAIR_AGENT_WITH_REPLICATION_SOFTWARE :: FailbackReplicationError
pattern FailbackReplicationError_FAILED_TO_PAIR_AGENT_WITH_REPLICATION_SOFTWARE = FailbackReplicationError' "FAILED_TO_PAIR_AGENT_WITH_REPLICATION_SOFTWARE"

pattern FailbackReplicationError_NOT_CONVERGING :: FailbackReplicationError
pattern FailbackReplicationError_NOT_CONVERGING = FailbackReplicationError' "NOT_CONVERGING"

pattern FailbackReplicationError_UNSTABLE_NETWORK :: FailbackReplicationError
pattern FailbackReplicationError_UNSTABLE_NETWORK = FailbackReplicationError' "UNSTABLE_NETWORK"

{-# COMPLETE
  FailbackReplicationError_AGENT_NOT_SEEN,
  FailbackReplicationError_FAILBACK_CLIENT_NOT_SEEN,
  FailbackReplicationError_FAILED_TO_CONFIGURE_REPLICATION_SOFTWARE,
  FailbackReplicationError_FAILED_TO_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT,
  FailbackReplicationError_FAILED_TO_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION,
  FailbackReplicationError_FAILED_TO_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION,
  FailbackReplicationError_FAILED_TO_PAIR_AGENT_WITH_REPLICATION_SOFTWARE,
  FailbackReplicationError_NOT_CONVERGING,
  FailbackReplicationError_UNSTABLE_NETWORK,
  FailbackReplicationError'
  #-}
