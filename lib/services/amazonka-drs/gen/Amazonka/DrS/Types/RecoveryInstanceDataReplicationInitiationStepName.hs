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
-- Module      : Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStepName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStepName
  ( RecoveryInstanceDataReplicationInitiationStepName
      ( ..,
        RecoveryInstanceDataReplicationInitiationStepName_COMPLETE_VOLUME_MAPPING,
        RecoveryInstanceDataReplicationInitiationStepName_CONFIGURE_REPLICATION_SOFTWARE,
        RecoveryInstanceDataReplicationInitiationStepName_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT,
        RecoveryInstanceDataReplicationInitiationStepName_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION,
        RecoveryInstanceDataReplicationInitiationStepName_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION,
        RecoveryInstanceDataReplicationInitiationStepName_LINK_FAILBACK_CLIENT_WITH_RECOVERY_INSTANCE,
        RecoveryInstanceDataReplicationInitiationStepName_PAIR_AGENT_WITH_REPLICATION_SOFTWARE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecoveryInstanceDataReplicationInitiationStepName = RecoveryInstanceDataReplicationInitiationStepName'
  { fromRecoveryInstanceDataReplicationInitiationStepName ::
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

pattern RecoveryInstanceDataReplicationInitiationStepName_COMPLETE_VOLUME_MAPPING :: RecoveryInstanceDataReplicationInitiationStepName
pattern RecoveryInstanceDataReplicationInitiationStepName_COMPLETE_VOLUME_MAPPING = RecoveryInstanceDataReplicationInitiationStepName' "COMPLETE_VOLUME_MAPPING"

pattern RecoveryInstanceDataReplicationInitiationStepName_CONFIGURE_REPLICATION_SOFTWARE :: RecoveryInstanceDataReplicationInitiationStepName
pattern RecoveryInstanceDataReplicationInitiationStepName_CONFIGURE_REPLICATION_SOFTWARE = RecoveryInstanceDataReplicationInitiationStepName' "CONFIGURE_REPLICATION_SOFTWARE"

pattern RecoveryInstanceDataReplicationInitiationStepName_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT :: RecoveryInstanceDataReplicationInitiationStepName
pattern RecoveryInstanceDataReplicationInitiationStepName_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT = RecoveryInstanceDataReplicationInitiationStepName' "DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT"

pattern RecoveryInstanceDataReplicationInitiationStepName_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION :: RecoveryInstanceDataReplicationInitiationStepName
pattern RecoveryInstanceDataReplicationInitiationStepName_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION = RecoveryInstanceDataReplicationInitiationStepName' "ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION"

pattern RecoveryInstanceDataReplicationInitiationStepName_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION :: RecoveryInstanceDataReplicationInitiationStepName
pattern RecoveryInstanceDataReplicationInitiationStepName_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION = RecoveryInstanceDataReplicationInitiationStepName' "ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION"

pattern RecoveryInstanceDataReplicationInitiationStepName_LINK_FAILBACK_CLIENT_WITH_RECOVERY_INSTANCE :: RecoveryInstanceDataReplicationInitiationStepName
pattern RecoveryInstanceDataReplicationInitiationStepName_LINK_FAILBACK_CLIENT_WITH_RECOVERY_INSTANCE = RecoveryInstanceDataReplicationInitiationStepName' "LINK_FAILBACK_CLIENT_WITH_RECOVERY_INSTANCE"

pattern RecoveryInstanceDataReplicationInitiationStepName_PAIR_AGENT_WITH_REPLICATION_SOFTWARE :: RecoveryInstanceDataReplicationInitiationStepName
pattern RecoveryInstanceDataReplicationInitiationStepName_PAIR_AGENT_WITH_REPLICATION_SOFTWARE = RecoveryInstanceDataReplicationInitiationStepName' "PAIR_AGENT_WITH_REPLICATION_SOFTWARE"

{-# COMPLETE
  RecoveryInstanceDataReplicationInitiationStepName_COMPLETE_VOLUME_MAPPING,
  RecoveryInstanceDataReplicationInitiationStepName_CONFIGURE_REPLICATION_SOFTWARE,
  RecoveryInstanceDataReplicationInitiationStepName_DOWNLOAD_REPLICATION_SOFTWARE_TO_FAILBACK_CLIENT,
  RecoveryInstanceDataReplicationInitiationStepName_ESTABLISH_AGENT_REPLICATOR_SOFTWARE_COMMUNICATION,
  RecoveryInstanceDataReplicationInitiationStepName_ESTABLISH_RECOVERY_INSTANCE_COMMUNICATION,
  RecoveryInstanceDataReplicationInitiationStepName_LINK_FAILBACK_CLIENT_WITH_RECOVERY_INSTANCE,
  RecoveryInstanceDataReplicationInitiationStepName_PAIR_AGENT_WITH_REPLICATION_SOFTWARE,
  RecoveryInstanceDataReplicationInitiationStepName'
  #-}
