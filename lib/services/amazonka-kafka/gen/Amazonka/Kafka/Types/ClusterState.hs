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
-- Module      : Amazonka.Kafka.Types.ClusterState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ClusterState
  ( ClusterState
      ( ..,
        ClusterState_ACTIVE,
        ClusterState_CREATING,
        ClusterState_DELETING,
        ClusterState_FAILED,
        ClusterState_HEALING,
        ClusterState_MAINTENANCE,
        ClusterState_REBOOTING_BROKER,
        ClusterState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The state of the Apache Kafka cluster.
newtype ClusterState = ClusterState'
  { fromClusterState ::
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

pattern ClusterState_ACTIVE :: ClusterState
pattern ClusterState_ACTIVE = ClusterState' "ACTIVE"

pattern ClusterState_CREATING :: ClusterState
pattern ClusterState_CREATING = ClusterState' "CREATING"

pattern ClusterState_DELETING :: ClusterState
pattern ClusterState_DELETING = ClusterState' "DELETING"

pattern ClusterState_FAILED :: ClusterState
pattern ClusterState_FAILED = ClusterState' "FAILED"

pattern ClusterState_HEALING :: ClusterState
pattern ClusterState_HEALING = ClusterState' "HEALING"

pattern ClusterState_MAINTENANCE :: ClusterState
pattern ClusterState_MAINTENANCE = ClusterState' "MAINTENANCE"

pattern ClusterState_REBOOTING_BROKER :: ClusterState
pattern ClusterState_REBOOTING_BROKER = ClusterState' "REBOOTING_BROKER"

pattern ClusterState_UPDATING :: ClusterState
pattern ClusterState_UPDATING = ClusterState' "UPDATING"

{-# COMPLETE
  ClusterState_ACTIVE,
  ClusterState_CREATING,
  ClusterState_DELETING,
  ClusterState_FAILED,
  ClusterState_HEALING,
  ClusterState_MAINTENANCE,
  ClusterState_REBOOTING_BROKER,
  ClusterState_UPDATING,
  ClusterState'
  #-}
