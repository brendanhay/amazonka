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
-- Module      : Amazonka.MediaConnect.Types.BridgeState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.BridgeState
  ( BridgeState
      ( ..,
        BridgeState_ACTIVE,
        BridgeState_CREATING,
        BridgeState_DELETED,
        BridgeState_DELETING,
        BridgeState_DEPLOYING,
        BridgeState_STANDBY,
        BridgeState_STARTING,
        BridgeState_START_FAILED,
        BridgeState_START_PENDING,
        BridgeState_STOPPING,
        BridgeState_STOP_FAILED,
        BridgeState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BridgeState = BridgeState'
  { fromBridgeState ::
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

pattern BridgeState_ACTIVE :: BridgeState
pattern BridgeState_ACTIVE = BridgeState' "ACTIVE"

pattern BridgeState_CREATING :: BridgeState
pattern BridgeState_CREATING = BridgeState' "CREATING"

pattern BridgeState_DELETED :: BridgeState
pattern BridgeState_DELETED = BridgeState' "DELETED"

pattern BridgeState_DELETING :: BridgeState
pattern BridgeState_DELETING = BridgeState' "DELETING"

pattern BridgeState_DEPLOYING :: BridgeState
pattern BridgeState_DEPLOYING = BridgeState' "DEPLOYING"

pattern BridgeState_STANDBY :: BridgeState
pattern BridgeState_STANDBY = BridgeState' "STANDBY"

pattern BridgeState_STARTING :: BridgeState
pattern BridgeState_STARTING = BridgeState' "STARTING"

pattern BridgeState_START_FAILED :: BridgeState
pattern BridgeState_START_FAILED = BridgeState' "START_FAILED"

pattern BridgeState_START_PENDING :: BridgeState
pattern BridgeState_START_PENDING = BridgeState' "START_PENDING"

pattern BridgeState_STOPPING :: BridgeState
pattern BridgeState_STOPPING = BridgeState' "STOPPING"

pattern BridgeState_STOP_FAILED :: BridgeState
pattern BridgeState_STOP_FAILED = BridgeState' "STOP_FAILED"

pattern BridgeState_UPDATING :: BridgeState
pattern BridgeState_UPDATING = BridgeState' "UPDATING"

{-# COMPLETE
  BridgeState_ACTIVE,
  BridgeState_CREATING,
  BridgeState_DELETED,
  BridgeState_DELETING,
  BridgeState_DEPLOYING,
  BridgeState_STANDBY,
  BridgeState_STARTING,
  BridgeState_START_FAILED,
  BridgeState_START_PENDING,
  BridgeState_STOPPING,
  BridgeState_STOP_FAILED,
  BridgeState_UPDATING,
  BridgeState'
  #-}
