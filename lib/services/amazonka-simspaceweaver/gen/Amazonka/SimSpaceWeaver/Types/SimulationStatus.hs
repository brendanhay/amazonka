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
-- Module      : Amazonka.SimSpaceWeaver.Types.SimulationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.SimulationStatus
  ( SimulationStatus
      ( ..,
        SimulationStatus_DELETED,
        SimulationStatus_DELETING,
        SimulationStatus_FAILED,
        SimulationStatus_STARTED,
        SimulationStatus_STARTING,
        SimulationStatus_STOPPED,
        SimulationStatus_STOPPING,
        SimulationStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SimulationStatus = SimulationStatus'
  { fromSimulationStatus ::
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

pattern SimulationStatus_DELETED :: SimulationStatus
pattern SimulationStatus_DELETED = SimulationStatus' "DELETED"

pattern SimulationStatus_DELETING :: SimulationStatus
pattern SimulationStatus_DELETING = SimulationStatus' "DELETING"

pattern SimulationStatus_FAILED :: SimulationStatus
pattern SimulationStatus_FAILED = SimulationStatus' "FAILED"

pattern SimulationStatus_STARTED :: SimulationStatus
pattern SimulationStatus_STARTED = SimulationStatus' "STARTED"

pattern SimulationStatus_STARTING :: SimulationStatus
pattern SimulationStatus_STARTING = SimulationStatus' "STARTING"

pattern SimulationStatus_STOPPED :: SimulationStatus
pattern SimulationStatus_STOPPED = SimulationStatus' "STOPPED"

pattern SimulationStatus_STOPPING :: SimulationStatus
pattern SimulationStatus_STOPPING = SimulationStatus' "STOPPING"

pattern SimulationStatus_UNKNOWN :: SimulationStatus
pattern SimulationStatus_UNKNOWN = SimulationStatus' "UNKNOWN"

{-# COMPLETE
  SimulationStatus_DELETED,
  SimulationStatus_DELETING,
  SimulationStatus_FAILED,
  SimulationStatus_STARTED,
  SimulationStatus_STARTING,
  SimulationStatus_STOPPED,
  SimulationStatus_STOPPING,
  SimulationStatus_UNKNOWN,
  SimulationStatus'
  #-}
