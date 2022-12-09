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
-- Module      : Amazonka.SimSpaceWeaver.Types.SimulationAppStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.SimulationAppStatus
  ( SimulationAppStatus
      ( ..,
        SimulationAppStatus_ERROR,
        SimulationAppStatus_STARTED,
        SimulationAppStatus_STARTING,
        SimulationAppStatus_STOPPED,
        SimulationAppStatus_STOPPING,
        SimulationAppStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SimulationAppStatus = SimulationAppStatus'
  { fromSimulationAppStatus ::
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

pattern SimulationAppStatus_ERROR :: SimulationAppStatus
pattern SimulationAppStatus_ERROR = SimulationAppStatus' "ERROR"

pattern SimulationAppStatus_STARTED :: SimulationAppStatus
pattern SimulationAppStatus_STARTED = SimulationAppStatus' "STARTED"

pattern SimulationAppStatus_STARTING :: SimulationAppStatus
pattern SimulationAppStatus_STARTING = SimulationAppStatus' "STARTING"

pattern SimulationAppStatus_STOPPED :: SimulationAppStatus
pattern SimulationAppStatus_STOPPED = SimulationAppStatus' "STOPPED"

pattern SimulationAppStatus_STOPPING :: SimulationAppStatus
pattern SimulationAppStatus_STOPPING = SimulationAppStatus' "STOPPING"

pattern SimulationAppStatus_UNKNOWN :: SimulationAppStatus
pattern SimulationAppStatus_UNKNOWN = SimulationAppStatus' "UNKNOWN"

{-# COMPLETE
  SimulationAppStatus_ERROR,
  SimulationAppStatus_STARTED,
  SimulationAppStatus_STARTING,
  SimulationAppStatus_STOPPED,
  SimulationAppStatus_STOPPING,
  SimulationAppStatus_UNKNOWN,
  SimulationAppStatus'
  #-}
