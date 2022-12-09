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
-- Module      : Amazonka.SimSpaceWeaver.Types.SimulationAppTargetStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.SimulationAppTargetStatus
  ( SimulationAppTargetStatus
      ( ..,
        SimulationAppTargetStatus_STARTED,
        SimulationAppTargetStatus_STOPPED,
        SimulationAppTargetStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SimulationAppTargetStatus = SimulationAppTargetStatus'
  { fromSimulationAppTargetStatus ::
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

pattern SimulationAppTargetStatus_STARTED :: SimulationAppTargetStatus
pattern SimulationAppTargetStatus_STARTED = SimulationAppTargetStatus' "STARTED"

pattern SimulationAppTargetStatus_STOPPED :: SimulationAppTargetStatus
pattern SimulationAppTargetStatus_STOPPED = SimulationAppTargetStatus' "STOPPED"

pattern SimulationAppTargetStatus_UNKNOWN :: SimulationAppTargetStatus
pattern SimulationAppTargetStatus_UNKNOWN = SimulationAppTargetStatus' "UNKNOWN"

{-# COMPLETE
  SimulationAppTargetStatus_STARTED,
  SimulationAppTargetStatus_STOPPED,
  SimulationAppTargetStatus_UNKNOWN,
  SimulationAppTargetStatus'
  #-}
