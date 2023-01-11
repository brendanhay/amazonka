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
-- Module      : Amazonka.SimSpaceWeaver.Types.SimulationTargetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.SimulationTargetStatus
  ( SimulationTargetStatus
      ( ..,
        SimulationTargetStatus_DELETED,
        SimulationTargetStatus_STARTED,
        SimulationTargetStatus_STOPPED,
        SimulationTargetStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SimulationTargetStatus = SimulationTargetStatus'
  { fromSimulationTargetStatus ::
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

pattern SimulationTargetStatus_DELETED :: SimulationTargetStatus
pattern SimulationTargetStatus_DELETED = SimulationTargetStatus' "DELETED"

pattern SimulationTargetStatus_STARTED :: SimulationTargetStatus
pattern SimulationTargetStatus_STARTED = SimulationTargetStatus' "STARTED"

pattern SimulationTargetStatus_STOPPED :: SimulationTargetStatus
pattern SimulationTargetStatus_STOPPED = SimulationTargetStatus' "STOPPED"

pattern SimulationTargetStatus_UNKNOWN :: SimulationTargetStatus
pattern SimulationTargetStatus_UNKNOWN = SimulationTargetStatus' "UNKNOWN"

{-# COMPLETE
  SimulationTargetStatus_DELETED,
  SimulationTargetStatus_STARTED,
  SimulationTargetStatus_STOPPED,
  SimulationTargetStatus_UNKNOWN,
  SimulationTargetStatus'
  #-}
