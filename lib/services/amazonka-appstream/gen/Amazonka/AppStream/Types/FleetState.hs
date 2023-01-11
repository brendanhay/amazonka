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
-- Module      : Amazonka.AppStream.Types.FleetState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.FleetState
  ( FleetState
      ( ..,
        FleetState_RUNNING,
        FleetState_STARTING,
        FleetState_STOPPED,
        FleetState_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FleetState = FleetState'
  { fromFleetState ::
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

pattern FleetState_RUNNING :: FleetState
pattern FleetState_RUNNING = FleetState' "RUNNING"

pattern FleetState_STARTING :: FleetState
pattern FleetState_STARTING = FleetState' "STARTING"

pattern FleetState_STOPPED :: FleetState
pattern FleetState_STOPPED = FleetState' "STOPPED"

pattern FleetState_STOPPING :: FleetState
pattern FleetState_STOPPING = FleetState' "STOPPING"

{-# COMPLETE
  FleetState_RUNNING,
  FleetState_STARTING,
  FleetState_STOPPED,
  FleetState_STOPPING,
  FleetState'
  #-}
