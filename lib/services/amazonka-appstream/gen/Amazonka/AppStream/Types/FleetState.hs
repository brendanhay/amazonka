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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype FleetState = FleetState'
  { fromFleetState ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
