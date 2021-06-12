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
-- Module      : Network.AWS.AppStream.Types.FleetState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetState
  ( FleetState
      ( ..,
        FleetState_RUNNING,
        FleetState_STARTING,
        FleetState_STOPPED,
        FleetState_STOPPING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype FleetState = FleetState'
  { fromFleetState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
