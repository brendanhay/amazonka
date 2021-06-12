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
-- Module      : Network.AWS.EMR.Types.InstanceFleetState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetState
  ( InstanceFleetState
      ( ..,
        InstanceFleetState_BOOTSTRAPPING,
        InstanceFleetState_PROVISIONING,
        InstanceFleetState_RESIZING,
        InstanceFleetState_RUNNING,
        InstanceFleetState_SUSPENDED,
        InstanceFleetState_TERMINATED,
        InstanceFleetState_TERMINATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype InstanceFleetState = InstanceFleetState'
  { fromInstanceFleetState ::
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

pattern InstanceFleetState_BOOTSTRAPPING :: InstanceFleetState
pattern InstanceFleetState_BOOTSTRAPPING = InstanceFleetState' "BOOTSTRAPPING"

pattern InstanceFleetState_PROVISIONING :: InstanceFleetState
pattern InstanceFleetState_PROVISIONING = InstanceFleetState' "PROVISIONING"

pattern InstanceFleetState_RESIZING :: InstanceFleetState
pattern InstanceFleetState_RESIZING = InstanceFleetState' "RESIZING"

pattern InstanceFleetState_RUNNING :: InstanceFleetState
pattern InstanceFleetState_RUNNING = InstanceFleetState' "RUNNING"

pattern InstanceFleetState_SUSPENDED :: InstanceFleetState
pattern InstanceFleetState_SUSPENDED = InstanceFleetState' "SUSPENDED"

pattern InstanceFleetState_TERMINATED :: InstanceFleetState
pattern InstanceFleetState_TERMINATED = InstanceFleetState' "TERMINATED"

pattern InstanceFleetState_TERMINATING :: InstanceFleetState
pattern InstanceFleetState_TERMINATING = InstanceFleetState' "TERMINATING"

{-# COMPLETE
  InstanceFleetState_BOOTSTRAPPING,
  InstanceFleetState_PROVISIONING,
  InstanceFleetState_RESIZING,
  InstanceFleetState_RUNNING,
  InstanceFleetState_SUSPENDED,
  InstanceFleetState_TERMINATED,
  InstanceFleetState_TERMINATING,
  InstanceFleetState'
  #-}
