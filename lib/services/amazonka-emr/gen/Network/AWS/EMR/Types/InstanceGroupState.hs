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
-- Module      : Network.AWS.EMR.Types.InstanceGroupState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupState
  ( InstanceGroupState
      ( ..,
        InstanceGroupState_ARRESTED,
        InstanceGroupState_BOOTSTRAPPING,
        InstanceGroupState_ENDED,
        InstanceGroupState_PROVISIONING,
        InstanceGroupState_RECONFIGURING,
        InstanceGroupState_RESIZING,
        InstanceGroupState_RUNNING,
        InstanceGroupState_SHUTTING_DOWN,
        InstanceGroupState_SUSPENDED,
        InstanceGroupState_TERMINATED,
        InstanceGroupState_TERMINATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InstanceGroupState = InstanceGroupState'
  { fromInstanceGroupState ::
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

pattern InstanceGroupState_ARRESTED :: InstanceGroupState
pattern InstanceGroupState_ARRESTED = InstanceGroupState' "ARRESTED"

pattern InstanceGroupState_BOOTSTRAPPING :: InstanceGroupState
pattern InstanceGroupState_BOOTSTRAPPING = InstanceGroupState' "BOOTSTRAPPING"

pattern InstanceGroupState_ENDED :: InstanceGroupState
pattern InstanceGroupState_ENDED = InstanceGroupState' "ENDED"

pattern InstanceGroupState_PROVISIONING :: InstanceGroupState
pattern InstanceGroupState_PROVISIONING = InstanceGroupState' "PROVISIONING"

pattern InstanceGroupState_RECONFIGURING :: InstanceGroupState
pattern InstanceGroupState_RECONFIGURING = InstanceGroupState' "RECONFIGURING"

pattern InstanceGroupState_RESIZING :: InstanceGroupState
pattern InstanceGroupState_RESIZING = InstanceGroupState' "RESIZING"

pattern InstanceGroupState_RUNNING :: InstanceGroupState
pattern InstanceGroupState_RUNNING = InstanceGroupState' "RUNNING"

pattern InstanceGroupState_SHUTTING_DOWN :: InstanceGroupState
pattern InstanceGroupState_SHUTTING_DOWN = InstanceGroupState' "SHUTTING_DOWN"

pattern InstanceGroupState_SUSPENDED :: InstanceGroupState
pattern InstanceGroupState_SUSPENDED = InstanceGroupState' "SUSPENDED"

pattern InstanceGroupState_TERMINATED :: InstanceGroupState
pattern InstanceGroupState_TERMINATED = InstanceGroupState' "TERMINATED"

pattern InstanceGroupState_TERMINATING :: InstanceGroupState
pattern InstanceGroupState_TERMINATING = InstanceGroupState' "TERMINATING"

{-# COMPLETE
  InstanceGroupState_ARRESTED,
  InstanceGroupState_BOOTSTRAPPING,
  InstanceGroupState_ENDED,
  InstanceGroupState_PROVISIONING,
  InstanceGroupState_RECONFIGURING,
  InstanceGroupState_RESIZING,
  InstanceGroupState_RUNNING,
  InstanceGroupState_SHUTTING_DOWN,
  InstanceGroupState_SUSPENDED,
  InstanceGroupState_TERMINATED,
  InstanceGroupState_TERMINATING,
  InstanceGroupState'
  #-}
