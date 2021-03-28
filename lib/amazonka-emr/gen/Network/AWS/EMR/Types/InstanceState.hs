{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceState
  ( InstanceState
    ( InstanceState'
    , InstanceStateAwaitingFulfillment
    , InstanceStateProvisioning
    , InstanceStateBootstrapping
    , InstanceStateRunning
    , InstanceStateTerminated
    , fromInstanceState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InstanceState = InstanceState'{fromInstanceState ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern InstanceStateAwaitingFulfillment :: InstanceState
pattern InstanceStateAwaitingFulfillment = InstanceState' "AWAITING_FULFILLMENT"

pattern InstanceStateProvisioning :: InstanceState
pattern InstanceStateProvisioning = InstanceState' "PROVISIONING"

pattern InstanceStateBootstrapping :: InstanceState
pattern InstanceStateBootstrapping = InstanceState' "BOOTSTRAPPING"

pattern InstanceStateRunning :: InstanceState
pattern InstanceStateRunning = InstanceState' "RUNNING"

pattern InstanceStateTerminated :: InstanceState
pattern InstanceStateTerminated = InstanceState' "TERMINATED"

{-# COMPLETE 
  InstanceStateAwaitingFulfillment,

  InstanceStateProvisioning,

  InstanceStateBootstrapping,

  InstanceStateRunning,

  InstanceStateTerminated,
  InstanceState'
  #-}
