{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.BrokerState
  ( BrokerState
    ( BrokerState'
    , BrokerStateCreationInProgress
    , BrokerStateCreationFailed
    , BrokerStateDeletionInProgress
    , BrokerStateRunning
    , BrokerStateRebootInProgress
    , fromBrokerState
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The status of the broker.
newtype BrokerState = BrokerState'{fromBrokerState :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern BrokerStateCreationInProgress :: BrokerState
pattern BrokerStateCreationInProgress = BrokerState' "CREATION_IN_PROGRESS"

pattern BrokerStateCreationFailed :: BrokerState
pattern BrokerStateCreationFailed = BrokerState' "CREATION_FAILED"

pattern BrokerStateDeletionInProgress :: BrokerState
pattern BrokerStateDeletionInProgress = BrokerState' "DELETION_IN_PROGRESS"

pattern BrokerStateRunning :: BrokerState
pattern BrokerStateRunning = BrokerState' "RUNNING"

pattern BrokerStateRebootInProgress :: BrokerState
pattern BrokerStateRebootInProgress = BrokerState' "REBOOT_IN_PROGRESS"

{-# COMPLETE 
  BrokerStateCreationInProgress,

  BrokerStateCreationFailed,

  BrokerStateDeletionInProgress,

  BrokerStateRunning,

  BrokerStateRebootInProgress,
  BrokerState'
  #-}
