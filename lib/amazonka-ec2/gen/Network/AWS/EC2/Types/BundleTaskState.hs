{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BundleTaskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.BundleTaskState
  ( BundleTaskState
    ( BundleTaskState'
    , BundleTaskStatePending
    , BundleTaskStateWaitingForShutdown
    , BundleTaskStateBundling
    , BundleTaskStateStoring
    , BundleTaskStateCancelling
    , BundleTaskStateComplete
    , BundleTaskStateFailed
    , fromBundleTaskState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BundleTaskState = BundleTaskState'{fromBundleTaskState ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern BundleTaskStatePending :: BundleTaskState
pattern BundleTaskStatePending = BundleTaskState' "pending"

pattern BundleTaskStateWaitingForShutdown :: BundleTaskState
pattern BundleTaskStateWaitingForShutdown = BundleTaskState' "waiting-for-shutdown"

pattern BundleTaskStateBundling :: BundleTaskState
pattern BundleTaskStateBundling = BundleTaskState' "bundling"

pattern BundleTaskStateStoring :: BundleTaskState
pattern BundleTaskStateStoring = BundleTaskState' "storing"

pattern BundleTaskStateCancelling :: BundleTaskState
pattern BundleTaskStateCancelling = BundleTaskState' "cancelling"

pattern BundleTaskStateComplete :: BundleTaskState
pattern BundleTaskStateComplete = BundleTaskState' "complete"

pattern BundleTaskStateFailed :: BundleTaskState
pattern BundleTaskStateFailed = BundleTaskState' "failed"

{-# COMPLETE 
  BundleTaskStatePending,

  BundleTaskStateWaitingForShutdown,

  BundleTaskStateBundling,

  BundleTaskStateStoring,

  BundleTaskStateCancelling,

  BundleTaskStateComplete,

  BundleTaskStateFailed,
  BundleTaskState'
  #-}
