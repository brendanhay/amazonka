{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageBuilderState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.ImageBuilderState
  ( ImageBuilderState
    ( ImageBuilderState'
    , ImageBuilderStatePending
    , ImageBuilderStateUpdatingAgent
    , ImageBuilderStateRunning
    , ImageBuilderStateStopping
    , ImageBuilderStateStopped
    , ImageBuilderStateRebooting
    , ImageBuilderStateSnapshotting
    , ImageBuilderStateDeleting
    , ImageBuilderStateFailed
    , fromImageBuilderState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ImageBuilderState = ImageBuilderState'{fromImageBuilderState
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern ImageBuilderStatePending :: ImageBuilderState
pattern ImageBuilderStatePending = ImageBuilderState' "PENDING"

pattern ImageBuilderStateUpdatingAgent :: ImageBuilderState
pattern ImageBuilderStateUpdatingAgent = ImageBuilderState' "UPDATING_AGENT"

pattern ImageBuilderStateRunning :: ImageBuilderState
pattern ImageBuilderStateRunning = ImageBuilderState' "RUNNING"

pattern ImageBuilderStateStopping :: ImageBuilderState
pattern ImageBuilderStateStopping = ImageBuilderState' "STOPPING"

pattern ImageBuilderStateStopped :: ImageBuilderState
pattern ImageBuilderStateStopped = ImageBuilderState' "STOPPED"

pattern ImageBuilderStateRebooting :: ImageBuilderState
pattern ImageBuilderStateRebooting = ImageBuilderState' "REBOOTING"

pattern ImageBuilderStateSnapshotting :: ImageBuilderState
pattern ImageBuilderStateSnapshotting = ImageBuilderState' "SNAPSHOTTING"

pattern ImageBuilderStateDeleting :: ImageBuilderState
pattern ImageBuilderStateDeleting = ImageBuilderState' "DELETING"

pattern ImageBuilderStateFailed :: ImageBuilderState
pattern ImageBuilderStateFailed = ImageBuilderState' "FAILED"

{-# COMPLETE 
  ImageBuilderStatePending,

  ImageBuilderStateUpdatingAgent,

  ImageBuilderStateRunning,

  ImageBuilderStateStopping,

  ImageBuilderStateStopped,

  ImageBuilderStateRebooting,

  ImageBuilderStateSnapshotting,

  ImageBuilderStateDeleting,

  ImageBuilderStateFailed,
  ImageBuilderState'
  #-}
