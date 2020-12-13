{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageBuilderState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageBuilderState
  ( ImageBuilderState
      ( ImageBuilderState',
        IBSPending,
        IBSUpdatingAgent,
        IBSRunning,
        IBSStopping,
        IBSStopped,
        IBSRebooting,
        IBSSnapshotting,
        IBSDeleting,
        IBSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ImageBuilderState = ImageBuilderState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern IBSPending :: ImageBuilderState
pattern IBSPending = ImageBuilderState' "PENDING"

pattern IBSUpdatingAgent :: ImageBuilderState
pattern IBSUpdatingAgent = ImageBuilderState' "UPDATING_AGENT"

pattern IBSRunning :: ImageBuilderState
pattern IBSRunning = ImageBuilderState' "RUNNING"

pattern IBSStopping :: ImageBuilderState
pattern IBSStopping = ImageBuilderState' "STOPPING"

pattern IBSStopped :: ImageBuilderState
pattern IBSStopped = ImageBuilderState' "STOPPED"

pattern IBSRebooting :: ImageBuilderState
pattern IBSRebooting = ImageBuilderState' "REBOOTING"

pattern IBSSnapshotting :: ImageBuilderState
pattern IBSSnapshotting = ImageBuilderState' "SNAPSHOTTING"

pattern IBSDeleting :: ImageBuilderState
pattern IBSDeleting = ImageBuilderState' "DELETING"

pattern IBSFailed :: ImageBuilderState
pattern IBSFailed = ImageBuilderState' "FAILED"

{-# COMPLETE
  IBSPending,
  IBSUpdatingAgent,
  IBSRunning,
  IBSStopping,
  IBSStopped,
  IBSRebooting,
  IBSSnapshotting,
  IBSDeleting,
  IBSFailed,
  ImageBuilderState'
  #-}
