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
        IBSDeleting,
        IBSFailed,
        IBSPending,
        IBSRebooting,
        IBSRunning,
        IBSSnapshotting,
        IBSStopped,
        IBSStopping,
        IBSUpdatingAgent
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

pattern IBSDeleting :: ImageBuilderState
pattern IBSDeleting = ImageBuilderState' "DELETING"

pattern IBSFailed :: ImageBuilderState
pattern IBSFailed = ImageBuilderState' "FAILED"

pattern IBSPending :: ImageBuilderState
pattern IBSPending = ImageBuilderState' "PENDING"

pattern IBSRebooting :: ImageBuilderState
pattern IBSRebooting = ImageBuilderState' "REBOOTING"

pattern IBSRunning :: ImageBuilderState
pattern IBSRunning = ImageBuilderState' "RUNNING"

pattern IBSSnapshotting :: ImageBuilderState
pattern IBSSnapshotting = ImageBuilderState' "SNAPSHOTTING"

pattern IBSStopped :: ImageBuilderState
pattern IBSStopped = ImageBuilderState' "STOPPED"

pattern IBSStopping :: ImageBuilderState
pattern IBSStopping = ImageBuilderState' "STOPPING"

pattern IBSUpdatingAgent :: ImageBuilderState
pattern IBSUpdatingAgent = ImageBuilderState' "UPDATING_AGENT"

{-# COMPLETE
  IBSDeleting,
  IBSFailed,
  IBSPending,
  IBSRebooting,
  IBSRunning,
  IBSSnapshotting,
  IBSStopped,
  IBSStopping,
  IBSUpdatingAgent,
  ImageBuilderState'
  #-}
