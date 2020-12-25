{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskSnapshotState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskSnapshotState
  ( DiskSnapshotState
      ( DiskSnapshotState',
        DiskSnapshotStatePending,
        DiskSnapshotStateCompleted,
        DiskSnapshotStateError,
        DiskSnapshotStateUnknown,
        fromDiskSnapshotState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DiskSnapshotState = DiskSnapshotState'
  { fromDiskSnapshotState ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DiskSnapshotStatePending :: DiskSnapshotState
pattern DiskSnapshotStatePending = DiskSnapshotState' "pending"

pattern DiskSnapshotStateCompleted :: DiskSnapshotState
pattern DiskSnapshotStateCompleted = DiskSnapshotState' "completed"

pattern DiskSnapshotStateError :: DiskSnapshotState
pattern DiskSnapshotStateError = DiskSnapshotState' "error"

pattern DiskSnapshotStateUnknown :: DiskSnapshotState
pattern DiskSnapshotStateUnknown = DiskSnapshotState' "unknown"

{-# COMPLETE
  DiskSnapshotStatePending,
  DiskSnapshotStateCompleted,
  DiskSnapshotStateError,
  DiskSnapshotStateUnknown,
  DiskSnapshotState'
  #-}
