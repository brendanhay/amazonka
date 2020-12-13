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
        DSSPending,
        DSSCompleted,
        DSSError,
        DSSUnknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DiskSnapshotState = DiskSnapshotState' Lude.Text
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

pattern DSSPending :: DiskSnapshotState
pattern DSSPending = DiskSnapshotState' "pending"

pattern DSSCompleted :: DiskSnapshotState
pattern DSSCompleted = DiskSnapshotState' "completed"

pattern DSSError :: DiskSnapshotState
pattern DSSError = DiskSnapshotState' "error"

pattern DSSUnknown :: DiskSnapshotState
pattern DSSUnknown = DiskSnapshotState' "unknown"

{-# COMPLETE
  DSSPending,
  DSSCompleted,
  DSSError,
  DSSUnknown,
  DiskSnapshotState'
  #-}
