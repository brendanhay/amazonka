{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskSnapshotState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskSnapshotState
  ( DiskSnapshotState
      ( ..,
        DiskSnapshotState_Completed,
        DiskSnapshotState_Error,
        DiskSnapshotState_Pending,
        DiskSnapshotState_Unknown
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DiskSnapshotState = DiskSnapshotState'
  { fromDiskSnapshotState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern DiskSnapshotState_Completed :: DiskSnapshotState
pattern DiskSnapshotState_Completed = DiskSnapshotState' "completed"

pattern DiskSnapshotState_Error :: DiskSnapshotState
pattern DiskSnapshotState_Error = DiskSnapshotState' "error"

pattern DiskSnapshotState_Pending :: DiskSnapshotState
pattern DiskSnapshotState_Pending = DiskSnapshotState' "pending"

pattern DiskSnapshotState_Unknown :: DiskSnapshotState
pattern DiskSnapshotState_Unknown = DiskSnapshotState' "unknown"

{-# COMPLETE
  DiskSnapshotState_Completed,
  DiskSnapshotState_Error,
  DiskSnapshotState_Pending,
  DiskSnapshotState_Unknown,
  DiskSnapshotState'
  #-}
