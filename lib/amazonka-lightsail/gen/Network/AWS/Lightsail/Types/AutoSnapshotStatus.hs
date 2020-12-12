{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AutoSnapshotStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AutoSnapshotStatus
  ( AutoSnapshotStatus
      ( AutoSnapshotStatus',
        ASSFailed,
        ASSInProgress,
        ASSNotFound,
        ASSSuccess
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutoSnapshotStatus = AutoSnapshotStatus' Lude.Text
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

pattern ASSFailed :: AutoSnapshotStatus
pattern ASSFailed = AutoSnapshotStatus' "Failed"

pattern ASSInProgress :: AutoSnapshotStatus
pattern ASSInProgress = AutoSnapshotStatus' "InProgress"

pattern ASSNotFound :: AutoSnapshotStatus
pattern ASSNotFound = AutoSnapshotStatus' "NotFound"

pattern ASSSuccess :: AutoSnapshotStatus
pattern ASSSuccess = AutoSnapshotStatus' "Success"

{-# COMPLETE
  ASSFailed,
  ASSInProgress,
  ASSNotFound,
  ASSSuccess,
  AutoSnapshotStatus'
  #-}
