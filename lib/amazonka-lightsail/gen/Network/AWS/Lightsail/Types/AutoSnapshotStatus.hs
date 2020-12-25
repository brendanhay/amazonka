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
        AutoSnapshotStatusSuccess,
        AutoSnapshotStatusFailed,
        AutoSnapshotStatusInProgress,
        AutoSnapshotStatusNotFound,
        fromAutoSnapshotStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AutoSnapshotStatus = AutoSnapshotStatus'
  { fromAutoSnapshotStatus ::
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

pattern AutoSnapshotStatusSuccess :: AutoSnapshotStatus
pattern AutoSnapshotStatusSuccess = AutoSnapshotStatus' "Success"

pattern AutoSnapshotStatusFailed :: AutoSnapshotStatus
pattern AutoSnapshotStatusFailed = AutoSnapshotStatus' "Failed"

pattern AutoSnapshotStatusInProgress :: AutoSnapshotStatus
pattern AutoSnapshotStatusInProgress = AutoSnapshotStatus' "InProgress"

pattern AutoSnapshotStatusNotFound :: AutoSnapshotStatus
pattern AutoSnapshotStatusNotFound = AutoSnapshotStatus' "NotFound"

{-# COMPLETE
  AutoSnapshotStatusSuccess,
  AutoSnapshotStatusFailed,
  AutoSnapshotStatusInProgress,
  AutoSnapshotStatusNotFound,
  AutoSnapshotStatus'
  #-}
