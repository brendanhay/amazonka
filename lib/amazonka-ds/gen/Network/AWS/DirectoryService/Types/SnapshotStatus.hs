{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SnapshotStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.SnapshotStatus
  ( SnapshotStatus
    ( SnapshotStatus'
    , SnapshotStatusCreating
    , SnapshotStatusCompleted
    , SnapshotStatusFailed
    , fromSnapshotStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SnapshotStatus = SnapshotStatus'{fromSnapshotStatus ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern SnapshotStatusCreating :: SnapshotStatus
pattern SnapshotStatusCreating = SnapshotStatus' "Creating"

pattern SnapshotStatusCompleted :: SnapshotStatus
pattern SnapshotStatusCompleted = SnapshotStatus' "Completed"

pattern SnapshotStatusFailed :: SnapshotStatus
pattern SnapshotStatusFailed = SnapshotStatus' "Failed"

{-# COMPLETE 
  SnapshotStatusCreating,

  SnapshotStatusCompleted,

  SnapshotStatusFailed,
  SnapshotStatus'
  #-}
