{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.MigrationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MigrationHub.Types.MigrationStatus
  ( MigrationStatus
    ( MigrationStatus'
    , MigrationStatusNotStarted
    , MigrationStatusInProgress
    , MigrationStatusFailed
    , MigrationStatusCompleted
    , fromMigrationStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MigrationStatus = MigrationStatus'{fromMigrationStatus ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern MigrationStatusNotStarted :: MigrationStatus
pattern MigrationStatusNotStarted = MigrationStatus' "NOT_STARTED"

pattern MigrationStatusInProgress :: MigrationStatus
pattern MigrationStatusInProgress = MigrationStatus' "IN_PROGRESS"

pattern MigrationStatusFailed :: MigrationStatus
pattern MigrationStatusFailed = MigrationStatus' "FAILED"

pattern MigrationStatusCompleted :: MigrationStatus
pattern MigrationStatusCompleted = MigrationStatus' "COMPLETED"

{-# COMPLETE 
  MigrationStatusNotStarted,

  MigrationStatusInProgress,

  MigrationStatusFailed,

  MigrationStatusCompleted,
  MigrationStatus'
  #-}
