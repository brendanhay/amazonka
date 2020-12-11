-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.MigrationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.MigrationStatus
  ( MigrationStatus
      ( MigrationStatus',
        MSCompleted,
        MSFailed,
        MSInProgress,
        MSNotStarted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MigrationStatus = MigrationStatus' Lude.Text
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

pattern MSCompleted :: MigrationStatus
pattern MSCompleted = MigrationStatus' "COMPLETED"

pattern MSFailed :: MigrationStatus
pattern MSFailed = MigrationStatus' "FAILED"

pattern MSInProgress :: MigrationStatus
pattern MSInProgress = MigrationStatus' "IN_PROGRESS"

pattern MSNotStarted :: MigrationStatus
pattern MSNotStarted = MigrationStatus' "NOT_STARTED"

{-# COMPLETE
  MSCompleted,
  MSFailed,
  MSInProgress,
  MSNotStarted,
  MigrationStatus'
  #-}
