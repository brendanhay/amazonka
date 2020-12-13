{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.ArchiveState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ArchiveState
  ( ArchiveState
      ( ArchiveState',
        ASEnabled,
        ASDisabled,
        ASCreating,
        ASUpdating,
        ASCreateFailed,
        ASUpdateFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ArchiveState = ArchiveState' Lude.Text
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

pattern ASEnabled :: ArchiveState
pattern ASEnabled = ArchiveState' "ENABLED"

pattern ASDisabled :: ArchiveState
pattern ASDisabled = ArchiveState' "DISABLED"

pattern ASCreating :: ArchiveState
pattern ASCreating = ArchiveState' "CREATING"

pattern ASUpdating :: ArchiveState
pattern ASUpdating = ArchiveState' "UPDATING"

pattern ASCreateFailed :: ArchiveState
pattern ASCreateFailed = ArchiveState' "CREATE_FAILED"

pattern ASUpdateFailed :: ArchiveState
pattern ASUpdateFailed = ArchiveState' "UPDATE_FAILED"

{-# COMPLETE
  ASEnabled,
  ASDisabled,
  ASCreating,
  ASUpdating,
  ASCreateFailed,
  ASUpdateFailed,
  ArchiveState'
  #-}
