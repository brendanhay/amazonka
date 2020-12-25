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
        ArchiveStateEnabled,
        ArchiveStateDisabled,
        ArchiveStateCreating,
        ArchiveStateUpdating,
        ArchiveStateCreateFailed,
        ArchiveStateUpdateFailed,
        fromArchiveState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ArchiveState = ArchiveState' {fromArchiveState :: Core.Text}
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

pattern ArchiveStateEnabled :: ArchiveState
pattern ArchiveStateEnabled = ArchiveState' "ENABLED"

pattern ArchiveStateDisabled :: ArchiveState
pattern ArchiveStateDisabled = ArchiveState' "DISABLED"

pattern ArchiveStateCreating :: ArchiveState
pattern ArchiveStateCreating = ArchiveState' "CREATING"

pattern ArchiveStateUpdating :: ArchiveState
pattern ArchiveStateUpdating = ArchiveState' "UPDATING"

pattern ArchiveStateCreateFailed :: ArchiveState
pattern ArchiveStateCreateFailed = ArchiveState' "CREATE_FAILED"

pattern ArchiveStateUpdateFailed :: ArchiveState
pattern ArchiveStateUpdateFailed = ArchiveState' "UPDATE_FAILED"

{-# COMPLETE
  ArchiveStateEnabled,
  ArchiveStateDisabled,
  ArchiveStateCreating,
  ArchiveStateUpdating,
  ArchiveStateCreateFailed,
  ArchiveStateUpdateFailed,
  ArchiveState'
  #-}
