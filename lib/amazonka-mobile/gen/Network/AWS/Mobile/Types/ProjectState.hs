{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.ProjectState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.ProjectState
  ( ProjectState
      ( ProjectState',
        ProjectStateNormal,
        ProjectStateSyncing,
        ProjectStateImporting,
        fromProjectState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Synchronization state for a project.
newtype ProjectState = ProjectState' {fromProjectState :: Core.Text}
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

pattern ProjectStateNormal :: ProjectState
pattern ProjectStateNormal = ProjectState' "NORMAL"

pattern ProjectStateSyncing :: ProjectState
pattern ProjectStateSyncing = ProjectState' "SYNCING"

pattern ProjectStateImporting :: ProjectState
pattern ProjectStateImporting = ProjectState' "IMPORTING"

{-# COMPLETE
  ProjectStateNormal,
  ProjectStateSyncing,
  ProjectStateImporting,
  ProjectState'
  #-}
