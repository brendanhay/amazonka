{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
  ( WorkspaceDirectoryState
      ( WorkspaceDirectoryState',
        WorkspaceDirectoryStateRegistering,
        WorkspaceDirectoryStateRegistered,
        WorkspaceDirectoryStateDeregistering,
        WorkspaceDirectoryStateDeregistered,
        WorkspaceDirectoryStateError,
        fromWorkspaceDirectoryState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype WorkspaceDirectoryState = WorkspaceDirectoryState'
  { fromWorkspaceDirectoryState ::
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

pattern WorkspaceDirectoryStateRegistering :: WorkspaceDirectoryState
pattern WorkspaceDirectoryStateRegistering = WorkspaceDirectoryState' "REGISTERING"

pattern WorkspaceDirectoryStateRegistered :: WorkspaceDirectoryState
pattern WorkspaceDirectoryStateRegistered = WorkspaceDirectoryState' "REGISTERED"

pattern WorkspaceDirectoryStateDeregistering :: WorkspaceDirectoryState
pattern WorkspaceDirectoryStateDeregistering = WorkspaceDirectoryState' "DEREGISTERING"

pattern WorkspaceDirectoryStateDeregistered :: WorkspaceDirectoryState
pattern WorkspaceDirectoryStateDeregistered = WorkspaceDirectoryState' "DEREGISTERED"

pattern WorkspaceDirectoryStateError :: WorkspaceDirectoryState
pattern WorkspaceDirectoryStateError = WorkspaceDirectoryState' "ERROR"

{-# COMPLETE
  WorkspaceDirectoryStateRegistering,
  WorkspaceDirectoryStateRegistered,
  WorkspaceDirectoryStateDeregistering,
  WorkspaceDirectoryStateDeregistered,
  WorkspaceDirectoryStateError,
  WorkspaceDirectoryState'
  #-}
