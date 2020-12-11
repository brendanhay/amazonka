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
        Deregistered,
        Deregistering,
        Error,
        Registered,
        Registering
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype WorkspaceDirectoryState = WorkspaceDirectoryState' Lude.Text
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

pattern Deregistered :: WorkspaceDirectoryState
pattern Deregistered = WorkspaceDirectoryState' "DEREGISTERED"

pattern Deregistering :: WorkspaceDirectoryState
pattern Deregistering = WorkspaceDirectoryState' "DEREGISTERING"

pattern Error :: WorkspaceDirectoryState
pattern Error = WorkspaceDirectoryState' "ERROR"

pattern Registered :: WorkspaceDirectoryState
pattern Registered = WorkspaceDirectoryState' "REGISTERED"

pattern Registering :: WorkspaceDirectoryState
pattern Registering = WorkspaceDirectoryState' "REGISTERING"

{-# COMPLETE
  Deregistered,
  Deregistering,
  Error,
  Registered,
  Registering,
  WorkspaceDirectoryState'
  #-}
