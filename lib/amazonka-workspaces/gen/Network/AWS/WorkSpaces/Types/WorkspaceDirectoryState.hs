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
        Registering,
        Registered,
        Deregistering,
        Deregistered,
        Error
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

pattern Registering :: WorkspaceDirectoryState
pattern Registering = WorkspaceDirectoryState' "REGISTERING"

pattern Registered :: WorkspaceDirectoryState
pattern Registered = WorkspaceDirectoryState' "REGISTERED"

pattern Deregistering :: WorkspaceDirectoryState
pattern Deregistering = WorkspaceDirectoryState' "DEREGISTERING"

pattern Deregistered :: WorkspaceDirectoryState
pattern Deregistered = WorkspaceDirectoryState' "DEREGISTERED"

pattern Error :: WorkspaceDirectoryState
pattern Error = WorkspaceDirectoryState' "ERROR"

{-# COMPLETE
  Registering,
  Registered,
  Deregistering,
  Deregistered,
  Error,
  WorkspaceDirectoryState'
  #-}
