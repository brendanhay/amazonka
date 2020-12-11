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
        Importing,
        Normal,
        Syncing
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Synchronization state for a project.
newtype ProjectState = ProjectState' Lude.Text
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

pattern Importing :: ProjectState
pattern Importing = ProjectState' "IMPORTING"

pattern Normal :: ProjectState
pattern Normal = ProjectState' "NORMAL"

pattern Syncing :: ProjectState
pattern Syncing = ProjectState' "SYNCING"

{-# COMPLETE
  Importing,
  Normal,
  Syncing,
  ProjectState'
  #-}
