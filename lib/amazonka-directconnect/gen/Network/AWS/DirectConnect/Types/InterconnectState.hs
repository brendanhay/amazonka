{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.InterconnectState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.InterconnectState
  ( InterconnectState
      ( InterconnectState',
        ISRequested,
        ISPending,
        ISAvailable,
        ISDown,
        ISDeleting,
        ISDeleted,
        ISUnknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InterconnectState = InterconnectState' Lude.Text
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

pattern ISRequested :: InterconnectState
pattern ISRequested = InterconnectState' "requested"

pattern ISPending :: InterconnectState
pattern ISPending = InterconnectState' "pending"

pattern ISAvailable :: InterconnectState
pattern ISAvailable = InterconnectState' "available"

pattern ISDown :: InterconnectState
pattern ISDown = InterconnectState' "down"

pattern ISDeleting :: InterconnectState
pattern ISDeleting = InterconnectState' "deleting"

pattern ISDeleted :: InterconnectState
pattern ISDeleted = InterconnectState' "deleted"

pattern ISUnknown :: InterconnectState
pattern ISUnknown = InterconnectState' "unknown"

{-# COMPLETE
  ISRequested,
  ISPending,
  ISAvailable,
  ISDown,
  ISDeleting,
  ISDeleted,
  ISUnknown,
  InterconnectState'
  #-}
