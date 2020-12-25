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
        InterconnectStateRequested,
        InterconnectStatePending,
        InterconnectStateAvailable,
        InterconnectStateDown,
        InterconnectStateDeleting,
        InterconnectStateDeleted,
        InterconnectStateUnknown,
        fromInterconnectState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype InterconnectState = InterconnectState'
  { fromInterconnectState ::
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

pattern InterconnectStateRequested :: InterconnectState
pattern InterconnectStateRequested = InterconnectState' "requested"

pattern InterconnectStatePending :: InterconnectState
pattern InterconnectStatePending = InterconnectState' "pending"

pattern InterconnectStateAvailable :: InterconnectState
pattern InterconnectStateAvailable = InterconnectState' "available"

pattern InterconnectStateDown :: InterconnectState
pattern InterconnectStateDown = InterconnectState' "down"

pattern InterconnectStateDeleting :: InterconnectState
pattern InterconnectStateDeleting = InterconnectState' "deleting"

pattern InterconnectStateDeleted :: InterconnectState
pattern InterconnectStateDeleted = InterconnectState' "deleted"

pattern InterconnectStateUnknown :: InterconnectState
pattern InterconnectStateUnknown = InterconnectState' "unknown"

{-# COMPLETE
  InterconnectStateRequested,
  InterconnectStatePending,
  InterconnectStateAvailable,
  InterconnectStateDown,
  InterconnectStateDeleting,
  InterconnectStateDeleted,
  InterconnectStateUnknown,
  InterconnectState'
  #-}
