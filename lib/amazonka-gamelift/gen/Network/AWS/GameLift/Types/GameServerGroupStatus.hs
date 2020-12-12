{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupStatus
  ( GameServerGroupStatus
      ( GameServerGroupStatus',
        GSGSActivating,
        GSGSActive,
        GSGSDeleteScheduled,
        GSGSDeleted,
        GSGSDeleting,
        GSGSError,
        GSGSNew
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GameServerGroupStatus = GameServerGroupStatus' Lude.Text
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

pattern GSGSActivating :: GameServerGroupStatus
pattern GSGSActivating = GameServerGroupStatus' "ACTIVATING"

pattern GSGSActive :: GameServerGroupStatus
pattern GSGSActive = GameServerGroupStatus' "ACTIVE"

pattern GSGSDeleteScheduled :: GameServerGroupStatus
pattern GSGSDeleteScheduled = GameServerGroupStatus' "DELETE_SCHEDULED"

pattern GSGSDeleted :: GameServerGroupStatus
pattern GSGSDeleted = GameServerGroupStatus' "DELETED"

pattern GSGSDeleting :: GameServerGroupStatus
pattern GSGSDeleting = GameServerGroupStatus' "DELETING"

pattern GSGSError :: GameServerGroupStatus
pattern GSGSError = GameServerGroupStatus' "ERROR"

pattern GSGSNew :: GameServerGroupStatus
pattern GSGSNew = GameServerGroupStatus' "NEW"

{-# COMPLETE
  GSGSActivating,
  GSGSActive,
  GSGSDeleteScheduled,
  GSGSDeleted,
  GSGSDeleting,
  GSGSError,
  GSGSNew,
  GameServerGroupStatus'
  #-}
