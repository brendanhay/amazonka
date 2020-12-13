{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ShareStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ShareStatus
  ( ShareStatus
      ( ShareStatus',
        SSShared,
        SSPendingAcceptance,
        SSRejected,
        SSRejecting,
        SSRejectFailed,
        SSSharing,
        SSShareFailed,
        SSDeleted,
        SSDeleting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ShareStatus = ShareStatus' Lude.Text
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

pattern SSShared :: ShareStatus
pattern SSShared = ShareStatus' "Shared"

pattern SSPendingAcceptance :: ShareStatus
pattern SSPendingAcceptance = ShareStatus' "PendingAcceptance"

pattern SSRejected :: ShareStatus
pattern SSRejected = ShareStatus' "Rejected"

pattern SSRejecting :: ShareStatus
pattern SSRejecting = ShareStatus' "Rejecting"

pattern SSRejectFailed :: ShareStatus
pattern SSRejectFailed = ShareStatus' "RejectFailed"

pattern SSSharing :: ShareStatus
pattern SSSharing = ShareStatus' "Sharing"

pattern SSShareFailed :: ShareStatus
pattern SSShareFailed = ShareStatus' "ShareFailed"

pattern SSDeleted :: ShareStatus
pattern SSDeleted = ShareStatus' "Deleted"

pattern SSDeleting :: ShareStatus
pattern SSDeleting = ShareStatus' "Deleting"

{-# COMPLETE
  SSShared,
  SSPendingAcceptance,
  SSRejected,
  SSRejecting,
  SSRejectFailed,
  SSSharing,
  SSShareFailed,
  SSDeleted,
  SSDeleting,
  ShareStatus'
  #-}
