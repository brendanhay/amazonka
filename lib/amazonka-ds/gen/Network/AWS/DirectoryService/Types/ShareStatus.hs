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
        SSDeleted,
        SSDeleting,
        SSPendingAcceptance,
        SSRejectFailed,
        SSRejected,
        SSRejecting,
        SSShareFailed,
        SSShared,
        SSSharing
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

pattern SSDeleted :: ShareStatus
pattern SSDeleted = ShareStatus' "Deleted"

pattern SSDeleting :: ShareStatus
pattern SSDeleting = ShareStatus' "Deleting"

pattern SSPendingAcceptance :: ShareStatus
pattern SSPendingAcceptance = ShareStatus' "PendingAcceptance"

pattern SSRejectFailed :: ShareStatus
pattern SSRejectFailed = ShareStatus' "RejectFailed"

pattern SSRejected :: ShareStatus
pattern SSRejected = ShareStatus' "Rejected"

pattern SSRejecting :: ShareStatus
pattern SSRejecting = ShareStatus' "Rejecting"

pattern SSShareFailed :: ShareStatus
pattern SSShareFailed = ShareStatus' "ShareFailed"

pattern SSShared :: ShareStatus
pattern SSShared = ShareStatus' "Shared"

pattern SSSharing :: ShareStatus
pattern SSSharing = ShareStatus' "Sharing"

{-# COMPLETE
  SSDeleted,
  SSDeleting,
  SSPendingAcceptance,
  SSRejectFailed,
  SSRejected,
  SSRejecting,
  SSShareFailed,
  SSShared,
  SSSharing,
  ShareStatus'
  #-}
