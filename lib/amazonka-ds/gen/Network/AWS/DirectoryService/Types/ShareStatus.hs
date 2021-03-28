{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ShareStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.ShareStatus
  ( ShareStatus
    ( ShareStatus'
    , ShareStatusShared
    , ShareStatusPendingAcceptance
    , ShareStatusRejected
    , ShareStatusRejecting
    , ShareStatusRejectFailed
    , ShareStatusSharing
    , ShareStatusShareFailed
    , ShareStatusDeleted
    , ShareStatusDeleting
    , fromShareStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ShareStatus = ShareStatus'{fromShareStatus :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern ShareStatusShared :: ShareStatus
pattern ShareStatusShared = ShareStatus' "Shared"

pattern ShareStatusPendingAcceptance :: ShareStatus
pattern ShareStatusPendingAcceptance = ShareStatus' "PendingAcceptance"

pattern ShareStatusRejected :: ShareStatus
pattern ShareStatusRejected = ShareStatus' "Rejected"

pattern ShareStatusRejecting :: ShareStatus
pattern ShareStatusRejecting = ShareStatus' "Rejecting"

pattern ShareStatusRejectFailed :: ShareStatus
pattern ShareStatusRejectFailed = ShareStatus' "RejectFailed"

pattern ShareStatusSharing :: ShareStatus
pattern ShareStatusSharing = ShareStatus' "Sharing"

pattern ShareStatusShareFailed :: ShareStatus
pattern ShareStatusShareFailed = ShareStatus' "ShareFailed"

pattern ShareStatusDeleted :: ShareStatus
pattern ShareStatusDeleted = ShareStatus' "Deleted"

pattern ShareStatusDeleting :: ShareStatus
pattern ShareStatusDeleting = ShareStatus' "Deleting"

{-# COMPLETE 
  ShareStatusShared,

  ShareStatusPendingAcceptance,

  ShareStatusRejected,

  ShareStatusRejecting,

  ShareStatusRejectFailed,

  ShareStatusSharing,

  ShareStatusShareFailed,

  ShareStatusDeleted,

  ShareStatusDeleting,
  ShareStatus'
  #-}
