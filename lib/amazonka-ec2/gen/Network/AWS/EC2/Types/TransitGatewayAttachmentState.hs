{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentState
  ( TransitGatewayAttachmentState
      ( TransitGatewayAttachmentState',
        TGASAvailable,
        TGASDeleted,
        TGASDeleting,
        TGASFailed,
        TGASFailing,
        TGASInitiating,
        TGASInitiatingRequest,
        TGASModifying,
        TGASPending,
        TGASPendingAcceptance,
        TGASRejected,
        TGASRejecting,
        TGASRollingBack
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransitGatewayAttachmentState = TransitGatewayAttachmentState' Lude.Text
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

pattern TGASAvailable :: TransitGatewayAttachmentState
pattern TGASAvailable = TransitGatewayAttachmentState' "available"

pattern TGASDeleted :: TransitGatewayAttachmentState
pattern TGASDeleted = TransitGatewayAttachmentState' "deleted"

pattern TGASDeleting :: TransitGatewayAttachmentState
pattern TGASDeleting = TransitGatewayAttachmentState' "deleting"

pattern TGASFailed :: TransitGatewayAttachmentState
pattern TGASFailed = TransitGatewayAttachmentState' "failed"

pattern TGASFailing :: TransitGatewayAttachmentState
pattern TGASFailing = TransitGatewayAttachmentState' "failing"

pattern TGASInitiating :: TransitGatewayAttachmentState
pattern TGASInitiating = TransitGatewayAttachmentState' "initiating"

pattern TGASInitiatingRequest :: TransitGatewayAttachmentState
pattern TGASInitiatingRequest = TransitGatewayAttachmentState' "initiatingRequest"

pattern TGASModifying :: TransitGatewayAttachmentState
pattern TGASModifying = TransitGatewayAttachmentState' "modifying"

pattern TGASPending :: TransitGatewayAttachmentState
pattern TGASPending = TransitGatewayAttachmentState' "pending"

pattern TGASPendingAcceptance :: TransitGatewayAttachmentState
pattern TGASPendingAcceptance = TransitGatewayAttachmentState' "pendingAcceptance"

pattern TGASRejected :: TransitGatewayAttachmentState
pattern TGASRejected = TransitGatewayAttachmentState' "rejected"

pattern TGASRejecting :: TransitGatewayAttachmentState
pattern TGASRejecting = TransitGatewayAttachmentState' "rejecting"

pattern TGASRollingBack :: TransitGatewayAttachmentState
pattern TGASRollingBack = TransitGatewayAttachmentState' "rollingBack"

{-# COMPLETE
  TGASAvailable,
  TGASDeleted,
  TGASDeleting,
  TGASFailed,
  TGASFailing,
  TGASInitiating,
  TGASInitiatingRequest,
  TGASModifying,
  TGASPending,
  TGASPendingAcceptance,
  TGASRejected,
  TGASRejecting,
  TGASRollingBack,
  TransitGatewayAttachmentState'
  #-}
