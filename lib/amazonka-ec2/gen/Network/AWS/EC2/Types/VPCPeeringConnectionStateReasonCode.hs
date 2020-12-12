{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnectionStateReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCPeeringConnectionStateReasonCode
  ( VPCPeeringConnectionStateReasonCode
      ( VPCPeeringConnectionStateReasonCode',
        VPCSRCActive,
        VPCSRCDeleted,
        VPCSRCDeleting,
        VPCSRCExpired,
        VPCSRCFailed,
        VPCSRCInitiatingRequest,
        VPCSRCPendingAcceptance,
        VPCSRCProvisioning,
        VPCSRCRejected
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VPCPeeringConnectionStateReasonCode = VPCPeeringConnectionStateReasonCode' Lude.Text
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

pattern VPCSRCActive :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCActive = VPCPeeringConnectionStateReasonCode' "active"

pattern VPCSRCDeleted :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCDeleted = VPCPeeringConnectionStateReasonCode' "deleted"

pattern VPCSRCDeleting :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCDeleting = VPCPeeringConnectionStateReasonCode' "deleting"

pattern VPCSRCExpired :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCExpired = VPCPeeringConnectionStateReasonCode' "expired"

pattern VPCSRCFailed :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCFailed = VPCPeeringConnectionStateReasonCode' "failed"

pattern VPCSRCInitiatingRequest :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCInitiatingRequest = VPCPeeringConnectionStateReasonCode' "initiating-request"

pattern VPCSRCPendingAcceptance :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCPendingAcceptance = VPCPeeringConnectionStateReasonCode' "pending-acceptance"

pattern VPCSRCProvisioning :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCProvisioning = VPCPeeringConnectionStateReasonCode' "provisioning"

pattern VPCSRCRejected :: VPCPeeringConnectionStateReasonCode
pattern VPCSRCRejected = VPCPeeringConnectionStateReasonCode' "rejected"

{-# COMPLETE
  VPCSRCActive,
  VPCSRCDeleted,
  VPCSRCDeleting,
  VPCSRCExpired,
  VPCSRCFailed,
  VPCSRCInitiatingRequest,
  VPCSRCPendingAcceptance,
  VPCSRCProvisioning,
  VPCSRCRejected,
  VPCPeeringConnectionStateReasonCode'
  #-}
