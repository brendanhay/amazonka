{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcPeeringConnectionStateReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcPeeringConnectionStateReasonCode
  ( VpcPeeringConnectionStateReasonCode
    ( VpcPeeringConnectionStateReasonCode'
    , VpcPeeringConnectionStateReasonCodeInitiatingRequest
    , VpcPeeringConnectionStateReasonCodePendingAcceptance
    , VpcPeeringConnectionStateReasonCodeActive
    , VpcPeeringConnectionStateReasonCodeDeleted
    , VpcPeeringConnectionStateReasonCodeRejected
    , VpcPeeringConnectionStateReasonCodeFailed
    , VpcPeeringConnectionStateReasonCodeExpired
    , VpcPeeringConnectionStateReasonCodeProvisioning
    , VpcPeeringConnectionStateReasonCodeDeleting
    , fromVpcPeeringConnectionStateReasonCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype VpcPeeringConnectionStateReasonCode = VpcPeeringConnectionStateReasonCode'{fromVpcPeeringConnectionStateReasonCode
                                                                                   :: Core.Text}
                                                deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                Core.Show, Core.Generic)
                                                deriving newtype (Core.IsString, Core.Hashable,
                                                                  Core.NFData, Core.ToJSONKey,
                                                                  Core.FromJSONKey, Core.ToJSON,
                                                                  Core.FromJSON, Core.ToXML,
                                                                  Core.FromXML, Core.ToText,
                                                                  Core.FromText, Core.ToByteString,
                                                                  Core.ToQuery, Core.ToHeader)

pattern VpcPeeringConnectionStateReasonCodeInitiatingRequest :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCodeInitiatingRequest = VpcPeeringConnectionStateReasonCode' "initiating-request"

pattern VpcPeeringConnectionStateReasonCodePendingAcceptance :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCodePendingAcceptance = VpcPeeringConnectionStateReasonCode' "pending-acceptance"

pattern VpcPeeringConnectionStateReasonCodeActive :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCodeActive = VpcPeeringConnectionStateReasonCode' "active"

pattern VpcPeeringConnectionStateReasonCodeDeleted :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCodeDeleted = VpcPeeringConnectionStateReasonCode' "deleted"

pattern VpcPeeringConnectionStateReasonCodeRejected :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCodeRejected = VpcPeeringConnectionStateReasonCode' "rejected"

pattern VpcPeeringConnectionStateReasonCodeFailed :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCodeFailed = VpcPeeringConnectionStateReasonCode' "failed"

pattern VpcPeeringConnectionStateReasonCodeExpired :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCodeExpired = VpcPeeringConnectionStateReasonCode' "expired"

pattern VpcPeeringConnectionStateReasonCodeProvisioning :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCodeProvisioning = VpcPeeringConnectionStateReasonCode' "provisioning"

pattern VpcPeeringConnectionStateReasonCodeDeleting :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCodeDeleting = VpcPeeringConnectionStateReasonCode' "deleting"

{-# COMPLETE 
  VpcPeeringConnectionStateReasonCodeInitiatingRequest,

  VpcPeeringConnectionStateReasonCodePendingAcceptance,

  VpcPeeringConnectionStateReasonCodeActive,

  VpcPeeringConnectionStateReasonCodeDeleted,

  VpcPeeringConnectionStateReasonCodeRejected,

  VpcPeeringConnectionStateReasonCodeFailed,

  VpcPeeringConnectionStateReasonCodeExpired,

  VpcPeeringConnectionStateReasonCodeProvisioning,

  VpcPeeringConnectionStateReasonCodeDeleting,
  VpcPeeringConnectionStateReasonCode'
  #-}
