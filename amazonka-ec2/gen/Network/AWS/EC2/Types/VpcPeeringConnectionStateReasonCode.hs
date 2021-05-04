{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcPeeringConnectionStateReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcPeeringConnectionStateReasonCode
  ( VpcPeeringConnectionStateReasonCode
      ( ..,
        VpcPeeringConnectionStateReasonCode_Active,
        VpcPeeringConnectionStateReasonCode_Deleted,
        VpcPeeringConnectionStateReasonCode_Deleting,
        VpcPeeringConnectionStateReasonCode_Expired,
        VpcPeeringConnectionStateReasonCode_Failed,
        VpcPeeringConnectionStateReasonCode_Initiating_request,
        VpcPeeringConnectionStateReasonCode_Pending_acceptance,
        VpcPeeringConnectionStateReasonCode_Provisioning,
        VpcPeeringConnectionStateReasonCode_Rejected
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype VpcPeeringConnectionStateReasonCode = VpcPeeringConnectionStateReasonCode'
  { fromVpcPeeringConnectionStateReasonCode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern VpcPeeringConnectionStateReasonCode_Active :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCode_Active = VpcPeeringConnectionStateReasonCode' "active"

pattern VpcPeeringConnectionStateReasonCode_Deleted :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCode_Deleted = VpcPeeringConnectionStateReasonCode' "deleted"

pattern VpcPeeringConnectionStateReasonCode_Deleting :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCode_Deleting = VpcPeeringConnectionStateReasonCode' "deleting"

pattern VpcPeeringConnectionStateReasonCode_Expired :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCode_Expired = VpcPeeringConnectionStateReasonCode' "expired"

pattern VpcPeeringConnectionStateReasonCode_Failed :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCode_Failed = VpcPeeringConnectionStateReasonCode' "failed"

pattern VpcPeeringConnectionStateReasonCode_Initiating_request :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCode_Initiating_request = VpcPeeringConnectionStateReasonCode' "initiating-request"

pattern VpcPeeringConnectionStateReasonCode_Pending_acceptance :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCode_Pending_acceptance = VpcPeeringConnectionStateReasonCode' "pending-acceptance"

pattern VpcPeeringConnectionStateReasonCode_Provisioning :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCode_Provisioning = VpcPeeringConnectionStateReasonCode' "provisioning"

pattern VpcPeeringConnectionStateReasonCode_Rejected :: VpcPeeringConnectionStateReasonCode
pattern VpcPeeringConnectionStateReasonCode_Rejected = VpcPeeringConnectionStateReasonCode' "rejected"

{-# COMPLETE
  VpcPeeringConnectionStateReasonCode_Active,
  VpcPeeringConnectionStateReasonCode_Deleted,
  VpcPeeringConnectionStateReasonCode_Deleting,
  VpcPeeringConnectionStateReasonCode_Expired,
  VpcPeeringConnectionStateReasonCode_Failed,
  VpcPeeringConnectionStateReasonCode_Initiating_request,
  VpcPeeringConnectionStateReasonCode_Pending_acceptance,
  VpcPeeringConnectionStateReasonCode_Provisioning,
  VpcPeeringConnectionStateReasonCode_Rejected,
  VpcPeeringConnectionStateReasonCode'
  #-}
