{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.StateReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.StateReasonCode
  ( StateReasonCode
      ( StateReasonCode',
        Idle,
        Creating,
        Restoring,
        EniLimitExceeded,
        InsufficientRolePermissions,
        InvalidConfiguration,
        InternalError,
        SubnetOutOfIPAddresses,
        InvalidSubnet,
        InvalidSecurityGroup
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StateReasonCode = StateReasonCode' Lude.Text
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

pattern Idle :: StateReasonCode
pattern Idle = StateReasonCode' "Idle"

pattern Creating :: StateReasonCode
pattern Creating = StateReasonCode' "Creating"

pattern Restoring :: StateReasonCode
pattern Restoring = StateReasonCode' "Restoring"

pattern EniLimitExceeded :: StateReasonCode
pattern EniLimitExceeded = StateReasonCode' "EniLimitExceeded"

pattern InsufficientRolePermissions :: StateReasonCode
pattern InsufficientRolePermissions = StateReasonCode' "InsufficientRolePermissions"

pattern InvalidConfiguration :: StateReasonCode
pattern InvalidConfiguration = StateReasonCode' "InvalidConfiguration"

pattern InternalError :: StateReasonCode
pattern InternalError = StateReasonCode' "InternalError"

pattern SubnetOutOfIPAddresses :: StateReasonCode
pattern SubnetOutOfIPAddresses = StateReasonCode' "SubnetOutOfIPAddresses"

pattern InvalidSubnet :: StateReasonCode
pattern InvalidSubnet = StateReasonCode' "InvalidSubnet"

pattern InvalidSecurityGroup :: StateReasonCode
pattern InvalidSecurityGroup = StateReasonCode' "InvalidSecurityGroup"

{-# COMPLETE
  Idle,
  Creating,
  Restoring,
  EniLimitExceeded,
  InsufficientRolePermissions,
  InvalidConfiguration,
  InternalError,
  SubnetOutOfIPAddresses,
  InvalidSubnet,
  InvalidSecurityGroup,
  StateReasonCode'
  #-}
