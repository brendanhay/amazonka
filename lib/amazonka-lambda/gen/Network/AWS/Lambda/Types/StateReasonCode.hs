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
        Creating,
        EniLimitExceeded,
        Idle,
        InsufficientRolePermissions,
        InternalError,
        InvalidConfiguration,
        InvalidSecurityGroup,
        InvalidSubnet,
        Restoring,
        SubnetOutOfIPAddresses
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

pattern Creating :: StateReasonCode
pattern Creating = StateReasonCode' "Creating"

pattern EniLimitExceeded :: StateReasonCode
pattern EniLimitExceeded = StateReasonCode' "EniLimitExceeded"

pattern Idle :: StateReasonCode
pattern Idle = StateReasonCode' "Idle"

pattern InsufficientRolePermissions :: StateReasonCode
pattern InsufficientRolePermissions = StateReasonCode' "InsufficientRolePermissions"

pattern InternalError :: StateReasonCode
pattern InternalError = StateReasonCode' "InternalError"

pattern InvalidConfiguration :: StateReasonCode
pattern InvalidConfiguration = StateReasonCode' "InvalidConfiguration"

pattern InvalidSecurityGroup :: StateReasonCode
pattern InvalidSecurityGroup = StateReasonCode' "InvalidSecurityGroup"

pattern InvalidSubnet :: StateReasonCode
pattern InvalidSubnet = StateReasonCode' "InvalidSubnet"

pattern Restoring :: StateReasonCode
pattern Restoring = StateReasonCode' "Restoring"

pattern SubnetOutOfIPAddresses :: StateReasonCode
pattern SubnetOutOfIPAddresses = StateReasonCode' "SubnetOutOfIPAddresses"

{-# COMPLETE
  Creating,
  EniLimitExceeded,
  Idle,
  InsufficientRolePermissions,
  InternalError,
  InvalidConfiguration,
  InvalidSecurityGroup,
  InvalidSubnet,
  Restoring,
  SubnetOutOfIPAddresses,
  StateReasonCode'
  #-}
