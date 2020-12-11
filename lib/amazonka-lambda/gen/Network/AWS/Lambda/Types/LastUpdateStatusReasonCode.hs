-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
  ( LastUpdateStatusReasonCode
      ( LastUpdateStatusReasonCode',
        LUSRCEniLimitExceeded,
        LUSRCInsufficientRolePermissions,
        LUSRCInternalError,
        LUSRCInvalidConfiguration,
        LUSRCInvalidSecurityGroup,
        LUSRCInvalidSubnet,
        LUSRCSubnetOutOfIPAddresses
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LastUpdateStatusReasonCode = LastUpdateStatusReasonCode' Lude.Text
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

pattern LUSRCEniLimitExceeded :: LastUpdateStatusReasonCode
pattern LUSRCEniLimitExceeded = LastUpdateStatusReasonCode' "EniLimitExceeded"

pattern LUSRCInsufficientRolePermissions :: LastUpdateStatusReasonCode
pattern LUSRCInsufficientRolePermissions = LastUpdateStatusReasonCode' "InsufficientRolePermissions"

pattern LUSRCInternalError :: LastUpdateStatusReasonCode
pattern LUSRCInternalError = LastUpdateStatusReasonCode' "InternalError"

pattern LUSRCInvalidConfiguration :: LastUpdateStatusReasonCode
pattern LUSRCInvalidConfiguration = LastUpdateStatusReasonCode' "InvalidConfiguration"

pattern LUSRCInvalidSecurityGroup :: LastUpdateStatusReasonCode
pattern LUSRCInvalidSecurityGroup = LastUpdateStatusReasonCode' "InvalidSecurityGroup"

pattern LUSRCInvalidSubnet :: LastUpdateStatusReasonCode
pattern LUSRCInvalidSubnet = LastUpdateStatusReasonCode' "InvalidSubnet"

pattern LUSRCSubnetOutOfIPAddresses :: LastUpdateStatusReasonCode
pattern LUSRCSubnetOutOfIPAddresses = LastUpdateStatusReasonCode' "SubnetOutOfIPAddresses"

{-# COMPLETE
  LUSRCEniLimitExceeded,
  LUSRCInsufficientRolePermissions,
  LUSRCInternalError,
  LUSRCInvalidConfiguration,
  LUSRCInvalidSecurityGroup,
  LUSRCInvalidSubnet,
  LUSRCSubnetOutOfIPAddresses,
  LastUpdateStatusReasonCode'
  #-}
