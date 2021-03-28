{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
  ( LastUpdateStatusReasonCode
    ( LastUpdateStatusReasonCode'
    , LastUpdateStatusReasonCodeEniLimitExceeded
    , LastUpdateStatusReasonCodeInsufficientRolePermissions
    , LastUpdateStatusReasonCodeInvalidConfiguration
    , LastUpdateStatusReasonCodeInternalError
    , LastUpdateStatusReasonCodeSubnetOutOfIPAddresses
    , LastUpdateStatusReasonCodeInvalidSubnet
    , LastUpdateStatusReasonCodeInvalidSecurityGroup
    , fromLastUpdateStatusReasonCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LastUpdateStatusReasonCode = LastUpdateStatusReasonCode'{fromLastUpdateStatusReasonCode
                                                                 :: Core.Text}
                                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                       Core.Generic)
                                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                         Core.ToJSONKey, Core.FromJSONKey,
                                                         Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                         Core.FromXML, Core.ToText, Core.FromText,
                                                         Core.ToByteString, Core.ToQuery,
                                                         Core.ToHeader)

pattern LastUpdateStatusReasonCodeEniLimitExceeded :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCodeEniLimitExceeded = LastUpdateStatusReasonCode' "EniLimitExceeded"

pattern LastUpdateStatusReasonCodeInsufficientRolePermissions :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCodeInsufficientRolePermissions = LastUpdateStatusReasonCode' "InsufficientRolePermissions"

pattern LastUpdateStatusReasonCodeInvalidConfiguration :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCodeInvalidConfiguration = LastUpdateStatusReasonCode' "InvalidConfiguration"

pattern LastUpdateStatusReasonCodeInternalError :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCodeInternalError = LastUpdateStatusReasonCode' "InternalError"

pattern LastUpdateStatusReasonCodeSubnetOutOfIPAddresses :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCodeSubnetOutOfIPAddresses = LastUpdateStatusReasonCode' "SubnetOutOfIPAddresses"

pattern LastUpdateStatusReasonCodeInvalidSubnet :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCodeInvalidSubnet = LastUpdateStatusReasonCode' "InvalidSubnet"

pattern LastUpdateStatusReasonCodeInvalidSecurityGroup :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCodeInvalidSecurityGroup = LastUpdateStatusReasonCode' "InvalidSecurityGroup"

{-# COMPLETE 
  LastUpdateStatusReasonCodeEniLimitExceeded,

  LastUpdateStatusReasonCodeInsufficientRolePermissions,

  LastUpdateStatusReasonCodeInvalidConfiguration,

  LastUpdateStatusReasonCodeInternalError,

  LastUpdateStatusReasonCodeSubnetOutOfIPAddresses,

  LastUpdateStatusReasonCodeInvalidSubnet,

  LastUpdateStatusReasonCodeInvalidSecurityGroup,
  LastUpdateStatusReasonCode'
  #-}
