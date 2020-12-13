{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetAttribute
  ( FleetAttribute
      ( FleetAttribute',
        VPCConfiguration,
        VPCConfigurationSecurityGroupIds,
        DomainJoinInfo,
        IAMRoleARN
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The fleet attribute.
newtype FleetAttribute = FleetAttribute' Lude.Text
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

pattern VPCConfiguration :: FleetAttribute
pattern VPCConfiguration = FleetAttribute' "VPC_CONFIGURATION"

pattern VPCConfigurationSecurityGroupIds :: FleetAttribute
pattern VPCConfigurationSecurityGroupIds = FleetAttribute' "VPC_CONFIGURATION_SECURITY_GROUP_IDS"

pattern DomainJoinInfo :: FleetAttribute
pattern DomainJoinInfo = FleetAttribute' "DOMAIN_JOIN_INFO"

pattern IAMRoleARN :: FleetAttribute
pattern IAMRoleARN = FleetAttribute' "IAM_ROLE_ARN"

{-# COMPLETE
  VPCConfiguration,
  VPCConfigurationSecurityGroupIds,
  DomainJoinInfo,
  IAMRoleARN,
  FleetAttribute'
  #-}
