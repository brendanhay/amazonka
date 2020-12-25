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
        FleetAttributeVpcConfiguration,
        FleetAttributeVpcConfigurationSecurityGroupIds,
        FleetAttributeDomainJoinInfo,
        FleetAttributeIamRoleArn,
        fromFleetAttribute
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The fleet attribute.
newtype FleetAttribute = FleetAttribute'
  { fromFleetAttribute ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern FleetAttributeVpcConfiguration :: FleetAttribute
pattern FleetAttributeVpcConfiguration = FleetAttribute' "VPC_CONFIGURATION"

pattern FleetAttributeVpcConfigurationSecurityGroupIds :: FleetAttribute
pattern FleetAttributeVpcConfigurationSecurityGroupIds = FleetAttribute' "VPC_CONFIGURATION_SECURITY_GROUP_IDS"

pattern FleetAttributeDomainJoinInfo :: FleetAttribute
pattern FleetAttributeDomainJoinInfo = FleetAttribute' "DOMAIN_JOIN_INFO"

pattern FleetAttributeIamRoleArn :: FleetAttribute
pattern FleetAttributeIamRoleArn = FleetAttribute' "IAM_ROLE_ARN"

{-# COMPLETE
  FleetAttributeVpcConfiguration,
  FleetAttributeVpcConfigurationSecurityGroupIds,
  FleetAttributeDomainJoinInfo,
  FleetAttributeIamRoleArn,
  FleetAttribute'
  #-}
