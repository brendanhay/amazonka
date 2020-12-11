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
        FADomainJoinInfo,
        FAIAMRoleARN,
        FAVPCConfiguration,
        FAVPCConfigurationSecurityGroupIds
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

pattern FADomainJoinInfo :: FleetAttribute
pattern FADomainJoinInfo = FleetAttribute' "DOMAIN_JOIN_INFO"

pattern FAIAMRoleARN :: FleetAttribute
pattern FAIAMRoleARN = FleetAttribute' "IAM_ROLE_ARN"

pattern FAVPCConfiguration :: FleetAttribute
pattern FAVPCConfiguration = FleetAttribute' "VPC_CONFIGURATION"

pattern FAVPCConfigurationSecurityGroupIds :: FleetAttribute
pattern FAVPCConfigurationSecurityGroupIds = FleetAttribute' "VPC_CONFIGURATION_SECURITY_GROUP_IDS"

{-# COMPLETE
  FADomainJoinInfo,
  FAIAMRoleARN,
  FAVPCConfiguration,
  FAVPCConfigurationSecurityGroupIds,
  FleetAttribute'
  #-}
