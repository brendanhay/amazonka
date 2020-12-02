{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetAttribute where

import Network.AWS.Prelude

-- | The fleet attribute.
data FleetAttribute
  = FADomainJoinInfo
  | FAIAMRoleARN
  | FAVPCConfiguration
  | FAVPCConfigurationSecurityGroupIds
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText FleetAttribute where
  parser =
    takeLowerText >>= \case
      "domain_join_info" -> pure FADomainJoinInfo
      "iam_role_arn" -> pure FAIAMRoleARN
      "vpc_configuration" -> pure FAVPCConfiguration
      "vpc_configuration_security_group_ids" -> pure FAVPCConfigurationSecurityGroupIds
      e ->
        fromTextError $
          "Failure parsing FleetAttribute from value: '" <> e
            <> "'. Accepted values: domain_join_info, iam_role_arn, vpc_configuration, vpc_configuration_security_group_ids"

instance ToText FleetAttribute where
  toText = \case
    FADomainJoinInfo -> "DOMAIN_JOIN_INFO"
    FAIAMRoleARN -> "IAM_ROLE_ARN"
    FAVPCConfiguration -> "VPC_CONFIGURATION"
    FAVPCConfigurationSecurityGroupIds -> "VPC_CONFIGURATION_SECURITY_GROUP_IDS"

instance Hashable FleetAttribute

instance NFData FleetAttribute

instance ToByteString FleetAttribute

instance ToQuery FleetAttribute

instance ToHeader FleetAttribute

instance ToJSON FleetAttribute where
  toJSON = toJSONText
