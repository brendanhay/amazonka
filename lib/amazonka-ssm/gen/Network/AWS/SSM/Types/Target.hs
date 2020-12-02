{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Target where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An array of search criteria that targets instances using a Key,Value combination that you specify.
--
--
-- Supported formats include the following.
--
--     * @Key=InstanceIds,Values=/instance-id-1/ ,/instance-id-2/ ,/instance-id-3/ @
--
--     * @Key=tag:/my-tag-key/ ,Values=/my-tag-value-1/ ,/my-tag-value-2/ @
--
--     * @Key=tag-key,Values=/my-tag-key-1/ ,/my-tag-key-2/ @
--
--     * __Run Command and Maintenance window targets only__ : @Key=resource-groups:Name,Values=/resource-group-name/ @
--
--     * __Maintenance window targets only__ : @Key=resource-groups:ResourceTypeFilters,Values=/resource-type-1/ ,/resource-type-2/ @
--
--     * __Automation targets only__ : @Key=ResourceGroup;Values=/resource-group-name/ @
--
--
--
-- For example:
--
--     * @Key=InstanceIds,Values=i-02573cafcfEXAMPLE,i-0471e04240EXAMPLE,i-07782c72faEXAMPLE@
--
--     * @Key=tag:CostCenter,Values=CostCenter1,CostCenter2,CostCenter3@
--
--     * @Key=tag-key,Values=Name,Instance-Type,CostCenter@
--
--     * __Run Command and Maintenance window targets only__ : @Key=resource-groups:Name,Values=ProductionResourceGroup@
--
-- This example demonstrates how to target all resources in the resource group __ProductionResourceGroup__ in your maintenance window.
--
--     * __Maintenance window targets only__ : @Key=resource-groups:ResourceTypeFilters,Values=/AWS::EC2::INSTANCE/ ,/AWS::EC2::VPC/ @
--
-- This example demonstrates how to target only EC2 instances and VPCs in your maintenance window.
--
--     * __Automation targets only__ : @Key=ResourceGroup,Values=MyResourceGroup@
--
--     * __State Manager association targets only__ : @Key=InstanceIds,Values=/*/ @
--
-- This example demonstrates how to target all managed instances in the AWS Region where the association was created.
--
--
--
-- For more information about how to send commands that target instances using @Key,Value@ parameters, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-targeting Targeting multiple instances> in the /AWS Systems Manager User Guide/ .
--
--
-- /See:/ 'target' smart constructor.
data Target = Target'
  { _tValues :: !(Maybe [Text]),
    _tKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tValues' - User-defined criteria that maps to @Key@ . For example, if you specified @tag:ServerRole@ , you could specify @value:WebServer@ to run a command on instances that include EC2 tags of @ServerRole,WebServer@ .
--
-- * 'tKey' - User-defined criteria for sending commands that target instances that meet the criteria.
target ::
  Target
target = Target' {_tValues = Nothing, _tKey = Nothing}

-- | User-defined criteria that maps to @Key@ . For example, if you specified @tag:ServerRole@ , you could specify @value:WebServer@ to run a command on instances that include EC2 tags of @ServerRole,WebServer@ .
tValues :: Lens' Target [Text]
tValues = lens _tValues (\s a -> s {_tValues = a}) . _Default . _Coerce

-- | User-defined criteria for sending commands that target instances that meet the criteria.
tKey :: Lens' Target (Maybe Text)
tKey = lens _tKey (\s a -> s {_tKey = a})

instance FromJSON Target where
  parseJSON =
    withObject
      "Target"
      (\x -> Target' <$> (x .:? "Values" .!= mempty) <*> (x .:? "Key"))

instance Hashable Target

instance NFData Target

instance ToJSON Target where
  toJSON Target' {..} =
    object
      (catMaybes [("Values" .=) <$> _tValues, ("Key" .=) <$> _tKey])
