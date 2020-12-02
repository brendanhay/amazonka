{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EC2SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EC2SecurityGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as a response element in the following actions:
--
--
--     * @AuthorizeDBSecurityGroupIngress@
--
--     * @DescribeDBSecurityGroups@
--
--     * @RevokeDBSecurityGroupIngress@
--
--
--
--
-- /See:/ 'ec2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
  { _esgStatus ::
      !(Maybe Text),
    _esgEC2SecurityGroupOwnerId :: !(Maybe Text),
    _esgEC2SecurityGroupName :: !(Maybe Text),
    _esgEC2SecurityGroupId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2SecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esgStatus' - Provides the status of the EC2 security group. Status can be "authorizing", "authorized", "revoking", and "revoked".
--
-- * 'esgEC2SecurityGroupOwnerId' - Specifies the AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field.
--
-- * 'esgEC2SecurityGroupName' - Specifies the name of the EC2 security group.
--
-- * 'esgEC2SecurityGroupId' - Specifies the id of the EC2 security group.
ec2SecurityGroup ::
  EC2SecurityGroup
ec2SecurityGroup =
  EC2SecurityGroup'
    { _esgStatus = Nothing,
      _esgEC2SecurityGroupOwnerId = Nothing,
      _esgEC2SecurityGroupName = Nothing,
      _esgEC2SecurityGroupId = Nothing
    }

-- | Provides the status of the EC2 security group. Status can be "authorizing", "authorized", "revoking", and "revoked".
esgStatus :: Lens' EC2SecurityGroup (Maybe Text)
esgStatus = lens _esgStatus (\s a -> s {_esgStatus = a})

-- | Specifies the AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field.
esgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupOwnerId = lens _esgEC2SecurityGroupOwnerId (\s a -> s {_esgEC2SecurityGroupOwnerId = a})

-- | Specifies the name of the EC2 security group.
esgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupName = lens _esgEC2SecurityGroupName (\s a -> s {_esgEC2SecurityGroupName = a})

-- | Specifies the id of the EC2 security group.
esgEC2SecurityGroupId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupId = lens _esgEC2SecurityGroupId (\s a -> s {_esgEC2SecurityGroupId = a})

instance FromXML EC2SecurityGroup where
  parseXML x =
    EC2SecurityGroup'
      <$> (x .@? "Status")
      <*> (x .@? "EC2SecurityGroupOwnerId")
      <*> (x .@? "EC2SecurityGroupName")
      <*> (x .@? "EC2SecurityGroupId")

instance Hashable EC2SecurityGroup

instance NFData EC2SecurityGroup
