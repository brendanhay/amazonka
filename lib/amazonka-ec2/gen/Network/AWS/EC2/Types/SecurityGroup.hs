{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroup where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IPPermission
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a security group
--
--
--
-- /See:/ 'securityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { _sgVPCId :: !(Maybe Text),
    _sgIPPermissions :: !(Maybe [IPPermission]),
    _sgIPPermissionsEgress :: !(Maybe [IPPermission]),
    _sgTags :: !(Maybe [Tag]),
    _sgOwnerId :: !Text,
    _sgGroupId :: !Text,
    _sgGroupName :: !Text,
    _sgDescription :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgVPCId' - [VPC only] The ID of the VPC for the security group.
--
-- * 'sgIPPermissions' - The inbound rules associated with the security group.
--
-- * 'sgIPPermissionsEgress' - [VPC only] The outbound rules associated with the security group.
--
-- * 'sgTags' - Any tags assigned to the security group.
--
-- * 'sgOwnerId' - The AWS account ID of the owner of the security group.
--
-- * 'sgGroupId' - The ID of the security group.
--
-- * 'sgGroupName' - The name of the security group.
--
-- * 'sgDescription' - A description of the security group.
securityGroup ::
  -- | 'sgOwnerId'
  Text ->
  -- | 'sgGroupId'
  Text ->
  -- | 'sgGroupName'
  Text ->
  -- | 'sgDescription'
  Text ->
  SecurityGroup
securityGroup pOwnerId_ pGroupId_ pGroupName_ pDescription_ =
  SecurityGroup'
    { _sgVPCId = Nothing,
      _sgIPPermissions = Nothing,
      _sgIPPermissionsEgress = Nothing,
      _sgTags = Nothing,
      _sgOwnerId = pOwnerId_,
      _sgGroupId = pGroupId_,
      _sgGroupName = pGroupName_,
      _sgDescription = pDescription_
    }

-- | [VPC only] The ID of the VPC for the security group.
sgVPCId :: Lens' SecurityGroup (Maybe Text)
sgVPCId = lens _sgVPCId (\s a -> s {_sgVPCId = a})

-- | The inbound rules associated with the security group.
sgIPPermissions :: Lens' SecurityGroup [IPPermission]
sgIPPermissions = lens _sgIPPermissions (\s a -> s {_sgIPPermissions = a}) . _Default . _Coerce

-- | [VPC only] The outbound rules associated with the security group.
sgIPPermissionsEgress :: Lens' SecurityGroup [IPPermission]
sgIPPermissionsEgress = lens _sgIPPermissionsEgress (\s a -> s {_sgIPPermissionsEgress = a}) . _Default . _Coerce

-- | Any tags assigned to the security group.
sgTags :: Lens' SecurityGroup [Tag]
sgTags = lens _sgTags (\s a -> s {_sgTags = a}) . _Default . _Coerce

-- | The AWS account ID of the owner of the security group.
sgOwnerId :: Lens' SecurityGroup Text
sgOwnerId = lens _sgOwnerId (\s a -> s {_sgOwnerId = a})

-- | The ID of the security group.
sgGroupId :: Lens' SecurityGroup Text
sgGroupId = lens _sgGroupId (\s a -> s {_sgGroupId = a})

-- | The name of the security group.
sgGroupName :: Lens' SecurityGroup Text
sgGroupName = lens _sgGroupName (\s a -> s {_sgGroupName = a})

-- | A description of the security group.
sgDescription :: Lens' SecurityGroup Text
sgDescription = lens _sgDescription (\s a -> s {_sgDescription = a})

instance FromXML SecurityGroup where
  parseXML x =
    SecurityGroup'
      <$> (x .@? "vpcId")
      <*> (x .@? "ipPermissions" .!@ mempty >>= may (parseXMLList "item"))
      <*> ( x .@? "ipPermissionsEgress" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "ownerId")
      <*> (x .@ "groupId")
      <*> (x .@ "groupName")
      <*> (x .@ "groupDescription")

instance Hashable SecurityGroup

instance NFData SecurityGroup
