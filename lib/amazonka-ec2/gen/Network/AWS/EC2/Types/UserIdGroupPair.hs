{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserIdGroupPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserIdGroupPair where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a security group and AWS account ID pair.
--
--
--
-- /See:/ 'userIdGroupPair' smart constructor.
data UserIdGroupPair = UserIdGroupPair'
  { _uigpVPCPeeringConnectionId ::
      !(Maybe Text),
    _uigpVPCId :: !(Maybe Text),
    _uigpUserId :: !(Maybe Text),
    _uigpGroupId :: !(Maybe Text),
    _uigpGroupName :: !(Maybe Text),
    _uigpDescription :: !(Maybe Text),
    _uigpPeeringStatus :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserIdGroupPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uigpVPCPeeringConnectionId' - The ID of the VPC peering connection, if applicable.
--
-- * 'uigpVPCId' - The ID of the VPC for the referenced security group, if applicable.
--
-- * 'uigpUserId' - The ID of an AWS account. For a referenced security group in another VPC, the account ID of the referenced security group is returned in the response. If the referenced security group is deleted, this value is not returned. [EC2-Classic] Required when adding or removing rules that reference a security group in another AWS account.
--
-- * 'uigpGroupId' - The ID of the security group.
--
-- * 'uigpGroupName' - The name of the security group. In a request, use this parameter for a security group in EC2-Classic or a default VPC only. For a security group in a nondefault VPC, use the security group ID.  For a referenced security group in another VPC, this value is not returned if the referenced security group is deleted.
--
-- * 'uigpDescription' - A description for the security group rule that references this user ID group pair. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
--
-- * 'uigpPeeringStatus' - The status of a VPC peering connection, if applicable.
userIdGroupPair ::
  UserIdGroupPair
userIdGroupPair =
  UserIdGroupPair'
    { _uigpVPCPeeringConnectionId = Nothing,
      _uigpVPCId = Nothing,
      _uigpUserId = Nothing,
      _uigpGroupId = Nothing,
      _uigpGroupName = Nothing,
      _uigpDescription = Nothing,
      _uigpPeeringStatus = Nothing
    }

-- | The ID of the VPC peering connection, if applicable.
uigpVPCPeeringConnectionId :: Lens' UserIdGroupPair (Maybe Text)
uigpVPCPeeringConnectionId = lens _uigpVPCPeeringConnectionId (\s a -> s {_uigpVPCPeeringConnectionId = a})

-- | The ID of the VPC for the referenced security group, if applicable.
uigpVPCId :: Lens' UserIdGroupPair (Maybe Text)
uigpVPCId = lens _uigpVPCId (\s a -> s {_uigpVPCId = a})

-- | The ID of an AWS account. For a referenced security group in another VPC, the account ID of the referenced security group is returned in the response. If the referenced security group is deleted, this value is not returned. [EC2-Classic] Required when adding or removing rules that reference a security group in another AWS account.
uigpUserId :: Lens' UserIdGroupPair (Maybe Text)
uigpUserId = lens _uigpUserId (\s a -> s {_uigpUserId = a})

-- | The ID of the security group.
uigpGroupId :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupId = lens _uigpGroupId (\s a -> s {_uigpGroupId = a})

-- | The name of the security group. In a request, use this parameter for a security group in EC2-Classic or a default VPC only. For a security group in a nondefault VPC, use the security group ID.  For a referenced security group in another VPC, this value is not returned if the referenced security group is deleted.
uigpGroupName :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupName = lens _uigpGroupName (\s a -> s {_uigpGroupName = a})

-- | A description for the security group rule that references this user ID group pair. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
uigpDescription :: Lens' UserIdGroupPair (Maybe Text)
uigpDescription = lens _uigpDescription (\s a -> s {_uigpDescription = a})

-- | The status of a VPC peering connection, if applicable.
uigpPeeringStatus :: Lens' UserIdGroupPair (Maybe Text)
uigpPeeringStatus = lens _uigpPeeringStatus (\s a -> s {_uigpPeeringStatus = a})

instance FromXML UserIdGroupPair where
  parseXML x =
    UserIdGroupPair'
      <$> (x .@? "vpcPeeringConnectionId")
      <*> (x .@? "vpcId")
      <*> (x .@? "userId")
      <*> (x .@? "groupId")
      <*> (x .@? "groupName")
      <*> (x .@? "description")
      <*> (x .@? "peeringStatus")

instance Hashable UserIdGroupPair

instance NFData UserIdGroupPair

instance ToQuery UserIdGroupPair where
  toQuery UserIdGroupPair' {..} =
    mconcat
      [ "VpcPeeringConnectionId" =: _uigpVPCPeeringConnectionId,
        "VpcId" =: _uigpVPCId,
        "UserId" =: _uigpUserId,
        "GroupId" =: _uigpGroupId,
        "GroupName" =: _uigpGroupName,
        "Description" =: _uigpDescription,
        "PeeringStatus" =: _uigpPeeringStatus
      ]
