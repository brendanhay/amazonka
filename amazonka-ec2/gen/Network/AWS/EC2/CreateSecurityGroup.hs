{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.CreateSecurityGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a security group.
--
-- A security group is for use with instances either in the EC2-Classic
-- platform or in a specific VPC. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups>
-- in the /Amazon Elastic Compute Cloud User Guide/ and
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- EC2-Classic: You can have up to 500 security groups.
--
-- EC2-VPC: You can create up to 100 security groups per VPC.
--
-- When you create a security group, you specify a friendly name of your
-- choice. You can have a security group for use in EC2-Classic with the
-- same name as a security group for use in a VPC. However, you can\'t have
-- two security groups for use in EC2-Classic with the same name or two
-- security groups for use in a VPC with the same name.
--
-- You have a default security group for use in EC2-Classic and a default
-- security group for use in your VPC. If you don\'t specify a security
-- group when you launch an instance, the instance is launched into the
-- appropriate default security group. A default security group includes a
-- default rule that grants instances unrestricted network access to each
-- other.
--
-- You can add or remove rules from your security groups using
-- AuthorizeSecurityGroupIngress, AuthorizeSecurityGroupEgress,
-- RevokeSecurityGroupIngress, and RevokeSecurityGroupEgress.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSecurityGroup.html>
module Network.AWS.EC2.CreateSecurityGroup
    (
    -- * Request
      CreateSecurityGroup
    -- ** Request constructor
    , createSecurityGroup
    -- ** Request lenses
    , csgVPCId
    , csgDryRun
    , csgGroupName
    , csgDescription

    -- * Response
    , CreateSecurityGroupResponse
    -- ** Response constructor
    , createSecurityGroupResponse
    -- ** Response lenses
    , csgrStatus
    , csgrGroupId
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgVPCId'
--
-- * 'csgDryRun'
--
-- * 'csgGroupName'
--
-- * 'csgDescription'
data CreateSecurityGroup = CreateSecurityGroup'
    { _csgVPCId       :: !(Maybe Text)
    , _csgDryRun      :: !(Maybe Bool)
    , _csgGroupName   :: !Text
    , _csgDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSecurityGroup' smart constructor.
createSecurityGroup :: Text -> Text -> CreateSecurityGroup
createSecurityGroup pGroupName pDescription =
    CreateSecurityGroup'
    { _csgVPCId = Nothing
    , _csgDryRun = Nothing
    , _csgGroupName = pGroupName
    , _csgDescription = pDescription
    }

-- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
csgVPCId :: Lens' CreateSecurityGroup (Maybe Text)
csgVPCId = lens _csgVPCId (\ s a -> s{_csgVPCId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
csgDryRun :: Lens' CreateSecurityGroup (Maybe Bool)
csgDryRun = lens _csgDryRun (\ s a -> s{_csgDryRun = a});

-- | The name of the security group.
--
-- Constraints: Up to 255 characters in length
--
-- Constraints for EC2-Classic: ASCII characters
--
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
-- ._-:\/()#,\@[]+=&;{}!$*
csgGroupName :: Lens' CreateSecurityGroup Text
csgGroupName = lens _csgGroupName (\ s a -> s{_csgGroupName = a});

-- | A description for the security group. This is informational only.
--
-- Constraints: Up to 255 characters in length
--
-- Constraints for EC2-Classic: ASCII characters
--
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
-- ._-:\/()#,\@[]+=&;{}!$*
csgDescription :: Lens' CreateSecurityGroup Text
csgDescription = lens _csgDescription (\ s a -> s{_csgDescription = a});

instance AWSRequest CreateSecurityGroup where
        type Sv CreateSecurityGroup = EC2
        type Rs CreateSecurityGroup =
             CreateSecurityGroupResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateSecurityGroupResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "groupId"))

instance ToHeaders CreateSecurityGroup where
        toHeaders = const mempty

instance ToPath CreateSecurityGroup where
        toPath = const "/"

instance ToQuery CreateSecurityGroup where
        toQuery CreateSecurityGroup'{..}
          = mconcat
              ["Action" =: ("CreateSecurityGroup" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "VpcId" =: _csgVPCId, "DryRun" =: _csgDryRun,
               "GroupName" =: _csgGroupName,
               "GroupDescription" =: _csgDescription]

-- | /See:/ 'createSecurityGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgrStatus'
--
-- * 'csgrGroupId'
data CreateSecurityGroupResponse = CreateSecurityGroupResponse'
    { _csgrStatus  :: !Int
    , _csgrGroupId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSecurityGroupResponse' smart constructor.
createSecurityGroupResponse :: Int -> Text -> CreateSecurityGroupResponse
createSecurityGroupResponse pStatus pGroupId =
    CreateSecurityGroupResponse'
    { _csgrStatus = pStatus
    , _csgrGroupId = pGroupId
    }

-- | FIXME: Undocumented member.
csgrStatus :: Lens' CreateSecurityGroupResponse Int
csgrStatus = lens _csgrStatus (\ s a -> s{_csgrStatus = a});

-- | The ID of the security group.
csgrGroupId :: Lens' CreateSecurityGroupResponse Text
csgrGroupId = lens _csgrGroupId (\ s a -> s{_csgrGroupId = a});
