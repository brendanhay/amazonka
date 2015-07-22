{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSecurityGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a security group.
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
    , csgrqVPCId
    , csgrqDryRun
    , csgrqGroupName
    , csgrqDescription

    -- * Response
    , CreateSecurityGroupResponse
    -- ** Response constructor
    , createSecurityGroupResponse
    -- ** Response lenses
    , csgrsStatus
    , csgrsGroupId
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgrqVPCId'
--
-- * 'csgrqDryRun'
--
-- * 'csgrqGroupName'
--
-- * 'csgrqDescription'
data CreateSecurityGroup = CreateSecurityGroup'
    { _csgrqVPCId       :: !(Maybe Text)
    , _csgrqDryRun      :: !(Maybe Bool)
    , _csgrqGroupName   :: !Text
    , _csgrqDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSecurityGroup' smart constructor.
createSecurityGroup :: Text -> Text -> CreateSecurityGroup
createSecurityGroup pGroupName_ pDescription_ =
    CreateSecurityGroup'
    { _csgrqVPCId = Nothing
    , _csgrqDryRun = Nothing
    , _csgrqGroupName = pGroupName_
    , _csgrqDescription = pDescription_
    }

-- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
csgrqVPCId :: Lens' CreateSecurityGroup (Maybe Text)
csgrqVPCId = lens _csgrqVPCId (\ s a -> s{_csgrqVPCId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
csgrqDryRun :: Lens' CreateSecurityGroup (Maybe Bool)
csgrqDryRun = lens _csgrqDryRun (\ s a -> s{_csgrqDryRun = a});

-- | The name of the security group.
--
-- Constraints: Up to 255 characters in length
--
-- Constraints for EC2-Classic: ASCII characters
--
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
-- ._-:\/()#,\@[]+=&;{}!$*
csgrqGroupName :: Lens' CreateSecurityGroup Text
csgrqGroupName = lens _csgrqGroupName (\ s a -> s{_csgrqGroupName = a});

-- | A description for the security group. This is informational only.
--
-- Constraints: Up to 255 characters in length
--
-- Constraints for EC2-Classic: ASCII characters
--
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
-- ._-:\/()#,\@[]+=&;{}!$*
csgrqDescription :: Lens' CreateSecurityGroup Text
csgrqDescription = lens _csgrqDescription (\ s a -> s{_csgrqDescription = a});

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
               "VpcId" =: _csgrqVPCId, "DryRun" =: _csgrqDryRun,
               "GroupName" =: _csgrqGroupName,
               "GroupDescription" =: _csgrqDescription]

-- | /See:/ 'createSecurityGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgrsStatus'
--
-- * 'csgrsGroupId'
data CreateSecurityGroupResponse = CreateSecurityGroupResponse'
    { _csgrsStatus  :: !Int
    , _csgrsGroupId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSecurityGroupResponse' smart constructor.
createSecurityGroupResponse :: Int -> Text -> CreateSecurityGroupResponse
createSecurityGroupResponse pStatus_ pGroupId_ =
    CreateSecurityGroupResponse'
    { _csgrsStatus = pStatus_
    , _csgrsGroupId = pGroupId_
    }

-- | FIXME: Undocumented member.
csgrsStatus :: Lens' CreateSecurityGroupResponse Int
csgrsStatus = lens _csgrsStatus (\ s a -> s{_csgrsStatus = a});

-- | The ID of the security group.
csgrsGroupId :: Lens' CreateSecurityGroupResponse Text
csgrsGroupId = lens _csgrsGroupId (\ s a -> s{_csgrsGroupId = a});
