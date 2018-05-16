{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSecurityGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a security group.
--
--
-- A security group is for use with instances either in the EC2-Classic platform or in a specific VPC. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon Elastic Compute Cloud User Guide/ and <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Important:/ EC2-Classic: You can have up to 500 security groups.
--
-- EC2-VPC: You can create up to 500 security groups per VPC.
--
-- When you create a security group, you specify a friendly name of your choice. You can have a security group for use in EC2-Classic with the same name as a security group for use in a VPC. However, you can't have two security groups for use in EC2-Classic with the same name or two security groups for use in a VPC with the same name.
--
-- You have a default security group for use in EC2-Classic and a default security group for use in your VPC. If you don't specify a security group when you launch an instance, the instance is launched into the appropriate default security group. A default security group includes a default rule that grants instances unrestricted network access to each other.
--
-- You can add or remove rules from your security groups using 'AuthorizeSecurityGroupIngress' , 'AuthorizeSecurityGroupEgress' , 'RevokeSecurityGroupIngress' , and 'RevokeSecurityGroupEgress' .
--
module Network.AWS.EC2.CreateSecurityGroup
    (
    -- * Creating a Request
      createSecurityGroup
    , CreateSecurityGroup
    -- * Request Lenses
    , csgVPCId
    , csgDryRun
    , csgDescription
    , csgGroupName

    -- * Destructuring the Response
    , createSecurityGroupResponse
    , CreateSecurityGroupResponse
    -- * Response Lenses
    , csgrsResponseStatus
    , csgrsGroupId
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateSecurityGroup.
--
--
--
-- /See:/ 'createSecurityGroup' smart constructor.
data CreateSecurityGroup = CreateSecurityGroup'
  { _csgVPCId       :: !(Maybe Text)
  , _csgDryRun      :: !(Maybe Bool)
  , _csgDescription :: !Text
  , _csgGroupName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgVPCId' - [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
--
-- * 'csgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'csgDescription' - A description for the security group. This is informational only. Constraints: Up to 255 characters in length Constraints for EC2-Classic: ASCII characters Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- * 'csgGroupName' - The name of the security group. Constraints: Up to 255 characters in length. Cannot start with @sg-@ . Constraints for EC2-Classic: ASCII characters Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
createSecurityGroup
    :: Text -- ^ 'csgDescription'
    -> Text -- ^ 'csgGroupName'
    -> CreateSecurityGroup
createSecurityGroup pDescription_ pGroupName_ =
  CreateSecurityGroup'
    { _csgVPCId = Nothing
    , _csgDryRun = Nothing
    , _csgDescription = pDescription_
    , _csgGroupName = pGroupName_
    }


-- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
csgVPCId :: Lens' CreateSecurityGroup (Maybe Text)
csgVPCId = lens _csgVPCId (\ s a -> s{_csgVPCId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
csgDryRun :: Lens' CreateSecurityGroup (Maybe Bool)
csgDryRun = lens _csgDryRun (\ s a -> s{_csgDryRun = a})

-- | A description for the security group. This is informational only. Constraints: Up to 255 characters in length Constraints for EC2-Classic: ASCII characters Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
csgDescription :: Lens' CreateSecurityGroup Text
csgDescription = lens _csgDescription (\ s a -> s{_csgDescription = a})

-- | The name of the security group. Constraints: Up to 255 characters in length. Cannot start with @sg-@ . Constraints for EC2-Classic: ASCII characters Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
csgGroupName :: Lens' CreateSecurityGroup Text
csgGroupName = lens _csgGroupName (\ s a -> s{_csgGroupName = a})

instance AWSRequest CreateSecurityGroup where
        type Rs CreateSecurityGroup =
             CreateSecurityGroupResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateSecurityGroupResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "groupId"))

instance Hashable CreateSecurityGroup where

instance NFData CreateSecurityGroup where

instance ToHeaders CreateSecurityGroup where
        toHeaders = const mempty

instance ToPath CreateSecurityGroup where
        toPath = const "/"

instance ToQuery CreateSecurityGroup where
        toQuery CreateSecurityGroup'{..}
          = mconcat
              ["Action" =: ("CreateSecurityGroup" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "VpcId" =: _csgVPCId, "DryRun" =: _csgDryRun,
               "GroupDescription" =: _csgDescription,
               "GroupName" =: _csgGroupName]

-- | Contains the output of CreateSecurityGroup.
--
--
--
-- /See:/ 'createSecurityGroupResponse' smart constructor.
data CreateSecurityGroupResponse = CreateSecurityGroupResponse'
  { _csgrsResponseStatus :: !Int
  , _csgrsGroupId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSecurityGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgrsResponseStatus' - -- | The response status code.
--
-- * 'csgrsGroupId' - The ID of the security group.
createSecurityGroupResponse
    :: Int -- ^ 'csgrsResponseStatus'
    -> Text -- ^ 'csgrsGroupId'
    -> CreateSecurityGroupResponse
createSecurityGroupResponse pResponseStatus_ pGroupId_ =
  CreateSecurityGroupResponse'
    {_csgrsResponseStatus = pResponseStatus_, _csgrsGroupId = pGroupId_}


-- | -- | The response status code.
csgrsResponseStatus :: Lens' CreateSecurityGroupResponse Int
csgrsResponseStatus = lens _csgrsResponseStatus (\ s a -> s{_csgrsResponseStatus = a})

-- | The ID of the security group.
csgrsGroupId :: Lens' CreateSecurityGroupResponse Text
csgrsGroupId = lens _csgrsGroupId (\ s a -> s{_csgrsGroupId = a})

instance NFData CreateSecurityGroupResponse where
