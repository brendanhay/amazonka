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
-- Module      : Network.AWS.EC2.DeleteSecurityGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a security group.
--
--
-- If you attempt to delete a security group that is associated with an instance, or is referenced by another security group, the operation fails with @InvalidGroup.InUse@ in EC2-Classic or @DependencyViolation@ in EC2-VPC.
--
module Network.AWS.EC2.DeleteSecurityGroup
    (
    -- * Creating a Request
      deleteSecurityGroup
    , DeleteSecurityGroup
    -- * Request Lenses
    , dsgGroupId
    , dsgGroupName
    , dsgDryRun

    -- * Destructuring the Response
    , deleteSecurityGroupResponse
    , DeleteSecurityGroupResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSecurityGroup' smart constructor.
data DeleteSecurityGroup = DeleteSecurityGroup'
  { _dsgGroupId   :: !(Maybe Text)
  , _dsgGroupName :: !(Maybe Text)
  , _dsgDryRun    :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgGroupId' - The ID of the security group. Required for a nondefault VPC.
--
-- * 'dsgGroupName' - [EC2-Classic, default VPC] The name of the security group. You can specify either the security group name or the security group ID.
--
-- * 'dsgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
deleteSecurityGroup
    :: DeleteSecurityGroup
deleteSecurityGroup =
  DeleteSecurityGroup'
    {_dsgGroupId = Nothing, _dsgGroupName = Nothing, _dsgDryRun = Nothing}


-- | The ID of the security group. Required for a nondefault VPC.
dsgGroupId :: Lens' DeleteSecurityGroup (Maybe Text)
dsgGroupId = lens _dsgGroupId (\ s a -> s{_dsgGroupId = a})

-- | [EC2-Classic, default VPC] The name of the security group. You can specify either the security group name or the security group ID.
dsgGroupName :: Lens' DeleteSecurityGroup (Maybe Text)
dsgGroupName = lens _dsgGroupName (\ s a -> s{_dsgGroupName = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsgDryRun :: Lens' DeleteSecurityGroup (Maybe Bool)
dsgDryRun = lens _dsgDryRun (\ s a -> s{_dsgDryRun = a})

instance AWSRequest DeleteSecurityGroup where
        type Rs DeleteSecurityGroup =
             DeleteSecurityGroupResponse
        request = postQuery ec2
        response = receiveNull DeleteSecurityGroupResponse'

instance Hashable DeleteSecurityGroup where

instance NFData DeleteSecurityGroup where

instance ToHeaders DeleteSecurityGroup where
        toHeaders = const mempty

instance ToPath DeleteSecurityGroup where
        toPath = const "/"

instance ToQuery DeleteSecurityGroup where
        toQuery DeleteSecurityGroup'{..}
          = mconcat
              ["Action" =: ("DeleteSecurityGroup" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "GroupId" =: _dsgGroupId,
               "GroupName" =: _dsgGroupName, "DryRun" =: _dsgDryRun]

-- | /See:/ 'deleteSecurityGroupResponse' smart constructor.
data DeleteSecurityGroupResponse =
  DeleteSecurityGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSecurityGroupResponse' with the minimum fields required to make a request.
--
deleteSecurityGroupResponse
    :: DeleteSecurityGroupResponse
deleteSecurityGroupResponse = DeleteSecurityGroupResponse'


instance NFData DeleteSecurityGroupResponse where
