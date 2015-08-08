{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSecurityGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a security group.
--
-- If you attempt to delete a security group that is associated with an
-- instance, or is referenced by another security group, the operation
-- fails with @InvalidGroup.InUse@ in EC2-Classic or @DependencyViolation@
-- in EC2-VPC.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSecurityGroup.html AWS API Reference> for DeleteSecurityGroup.
module Network.AWS.EC2.DeleteSecurityGroup
    (
    -- * Creating a Request
      DeleteSecurityGroup
    , deleteSecurityGroup
    -- * Request Lenses
    , dsgGroupId
    , dsgGroupName
    , dsgDryRun

    -- * Destructuring the Response
    , DeleteSecurityGroupResponse
    , deleteSecurityGroupResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsgGroupId'
--
-- * 'dsgGroupName'
--
-- * 'dsgDryRun'
data DeleteSecurityGroup = DeleteSecurityGroup'
    { _dsgGroupId   :: !(Maybe Text)
    , _dsgGroupName :: !(Maybe Text)
    , _dsgDryRun    :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSecurityGroup' smart constructor.
deleteSecurityGroup :: DeleteSecurityGroup
deleteSecurityGroup =
    DeleteSecurityGroup'
    { _dsgGroupId = Nothing
    , _dsgGroupName = Nothing
    , _dsgDryRun = Nothing
    }

-- | The ID of the security group. Required for a nondefault VPC.
dsgGroupId :: Lens' DeleteSecurityGroup (Maybe Text)
dsgGroupId = lens _dsgGroupId (\ s a -> s{_dsgGroupId = a});

-- | [EC2-Classic, default VPC] The name of the security group. You can
-- specify either the security group name or the security group ID.
dsgGroupName :: Lens' DeleteSecurityGroup (Maybe Text)
dsgGroupName = lens _dsgGroupName (\ s a -> s{_dsgGroupName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsgDryRun :: Lens' DeleteSecurityGroup (Maybe Bool)
dsgDryRun = lens _dsgDryRun (\ s a -> s{_dsgDryRun = a});

instance AWSRequest DeleteSecurityGroup where
        type Sv DeleteSecurityGroup = EC2
        type Rs DeleteSecurityGroup =
             DeleteSecurityGroupResponse
        request = post
        response = receiveNull DeleteSecurityGroupResponse'

instance ToHeaders DeleteSecurityGroup where
        toHeaders = const mempty

instance ToPath DeleteSecurityGroup where
        toPath = const "/"

instance ToQuery DeleteSecurityGroup where
        toQuery DeleteSecurityGroup'{..}
          = mconcat
              ["Action" =: ("DeleteSecurityGroup" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "GroupId" =: _dsgGroupId,
               "GroupName" =: _dsgGroupName, "DryRun" =: _dsgDryRun]

-- | /See:/ 'deleteSecurityGroupResponse' smart constructor.
data DeleteSecurityGroupResponse =
    DeleteSecurityGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSecurityGroupResponse' smart constructor.
deleteSecurityGroupResponse :: DeleteSecurityGroupResponse
deleteSecurityGroupResponse = DeleteSecurityGroupResponse'
