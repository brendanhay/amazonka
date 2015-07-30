{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified instance, which terminates the associated Amazon EC2
-- instance. You must stop an instance before you can delete it.
--
-- For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-delete.html Deleting Instances>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteInstance.html>
module Network.AWS.OpsWorks.DeleteInstance
    (
    -- * Request
      DeleteInstance
    -- ** Request constructor
    , deleteInstance
    -- ** Request lenses
    , diDeleteVolumes
    , diDeleteElasticIP
    , diInstanceId

    -- * Response
    , DeleteInstanceResponse
    -- ** Response constructor
    , deleteInstanceResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diDeleteVolumes'
--
-- * 'diDeleteElasticIP'
--
-- * 'diInstanceId'
data DeleteInstance = DeleteInstance'
    { _diDeleteVolumes   :: !(Maybe Bool)
    , _diDeleteElasticIP :: !(Maybe Bool)
    , _diInstanceId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteInstance' smart constructor.
deleteInstance :: Text -> DeleteInstance
deleteInstance pInstanceId_ =
    DeleteInstance'
    { _diDeleteVolumes = Nothing
    , _diDeleteElasticIP = Nothing
    , _diInstanceId = pInstanceId_
    }

-- | Whether to delete the instance\'s Amazon EBS volumes.
diDeleteVolumes :: Lens' DeleteInstance (Maybe Bool)
diDeleteVolumes = lens _diDeleteVolumes (\ s a -> s{_diDeleteVolumes = a});

-- | Whether to delete the instance Elastic IP address.
diDeleteElasticIP :: Lens' DeleteInstance (Maybe Bool)
diDeleteElasticIP = lens _diDeleteElasticIP (\ s a -> s{_diDeleteElasticIP = a});

-- | The instance ID.
diInstanceId :: Lens' DeleteInstance Text
diInstanceId = lens _diInstanceId (\ s a -> s{_diInstanceId = a});

instance AWSRequest DeleteInstance where
        type Sv DeleteInstance = OpsWorks
        type Rs DeleteInstance = DeleteInstanceResponse
        request = postJSON
        response = receiveNull DeleteInstanceResponse'

instance ToHeaders DeleteInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeleteInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteInstance where
        toJSON DeleteInstance'{..}
          = object
              ["DeleteVolumes" .= _diDeleteVolumes,
               "DeleteElasticIp" .= _diDeleteElasticIP,
               "InstanceId" .= _diInstanceId]

instance ToPath DeleteInstance where
        toPath = const mempty

instance ToQuery DeleteInstance where
        toQuery = const mempty

-- | /See:/ 'deleteInstanceResponse' smart constructor.
data DeleteInstanceResponse =
    DeleteInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteInstanceResponse' smart constructor.
deleteInstanceResponse :: DeleteInstanceResponse
deleteInstanceResponse = DeleteInstanceResponse'
