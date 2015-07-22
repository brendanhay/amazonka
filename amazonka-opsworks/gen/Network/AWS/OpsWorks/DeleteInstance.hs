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
    , dirqDeleteVolumes
    , dirqDeleteElasticIP
    , dirqInstanceId

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
-- * 'dirqDeleteVolumes'
--
-- * 'dirqDeleteElasticIP'
--
-- * 'dirqInstanceId'
data DeleteInstance = DeleteInstance'
    { _dirqDeleteVolumes   :: !(Maybe Bool)
    , _dirqDeleteElasticIP :: !(Maybe Bool)
    , _dirqInstanceId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteInstance' smart constructor.
deleteInstance :: Text -> DeleteInstance
deleteInstance pInstanceId =
    DeleteInstance'
    { _dirqDeleteVolumes = Nothing
    , _dirqDeleteElasticIP = Nothing
    , _dirqInstanceId = pInstanceId
    }

-- | Whether to delete the instance\'s Amazon EBS volumes.
dirqDeleteVolumes :: Lens' DeleteInstance (Maybe Bool)
dirqDeleteVolumes = lens _dirqDeleteVolumes (\ s a -> s{_dirqDeleteVolumes = a});

-- | Whether to delete the instance Elastic IP address.
dirqDeleteElasticIP :: Lens' DeleteInstance (Maybe Bool)
dirqDeleteElasticIP = lens _dirqDeleteElasticIP (\ s a -> s{_dirqDeleteElasticIP = a});

-- | The instance ID.
dirqInstanceId :: Lens' DeleteInstance Text
dirqInstanceId = lens _dirqInstanceId (\ s a -> s{_dirqInstanceId = a});

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
              ["DeleteVolumes" .= _dirqDeleteVolumes,
               "DeleteElasticIp" .= _dirqDeleteElasticIP,
               "InstanceId" .= _dirqInstanceId]

instance ToPath DeleteInstance where
        toPath = const "/"

instance ToQuery DeleteInstance where
        toQuery = const mempty

-- | /See:/ 'deleteInstanceResponse' smart constructor.
data DeleteInstanceResponse =
    DeleteInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteInstanceResponse' smart constructor.
deleteInstanceResponse :: DeleteInstanceResponse
deleteInstanceResponse = DeleteInstanceResponse'
