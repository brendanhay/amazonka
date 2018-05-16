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
-- Module      : Network.AWS.OpsWorks.DeleteInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified instance, which terminates the associated Amazon EC2 instance. You must stop an instance before you can delete it.
--
--
-- For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-delete.html Deleting Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DeleteInstance
    (
    -- * Creating a Request
      deleteInstance
    , DeleteInstance
    -- * Request Lenses
    , diDeleteVolumes
    , diDeleteElasticIP
    , diInstanceId

    -- * Destructuring the Response
    , deleteInstanceResponse
    , DeleteInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteInstance' smart constructor.
data DeleteInstance = DeleteInstance'
  { _diDeleteVolumes   :: !(Maybe Bool)
  , _diDeleteElasticIP :: !(Maybe Bool)
  , _diInstanceId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diDeleteVolumes' - Whether to delete the instance's Amazon EBS volumes.
--
-- * 'diDeleteElasticIP' - Whether to delete the instance Elastic IP address.
--
-- * 'diInstanceId' - The instance ID.
deleteInstance
    :: Text -- ^ 'diInstanceId'
    -> DeleteInstance
deleteInstance pInstanceId_ =
  DeleteInstance'
    { _diDeleteVolumes = Nothing
    , _diDeleteElasticIP = Nothing
    , _diInstanceId = pInstanceId_
    }


-- | Whether to delete the instance's Amazon EBS volumes.
diDeleteVolumes :: Lens' DeleteInstance (Maybe Bool)
diDeleteVolumes = lens _diDeleteVolumes (\ s a -> s{_diDeleteVolumes = a})

-- | Whether to delete the instance Elastic IP address.
diDeleteElasticIP :: Lens' DeleteInstance (Maybe Bool)
diDeleteElasticIP = lens _diDeleteElasticIP (\ s a -> s{_diDeleteElasticIP = a})

-- | The instance ID.
diInstanceId :: Lens' DeleteInstance Text
diInstanceId = lens _diInstanceId (\ s a -> s{_diInstanceId = a})

instance AWSRequest DeleteInstance where
        type Rs DeleteInstance = DeleteInstanceResponse
        request = postJSON opsWorks
        response = receiveNull DeleteInstanceResponse'

instance Hashable DeleteInstance where

instance NFData DeleteInstance where

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
              (catMaybes
                 [("DeleteVolumes" .=) <$> _diDeleteVolumes,
                  ("DeleteElasticIp" .=) <$> _diDeleteElasticIP,
                  Just ("InstanceId" .= _diInstanceId)])

instance ToPath DeleteInstance where
        toPath = const "/"

instance ToQuery DeleteInstance where
        toQuery = const mempty

-- | /See:/ 'deleteInstanceResponse' smart constructor.
data DeleteInstanceResponse =
  DeleteInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInstanceResponse' with the minimum fields required to make a request.
--
deleteInstanceResponse
    :: DeleteInstanceResponse
deleteInstanceResponse = DeleteInstanceResponse'


instance NFData DeleteInstanceResponse where
