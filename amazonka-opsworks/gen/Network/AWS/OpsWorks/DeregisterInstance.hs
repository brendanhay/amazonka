{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deregister a registered Amazon EC2 or on-premises instance. This action
-- removes the instance from the stack and returns it to your control. This
-- action can not be used with instances that were created with AWS
-- OpsWorks.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeregisterInstance.html>
module Network.AWS.OpsWorks.DeregisterInstance
    (
    -- * Request
      DeregisterInstance
    -- ** Request constructor
    , deregisterInstance
    -- ** Request lenses
    , drqInstanceId

    -- * Response
    , DeregisterInstanceResponse
    -- ** Response constructor
    , deregisterInstanceResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqInstanceId'
newtype DeregisterInstance = DeregisterInstance'
    { _drqInstanceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterInstance' smart constructor.
deregisterInstance :: Text -> DeregisterInstance
deregisterInstance pInstanceId =
    DeregisterInstance'
    { _drqInstanceId = pInstanceId
    }

-- | The instance ID.
drqInstanceId :: Lens' DeregisterInstance Text
drqInstanceId = lens _drqInstanceId (\ s a -> s{_drqInstanceId = a});

instance AWSRequest DeregisterInstance where
        type Sv DeregisterInstance = OpsWorks
        type Rs DeregisterInstance =
             DeregisterInstanceResponse
        request = postJSON
        response = receiveNull DeregisterInstanceResponse'

instance ToHeaders DeregisterInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeregisterInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterInstance where
        toJSON DeregisterInstance'{..}
          = object ["InstanceId" .= _drqInstanceId]

instance ToPath DeregisterInstance where
        toPath = const "/"

instance ToQuery DeregisterInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterInstanceResponse' smart constructor.
data DeregisterInstanceResponse =
    DeregisterInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterInstanceResponse' smart constructor.
deregisterInstanceResponse :: DeregisterInstanceResponse
deregisterInstanceResponse = DeregisterInstanceResponse'
