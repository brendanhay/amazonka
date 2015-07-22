{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.StartInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Starts a specified instance. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_StartInstance.html>
module Network.AWS.OpsWorks.StartInstance
    (
    -- * Request
      StartInstance
    -- ** Request constructor
    , startInstance
    -- ** Request lenses
    , srqInstanceId

    -- * Response
    , StartInstanceResponse
    -- ** Response constructor
    , startInstanceResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srqInstanceId'
newtype StartInstance = StartInstance'
    { _srqInstanceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartInstance' smart constructor.
startInstance :: Text -> StartInstance
startInstance pInstanceId =
    StartInstance'
    { _srqInstanceId = pInstanceId
    }

-- | The instance ID.
srqInstanceId :: Lens' StartInstance Text
srqInstanceId = lens _srqInstanceId (\ s a -> s{_srqInstanceId = a});

instance AWSRequest StartInstance where
        type Sv StartInstance = OpsWorks
        type Rs StartInstance = StartInstanceResponse
        request = postJSON
        response = receiveNull StartInstanceResponse'

instance ToHeaders StartInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.StartInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartInstance where
        toJSON StartInstance'{..}
          = object ["InstanceId" .= _srqInstanceId]

instance ToPath StartInstance where
        toPath = const "/"

instance ToQuery StartInstance where
        toQuery = const mempty

-- | /See:/ 'startInstanceResponse' smart constructor.
data StartInstanceResponse =
    StartInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartInstanceResponse' smart constructor.
startInstanceResponse :: StartInstanceResponse
startInstanceResponse = StartInstanceResponse'
