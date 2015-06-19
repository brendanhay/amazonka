{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.StartInstance
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Starts a specified instance. For more information, see
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
    , staInstanceId

    -- * Response
    , StartInstanceResponse
    -- ** Response constructor
    , startInstanceResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staInstanceId'
newtype StartInstance = StartInstance'{_staInstanceId :: Text} deriving (Eq, Read, Show)

-- | 'StartInstance' smart constructor.
startInstance :: Text -> StartInstance
startInstance pInstanceId = StartInstance'{_staInstanceId = pInstanceId};

-- | The instance ID.
staInstanceId :: Lens' StartInstance Text
staInstanceId = lens _staInstanceId (\ s a -> s{_staInstanceId = a});

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
          = object ["InstanceId" .= _staInstanceId]

instance ToPath StartInstance where
        toPath = const "/"

instance ToQuery StartInstance where
        toQuery = const mempty

-- | /See:/ 'startInstanceResponse' smart constructor.
data StartInstanceResponse = StartInstanceResponse' deriving (Eq, Read, Show)

-- | 'StartInstanceResponse' smart constructor.
startInstanceResponse :: StartInstanceResponse
startInstanceResponse = StartInstanceResponse';
