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
-- Module      : Network.AWS.OpsWorks.StartInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specified instance. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.StartInstance
    (
    -- * Creating a Request
      startInstance
    , StartInstance
    -- * Request Lenses
    , sInstanceId

    -- * Destructuring the Response
    , startInstanceResponse
    , StartInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startInstance' smart constructor.
newtype StartInstance = StartInstance'
  { _sInstanceId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sInstanceId' - The instance ID.
startInstance
    :: Text -- ^ 'sInstanceId'
    -> StartInstance
startInstance pInstanceId_ = StartInstance' {_sInstanceId = pInstanceId_}


-- | The instance ID.
sInstanceId :: Lens' StartInstance Text
sInstanceId = lens _sInstanceId (\ s a -> s{_sInstanceId = a})

instance AWSRequest StartInstance where
        type Rs StartInstance = StartInstanceResponse
        request = postJSON opsWorks
        response = receiveNull StartInstanceResponse'

instance Hashable StartInstance where

instance NFData StartInstance where

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
          = object
              (catMaybes [Just ("InstanceId" .= _sInstanceId)])

instance ToPath StartInstance where
        toPath = const "/"

instance ToQuery StartInstance where
        toQuery = const mempty

-- | /See:/ 'startInstanceResponse' smart constructor.
data StartInstanceResponse =
  StartInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartInstanceResponse' with the minimum fields required to make a request.
--
startInstanceResponse
    :: StartInstanceResponse
startInstanceResponse = StartInstanceResponse'


instance NFData StartInstanceResponse where
