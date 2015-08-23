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
-- Module      : Network.AWS.OpsWorks.DeregisterInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeregisterInstance.html AWS API Reference> for DeregisterInstance.
module Network.AWS.OpsWorks.DeregisterInstance
    (
    -- * Creating a Request
      deregisterInstance
    , DeregisterInstance
    -- * Request Lenses
    , dInstanceId

    -- * Destructuring the Response
    , deregisterInstanceResponse
    , DeregisterInstanceResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterInstance' smart constructor.
newtype DeregisterInstance = DeregisterInstance'
    { _dInstanceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeregisterInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dInstanceId'
deregisterInstance
    :: Text -- ^ 'dInstanceId'
    -> DeregisterInstance
deregisterInstance pInstanceId_ =
    DeregisterInstance'
    { _dInstanceId = pInstanceId_
    }

-- | The instance ID.
dInstanceId :: Lens' DeregisterInstance Text
dInstanceId = lens _dInstanceId (\ s a -> s{_dInstanceId = a});

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
          = object
              (catMaybes [Just ("InstanceId" .= _dInstanceId)])

instance ToPath DeregisterInstance where
        toPath = const "/"

instance ToQuery DeregisterInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterInstanceResponse' smart constructor.
data DeregisterInstanceResponse =
    DeregisterInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeregisterInstanceResponse' with the minimum fields required to make a request.
--
deregisterInstanceResponse
    :: DeregisterInstanceResponse
deregisterInstanceResponse = DeregisterInstanceResponse'
