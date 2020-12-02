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
-- Module      : Network.AWS.OpsWorks.RebootInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a specified instance. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.RebootInstance
    (
    -- * Creating a Request
      rebootInstance
    , RebootInstance
    -- * Request Lenses
    , riInstanceId

    -- * Destructuring the Response
    , rebootInstanceResponse
    , RebootInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rebootInstance' smart constructor.
newtype RebootInstance = RebootInstance'
  { _riInstanceId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riInstanceId' - The instance ID.
rebootInstance
    :: Text -- ^ 'riInstanceId'
    -> RebootInstance
rebootInstance pInstanceId_ = RebootInstance' {_riInstanceId = pInstanceId_}


-- | The instance ID.
riInstanceId :: Lens' RebootInstance Text
riInstanceId = lens _riInstanceId (\ s a -> s{_riInstanceId = a})

instance AWSRequest RebootInstance where
        type Rs RebootInstance = RebootInstanceResponse
        request = postJSON opsWorks
        response = receiveNull RebootInstanceResponse'

instance Hashable RebootInstance where

instance NFData RebootInstance where

instance ToHeaders RebootInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.RebootInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RebootInstance where
        toJSON RebootInstance'{..}
          = object
              (catMaybes [Just ("InstanceId" .= _riInstanceId)])

instance ToPath RebootInstance where
        toPath = const "/"

instance ToQuery RebootInstance where
        toQuery = const mempty

-- | /See:/ 'rebootInstanceResponse' smart constructor.
data RebootInstanceResponse =
  RebootInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootInstanceResponse' with the minimum fields required to make a request.
--
rebootInstanceResponse
    :: RebootInstanceResponse
rebootInstanceResponse = RebootInstanceResponse'


instance NFData RebootInstanceResponse where
