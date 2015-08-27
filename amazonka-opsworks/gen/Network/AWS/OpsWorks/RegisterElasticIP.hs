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
-- Module      : Network.AWS.OpsWorks.RegisterElasticIP
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Elastic IP address with a specified stack. An address can
-- be registered with only one stack at a time. If the address is already
-- registered, you must first deregister it by calling DeregisterElasticIp.
-- For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterElasticIP.html AWS API Reference> for RegisterElasticIP.
module Network.AWS.OpsWorks.RegisterElasticIP
    (
    -- * Creating a Request
      registerElasticIP
    , RegisterElasticIP
    -- * Request Lenses
    , reiElasticIP
    , reiStackId

    -- * Destructuring the Response
    , registerElasticIPResponse
    , RegisterElasticIPResponse
    -- * Response Lenses
    , reirsElasticIP
    , reirsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerElasticIP' smart constructor.
data RegisterElasticIP = RegisterElasticIP'
    { _reiElasticIP :: !Text
    , _reiStackId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterElasticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reiElasticIP'
--
-- * 'reiStackId'
registerElasticIP
    :: Text -- ^ 'reiElasticIP'
    -> Text -- ^ 'reiStackId'
    -> RegisterElasticIP
registerElasticIP pElasticIP_ pStackId_ =
    RegisterElasticIP'
    { _reiElasticIP = pElasticIP_
    , _reiStackId = pStackId_
    }

-- | The Elastic IP address.
reiElasticIP :: Lens' RegisterElasticIP Text
reiElasticIP = lens _reiElasticIP (\ s a -> s{_reiElasticIP = a});

-- | The stack ID.
reiStackId :: Lens' RegisterElasticIP Text
reiStackId = lens _reiStackId (\ s a -> s{_reiStackId = a});

instance AWSRequest RegisterElasticIP where
        type Rs RegisterElasticIP = RegisterElasticIPResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 RegisterElasticIPResponse' <$>
                   (x .?> "ElasticIp") <*> (pure (fromEnum s)))

instance ToHeaders RegisterElasticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.RegisterElasticIp" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterElasticIP where
        toJSON RegisterElasticIP'{..}
          = object
              (catMaybes
                 [Just ("ElasticIp" .= _reiElasticIP),
                  Just ("StackId" .= _reiStackId)])

instance ToPath RegisterElasticIP where
        toPath = const "/"

instance ToQuery RegisterElasticIP where
        toQuery = const mempty

-- | Contains the response to a 'RegisterElasticIp' request.
--
-- /See:/ 'registerElasticIPResponse' smart constructor.
data RegisterElasticIPResponse = RegisterElasticIPResponse'
    { _reirsElasticIP :: !(Maybe Text)
    , _reirsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterElasticIPResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reirsElasticIP'
--
-- * 'reirsStatus'
registerElasticIPResponse
    :: Int -- ^ 'reirsStatus'
    -> RegisterElasticIPResponse
registerElasticIPResponse pStatus_ =
    RegisterElasticIPResponse'
    { _reirsElasticIP = Nothing
    , _reirsStatus = pStatus_
    }

-- | The Elastic IP address.
reirsElasticIP :: Lens' RegisterElasticIPResponse (Maybe Text)
reirsElasticIP = lens _reirsElasticIP (\ s a -> s{_reirsElasticIP = a});

-- | The response status code.
reirsStatus :: Lens' RegisterElasticIPResponse Int
reirsStatus = lens _reirsStatus (\ s a -> s{_reirsStatus = a});
