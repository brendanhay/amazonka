{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.RegisterElasticIP
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

-- | Registers an Elastic IP address with a specified stack. An address can
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
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterElasticIP.html>
module Network.AWS.OpsWorks.RegisterElasticIP
    (
    -- * Request
      RegisterElasticIP
    -- ** Request constructor
    , registerElasticIP
    -- ** Request lenses
    , reiElasticIP
    , reiStackId

    -- * Response
    , RegisterElasticIPResponse
    -- ** Response constructor
    , registerElasticIPResponse
    -- ** Response lenses
    , reirElasticIP
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'registerElasticIP' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reiElasticIP'
--
-- * 'reiStackId'
data RegisterElasticIP = RegisterElasticIP'{_reiElasticIP :: Text, _reiStackId :: Text} deriving (Eq, Read, Show)

-- | 'RegisterElasticIP' smart constructor.
registerElasticIP :: Text -> Text -> RegisterElasticIP
registerElasticIP pElasticIP pStackId = RegisterElasticIP'{_reiElasticIP = pElasticIP, _reiStackId = pStackId};

-- | The Elastic IP address.
reiElasticIP :: Lens' RegisterElasticIP Text
reiElasticIP = lens _reiElasticIP (\ s a -> s{_reiElasticIP = a});

-- | The stack ID.
reiStackId :: Lens' RegisterElasticIP Text
reiStackId = lens _reiStackId (\ s a -> s{_reiStackId = a});

instance AWSRequest RegisterElasticIP where
        type Sv RegisterElasticIP = OpsWorks
        type Rs RegisterElasticIP = RegisterElasticIPResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterElasticIPResponse' <$> (x .?> "ElasticIp"))

instance ToHeaders RegisterElasticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.RegisterElasticIP" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterElasticIP where
        toJSON RegisterElasticIP'{..}
          = object
              ["ElasticIp" .= _reiElasticIP,
               "StackId" .= _reiStackId]

instance ToPath RegisterElasticIP where
        toPath = const "/"

instance ToQuery RegisterElasticIP where
        toQuery = const mempty

-- | /See:/ 'registerElasticIPResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reirElasticIP'
newtype RegisterElasticIPResponse = RegisterElasticIPResponse'{_reirElasticIP :: Maybe Text} deriving (Eq, Read, Show)

-- | 'RegisterElasticIPResponse' smart constructor.
registerElasticIPResponse :: RegisterElasticIPResponse
registerElasticIPResponse = RegisterElasticIPResponse'{_reirElasticIP = Nothing};

-- | The Elastic IP address.
reirElasticIP :: Lens' RegisterElasticIPResponse (Maybe Text)
reirElasticIP = lens _reirElasticIP (\ s a -> s{_reirElasticIP = a});
