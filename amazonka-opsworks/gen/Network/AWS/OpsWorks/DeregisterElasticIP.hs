{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DeregisterElasticIP
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

-- | Deregisters a specified Elastic IP address. The address can then be
-- registered by another stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeregisterElasticIP.html>
module Network.AWS.OpsWorks.DeregisterElasticIP
    (
    -- * Request
      DeregisterElasticIP
    -- ** Request constructor
    , deregisterElasticIP
    -- ** Request lenses
    , derElasticIP

    -- * Response
    , DeregisterElasticIPResponse
    -- ** Response constructor
    , deregisterElasticIPResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterElasticIP' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derElasticIP'
newtype DeregisterElasticIP = DeregisterElasticIP'{_derElasticIP :: Text} deriving (Eq, Read, Show)

-- | 'DeregisterElasticIP' smart constructor.
deregisterElasticIP :: Text -> DeregisterElasticIP
deregisterElasticIP pElasticIP = DeregisterElasticIP'{_derElasticIP = pElasticIP};

-- | The Elastic IP address.
derElasticIP :: Lens' DeregisterElasticIP Text
derElasticIP = lens _derElasticIP (\ s a -> s{_derElasticIP = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeregisterElasticIP where
        type Sv DeregisterElasticIP = OpsWorks
        type Rs DeregisterElasticIP =
             DeregisterElasticIPResponse
        request = postJSON
        response = receiveNull DeregisterElasticIPResponse'

instance ToHeaders DeregisterElasticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeregisterElasticIP" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterElasticIP where
        toJSON DeregisterElasticIP'{..}
          = object ["ElasticIp" .= _derElasticIP]

instance ToPath DeregisterElasticIP where
        toPath = const "/"

instance ToQuery DeregisterElasticIP where
        toQuery = const mempty

-- | /See:/ 'deregisterElasticIPResponse' smart constructor.
data DeregisterElasticIPResponse = DeregisterElasticIPResponse' deriving (Eq, Read, Show)

-- | 'DeregisterElasticIPResponse' smart constructor.
deregisterElasticIPResponse :: DeregisterElasticIPResponse
deregisterElasticIPResponse = DeregisterElasticIPResponse';
