{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.CreateInternetGateway
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

-- | Creates an Internet gateway for use with a VPC. After creating the
-- Internet gateway, you attach it to a VPC using AttachInternetGateway.
--
-- For more information about your VPC and Internet gateway, see the
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/ Amazon Virtual Private Cloud User Guide>.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInternetGateway.html>
module Network.AWS.EC2.CreateInternetGateway
    (
    -- * Request
      CreateInternetGateway
    -- ** Request constructor
    , createInternetGateway
    -- ** Request lenses
    , cigDryRun

    -- * Response
    , CreateInternetGatewayResponse
    -- ** Response constructor
    , createInternetGatewayResponse
    -- ** Response lenses
    , cigrInternetGateway
    , cigrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createInternetGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cigDryRun'
newtype CreateInternetGateway = CreateInternetGateway'
    { _cigDryRun :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'CreateInternetGateway' smart constructor.
createInternetGateway :: CreateInternetGateway
createInternetGateway =
    CreateInternetGateway'
    { _cigDryRun = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cigDryRun :: Lens' CreateInternetGateway (Maybe Bool)
cigDryRun = lens _cigDryRun (\ s a -> s{_cigDryRun = a});

instance AWSRequest CreateInternetGateway where
        type Sv CreateInternetGateway = EC2
        type Rs CreateInternetGateway =
             CreateInternetGatewayResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateInternetGatewayResponse' <$>
                   (x .@? "internetGateway") <*> (pure (fromEnum s)))

instance ToHeaders CreateInternetGateway where
        toHeaders = const mempty

instance ToPath CreateInternetGateway where
        toPath = const "/"

instance ToQuery CreateInternetGateway where
        toQuery CreateInternetGateway'{..}
          = mconcat
              ["Action" =: ("CreateInternetGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _cigDryRun]

-- | /See:/ 'createInternetGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cigrInternetGateway'
--
-- * 'cigrStatus'
data CreateInternetGatewayResponse = CreateInternetGatewayResponse'
    { _cigrInternetGateway :: Maybe InternetGateway
    , _cigrStatus          :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateInternetGatewayResponse' smart constructor.
createInternetGatewayResponse :: Int -> CreateInternetGatewayResponse
createInternetGatewayResponse pStatus =
    CreateInternetGatewayResponse'
    { _cigrInternetGateway = Nothing
    , _cigrStatus = pStatus
    }

-- | Information about the Internet gateway.
cigrInternetGateway :: Lens' CreateInternetGatewayResponse (Maybe InternetGateway)
cigrInternetGateway = lens _cigrInternetGateway (\ s a -> s{_cigrInternetGateway = a});

-- | FIXME: Undocumented member.
cigrStatus :: Lens' CreateInternetGatewayResponse Int
cigrStatus = lens _cigrStatus (\ s a -> s{_cigrStatus = a});
