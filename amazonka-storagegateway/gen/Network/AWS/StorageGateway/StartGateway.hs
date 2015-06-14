{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.StartGateway
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

-- | This operation starts a gateway that you previously shut down (see
-- ShutdownGateway). After the gateway starts, you can then make other API
-- calls, your applications can read from or write to the gateway\'s
-- storage volumes and you will be able to take snapshot backups.
--
-- When you make a request, you will get a 200 OK success response
-- immediately. However, it might take some time for the gateway to be
-- ready. You should call DescribeGatewayInformation and check the status
-- before making any additional API calls. For more information, see
-- ActivateGateway.
--
-- To specify which gateway to start, use the Amazon Resource Name (ARN) of
-- the gateway in your request.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_StartGateway.html>
module Network.AWS.StorageGateway.StartGateway
    (
    -- * Request
      StartGateway
    -- ** Request constructor
    , startGateway
    -- ** Request lenses
    , sgGatewayARN

    -- * Response
    , StartGatewayResponse
    -- ** Response constructor
    , startGatewayResponse
    -- ** Response lenses
    , staGatewayARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'startGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgGatewayARN'
newtype StartGateway = StartGateway'{_sgGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'StartGateway' smart constructor.
startGateway :: Text -> StartGateway
startGateway pGatewayARN = StartGateway'{_sgGatewayARN = pGatewayARN};

-- | FIXME: Undocumented member.
sgGatewayARN :: Lens' StartGateway Text
sgGatewayARN = lens _sgGatewayARN (\ s a -> s{_sgGatewayARN = a});

instance AWSRequest StartGateway where
        type Sv StartGateway = StorageGateway
        type Rs StartGateway = StartGatewayResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 StartGatewayResponse' <$> x .:> "GatewayARN")

instance ToHeaders StartGateway where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.StartGateway" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartGateway where
        toJSON StartGateway'{..}
          = object ["GatewayARN" .= _sgGatewayARN]

instance ToPath StartGateway where
        toPath = const "/"

instance ToQuery StartGateway where
        toQuery = const mempty

-- | /See:/ 'startGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staGatewayARN'
newtype StartGatewayResponse = StartGatewayResponse'{_staGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'StartGatewayResponse' smart constructor.
startGatewayResponse :: Text -> StartGatewayResponse
startGatewayResponse pGatewayARN = StartGatewayResponse'{_staGatewayARN = pGatewayARN};

-- | FIXME: Undocumented member.
staGatewayARN :: Lens' StartGatewayResponse Text
staGatewayARN = lens _staGatewayARN (\ s a -> s{_staGatewayARN = a});
