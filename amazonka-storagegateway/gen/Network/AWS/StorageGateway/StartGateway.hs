{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.StartGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation starts a gateway that you previously shut down (see
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
    , sgrqGatewayARN

    -- * Response
    , StartGatewayResponse
    -- ** Response constructor
    , startGatewayResponse
    -- ** Response lenses
    , sgrsGatewayARN
    , sgrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing the of the gateway to start.
--
-- /See:/ 'startGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgrqGatewayARN'
newtype StartGateway = StartGateway'
    { _sgrqGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartGateway' smart constructor.
startGateway :: Text -> StartGateway
startGateway pGatewayARN =
    StartGateway'
    { _sgrqGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
sgrqGatewayARN :: Lens' StartGateway Text
sgrqGatewayARN = lens _sgrqGatewayARN (\ s a -> s{_sgrqGatewayARN = a});

instance AWSRequest StartGateway where
        type Sv StartGateway = StorageGateway
        type Rs StartGateway = StartGatewayResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 StartGatewayResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

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
          = object ["GatewayARN" .= _sgrqGatewayARN]

instance ToPath StartGateway where
        toPath = const "/"

instance ToQuery StartGateway where
        toQuery = const mempty

-- | A JSON object containing the of the gateway that was restarted.
--
-- /See:/ 'startGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgrsGatewayARN'
--
-- * 'sgrsStatus'
data StartGatewayResponse = StartGatewayResponse'
    { _sgrsGatewayARN :: !(Maybe Text)
    , _sgrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartGatewayResponse' smart constructor.
startGatewayResponse :: Int -> StartGatewayResponse
startGatewayResponse pStatus =
    StartGatewayResponse'
    { _sgrsGatewayARN = Nothing
    , _sgrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
sgrsGatewayARN :: Lens' StartGatewayResponse (Maybe Text)
sgrsGatewayARN = lens _sgrsGatewayARN (\ s a -> s{_sgrsGatewayARN = a});

-- | FIXME: Undocumented member.
sgrsStatus :: Lens' StartGatewayResponse Int
sgrsStatus = lens _sgrsStatus (\ s a -> s{_sgrsStatus = a});
