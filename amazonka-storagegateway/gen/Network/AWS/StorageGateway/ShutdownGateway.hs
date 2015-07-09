{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ShutdownGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | This operation shuts down a gateway. To specify which gateway to shut
-- down, use the Amazon Resource Name (ARN) of the gateway in the body of
-- your request.
--
-- The operation shuts down the gateway service component running in the
-- storage gateway\'s virtual machine (VM) and not the VM.
--
-- If you want to shut down the VM, it is recommended that you first shut
-- down the gateway component in the VM to avoid unpredictable conditions.
--
-- After the gateway is shutdown, you cannot call any other API except
-- StartGateway, DescribeGatewayInformation, and ListGateways. For more
-- information, see ActivateGateway. Your applications cannot read from or
-- write to the gateway\'s storage volumes, and there are no snapshots
-- taken.
--
-- When you make a shutdown request, you will get a @200 OK@ success
-- response immediately. However, it might take some time for the gateway
-- to shut down. You can call the DescribeGatewayInformation API to check
-- the status. For more information, see ActivateGateway.
--
-- If do not intend to use the gateway again, you must delete the gateway
-- (using DeleteGateway) to no longer pay software charges associated with
-- the gateway.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ShutdownGateway.html>
module Network.AWS.StorageGateway.ShutdownGateway
    (
    -- * Request
      ShutdownGateway
    -- ** Request constructor
    , shutdownGateway
    -- ** Request lenses
    , shuGatewayARN

    -- * Response
    , ShutdownGatewayResponse
    -- ** Response constructor
    , shutdownGatewayResponse
    -- ** Response lenses
    , sgrGatewayARN
    , sgrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing the of the gateway to shut down.
--
-- /See:/ 'shutdownGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'shuGatewayARN'
newtype ShutdownGateway = ShutdownGateway'
    { _shuGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ShutdownGateway' smart constructor.
shutdownGateway :: Text -> ShutdownGateway
shutdownGateway pGatewayARN =
    ShutdownGateway'
    { _shuGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
shuGatewayARN :: Lens' ShutdownGateway Text
shuGatewayARN = lens _shuGatewayARN (\ s a -> s{_shuGatewayARN = a});

instance AWSRequest ShutdownGateway where
        type Sv ShutdownGateway = StorageGateway
        type Rs ShutdownGateway = ShutdownGatewayResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ShutdownGatewayResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance ToHeaders ShutdownGateway where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ShutdownGateway" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ShutdownGateway where
        toJSON ShutdownGateway'{..}
          = object ["GatewayARN" .= _shuGatewayARN]

instance ToPath ShutdownGateway where
        toPath = const "/"

instance ToQuery ShutdownGateway where
        toQuery = const mempty

-- | A JSON object containing the of the gateway that was shut down.
--
-- /See:/ 'shutdownGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgrGatewayARN'
--
-- * 'sgrStatus'
data ShutdownGatewayResponse = ShutdownGatewayResponse'
    { _sgrGatewayARN :: !(Maybe Text)
    , _sgrStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ShutdownGatewayResponse' smart constructor.
shutdownGatewayResponse :: Int -> ShutdownGatewayResponse
shutdownGatewayResponse pStatus =
    ShutdownGatewayResponse'
    { _sgrGatewayARN = Nothing
    , _sgrStatus = pStatus
    }

-- | FIXME: Undocumented member.
sgrGatewayARN :: Lens' ShutdownGatewayResponse (Maybe Text)
sgrGatewayARN = lens _sgrGatewayARN (\ s a -> s{_sgrGatewayARN = a});

-- | FIXME: Undocumented member.
sgrStatus :: Lens' ShutdownGatewayResponse Int
sgrStatus = lens _sgrStatus (\ s a -> s{_sgrStatus = a});
