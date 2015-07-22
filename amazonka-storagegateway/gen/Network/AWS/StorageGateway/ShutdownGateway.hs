{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ShutdownGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation shuts down a gateway. To specify which gateway to shut
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
    , srqGatewayARN

    -- * Response
    , ShutdownGatewayResponse
    -- ** Response constructor
    , shutdownGatewayResponse
    -- ** Response lenses
    , sgrsGatewayARN
    , sgrsStatus
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
-- * 'srqGatewayARN'
newtype ShutdownGateway = ShutdownGateway'
    { _srqGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ShutdownGateway' smart constructor.
shutdownGateway :: Text -> ShutdownGateway
shutdownGateway pGatewayARN_ =
    ShutdownGateway'
    { _srqGatewayARN = pGatewayARN_
    }

-- | FIXME: Undocumented member.
srqGatewayARN :: Lens' ShutdownGateway Text
srqGatewayARN = lens _srqGatewayARN (\ s a -> s{_srqGatewayARN = a});

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
          = object ["GatewayARN" .= _srqGatewayARN]

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
-- * 'sgrsGatewayARN'
--
-- * 'sgrsStatus'
data ShutdownGatewayResponse = ShutdownGatewayResponse'
    { _sgrsGatewayARN :: !(Maybe Text)
    , _sgrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ShutdownGatewayResponse' smart constructor.
shutdownGatewayResponse :: Int -> ShutdownGatewayResponse
shutdownGatewayResponse pStatus_ =
    ShutdownGatewayResponse'
    { _sgrsGatewayARN = Nothing
    , _sgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
sgrsGatewayARN :: Lens' ShutdownGatewayResponse (Maybe Text)
sgrsGatewayARN = lens _sgrsGatewayARN (\ s a -> s{_sgrsGatewayARN = a});

-- | FIXME: Undocumented member.
sgrsStatus :: Lens' ShutdownGatewayResponse Int
sgrsStatus = lens _sgrsStatus (\ s a -> s{_sgrsStatus = a});
