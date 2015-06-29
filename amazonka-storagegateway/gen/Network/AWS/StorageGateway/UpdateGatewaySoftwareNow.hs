{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
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

-- | This operation updates the gateway virtual machine (VM) software. The
-- request immediately triggers the software update.
--
-- When you make this request, you get a @200 OK@ success response
-- immediately. However, it might take some time for the update to
-- complete. You can call DescribeGatewayInformation to verify the gateway
-- is in the @STATE_RUNNING@ state.
--
-- A software update forces a system restart of your gateway. You can
-- minimize the chance of any disruption to your applications by increasing
-- your iSCSI Initiators\' timeouts. For more information about increasing
-- iSCSI Initiator timeouts for Windows and Linux, see
-- <http://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorWindowsClient.html#CustomizeWindowsiSCSISettings Customizing Your Windows iSCSI Settings>
-- and
-- <http://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorRedHatClient.html#CustomizeLinuxiSCSISettings Customizing Your Linux iSCSI Settings>,
-- respectively.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateGatewaySoftwareNow.html>
module Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
    (
    -- * Request
      UpdateGatewaySoftwareNow
    -- ** Request constructor
    , updateGatewaySoftwareNow
    -- ** Request lenses
    , ugsnGatewayARN

    -- * Response
    , UpdateGatewaySoftwareNowResponse
    -- ** Response constructor
    , updateGatewaySoftwareNowResponse
    -- ** Response lenses
    , ugsnrGatewayARN
    , ugsnrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing the of the gateway to update.
--
-- /See:/ 'updateGatewaySoftwareNow' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugsnGatewayARN'
newtype UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNow'
    { _ugsnGatewayARN :: Text
    } deriving (Eq,Read,Show)

-- | 'UpdateGatewaySoftwareNow' smart constructor.
updateGatewaySoftwareNow :: Text -> UpdateGatewaySoftwareNow
updateGatewaySoftwareNow pGatewayARN =
    UpdateGatewaySoftwareNow'
    { _ugsnGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
ugsnGatewayARN :: Lens' UpdateGatewaySoftwareNow Text
ugsnGatewayARN = lens _ugsnGatewayARN (\ s a -> s{_ugsnGatewayARN = a});

instance AWSRequest UpdateGatewaySoftwareNow where
        type Sv UpdateGatewaySoftwareNow = StorageGateway
        type Rs UpdateGatewaySoftwareNow =
             UpdateGatewaySoftwareNowResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGatewaySoftwareNowResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance ToHeaders UpdateGatewaySoftwareNow where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.UpdateGatewaySoftwareNow"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGatewaySoftwareNow where
        toJSON UpdateGatewaySoftwareNow'{..}
          = object ["GatewayARN" .= _ugsnGatewayARN]

instance ToPath UpdateGatewaySoftwareNow where
        toPath = const "/"

instance ToQuery UpdateGatewaySoftwareNow where
        toQuery = const mempty

-- | A JSON object containing the of the gateway that was updated.
--
-- /See:/ 'updateGatewaySoftwareNowResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugsnrGatewayARN'
--
-- * 'ugsnrStatus'
data UpdateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse'
    { _ugsnrGatewayARN :: !(Maybe Text)
    , _ugsnrStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'UpdateGatewaySoftwareNowResponse' smart constructor.
updateGatewaySoftwareNowResponse :: Int -> UpdateGatewaySoftwareNowResponse
updateGatewaySoftwareNowResponse pStatus =
    UpdateGatewaySoftwareNowResponse'
    { _ugsnrGatewayARN = Nothing
    , _ugsnrStatus = pStatus
    }

-- | FIXME: Undocumented member.
ugsnrGatewayARN :: Lens' UpdateGatewaySoftwareNowResponse (Maybe Text)
ugsnrGatewayARN = lens _ugsnrGatewayARN (\ s a -> s{_ugsnrGatewayARN = a});

-- | FIXME: Undocumented member.
ugsnrStatus :: Lens' UpdateGatewaySoftwareNowResponse Int
ugsnrStatus = lens _ugsnrStatus (\ s a -> s{_ugsnrStatus = a});
