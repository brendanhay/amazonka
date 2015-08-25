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
-- Module      : Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the gateway virtual machine (VM) software. The
-- request immediately triggers the software update.
--
-- When you make this request, you get a '200 OK' success response
-- immediately. However, it might take some time for the update to
-- complete. You can call DescribeGatewayInformation to verify the gateway
-- is in the 'STATE_RUNNING' state.
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
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateGatewaySoftwareNow.html AWS API Reference> for UpdateGatewaySoftwareNow.
module Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
    (
    -- * Creating a Request
      updateGatewaySoftwareNow
    , UpdateGatewaySoftwareNow
    -- * Request Lenses
    , ugsnGatewayARN

    -- * Destructuring the Response
    , updateGatewaySoftwareNowResponse
    , UpdateGatewaySoftwareNowResponse
    -- * Response Lenses
    , ugsnrsGatewayARN
    , ugsnrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing the of the gateway to update.
--
-- /See:/ 'updateGatewaySoftwareNow' smart constructor.
newtype UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNow'
    { _ugsnGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateGatewaySoftwareNow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsnGatewayARN'
updateGatewaySoftwareNow
    :: Text -- ^ 'ugsnGatewayARN'
    -> UpdateGatewaySoftwareNow
updateGatewaySoftwareNow pGatewayARN_ =
    UpdateGatewaySoftwareNow'
    { _ugsnGatewayARN = pGatewayARN_
    }

-- | Undocumented member.
ugsnGatewayARN :: Lens' UpdateGatewaySoftwareNow Text
ugsnGatewayARN = lens _ugsnGatewayARN (\ s a -> s{_ugsnGatewayARN = a});

instance AWSRequest UpdateGatewaySoftwareNow where
        type Rs UpdateGatewaySoftwareNow =
             UpdateGatewaySoftwareNowResponse
        request = postJSON storageGateway
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
          = object
              (catMaybes [Just ("GatewayARN" .= _ugsnGatewayARN)])

instance ToPath UpdateGatewaySoftwareNow where
        toPath = const "/"

instance ToQuery UpdateGatewaySoftwareNow where
        toQuery = const mempty

-- | A JSON object containing the of the gateway that was updated.
--
-- /See:/ 'updateGatewaySoftwareNowResponse' smart constructor.
data UpdateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse'
    { _ugsnrsGatewayARN :: !(Maybe Text)
    , _ugsnrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateGatewaySoftwareNowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsnrsGatewayARN'
--
-- * 'ugsnrsStatus'
updateGatewaySoftwareNowResponse
    :: Int -- ^ 'ugsnrsStatus'
    -> UpdateGatewaySoftwareNowResponse
updateGatewaySoftwareNowResponse pStatus_ =
    UpdateGatewaySoftwareNowResponse'
    { _ugsnrsGatewayARN = Nothing
    , _ugsnrsStatus = pStatus_
    }

-- | Undocumented member.
ugsnrsGatewayARN :: Lens' UpdateGatewaySoftwareNowResponse (Maybe Text)
ugsnrsGatewayARN = lens _ugsnrsGatewayARN (\ s a -> s{_ugsnrsGatewayARN = a});

-- | The response status code.
ugsnrsStatus :: Lens' UpdateGatewaySoftwareNowResponse Int
ugsnrsStatus = lens _ugsnrsStatus (\ s a -> s{_ugsnrsStatus = a});
