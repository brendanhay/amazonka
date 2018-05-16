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
-- Module      : Network.AWS.StorageGateway.ShutdownGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shuts down a gateway. To specify which gateway to shut down, use the Amazon Resource Name (ARN) of the gateway in the body of your request.
--
--
-- The operation shuts down the gateway service component running in the gateway's virtual machine (VM) and not the host VM.
--
-- After the gateway is shutdown, you cannot call any other API except 'StartGateway' , 'DescribeGatewayInformation' , and 'ListGateways' . For more information, see 'ActivateGateway' . Your applications cannot read from or write to the gateway's storage volumes, and there are no snapshots taken.
--
-- If do not intend to use the gateway again, you must delete the gateway (using 'DeleteGateway' ) to no longer pay software charges associated with the gateway.
--
module Network.AWS.StorageGateway.ShutdownGateway
    (
    -- * Creating a Request
      shutdownGateway
    , ShutdownGateway
    -- * Request Lenses
    , sGatewayARN

    -- * Destructuring the Response
    , shutdownGatewayResponse
    , ShutdownGatewayResponse
    -- * Response Lenses
    , srsGatewayARN
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing the of the gateway to shut down.
--
--
--
-- /See:/ 'shutdownGateway' smart constructor.
newtype ShutdownGateway = ShutdownGateway'
  { _sGatewayARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ShutdownGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sGatewayARN' - Undocumented member.
shutdownGateway
    :: Text -- ^ 'sGatewayARN'
    -> ShutdownGateway
shutdownGateway pGatewayARN_ = ShutdownGateway' {_sGatewayARN = pGatewayARN_}


-- | Undocumented member.
sGatewayARN :: Lens' ShutdownGateway Text
sGatewayARN = lens _sGatewayARN (\ s a -> s{_sGatewayARN = a})

instance AWSRequest ShutdownGateway where
        type Rs ShutdownGateway = ShutdownGatewayResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 ShutdownGatewayResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance Hashable ShutdownGateway where

instance NFData ShutdownGateway where

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
          = object
              (catMaybes [Just ("GatewayARN" .= _sGatewayARN)])

instance ToPath ShutdownGateway where
        toPath = const "/"

instance ToQuery ShutdownGateway where
        toQuery = const mempty

-- | A JSON object containing the of the gateway that was shut down.
--
--
--
-- /See:/ 'shutdownGatewayResponse' smart constructor.
data ShutdownGatewayResponse = ShutdownGatewayResponse'
  { _srsGatewayARN     :: !(Maybe Text)
  , _srsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ShutdownGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsGatewayARN' - Undocumented member.
--
-- * 'srsResponseStatus' - -- | The response status code.
shutdownGatewayResponse
    :: Int -- ^ 'srsResponseStatus'
    -> ShutdownGatewayResponse
shutdownGatewayResponse pResponseStatus_ =
  ShutdownGatewayResponse'
    {_srsGatewayARN = Nothing, _srsResponseStatus = pResponseStatus_}


-- | Undocumented member.
srsGatewayARN :: Lens' ShutdownGatewayResponse (Maybe Text)
srsGatewayARN = lens _srsGatewayARN (\ s a -> s{_srsGatewayARN = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' ShutdownGatewayResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData ShutdownGatewayResponse where
