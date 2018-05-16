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
-- Module      : Network.AWS.StorageGateway.StartGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a gateway that you previously shut down (see 'ShutdownGateway' ). After the gateway starts, you can then make other API calls, your applications can read from or write to the gateway's storage volumes and you will be able to take snapshot backups.
--
--
-- To specify which gateway to start, use the Amazon Resource Name (ARN) of the gateway in your request.
--
module Network.AWS.StorageGateway.StartGateway
    (
    -- * Creating a Request
      startGateway
    , StartGateway
    -- * Request Lenses
    , sgGatewayARN

    -- * Destructuring the Response
    , startGatewayResponse
    , StartGatewayResponse
    -- * Response Lenses
    , sgrsGatewayARN
    , sgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing the of the gateway to start.
--
--
--
-- /See:/ 'startGateway' smart constructor.
newtype StartGateway = StartGateway'
  { _sgGatewayARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgGatewayARN' - Undocumented member.
startGateway
    :: Text -- ^ 'sgGatewayARN'
    -> StartGateway
startGateway pGatewayARN_ = StartGateway' {_sgGatewayARN = pGatewayARN_}


-- | Undocumented member.
sgGatewayARN :: Lens' StartGateway Text
sgGatewayARN = lens _sgGatewayARN (\ s a -> s{_sgGatewayARN = a})

instance AWSRequest StartGateway where
        type Rs StartGateway = StartGatewayResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 StartGatewayResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance Hashable StartGateway where

instance NFData StartGateway where

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
          = object
              (catMaybes [Just ("GatewayARN" .= _sgGatewayARN)])

instance ToPath StartGateway where
        toPath = const "/"

instance ToQuery StartGateway where
        toQuery = const mempty

-- | A JSON object containing the of the gateway that was restarted.
--
--
--
-- /See:/ 'startGatewayResponse' smart constructor.
data StartGatewayResponse = StartGatewayResponse'
  { _sgrsGatewayARN     :: !(Maybe Text)
  , _sgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgrsGatewayARN' - Undocumented member.
--
-- * 'sgrsResponseStatus' - -- | The response status code.
startGatewayResponse
    :: Int -- ^ 'sgrsResponseStatus'
    -> StartGatewayResponse
startGatewayResponse pResponseStatus_ =
  StartGatewayResponse'
    {_sgrsGatewayARN = Nothing, _sgrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
sgrsGatewayARN :: Lens' StartGatewayResponse (Maybe Text)
sgrsGatewayARN = lens _sgrsGatewayARN (\ s a -> s{_sgrsGatewayARN = a})

-- | -- | The response status code.
sgrsResponseStatus :: Lens' StartGatewayResponse Int
sgrsResponseStatus = lens _sgrsResponseStatus (\ s a -> s{_sgrsResponseStatus = a})

instance NFData StartGatewayResponse where
