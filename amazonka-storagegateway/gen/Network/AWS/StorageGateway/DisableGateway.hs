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
-- Module      : Network.AWS.StorageGateway.DisableGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a tape gateway when the gateway is no longer functioning. For example, if your gateway VM is damaged, you can disable the gateway so you can recover virtual tapes.
--
--
-- Use this operation for a tape gateway that is not reachable or not functioning. This operation is only supported in the tape gateway type.
--
-- /Important:/ Once a gateway is disabled it cannot be enabled.
--
module Network.AWS.StorageGateway.DisableGateway
    (
    -- * Creating a Request
      disableGateway
    , DisableGateway
    -- * Request Lenses
    , dGatewayARN

    -- * Destructuring the Response
    , disableGatewayResponse
    , DisableGatewayResponse
    -- * Response Lenses
    , disrsGatewayARN
    , disrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | DisableGatewayInput
--
--
--
-- /See:/ 'disableGateway' smart constructor.
newtype DisableGateway = DisableGateway'
  { _dGatewayARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dGatewayARN' - Undocumented member.
disableGateway
    :: Text -- ^ 'dGatewayARN'
    -> DisableGateway
disableGateway pGatewayARN_ = DisableGateway' {_dGatewayARN = pGatewayARN_}


-- | Undocumented member.
dGatewayARN :: Lens' DisableGateway Text
dGatewayARN = lens _dGatewayARN (\ s a -> s{_dGatewayARN = a})

instance AWSRequest DisableGateway where
        type Rs DisableGateway = DisableGatewayResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DisableGatewayResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance Hashable DisableGateway where

instance NFData DisableGateway where

instance ToHeaders DisableGateway where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DisableGateway" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableGateway where
        toJSON DisableGateway'{..}
          = object
              (catMaybes [Just ("GatewayARN" .= _dGatewayARN)])

instance ToPath DisableGateway where
        toPath = const "/"

instance ToQuery DisableGateway where
        toQuery = const mempty

-- | DisableGatewayOutput
--
--
--
-- /See:/ 'disableGatewayResponse' smart constructor.
data DisableGatewayResponse = DisableGatewayResponse'
  { _disrsGatewayARN     :: !(Maybe Text)
  , _disrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsGatewayARN' - The unique Amazon Resource Name of the disabled gateway.
--
-- * 'disrsResponseStatus' - -- | The response status code.
disableGatewayResponse
    :: Int -- ^ 'disrsResponseStatus'
    -> DisableGatewayResponse
disableGatewayResponse pResponseStatus_ =
  DisableGatewayResponse'
    {_disrsGatewayARN = Nothing, _disrsResponseStatus = pResponseStatus_}


-- | The unique Amazon Resource Name of the disabled gateway.
disrsGatewayARN :: Lens' DisableGatewayResponse (Maybe Text)
disrsGatewayARN = lens _disrsGatewayARN (\ s a -> s{_disrsGatewayARN = a})

-- | -- | The response status code.
disrsResponseStatus :: Lens' DisableGatewayResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\ s a -> s{_disrsResponseStatus = a})

instance NFData DisableGatewayResponse where
