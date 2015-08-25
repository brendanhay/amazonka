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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a gateway when the gateway is no longer functioning. For
-- example, if your gateway VM is damaged, you can disable the gateway so
-- you can recover virtual tapes.
--
-- Use this operation for a gateway-VTL that is not reachable or not
-- functioning.
--
-- Once a gateway is disabled it cannot be enabled.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DisableGateway.html AWS API Reference> for DisableGateway.
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
    , disrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | DisableGatewayInput
--
-- /See:/ 'disableGateway' smart constructor.
newtype DisableGateway = DisableGateway'
    { _dGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dGatewayARN'
disableGateway
    :: Text -- ^ 'dGatewayARN'
    -> DisableGateway
disableGateway pGatewayARN_ =
    DisableGateway'
    { _dGatewayARN = pGatewayARN_
    }

-- | Undocumented member.
dGatewayARN :: Lens' DisableGateway Text
dGatewayARN = lens _dGatewayARN (\ s a -> s{_dGatewayARN = a});

instance AWSRequest DisableGateway where
        type Rs DisableGateway = DisableGatewayResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DisableGatewayResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

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
-- /See:/ 'disableGatewayResponse' smart constructor.
data DisableGatewayResponse = DisableGatewayResponse'
    { _disrsGatewayARN :: !(Maybe Text)
    , _disrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsGatewayARN'
--
-- * 'disrsStatus'
disableGatewayResponse
    :: Int -- ^ 'disrsStatus'
    -> DisableGatewayResponse
disableGatewayResponse pStatus_ =
    DisableGatewayResponse'
    { _disrsGatewayARN = Nothing
    , _disrsStatus = pStatus_
    }

-- | The unique Amazon Resource Name of the disabled gateway.
disrsGatewayARN :: Lens' DisableGatewayResponse (Maybe Text)
disrsGatewayARN = lens _disrsGatewayARN (\ s a -> s{_disrsGatewayARN = a});

-- | The response status code.
disrsStatus :: Lens' DisableGatewayResponse Int
disrsStatus = lens _disrsStatus (\ s a -> s{_disrsStatus = a});
