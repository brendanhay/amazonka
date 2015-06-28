{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.DisableGateway
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

-- | Disables a gateway when the gateway is no longer functioning. For
-- example, if your gateway VM is damaged, you can disable the gateway so
-- you can recover virtual tapes.
--
-- Use this operation for a gateway-VTL that is not reachable or not
-- functioning.
--
-- Once a gateway is disabled it cannot be enabled.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DisableGateway.html>
module Network.AWS.StorageGateway.DisableGateway
    (
    -- * Request
      DisableGateway
    -- ** Request constructor
    , disableGateway
    -- ** Request lenses
    , disGatewayARN

    -- * Response
    , DisableGatewayResponse
    -- ** Response constructor
    , disableGatewayResponse
    -- ** Response lenses
    , dGatewayARN
    , dStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | DisableGatewayInput
--
-- /See:/ 'disableGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disGatewayARN'
newtype DisableGateway = DisableGateway'
    { _disGatewayARN :: Text
    } deriving (Eq,Read,Show)

-- | 'DisableGateway' smart constructor.
disableGateway :: Text -> DisableGateway
disableGateway pGatewayARN =
    DisableGateway'
    { _disGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
disGatewayARN :: Lens' DisableGateway Text
disGatewayARN = lens _disGatewayARN (\ s a -> s{_disGatewayARN = a});

instance AWSRequest DisableGateway where
        type Sv DisableGateway = StorageGateway
        type Rs DisableGateway = DisableGatewayResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DisableGatewayResponse' <$>
                   (x .?> "GatewayARN") <*> (pure s))

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
          = object ["GatewayARN" .= _disGatewayARN]

instance ToPath DisableGateway where
        toPath = const "/"

instance ToQuery DisableGateway where
        toQuery = const mempty

-- | DisableGatewayOutput
--
-- /See:/ 'disableGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dGatewayARN'
--
-- * 'dStatus'
data DisableGatewayResponse = DisableGatewayResponse'
    { _dGatewayARN :: !(Maybe Text)
    , _dStatus     :: !Status
    } deriving (Eq,Show)

-- | 'DisableGatewayResponse' smart constructor.
disableGatewayResponse :: Status -> DisableGatewayResponse
disableGatewayResponse pStatus =
    DisableGatewayResponse'
    { _dGatewayARN = Nothing
    , _dStatus = pStatus
    }

-- | The unique Amazon Resource Name of the disabled gateway.
dGatewayARN :: Lens' DisableGatewayResponse (Maybe Text)
dGatewayARN = lens _dGatewayARN (\ s a -> s{_dGatewayARN = a});

-- | FIXME: Undocumented member.
dStatus :: Lens' DisableGatewayResponse Status
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});
