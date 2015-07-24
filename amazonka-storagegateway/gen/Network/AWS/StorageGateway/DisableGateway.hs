{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DisableGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DisableGateway.html>
module Network.AWS.StorageGateway.DisableGateway
    (
    -- * Request
      DisableGateway
    -- ** Request constructor
    , disableGateway
    -- ** Request lenses
    , dGatewayARN

    -- * Response
    , DisableGatewayResponse
    -- ** Response constructor
    , disableGatewayResponse
    -- ** Response lenses
    , disrsGatewayARN
    , disrsStatus
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
-- * 'dGatewayARN'
newtype DisableGateway = DisableGateway'
    { _dGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableGateway' smart constructor.
disableGateway :: Text -> DisableGateway
disableGateway pGatewayARN_ =
    DisableGateway'
    { _dGatewayARN = pGatewayARN_
    }

-- | FIXME: Undocumented member.
dGatewayARN :: Lens' DisableGateway Text
dGatewayARN = lens _dGatewayARN (\ s a -> s{_dGatewayARN = a});

instance AWSRequest DisableGateway where
        type Sv DisableGateway = StorageGateway
        type Rs DisableGateway = DisableGatewayResponse
        request = postJSON "DisableGateway"
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
          = object ["GatewayARN" .= _dGatewayARN]

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
-- * 'disrsGatewayARN'
--
-- * 'disrsStatus'
data DisableGatewayResponse = DisableGatewayResponse'
    { _disrsGatewayARN :: !(Maybe Text)
    , _disrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableGatewayResponse' smart constructor.
disableGatewayResponse :: Int -> DisableGatewayResponse
disableGatewayResponse pStatus_ =
    DisableGatewayResponse'
    { _disrsGatewayARN = Nothing
    , _disrsStatus = pStatus_
    }

-- | The unique Amazon Resource Name of the disabled gateway.
disrsGatewayARN :: Lens' DisableGatewayResponse (Maybe Text)
disrsGatewayARN = lens _disrsGatewayARN (\ s a -> s{_disrsGatewayARN = a});

-- | FIXME: Undocumented member.
disrsStatus :: Lens' DisableGatewayResponse Int
disrsStatus = lens _disrsStatus (\ s a -> s{_disrsStatus = a});
