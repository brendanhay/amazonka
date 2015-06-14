{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.CancelArchival
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

-- | Cancels archiving of a virtual tape to the virtual tape shelf (VTS)
-- after the archiving process is initiated.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CancelArchival.html>
module Network.AWS.StorageGateway.CancelArchival
    (
    -- * Request
      CancelArchival
    -- ** Request constructor
    , cancelArchival
    -- ** Request lenses
    , caGatewayARN
    , caTapeARN

    -- * Response
    , CancelArchivalResponse
    -- ** Response constructor
    , cancelArchivalResponse
    -- ** Response lenses
    , carTapeARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'cancelArchival' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caGatewayARN'
--
-- * 'caTapeARN'
data CancelArchival = CancelArchival'{_caGatewayARN :: Text, _caTapeARN :: Text} deriving (Eq, Read, Show)

-- | 'CancelArchival' smart constructor.
cancelArchival :: Text -> Text -> CancelArchival
cancelArchival pGatewayARN pTapeARN = CancelArchival'{_caGatewayARN = pGatewayARN, _caTapeARN = pTapeARN};

-- | FIXME: Undocumented member.
caGatewayARN :: Lens' CancelArchival Text
caGatewayARN = lens _caGatewayARN (\ s a -> s{_caGatewayARN = a});

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- archiving for.
caTapeARN :: Lens' CancelArchival Text
caTapeARN = lens _caTapeARN (\ s a -> s{_caTapeARN = a});

instance AWSRequest CancelArchival where
        type Sv CancelArchival = StorageGateway
        type Rs CancelArchival = CancelArchivalResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CancelArchivalResponse' <$> x .:> "TapeARN")

instance ToHeaders CancelArchival where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CancelArchival" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelArchival where
        toJSON CancelArchival'{..}
          = object
              ["GatewayARN" .= _caGatewayARN,
               "TapeARN" .= _caTapeARN]

instance ToPath CancelArchival where
        toPath = const "/"

instance ToQuery CancelArchival where
        toQuery = const mempty

-- | /See:/ 'cancelArchivalResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carTapeARN'
newtype CancelArchivalResponse = CancelArchivalResponse'{_carTapeARN :: Text} deriving (Eq, Read, Show)

-- | 'CancelArchivalResponse' smart constructor.
cancelArchivalResponse :: Text -> CancelArchivalResponse
cancelArchivalResponse pTapeARN = CancelArchivalResponse'{_carTapeARN = pTapeARN};

-- | The Amazon Resource Name (ARN) of the virtual tape for which archiving
-- was canceled.
carTapeARN :: Lens' CancelArchivalResponse Text
carTapeARN = lens _carTapeARN (\ s a -> s{_carTapeARN = a});
