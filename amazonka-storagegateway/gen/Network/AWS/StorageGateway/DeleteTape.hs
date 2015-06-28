{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.DeleteTape
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

-- | Deletes the specified virtual tape.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteTape.html>
module Network.AWS.StorageGateway.DeleteTape
    (
    -- * Request
      DeleteTape
    -- ** Request constructor
    , deleteTape
    -- ** Request lenses
    , dt1GatewayARN
    , dt1TapeARN

    -- * Response
    , DeleteTapeResponse
    -- ** Response constructor
    , deleteTapeResponse
    -- ** Response lenses
    , dtrTapeARN
    , dtrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | DeleteTapeInput
--
-- /See:/ 'deleteTape' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dt1GatewayARN'
--
-- * 'dt1TapeARN'
data DeleteTape = DeleteTape'
    { _dt1GatewayARN :: !Text
    , _dt1TapeARN    :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeleteTape' smart constructor.
deleteTape :: Text -> Text -> DeleteTape
deleteTape pGatewayARN pTapeARN =
    DeleteTape'
    { _dt1GatewayARN = pGatewayARN
    , _dt1TapeARN = pTapeARN
    }

-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual
-- tape to delete is associated with. Use the ListGateways operation to
-- return a list of gateways for your account and region.
dt1GatewayARN :: Lens' DeleteTape Text
dt1GatewayARN = lens _dt1GatewayARN (\ s a -> s{_dt1GatewayARN = a});

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
dt1TapeARN :: Lens' DeleteTape Text
dt1TapeARN = lens _dt1TapeARN (\ s a -> s{_dt1TapeARN = a});

instance AWSRequest DeleteTape where
        type Sv DeleteTape = StorageGateway
        type Rs DeleteTape = DeleteTapeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTapeResponse' <$>
                   (x .?> "TapeARN") <*> (pure s))

instance ToHeaders DeleteTape where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DeleteTape" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTape where
        toJSON DeleteTape'{..}
          = object
              ["GatewayARN" .= _dt1GatewayARN,
               "TapeARN" .= _dt1TapeARN]

instance ToPath DeleteTape where
        toPath = const "/"

instance ToQuery DeleteTape where
        toQuery = const mempty

-- | DeleteTapeOutput
--
-- /See:/ 'deleteTapeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTapeARN'
--
-- * 'dtrStatus'
data DeleteTapeResponse = DeleteTapeResponse'
    { _dtrTapeARN :: !(Maybe Text)
    , _dtrStatus  :: !Status
    } deriving (Eq,Read,Show)

-- | 'DeleteTapeResponse' smart constructor.
deleteTapeResponse :: Status -> DeleteTapeResponse
deleteTapeResponse pStatus =
    DeleteTapeResponse'
    { _dtrTapeARN = Nothing
    , _dtrStatus = pStatus
    }

-- | The Amazon Resource Name (ARN) of the deleted virtual tape.
dtrTapeARN :: Lens' DeleteTapeResponse (Maybe Text)
dtrTapeARN = lens _dtrTapeARN (\ s a -> s{_dtrTapeARN = a});

-- | FIXME: Undocumented member.
dtrStatus :: Lens' DeleteTapeResponse Status
dtrStatus = lens _dtrStatus (\ s a -> s{_dtrStatus = a});
