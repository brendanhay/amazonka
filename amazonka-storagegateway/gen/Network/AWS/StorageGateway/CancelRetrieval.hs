{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.CancelRetrieval
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

-- | Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to
-- a gateway after the retrieval process is initiated. The virtual tape is
-- returned to the VTS.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CancelRetrieval.html>
module Network.AWS.StorageGateway.CancelRetrieval
    (
    -- * Request
      CancelRetrieval
    -- ** Request constructor
    , cancelRetrieval
    -- ** Request lenses
    , crGatewayARN
    , crTapeARN

    -- * Response
    , CancelRetrievalResponse
    -- ** Response constructor
    , cancelRetrievalResponse
    -- ** Response lenses
    , crrTapeARN
    , crrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | CancelRetrievalInput
--
-- /See:/ 'cancelRetrieval' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crGatewayARN'
--
-- * 'crTapeARN'
data CancelRetrieval = CancelRetrieval'
    { _crGatewayARN :: !Text
    , _crTapeARN    :: !Text
    } deriving (Eq,Read,Show)

-- | 'CancelRetrieval' smart constructor.
cancelRetrieval :: Text -> Text -> CancelRetrieval
cancelRetrieval pGatewayARN pTapeARN =
    CancelRetrieval'
    { _crGatewayARN = pGatewayARN
    , _crTapeARN = pTapeARN
    }

-- | FIXME: Undocumented member.
crGatewayARN :: Lens' CancelRetrieval Text
crGatewayARN = lens _crGatewayARN (\ s a -> s{_crGatewayARN = a});

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- retrieval for.
crTapeARN :: Lens' CancelRetrieval Text
crTapeARN = lens _crTapeARN (\ s a -> s{_crTapeARN = a});

instance AWSRequest CancelRetrieval where
        type Sv CancelRetrieval = StorageGateway
        type Rs CancelRetrieval = CancelRetrievalResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CancelRetrievalResponse' <$>
                   (x .?> "TapeARN") <*> (pure (fromEnum s)))

instance ToHeaders CancelRetrieval where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CancelRetrieval" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelRetrieval where
        toJSON CancelRetrieval'{..}
          = object
              ["GatewayARN" .= _crGatewayARN,
               "TapeARN" .= _crTapeARN]

instance ToPath CancelRetrieval where
        toPath = const "/"

instance ToQuery CancelRetrieval where
        toQuery = const mempty

-- | CancelRetrievalOutput
--
-- /See:/ 'cancelRetrievalResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrTapeARN'
--
-- * 'crrStatus'
data CancelRetrievalResponse = CancelRetrievalResponse'
    { _crrTapeARN :: !(Maybe Text)
    , _crrStatus  :: !Int
    } deriving (Eq,Read,Show)

-- | 'CancelRetrievalResponse' smart constructor.
cancelRetrievalResponse :: Int -> CancelRetrievalResponse
cancelRetrievalResponse pStatus =
    CancelRetrievalResponse'
    { _crrTapeARN = Nothing
    , _crrStatus = pStatus
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval
-- was canceled.
crrTapeARN :: Lens' CancelRetrievalResponse (Maybe Text)
crrTapeARN = lens _crrTapeARN (\ s a -> s{_crrTapeARN = a});

-- | FIXME: Undocumented member.
crrStatus :: Lens' CancelRetrievalResponse Int
crrStatus = lens _crrStatus (\ s a -> s{_crrStatus = a});
