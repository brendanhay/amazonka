{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CancelRetrieval
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to
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
    , crrsTapeARN
    , crrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelRetrieval' smart constructor.
cancelRetrieval :: Text -> Text -> CancelRetrieval
cancelRetrieval pGatewayARN_ pTapeARN_ =
    CancelRetrieval'
    { _crGatewayARN = pGatewayARN_
    , _crTapeARN = pTapeARN_
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
        request = postJSON "CancelRetrieval"
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
-- * 'crrsTapeARN'
--
-- * 'crrsStatus'
data CancelRetrievalResponse = CancelRetrievalResponse'
    { _crrsTapeARN :: !(Maybe Text)
    , _crrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelRetrievalResponse' smart constructor.
cancelRetrievalResponse :: Int -> CancelRetrievalResponse
cancelRetrievalResponse pStatus_ =
    CancelRetrievalResponse'
    { _crrsTapeARN = Nothing
    , _crrsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval
-- was canceled.
crrsTapeARN :: Lens' CancelRetrievalResponse (Maybe Text)
crrsTapeARN = lens _crrsTapeARN (\ s a -> s{_crrsTapeARN = a});

-- | FIXME: Undocumented member.
crrsStatus :: Lens' CancelRetrievalResponse Int
crrsStatus = lens _crrsStatus (\ s a -> s{_crrsStatus = a});
