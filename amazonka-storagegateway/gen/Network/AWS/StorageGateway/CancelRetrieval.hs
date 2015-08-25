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
-- Module      : Network.AWS.StorageGateway.CancelRetrieval
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to
-- a gateway after the retrieval process is initiated. The virtual tape is
-- returned to the VTS.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CancelRetrieval.html AWS API Reference> for CancelRetrieval.
module Network.AWS.StorageGateway.CancelRetrieval
    (
    -- * Creating a Request
      cancelRetrieval
    , CancelRetrieval
    -- * Request Lenses
    , crGatewayARN
    , crTapeARN

    -- * Destructuring the Response
    , cancelRetrievalResponse
    , CancelRetrievalResponse
    -- * Response Lenses
    , crrsTapeARN
    , crrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | CancelRetrievalInput
--
-- /See:/ 'cancelRetrieval' smart constructor.
data CancelRetrieval = CancelRetrieval'
    { _crGatewayARN :: !Text
    , _crTapeARN    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelRetrieval' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crGatewayARN'
--
-- * 'crTapeARN'
cancelRetrieval
    :: Text -- ^ 'crGatewayARN'
    -> Text -- ^ 'crTapeARN'
    -> CancelRetrieval
cancelRetrieval pGatewayARN_ pTapeARN_ =
    CancelRetrieval'
    { _crGatewayARN = pGatewayARN_
    , _crTapeARN = pTapeARN_
    }

-- | Undocumented member.
crGatewayARN :: Lens' CancelRetrieval Text
crGatewayARN = lens _crGatewayARN (\ s a -> s{_crGatewayARN = a});

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- retrieval for.
crTapeARN :: Lens' CancelRetrieval Text
crTapeARN = lens _crTapeARN (\ s a -> s{_crTapeARN = a});

instance AWSRequest CancelRetrieval where
        type Rs CancelRetrieval = CancelRetrievalResponse
        request = postJSON storageGateway
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
              (catMaybes
                 [Just ("GatewayARN" .= _crGatewayARN),
                  Just ("TapeARN" .= _crTapeARN)])

instance ToPath CancelRetrieval where
        toPath = const "/"

instance ToQuery CancelRetrieval where
        toQuery = const mempty

-- | CancelRetrievalOutput
--
-- /See:/ 'cancelRetrievalResponse' smart constructor.
data CancelRetrievalResponse = CancelRetrievalResponse'
    { _crrsTapeARN :: !(Maybe Text)
    , _crrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelRetrievalResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsTapeARN'
--
-- * 'crrsStatus'
cancelRetrievalResponse
    :: Int -- ^ 'crrsStatus'
    -> CancelRetrievalResponse
cancelRetrievalResponse pStatus_ =
    CancelRetrievalResponse'
    { _crrsTapeARN = Nothing
    , _crrsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval
-- was canceled.
crrsTapeARN :: Lens' CancelRetrievalResponse (Maybe Text)
crrsTapeARN = lens _crrsTapeARN (\ s a -> s{_crrsTapeARN = a});

-- | The response status code.
crrsStatus :: Lens' CancelRetrievalResponse Int
crrsStatus = lens _crrsStatus (\ s a -> s{_crrsStatus = a});
