{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTape
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteTape.html AWS API Reference> for DeleteTape.
module Network.AWS.StorageGateway.DeleteTape
    (
    -- * Creating a Request
      DeleteTape
    , deleteTape
    -- * Request Lenses
    , dttGatewayARN
    , dttTapeARN

    -- * Destructuring the Response
    , DeleteTapeResponse
    , deleteTapeResponse
    -- * Response Lenses
    , dtrsTapeARN
    , dtrsStatus
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
-- * 'dttGatewayARN'
--
-- * 'dttTapeARN'
data DeleteTape = DeleteTape'
    { _dttGatewayARN :: !Text
    , _dttTapeARN    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTape' smart constructor.
deleteTape :: Text -> Text -> DeleteTape
deleteTape pGatewayARN_ pTapeARN_ =
    DeleteTape'
    { _dttGatewayARN = pGatewayARN_
    , _dttTapeARN = pTapeARN_
    }

-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual
-- tape to delete is associated with. Use the ListGateways operation to
-- return a list of gateways for your account and region.
dttGatewayARN :: Lens' DeleteTape Text
dttGatewayARN = lens _dttGatewayARN (\ s a -> s{_dttGatewayARN = a});

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
dttTapeARN :: Lens' DeleteTape Text
dttTapeARN = lens _dttTapeARN (\ s a -> s{_dttTapeARN = a});

instance AWSRequest DeleteTape where
        type Sv DeleteTape = StorageGateway
        type Rs DeleteTape = DeleteTapeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTapeResponse' <$>
                   (x .?> "TapeARN") <*> (pure (fromEnum s)))

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
              ["GatewayARN" .= _dttGatewayARN,
               "TapeARN" .= _dttTapeARN]

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
-- * 'dtrsTapeARN'
--
-- * 'dtrsStatus'
data DeleteTapeResponse = DeleteTapeResponse'
    { _dtrsTapeARN :: !(Maybe Text)
    , _dtrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTapeResponse' smart constructor.
deleteTapeResponse :: Int -> DeleteTapeResponse
deleteTapeResponse pStatus_ =
    DeleteTapeResponse'
    { _dtrsTapeARN = Nothing
    , _dtrsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted virtual tape.
dtrsTapeARN :: Lens' DeleteTapeResponse (Maybe Text)
dtrsTapeARN = lens _dtrsTapeARN (\ s a -> s{_dtrsTapeARN = a});

-- | Undocumented member.
dtrsStatus :: Lens' DeleteTapeResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
