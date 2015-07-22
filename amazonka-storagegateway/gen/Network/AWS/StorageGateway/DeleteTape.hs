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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteTape.html>
module Network.AWS.StorageGateway.DeleteTape
    (
    -- * Request
      DeleteTape
    -- ** Request constructor
    , deleteTape
    -- ** Request lenses
    , dttGatewayARN
    , dttTapeARN

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
-- * 'dttGatewayARN'
--
-- * 'dttTapeARN'
data DeleteTape = DeleteTape'
    { _dttGatewayARN :: !Text
    , _dttTapeARN    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTape' smart constructor.
deleteTape :: Text -> Text -> DeleteTape
deleteTape pGatewayARN pTapeARN =
    DeleteTape'
    { _dttGatewayARN = pGatewayARN
    , _dttTapeARN = pTapeARN
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
-- * 'dtrTapeARN'
--
-- * 'dtrStatus'
data DeleteTapeResponse = DeleteTapeResponse'
    { _dtrTapeARN :: !(Maybe Text)
    , _dtrStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTapeResponse' smart constructor.
deleteTapeResponse :: Int -> DeleteTapeResponse
deleteTapeResponse pStatus =
    DeleteTapeResponse'
    { _dtrTapeARN = Nothing
    , _dtrStatus = pStatus
    }

-- | The Amazon Resource Name (ARN) of the deleted virtual tape.
dtrTapeARN :: Lens' DeleteTapeResponse (Maybe Text)
dtrTapeARN = lens _dtrTapeARN (\ s a -> s{_dtrTapeARN = a});

-- | FIXME: Undocumented member.
dtrStatus :: Lens' DeleteTapeResponse Int
dtrStatus = lens _dtrStatus (\ s a -> s{_dtrStatus = a});
