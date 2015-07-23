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
    , dttrqGatewayARN
    , dttrqTapeARN

    -- * Response
    , DeleteTapeResponse
    -- ** Response constructor
    , deleteTapeResponse
    -- ** Response lenses
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
-- * 'dttrqGatewayARN'
--
-- * 'dttrqTapeARN'
data DeleteTape = DeleteTape'
    { _dttrqGatewayARN :: !Text
    , _dttrqTapeARN    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTape' smart constructor.
deleteTape :: Text -> Text -> DeleteTape
deleteTape pGatewayARN_ pTapeARN_ =
    DeleteTape'
    { _dttrqGatewayARN = pGatewayARN_
    , _dttrqTapeARN = pTapeARN_
    }

-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual
-- tape to delete is associated with. Use the ListGateways operation to
-- return a list of gateways for your account and region.
dttrqGatewayARN :: Lens' DeleteTape Text
dttrqGatewayARN = lens _dttrqGatewayARN (\ s a -> s{_dttrqGatewayARN = a});

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
dttrqTapeARN :: Lens' DeleteTape Text
dttrqTapeARN = lens _dttrqTapeARN (\ s a -> s{_dttrqTapeARN = a});

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
              ["GatewayARN" .= _dttrqGatewayARN,
               "TapeARN" .= _dttrqTapeARN]

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

-- | FIXME: Undocumented member.
dtrsStatus :: Lens' DeleteTapeResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
