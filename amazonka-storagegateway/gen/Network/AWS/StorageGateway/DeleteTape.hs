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
-- Module      : Network.AWS.StorageGateway.DeleteTape
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape. This operation is only supported in the tape gateway type.
--
--
module Network.AWS.StorageGateway.DeleteTape
    (
    -- * Creating a Request
      deleteTape
    , DeleteTape
    -- * Request Lenses
    , delGatewayARN
    , delTapeARN

    -- * Destructuring the Response
    , deleteTapeResponse
    , DeleteTapeResponse
    -- * Response Lenses
    , dtrsTapeARN
    , dtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | DeleteTapeInput
--
--
--
-- /See:/ 'deleteTape' smart constructor.
data DeleteTape = DeleteTape'
  { _delGatewayARN :: !Text
  , _delTapeARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTape' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delGatewayARN' - The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and region.
--
-- * 'delTapeARN' - The Amazon Resource Name (ARN) of the virtual tape to delete.
deleteTape
    :: Text -- ^ 'delGatewayARN'
    -> Text -- ^ 'delTapeARN'
    -> DeleteTape
deleteTape pGatewayARN_ pTapeARN_ =
  DeleteTape' {_delGatewayARN = pGatewayARN_, _delTapeARN = pTapeARN_}


-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and region.
delGatewayARN :: Lens' DeleteTape Text
delGatewayARN = lens _delGatewayARN (\ s a -> s{_delGatewayARN = a})

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
delTapeARN :: Lens' DeleteTape Text
delTapeARN = lens _delTapeARN (\ s a -> s{_delTapeARN = a})

instance AWSRequest DeleteTape where
        type Rs DeleteTape = DeleteTapeResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTapeResponse' <$>
                   (x .?> "TapeARN") <*> (pure (fromEnum s)))

instance Hashable DeleteTape where

instance NFData DeleteTape where

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
              (catMaybes
                 [Just ("GatewayARN" .= _delGatewayARN),
                  Just ("TapeARN" .= _delTapeARN)])

instance ToPath DeleteTape where
        toPath = const "/"

instance ToQuery DeleteTape where
        toQuery = const mempty

-- | DeleteTapeOutput
--
--
--
-- /See:/ 'deleteTapeResponse' smart constructor.
data DeleteTapeResponse = DeleteTapeResponse'
  { _dtrsTapeARN        :: !(Maybe Text)
  , _dtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTapeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsTapeARN' - The Amazon Resource Name (ARN) of the deleted virtual tape.
--
-- * 'dtrsResponseStatus' - -- | The response status code.
deleteTapeResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DeleteTapeResponse
deleteTapeResponse pResponseStatus_ =
  DeleteTapeResponse'
    {_dtrsTapeARN = Nothing, _dtrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the deleted virtual tape.
dtrsTapeARN :: Lens' DeleteTapeResponse (Maybe Text)
dtrsTapeARN = lens _dtrsTapeARN (\ s a -> s{_dtrsTapeARN = a})

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DeleteTapeResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DeleteTapeResponse where
