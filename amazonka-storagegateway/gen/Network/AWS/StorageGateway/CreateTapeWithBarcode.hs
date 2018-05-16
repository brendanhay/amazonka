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
-- Module      : Network.AWS.StorageGateway.CreateTapeWithBarcode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual tape by using your own barcode. You write data to the virtual tape and then archive the tape. A barcode is unique and can not be reused if it has already been used on a tape . This applies to barcodes used on deleted tapes. This operation is only supported in the tape gateway type.
--
--
module Network.AWS.StorageGateway.CreateTapeWithBarcode
    (
    -- * Creating a Request
      createTapeWithBarcode
    , CreateTapeWithBarcode
    -- * Request Lenses
    , ctwbGatewayARN
    , ctwbTapeSizeInBytes
    , ctwbTapeBarcode

    -- * Destructuring the Response
    , createTapeWithBarcodeResponse
    , CreateTapeWithBarcodeResponse
    -- * Response Lenses
    , ctwbrsTapeARN
    , ctwbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | CreateTapeWithBarcodeInput
--
--
--
-- /See:/ 'createTapeWithBarcode' smart constructor.
data CreateTapeWithBarcode = CreateTapeWithBarcode'
  { _ctwbGatewayARN      :: !Text
  , _ctwbTapeSizeInBytes :: !Integer
  , _ctwbTapeBarcode     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTapeWithBarcode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctwbGatewayARN' - The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tape with. Use the 'ListGateways' operation to return a list of gateways for your account and region.
--
-- * 'ctwbTapeSizeInBytes' - The size, in bytes, of the virtual tape that you want to create.
--
-- * 'ctwbTapeBarcode' - The barcode that you want to assign to the tape.
createTapeWithBarcode
    :: Text -- ^ 'ctwbGatewayARN'
    -> Integer -- ^ 'ctwbTapeSizeInBytes'
    -> Text -- ^ 'ctwbTapeBarcode'
    -> CreateTapeWithBarcode
createTapeWithBarcode pGatewayARN_ pTapeSizeInBytes_ pTapeBarcode_ =
  CreateTapeWithBarcode'
    { _ctwbGatewayARN = pGatewayARN_
    , _ctwbTapeSizeInBytes = pTapeSizeInBytes_
    , _ctwbTapeBarcode = pTapeBarcode_
    }


-- | The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tape with. Use the 'ListGateways' operation to return a list of gateways for your account and region.
ctwbGatewayARN :: Lens' CreateTapeWithBarcode Text
ctwbGatewayARN = lens _ctwbGatewayARN (\ s a -> s{_ctwbGatewayARN = a})

-- | The size, in bytes, of the virtual tape that you want to create.
ctwbTapeSizeInBytes :: Lens' CreateTapeWithBarcode Integer
ctwbTapeSizeInBytes = lens _ctwbTapeSizeInBytes (\ s a -> s{_ctwbTapeSizeInBytes = a})

-- | The barcode that you want to assign to the tape.
ctwbTapeBarcode :: Lens' CreateTapeWithBarcode Text
ctwbTapeBarcode = lens _ctwbTapeBarcode (\ s a -> s{_ctwbTapeBarcode = a})

instance AWSRequest CreateTapeWithBarcode where
        type Rs CreateTapeWithBarcode =
             CreateTapeWithBarcodeResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 CreateTapeWithBarcodeResponse' <$>
                   (x .?> "TapeARN") <*> (pure (fromEnum s)))

instance Hashable CreateTapeWithBarcode where

instance NFData CreateTapeWithBarcode where

instance ToHeaders CreateTapeWithBarcode where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CreateTapeWithBarcode" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateTapeWithBarcode where
        toJSON CreateTapeWithBarcode'{..}
          = object
              (catMaybes
                 [Just ("GatewayARN" .= _ctwbGatewayARN),
                  Just ("TapeSizeInBytes" .= _ctwbTapeSizeInBytes),
                  Just ("TapeBarcode" .= _ctwbTapeBarcode)])

instance ToPath CreateTapeWithBarcode where
        toPath = const "/"

instance ToQuery CreateTapeWithBarcode where
        toQuery = const mempty

-- | CreateTapeOutput
--
--
--
-- /See:/ 'createTapeWithBarcodeResponse' smart constructor.
data CreateTapeWithBarcodeResponse = CreateTapeWithBarcodeResponse'
  { _ctwbrsTapeARN        :: !(Maybe Text)
  , _ctwbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTapeWithBarcodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctwbrsTapeARN' - A unique Amazon Resource Name (ARN) that represents the virtual tape that was created.
--
-- * 'ctwbrsResponseStatus' - -- | The response status code.
createTapeWithBarcodeResponse
    :: Int -- ^ 'ctwbrsResponseStatus'
    -> CreateTapeWithBarcodeResponse
createTapeWithBarcodeResponse pResponseStatus_ =
  CreateTapeWithBarcodeResponse'
    {_ctwbrsTapeARN = Nothing, _ctwbrsResponseStatus = pResponseStatus_}


-- | A unique Amazon Resource Name (ARN) that represents the virtual tape that was created.
ctwbrsTapeARN :: Lens' CreateTapeWithBarcodeResponse (Maybe Text)
ctwbrsTapeARN = lens _ctwbrsTapeARN (\ s a -> s{_ctwbrsTapeARN = a})

-- | -- | The response status code.
ctwbrsResponseStatus :: Lens' CreateTapeWithBarcodeResponse Int
ctwbrsResponseStatus = lens _ctwbrsResponseStatus (\ s a -> s{_ctwbrsResponseStatus = a})

instance NFData CreateTapeWithBarcodeResponse where
