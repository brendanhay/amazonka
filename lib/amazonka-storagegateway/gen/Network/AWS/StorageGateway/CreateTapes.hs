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
-- Module      : Network.AWS.StorageGateway.CreateTapes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more virtual tapes. You write data to the virtual tapes and then archive the tapes. This operation is only supported in the tape gateway type.
--
--
module Network.AWS.StorageGateway.CreateTapes
    (
    -- * Creating a Request
      createTapes
    , CreateTapes
    -- * Request Lenses
    , ctGatewayARN
    , ctTapeSizeInBytes
    , ctClientToken
    , ctNumTapesToCreate
    , ctTapeBarcodePrefix

    -- * Destructuring the Response
    , createTapesResponse
    , CreateTapesResponse
    -- * Response Lenses
    , ctrsTapeARNs
    , ctrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | CreateTapesInput
--
--
--
-- /See:/ 'createTapes' smart constructor.
data CreateTapes = CreateTapes'
  { _ctGatewayARN        :: !Text
  , _ctTapeSizeInBytes   :: !Integer
  , _ctClientToken       :: !Text
  , _ctNumTapesToCreate  :: !Nat
  , _ctTapeBarcodePrefix :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTapes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctGatewayARN' - The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tapes with. Use the 'ListGateways' operation to return a list of gateways for your account and region.
--
-- * 'ctTapeSizeInBytes' - The size, in bytes, of the virtual tapes that you want to create.
--
-- * 'ctClientToken' - A unique identifier that you use to retry a request. If you retry a request, use the same @ClientToken@ you specified in the initial request.
--
-- * 'ctNumTapesToCreate' - The number of virtual tapes that you want to create.
--
-- * 'ctTapeBarcodePrefix' - A prefix that you append to the barcode of the virtual tape you are creating. This prefix makes the barcode unique.
createTapes
    :: Text -- ^ 'ctGatewayARN'
    -> Integer -- ^ 'ctTapeSizeInBytes'
    -> Text -- ^ 'ctClientToken'
    -> Natural -- ^ 'ctNumTapesToCreate'
    -> Text -- ^ 'ctTapeBarcodePrefix'
    -> CreateTapes
createTapes pGatewayARN_ pTapeSizeInBytes_ pClientToken_ pNumTapesToCreate_ pTapeBarcodePrefix_ =
  CreateTapes'
    { _ctGatewayARN = pGatewayARN_
    , _ctTapeSizeInBytes = pTapeSizeInBytes_
    , _ctClientToken = pClientToken_
    , _ctNumTapesToCreate = _Nat # pNumTapesToCreate_
    , _ctTapeBarcodePrefix = pTapeBarcodePrefix_
    }


-- | The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tapes with. Use the 'ListGateways' operation to return a list of gateways for your account and region.
ctGatewayARN :: Lens' CreateTapes Text
ctGatewayARN = lens _ctGatewayARN (\ s a -> s{_ctGatewayARN = a})

-- | The size, in bytes, of the virtual tapes that you want to create.
ctTapeSizeInBytes :: Lens' CreateTapes Integer
ctTapeSizeInBytes = lens _ctTapeSizeInBytes (\ s a -> s{_ctTapeSizeInBytes = a})

-- | A unique identifier that you use to retry a request. If you retry a request, use the same @ClientToken@ you specified in the initial request.
ctClientToken :: Lens' CreateTapes Text
ctClientToken = lens _ctClientToken (\ s a -> s{_ctClientToken = a})

-- | The number of virtual tapes that you want to create.
ctNumTapesToCreate :: Lens' CreateTapes Natural
ctNumTapesToCreate = lens _ctNumTapesToCreate (\ s a -> s{_ctNumTapesToCreate = a}) . _Nat

-- | A prefix that you append to the barcode of the virtual tape you are creating. This prefix makes the barcode unique.
ctTapeBarcodePrefix :: Lens' CreateTapes Text
ctTapeBarcodePrefix = lens _ctTapeBarcodePrefix (\ s a -> s{_ctTapeBarcodePrefix = a})

instance AWSRequest CreateTapes where
        type Rs CreateTapes = CreateTapesResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 CreateTapesResponse' <$>
                   (x .?> "TapeARNs" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable CreateTapes where

instance NFData CreateTapes where

instance ToHeaders CreateTapes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CreateTapes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateTapes where
        toJSON CreateTapes'{..}
          = object
              (catMaybes
                 [Just ("GatewayARN" .= _ctGatewayARN),
                  Just ("TapeSizeInBytes" .= _ctTapeSizeInBytes),
                  Just ("ClientToken" .= _ctClientToken),
                  Just ("NumTapesToCreate" .= _ctNumTapesToCreate),
                  Just ("TapeBarcodePrefix" .= _ctTapeBarcodePrefix)])

instance ToPath CreateTapes where
        toPath = const "/"

instance ToQuery CreateTapes where
        toQuery = const mempty

-- | CreateTapeOutput
--
--
--
-- /See:/ 'createTapesResponse' smart constructor.
data CreateTapesResponse = CreateTapesResponse'
  { _ctrsTapeARNs       :: !(Maybe [Text])
  , _ctrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTapesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsTapeARNs' - A list of unique Amazon Resource Names (ARNs) that represents the virtual tapes that were created.
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTapesResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateTapesResponse
createTapesResponse pResponseStatus_ =
  CreateTapesResponse'
    {_ctrsTapeARNs = Nothing, _ctrsResponseStatus = pResponseStatus_}


-- | A list of unique Amazon Resource Names (ARNs) that represents the virtual tapes that were created.
ctrsTapeARNs :: Lens' CreateTapesResponse [Text]
ctrsTapeARNs = lens _ctrsTapeARNs (\ s a -> s{_ctrsTapeARNs = a}) . _Default . _Coerce

-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTapesResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a})

instance NFData CreateTapesResponse where
