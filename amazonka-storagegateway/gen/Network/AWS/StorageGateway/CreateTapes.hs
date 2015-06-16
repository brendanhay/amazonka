{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.CreateTapes
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

-- | Creates one or more virtual tapes. You write data to the virtual tapes
-- and then archive the tapes.
--
-- Cache storage must be allocated to the gateway before you can create
-- virtual tapes. Use the AddCache operation to add cache storage to a
-- gateway.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CreateTapes.html>
module Network.AWS.StorageGateway.CreateTapes
    (
    -- * Request
      CreateTapes
    -- ** Request constructor
    , createTapes
    -- ** Request lenses
    , ctGatewayARN
    , ctTapeSizeInBytes
    , ctClientToken
    , ctNumTapesToCreate
    , ctTapeBarcodePrefix

    -- * Response
    , CreateTapesResponse
    -- ** Response constructor
    , createTapesResponse
    -- ** Response lenses
    , ctrTapeARNs
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'createTapes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctGatewayARN'
--
-- * 'ctTapeSizeInBytes'
--
-- * 'ctClientToken'
--
-- * 'ctNumTapesToCreate'
--
-- * 'ctTapeBarcodePrefix'
data CreateTapes = CreateTapes'{_ctGatewayARN :: Text, _ctTapeSizeInBytes :: Integer, _ctClientToken :: Text, _ctNumTapesToCreate :: Nat, _ctTapeBarcodePrefix :: Text} deriving (Eq, Read, Show)

-- | 'CreateTapes' smart constructor.
createTapes :: Text -> Integer -> Text -> Natural -> Text -> CreateTapes
createTapes pGatewayARN pTapeSizeInBytes pClientToken pNumTapesToCreate pTapeBarcodePrefix = CreateTapes'{_ctGatewayARN = pGatewayARN, _ctTapeSizeInBytes = pTapeSizeInBytes, _ctClientToken = pClientToken, _ctNumTapesToCreate = _Nat # pNumTapesToCreate, _ctTapeBarcodePrefix = pTapeBarcodePrefix};

-- | The unique Amazon Resource Name(ARN) that represents the gateway to
-- associate the virtual tapes with. Use the ListGateways operation to
-- return a list of gateways for your account and region.
ctGatewayARN :: Lens' CreateTapes Text
ctGatewayARN = lens _ctGatewayARN (\ s a -> s{_ctGatewayARN = a});

-- | The size, in bytes, of the virtual tapes you want to create.
--
-- The size must be gigabyte (1024*1024*1024 byte) aligned.
ctTapeSizeInBytes :: Lens' CreateTapes Integer
ctTapeSizeInBytes = lens _ctTapeSizeInBytes (\ s a -> s{_ctTapeSizeInBytes = a});

-- | A unique identifier that you use to retry a request. If you retry a
-- request, use the same @ClientToken@ you specified in the initial
-- request.
--
-- Using the same @ClientToken@ prevents creating the tape multiple times.
ctClientToken :: Lens' CreateTapes Text
ctClientToken = lens _ctClientToken (\ s a -> s{_ctClientToken = a});

-- | The number of virtual tapes you want to create.
ctNumTapesToCreate :: Lens' CreateTapes Natural
ctNumTapesToCreate = lens _ctNumTapesToCreate (\ s a -> s{_ctNumTapesToCreate = a}) . _Nat;

-- | A prefix you append to the barcode of the virtual tape you are creating.
-- This makes a barcode unique.
--
-- The prefix must be 1 to 4 characters in length and must be upper-case
-- letters A-Z.
ctTapeBarcodePrefix :: Lens' CreateTapes Text
ctTapeBarcodePrefix = lens _ctTapeBarcodePrefix (\ s a -> s{_ctTapeBarcodePrefix = a});

instance AWSRequest CreateTapes where
        type Sv CreateTapes = StorageGateway
        type Rs CreateTapes = CreateTapesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateTapesResponse' <$>
                   (x .?> "TapeARNs" .!@ mempty))

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
              ["GatewayARN" .= _ctGatewayARN,
               "TapeSizeInBytes" .= _ctTapeSizeInBytes,
               "ClientToken" .= _ctClientToken,
               "NumTapesToCreate" .= _ctNumTapesToCreate,
               "TapeBarcodePrefix" .= _ctTapeBarcodePrefix]

instance ToPath CreateTapes where
        toPath = const "/"

instance ToQuery CreateTapes where
        toQuery = const mempty

-- | /See:/ 'createTapesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrTapeARNs'
newtype CreateTapesResponse = CreateTapesResponse'{_ctrTapeARNs :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'CreateTapesResponse' smart constructor.
createTapesResponse :: CreateTapesResponse
createTapesResponse = CreateTapesResponse'{_ctrTapeARNs = Nothing};

-- | A list of unique Amazon Resource Named (ARN) the represents the virtual
-- tapes that were created.
ctrTapeARNs :: Lens' CreateTapesResponse [Text]
ctrTapeARNs = lens _ctrTapeARNs (\ s a -> s{_ctrTapeARNs = a}) . _Default;
