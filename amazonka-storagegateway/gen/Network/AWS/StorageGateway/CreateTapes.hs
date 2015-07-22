{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateTapes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more virtual tapes. You write data to the virtual tapes
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
    , ctrqGatewayARN
    , ctrqTapeSizeInBytes
    , ctrqClientToken
    , ctrqNumTapesToCreate
    , ctrqTapeBarcodePrefix

    -- * Response
    , CreateTapesResponse
    -- ** Response constructor
    , createTapesResponse
    -- ** Response lenses
    , ctrsTapeARNs
    , ctrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | CreateTapesInput
--
-- /See:/ 'createTapes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrqGatewayARN'
--
-- * 'ctrqTapeSizeInBytes'
--
-- * 'ctrqClientToken'
--
-- * 'ctrqNumTapesToCreate'
--
-- * 'ctrqTapeBarcodePrefix'
data CreateTapes = CreateTapes'
    { _ctrqGatewayARN        :: !Text
    , _ctrqTapeSizeInBytes   :: !Integer
    , _ctrqClientToken       :: !Text
    , _ctrqNumTapesToCreate  :: !Nat
    , _ctrqTapeBarcodePrefix :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateTapes' smart constructor.
createTapes :: Text -> Integer -> Text -> Natural -> Text -> CreateTapes
createTapes pGatewayARN_ pTapeSizeInBytes_ pClientToken_ pNumTapesToCreate_ pTapeBarcodePrefix_ =
    CreateTapes'
    { _ctrqGatewayARN = pGatewayARN_
    , _ctrqTapeSizeInBytes = pTapeSizeInBytes_
    , _ctrqClientToken = pClientToken_
    , _ctrqNumTapesToCreate = _Nat # pNumTapesToCreate_
    , _ctrqTapeBarcodePrefix = pTapeBarcodePrefix_
    }

-- | The unique Amazon Resource Name(ARN) that represents the gateway to
-- associate the virtual tapes with. Use the ListGateways operation to
-- return a list of gateways for your account and region.
ctrqGatewayARN :: Lens' CreateTapes Text
ctrqGatewayARN = lens _ctrqGatewayARN (\ s a -> s{_ctrqGatewayARN = a});

-- | The size, in bytes, of the virtual tapes you want to create.
--
-- The size must be gigabyte (1024*1024*1024 byte) aligned.
ctrqTapeSizeInBytes :: Lens' CreateTapes Integer
ctrqTapeSizeInBytes = lens _ctrqTapeSizeInBytes (\ s a -> s{_ctrqTapeSizeInBytes = a});

-- | A unique identifier that you use to retry a request. If you retry a
-- request, use the same @ClientToken@ you specified in the initial
-- request.
--
-- Using the same @ClientToken@ prevents creating the tape multiple times.
ctrqClientToken :: Lens' CreateTapes Text
ctrqClientToken = lens _ctrqClientToken (\ s a -> s{_ctrqClientToken = a});

-- | The number of virtual tapes you want to create.
ctrqNumTapesToCreate :: Lens' CreateTapes Natural
ctrqNumTapesToCreate = lens _ctrqNumTapesToCreate (\ s a -> s{_ctrqNumTapesToCreate = a}) . _Nat;

-- | A prefix you append to the barcode of the virtual tape you are creating.
-- This makes a barcode unique.
--
-- The prefix must be 1 to 4 characters in length and must be upper-case
-- letters A-Z.
ctrqTapeBarcodePrefix :: Lens' CreateTapes Text
ctrqTapeBarcodePrefix = lens _ctrqTapeBarcodePrefix (\ s a -> s{_ctrqTapeBarcodePrefix = a});

instance AWSRequest CreateTapes where
        type Sv CreateTapes = StorageGateway
        type Rs CreateTapes = CreateTapesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateTapesResponse' <$>
                   (x .?> "TapeARNs" .!@ mempty) <*>
                     (pure (fromEnum s)))

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
              ["GatewayARN" .= _ctrqGatewayARN,
               "TapeSizeInBytes" .= _ctrqTapeSizeInBytes,
               "ClientToken" .= _ctrqClientToken,
               "NumTapesToCreate" .= _ctrqNumTapesToCreate,
               "TapeBarcodePrefix" .= _ctrqTapeBarcodePrefix]

instance ToPath CreateTapes where
        toPath = const "/"

instance ToQuery CreateTapes where
        toQuery = const mempty

-- | CreateTapeOutput
--
-- /See:/ 'createTapesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrsTapeARNs'
--
-- * 'ctrsStatus'
data CreateTapesResponse = CreateTapesResponse'
    { _ctrsTapeARNs :: !(Maybe [Text])
    , _ctrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateTapesResponse' smart constructor.
createTapesResponse :: Int -> CreateTapesResponse
createTapesResponse pStatus_ =
    CreateTapesResponse'
    { _ctrsTapeARNs = Nothing
    , _ctrsStatus = pStatus_
    }

-- | A list of unique Amazon Resource Named (ARN) the represents the virtual
-- tapes that were created.
ctrsTapeARNs :: Lens' CreateTapesResponse [Text]
ctrsTapeARNs = lens _ctrsTapeARNs (\ s a -> s{_ctrsTapeARNs = a}) . _Default;

-- | FIXME: Undocumented member.
ctrsStatus :: Lens' CreateTapesResponse Int
ctrsStatus = lens _ctrsStatus (\ s a -> s{_ctrsStatus = a});
