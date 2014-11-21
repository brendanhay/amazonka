{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.CreateTapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates one or more virtual tapes. You write data to the virtual tapes and
-- then archive the tapes.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CreateTapes.html>
module Network.AWS.StorageGateway.CreateTapes
    (
    -- * Request
      CreateTapes
    -- ** Request constructor
    , createTapes
    -- ** Request lenses
    , ctClientToken
    , ctGatewayARN
    , ctNumTapesToCreate
    , ctTapeBarcodePrefix
    , ctTapeSizeInBytes

    -- * Response
    , CreateTapesResponse
    -- ** Response constructor
    , createTapesResponse
    -- ** Response lenses
    , ctrTapeARNs
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data CreateTapes = CreateTapes
    { _ctClientToken       :: Text
    , _ctGatewayARN        :: Text
    , _ctNumTapesToCreate  :: Nat
    , _ctTapeBarcodePrefix :: Text
    , _ctTapeSizeInBytes   :: Nat
    } deriving (Eq, Ord, Show)

-- | 'CreateTapes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctClientToken' @::@ 'Text'
--
-- * 'ctGatewayARN' @::@ 'Text'
--
-- * 'ctNumTapesToCreate' @::@ 'Natural'
--
-- * 'ctTapeBarcodePrefix' @::@ 'Text'
--
-- * 'ctTapeSizeInBytes' @::@ 'Natural'
--
createTapes :: Text -- ^ 'ctGatewayARN'
            -> Natural -- ^ 'ctTapeSizeInBytes'
            -> Text -- ^ 'ctClientToken'
            -> Natural -- ^ 'ctNumTapesToCreate'
            -> Text -- ^ 'ctTapeBarcodePrefix'
            -> CreateTapes
createTapes p1 p2 p3 p4 p5 = CreateTapes
    { _ctGatewayARN        = p1
    , _ctTapeSizeInBytes   = withIso _Nat (const id) p2
    , _ctClientToken       = p3
    , _ctNumTapesToCreate  = withIso _Nat (const id) p4
    , _ctTapeBarcodePrefix = p5
    }

-- | A unique identifier that you use to retry a request. If you retry a
-- request, use the same ClientToken you specified in the initial request.
ctClientToken :: Lens' CreateTapes Text
ctClientToken = lens _ctClientToken (\s a -> s { _ctClientToken = a })

-- | The unique Amazon Resource Name(ARN) that represents the gateway to
-- associate the virtual tapes with. Use the ListGateways operation to
-- return a list of gateways for your account and region.
ctGatewayARN :: Lens' CreateTapes Text
ctGatewayARN = lens _ctGatewayARN (\s a -> s { _ctGatewayARN = a })

-- | The number of virtual tapes you want to create.
ctNumTapesToCreate :: Lens' CreateTapes Natural
ctNumTapesToCreate =
    lens _ctNumTapesToCreate (\s a -> s { _ctNumTapesToCreate = a })
        . _Nat

-- | A prefix you append to the barcode of the virtual tape you are creating.
-- This makes a barcode unique.
ctTapeBarcodePrefix :: Lens' CreateTapes Text
ctTapeBarcodePrefix =
    lens _ctTapeBarcodePrefix (\s a -> s { _ctTapeBarcodePrefix = a })

-- | The size, in bytes, of the virtual tapes you want to create.
ctTapeSizeInBytes :: Lens' CreateTapes Natural
ctTapeSizeInBytes =
    lens _ctTapeSizeInBytes (\s a -> s { _ctTapeSizeInBytes = a })
        . _Nat

newtype CreateTapesResponse = CreateTapesResponse
    { _ctrTapeARNs :: List "TapeARNs" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList CreateTapesResponse where
    type Item CreateTapesResponse = Text

    fromList = CreateTapesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ctrTapeARNs

-- | 'CreateTapesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrTapeARNs' @::@ ['Text']
--
createTapesResponse :: CreateTapesResponse
createTapesResponse = CreateTapesResponse
    { _ctrTapeARNs = mempty
    }

-- | A list of unique Amazon Resource Named (ARN) the represents the virtual
-- tapes that were created.
ctrTapeARNs :: Lens' CreateTapesResponse [Text]
ctrTapeARNs = lens _ctrTapeARNs (\s a -> s { _ctrTapeARNs = a }) . _List

instance ToPath CreateTapes where
    toPath = const "/"

instance ToQuery CreateTapes where
    toQuery = const mempty

instance ToHeaders CreateTapes

instance ToJSON CreateTapes where
    toJSON CreateTapes{..} = object
        [ "GatewayARN"        .= _ctGatewayARN
        , "TapeSizeInBytes"   .= _ctTapeSizeInBytes
        , "ClientToken"       .= _ctClientToken
        , "NumTapesToCreate"  .= _ctNumTapesToCreate
        , "TapeBarcodePrefix" .= _ctTapeBarcodePrefix
        ]

instance AWSRequest CreateTapes where
    type Sv CreateTapes = StorageGateway
    type Rs CreateTapes = CreateTapesResponse

    request  = post "CreateTapes"
    response = jsonResponse

instance FromJSON CreateTapesResponse where
    parseJSON = withObject "CreateTapesResponse" $ \o -> CreateTapesResponse
        <$> o .:  "TapeARNs"
