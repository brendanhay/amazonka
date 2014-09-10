{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

module Network.AWS.StorageGateway
    (
    -- * Request
      CreateTapes
    -- ** Request constructor
    , mkCreateTapes
    -- ** Request lenses
    , ctGatewayARN
    , ctTapeSizeInBytes
    , ctClientToken
    , ctNumTapesToCreate
    , ctTapeBarcodePrefix

    -- * Response
    , CreateTapesResponse
    -- ** Response constructor
    , mkCreateTapesResponse
    -- ** Response lenses
    , ctrTapeARNs
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CreateTapes = CreateTapes
    { _ctGatewayARN :: !Text
    , _ctTapeSizeInBytes :: !Integer
    , _ctClientToken :: !Text
    , _ctNumTapesToCreate :: !Integer
    , _ctTapeBarcodePrefix :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateTapes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
-- * @TapeSizeInBytes ::@ @Integer@
--
-- * @ClientToken ::@ @Text@
--
-- * @NumTapesToCreate ::@ @Integer@
--
-- * @TapeBarcodePrefix ::@ @Text@
--
mkCreateTapes :: Text -- ^ 'ctGatewayARN'
              -> Integer -- ^ 'ctTapeSizeInBytes'
              -> Text -- ^ 'ctClientToken'
              -> Integer -- ^ 'ctNumTapesToCreate'
              -> Text -- ^ 'ctTapeBarcodePrefix'
              -> CreateTapes
mkCreateTapes p1 p2 p3 p4 p5 = CreateTapes
    { _ctGatewayARN = p1
    , _ctTapeSizeInBytes = p2
    , _ctClientToken = p3
    , _ctNumTapesToCreate = p4
    , _ctTapeBarcodePrefix = p5
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
ctGatewayARN :: Lens' CreateTapes Text
ctGatewayARN = lens _ctGatewayARN (\s a -> s { _ctGatewayARN = a })

ctTapeSizeInBytes :: Lens' CreateTapes Integer
ctTapeSizeInBytes =
    lens _ctTapeSizeInBytes (\s a -> s { _ctTapeSizeInBytes = a })

ctClientToken :: Lens' CreateTapes Text
ctClientToken = lens _ctClientToken (\s a -> s { _ctClientToken = a })

ctNumTapesToCreate :: Lens' CreateTapes Integer
ctNumTapesToCreate =
    lens _ctNumTapesToCreate (\s a -> s { _ctNumTapesToCreate = a })

ctTapeBarcodePrefix :: Lens' CreateTapes Text
ctTapeBarcodePrefix =
    lens _ctTapeBarcodePrefix (\s a -> s { _ctTapeBarcodePrefix = a })

instance ToPath CreateTapes

instance ToQuery CreateTapes

instance ToHeaders CreateTapes

instance ToJSON CreateTapes

newtype CreateTapesResponse = CreateTapesResponse
    { _ctrTapeARNs :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateTapesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TapeARNs ::@ @[Text]@
--
mkCreateTapesResponse :: CreateTapesResponse
mkCreateTapesResponse = CreateTapesResponse
    { _ctrTapeARNs = mempty
    }

ctrTapeARNs :: Lens' CreateTapesResponse [Text]
ctrTapeARNs = lens _ctrTapeARNs (\s a -> s { _ctrTapeARNs = a })

instance FromJSON CreateTapesResponse

instance AWSRequest CreateTapes where
    type Sv CreateTapes = StorageGateway
    type Rs CreateTapes = CreateTapesResponse

    request = get
    response _ = jsonResponse
