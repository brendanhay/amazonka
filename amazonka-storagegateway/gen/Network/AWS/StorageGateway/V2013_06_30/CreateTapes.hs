{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.CreateTapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.CreateTapes
    (
    -- * Request
      CreateTapes
    -- ** Request constructor
    , mkCreateTapesInput
    -- ** Request lenses
    , ctiGatewayARN
    , ctiTapeSizeInBytes
    , ctiClientToken
    , ctiNumTapesToCreate
    , ctiTapeBarcodePrefix

    -- * Response
    , CreateTapesResponse
    -- ** Response lenses
    , ctoTapeARNs
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateTapes' request.
mkCreateTapesInput :: Text -- ^ 'ctiGatewayARN'
                   -> Integer -- ^ 'ctiTapeSizeInBytes'
                   -> Text -- ^ 'ctiClientToken'
                   -> Integer -- ^ 'ctiNumTapesToCreate'
                   -> Text -- ^ 'ctiTapeBarcodePrefix'
                   -> CreateTapes
mkCreateTapesInput p1 p2 p3 p4 p5 = CreateTapes
    { _ctiGatewayARN = p1
    , _ctiTapeSizeInBytes = p2
    , _ctiClientToken = p3
    , _ctiNumTapesToCreate = p4
    , _ctiTapeBarcodePrefix = p5
    }
{-# INLINE mkCreateTapesInput #-}

data CreateTapes = CreateTapes
    { _ctiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _ctiTapeSizeInBytes :: Integer
    , _ctiClientToken :: Text
    , _ctiNumTapesToCreate :: Integer
    , _ctiTapeBarcodePrefix :: Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
ctiGatewayARN :: Lens' CreateTapes (Text)
ctiGatewayARN = lens _ctiGatewayARN (\s a -> s { _ctiGatewayARN = a })
{-# INLINE ctiGatewayARN #-}

ctiTapeSizeInBytes :: Lens' CreateTapes (Integer)
ctiTapeSizeInBytes = lens _ctiTapeSizeInBytes (\s a -> s { _ctiTapeSizeInBytes = a })
{-# INLINE ctiTapeSizeInBytes #-}

ctiClientToken :: Lens' CreateTapes (Text)
ctiClientToken = lens _ctiClientToken (\s a -> s { _ctiClientToken = a })
{-# INLINE ctiClientToken #-}

ctiNumTapesToCreate :: Lens' CreateTapes (Integer)
ctiNumTapesToCreate = lens _ctiNumTapesToCreate (\s a -> s { _ctiNumTapesToCreate = a })
{-# INLINE ctiNumTapesToCreate #-}

ctiTapeBarcodePrefix :: Lens' CreateTapes (Text)
ctiTapeBarcodePrefix = lens _ctiTapeBarcodePrefix (\s a -> s { _ctiTapeBarcodePrefix = a })
{-# INLINE ctiTapeBarcodePrefix #-}

instance ToPath CreateTapes

instance ToQuery CreateTapes

instance ToHeaders CreateTapes

instance ToJSON CreateTapes

newtype CreateTapesResponse = CreateTapesResponse
    { _ctoTapeARNs :: [Text]
    } deriving (Show, Generic)

ctoTapeARNs :: Lens' CreateTapesResponse ([Text])
ctoTapeARNs = lens _ctoTapeARNs (\s a -> s { _ctoTapeARNs = a })
{-# INLINE ctoTapeARNs #-}

instance FromJSON CreateTapesResponse

instance AWSRequest CreateTapes where
    type Sv CreateTapes = StorageGateway
    type Rs CreateTapes = CreateTapesResponse

    request = get
    response _ = jsonResponse
