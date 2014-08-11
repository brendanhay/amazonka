{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeTapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.DescribeTapes where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeTapes' request.
describeTapes :: Text -- ^ '_dtiGatewayARN'
              -> DescribeTapes
describeTapes p1 = DescribeTapes
    { _dtiGatewayARN = p1
    , _dtiMarker = Nothing
    , _dtiLimit = Nothing
    , _dtiTapeARNs = mempty
    }

data DescribeTapes = DescribeTapes
    { _dtiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dtiMarker :: Maybe Text
    , _dtiLimit :: Maybe Integer
    , _dtiTapeARNs :: [Text]
    } deriving (Show, Generic)

makeLenses ''DescribeTapes

instance ToPath DescribeTapes

instance ToQuery DescribeTapes

instance ToHeaders DescribeTapes

instance ToJSON DescribeTapes

data DescribeTapesResponse = DescribeTapesResponse
    { _dtoMarker :: Maybe Text
    , _dtoTapes :: [Tape]
    } deriving (Show, Generic)

makeLenses ''DescribeTapesResponse

instance FromJSON DescribeTapesResponse

instance AWSRequest DescribeTapes where
    type Sv DescribeTapes = StorageGateway
    type Rs DescribeTapes = DescribeTapesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapes where
    next rq rs = (\x -> rq { _dtiMarker = Just x })
        <$> (_dtoMarker rs)
