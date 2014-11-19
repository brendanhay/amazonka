{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeTapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of the specified Amazon Resource Name (ARN) of
-- virtual tapes. If a TapeARN is not specified, returns a description of all
-- virtual tapes associated with the specified gateway.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeTapes.html>
module Network.AWS.StorageGateway.DescribeTapes
    (
    -- * Request
      DescribeTapes
    -- ** Request constructor
    , describeTapes
    -- ** Request lenses
    , dtGatewayARN
    , dtLimit
    , dtMarker
    , dtTapeARNs

    -- * Response
    , DescribeTapesResponse
    -- ** Response constructor
    , describeTapesResponse
    -- ** Response lenses
    , dtrMarker
    , dtrTapes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data DescribeTapes = DescribeTapes
    { _dtGatewayARN :: Text
    , _dtLimit      :: Maybe Nat
    , _dtMarker     :: Maybe Text
    , _dtTapeARNs   :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeTapes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtGatewayARN' @::@ 'Text'
--
-- * 'dtLimit' @::@ 'Maybe' 'Natural'
--
-- * 'dtMarker' @::@ 'Maybe' 'Text'
--
-- * 'dtTapeARNs' @::@ ['Text']
--
describeTapes :: Text -- ^ 'dtGatewayARN'
              -> DescribeTapes
describeTapes p1 = DescribeTapes
    { _dtGatewayARN = p1
    , _dtTapeARNs   = mempty
    , _dtMarker     = Nothing
    , _dtLimit      = Nothing
    }

dtGatewayARN :: Lens' DescribeTapes Text
dtGatewayARN = lens _dtGatewayARN (\s a -> s { _dtGatewayARN = a })

-- | Specifies that the number of virtual tapes described be limited to the
-- specified number.
dtLimit :: Lens' DescribeTapes (Maybe Natural)
dtLimit = lens _dtLimit (\s a -> s { _dtLimit = a })
    . mapping _Nat

-- | A marker value, obtained in a previous call to DescribeTapes. This marker
-- indicates which page of results to retrieve. If not specified, the first
-- page of results is retrieved.
dtMarker :: Lens' DescribeTapes (Maybe Text)
dtMarker = lens _dtMarker (\s a -> s { _dtMarker = a })

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe. If this parameter is not
-- specified, AWS Storage Gateway returns a description of all virtual tapes
-- associated with the specified gateway.
dtTapeARNs :: Lens' DescribeTapes [Text]
dtTapeARNs = lens _dtTapeARNs (\s a -> s { _dtTapeARNs = a })

data DescribeTapesResponse = DescribeTapesResponse
    { _dtrMarker :: Maybe Text
    , _dtrTapes  :: [Tape]
    } deriving (Eq, Show, Generic)

-- | 'DescribeTapesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrMarker' @::@ 'Maybe' 'Text'
--
-- * 'dtrTapes' @::@ ['Tape']
--
describeTapesResponse :: DescribeTapesResponse
describeTapesResponse = DescribeTapesResponse
    { _dtrTapes  = mempty
    , _dtrMarker = Nothing
    }

-- | An opaque string which can be used as part of a subsequent DescribeTapes
-- call to retrieve the next page of results. If a response does not contain
-- a marker, then there are no more results to be retrieved.
dtrMarker :: Lens' DescribeTapesResponse (Maybe Text)
dtrMarker = lens _dtrMarker (\s a -> s { _dtrMarker = a })

-- | An array of virtual tape descriptions.
dtrTapes :: Lens' DescribeTapesResponse [Tape]
dtrTapes = lens _dtrTapes (\s a -> s { _dtrTapes = a })

instance ToPath DescribeTapes where
    toPath = const "/"

instance ToQuery DescribeTapes where
    toQuery = const mempty

instance ToHeaders DescribeTapes

instance ToJSON DescribeTapes where
    toJSON DescribeTapes{..} = object
        [ "GatewayARN" .= _dtGatewayARN
        , "TapeARNs"   .= _dtTapeARNs
        , "Marker"     .= _dtMarker
        , "Limit"      .= _dtLimit
        ]

instance AWSRequest DescribeTapes where
    type Sv DescribeTapes = StorageGateway
    type Rs DescribeTapes = DescribeTapesResponse

    request  = post "DescribeTapes"
    response = jsonResponse

instance FromJSON DescribeTapesResponse where
    parseJSON = withObject "DescribeTapesResponse" $ \o -> DescribeTapesResponse
        <$> o .:? "Marker"
        <*> o .: "Tapes"

instance AWSPager DescribeTapes where
    next rq rs = (\x -> rq & dtMarker ?~ x)
        <$> (rs ^. dtrMarker)
