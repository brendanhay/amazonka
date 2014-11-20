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

-- Module      : Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of virtual tape recovery points that are available for the
-- specified gateway-VTL. A recovery point is a point in time view of a
-- virtual tape at which all the data on the virtual tape is consistent. If
-- your gateway crashes, virtual tapes that have recovery points can be
-- recovered to a new gateway.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeTapeRecoveryPoints.html>
module Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
    (
    -- * Request
      DescribeTapeRecoveryPoints
    -- ** Request constructor
    , describeTapeRecoveryPoints
    -- ** Request lenses
    , dtrpGatewayARN
    , dtrpLimit
    , dtrpMarker

    -- * Response
    , DescribeTapeRecoveryPointsResponse
    -- ** Response constructor
    , describeTapeRecoveryPointsResponse
    -- ** Response lenses
    , dtrprGatewayARN
    , dtrprMarker
    , dtrprTapeRecoveryPointInfos
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data DescribeTapeRecoveryPoints = DescribeTapeRecoveryPoints
    { _dtrpGatewayARN :: Text
    , _dtrpLimit      :: Maybe Nat
    , _dtrpMarker     :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeTapeRecoveryPoints' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrpGatewayARN' @::@ 'Text'
--
-- * 'dtrpLimit' @::@ 'Maybe' 'Natural'
--
-- * 'dtrpMarker' @::@ 'Maybe' 'Text'
--
describeTapeRecoveryPoints :: Text -- ^ 'dtrpGatewayARN'
                           -> DescribeTapeRecoveryPoints
describeTapeRecoveryPoints p1 = DescribeTapeRecoveryPoints
    { _dtrpGatewayARN = p1
    , _dtrpMarker     = Nothing
    , _dtrpLimit      = Nothing
    }

dtrpGatewayARN :: Lens' DescribeTapeRecoveryPoints Text
dtrpGatewayARN = lens _dtrpGatewayARN (\s a -> s { _dtrpGatewayARN = a })

-- | Specifies that the number of virtual tape recovery points that are
-- described be limited to the specified number.
dtrpLimit :: Lens' DescribeTapeRecoveryPoints (Maybe Natural)
dtrpLimit = lens _dtrpLimit (\s a -> s { _dtrpLimit = a }) . mapping _Nat

-- | An opaque string that indicates the position at which to begin describing
-- the virtual tape recovery points.
dtrpMarker :: Lens' DescribeTapeRecoveryPoints (Maybe Text)
dtrpMarker = lens _dtrpMarker (\s a -> s { _dtrpMarker = a })

data DescribeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse
    { _dtrprGatewayARN             :: Maybe Text
    , _dtrprMarker                 :: Maybe Text
    , _dtrprTapeRecoveryPointInfos :: List "TapeRecoveryPointInfos" TapeRecoveryPointInfo
    } deriving (Eq, Show)

-- | 'DescribeTapeRecoveryPointsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrprGatewayARN' @::@ 'Maybe' 'Text'
--
-- * 'dtrprMarker' @::@ 'Maybe' 'Text'
--
-- * 'dtrprTapeRecoveryPointInfos' @::@ ['TapeRecoveryPointInfo']
--
describeTapeRecoveryPointsResponse :: DescribeTapeRecoveryPointsResponse
describeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse
    { _dtrprGatewayARN             = Nothing
    , _dtrprTapeRecoveryPointInfos = mempty
    , _dtrprMarker                 = Nothing
    }

dtrprGatewayARN :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrprGatewayARN = lens _dtrprGatewayARN (\s a -> s { _dtrprGatewayARN = a })

-- | An opaque string that indicates the position at which the virtual tape
-- recovery points that were listed for description ended. Use this marker
-- in your next request to list the next set of virtual tape recovery points
-- in the list. If there are no more recovery points to describe, this field
-- does not appear in the response.
dtrprMarker :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrprMarker = lens _dtrprMarker (\s a -> s { _dtrprMarker = a })

-- | An array of TapeRecoveryPointInfos that are available for the specified
-- gateway.
dtrprTapeRecoveryPointInfos :: Lens' DescribeTapeRecoveryPointsResponse [TapeRecoveryPointInfo]
dtrprTapeRecoveryPointInfos =
    lens _dtrprTapeRecoveryPointInfos
        (\s a -> s { _dtrprTapeRecoveryPointInfos = a })
            . _List

instance ToPath DescribeTapeRecoveryPoints where
    toPath = const "/"

instance ToQuery DescribeTapeRecoveryPoints where
    toQuery = const mempty

instance ToHeaders DescribeTapeRecoveryPoints

instance ToJSON DescribeTapeRecoveryPoints where
    toJSON DescribeTapeRecoveryPoints{..} = object
        [ "GatewayARN" .= _dtrpGatewayARN
        , "Marker"     .= _dtrpMarker
        , "Limit"      .= _dtrpLimit
        ]

json

instance AWSRequest DescribeTapeRecoveryPoints where
    type Sv DescribeTapeRecoveryPoints = StorageGateway
    type Rs DescribeTapeRecoveryPoints = DescribeTapeRecoveryPointsResponse

    request  = post "DescribeTapeRecoveryPoints"
    response = jsonResponse

instance FromJSON DescribeTapeRecoveryPointsResponse where
    parseJSON = withObject "DescribeTapeRecoveryPointsResponse" $ \o -> DescribeTapeRecoveryPointsResponse
        <$> o .:? "GatewayARN"
        <*> o .:? "Marker"
        <*> o .:  "TapeRecoveryPointInfos"

instance AWSPager DescribeTapeRecoveryPoints where
    page rq rs
        | stop (rq ^. dtrpMarker) = Nothing
        | otherwise = (\x -> rq & dtrpMarker ?~ x)
            <$> (rs ^. dtrprMarker)
