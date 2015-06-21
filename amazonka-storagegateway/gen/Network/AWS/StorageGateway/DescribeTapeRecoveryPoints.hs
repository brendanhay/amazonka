{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
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

-- | Returns a list of virtual tape recovery points that are available for
-- the specified gateway-VTL.
--
-- A recovery point is a point in time view of a virtual tape at which all
-- the data on the virtual tape is consistent. If your gateway crashes,
-- virtual tapes that have recovery points can be recovered to a new
-- gateway.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeTapeRecoveryPoints.html>
module Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
    (
    -- * Request
      DescribeTapeRecoveryPoints
    -- ** Request constructor
    , describeTapeRecoveryPoints
    -- ** Request lenses
    , dtrpMarker
    , dtrpLimit
    , dtrpGatewayARN

    -- * Response
    , DescribeTapeRecoveryPointsResponse
    -- ** Response constructor
    , describeTapeRecoveryPointsResponse
    -- ** Response lenses
    , dtrprTapeRecoveryPointInfos
    , dtrprGatewayARN
    , dtrprMarker
    ) where

import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'describeTapeRecoveryPoints' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrpMarker'
--
-- * 'dtrpLimit'
--
-- * 'dtrpGatewayARN'
data DescribeTapeRecoveryPoints = DescribeTapeRecoveryPoints'{_dtrpMarker :: Maybe Text, _dtrpLimit :: Maybe Nat, _dtrpGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'DescribeTapeRecoveryPoints' smart constructor.
describeTapeRecoveryPoints :: Text -> DescribeTapeRecoveryPoints
describeTapeRecoveryPoints pGatewayARN = DescribeTapeRecoveryPoints'{_dtrpMarker = Nothing, _dtrpLimit = Nothing, _dtrpGatewayARN = pGatewayARN};

-- | An opaque string that indicates the position at which to begin
-- describing the virtual tape recovery points.
dtrpMarker :: Lens' DescribeTapeRecoveryPoints (Maybe Text)
dtrpMarker = lens _dtrpMarker (\ s a -> s{_dtrpMarker = a});

-- | Specifies that the number of virtual tape recovery points that are
-- described be limited to the specified number.
dtrpLimit :: Lens' DescribeTapeRecoveryPoints (Maybe Natural)
dtrpLimit = lens _dtrpLimit (\ s a -> s{_dtrpLimit = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dtrpGatewayARN :: Lens' DescribeTapeRecoveryPoints Text
dtrpGatewayARN = lens _dtrpGatewayARN (\ s a -> s{_dtrpGatewayARN = a});

instance AWSPager DescribeTapeRecoveryPoints where
        page rq rs
          | stop (rs ^. dtrprMarker) = Nothing
          | otherwise = rq & dtrpMarker ?~ rs ^. dtrprMarker

instance AWSRequest DescribeTapeRecoveryPoints where
        type Sv DescribeTapeRecoveryPoints = StorageGateway
        type Rs DescribeTapeRecoveryPoints =
             DescribeTapeRecoveryPointsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTapeRecoveryPointsResponse' <$>
                   (x .?> "TapeRecoveryPointInfos" .!@ mempty) <*>
                     (x .?> "GatewayARN")
                     <*> (x .?> "Marker"))

instance ToHeaders DescribeTapeRecoveryPoints where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeTapeRecoveryPoints"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTapeRecoveryPoints where
        toJSON DescribeTapeRecoveryPoints'{..}
          = object
              ["Marker" .= _dtrpMarker, "Limit" .= _dtrpLimit,
               "GatewayARN" .= _dtrpGatewayARN]

instance ToPath DescribeTapeRecoveryPoints where
        toPath = const "/"

instance ToQuery DescribeTapeRecoveryPoints where
        toQuery = const mempty

-- | /See:/ 'describeTapeRecoveryPointsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrprTapeRecoveryPointInfos'
--
-- * 'dtrprGatewayARN'
--
-- * 'dtrprMarker'
data DescribeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse'{_dtrprTapeRecoveryPointInfos :: Maybe [TapeRecoveryPointInfo], _dtrprGatewayARN :: Maybe Text, _dtrprMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeTapeRecoveryPointsResponse' smart constructor.
describeTapeRecoveryPointsResponse :: DescribeTapeRecoveryPointsResponse
describeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse'{_dtrprTapeRecoveryPointInfos = Nothing, _dtrprGatewayARN = Nothing, _dtrprMarker = Nothing};

-- | An array of TapeRecoveryPointInfos that are available for the specified
-- gateway.
dtrprTapeRecoveryPointInfos :: Lens' DescribeTapeRecoveryPointsResponse [TapeRecoveryPointInfo]
dtrprTapeRecoveryPointInfos = lens _dtrprTapeRecoveryPointInfos (\ s a -> s{_dtrprTapeRecoveryPointInfos = a}) . _Default;

-- | FIXME: Undocumented member.
dtrprGatewayARN :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrprGatewayARN = lens _dtrprGatewayARN (\ s a -> s{_dtrprGatewayARN = a});

-- | An opaque string that indicates the position at which the virtual tape
-- recovery points that were listed for description ended.
--
-- Use this marker in your next request to list the next set of virtual
-- tape recovery points in the list. If there are no more recovery points
-- to describe, this field does not appear in the response.
dtrprMarker :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrprMarker = lens _dtrprMarker (\ s a -> s{_dtrprMarker = a});
