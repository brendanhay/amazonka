{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of virtual tape recovery points that are available for
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
    , dtrprsTapeRecoveryPointInfos
    , dtrprsGatewayARN
    , dtrprsMarker
    , dtrprsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | DescribeTapeRecoveryPointsInput
--
-- /See:/ 'describeTapeRecoveryPoints' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrpMarker'
--
-- * 'dtrpLimit'
--
-- * 'dtrpGatewayARN'
data DescribeTapeRecoveryPoints = DescribeTapeRecoveryPoints'
    { _dtrpMarker     :: !(Maybe Text)
    , _dtrpLimit      :: !(Maybe Nat)
    , _dtrpGatewayARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTapeRecoveryPoints' smart constructor.
describeTapeRecoveryPoints :: Text -> DescribeTapeRecoveryPoints
describeTapeRecoveryPoints pGatewayARN_ =
    DescribeTapeRecoveryPoints'
    { _dtrpMarker = Nothing
    , _dtrpLimit = Nothing
    , _dtrpGatewayARN = pGatewayARN_
    }

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
          | stop (rs ^. dtrprsMarker) = Nothing
          | stop (rs ^. dtrprsTapeRecoveryPointInfos) = Nothing
          | otherwise =
            Just $ rq & dtrpMarker .~ rs ^. dtrprsMarker

instance AWSRequest DescribeTapeRecoveryPoints where
        type Sv DescribeTapeRecoveryPoints = StorageGateway
        type Rs DescribeTapeRecoveryPoints =
             DescribeTapeRecoveryPointsResponse
        request = postJSON "DescribeTapeRecoveryPoints"
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTapeRecoveryPointsResponse' <$>
                   (x .?> "TapeRecoveryPointInfos" .!@ mempty) <*>
                     (x .?> "GatewayARN")
                     <*> (x .?> "Marker")
                     <*> (pure (fromEnum s)))

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

-- | DescribeTapeRecoveryPointsOutput
--
-- /See:/ 'describeTapeRecoveryPointsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrprsTapeRecoveryPointInfos'
--
-- * 'dtrprsGatewayARN'
--
-- * 'dtrprsMarker'
--
-- * 'dtrprsStatus'
data DescribeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse'
    { _dtrprsTapeRecoveryPointInfos :: !(Maybe [TapeRecoveryPointInfo])
    , _dtrprsGatewayARN             :: !(Maybe Text)
    , _dtrprsMarker                 :: !(Maybe Text)
    , _dtrprsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTapeRecoveryPointsResponse' smart constructor.
describeTapeRecoveryPointsResponse :: Int -> DescribeTapeRecoveryPointsResponse
describeTapeRecoveryPointsResponse pStatus_ =
    DescribeTapeRecoveryPointsResponse'
    { _dtrprsTapeRecoveryPointInfos = Nothing
    , _dtrprsGatewayARN = Nothing
    , _dtrprsMarker = Nothing
    , _dtrprsStatus = pStatus_
    }

-- | An array of TapeRecoveryPointInfos that are available for the specified
-- gateway.
dtrprsTapeRecoveryPointInfos :: Lens' DescribeTapeRecoveryPointsResponse [TapeRecoveryPointInfo]
dtrprsTapeRecoveryPointInfos = lens _dtrprsTapeRecoveryPointInfos (\ s a -> s{_dtrprsTapeRecoveryPointInfos = a}) . _Default;

-- | FIXME: Undocumented member.
dtrprsGatewayARN :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrprsGatewayARN = lens _dtrprsGatewayARN (\ s a -> s{_dtrprsGatewayARN = a});

-- | An opaque string that indicates the position at which the virtual tape
-- recovery points that were listed for description ended.
--
-- Use this marker in your next request to list the next set of virtual
-- tape recovery points in the list. If there are no more recovery points
-- to describe, this field does not appear in the response.
dtrprsMarker :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrprsMarker = lens _dtrprsMarker (\ s a -> s{_dtrprsMarker = a});

-- | FIXME: Undocumented member.
dtrprsStatus :: Lens' DescribeTapeRecoveryPointsResponse Int
dtrprsStatus = lens _dtrprsStatus (\ s a -> s{_dtrprsStatus = a});
