{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.DescribeReservedNodes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the descriptions of the reserved nodes.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeReservedNodes.html>
module Network.AWS.Redshift.DescribeReservedNodes
    (
    -- * Request
      DescribeReservedNodes
    -- ** Request constructor
    , describeReservedNodes
    -- ** Request lenses
    , drnReservedNodeId
    , drnMaxRecords
    , drnMarker

    -- * Response
    , DescribeReservedNodesResponse
    -- ** Response constructor
    , describeReservedNodesResponse
    -- ** Response lenses
    , drnrReservedNodes
    , drnrMarker
    , drnrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeReservedNodes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drnReservedNodeId'
--
-- * 'drnMaxRecords'
--
-- * 'drnMarker'
data DescribeReservedNodes = DescribeReservedNodes'
    { _drnReservedNodeId :: !(Maybe Text)
    , _drnMaxRecords     :: !(Maybe Int)
    , _drnMarker         :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeReservedNodes' smart constructor.
describeReservedNodes :: DescribeReservedNodes
describeReservedNodes =
    DescribeReservedNodes'
    { _drnReservedNodeId = Nothing
    , _drnMaxRecords = Nothing
    , _drnMarker = Nothing
    }

-- | Identifier for the node reservation.
drnReservedNodeId :: Lens' DescribeReservedNodes (Maybe Text)
drnReservedNodeId = lens _drnReservedNodeId (\ s a -> s{_drnReservedNodeId = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
drnMaxRecords :: Lens' DescribeReservedNodes (Maybe Int)
drnMaxRecords = lens _drnMaxRecords (\ s a -> s{_drnMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodes request
-- exceed the value specified in @MaxRecords@, AWS returns a value in the
-- @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
drnMarker :: Lens' DescribeReservedNodes (Maybe Text)
drnMarker = lens _drnMarker (\ s a -> s{_drnMarker = a});

instance AWSPager DescribeReservedNodes where
        page rq rs
          | stop (rs ^. drnrMarker) = Nothing
          | stop (rs ^. drnrReservedNodes) = Nothing
          | otherwise =
            Just $ rq & drnMarker .~ rs ^. drnrMarker

instance AWSRequest DescribeReservedNodes where
        type Sv DescribeReservedNodes = Redshift
        type Rs DescribeReservedNodes =
             DescribeReservedNodesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeReservedNodesResult"
              (\ s h x ->
                 DescribeReservedNodesResponse' <$>
                   (x .@? "ReservedNodes" .!@ mempty >>=
                      may (parseXMLList "ReservedNode"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeReservedNodes where
        toHeaders = const mempty

instance ToPath DescribeReservedNodes where
        toPath = const "/"

instance ToQuery DescribeReservedNodes where
        toQuery DescribeReservedNodes'{..}
          = mconcat
              ["Action" =: ("DescribeReservedNodes" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ReservedNodeId" =: _drnReservedNodeId,
               "MaxRecords" =: _drnMaxRecords,
               "Marker" =: _drnMarker]

-- | Contains the output from the DescribeReservedNodes action.
--
-- /See:/ 'describeReservedNodesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drnrReservedNodes'
--
-- * 'drnrMarker'
--
-- * 'drnrStatus'
data DescribeReservedNodesResponse = DescribeReservedNodesResponse'
    { _drnrReservedNodes :: !(Maybe [ReservedNode])
    , _drnrMarker        :: !(Maybe Text)
    , _drnrStatus        :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeReservedNodesResponse' smart constructor.
describeReservedNodesResponse :: Int -> DescribeReservedNodesResponse
describeReservedNodesResponse pStatus =
    DescribeReservedNodesResponse'
    { _drnrReservedNodes = Nothing
    , _drnrMarker = Nothing
    , _drnrStatus = pStatus
    }

-- | The list of reserved nodes.
drnrReservedNodes :: Lens' DescribeReservedNodesResponse [ReservedNode]
drnrReservedNodes = lens _drnrReservedNodes (\ s a -> s{_drnrReservedNodes = a}) . _Default;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
drnrMarker :: Lens' DescribeReservedNodesResponse (Maybe Text)
drnrMarker = lens _drnrMarker (\ s a -> s{_drnrMarker = a});

-- | FIXME: Undocumented member.
drnrStatus :: Lens' DescribeReservedNodesResponse Int
drnrStatus = lens _drnrStatus (\ s a -> s{_drnrStatus = a});
