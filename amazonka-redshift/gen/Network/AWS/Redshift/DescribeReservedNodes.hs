{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeReservedNodes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of the reserved nodes.
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
    , drnrsReservedNodes
    , drnrsMarker
    , drnrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
          | stop (rs ^. drnrsMarker) = Nothing
          | stop (rs ^. drnrsReservedNodes) = Nothing
          | otherwise =
            Just $ rq & drnMarker .~ rs ^. drnrsMarker

instance AWSRequest DescribeReservedNodes where
        type Sv DescribeReservedNodes = Redshift
        type Rs DescribeReservedNodes =
             DescribeReservedNodesResponse
        request = post "DescribeReservedNodes"
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
-- * 'drnrsReservedNodes'
--
-- * 'drnrsMarker'
--
-- * 'drnrsStatus'
data DescribeReservedNodesResponse = DescribeReservedNodesResponse'
    { _drnrsReservedNodes :: !(Maybe [ReservedNode])
    , _drnrsMarker        :: !(Maybe Text)
    , _drnrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedNodesResponse' smart constructor.
describeReservedNodesResponse :: Int -> DescribeReservedNodesResponse
describeReservedNodesResponse pStatus_ =
    DescribeReservedNodesResponse'
    { _drnrsReservedNodes = Nothing
    , _drnrsMarker = Nothing
    , _drnrsStatus = pStatus_
    }

-- | The list of reserved nodes.
drnrsReservedNodes :: Lens' DescribeReservedNodesResponse [ReservedNode]
drnrsReservedNodes = lens _drnrsReservedNodes (\ s a -> s{_drnrsReservedNodes = a}) . _Default;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
drnrsMarker :: Lens' DescribeReservedNodesResponse (Maybe Text)
drnrsMarker = lens _drnrsMarker (\ s a -> s{_drnrsMarker = a});

-- | FIXME: Undocumented member.
drnrsStatus :: Lens' DescribeReservedNodesResponse Int
drnrsStatus = lens _drnrsStatus (\ s a -> s{_drnrsStatus = a});
