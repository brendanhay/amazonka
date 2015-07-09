{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Returns properties of provisioned clusters including general cluster
-- properties, cluster database properties, maintenance and backup
-- properties, and security and access properties. This operation supports
-- pagination. For more information about managing clusters, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/ .
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all clusters that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- clusters that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, clusters are
-- returned regardless of whether they have tag keys or values associated
-- with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusters.html>
module Network.AWS.Redshift.DescribeClusters
    (
    -- * Request
      DescribeClusters
    -- ** Request constructor
    , describeClusters
    -- ** Request lenses
    , dcTagValues
    , dcTagKeys
    , dcClusterIdentifier
    , dcMaxRecords
    , dcMarker

    -- * Response
    , DescribeClustersResponse
    -- ** Response constructor
    , describeClustersResponse
    -- ** Response lenses
    , dcrMarker
    , dcrClusters
    , dcrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeClusters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcTagValues'
--
-- * 'dcTagKeys'
--
-- * 'dcClusterIdentifier'
--
-- * 'dcMaxRecords'
--
-- * 'dcMarker'
data DescribeClusters = DescribeClusters'
    { _dcTagValues         :: !(Maybe [Text])
    , _dcTagKeys           :: !(Maybe [Text])
    , _dcClusterIdentifier :: !(Maybe Text)
    , _dcMaxRecords        :: !(Maybe Int)
    , _dcMarker            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusters' smart constructor.
describeClusters :: DescribeClusters
describeClusters =
    DescribeClusters'
    { _dcTagValues = Nothing
    , _dcTagKeys = Nothing
    , _dcClusterIdentifier = Nothing
    , _dcMaxRecords = Nothing
    , _dcMarker = Nothing
    }

-- | A tag value or values for which you want to return all matching clusters
-- that are associated with the specified tag value or values. For example,
-- suppose that you have clusters that are tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with the clusters that have
-- either or both of these tag values associated with them.
dcTagValues :: Lens' DescribeClusters [Text]
dcTagValues = lens _dcTagValues (\ s a -> s{_dcTagValues = a}) . _Default;

-- | A tag key or keys for which you want to return all matching clusters
-- that are associated with the specified key or keys. For example, suppose
-- that you have clusters that are tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with the clusters that have either or
-- both of these tag keys associated with them.
dcTagKeys :: Lens' DescribeClusters [Text]
dcTagKeys = lens _dcTagKeys (\ s a -> s{_dcTagKeys = a}) . _Default;

-- | The unique identifier of a cluster whose properties you are requesting.
-- This parameter is case sensitive.
--
-- The default is that all clusters defined for an account are returned.
dcClusterIdentifier :: Lens' DescribeClusters (Maybe Text)
dcClusterIdentifier = lens _dcClusterIdentifier (\ s a -> s{_dcClusterIdentifier = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dcMaxRecords :: Lens' DescribeClusters (Maybe Int)
dcMaxRecords = lens _dcMaxRecords (\ s a -> s{_dcMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusters request
-- exceed the value specified in @MaxRecords@, AWS returns a value in the
-- @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- Constraints: You can specify either the __ClusterIdentifier__ parameter
-- or the __Marker__ parameter, but not both.
dcMarker :: Lens' DescribeClusters (Maybe Text)
dcMarker = lens _dcMarker (\ s a -> s{_dcMarker = a});

instance AWSPager DescribeClusters where
        page rq rs
          | stop (rs ^. dcrMarker) = Nothing
          | stop (rs ^. dcrClusters) = Nothing
          | otherwise = Just $ rq & dcMarker .~ rs ^. dcrMarker

instance AWSRequest DescribeClusters where
        type Sv DescribeClusters = Redshift
        type Rs DescribeClusters = DescribeClustersResponse
        request = post
        response
          = receiveXMLWrapper "DescribeClustersResult"
              (\ s h x ->
                 DescribeClustersResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "Clusters" .!@ mempty >>=
                        may (parseXMLList "Cluster"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeClusters where
        toHeaders = const mempty

instance ToPath DescribeClusters where
        toPath = const "/"

instance ToQuery DescribeClusters where
        toQuery DescribeClusters'{..}
          = mconcat
              ["Action" =: ("DescribeClusters" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dcTagValues),
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dcTagKeys),
               "ClusterIdentifier" =: _dcClusterIdentifier,
               "MaxRecords" =: _dcMaxRecords, "Marker" =: _dcMarker]

-- | Contains the output from the DescribeClusters action.
--
-- /See:/ 'describeClustersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrMarker'
--
-- * 'dcrClusters'
--
-- * 'dcrStatus'
data DescribeClustersResponse = DescribeClustersResponse'
    { _dcrMarker   :: !(Maybe Text)
    , _dcrClusters :: !(Maybe [Cluster])
    , _dcrStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClustersResponse' smart constructor.
describeClustersResponse :: Int -> DescribeClustersResponse
describeClustersResponse pStatus =
    DescribeClustersResponse'
    { _dcrMarker = Nothing
    , _dcrClusters = Nothing
    , _dcrStatus = pStatus
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcrMarker :: Lens' DescribeClustersResponse (Maybe Text)
dcrMarker = lens _dcrMarker (\ s a -> s{_dcrMarker = a});

-- | A list of Cluster objects, where each object describes one cluster.
dcrClusters :: Lens' DescribeClustersResponse [Cluster]
dcrClusters = lens _dcrClusters (\ s a -> s{_dcrClusters = a}) . _Default;

-- | FIXME: Undocumented member.
dcrStatus :: Lens' DescribeClustersResponse Int
dcrStatus = lens _dcrStatus (\ s a -> s{_dcrStatus = a});
