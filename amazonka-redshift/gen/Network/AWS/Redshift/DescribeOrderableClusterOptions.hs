{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeOrderableClusterOptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of orderable cluster options. Before you create a new
-- cluster you can use this operation to find what options are available,
-- such as the EC2 Availability Zones (AZ) in the specific AWS region that
-- you can specify, and the node types you can request. The node types
-- differ by available storage, memory, CPU and price. With the cost
-- involved you might want to obtain a list of cluster options in the
-- specific region and specify values when creating a cluster. For more
-- information about managing clusters, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeOrderableClusterOptions.html>
module Network.AWS.Redshift.DescribeOrderableClusterOptions
    (
    -- * Request
      DescribeOrderableClusterOptions
    -- ** Request constructor
    , describeOrderableClusterOptions
    -- ** Request lenses
    , docoMaxRecords
    , docoMarker
    , docoClusterVersion
    , docoNodeType

    -- * Response
    , DescribeOrderableClusterOptionsResponse
    -- ** Response constructor
    , describeOrderableClusterOptionsResponse
    -- ** Response lenses
    , docorsMarker
    , docorsOrderableClusterOptions
    , docorsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeOrderableClusterOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'docoMaxRecords'
--
-- * 'docoMarker'
--
-- * 'docoClusterVersion'
--
-- * 'docoNodeType'
data DescribeOrderableClusterOptions = DescribeOrderableClusterOptions'
    { _docoMaxRecords     :: !(Maybe Int)
    , _docoMarker         :: !(Maybe Text)
    , _docoClusterVersion :: !(Maybe Text)
    , _docoNodeType       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeOrderableClusterOptions' smart constructor.
describeOrderableClusterOptions :: DescribeOrderableClusterOptions
describeOrderableClusterOptions =
    DescribeOrderableClusterOptions'
    { _docoMaxRecords = Nothing
    , _docoMarker = Nothing
    , _docoClusterVersion = Nothing
    , _docoNodeType = Nothing
    }

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
docoMaxRecords :: Lens' DescribeOrderableClusterOptions (Maybe Int)
docoMaxRecords = lens _docoMaxRecords (\ s a -> s{_docoMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeOrderableClusterOptions request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
docoMarker :: Lens' DescribeOrderableClusterOptions (Maybe Text)
docoMarker = lens _docoMarker (\ s a -> s{_docoMarker = a});

-- | The version filter value. Specify this parameter to show only the
-- available offerings matching the specified version.
--
-- Default: All versions.
--
-- Constraints: Must be one of the version returned from
-- DescribeClusterVersions.
docoClusterVersion :: Lens' DescribeOrderableClusterOptions (Maybe Text)
docoClusterVersion = lens _docoClusterVersion (\ s a -> s{_docoClusterVersion = a});

-- | The node type filter value. Specify this parameter to show only the
-- available offerings matching the specified node type.
docoNodeType :: Lens' DescribeOrderableClusterOptions (Maybe Text)
docoNodeType = lens _docoNodeType (\ s a -> s{_docoNodeType = a});

instance AWSPager DescribeOrderableClusterOptions
         where
        page rq rs
          | stop (rs ^. docorsMarker) = Nothing
          | stop (rs ^. docorsOrderableClusterOptions) =
            Nothing
          | otherwise =
            Just $ rq & docoMarker .~ rs ^. docorsMarker

instance AWSRequest DescribeOrderableClusterOptions
         where
        type Sv DescribeOrderableClusterOptions = Redshift
        type Rs DescribeOrderableClusterOptions =
             DescribeOrderableClusterOptionsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeOrderableClusterOptionsResult"
              (\ s h x ->
                 DescribeOrderableClusterOptionsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "OrderableClusterOptions" .!@ mempty >>=
                        may (parseXMLList "OrderableClusterOption"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeOrderableClusterOptions
         where
        toHeaders = const mempty

instance ToPath DescribeOrderableClusterOptions where
        toPath = const "/"

instance ToQuery DescribeOrderableClusterOptions
         where
        toQuery DescribeOrderableClusterOptions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeOrderableClusterOptions" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "MaxRecords" =: _docoMaxRecords,
               "Marker" =: _docoMarker,
               "ClusterVersion" =: _docoClusterVersion,
               "NodeType" =: _docoNodeType]

-- | Contains the output from the DescribeOrderableClusterOptions action.
--
-- /See:/ 'describeOrderableClusterOptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'docorsMarker'
--
-- * 'docorsOrderableClusterOptions'
--
-- * 'docorsStatus'
data DescribeOrderableClusterOptionsResponse = DescribeOrderableClusterOptionsResponse'
    { _docorsMarker                  :: !(Maybe Text)
    , _docorsOrderableClusterOptions :: !(Maybe [OrderableClusterOption])
    , _docorsStatus                  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeOrderableClusterOptionsResponse' smart constructor.
describeOrderableClusterOptionsResponse :: Int -> DescribeOrderableClusterOptionsResponse
describeOrderableClusterOptionsResponse pStatus_ =
    DescribeOrderableClusterOptionsResponse'
    { _docorsMarker = Nothing
    , _docorsOrderableClusterOptions = Nothing
    , _docorsStatus = pStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
docorsMarker :: Lens' DescribeOrderableClusterOptionsResponse (Maybe Text)
docorsMarker = lens _docorsMarker (\ s a -> s{_docorsMarker = a});

-- | An OrderableClusterOption structure containing information about
-- orderable options for the Cluster.
docorsOrderableClusterOptions :: Lens' DescribeOrderableClusterOptionsResponse [OrderableClusterOption]
docorsOrderableClusterOptions = lens _docorsOrderableClusterOptions (\ s a -> s{_docorsOrderableClusterOptions = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
docorsStatus :: Lens' DescribeOrderableClusterOptionsResponse Int
docorsStatus = lens _docorsStatus (\ s a -> s{_docorsStatus = a});
