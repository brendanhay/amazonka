{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB cluster snapshots. This API action supports pagination.
--
--
-- For more information on Amazon Aurora, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS> in the /Amazon RDS User Guide./
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterSnapshots
    (
    -- * Creating a Request
      describeDBClusterSnapshots
    , DescribeDBClusterSnapshots
    -- * Request Lenses
    , ddbcsDBClusterIdentifier
    , ddbcsIncludeShared
    , ddbcsDBClusterSnapshotIdentifier
    , ddbcsFilters
    , ddbcsSnapshotType
    , ddbcsMarker
    , ddbcsMaxRecords
    , ddbcsIncludePublic

    -- * Destructuring the Response
    , describeDBClusterSnapshotsResponse
    , DescribeDBClusterSnapshotsResponse
    -- * Response Lenses
    , ddbcsrsMarker
    , ddbcsrsDBClusterSnapshots
    , ddbcsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeDBClusterSnapshots' smart constructor.
data DescribeDBClusterSnapshots = DescribeDBClusterSnapshots'
  { _ddbcsDBClusterIdentifier         :: !(Maybe Text)
  , _ddbcsIncludeShared               :: !(Maybe Bool)
  , _ddbcsDBClusterSnapshotIdentifier :: !(Maybe Text)
  , _ddbcsFilters                     :: !(Maybe [Filter])
  , _ddbcsSnapshotType                :: !(Maybe Text)
  , _ddbcsMarker                      :: !(Maybe Text)
  , _ddbcsMaxRecords                  :: !(Maybe Int)
  , _ddbcsIncludePublic               :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBClusterSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbcsDBClusterIdentifier' - The ID of the DB cluster to retrieve the list of DB cluster snapshots for. This parameter can't be used in conjunction with the @DBClusterSnapshotIdentifier@ parameter. This parameter is not case-sensitive.  Constraints:     * If supplied, must match the identifier of an existing DBCluster.
--
-- * 'ddbcsIncludeShared' - True to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore, and otherwise false. The default is @false@ . You can give an AWS account permission to restore a manual DB cluster snapshot from another AWS account by the 'ModifyDBClusterSnapshotAttribute' API action.
--
-- * 'ddbcsDBClusterSnapshotIdentifier' - A specific DB cluster snapshot identifier to describe. This parameter can't be used in conjunction with the @DBClusterIdentifier@ parameter. This value is stored as a lowercase string.  Constraints:     * If supplied, must match the identifier of an existing DBClusterSnapshot.     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
--
-- * 'ddbcsFilters' - This parameter is not currently supported.
--
-- * 'ddbcsSnapshotType' - The type of DB cluster snapshots to be returned. You can specify one of the following values:     * @automated@ - Return all DB cluster snapshots that have been automatically taken by Amazon RDS for my AWS account.     * @manual@ - Return all DB cluster snapshots that have been taken by my AWS account.     * @shared@ - Return all manual DB cluster snapshots that have been shared to my AWS account.     * @public@ - Return all DB cluster snapshots that have been marked as public. If you don't specify a @SnapshotType@ value, then both automated and manual DB cluster snapshots are returned. You can include shared DB cluster snapshots with these results by setting the @IncludeShared@ parameter to @true@ . You can include public DB cluster snapshots with these results by setting the @IncludePublic@ parameter to @true@ . The @IncludeShared@ and @IncludePublic@ parameters don't apply for @SnapshotType@ values of @manual@ or @automated@ . The @IncludePublic@ parameter doesn't apply when @SnapshotType@ is set to @shared@ . The @IncludeShared@ parameter doesn't apply when @SnapshotType@ is set to @public@ .
--
-- * 'ddbcsMarker' - An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddbcsMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'ddbcsIncludePublic' - True to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account, and otherwise false. The default is @false@ . The default is false. You can share a manual DB cluster snapshot as public by using the 'ModifyDBClusterSnapshotAttribute' API action.
describeDBClusterSnapshots
    :: DescribeDBClusterSnapshots
describeDBClusterSnapshots =
  DescribeDBClusterSnapshots'
    { _ddbcsDBClusterIdentifier = Nothing
    , _ddbcsIncludeShared = Nothing
    , _ddbcsDBClusterSnapshotIdentifier = Nothing
    , _ddbcsFilters = Nothing
    , _ddbcsSnapshotType = Nothing
    , _ddbcsMarker = Nothing
    , _ddbcsMaxRecords = Nothing
    , _ddbcsIncludePublic = Nothing
    }


-- | The ID of the DB cluster to retrieve the list of DB cluster snapshots for. This parameter can't be used in conjunction with the @DBClusterSnapshotIdentifier@ parameter. This parameter is not case-sensitive.  Constraints:     * If supplied, must match the identifier of an existing DBCluster.
ddbcsDBClusterIdentifier :: Lens' DescribeDBClusterSnapshots (Maybe Text)
ddbcsDBClusterIdentifier = lens _ddbcsDBClusterIdentifier (\ s a -> s{_ddbcsDBClusterIdentifier = a})

-- | True to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore, and otherwise false. The default is @false@ . You can give an AWS account permission to restore a manual DB cluster snapshot from another AWS account by the 'ModifyDBClusterSnapshotAttribute' API action.
ddbcsIncludeShared :: Lens' DescribeDBClusterSnapshots (Maybe Bool)
ddbcsIncludeShared = lens _ddbcsIncludeShared (\ s a -> s{_ddbcsIncludeShared = a})

-- | A specific DB cluster snapshot identifier to describe. This parameter can't be used in conjunction with the @DBClusterIdentifier@ parameter. This value is stored as a lowercase string.  Constraints:     * If supplied, must match the identifier of an existing DBClusterSnapshot.     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
ddbcsDBClusterSnapshotIdentifier :: Lens' DescribeDBClusterSnapshots (Maybe Text)
ddbcsDBClusterSnapshotIdentifier = lens _ddbcsDBClusterSnapshotIdentifier (\ s a -> s{_ddbcsDBClusterSnapshotIdentifier = a})

-- | This parameter is not currently supported.
ddbcsFilters :: Lens' DescribeDBClusterSnapshots [Filter]
ddbcsFilters = lens _ddbcsFilters (\ s a -> s{_ddbcsFilters = a}) . _Default . _Coerce

-- | The type of DB cluster snapshots to be returned. You can specify one of the following values:     * @automated@ - Return all DB cluster snapshots that have been automatically taken by Amazon RDS for my AWS account.     * @manual@ - Return all DB cluster snapshots that have been taken by my AWS account.     * @shared@ - Return all manual DB cluster snapshots that have been shared to my AWS account.     * @public@ - Return all DB cluster snapshots that have been marked as public. If you don't specify a @SnapshotType@ value, then both automated and manual DB cluster snapshots are returned. You can include shared DB cluster snapshots with these results by setting the @IncludeShared@ parameter to @true@ . You can include public DB cluster snapshots with these results by setting the @IncludePublic@ parameter to @true@ . The @IncludeShared@ and @IncludePublic@ parameters don't apply for @SnapshotType@ values of @manual@ or @automated@ . The @IncludePublic@ parameter doesn't apply when @SnapshotType@ is set to @shared@ . The @IncludeShared@ parameter doesn't apply when @SnapshotType@ is set to @public@ .
ddbcsSnapshotType :: Lens' DescribeDBClusterSnapshots (Maybe Text)
ddbcsSnapshotType = lens _ddbcsSnapshotType (\ s a -> s{_ddbcsSnapshotType = a})

-- | An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddbcsMarker :: Lens' DescribeDBClusterSnapshots (Maybe Text)
ddbcsMarker = lens _ddbcsMarker (\ s a -> s{_ddbcsMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddbcsMaxRecords :: Lens' DescribeDBClusterSnapshots (Maybe Int)
ddbcsMaxRecords = lens _ddbcsMaxRecords (\ s a -> s{_ddbcsMaxRecords = a})

-- | True to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account, and otherwise false. The default is @false@ . The default is false. You can share a manual DB cluster snapshot as public by using the 'ModifyDBClusterSnapshotAttribute' API action.
ddbcsIncludePublic :: Lens' DescribeDBClusterSnapshots (Maybe Bool)
ddbcsIncludePublic = lens _ddbcsIncludePublic (\ s a -> s{_ddbcsIncludePublic = a})

instance AWSPager DescribeDBClusterSnapshots where
        page rq rs
          | stop (rs ^. ddbcsrsMarker) = Nothing
          | stop (rs ^. ddbcsrsDBClusterSnapshots) = Nothing
          | otherwise =
            Just $ rq & ddbcsMarker .~ rs ^. ddbcsrsMarker

instance AWSRequest DescribeDBClusterSnapshots where
        type Rs DescribeDBClusterSnapshots =
             DescribeDBClusterSnapshotsResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DescribeDBClusterSnapshotsResult"
              (\ s h x ->
                 DescribeDBClusterSnapshotsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "DBClusterSnapshots" .!@ mempty >>=
                        may (parseXMLList "DBClusterSnapshot"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBClusterSnapshots where

instance NFData DescribeDBClusterSnapshots where

instance ToHeaders DescribeDBClusterSnapshots where
        toHeaders = const mempty

instance ToPath DescribeDBClusterSnapshots where
        toPath = const "/"

instance ToQuery DescribeDBClusterSnapshots where
        toQuery DescribeDBClusterSnapshots'{..}
          = mconcat
              ["Action" =:
                 ("DescribeDBClusterSnapshots" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterIdentifier" =: _ddbcsDBClusterIdentifier,
               "IncludeShared" =: _ddbcsIncludeShared,
               "DBClusterSnapshotIdentifier" =:
                 _ddbcsDBClusterSnapshotIdentifier,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddbcsFilters),
               "SnapshotType" =: _ddbcsSnapshotType,
               "Marker" =: _ddbcsMarker,
               "MaxRecords" =: _ddbcsMaxRecords,
               "IncludePublic" =: _ddbcsIncludePublic]

-- | Provides a list of DB cluster snapshots for the user as the result of a call to the 'DescribeDBClusterSnapshots' action.
--
--
--
-- /See:/ 'describeDBClusterSnapshotsResponse' smart constructor.
data DescribeDBClusterSnapshotsResponse = DescribeDBClusterSnapshotsResponse'
  { _ddbcsrsMarker             :: !(Maybe Text)
  , _ddbcsrsDBClusterSnapshots :: !(Maybe [DBClusterSnapshot])
  , _ddbcsrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBClusterSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbcsrsMarker' - An optional pagination token provided by a previous 'DescribeDBClusterSnapshots' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddbcsrsDBClusterSnapshots' - Provides a list of DB cluster snapshots for the user.
--
-- * 'ddbcsrsResponseStatus' - -- | The response status code.
describeDBClusterSnapshotsResponse
    :: Int -- ^ 'ddbcsrsResponseStatus'
    -> DescribeDBClusterSnapshotsResponse
describeDBClusterSnapshotsResponse pResponseStatus_ =
  DescribeDBClusterSnapshotsResponse'
    { _ddbcsrsMarker = Nothing
    , _ddbcsrsDBClusterSnapshots = Nothing
    , _ddbcsrsResponseStatus = pResponseStatus_
    }


-- | An optional pagination token provided by a previous 'DescribeDBClusterSnapshots' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddbcsrsMarker :: Lens' DescribeDBClusterSnapshotsResponse (Maybe Text)
ddbcsrsMarker = lens _ddbcsrsMarker (\ s a -> s{_ddbcsrsMarker = a})

-- | Provides a list of DB cluster snapshots for the user.
ddbcsrsDBClusterSnapshots :: Lens' DescribeDBClusterSnapshotsResponse [DBClusterSnapshot]
ddbcsrsDBClusterSnapshots = lens _ddbcsrsDBClusterSnapshots (\ s a -> s{_ddbcsrsDBClusterSnapshots = a}) . _Default . _Coerce

-- | -- | The response status code.
ddbcsrsResponseStatus :: Lens' DescribeDBClusterSnapshotsResponse Int
ddbcsrsResponseStatus = lens _ddbcsrsResponseStatus (\ s a -> s{_ddbcsrsResponseStatus = a})

instance NFData DescribeDBClusterSnapshotsResponse
         where
