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
-- Module      : Network.AWS.RDS.DescribeDBSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB snapshots. This API action supports pagination.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBSnapshots
    (
    -- * Creating a Request
      describeDBSnapshots
    , DescribeDBSnapshots
    -- * Request Lenses
    , ddsIncludeShared
    , ddsFilters
    , ddsDBSnapshotIdentifier
    , ddsSnapshotType
    , ddsDBInstanceIdentifier
    , ddsMarker
    , ddsMaxRecords
    , ddsIncludePublic

    -- * Destructuring the Response
    , describeDBSnapshotsResponse
    , DescribeDBSnapshotsResponse
    -- * Response Lenses
    , ddsrsMarker
    , ddsrsDBSnapshots
    , ddsrsResponseStatus
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
-- /See:/ 'describeDBSnapshots' smart constructor.
data DescribeDBSnapshots = DescribeDBSnapshots'
  { _ddsIncludeShared        :: !(Maybe Bool)
  , _ddsFilters              :: !(Maybe [Filter])
  , _ddsDBSnapshotIdentifier :: !(Maybe Text)
  , _ddsSnapshotType         :: !(Maybe Text)
  , _ddsDBInstanceIdentifier :: !(Maybe Text)
  , _ddsMarker               :: !(Maybe Text)
  , _ddsMaxRecords           :: !(Maybe Int)
  , _ddsIncludePublic        :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsIncludeShared' - True to include shared manual DB snapshots from other AWS accounts that this AWS account has been given permission to copy or restore, and otherwise false. The default is @false@ . You can give an AWS account permission to restore a manual DB snapshot from another AWS account by using the 'ModifyDBSnapshotAttribute' API action.
--
-- * 'ddsFilters' - This parameter is not currently supported.
--
-- * 'ddsDBSnapshotIdentifier' - A specific DB snapshot identifier to describe. This parameter can't be used in conjunction with @DBInstanceIdentifier@ . This value is stored as a lowercase string.  Constraints:     * If supplied, must match the identifier of an existing DBSnapshot.     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
--
-- * 'ddsSnapshotType' - The type of snapshots to be returned. You can specify one of the following values:     * @automated@ - Return all DB snapshots that have been automatically taken by Amazon RDS for my AWS account.     * @manual@ - Return all DB snapshots that have been taken by my AWS account.     * @shared@ - Return all manual DB snapshots that have been shared to my AWS account.     * @public@ - Return all DB snapshots that have been marked as public. If you don't specify a @SnapshotType@ value, then both automated and manual snapshots are returned. Shared and public DB snapshots are not included in the returned results by default. You can include shared snapshots with these results by setting the @IncludeShared@ parameter to @true@ . You can include public snapshots with these results by setting the @IncludePublic@ parameter to @true@ . The @IncludeShared@ and @IncludePublic@ parameters don't apply for @SnapshotType@ values of @manual@ or @automated@ . The @IncludePublic@ parameter doesn't apply when @SnapshotType@ is set to @shared@ . The @IncludeShared@ parameter doesn't apply when @SnapshotType@ is set to @public@ .
--
-- * 'ddsDBInstanceIdentifier' - The ID of the DB instance to retrieve the list of DB snapshots for. This parameter can't be used in conjunction with @DBSnapshotIdentifier@ . This parameter is not case-sensitive.  Constraints:     * If supplied, must match the identifier of an existing DBInstance.
--
-- * 'ddsMarker' - An optional pagination token provided by a previous @DescribeDBSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddsMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'ddsIncludePublic' - True to include manual DB snapshots that are public and can be copied or restored by any AWS account, and otherwise false. The default is false. You can share a manual DB snapshot as public by using the 'ModifyDBSnapshotAttribute' API.
describeDBSnapshots
    :: DescribeDBSnapshots
describeDBSnapshots =
  DescribeDBSnapshots'
    { _ddsIncludeShared = Nothing
    , _ddsFilters = Nothing
    , _ddsDBSnapshotIdentifier = Nothing
    , _ddsSnapshotType = Nothing
    , _ddsDBInstanceIdentifier = Nothing
    , _ddsMarker = Nothing
    , _ddsMaxRecords = Nothing
    , _ddsIncludePublic = Nothing
    }


-- | True to include shared manual DB snapshots from other AWS accounts that this AWS account has been given permission to copy or restore, and otherwise false. The default is @false@ . You can give an AWS account permission to restore a manual DB snapshot from another AWS account by using the 'ModifyDBSnapshotAttribute' API action.
ddsIncludeShared :: Lens' DescribeDBSnapshots (Maybe Bool)
ddsIncludeShared = lens _ddsIncludeShared (\ s a -> s{_ddsIncludeShared = a})

-- | This parameter is not currently supported.
ddsFilters :: Lens' DescribeDBSnapshots [Filter]
ddsFilters = lens _ddsFilters (\ s a -> s{_ddsFilters = a}) . _Default . _Coerce

-- | A specific DB snapshot identifier to describe. This parameter can't be used in conjunction with @DBInstanceIdentifier@ . This value is stored as a lowercase string.  Constraints:     * If supplied, must match the identifier of an existing DBSnapshot.     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
ddsDBSnapshotIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddsDBSnapshotIdentifier = lens _ddsDBSnapshotIdentifier (\ s a -> s{_ddsDBSnapshotIdentifier = a})

-- | The type of snapshots to be returned. You can specify one of the following values:     * @automated@ - Return all DB snapshots that have been automatically taken by Amazon RDS for my AWS account.     * @manual@ - Return all DB snapshots that have been taken by my AWS account.     * @shared@ - Return all manual DB snapshots that have been shared to my AWS account.     * @public@ - Return all DB snapshots that have been marked as public. If you don't specify a @SnapshotType@ value, then both automated and manual snapshots are returned. Shared and public DB snapshots are not included in the returned results by default. You can include shared snapshots with these results by setting the @IncludeShared@ parameter to @true@ . You can include public snapshots with these results by setting the @IncludePublic@ parameter to @true@ . The @IncludeShared@ and @IncludePublic@ parameters don't apply for @SnapshotType@ values of @manual@ or @automated@ . The @IncludePublic@ parameter doesn't apply when @SnapshotType@ is set to @shared@ . The @IncludeShared@ parameter doesn't apply when @SnapshotType@ is set to @public@ .
ddsSnapshotType :: Lens' DescribeDBSnapshots (Maybe Text)
ddsSnapshotType = lens _ddsSnapshotType (\ s a -> s{_ddsSnapshotType = a})

-- | The ID of the DB instance to retrieve the list of DB snapshots for. This parameter can't be used in conjunction with @DBSnapshotIdentifier@ . This parameter is not case-sensitive.  Constraints:     * If supplied, must match the identifier of an existing DBInstance.
ddsDBInstanceIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddsDBInstanceIdentifier = lens _ddsDBInstanceIdentifier (\ s a -> s{_ddsDBInstanceIdentifier = a})

-- | An optional pagination token provided by a previous @DescribeDBSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddsMarker :: Lens' DescribeDBSnapshots (Maybe Text)
ddsMarker = lens _ddsMarker (\ s a -> s{_ddsMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddsMaxRecords :: Lens' DescribeDBSnapshots (Maybe Int)
ddsMaxRecords = lens _ddsMaxRecords (\ s a -> s{_ddsMaxRecords = a})

-- | True to include manual DB snapshots that are public and can be copied or restored by any AWS account, and otherwise false. The default is false. You can share a manual DB snapshot as public by using the 'ModifyDBSnapshotAttribute' API.
ddsIncludePublic :: Lens' DescribeDBSnapshots (Maybe Bool)
ddsIncludePublic = lens _ddsIncludePublic (\ s a -> s{_ddsIncludePublic = a})

instance AWSPager DescribeDBSnapshots where
        page rq rs
          | stop (rs ^. ddsrsMarker) = Nothing
          | stop (rs ^. ddsrsDBSnapshots) = Nothing
          | otherwise =
            Just $ rq & ddsMarker .~ rs ^. ddsrsMarker

instance AWSRequest DescribeDBSnapshots where
        type Rs DescribeDBSnapshots =
             DescribeDBSnapshotsResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeDBSnapshotsResult"
              (\ s h x ->
                 DescribeDBSnapshotsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "DBSnapshots" .!@ mempty >>=
                        may (parseXMLList "DBSnapshot"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBSnapshots where

instance NFData DescribeDBSnapshots where

instance ToHeaders DescribeDBSnapshots where
        toHeaders = const mempty

instance ToPath DescribeDBSnapshots where
        toPath = const "/"

instance ToQuery DescribeDBSnapshots where
        toQuery DescribeDBSnapshots'{..}
          = mconcat
              ["Action" =: ("DescribeDBSnapshots" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "IncludeShared" =: _ddsIncludeShared,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddsFilters),
               "DBSnapshotIdentifier" =: _ddsDBSnapshotIdentifier,
               "SnapshotType" =: _ddsSnapshotType,
               "DBInstanceIdentifier" =: _ddsDBInstanceIdentifier,
               "Marker" =: _ddsMarker,
               "MaxRecords" =: _ddsMaxRecords,
               "IncludePublic" =: _ddsIncludePublic]

-- | Contains the result of a successful invocation of the 'DescribeDBSnapshots' action.
--
--
--
-- /See:/ 'describeDBSnapshotsResponse' smart constructor.
data DescribeDBSnapshotsResponse = DescribeDBSnapshotsResponse'
  { _ddsrsMarker         :: !(Maybe Text)
  , _ddsrsDBSnapshots    :: !(Maybe [DBSnapshot])
  , _ddsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddsrsDBSnapshots' - A list of 'DBSnapshot' instances.
--
-- * 'ddsrsResponseStatus' - -- | The response status code.
describeDBSnapshotsResponse
    :: Int -- ^ 'ddsrsResponseStatus'
    -> DescribeDBSnapshotsResponse
describeDBSnapshotsResponse pResponseStatus_ =
  DescribeDBSnapshotsResponse'
    { _ddsrsMarker = Nothing
    , _ddsrsDBSnapshots = Nothing
    , _ddsrsResponseStatus = pResponseStatus_
    }


-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddsrsMarker :: Lens' DescribeDBSnapshotsResponse (Maybe Text)
ddsrsMarker = lens _ddsrsMarker (\ s a -> s{_ddsrsMarker = a})

-- | A list of 'DBSnapshot' instances.
ddsrsDBSnapshots :: Lens' DescribeDBSnapshotsResponse [DBSnapshot]
ddsrsDBSnapshots = lens _ddsrsDBSnapshots (\ s a -> s{_ddsrsDBSnapshots = a}) . _Default . _Coerce

-- | -- | The response status code.
ddsrsResponseStatus :: Lens' DescribeDBSnapshotsResponse Int
ddsrsResponseStatus = lens _ddsrsResponseStatus (\ s a -> s{_ddsrsResponseStatus = a})

instance NFData DescribeDBSnapshotsResponse where
