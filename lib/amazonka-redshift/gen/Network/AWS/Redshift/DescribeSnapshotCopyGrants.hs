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
-- Module      : Network.AWS.Redshift.DescribeSnapshotCopyGrants
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of snapshot copy grants owned by the AWS account in the destination region.
--
--
-- For more information about managing snapshot copy grants, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.DescribeSnapshotCopyGrants
    (
    -- * Creating a Request
      describeSnapshotCopyGrants
    , DescribeSnapshotCopyGrants
    -- * Request Lenses
    , dscgsTagValues
    , dscgsTagKeys
    , dscgsMarker
    , dscgsMaxRecords
    , dscgsSnapshotCopyGrantName

    -- * Destructuring the Response
    , describeSnapshotCopyGrantsResponse
    , DescribeSnapshotCopyGrantsResponse
    -- * Response Lenses
    , dscgrsSnapshotCopyGrants
    , dscgrsMarker
    , dscgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | The result of the @DescribeSnapshotCopyGrants@ action.
--
--
--
-- /See:/ 'describeSnapshotCopyGrants' smart constructor.
data DescribeSnapshotCopyGrants = DescribeSnapshotCopyGrants'
  { _dscgsTagValues             :: !(Maybe [Text])
  , _dscgsTagKeys               :: !(Maybe [Text])
  , _dscgsMarker                :: !(Maybe Text)
  , _dscgsMaxRecords            :: !(Maybe Int)
  , _dscgsSnapshotCopyGrantName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSnapshotCopyGrants' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscgsTagValues' - A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
--
-- * 'dscgsTagKeys' - A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
--
-- * 'dscgsMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.  Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
--
-- * 'dscgsMaxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
--
-- * 'dscgsSnapshotCopyGrantName' - The name of the snapshot copy grant.
describeSnapshotCopyGrants
    :: DescribeSnapshotCopyGrants
describeSnapshotCopyGrants =
  DescribeSnapshotCopyGrants'
    { _dscgsTagValues = Nothing
    , _dscgsTagKeys = Nothing
    , _dscgsMarker = Nothing
    , _dscgsMaxRecords = Nothing
    , _dscgsSnapshotCopyGrantName = Nothing
    }


-- | A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
dscgsTagValues :: Lens' DescribeSnapshotCopyGrants [Text]
dscgsTagValues = lens _dscgsTagValues (\ s a -> s{_dscgsTagValues = a}) . _Default . _Coerce

-- | A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
dscgsTagKeys :: Lens' DescribeSnapshotCopyGrants [Text]
dscgsTagKeys = lens _dscgsTagKeys (\ s a -> s{_dscgsTagKeys = a}) . _Default . _Coerce

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.  Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
dscgsMarker :: Lens' DescribeSnapshotCopyGrants (Maybe Text)
dscgsMarker = lens _dscgsMarker (\ s a -> s{_dscgsMarker = a})

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
dscgsMaxRecords :: Lens' DescribeSnapshotCopyGrants (Maybe Int)
dscgsMaxRecords = lens _dscgsMaxRecords (\ s a -> s{_dscgsMaxRecords = a})

-- | The name of the snapshot copy grant.
dscgsSnapshotCopyGrantName :: Lens' DescribeSnapshotCopyGrants (Maybe Text)
dscgsSnapshotCopyGrantName = lens _dscgsSnapshotCopyGrantName (\ s a -> s{_dscgsSnapshotCopyGrantName = a})

instance AWSRequest DescribeSnapshotCopyGrants where
        type Rs DescribeSnapshotCopyGrants =
             DescribeSnapshotCopyGrantsResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "DescribeSnapshotCopyGrantsResult"
              (\ s h x ->
                 DescribeSnapshotCopyGrantsResponse' <$>
                   (x .@? "SnapshotCopyGrants" .!@ mempty >>=
                      may (parseXMLList "SnapshotCopyGrant"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSnapshotCopyGrants where

instance NFData DescribeSnapshotCopyGrants where

instance ToHeaders DescribeSnapshotCopyGrants where
        toHeaders = const mempty

instance ToPath DescribeSnapshotCopyGrants where
        toPath = const "/"

instance ToQuery DescribeSnapshotCopyGrants where
        toQuery DescribeSnapshotCopyGrants'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSnapshotCopyGrants" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dscgsTagValues),
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dscgsTagKeys),
               "Marker" =: _dscgsMarker,
               "MaxRecords" =: _dscgsMaxRecords,
               "SnapshotCopyGrantName" =:
                 _dscgsSnapshotCopyGrantName]

-- |
--
--
--
-- /See:/ 'describeSnapshotCopyGrantsResponse' smart constructor.
data DescribeSnapshotCopyGrantsResponse = DescribeSnapshotCopyGrantsResponse'
  { _dscgrsSnapshotCopyGrants :: !(Maybe [SnapshotCopyGrant])
  , _dscgrsMarker             :: !(Maybe Text)
  , _dscgrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSnapshotCopyGrantsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscgrsSnapshotCopyGrants' - The list of @SnapshotCopyGrant@ objects.
--
-- * 'dscgrsMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.  Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
--
-- * 'dscgrsResponseStatus' - -- | The response status code.
describeSnapshotCopyGrantsResponse
    :: Int -- ^ 'dscgrsResponseStatus'
    -> DescribeSnapshotCopyGrantsResponse
describeSnapshotCopyGrantsResponse pResponseStatus_ =
  DescribeSnapshotCopyGrantsResponse'
    { _dscgrsSnapshotCopyGrants = Nothing
    , _dscgrsMarker = Nothing
    , _dscgrsResponseStatus = pResponseStatus_
    }


-- | The list of @SnapshotCopyGrant@ objects.
dscgrsSnapshotCopyGrants :: Lens' DescribeSnapshotCopyGrantsResponse [SnapshotCopyGrant]
dscgrsSnapshotCopyGrants = lens _dscgrsSnapshotCopyGrants (\ s a -> s{_dscgrsSnapshotCopyGrants = a}) . _Default . _Coerce

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.  Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
dscgrsMarker :: Lens' DescribeSnapshotCopyGrantsResponse (Maybe Text)
dscgrsMarker = lens _dscgrsMarker (\ s a -> s{_dscgrsMarker = a})

-- | -- | The response status code.
dscgrsResponseStatus :: Lens' DescribeSnapshotCopyGrantsResponse Int
dscgrsResponseStatus = lens _dscgrsResponseStatus (\ s a -> s{_dscgrsResponseStatus = a})

instance NFData DescribeSnapshotCopyGrantsResponse
         where
