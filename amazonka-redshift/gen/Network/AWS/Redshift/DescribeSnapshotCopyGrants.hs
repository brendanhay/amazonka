{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeSnapshotCopyGrants
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of snapshot copy grants owned by the AWS account in the
-- destination region.
--
-- For more information about managing snapshot copy grants, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeSnapshotCopyGrants.html>
module Network.AWS.Redshift.DescribeSnapshotCopyGrants
    (
    -- * Request
      DescribeSnapshotCopyGrants
    -- ** Request constructor
    , describeSnapshotCopyGrants
    -- ** Request lenses
    , dscgsTagValues
    , dscgsTagKeys
    , dscgsMaxRecords
    , dscgsMarker
    , dscgsSnapshotCopyGrantName

    -- * Response
    , DescribeSnapshotCopyGrantsResponse
    -- ** Response constructor
    , describeSnapshotCopyGrantsResponse
    -- ** Response lenses
    , dscgrsSnapshotCopyGrants
    , dscgrsMarker
    , dscgrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | The result of the @DescribeSnapshotCopyGrants@ action.
--
-- /See:/ 'describeSnapshotCopyGrants' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscgsTagValues'
--
-- * 'dscgsTagKeys'
--
-- * 'dscgsMaxRecords'
--
-- * 'dscgsMarker'
--
-- * 'dscgsSnapshotCopyGrantName'
data DescribeSnapshotCopyGrants = DescribeSnapshotCopyGrants'
    { _dscgsTagValues             :: !(Maybe [Text])
    , _dscgsTagKeys               :: !(Maybe [Text])
    , _dscgsMaxRecords            :: !(Maybe Int)
    , _dscgsMarker                :: !(Maybe Text)
    , _dscgsSnapshotCopyGrantName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotCopyGrants' smart constructor.
describeSnapshotCopyGrants :: DescribeSnapshotCopyGrants
describeSnapshotCopyGrants =
    DescribeSnapshotCopyGrants'
    { _dscgsTagValues = Nothing
    , _dscgsTagKeys = Nothing
    , _dscgsMaxRecords = Nothing
    , _dscgsMarker = Nothing
    , _dscgsSnapshotCopyGrantName = Nothing
    }

-- | A tag value or values for which you want to return all matching
-- resources that are associated with the specified value or values. For
-- example, suppose that you have resources tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with all resources that have
-- either or both of these tag values associated with them.
dscgsTagValues :: Lens' DescribeSnapshotCopyGrants [Text]
dscgsTagValues = lens _dscgsTagValues (\ s a -> s{_dscgsTagValues = a}) . _Default . _Coerce;

-- | A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with all resources that have either
-- or both of these tag keys associated with them.
dscgsTagKeys :: Lens' DescribeSnapshotCopyGrants [Text]
dscgsTagKeys = lens _dscgsTagKeys (\ s a -> s{_dscgsTagKeys = a}) . _Default . _Coerce;

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dscgsMaxRecords :: Lens' DescribeSnapshotCopyGrants (Maybe Int)
dscgsMaxRecords = lens _dscgsMaxRecords (\ s a -> s{_dscgsMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
dscgsMarker :: Lens' DescribeSnapshotCopyGrants (Maybe Text)
dscgsMarker = lens _dscgsMarker (\ s a -> s{_dscgsMarker = a});

-- | The name of the snapshot copy grant.
dscgsSnapshotCopyGrantName :: Lens' DescribeSnapshotCopyGrants (Maybe Text)
dscgsSnapshotCopyGrantName = lens _dscgsSnapshotCopyGrantName (\ s a -> s{_dscgsSnapshotCopyGrantName = a});

instance AWSRequest DescribeSnapshotCopyGrants where
        type Sv DescribeSnapshotCopyGrants = Redshift
        type Rs DescribeSnapshotCopyGrants =
             DescribeSnapshotCopyGrantsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeSnapshotCopyGrantsResult"
              (\ s h x ->
                 DescribeSnapshotCopyGrantsResponse' <$>
                   (x .@? "SnapshotCopyGrants" .!@ mempty >>=
                      may (parseXMLList "SnapshotCopyGrant"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
               "MaxRecords" =: _dscgsMaxRecords,
               "Marker" =: _dscgsMarker,
               "SnapshotCopyGrantName" =:
                 _dscgsSnapshotCopyGrantName]

-- | The result of the snapshot copy grant.
--
-- /See:/ 'describeSnapshotCopyGrantsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscgrsSnapshotCopyGrants'
--
-- * 'dscgrsMarker'
--
-- * 'dscgrsStatus'
data DescribeSnapshotCopyGrantsResponse = DescribeSnapshotCopyGrantsResponse'
    { _dscgrsSnapshotCopyGrants :: !(Maybe [SnapshotCopyGrant])
    , _dscgrsMarker             :: !(Maybe Text)
    , _dscgrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotCopyGrantsResponse' smart constructor.
describeSnapshotCopyGrantsResponse :: Int -> DescribeSnapshotCopyGrantsResponse
describeSnapshotCopyGrantsResponse pStatus_ =
    DescribeSnapshotCopyGrantsResponse'
    { _dscgrsSnapshotCopyGrants = Nothing
    , _dscgrsMarker = Nothing
    , _dscgrsStatus = pStatus_
    }

-- | The list of snapshot copy grants.
dscgrsSnapshotCopyGrants :: Lens' DescribeSnapshotCopyGrantsResponse [SnapshotCopyGrant]
dscgrsSnapshotCopyGrants = lens _dscgrsSnapshotCopyGrants (\ s a -> s{_dscgrsSnapshotCopyGrants = a}) . _Default . _Coerce;

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
dscgrsMarker :: Lens' DescribeSnapshotCopyGrantsResponse (Maybe Text)
dscgrsMarker = lens _dscgrsMarker (\ s a -> s{_dscgrsMarker = a});

-- | FIXME: Undocumented member.
dscgrsStatus :: Lens' DescribeSnapshotCopyGrantsResponse Int
dscgrsStatus = lens _dscgrsStatus (\ s a -> s{_dscgrsStatus = a});
