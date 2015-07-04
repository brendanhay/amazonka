{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.Redshift.DescribeSnapshotCopyGrants
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

-- | Returns a list of snapshot copy grants owned by the AWS account in the
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
    , dscg1TagValues
    , dscg1TagKeys
    , dscg1MaxRecords
    , dscg1Marker
    , dscg1SnapshotCopyGrantName

    -- * Response
    , DescribeSnapshotCopyGrantsResponse
    -- ** Response constructor
    , describeSnapshotCopyGrantsResponse
    -- ** Response lenses
    , dscgrSnapshotCopyGrants
    , dscgrMarker
    , dscgrStatus
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
-- * 'dscg1TagValues'
--
-- * 'dscg1TagKeys'
--
-- * 'dscg1MaxRecords'
--
-- * 'dscg1Marker'
--
-- * 'dscg1SnapshotCopyGrantName'
data DescribeSnapshotCopyGrants = DescribeSnapshotCopyGrants'
    { _dscg1TagValues             :: !(Maybe [Text])
    , _dscg1TagKeys               :: !(Maybe [Text])
    , _dscg1MaxRecords            :: !(Maybe Int)
    , _dscg1Marker                :: !(Maybe Text)
    , _dscg1SnapshotCopyGrantName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotCopyGrants' smart constructor.
describeSnapshotCopyGrants :: DescribeSnapshotCopyGrants
describeSnapshotCopyGrants =
    DescribeSnapshotCopyGrants'
    { _dscg1TagValues = Nothing
    , _dscg1TagKeys = Nothing
    , _dscg1MaxRecords = Nothing
    , _dscg1Marker = Nothing
    , _dscg1SnapshotCopyGrantName = Nothing
    }

-- | A tag value or values for which you want to return all matching
-- resources that are associated with the specified value or values. For
-- example, suppose that you have resources tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with all resources that have
-- either or both of these tag values associated with them.
dscg1TagValues :: Lens' DescribeSnapshotCopyGrants [Text]
dscg1TagValues = lens _dscg1TagValues (\ s a -> s{_dscg1TagValues = a}) . _Default;

-- | A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with all resources that have either
-- or both of these tag keys associated with them.
dscg1TagKeys :: Lens' DescribeSnapshotCopyGrants [Text]
dscg1TagKeys = lens _dscg1TagKeys (\ s a -> s{_dscg1TagKeys = a}) . _Default;

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dscg1MaxRecords :: Lens' DescribeSnapshotCopyGrants (Maybe Int)
dscg1MaxRecords = lens _dscg1MaxRecords (\ s a -> s{_dscg1MaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
dscg1Marker :: Lens' DescribeSnapshotCopyGrants (Maybe Text)
dscg1Marker = lens _dscg1Marker (\ s a -> s{_dscg1Marker = a});

-- | The name of the snapshot copy grant.
dscg1SnapshotCopyGrantName :: Lens' DescribeSnapshotCopyGrants (Maybe Text)
dscg1SnapshotCopyGrantName = lens _dscg1SnapshotCopyGrantName (\ s a -> s{_dscg1SnapshotCopyGrantName = a});

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
                 toQuery (toQueryList "TagValue" <$> _dscg1TagValues),
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dscg1TagKeys),
               "MaxRecords" =: _dscg1MaxRecords,
               "Marker" =: _dscg1Marker,
               "SnapshotCopyGrantName" =:
                 _dscg1SnapshotCopyGrantName]

-- | The result of the snapshot copy grant.
--
-- /See:/ 'describeSnapshotCopyGrantsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscgrSnapshotCopyGrants'
--
-- * 'dscgrMarker'
--
-- * 'dscgrStatus'
data DescribeSnapshotCopyGrantsResponse = DescribeSnapshotCopyGrantsResponse'
    { _dscgrSnapshotCopyGrants :: !(Maybe [SnapshotCopyGrant])
    , _dscgrMarker             :: !(Maybe Text)
    , _dscgrStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotCopyGrantsResponse' smart constructor.
describeSnapshotCopyGrantsResponse :: Int -> DescribeSnapshotCopyGrantsResponse
describeSnapshotCopyGrantsResponse pStatus =
    DescribeSnapshotCopyGrantsResponse'
    { _dscgrSnapshotCopyGrants = Nothing
    , _dscgrMarker = Nothing
    , _dscgrStatus = pStatus
    }

-- | The list of snapshot copy grants.
dscgrSnapshotCopyGrants :: Lens' DescribeSnapshotCopyGrantsResponse [SnapshotCopyGrant]
dscgrSnapshotCopyGrants = lens _dscgrSnapshotCopyGrants (\ s a -> s{_dscgrSnapshotCopyGrants = a}) . _Default;

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
dscgrMarker :: Lens' DescribeSnapshotCopyGrantsResponse (Maybe Text)
dscgrMarker = lens _dscgrMarker (\ s a -> s{_dscgrMarker = a});

-- | FIXME: Undocumented member.
dscgrStatus :: Lens' DescribeSnapshotCopyGrantsResponse Int
dscgrStatus = lens _dscgrStatus (\ s a -> s{_dscgrStatus = a});
