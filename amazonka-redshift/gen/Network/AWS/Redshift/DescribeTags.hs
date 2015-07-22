{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags. You can return tags from a specific resource by
-- specifying an ARN, or you can return all tags for a given type of
-- resource, such as clusters, snapshots, and so on.
--
-- The following are limitations for @DescribeTags@:
--
-- -   You cannot specify an ARN and a resource-type value together in the
--     same request.
-- -   You cannot use the @MaxRecords@ and @Marker@ parameters together
--     with the ARN parameter.
-- -   The @MaxRecords@ parameter can be a range from 10 to 50 results to
--     return in a request.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all resources that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- resources that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, resources are
-- returned regardless of whether they have tag keys or values associated
-- with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeTags.html>
module Network.AWS.Redshift.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , describeTags
    -- ** Request lenses
    , dtrqResourceType
    , dtrqTagValues
    , dtrqResourceName
    , dtrqTagKeys
    , dtrqMaxRecords
    , dtrqMarker

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , describeTagsResponse
    -- ** Response lenses
    , dtrsMarker
    , dtrsTaggedResources
    , dtrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the output from the @DescribeTags@ action.
--
-- /See:/ 'describeTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrqResourceType'
--
-- * 'dtrqTagValues'
--
-- * 'dtrqResourceName'
--
-- * 'dtrqTagKeys'
--
-- * 'dtrqMaxRecords'
--
-- * 'dtrqMarker'
data DescribeTags = DescribeTags'
    { _dtrqResourceType :: !(Maybe Text)
    , _dtrqTagValues    :: !(Maybe [Text])
    , _dtrqResourceName :: !(Maybe Text)
    , _dtrqTagKeys      :: !(Maybe [Text])
    , _dtrqMaxRecords   :: !(Maybe Int)
    , _dtrqMarker       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTags' smart constructor.
describeTags :: DescribeTags
describeTags =
    DescribeTags'
    { _dtrqResourceType = Nothing
    , _dtrqTagValues = Nothing
    , _dtrqResourceName = Nothing
    , _dtrqTagKeys = Nothing
    , _dtrqMaxRecords = Nothing
    , _dtrqMarker = Nothing
    }

-- | The type of resource with which you want to view tags. Valid resource
-- types are:
--
-- -   Cluster
-- -   CIDR\/IP
-- -   EC2 security group
-- -   Snapshot
-- -   Cluster security group
-- -   Subnet group
-- -   HSM connection
-- -   HSM certificate
-- -   Parameter group
-- -   Snapshot copy grant
--
-- For more information about Amazon Redshift resource types and
-- constructing ARNs, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/constructing-redshift-arn.html Constructing an Amazon Redshift Amazon Resource Name (ARN)>
-- in the Amazon Redshift Cluster Management Guide.
dtrqResourceType :: Lens' DescribeTags (Maybe Text)
dtrqResourceType = lens _dtrqResourceType (\ s a -> s{_dtrqResourceType = a});

-- | A tag value or values for which you want to return all matching
-- resources that are associated with the specified value or values. For
-- example, suppose that you have resources tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with all resources that have
-- either or both of these tag values associated with them.
dtrqTagValues :: Lens' DescribeTags [Text]
dtrqTagValues = lens _dtrqTagValues (\ s a -> s{_dtrqTagValues = a}) . _Default;

-- | The Amazon Resource Name (ARN) for which you want to describe the tag or
-- tags. For example, @arn:aws:redshift:us-east-1:123456789:cluster:t1@.
dtrqResourceName :: Lens' DescribeTags (Maybe Text)
dtrqResourceName = lens _dtrqResourceName (\ s a -> s{_dtrqResourceName = a});

-- | A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with all resources that have either
-- or both of these tag keys associated with them.
dtrqTagKeys :: Lens' DescribeTags [Text]
dtrqTagKeys = lens _dtrqTagKeys (\ s a -> s{_dtrqTagKeys = a}) . _Default;

-- | The maximum number or response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned @marker@ value.
dtrqMaxRecords :: Lens' DescribeTags (Maybe Int)
dtrqMaxRecords = lens _dtrqMaxRecords (\ s a -> s{_dtrqMaxRecords = a});

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
dtrqMarker :: Lens' DescribeTags (Maybe Text)
dtrqMarker = lens _dtrqMarker (\ s a -> s{_dtrqMarker = a});

instance AWSRequest DescribeTags where
        type Sv DescribeTags = Redshift
        type Rs DescribeTags = DescribeTagsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeTagsResult"
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "TaggedResources" .!@ mempty >>=
                        may (parseXMLList "TaggedResource"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeTags where
        toHeaders = const mempty

instance ToPath DescribeTags where
        toPath = const "/"

instance ToQuery DescribeTags where
        toQuery DescribeTags'{..}
          = mconcat
              ["Action" =: ("DescribeTags" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ResourceType" =: _dtrqResourceType,
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dtrqTagValues),
               "ResourceName" =: _dtrqResourceName,
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dtrqTagKeys),
               "MaxRecords" =: _dtrqMaxRecords,
               "Marker" =: _dtrqMarker]

-- | Contains the output from the @DescribeTags@ action.
--
-- /See:/ 'describeTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrsMarker'
--
-- * 'dtrsTaggedResources'
--
-- * 'dtrsStatus'
data DescribeTagsResponse = DescribeTagsResponse'
    { _dtrsMarker          :: !(Maybe Text)
    , _dtrsTaggedResources :: !(Maybe [TaggedResource])
    , _dtrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTagsResponse' smart constructor.
describeTagsResponse :: Int -> DescribeTagsResponse
describeTagsResponse pStatus_ =
    DescribeTagsResponse'
    { _dtrsMarker = Nothing
    , _dtrsTaggedResources = Nothing
    , _dtrsStatus = pStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dtrsMarker :: Lens' DescribeTagsResponse (Maybe Text)
dtrsMarker = lens _dtrsMarker (\ s a -> s{_dtrsMarker = a});

-- | A list of tags with their associated resources.
dtrsTaggedResources :: Lens' DescribeTagsResponse [TaggedResource]
dtrsTaggedResources = lens _dtrsTaggedResources (\ s a -> s{_dtrsTaggedResources = a}) . _Default;

-- | FIXME: Undocumented member.
dtrsStatus :: Lens' DescribeTagsResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
