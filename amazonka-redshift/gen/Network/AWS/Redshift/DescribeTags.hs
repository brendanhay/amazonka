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
-- Module      : Network.AWS.Redshift.DescribeTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags. You can return tags from a specific resource by
-- specifying an ARN, or you can return all tags for a given type of
-- resource, such as clusters, snapshots, and so on.
--
-- The following are limitations for 'DescribeTags':
--
-- -   You cannot specify an ARN and a resource-type value together in the
--     same request.
-- -   You cannot use the 'MaxRecords' and 'Marker' parameters together
--     with the ARN parameter.
-- -   The 'MaxRecords' parameter can be a range from 10 to 50 results to
--     return in a request.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all resources that match any combination of the
-- specified keys and values. For example, if you have 'owner' and
-- 'environment' for tag keys, and 'admin' and 'test' for tag values, all
-- resources that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, resources are
-- returned regardless of whether they have tag keys or values associated
-- with them.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeTags.html AWS API Reference> for DescribeTags.
module Network.AWS.Redshift.DescribeTags
    (
    -- * Creating a Request
      describeTags
    , DescribeTags
    -- * Request Lenses
    , dtTagValues
    , dtResourceType
    , dtResourceName
    , dtTagKeys
    , dtMarker
    , dtMaxRecords

    -- * Destructuring the Response
    , describeTagsResponse
    , DescribeTagsResponse
    -- * Response Lenses
    , dtrsMarker
    , dtrsTaggedResources
    , dtrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the output from the 'DescribeTags' action.
--
-- /See:/ 'describeTags' smart constructor.
data DescribeTags = DescribeTags'
    { _dtTagValues    :: !(Maybe [Text])
    , _dtResourceType :: !(Maybe Text)
    , _dtResourceName :: !(Maybe Text)
    , _dtTagKeys      :: !(Maybe [Text])
    , _dtMarker       :: !(Maybe Text)
    , _dtMaxRecords   :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTagValues'
--
-- * 'dtResourceType'
--
-- * 'dtResourceName'
--
-- * 'dtTagKeys'
--
-- * 'dtMarker'
--
-- * 'dtMaxRecords'
describeTags
    :: DescribeTags
describeTags =
    DescribeTags'
    { _dtTagValues = Nothing
    , _dtResourceType = Nothing
    , _dtResourceName = Nothing
    , _dtTagKeys = Nothing
    , _dtMarker = Nothing
    , _dtMaxRecords = Nothing
    }

-- | A tag value or values for which you want to return all matching
-- resources that are associated with the specified value or values. For
-- example, suppose that you have resources tagged with values called
-- 'admin' and 'test'. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with all resources that have
-- either or both of these tag values associated with them.
dtTagValues :: Lens' DescribeTags [Text]
dtTagValues = lens _dtTagValues (\ s a -> s{_dtTagValues = a}) . _Default . _Coerce;

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
dtResourceType :: Lens' DescribeTags (Maybe Text)
dtResourceType = lens _dtResourceType (\ s a -> s{_dtResourceType = a});

-- | The Amazon Resource Name (ARN) for which you want to describe the tag or
-- tags. For example, 'arn:aws:redshift:us-east-1:123456789:cluster:t1'.
dtResourceName :: Lens' DescribeTags (Maybe Text)
dtResourceName = lens _dtResourceName (\ s a -> s{_dtResourceName = a});

-- | A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called 'owner' and
-- 'environment'. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with all resources that have either
-- or both of these tag keys associated with them.
dtTagKeys :: Lens' DescribeTags [Text]
dtTagKeys = lens _dtTagKeys (\ s a -> s{_dtTagKeys = a}) . _Default . _Coerce;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the 'marker' parameter and retrying the command. If the
-- 'marker' field is empty, all response records have been retrieved for
-- the request.
dtMarker :: Lens' DescribeTags (Maybe Text)
dtMarker = lens _dtMarker (\ s a -> s{_dtMarker = a});

-- | The maximum number or response records to return in each call. If the
-- number of remaining response records exceeds the specified 'MaxRecords'
-- value, a value is returned in a 'marker' field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned 'marker' value.
dtMaxRecords :: Lens' DescribeTags (Maybe Int)
dtMaxRecords = lens _dtMaxRecords (\ s a -> s{_dtMaxRecords = a});

instance AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
        request = postQuery redshift
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
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dtTagValues),
               "ResourceType" =: _dtResourceType,
               "ResourceName" =: _dtResourceName,
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dtTagKeys),
               "Marker" =: _dtMarker, "MaxRecords" =: _dtMaxRecords]

-- | Contains the output from the 'DescribeTags' action.
--
-- /See:/ 'describeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
    { _dtrsMarker          :: !(Maybe Text)
    , _dtrsTaggedResources :: !(Maybe [TaggedResource])
    , _dtrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsMarker'
--
-- * 'dtrsTaggedResources'
--
-- * 'dtrsStatus'
describeTagsResponse
    :: Int -- ^ 'dtrsStatus'
    -> DescribeTagsResponse
describeTagsResponse pStatus_ =
    DescribeTagsResponse'
    { _dtrsMarker = Nothing
    , _dtrsTaggedResources = Nothing
    , _dtrsStatus = pStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the 'Marker' parameter and retrying the command. If the
-- 'Marker' field is empty, all response records have been retrieved for
-- the request.
dtrsMarker :: Lens' DescribeTagsResponse (Maybe Text)
dtrsMarker = lens _dtrsMarker (\ s a -> s{_dtrsMarker = a});

-- | A list of tags with their associated resources.
dtrsTaggedResources :: Lens' DescribeTagsResponse [TaggedResource]
dtrsTaggedResources = lens _dtrsTaggedResources (\ s a -> s{_dtrsTaggedResources = a}) . _Default . _Coerce;

-- | The response status code.
dtrsStatus :: Lens' DescribeTagsResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
