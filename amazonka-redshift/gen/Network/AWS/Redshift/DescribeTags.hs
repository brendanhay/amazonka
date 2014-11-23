{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of tags. You can return tags from a specific resource by
-- specifying an ARN, or you can return all tags for a given type of resource,
-- such as clusters, snapshots, and so on. The following are limitations for
-- DescribeTags: You cannot specify an ARN and a resource-type value together
-- in the same request. You cannot use the MaxRecords and Marker parameters
-- together with the ARN parameter. The MaxRecords parameter can be a range
-- from 10 to 50 results to return in a request. If you specify both tag keys
-- and tag values in the same request, Amazon Redshift returns all resources
-- that match any combination of the specified keys and values. For example,
-- if you have owner and environment for tag keys, and admin and test for tag
-- values, all resources that have any combination of those values are
-- returned. If both tag keys and values are omitted from the request,
-- resources are returned regardless of whether they have tag keys or values
-- associated with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeTags.html>
module Network.AWS.Redshift.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , describeTags
    -- ** Request lenses
    , dtMarker
    , dtMaxRecords
    , dtResourceName
    , dtResourceType
    , dtTagKeys
    , dtTagValues

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , describeTagsResponse
    -- ** Response lenses
    , dtrMarker
    , dtrTaggedResources
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeTags = DescribeTags
    { _dtMarker       :: Maybe Text
    , _dtMaxRecords   :: Maybe Int
    , _dtResourceName :: Maybe Text
    , _dtResourceType :: Maybe Text
    , _dtTagKeys      :: List "TagKey" Text
    , _dtTagValues    :: List "TagValue" Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtMarker' @::@ 'Maybe' 'Text'
--
-- * 'dtMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dtResourceName' @::@ 'Maybe' 'Text'
--
-- * 'dtResourceType' @::@ 'Maybe' 'Text'
--
-- * 'dtTagKeys' @::@ ['Text']
--
-- * 'dtTagValues' @::@ ['Text']
--
describeTags :: DescribeTags
describeTags = DescribeTags
    { _dtResourceName = Nothing
    , _dtResourceType = Nothing
    , _dtMaxRecords   = Nothing
    , _dtMarker       = Nothing
    , _dtTagKeys      = mempty
    , _dtTagValues    = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the marker parameter and retrying the command. If the
-- marker field is empty, all response records have been retrieved for the
-- request.
dtMarker :: Lens' DescribeTags (Maybe Text)
dtMarker = lens _dtMarker (\s a -> s { _dtMarker = a })

-- | The maximum number or response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
dtMaxRecords :: Lens' DescribeTags (Maybe Int)
dtMaxRecords = lens _dtMaxRecords (\s a -> s { _dtMaxRecords = a })

-- | The Amazon Resource Name (ARN) for which you want to describe the tag or
-- tags. For example, arn:aws:redshift:us-east-1:123456789:cluster:t1.
dtResourceName :: Lens' DescribeTags (Maybe Text)
dtResourceName = lens _dtResourceName (\s a -> s { _dtResourceName = a })

-- | The type of resource with which you want to view tags. Valid resource
-- types are: Cluster CIDR/IP EC2 security group Snapshot Cluster security
-- group Subnet group HSM connection HSM certificate Parameter group For
-- more information about Amazon Redshift resource types and constructing
-- ARNs, go to Constructing an Amazon Redshift Amazon Resource Name (ARN) in
-- the Amazon Redshift Cluster Management Guide.
dtResourceType :: Lens' DescribeTags (Maybe Text)
dtResourceType = lens _dtResourceType (\s a -> s { _dtResourceType = a })

-- | A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called owner and environment. If
-- you specify both of these tag keys in the request, Amazon Redshift
-- returns a response with all resources that have either or both of these
-- tag keys associated with them.
dtTagKeys :: Lens' DescribeTags [Text]
dtTagKeys = lens _dtTagKeys (\s a -> s { _dtTagKeys = a }) . _List

-- | A tag value or values for which you want to return all matching resources
-- that are associated with the specified value or values. For example,
-- suppose that you have resources tagged with values called admin and test.
-- If you specify both of these tag values in the request, Amazon Redshift
-- returns a response with all resources that have either or both of these
-- tag values associated with them.
dtTagValues :: Lens' DescribeTags [Text]
dtTagValues = lens _dtTagValues (\s a -> s { _dtTagValues = a }) . _List

data DescribeTagsResponse = DescribeTagsResponse
    { _dtrMarker          :: Maybe Text
    , _dtrTaggedResources :: List "TaggedResource" TaggedResource
    } deriving (Eq, Show)

-- | 'DescribeTagsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrMarker' @::@ 'Maybe' 'Text'
--
-- * 'dtrTaggedResources' @::@ ['TaggedResource']
--
describeTagsResponse :: DescribeTagsResponse
describeTagsResponse = DescribeTagsResponse
    { _dtrTaggedResources = mempty
    , _dtrMarker          = Nothing
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
dtrMarker :: Lens' DescribeTagsResponse (Maybe Text)
dtrMarker = lens _dtrMarker (\s a -> s { _dtrMarker = a })

-- | A list of tags with their associated resources.
dtrTaggedResources :: Lens' DescribeTagsResponse [TaggedResource]
dtrTaggedResources =
    lens _dtrTaggedResources (\s a -> s { _dtrTaggedResources = a })
        . _List

instance ToPath DescribeTags where
    toPath = const "/"

instance ToQuery DescribeTags where
    toQuery DescribeTags{..} = mconcat
        [ "Marker"       =? _dtMarker
        , "MaxRecords"   =? _dtMaxRecords
        , "ResourceName" =? _dtResourceName
        , "ResourceType" =? _dtResourceType
        , "TagKeys"      =? _dtTagKeys
        , "TagValues"    =? _dtTagValues
        ]

instance ToHeaders DescribeTags

instance AWSRequest DescribeTags where
    type Sv DescribeTags = Redshift
    type Rs DescribeTags = DescribeTagsResponse

    request  = post "DescribeTags"
    response = xmlResponse

instance FromXML DescribeTagsResponse where
    parseXML = withElement "DescribeTagsResult" $ \x -> DescribeTagsResponse
        <$> x .@? "Marker"
        <*> x .@? "TaggedResources"
