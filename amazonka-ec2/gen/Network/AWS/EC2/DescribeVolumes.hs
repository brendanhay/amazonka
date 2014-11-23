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

-- Module      : Network.AWS.EC2.DescribeVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified Amazon EBS volumes. If you are describing a long
-- list of volumes, you can paginate the output to make the list more
-- manageable. The MaxResults parameter sets the maximum number of results
-- returned in a single page. If the list of results exceeds your MaxResults
-- value, then that number of results is returned along with a NextToken value
-- that can be passed to a subsequent DescribeVolumes request to retrieve the
-- remaining results. For more information about Amazon EBS volumes, see
-- Amazon EBS Volumes in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html>
module Network.AWS.EC2.DescribeVolumes
    (
    -- * Request
      DescribeVolumes
    -- ** Request constructor
    , describeVolumes
    -- ** Request lenses
    , dv2DryRun
    , dv2Filters
    , dv2MaxResults
    , dv2NextToken
    , dv2VolumeIds

    -- * Response
    , DescribeVolumesResponse
    -- ** Response constructor
    , describeVolumesResponse
    -- ** Response lenses
    , dvrNextToken
    , dvrVolumes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVolumes = DescribeVolumes
    { _dv2DryRun     :: Maybe Bool
    , _dv2Filters    :: List "Filter" Filter
    , _dv2MaxResults :: Maybe Int
    , _dv2NextToken  :: Maybe Text
    , _dv2VolumeIds  :: List "VolumeId" Text
    } deriving (Eq, Show)

-- | 'DescribeVolumes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dv2Filters' @::@ ['Filter']
--
-- * 'dv2MaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dv2NextToken' @::@ 'Maybe' 'Text'
--
-- * 'dv2VolumeIds' @::@ ['Text']
--
describeVolumes :: DescribeVolumes
describeVolumes = DescribeVolumes
    { _dv2DryRun     = Nothing
    , _dv2VolumeIds  = mempty
    , _dv2Filters    = mempty
    , _dv2NextToken  = Nothing
    , _dv2MaxResults = Nothing
    }

dv2DryRun :: Lens' DescribeVolumes (Maybe Bool)
dv2DryRun = lens _dv2DryRun (\s a -> s { _dv2DryRun = a })

-- | One or more filters. attachment.attach-time - The time stamp when the
-- attachment initiated. attachment.delete-on-termination - Whether the
-- volume is deleted on instance termination. attachment.device - The device
-- name that is exposed to the instance (for example, /dev/sda1).
-- attachment.instance-id - The ID of the instance the volume is attached
-- to. attachment.status - The attachment state (attaching | attached |
-- detaching | detached). availability-zone - The Availability Zone in which
-- the volume was created. create-time - The time stamp when the volume was
-- created. encrypted - The encryption status of the volume. size - The size
-- of the volume, in GiB. snapshot-id - The snapshot from which the volume
-- was created. status - The status of the volume (creating | available |
-- in-use | deleting | deleted | error). tag:key=value - The key/value
-- combination of a tag assigned to the resource. tag-key - The key of a tag
-- assigned to the resource. This filter is independent of the tag-value
-- filter. For example, if you use both the filter "tag-key=Purpose" and the
-- filter "tag-value=X", you get any resources assigned both the tag key
-- Purpose (regardless of what the tag's value is), and the tag value X
-- (regardless of what the tag's key is). If you want to list only resources
-- where Purpose is X, see the tag:key=value filter. tag-value - The value
-- of a tag assigned to the resource. This filter is independent of the
-- tag-key filter. volume-id - The volume ID. volume-type - The Amazon EBS
-- volume type. This can be gp2 for General Purpose (SSD) volumes, io1 for
-- Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes.
dv2Filters :: Lens' DescribeVolumes [Filter]
dv2Filters = lens _dv2Filters (\s a -> s { _dv2Filters = a }) . _List

-- | The maximum number of volume results returned by DescribeVolumes in
-- paginated output. When this parameter is used, DescribeVolumes only
-- returns MaxResults results in a single page along with a NextToken
-- response element. The remaining results of the initial request can be
-- seen by sending another DescribeVolumes request with the returned
-- NextToken value. This value can be between 5 and 1000; if MaxResults is
-- given a value larger than 1000, only 1000 results are returned. If this
-- parameter is not used, then DescribeVolumes returns all results.
dv2MaxResults :: Lens' DescribeVolumes (Maybe Int)
dv2MaxResults = lens _dv2MaxResults (\s a -> s { _dv2MaxResults = a })

-- | The NextToken value returned from a previous paginated DescribeVolumes
-- request where MaxResults was used and the results exceeded the value of
-- that parameter. Pagination continues from the end of the previous results
-- that returned the NextToken value. This value is null when there are no
-- more results to return.
dv2NextToken :: Lens' DescribeVolumes (Maybe Text)
dv2NextToken = lens _dv2NextToken (\s a -> s { _dv2NextToken = a })

-- | One or more volume IDs.
dv2VolumeIds :: Lens' DescribeVolumes [Text]
dv2VolumeIds = lens _dv2VolumeIds (\s a -> s { _dv2VolumeIds = a }) . _List

data DescribeVolumesResponse = DescribeVolumesResponse
    { _dvrNextToken :: Maybe Text
    , _dvrVolumes   :: List "item" Volume
    } deriving (Eq, Show)

-- | 'DescribeVolumesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dvrVolumes' @::@ ['Volume']
--
describeVolumesResponse :: DescribeVolumesResponse
describeVolumesResponse = DescribeVolumesResponse
    { _dvrVolumes   = mempty
    , _dvrNextToken = Nothing
    }

-- | The NextToken value to include in a future DescribeVolumes request. When
-- the results of a DescribeVolumes request exceed MaxResults, this value
-- can be used to retrieve the next page of results. This value is null when
-- there are no more results to return.
dvrNextToken :: Lens' DescribeVolumesResponse (Maybe Text)
dvrNextToken = lens _dvrNextToken (\s a -> s { _dvrNextToken = a })

dvrVolumes :: Lens' DescribeVolumesResponse [Volume]
dvrVolumes = lens _dvrVolumes (\s a -> s { _dvrVolumes = a }) . _List

instance ToPath DescribeVolumes where
    toPath = const "/"

instance ToQuery DescribeVolumes where
    toQuery DescribeVolumes{..} = mconcat
        [ "dryRun"     =? _dv2DryRun
        , "Filter"     =? _dv2Filters
        , "maxResults" =? _dv2MaxResults
        , "nextToken"  =? _dv2NextToken
        , "VolumeId"   =? _dv2VolumeIds
        ]

instance ToHeaders DescribeVolumes

instance AWSRequest DescribeVolumes where
    type Sv DescribeVolumes = EC2
    type Rs DescribeVolumes = DescribeVolumesResponse

    request  = post "DescribeVolumes"
    response = xmlResponse

instance FromXML DescribeVolumesResponse where
    parseXML x = DescribeVolumesResponse
        <$> x .@? "nextToken"
        <*> x .@? "volumeSet"
