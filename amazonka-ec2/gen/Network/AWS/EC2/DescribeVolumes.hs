{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.EC2.DescribeVolumes
    (
    -- * Request
      DescribeVolumes
    -- ** Request constructor
    , describeVolumes
    -- ** Request lenses
    , dvDryRun
    , dvFilters
    , dvMaxResults
    , dvNextToken
    , dvVolumeIds

    -- * Response
    , DescribeVolumesResult
    -- ** Response constructor
    , describeVolumesResponse
    -- ** Response lenses
    , dvrNextToken
    , dvrVolumes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeVolumes = DescribeVolumes
    { _dvDryRun     :: Maybe Bool
    , _dvFilters    :: [Filter]
    , _dvMaxResults :: Maybe Int
    , _dvNextToken  :: Maybe Text
    , _dvVolumeIds  :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeVolumes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvFilters' @::@ ['Filter']
--
-- * 'dvMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dvNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dvVolumeIds' @::@ ['Text']
--
describeVolumes :: DescribeVolumes
describeVolumes = DescribeVolumes
    { _dvDryRun     = Nothing
    , _dvVolumeIds  = mempty
    , _dvFilters    = mempty
    , _dvNextToken  = Nothing
    , _dvMaxResults = Nothing
    }

dvDryRun :: Lens' DescribeVolumes (Maybe Bool)
dvDryRun = lens _dvDryRun (\s a -> s { _dvDryRun = a })

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
dvFilters :: Lens' DescribeVolumes [Filter]
dvFilters = lens _dvFilters (\s a -> s { _dvFilters = a })

-- | The maximum number of volume results returned by DescribeVolumes in
-- paginated output. When this parameter is used, DescribeVolumes only
-- returns MaxResults results in a single page along with a NextToken
-- response element. The remaining results of the initial request can be
-- seen by sending another DescribeVolumes request with the returned
-- NextToken value. This value can be between 5 and 1000; if MaxResults is
-- given a value larger than 1000, only 1000 results are returned. If this
-- parameter is not used, then DescribeVolumes returns all results.
dvMaxResults :: Lens' DescribeVolumes (Maybe Int)
dvMaxResults = lens _dvMaxResults (\s a -> s { _dvMaxResults = a })

-- | The NextToken value returned from a previous paginated DescribeVolumes
-- request where MaxResults was used and the results exceeded the value of
-- that parameter. Pagination continues from the end of the previous results
-- that returned the NextToken value. This value is null when there are no
-- more results to return.
dvNextToken :: Lens' DescribeVolumes (Maybe Text)
dvNextToken = lens _dvNextToken (\s a -> s { _dvNextToken = a })

-- | One or more volume IDs.
dvVolumeIds :: Lens' DescribeVolumes [Text]
dvVolumeIds = lens _dvVolumeIds (\s a -> s { _dvVolumeIds = a })

instance ToQuery DescribeVolumes

instance ToPath DescribeVolumes where
    toPath = const "/"

data DescribeVolumesResult = DescribeVolumesResult
    { _dvrNextToken :: Maybe Text
    , _dvrVolumes   :: [Volume]
    } deriving (Eq, Show, Generic)

-- | 'DescribeVolumesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dvrVolumes' @::@ ['Volume']
--
describeVolumesResponse :: DescribeVolumesResult
describeVolumesResponse = DescribeVolumesResult
    { _dvrVolumes   = mempty
    , _dvrNextToken = Nothing
    }

-- | The NextToken value to include in a future DescribeVolumes request. When
-- the results of a DescribeVolumes request exceed MaxResults, this value
-- can be used to retrieve the next page of results. This value is null when
-- there are no more results to return.
dvrNextToken :: Lens' DescribeVolumesResult (Maybe Text)
dvrNextToken = lens _dvrNextToken (\s a -> s { _dvrNextToken = a })

dvrVolumes :: Lens' DescribeVolumesResult [Volume]
dvrVolumes = lens _dvrVolumes (\s a -> s { _dvrVolumes = a })

instance FromXML DescribeVolumesResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeVolumesResult"

instance AWSRequest DescribeVolumes where
    type Sv DescribeVolumes = EC2
    type Rs DescribeVolumes = DescribeVolumesResult

    request  = post "DescribeVolumes"
    response = xmlResponse $ \h x -> DescribeVolumesResult
        <$> x %| "nextToken"
        <*> x %| "volumeSet"
