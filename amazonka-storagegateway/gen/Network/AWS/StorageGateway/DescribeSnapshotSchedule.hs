{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeSnapshotSchedule
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation describes the snapshot schedule for the specified gateway
-- volume. The snapshot schedule information includes intervals at which
-- snapshots are automatically initiated on the volume. Example Request The
-- following example shows a request that retrieves the snapshot schedule for
-- a volume. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- x-amz-Date: 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeSnapshotSchedule { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 211 {
-- "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "StartAt": 6, "RecurrenceInHours": 24, "Description":
-- "sgw-AABB1122:vol-AABB1122:Schedule", "Timezone": "GMT+7:00" }.
module Network.AWS.StorageGateway.DescribeSnapshotSchedule
    (
    -- * Request
      DescribeSnapshotSchedule
    -- ** Request constructor
    , mkDescribeSnapshotSchedule
    -- ** Request lenses
    , dss1VolumeARN

    -- * Response
    , DescribeSnapshotScheduleResponse
    -- ** Response constructor
    , mkDescribeSnapshotScheduleResponse
    -- ** Response lenses
    , dssrrVolumeARN
    , dssrrStartAt
    , dssrrRecurrenceInHours
    , dssrrDescription
    , dssrrTimezone
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing the DescribeSnapshotScheduleInput$VolumeARN of the
-- volume.
newtype DescribeSnapshotSchedule = DescribeSnapshotSchedule
    { _dss1VolumeARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSnapshotSchedule' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Text@
--
mkDescribeSnapshotSchedule :: Text -- ^ 'dss1VolumeARN'
                           -> DescribeSnapshotSchedule
mkDescribeSnapshotSchedule p1 = DescribeSnapshotSchedule
    { _dss1VolumeARN = p1
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes operation
-- to return a list of gateway volumes.
dss1VolumeARN :: Lens' DescribeSnapshotSchedule Text
dss1VolumeARN = lens _dss1VolumeARN (\s a -> s { _dss1VolumeARN = a })

instance ToPath DescribeSnapshotSchedule

instance ToQuery DescribeSnapshotSchedule

instance ToHeaders DescribeSnapshotSchedule

instance ToJSON DescribeSnapshotSchedule

data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse
    { _dssrrVolumeARN :: Maybe Text
    , _dssrrStartAt :: Maybe Integer
    , _dssrrRecurrenceInHours :: Maybe Integer
    , _dssrrDescription :: Maybe Text
    , _dssrrTimezone :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSnapshotScheduleResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Maybe Text@
--
-- * @StartAt ::@ @Maybe Integer@
--
-- * @RecurrenceInHours ::@ @Maybe Integer@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Timezone ::@ @Maybe Text@
--
mkDescribeSnapshotScheduleResponse :: DescribeSnapshotScheduleResponse
mkDescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse
    { _dssrrVolumeARN = Nothing
    , _dssrrStartAt = Nothing
    , _dssrrRecurrenceInHours = Nothing
    , _dssrrDescription = Nothing
    , _dssrrTimezone = Nothing
    }

dssrrVolumeARN :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrrVolumeARN = lens _dssrrVolumeARN (\s a -> s { _dssrrVolumeARN = a })

dssrrStartAt :: Lens' DescribeSnapshotScheduleResponse (Maybe Integer)
dssrrStartAt = lens _dssrrStartAt (\s a -> s { _dssrrStartAt = a })

dssrrRecurrenceInHours :: Lens' DescribeSnapshotScheduleResponse (Maybe Integer)
dssrrRecurrenceInHours =
    lens _dssrrRecurrenceInHours (\s a -> s { _dssrrRecurrenceInHours = a })

dssrrDescription :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrrDescription =
    lens _dssrrDescription (\s a -> s { _dssrrDescription = a })

dssrrTimezone :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrrTimezone = lens _dssrrTimezone (\s a -> s { _dssrrTimezone = a })

instance FromJSON DescribeSnapshotScheduleResponse

instance AWSRequest DescribeSnapshotSchedule where
    type Sv DescribeSnapshotSchedule = StorageGateway
    type Rs DescribeSnapshotSchedule = DescribeSnapshotScheduleResponse

    request = get
    response _ = jsonResponse
