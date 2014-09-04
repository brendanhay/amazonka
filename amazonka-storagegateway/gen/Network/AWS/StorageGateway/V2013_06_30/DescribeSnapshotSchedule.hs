{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeSnapshotSchedule
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
module Network.AWS.StorageGateway.V2013_06_30.DescribeSnapshotSchedule
    (
    -- * Request
      DescribeSnapshotSchedule
    -- ** Request constructor
    , describeSnapshotSchedule
    -- ** Request lenses
    , dssjVolumeARN

    -- * Response
    , DescribeSnapshotScheduleResponse
    -- ** Response lenses
    , dsspDescription
    , dsspTimezone
    , dsspStartAt
    , dsspRecurrenceInHours
    , dsspVolumeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeSnapshotSchedule' request.
describeSnapshotSchedule :: Text -- ^ 'dssjVolumeARN'
                         -> DescribeSnapshotSchedule
describeSnapshotSchedule p1 = DescribeSnapshotSchedule
    { _dssjVolumeARN = p1
    }
{-# INLINE describeSnapshotSchedule #-}

data DescribeSnapshotSchedule = DescribeSnapshotSchedule
    { _dssjVolumeARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
      -- operation to return a list of gateway volumes.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes operation
-- to return a list of gateway volumes.
dssjVolumeARN :: Lens' DescribeSnapshotSchedule (Text)
dssjVolumeARN f x =
    f (_dssjVolumeARN x)
        <&> \y -> x { _dssjVolumeARN = y }
{-# INLINE dssjVolumeARN #-}

instance ToPath DescribeSnapshotSchedule

instance ToQuery DescribeSnapshotSchedule

instance ToHeaders DescribeSnapshotSchedule

instance ToJSON DescribeSnapshotSchedule

data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse
    { _dsspDescription :: Maybe Text
    , _dsspTimezone :: Maybe Text
    , _dsspStartAt :: Maybe Integer
    , _dsspRecurrenceInHours :: Maybe Integer
    , _dsspVolumeARN :: Maybe Text
    } deriving (Show, Generic)

dsspDescription :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dsspDescription f x =
    f (_dsspDescription x)
        <&> \y -> x { _dsspDescription = y }
{-# INLINE dsspDescription #-}

dsspTimezone :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dsspTimezone f x =
    f (_dsspTimezone x)
        <&> \y -> x { _dsspTimezone = y }
{-# INLINE dsspTimezone #-}

dsspStartAt :: Lens' DescribeSnapshotScheduleResponse (Maybe Integer)
dsspStartAt f x =
    f (_dsspStartAt x)
        <&> \y -> x { _dsspStartAt = y }
{-# INLINE dsspStartAt #-}

dsspRecurrenceInHours :: Lens' DescribeSnapshotScheduleResponse (Maybe Integer)
dsspRecurrenceInHours f x =
    f (_dsspRecurrenceInHours x)
        <&> \y -> x { _dsspRecurrenceInHours = y }
{-# INLINE dsspRecurrenceInHours #-}

dsspVolumeARN :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dsspVolumeARN f x =
    f (_dsspVolumeARN x)
        <&> \y -> x { _dsspVolumeARN = y }
{-# INLINE dsspVolumeARN #-}

instance FromJSON DescribeSnapshotScheduleResponse

instance AWSRequest DescribeSnapshotSchedule where
    type Sv DescribeSnapshotSchedule = StorageGateway
    type Rs DescribeSnapshotSchedule = DescribeSnapshotScheduleResponse

    request = get
    response _ = jsonResponse
