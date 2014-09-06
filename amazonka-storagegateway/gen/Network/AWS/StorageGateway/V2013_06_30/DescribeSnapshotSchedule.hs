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
    , mkDescribeSnapshotSchedule
    -- ** Request lenses
    , dss1VolumeARN

    -- * Response
    , DescribeSnapshotScheduleResponse
    -- ** Response lenses
    , dssrsrsVolumeARN
    , dssrsrsStartAt
    , dssrsrsRecurrenceInHours
    , dssrsrsDescription
    , dssrsrsTimezone
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | A JSON object containing the DescribeSnapshotScheduleInput$VolumeARN of the
-- volume.
newtype DescribeSnapshotSchedule = DescribeSnapshotSchedule
    { _dss1VolumeARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSnapshotSchedule' request.
mkDescribeSnapshotSchedule :: Text -- ^ 'dss1VolumeARN'
                           -> DescribeSnapshotSchedule
mkDescribeSnapshotSchedule p1 = DescribeSnapshotSchedule
    { _dss1VolumeARN = p1
    }
{-# INLINE mkDescribeSnapshotSchedule #-}

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes operation
-- to return a list of gateway volumes.
dss1VolumeARN :: Lens' DescribeSnapshotSchedule Text
dss1VolumeARN = lens _dss1VolumeARN (\s a -> s { _dss1VolumeARN = a })
{-# INLINE dss1VolumeARN #-}

instance ToPath DescribeSnapshotSchedule

instance ToQuery DescribeSnapshotSchedule

instance ToHeaders DescribeSnapshotSchedule

instance ToJSON DescribeSnapshotSchedule

data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse
    { _dssrsrsVolumeARN :: Maybe Text
    , _dssrsrsStartAt :: Maybe Integer
    , _dssrsrsRecurrenceInHours :: Maybe Integer
    , _dssrsrsDescription :: Maybe Text
    , _dssrsrsTimezone :: Maybe Text
    } deriving (Show, Generic)

dssrsrsVolumeARN :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsrsVolumeARN =
    lens _dssrsrsVolumeARN (\s a -> s { _dssrsrsVolumeARN = a })
{-# INLINE dssrsrsVolumeARN #-}

dssrsrsStartAt :: Lens' DescribeSnapshotScheduleResponse (Maybe Integer)
dssrsrsStartAt = lens _dssrsrsStartAt (\s a -> s { _dssrsrsStartAt = a })
{-# INLINE dssrsrsStartAt #-}

dssrsrsRecurrenceInHours :: Lens' DescribeSnapshotScheduleResponse (Maybe Integer)
dssrsrsRecurrenceInHours =
    lens _dssrsrsRecurrenceInHours
         (\s a -> s { _dssrsrsRecurrenceInHours = a })
{-# INLINE dssrsrsRecurrenceInHours #-}

dssrsrsDescription :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsrsDescription =
    lens _dssrsrsDescription (\s a -> s { _dssrsrsDescription = a })
{-# INLINE dssrsrsDescription #-}

dssrsrsTimezone :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsrsTimezone = lens _dssrsrsTimezone (\s a -> s { _dssrsrsTimezone = a })
{-# INLINE dssrsrsTimezone #-}

instance FromJSON DescribeSnapshotScheduleResponse

instance AWSRequest DescribeSnapshotSchedule where
    type Sv DescribeSnapshotSchedule = StorageGateway
    type Rs DescribeSnapshotSchedule = DescribeSnapshotScheduleResponse

    request = get
    response _ = jsonResponse
