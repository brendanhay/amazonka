{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.UpdateSnapshotSchedule
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates a snapshot schedule configured for a gateway volume.
-- The default snapshot schedule for volume is once every 24 hours, starting
-- at the creation time of the volume. You can use this API to change the
-- shapshot schedule configured for the volume. In the request you must
-- identify the gateway volume whose snapshot schedule you want to update, and
-- the schedule information, including when you want the snapshot to begin on
-- a day and the frequency (in hours) of snapshots. Example Request The
-- following example shows a request that updates a snapshot schedule. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateSnapshotSchedule { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "StartAt": 0, "RecurrenceInHours": 1, "Description": "hourly snapshot" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 99 {
-- "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- }.
module Network.AWS.StorageGateway.V2013_06_30.UpdateSnapshotSchedule
    (
    -- * Request
      UpdateSnapshotSchedule
    -- ** Request constructor
    , updateSnapshotSchedule
    -- ** Request lenses
    , ussiStartAt
    , ussiRecurrenceInHours
    , ussiVolumeARN
    , ussiDescription

    -- * Response
    , UpdateSnapshotScheduleResponse
    -- ** Response lenses
    , ussoVolumeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UpdateSnapshotSchedule' request.
updateSnapshotSchedule :: Integer -- ^ 'ussiStartAt'
                       -> Integer -- ^ 'ussiRecurrenceInHours'
                       -> Text -- ^ 'ussiVolumeARN'
                       -> UpdateSnapshotSchedule
updateSnapshotSchedule p1 p2 p3 = UpdateSnapshotSchedule
    { _ussiStartAt = p1
    , _ussiRecurrenceInHours = p2
    , _ussiVolumeARN = p3
    , _ussiDescription = Nothing
    }

data UpdateSnapshotSchedule = UpdateSnapshotSchedule
    { _ussiStartAt :: Integer
      -- ^ The hour of the day at which the snapshot schedule begins
      -- represented as hh, where hh is the hour (0 to 23). The hour of
      -- the day is in the time zone of the gateway.
    , _ussiRecurrenceInHours :: Integer
      -- ^ Frequency of snapshots. Specify the number of hours between
      -- snapshots.
    , _ussiVolumeARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
      -- operation to return a list of gateway volumes.
    , _ussiDescription :: Maybe Text
      -- ^ Optional description of the snapshot that overwrites the existing
      -- description.
    } deriving (Show, Generic)

-- | The hour of the day at which the snapshot schedule begins represented as
-- hh, where hh is the hour (0 to 23). The hour of the day is in the time zone
-- of the gateway.
ussiStartAt
    :: Functor f
    => (Integer
    -> f (Integer))
    -> UpdateSnapshotSchedule
    -> f UpdateSnapshotSchedule
ussiStartAt f x =
    (\y -> x { _ussiStartAt = y })
       <$> f (_ussiStartAt x)
{-# INLINE ussiStartAt #-}

-- | Frequency of snapshots. Specify the number of hours between snapshots.
ussiRecurrenceInHours
    :: Functor f
    => (Integer
    -> f (Integer))
    -> UpdateSnapshotSchedule
    -> f UpdateSnapshotSchedule
ussiRecurrenceInHours f x =
    (\y -> x { _ussiRecurrenceInHours = y })
       <$> f (_ussiRecurrenceInHours x)
{-# INLINE ussiRecurrenceInHours #-}

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes operation
-- to return a list of gateway volumes.
ussiVolumeARN
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateSnapshotSchedule
    -> f UpdateSnapshotSchedule
ussiVolumeARN f x =
    (\y -> x { _ussiVolumeARN = y })
       <$> f (_ussiVolumeARN x)
{-# INLINE ussiVolumeARN #-}

-- | Optional description of the snapshot that overwrites the existing
-- description.
ussiDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateSnapshotSchedule
    -> f UpdateSnapshotSchedule
ussiDescription f x =
    (\y -> x { _ussiDescription = y })
       <$> f (_ussiDescription x)
{-# INLINE ussiDescription #-}

instance ToPath UpdateSnapshotSchedule

instance ToQuery UpdateSnapshotSchedule

instance ToHeaders UpdateSnapshotSchedule

instance ToJSON UpdateSnapshotSchedule

data UpdateSnapshotScheduleResponse = UpdateSnapshotScheduleResponse
    { _ussoVolumeARN :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

-- | 
ussoVolumeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateSnapshotScheduleResponse
    -> f UpdateSnapshotScheduleResponse
ussoVolumeARN f x =
    (\y -> x { _ussoVolumeARN = y })
       <$> f (_ussoVolumeARN x)
{-# INLINE ussoVolumeARN #-}

instance FromJSON UpdateSnapshotScheduleResponse

instance AWSRequest UpdateSnapshotSchedule where
    type Sv UpdateSnapshotSchedule = StorageGateway
    type Rs UpdateSnapshotSchedule = UpdateSnapshotScheduleResponse

    request = get
    response _ = jsonResponse
