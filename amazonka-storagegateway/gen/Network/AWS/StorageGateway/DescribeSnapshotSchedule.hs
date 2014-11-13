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
-- snapshots are automatically initiated on the volume.
module Network.AWS.StorageGateway.DescribeSnapshotSchedule
    (
    -- * Request
      DescribeSnapshotSchedule
    -- ** Request constructor
    , describeSnapshotSchedule
    -- ** Request lenses
    , dssVolumeARN

    -- * Response
    , DescribeSnapshotScheduleResponse
    -- ** Response constructor
    , describeSnapshotScheduleResponse
    -- ** Response lenses
    , dssrDescription
    , dssrRecurrenceInHours
    , dssrStartAt
    , dssrTimezone
    , dssrVolumeARN
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

newtype DescribeSnapshotSchedule = DescribeSnapshotSchedule
    { _dssVolumeARN :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DescribeSnapshotSchedule' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssVolumeARN' @::@ 'Text'
--
describeSnapshotSchedule :: Text -- ^ 'dssVolumeARN'
                         -> DescribeSnapshotSchedule
describeSnapshotSchedule p1 = DescribeSnapshotSchedule
    { _dssVolumeARN = p1
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
dssVolumeARN :: Lens' DescribeSnapshotSchedule Text
dssVolumeARN = lens _dssVolumeARN (\s a -> s { _dssVolumeARN = a })

instance ToPath DescribeSnapshotSchedule where
    toPath = const "/"

instance ToQuery DescribeSnapshotSchedule where
    toQuery = const mempty

instance ToHeaders DescribeSnapshotSchedule

instance ToBody DescribeSnapshotSchedule where
    toBody = toBody . encode . _dssVolumeARN

data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse
    { _dssrDescription       :: Maybe Text
    , _dssrRecurrenceInHours :: Maybe Natural
    , _dssrStartAt           :: Maybe Natural
    , _dssrTimezone          :: Maybe Text
    , _dssrVolumeARN         :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeSnapshotScheduleResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssrDescription' @::@ 'Maybe' 'Text'
--
-- * 'dssrRecurrenceInHours' @::@ 'Maybe' 'Natural'
--
-- * 'dssrStartAt' @::@ 'Maybe' 'Natural'
--
-- * 'dssrTimezone' @::@ 'Maybe' 'Text'
--
-- * 'dssrVolumeARN' @::@ 'Maybe' 'Text'
--
describeSnapshotScheduleResponse :: DescribeSnapshotScheduleResponse
describeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse
    { _dssrVolumeARN         = Nothing
    , _dssrStartAt           = Nothing
    , _dssrRecurrenceInHours = Nothing
    , _dssrDescription       = Nothing
    , _dssrTimezone          = Nothing
    }

dssrDescription :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrDescription = lens _dssrDescription (\s a -> s { _dssrDescription = a })

dssrRecurrenceInHours :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrRecurrenceInHours =
    lens _dssrRecurrenceInHours (\s a -> s { _dssrRecurrenceInHours = a })

dssrStartAt :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrStartAt = lens _dssrStartAt (\s a -> s { _dssrStartAt = a })

dssrTimezone :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrTimezone = lens _dssrTimezone (\s a -> s { _dssrTimezone = a })

dssrVolumeARN :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrVolumeARN = lens _dssrVolumeARN (\s a -> s { _dssrVolumeARN = a })

-- FromJSON

instance AWSRequest DescribeSnapshotSchedule where
    type Sv DescribeSnapshotSchedule = StorageGateway
    type Rs DescribeSnapshotSchedule = DescribeSnapshotScheduleResponse

    request  = post'
    response = jsonResponse $ \h o -> DescribeSnapshotScheduleResponse
        <$> o .: "Description"
        <*> o .: "RecurrenceInHours"
        <*> o .: "StartAt"
        <*> o .: "Timezone"
        <*> o .: "VolumeARN"
