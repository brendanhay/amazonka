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

-- Module      : Network.AWS.StorageGateway.UpdateSnapshotSchedule
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
-- snapshot schedule configured for the volume. In the request you must
-- identify the gateway volume whose snapshot schedule you want to update, and
-- the schedule information, including when you want the snapshot to begin on
-- a day and the frequency (in hours) of snapshots.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateSnapshotSchedule.html>
module Network.AWS.StorageGateway.UpdateSnapshotSchedule
    (
    -- * Request
      UpdateSnapshotSchedule
    -- ** Request constructor
    , updateSnapshotSchedule
    -- ** Request lenses
    , ussDescription
    , ussRecurrenceInHours
    , ussStartAt
    , ussVolumeARN

    -- * Response
    , UpdateSnapshotScheduleResponse
    -- ** Response constructor
    , updateSnapshotScheduleResponse
    -- ** Response lenses
    , ussrVolumeARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data UpdateSnapshotSchedule = UpdateSnapshotSchedule
    { _ussDescription       :: Maybe Text
    , _ussRecurrenceInHours :: Nat
    , _ussStartAt           :: Nat
    , _ussVolumeARN         :: Text
    } deriving (Eq, Ord, Show)

-- | 'UpdateSnapshotSchedule' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ussDescription' @::@ 'Maybe' 'Text'
--
-- * 'ussRecurrenceInHours' @::@ 'Natural'
--
-- * 'ussStartAt' @::@ 'Natural'
--
-- * 'ussVolumeARN' @::@ 'Text'
--
updateSnapshotSchedule :: Text -- ^ 'ussVolumeARN'
                       -> Natural -- ^ 'ussStartAt'
                       -> Natural -- ^ 'ussRecurrenceInHours'
                       -> UpdateSnapshotSchedule
updateSnapshotSchedule p1 p2 p3 = UpdateSnapshotSchedule
    { _ussVolumeARN         = p1
    , _ussStartAt           = withIso _Nat (const id) p2
    , _ussRecurrenceInHours = withIso _Nat (const id) p3
    , _ussDescription       = Nothing
    }

-- | Optional description of the snapshot that overwrites the existing
-- description.
ussDescription :: Lens' UpdateSnapshotSchedule (Maybe Text)
ussDescription = lens _ussDescription (\s a -> s { _ussDescription = a })

-- | Frequency of snapshots. Specify the number of hours between snapshots.
ussRecurrenceInHours :: Lens' UpdateSnapshotSchedule Natural
ussRecurrenceInHours =
    lens _ussRecurrenceInHours (\s a -> s { _ussRecurrenceInHours = a })
        . _Nat

-- | The hour of the day at which the snapshot schedule begins represented as
-- /hh/, where /hh/ is the hour (0 to 23). The hour of the day is in the
-- time zone of the gateway.
ussStartAt :: Lens' UpdateSnapshotSchedule Natural
ussStartAt = lens _ussStartAt (\s a -> s { _ussStartAt = a }) . _Nat

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes'
-- operation to return a list of gateway volumes.
ussVolumeARN :: Lens' UpdateSnapshotSchedule Text
ussVolumeARN = lens _ussVolumeARN (\s a -> s { _ussVolumeARN = a })

newtype UpdateSnapshotScheduleResponse = UpdateSnapshotScheduleResponse
    { _ussrVolumeARN :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'UpdateSnapshotScheduleResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ussrVolumeARN' @::@ 'Maybe' 'Text'
--
updateSnapshotScheduleResponse :: UpdateSnapshotScheduleResponse
updateSnapshotScheduleResponse = UpdateSnapshotScheduleResponse
    { _ussrVolumeARN = Nothing
    }

ussrVolumeARN :: Lens' UpdateSnapshotScheduleResponse (Maybe Text)
ussrVolumeARN = lens _ussrVolumeARN (\s a -> s { _ussrVolumeARN = a })

instance ToPath UpdateSnapshotSchedule where
    toPath = const "/"

instance ToQuery UpdateSnapshotSchedule where
    toQuery = const mempty

instance ToHeaders UpdateSnapshotSchedule

instance ToJSON UpdateSnapshotSchedule where
    toJSON UpdateSnapshotSchedule{..} = object
        [ "VolumeARN"         .= _ussVolumeARN
        , "StartAt"           .= _ussStartAt
        , "RecurrenceInHours" .= _ussRecurrenceInHours
        , "Description"       .= _ussDescription
        ]

instance AWSRequest UpdateSnapshotSchedule where
    type Sv UpdateSnapshotSchedule = StorageGateway
    type Rs UpdateSnapshotSchedule = UpdateSnapshotScheduleResponse

    request  = post "UpdateSnapshotSchedule"
    response = jsonResponse

instance FromJSON UpdateSnapshotScheduleResponse where
    parseJSON = withObject "UpdateSnapshotScheduleResponse" $ \o -> UpdateSnapshotScheduleResponse
        <$> o .:? "VolumeARN"
