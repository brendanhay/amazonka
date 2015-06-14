{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.DescribeSnapshotSchedule
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation describes the snapshot schedule for the specified gateway
-- volume. The snapshot schedule information includes intervals at which
-- snapshots are automatically initiated on the volume.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeSnapshotSchedule.html>
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
    , dssrStartAt
    , dssrVolumeARN
    , dssrRecurrenceInHours
    , dssrTimezone
    , dssrDescription
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'describeSnapshotSchedule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssVolumeARN'
newtype DescribeSnapshotSchedule = DescribeSnapshotSchedule'{_dssVolumeARN :: Text} deriving (Eq, Read, Show)

-- | 'DescribeSnapshotSchedule' smart constructor.
describeSnapshotSchedule :: Text -> DescribeSnapshotSchedule
describeSnapshotSchedule pVolumeARN = DescribeSnapshotSchedule'{_dssVolumeARN = pVolumeARN};

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
dssVolumeARN :: Lens' DescribeSnapshotSchedule Text
dssVolumeARN = lens _dssVolumeARN (\ s a -> s{_dssVolumeARN = a});

instance AWSRequest DescribeSnapshotSchedule where
        type Sv DescribeSnapshotSchedule = StorageGateway
        type Rs DescribeSnapshotSchedule =
             DescribeSnapshotScheduleResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSnapshotScheduleResponse' <$>
                   x .?> "StartAt" <*> x .:> "VolumeARN" <*>
                     x .:> "RecurrenceInHours"
                     <*> x .:> "Timezone"
                     <*> x .:> "Description")

instance ToHeaders DescribeSnapshotSchedule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeSnapshotSchedule"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSnapshotSchedule where
        toJSON DescribeSnapshotSchedule'{..}
          = object ["VolumeARN" .= _dssVolumeARN]

instance ToPath DescribeSnapshotSchedule where
        toPath = const "/"

instance ToQuery DescribeSnapshotSchedule where
        toQuery = const mempty

-- | /See:/ 'describeSnapshotScheduleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssrStartAt'
--
-- * 'dssrVolumeARN'
--
-- * 'dssrRecurrenceInHours'
--
-- * 'dssrTimezone'
--
-- * 'dssrDescription'
data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse'{_dssrStartAt :: Maybe Nat, _dssrVolumeARN :: Text, _dssrRecurrenceInHours :: Nat, _dssrTimezone :: Text, _dssrDescription :: Text} deriving (Eq, Read, Show)

-- | 'DescribeSnapshotScheduleResponse' smart constructor.
describeSnapshotScheduleResponse :: Text -> Natural -> Text -> Text -> DescribeSnapshotScheduleResponse
describeSnapshotScheduleResponse pVolumeARN pRecurrenceInHours pTimezone pDescription = DescribeSnapshotScheduleResponse'{_dssrStartAt = Nothing, _dssrVolumeARN = pVolumeARN, _dssrRecurrenceInHours = _Nat # pRecurrenceInHours, _dssrTimezone = pTimezone, _dssrDescription = pDescription};

-- | FIXME: Undocumented member.
dssrStartAt :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrStartAt = lens _dssrStartAt (\ s a -> s{_dssrStartAt = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dssrVolumeARN :: Lens' DescribeSnapshotScheduleResponse Text
dssrVolumeARN = lens _dssrVolumeARN (\ s a -> s{_dssrVolumeARN = a});

-- | FIXME: Undocumented member.
dssrRecurrenceInHours :: Lens' DescribeSnapshotScheduleResponse Natural
dssrRecurrenceInHours = lens _dssrRecurrenceInHours (\ s a -> s{_dssrRecurrenceInHours = a}) . _Nat;

-- | FIXME: Undocumented member.
dssrTimezone :: Lens' DescribeSnapshotScheduleResponse Text
dssrTimezone = lens _dssrTimezone (\ s a -> s{_dssrTimezone = a});

-- | FIXME: Undocumented member.
dssrDescription :: Lens' DescribeSnapshotScheduleResponse Text
dssrDescription = lens _dssrDescription (\ s a -> s{_dssrDescription = a});
