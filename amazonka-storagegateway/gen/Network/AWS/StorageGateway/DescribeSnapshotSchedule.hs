{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.StorageGateway.DescribeSnapshotSchedule
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , dssrVolumeARN
    , dssrStartAt
    , dssrRecurrenceInHours
    , dssrTimezone
    , dssrDescription
    , dssrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing the DescribeSnapshotScheduleInput$VolumeARN of
-- the volume.
--
-- /See:/ 'describeSnapshotSchedule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssVolumeARN'
newtype DescribeSnapshotSchedule = DescribeSnapshotSchedule'
    { _dssVolumeARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotSchedule' smart constructor.
describeSnapshotSchedule :: Text -> DescribeSnapshotSchedule
describeSnapshotSchedule pVolumeARN =
    DescribeSnapshotSchedule'
    { _dssVolumeARN = pVolumeARN
    }

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
                   (x .?> "VolumeARN") <*> (x .?> "StartAt") <*>
                     (x .?> "RecurrenceInHours")
                     <*> (x .?> "Timezone")
                     <*> (x .?> "Description")
                     <*> (pure (fromEnum s)))

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
-- * 'dssrVolumeARN'
--
-- * 'dssrStartAt'
--
-- * 'dssrRecurrenceInHours'
--
-- * 'dssrTimezone'
--
-- * 'dssrDescription'
--
-- * 'dssrStatus'
data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse'
    { _dssrVolumeARN         :: !(Maybe Text)
    , _dssrStartAt           :: !(Maybe Nat)
    , _dssrRecurrenceInHours :: !(Maybe Nat)
    , _dssrTimezone          :: !(Maybe Text)
    , _dssrDescription       :: !(Maybe Text)
    , _dssrStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotScheduleResponse' smart constructor.
describeSnapshotScheduleResponse :: Int -> DescribeSnapshotScheduleResponse
describeSnapshotScheduleResponse pStatus =
    DescribeSnapshotScheduleResponse'
    { _dssrVolumeARN = Nothing
    , _dssrStartAt = Nothing
    , _dssrRecurrenceInHours = Nothing
    , _dssrTimezone = Nothing
    , _dssrDescription = Nothing
    , _dssrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dssrVolumeARN :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrVolumeARN = lens _dssrVolumeARN (\ s a -> s{_dssrVolumeARN = a});

-- | FIXME: Undocumented member.
dssrStartAt :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrStartAt = lens _dssrStartAt (\ s a -> s{_dssrStartAt = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dssrRecurrenceInHours :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrRecurrenceInHours = lens _dssrRecurrenceInHours (\ s a -> s{_dssrRecurrenceInHours = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dssrTimezone :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrTimezone = lens _dssrTimezone (\ s a -> s{_dssrTimezone = a});

-- | FIXME: Undocumented member.
dssrDescription :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrDescription = lens _dssrDescription (\ s a -> s{_dssrDescription = a});

-- | FIXME: Undocumented member.
dssrStatus :: Lens' DescribeSnapshotScheduleResponse Int
dssrStatus = lens _dssrStatus (\ s a -> s{_dssrStatus = a});
