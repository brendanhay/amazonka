{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeSnapshotSchedule
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation describes the snapshot schedule for the specified gateway
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
    , dssrqVolumeARN

    -- * Response
    , DescribeSnapshotScheduleResponse
    -- ** Response constructor
    , describeSnapshotScheduleResponse
    -- ** Response lenses
    , dssrsVolumeARN
    , dssrsStartAt
    , dssrsRecurrenceInHours
    , dssrsTimezone
    , dssrsDescription
    , dssrsStatus
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
-- * 'dssrqVolumeARN'
newtype DescribeSnapshotSchedule = DescribeSnapshotSchedule'
    { _dssrqVolumeARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotSchedule' smart constructor.
describeSnapshotSchedule :: Text -> DescribeSnapshotSchedule
describeSnapshotSchedule pVolumeARN_ =
    DescribeSnapshotSchedule'
    { _dssrqVolumeARN = pVolumeARN_
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
dssrqVolumeARN :: Lens' DescribeSnapshotSchedule Text
dssrqVolumeARN = lens _dssrqVolumeARN (\ s a -> s{_dssrqVolumeARN = a});

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
          = object ["VolumeARN" .= _dssrqVolumeARN]

instance ToPath DescribeSnapshotSchedule where
        toPath = const "/"

instance ToQuery DescribeSnapshotSchedule where
        toQuery = const mempty

-- | /See:/ 'describeSnapshotScheduleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssrsVolumeARN'
--
-- * 'dssrsStartAt'
--
-- * 'dssrsRecurrenceInHours'
--
-- * 'dssrsTimezone'
--
-- * 'dssrsDescription'
--
-- * 'dssrsStatus'
data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse'
    { _dssrsVolumeARN         :: !(Maybe Text)
    , _dssrsStartAt           :: !(Maybe Nat)
    , _dssrsRecurrenceInHours :: !(Maybe Nat)
    , _dssrsTimezone          :: !(Maybe Text)
    , _dssrsDescription       :: !(Maybe Text)
    , _dssrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotScheduleResponse' smart constructor.
describeSnapshotScheduleResponse :: Int -> DescribeSnapshotScheduleResponse
describeSnapshotScheduleResponse pStatus_ =
    DescribeSnapshotScheduleResponse'
    { _dssrsVolumeARN = Nothing
    , _dssrsStartAt = Nothing
    , _dssrsRecurrenceInHours = Nothing
    , _dssrsTimezone = Nothing
    , _dssrsDescription = Nothing
    , _dssrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dssrsVolumeARN :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsVolumeARN = lens _dssrsVolumeARN (\ s a -> s{_dssrsVolumeARN = a});

-- | FIXME: Undocumented member.
dssrsStartAt :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrsStartAt = lens _dssrsStartAt (\ s a -> s{_dssrsStartAt = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dssrsRecurrenceInHours :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrsRecurrenceInHours = lens _dssrsRecurrenceInHours (\ s a -> s{_dssrsRecurrenceInHours = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dssrsTimezone :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsTimezone = lens _dssrsTimezone (\ s a -> s{_dssrsTimezone = a});

-- | FIXME: Undocumented member.
dssrsDescription :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsDescription = lens _dssrsDescription (\ s a -> s{_dssrsDescription = a});

-- | FIXME: Undocumented member.
dssrsStatus :: Lens' DescribeSnapshotScheduleResponse Int
dssrsStatus = lens _dssrsStatus (\ s a -> s{_dssrsStatus = a});
