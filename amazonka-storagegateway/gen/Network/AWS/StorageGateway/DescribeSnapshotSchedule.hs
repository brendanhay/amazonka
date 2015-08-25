{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeSnapshotSchedule
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation describes the snapshot schedule for the specified gateway
-- volume. The snapshot schedule information includes intervals at which
-- snapshots are automatically initiated on the volume.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeSnapshotSchedule.html AWS API Reference> for DescribeSnapshotSchedule.
module Network.AWS.StorageGateway.DescribeSnapshotSchedule
    (
    -- * Creating a Request
      describeSnapshotSchedule
    , DescribeSnapshotSchedule
    -- * Request Lenses
    , dssVolumeARN

    -- * Destructuring the Response
    , describeSnapshotScheduleResponse
    , DescribeSnapshotScheduleResponse
    -- * Response Lenses
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
import           Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing the DescribeSnapshotScheduleInput$VolumeARN of
-- the volume.
--
-- /See:/ 'describeSnapshotSchedule' smart constructor.
newtype DescribeSnapshotSchedule = DescribeSnapshotSchedule'
    { _dssVolumeARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeSnapshotSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssVolumeARN'
describeSnapshotSchedule
    :: Text -- ^ 'dssVolumeARN'
    -> DescribeSnapshotSchedule
describeSnapshotSchedule pVolumeARN_ =
    DescribeSnapshotSchedule'
    { _dssVolumeARN = pVolumeARN_
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
dssVolumeARN :: Lens' DescribeSnapshotSchedule Text
dssVolumeARN = lens _dssVolumeARN (\ s a -> s{_dssVolumeARN = a});

instance AWSRequest DescribeSnapshotSchedule where
        type Rs DescribeSnapshotSchedule =
             DescribeSnapshotScheduleResponse
        request = postJSON storageGateway
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
          = object
              (catMaybes [Just ("VolumeARN" .= _dssVolumeARN)])

instance ToPath DescribeSnapshotSchedule where
        toPath = const "/"

instance ToQuery DescribeSnapshotSchedule where
        toQuery = const mempty

-- | /See:/ 'describeSnapshotScheduleResponse' smart constructor.
data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse'
    { _dssrsVolumeARN         :: !(Maybe Text)
    , _dssrsStartAt           :: !(Maybe Nat)
    , _dssrsRecurrenceInHours :: !(Maybe Nat)
    , _dssrsTimezone          :: !(Maybe Text)
    , _dssrsDescription       :: !(Maybe Text)
    , _dssrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeSnapshotScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
describeSnapshotScheduleResponse
    :: Int -- ^ 'dssrsStatus'
    -> DescribeSnapshotScheduleResponse
describeSnapshotScheduleResponse pStatus_ =
    DescribeSnapshotScheduleResponse'
    { _dssrsVolumeARN = Nothing
    , _dssrsStartAt = Nothing
    , _dssrsRecurrenceInHours = Nothing
    , _dssrsTimezone = Nothing
    , _dssrsDescription = Nothing
    , _dssrsStatus = pStatus_
    }

-- | Undocumented member.
dssrsVolumeARN :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsVolumeARN = lens _dssrsVolumeARN (\ s a -> s{_dssrsVolumeARN = a});

-- | Undocumented member.
dssrsStartAt :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrsStartAt = lens _dssrsStartAt (\ s a -> s{_dssrsStartAt = a}) . mapping _Nat;

-- | Undocumented member.
dssrsRecurrenceInHours :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrsRecurrenceInHours = lens _dssrsRecurrenceInHours (\ s a -> s{_dssrsRecurrenceInHours = a}) . mapping _Nat;

-- | Undocumented member.
dssrsTimezone :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsTimezone = lens _dssrsTimezone (\ s a -> s{_dssrsTimezone = a});

-- | Undocumented member.
dssrsDescription :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsDescription = lens _dssrsDescription (\ s a -> s{_dssrsDescription = a});

-- | The response status code.
dssrsStatus :: Lens' DescribeSnapshotScheduleResponse Int
dssrsStatus = lens _dssrsStatus (\ s a -> s{_dssrsStatus = a});
