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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the snapshot schedule for the specified gateway volume. The snapshot schedule information includes intervals at which snapshots are automatically initiated on the volume. This operation is only supported in the cached volume and stored volume types.
--
--
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
    , dssrsStartAt
    , dssrsVolumeARN
    , dssrsRecurrenceInHours
    , dssrsTimezone
    , dssrsDescription
    , dssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing the 'DescribeSnapshotScheduleInput$VolumeARN' of the volume.
--
--
--
-- /See:/ 'describeSnapshotSchedule' smart constructor.
newtype DescribeSnapshotSchedule = DescribeSnapshotSchedule'
  { _dssVolumeARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSnapshotSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssVolumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
describeSnapshotSchedule
    :: Text -- ^ 'dssVolumeARN'
    -> DescribeSnapshotSchedule
describeSnapshotSchedule pVolumeARN_ =
  DescribeSnapshotSchedule' {_dssVolumeARN = pVolumeARN_}


-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
dssVolumeARN :: Lens' DescribeSnapshotSchedule Text
dssVolumeARN = lens _dssVolumeARN (\ s a -> s{_dssVolumeARN = a})

instance AWSRequest DescribeSnapshotSchedule where
        type Rs DescribeSnapshotSchedule =
             DescribeSnapshotScheduleResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSnapshotScheduleResponse' <$>
                   (x .?> "StartAt") <*> (x .?> "VolumeARN") <*>
                     (x .?> "RecurrenceInHours")
                     <*> (x .?> "Timezone")
                     <*> (x .?> "Description")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSnapshotSchedule where

instance NFData DescribeSnapshotSchedule where

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
  { _dssrsStartAt           :: !(Maybe Nat)
  , _dssrsVolumeARN         :: !(Maybe Text)
  , _dssrsRecurrenceInHours :: !(Maybe Nat)
  , _dssrsTimezone          :: !(Maybe Text)
  , _dssrsDescription       :: !(Maybe Text)
  , _dssrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSnapshotScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsStartAt' - Undocumented member.
--
-- * 'dssrsVolumeARN' - Undocumented member.
--
-- * 'dssrsRecurrenceInHours' - Undocumented member.
--
-- * 'dssrsTimezone' - Undocumented member.
--
-- * 'dssrsDescription' - Undocumented member.
--
-- * 'dssrsResponseStatus' - -- | The response status code.
describeSnapshotScheduleResponse
    :: Int -- ^ 'dssrsResponseStatus'
    -> DescribeSnapshotScheduleResponse
describeSnapshotScheduleResponse pResponseStatus_ =
  DescribeSnapshotScheduleResponse'
    { _dssrsStartAt = Nothing
    , _dssrsVolumeARN = Nothing
    , _dssrsRecurrenceInHours = Nothing
    , _dssrsTimezone = Nothing
    , _dssrsDescription = Nothing
    , _dssrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
dssrsStartAt :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrsStartAt = lens _dssrsStartAt (\ s a -> s{_dssrsStartAt = a}) . mapping _Nat

-- | Undocumented member.
dssrsVolumeARN :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsVolumeARN = lens _dssrsVolumeARN (\ s a -> s{_dssrsVolumeARN = a})

-- | Undocumented member.
dssrsRecurrenceInHours :: Lens' DescribeSnapshotScheduleResponse (Maybe Natural)
dssrsRecurrenceInHours = lens _dssrsRecurrenceInHours (\ s a -> s{_dssrsRecurrenceInHours = a}) . mapping _Nat

-- | Undocumented member.
dssrsTimezone :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsTimezone = lens _dssrsTimezone (\ s a -> s{_dssrsTimezone = a})

-- | Undocumented member.
dssrsDescription :: Lens' DescribeSnapshotScheduleResponse (Maybe Text)
dssrsDescription = lens _dssrsDescription (\ s a -> s{_dssrsDescription = a})

-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeSnapshotScheduleResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\ s a -> s{_dssrsResponseStatus = a})

instance NFData DescribeSnapshotScheduleResponse
         where
