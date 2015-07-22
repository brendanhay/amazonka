{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ScheduleRun
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Schedules a run.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ScheduleRun.html>
module Network.AWS.DeviceFarm.ScheduleRun
    (
    -- * Request
      ScheduleRun
    -- ** Request constructor
    , scheduleRun
    -- ** Request lenses
    , srrqName
    , srrqConfiguration
    , srrqProjectARN
    , srrqAppARN
    , srrqDevicePoolARN
    , srrqTest

    -- * Response
    , ScheduleRunResponse
    -- ** Response constructor
    , scheduleRunResponse
    -- ** Response lenses
    , srrsRun
    , srrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the schedule run operation.
--
-- /See:/ 'scheduleRun' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srrqName'
--
-- * 'srrqConfiguration'
--
-- * 'srrqProjectARN'
--
-- * 'srrqAppARN'
--
-- * 'srrqDevicePoolARN'
--
-- * 'srrqTest'
data ScheduleRun = ScheduleRun'
    { _srrqName          :: !(Maybe Text)
    , _srrqConfiguration :: !(Maybe ScheduleRunConfiguration)
    , _srrqProjectARN    :: !Text
    , _srrqAppARN        :: !Text
    , _srrqDevicePoolARN :: !Text
    , _srrqTest          :: !ScheduleRunTest
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ScheduleRun' smart constructor.
scheduleRun :: Text -> Text -> Text -> ScheduleRunTest -> ScheduleRun
scheduleRun pProjectARN pAppARN pDevicePoolARN pTest =
    ScheduleRun'
    { _srrqName = Nothing
    , _srrqConfiguration = Nothing
    , _srrqProjectARN = pProjectARN
    , _srrqAppARN = pAppARN
    , _srrqDevicePoolARN = pDevicePoolARN
    , _srrqTest = pTest
    }

-- | The name for the run to be scheduled.
srrqName :: Lens' ScheduleRun (Maybe Text)
srrqName = lens _srrqName (\ s a -> s{_srrqName = a});

-- | Information about the settings for the run to be scheduled.
srrqConfiguration :: Lens' ScheduleRun (Maybe ScheduleRunConfiguration)
srrqConfiguration = lens _srrqConfiguration (\ s a -> s{_srrqConfiguration = a});

-- | The ARN of the project for the run to be scheduled.
srrqProjectARN :: Lens' ScheduleRun Text
srrqProjectARN = lens _srrqProjectARN (\ s a -> s{_srrqProjectARN = a});

-- | The ARN of the app to schedule a run.
srrqAppARN :: Lens' ScheduleRun Text
srrqAppARN = lens _srrqAppARN (\ s a -> s{_srrqAppARN = a});

-- | The ARN of the device pool for the run to be scheduled.
srrqDevicePoolARN :: Lens' ScheduleRun Text
srrqDevicePoolARN = lens _srrqDevicePoolARN (\ s a -> s{_srrqDevicePoolARN = a});

-- | Information about the test for the run to be scheduled.
srrqTest :: Lens' ScheduleRun ScheduleRunTest
srrqTest = lens _srrqTest (\ s a -> s{_srrqTest = a});

instance AWSRequest ScheduleRun where
        type Sv ScheduleRun = DeviceFarm
        type Rs ScheduleRun = ScheduleRunResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ScheduleRunResponse' <$>
                   (x .?> "run") <*> (pure (fromEnum s)))

instance ToHeaders ScheduleRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ScheduleRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ScheduleRun where
        toJSON ScheduleRun'{..}
          = object
              ["name" .= _srrqName,
               "configuration" .= _srrqConfiguration,
               "projectArn" .= _srrqProjectARN,
               "appArn" .= _srrqAppARN,
               "devicePoolArn" .= _srrqDevicePoolARN,
               "test" .= _srrqTest]

instance ToPath ScheduleRun where
        toPath = const "/"

instance ToQuery ScheduleRun where
        toQuery = const mempty

-- | Represents the result of a schedule run request.
--
-- /See:/ 'scheduleRunResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srrsRun'
--
-- * 'srrsStatus'
data ScheduleRunResponse = ScheduleRunResponse'
    { _srrsRun    :: !(Maybe Run)
    , _srrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ScheduleRunResponse' smart constructor.
scheduleRunResponse :: Int -> ScheduleRunResponse
scheduleRunResponse pStatus =
    ScheduleRunResponse'
    { _srrsRun = Nothing
    , _srrsStatus = pStatus
    }

-- | Information about the scheduled run.
srrsRun :: Lens' ScheduleRunResponse (Maybe Run)
srrsRun = lens _srrsRun (\ s a -> s{_srrsRun = a});

-- | FIXME: Undocumented member.
srrsStatus :: Lens' ScheduleRunResponse Int
srrsStatus = lens _srrsStatus (\ s a -> s{_srrsStatus = a});
