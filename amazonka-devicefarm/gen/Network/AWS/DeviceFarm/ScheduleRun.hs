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
-- Module      : Network.AWS.DeviceFarm.ScheduleRun
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a run.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ScheduleRun.html AWS API Reference> for ScheduleRun.
module Network.AWS.DeviceFarm.ScheduleRun
    (
    -- * Creating a Request
      scheduleRun
    , ScheduleRun
    -- * Request Lenses
    , srName
    , srConfiguration
    , srProjectARN
    , srAppARN
    , srDevicePoolARN
    , srTest

    -- * Destructuring the Response
    , scheduleRunResponse
    , ScheduleRunResponse
    -- * Response Lenses
    , srrsRun
    , srrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.DeviceFarm.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the schedule run operation.
--
-- /See:/ 'scheduleRun' smart constructor.
data ScheduleRun = ScheduleRun'
    { _srName          :: !(Maybe Text)
    , _srConfiguration :: !(Maybe ScheduleRunConfiguration)
    , _srProjectARN    :: !Text
    , _srAppARN        :: !Text
    , _srDevicePoolARN :: !Text
    , _srTest          :: !ScheduleRunTest
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScheduleRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srName'
--
-- * 'srConfiguration'
--
-- * 'srProjectARN'
--
-- * 'srAppARN'
--
-- * 'srDevicePoolARN'
--
-- * 'srTest'
scheduleRun
    :: Text -- ^ 'srProjectARN'
    -> Text -- ^ 'srAppARN'
    -> Text -- ^ 'srDevicePoolARN'
    -> ScheduleRunTest -- ^ 'srTest'
    -> ScheduleRun
scheduleRun pProjectARN_ pAppARN_ pDevicePoolARN_ pTest_ =
    ScheduleRun'
    { _srName = Nothing
    , _srConfiguration = Nothing
    , _srProjectARN = pProjectARN_
    , _srAppARN = pAppARN_
    , _srDevicePoolARN = pDevicePoolARN_
    , _srTest = pTest_
    }

-- | The name for the run to be scheduled.
srName :: Lens' ScheduleRun (Maybe Text)
srName = lens _srName (\ s a -> s{_srName = a});

-- | Information about the settings for the run to be scheduled.
srConfiguration :: Lens' ScheduleRun (Maybe ScheduleRunConfiguration)
srConfiguration = lens _srConfiguration (\ s a -> s{_srConfiguration = a});

-- | The ARN of the project for the run to be scheduled.
srProjectARN :: Lens' ScheduleRun Text
srProjectARN = lens _srProjectARN (\ s a -> s{_srProjectARN = a});

-- | The ARN of the app to schedule a run.
srAppARN :: Lens' ScheduleRun Text
srAppARN = lens _srAppARN (\ s a -> s{_srAppARN = a});

-- | The ARN of the device pool for the run to be scheduled.
srDevicePoolARN :: Lens' ScheduleRun Text
srDevicePoolARN = lens _srDevicePoolARN (\ s a -> s{_srDevicePoolARN = a});

-- | Information about the test for the run to be scheduled.
srTest :: Lens' ScheduleRun ScheduleRunTest
srTest = lens _srTest (\ s a -> s{_srTest = a});

instance AWSRequest ScheduleRun where
        type Rs ScheduleRun = ScheduleRunResponse
        request = postJSON deviceFarm
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
              (catMaybes
                 [("name" .=) <$> _srName,
                  ("configuration" .=) <$> _srConfiguration,
                  Just ("projectArn" .= _srProjectARN),
                  Just ("appArn" .= _srAppARN),
                  Just ("devicePoolArn" .= _srDevicePoolARN),
                  Just ("test" .= _srTest)])

instance ToPath ScheduleRun where
        toPath = const "/"

instance ToQuery ScheduleRun where
        toQuery = const mempty

-- | Represents the result of a schedule run request.
--
-- /See:/ 'scheduleRunResponse' smart constructor.
data ScheduleRunResponse = ScheduleRunResponse'
    { _srrsRun    :: !(Maybe Run)
    , _srrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScheduleRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrsRun'
--
-- * 'srrsStatus'
scheduleRunResponse
    :: Int -- ^ 'srrsStatus'
    -> ScheduleRunResponse
scheduleRunResponse pStatus_ =
    ScheduleRunResponse'
    { _srrsRun = Nothing
    , _srrsStatus = pStatus_
    }

-- | Information about the scheduled run.
srrsRun :: Lens' ScheduleRunResponse (Maybe Run)
srrsRun = lens _srrsRun (\ s a -> s{_srrsRun = a});

-- | The response status code.
srrsStatus :: Lens' ScheduleRunResponse Int
srrsStatus = lens _srrsStatus (\ s a -> s{_srrsStatus = a});
