{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scheduled scaling action for an Auto Scaling group.
-- When updating a scheduled scaling action, if you leave a parameter
-- unspecified, the corresponding value remains unchanged in the affected
-- Auto Scaling group.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/schedule_time.html Scheduled Scaling>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutScheduledUpdateGroupAction.html>
module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
    (
    -- * Request
      PutScheduledUpdateGroupAction
    -- ** Request constructor
    , putScheduledUpdateGroupAction
    -- ** Request lenses
    , psugaTime
    , psugaStartTime
    , psugaMaxSize
    , psugaDesiredCapacity
    , psugaRecurrence
    , psugaMinSize
    , psugaEndTime
    , psugaAutoScalingGroupName
    , psugaScheduledActionName

    -- * Response
    , PutScheduledUpdateGroupActionResponse
    -- ** Response constructor
    , putScheduledUpdateGroupActionResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putScheduledUpdateGroupAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psugaTime'
--
-- * 'psugaStartTime'
--
-- * 'psugaMaxSize'
--
-- * 'psugaDesiredCapacity'
--
-- * 'psugaRecurrence'
--
-- * 'psugaMinSize'
--
-- * 'psugaEndTime'
--
-- * 'psugaAutoScalingGroupName'
--
-- * 'psugaScheduledActionName'
data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction'
    { _psugaTime                 :: !(Maybe ISO8601)
    , _psugaStartTime            :: !(Maybe ISO8601)
    , _psugaMaxSize              :: !(Maybe Int)
    , _psugaDesiredCapacity      :: !(Maybe Int)
    , _psugaRecurrence           :: !(Maybe Text)
    , _psugaMinSize              :: !(Maybe Int)
    , _psugaEndTime              :: !(Maybe ISO8601)
    , _psugaAutoScalingGroupName :: !Text
    , _psugaScheduledActionName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutScheduledUpdateGroupAction' smart constructor.
putScheduledUpdateGroupAction :: Text -> Text -> PutScheduledUpdateGroupAction
putScheduledUpdateGroupAction pAutoScalingGroupName_ pScheduledActionName_ =
    PutScheduledUpdateGroupAction'
    { _psugaTime = Nothing
    , _psugaStartTime = Nothing
    , _psugaMaxSize = Nothing
    , _psugaDesiredCapacity = Nothing
    , _psugaRecurrence = Nothing
    , _psugaMinSize = Nothing
    , _psugaEndTime = Nothing
    , _psugaAutoScalingGroupName = pAutoScalingGroupName_
    , _psugaScheduledActionName = pScheduledActionName_
    }

-- | This parameter is deprecated; use @StartTime@ instead.
--
-- The time for this action to start. If both @Time@ and @StartTime@ are
-- specified, their values must be identical.
psugaTime :: Lens' PutScheduledUpdateGroupAction (Maybe UTCTime)
psugaTime = lens _psugaTime (\ s a -> s{_psugaTime = a}) . mapping _Time;

-- | The time for this action to start, in \"YYYY-MM-DDThh:mm:ssZ\" format in
-- UTC\/GMT only (for example, @2014-06-01T00:00:00Z@).
--
-- If you try to schedule your action in the past, Auto Scaling returns an
-- error message.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
psugaStartTime :: Lens' PutScheduledUpdateGroupAction (Maybe UTCTime)
psugaStartTime = lens _psugaStartTime (\ s a -> s{_psugaStartTime = a}) . mapping _Time;

-- | The maximum size for the Auto Scaling group.
psugaMaxSize :: Lens' PutScheduledUpdateGroupAction (Maybe Int)
psugaMaxSize = lens _psugaMaxSize (\ s a -> s{_psugaMaxSize = a});

-- | The number of EC2 instances that should be running in the group.
psugaDesiredCapacity :: Lens' PutScheduledUpdateGroupAction (Maybe Int)
psugaDesiredCapacity = lens _psugaDesiredCapacity (\ s a -> s{_psugaDesiredCapacity = a});

-- | The time when recurring future actions will start. Start time is
-- specified by the user following the Unix cron syntax format. For more
-- information, see <http://en.wikipedia.org/wiki/Cron Cron> in Wikipedia.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action will start and stop.
psugaRecurrence :: Lens' PutScheduledUpdateGroupAction (Maybe Text)
psugaRecurrence = lens _psugaRecurrence (\ s a -> s{_psugaRecurrence = a});

-- | The minimum size for the Auto Scaling group.
psugaMinSize :: Lens' PutScheduledUpdateGroupAction (Maybe Int)
psugaMinSize = lens _psugaMinSize (\ s a -> s{_psugaMinSize = a});

-- | The time for this action to end.
psugaEndTime :: Lens' PutScheduledUpdateGroupAction (Maybe UTCTime)
psugaEndTime = lens _psugaEndTime (\ s a -> s{_psugaEndTime = a}) . mapping _Time;

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
psugaAutoScalingGroupName :: Lens' PutScheduledUpdateGroupAction Text
psugaAutoScalingGroupName = lens _psugaAutoScalingGroupName (\ s a -> s{_psugaAutoScalingGroupName = a});

-- | The name of this scaling action.
psugaScheduledActionName :: Lens' PutScheduledUpdateGroupAction Text
psugaScheduledActionName = lens _psugaScheduledActionName (\ s a -> s{_psugaScheduledActionName = a});

instance AWSRequest PutScheduledUpdateGroupAction
         where
        type Sv PutScheduledUpdateGroupAction = AutoScaling
        type Rs PutScheduledUpdateGroupAction =
             PutScheduledUpdateGroupActionResponse
        request = postQuery
        response
          = receiveNull PutScheduledUpdateGroupActionResponse'

instance ToHeaders PutScheduledUpdateGroupAction
         where
        toHeaders = const mempty

instance ToPath PutScheduledUpdateGroupAction where
        toPath = const mempty

instance ToQuery PutScheduledUpdateGroupAction where
        toQuery PutScheduledUpdateGroupAction'{..}
          = mconcat
              ["Action" =:
                 ("PutScheduledUpdateGroupAction" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "Time" =: _psugaTime, "StartTime" =: _psugaStartTime,
               "MaxSize" =: _psugaMaxSize,
               "DesiredCapacity" =: _psugaDesiredCapacity,
               "Recurrence" =: _psugaRecurrence,
               "MinSize" =: _psugaMinSize,
               "EndTime" =: _psugaEndTime,
               "AutoScalingGroupName" =: _psugaAutoScalingGroupName,
               "ScheduledActionName" =: _psugaScheduledActionName]

-- | /See:/ 'putScheduledUpdateGroupActionResponse' smart constructor.
data PutScheduledUpdateGroupActionResponse =
    PutScheduledUpdateGroupActionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutScheduledUpdateGroupActionResponse' smart constructor.
putScheduledUpdateGroupActionResponse :: PutScheduledUpdateGroupActionResponse
putScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse'
