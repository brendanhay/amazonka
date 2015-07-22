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
    , psugarqTime
    , psugarqStartTime
    , psugarqMaxSize
    , psugarqDesiredCapacity
    , psugarqRecurrence
    , psugarqMinSize
    , psugarqEndTime
    , psugarqAutoScalingGroupName
    , psugarqScheduledActionName

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
-- * 'psugarqTime'
--
-- * 'psugarqStartTime'
--
-- * 'psugarqMaxSize'
--
-- * 'psugarqDesiredCapacity'
--
-- * 'psugarqRecurrence'
--
-- * 'psugarqMinSize'
--
-- * 'psugarqEndTime'
--
-- * 'psugarqAutoScalingGroupName'
--
-- * 'psugarqScheduledActionName'
data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction'
    { _psugarqTime                 :: !(Maybe ISO8601)
    , _psugarqStartTime            :: !(Maybe ISO8601)
    , _psugarqMaxSize              :: !(Maybe Int)
    , _psugarqDesiredCapacity      :: !(Maybe Int)
    , _psugarqRecurrence           :: !(Maybe Text)
    , _psugarqMinSize              :: !(Maybe Int)
    , _psugarqEndTime              :: !(Maybe ISO8601)
    , _psugarqAutoScalingGroupName :: !Text
    , _psugarqScheduledActionName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutScheduledUpdateGroupAction' smart constructor.
putScheduledUpdateGroupAction :: Text -> Text -> PutScheduledUpdateGroupAction
putScheduledUpdateGroupAction pAutoScalingGroupName pScheduledActionName =
    PutScheduledUpdateGroupAction'
    { _psugarqTime = Nothing
    , _psugarqStartTime = Nothing
    , _psugarqMaxSize = Nothing
    , _psugarqDesiredCapacity = Nothing
    , _psugarqRecurrence = Nothing
    , _psugarqMinSize = Nothing
    , _psugarqEndTime = Nothing
    , _psugarqAutoScalingGroupName = pAutoScalingGroupName
    , _psugarqScheduledActionName = pScheduledActionName
    }

-- | This parameter is deprecated; use @StartTime@ instead.
--
-- The time for this action to start. If both @Time@ and @StartTime@ are
-- specified, their values must be identical.
psugarqTime :: Lens' PutScheduledUpdateGroupAction (Maybe UTCTime)
psugarqTime = lens _psugarqTime (\ s a -> s{_psugarqTime = a}) . mapping _Time;

-- | The time for this action to start, in \"YYYY-MM-DDThh:mm:ssZ\" format in
-- UTC\/GMT only (for example, @2014-06-01T00:00:00Z@).
--
-- If you try to schedule your action in the past, Auto Scaling returns an
-- error message.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
psugarqStartTime :: Lens' PutScheduledUpdateGroupAction (Maybe UTCTime)
psugarqStartTime = lens _psugarqStartTime (\ s a -> s{_psugarqStartTime = a}) . mapping _Time;

-- | The maximum size for the Auto Scaling group.
psugarqMaxSize :: Lens' PutScheduledUpdateGroupAction (Maybe Int)
psugarqMaxSize = lens _psugarqMaxSize (\ s a -> s{_psugarqMaxSize = a});

-- | The number of EC2 instances that should be running in the group.
psugarqDesiredCapacity :: Lens' PutScheduledUpdateGroupAction (Maybe Int)
psugarqDesiredCapacity = lens _psugarqDesiredCapacity (\ s a -> s{_psugarqDesiredCapacity = a});

-- | The time when recurring future actions will start. Start time is
-- specified by the user following the Unix cron syntax format. For more
-- information, see <http://en.wikipedia.org/wiki/Cron Cron> in Wikipedia.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action will start and stop.
psugarqRecurrence :: Lens' PutScheduledUpdateGroupAction (Maybe Text)
psugarqRecurrence = lens _psugarqRecurrence (\ s a -> s{_psugarqRecurrence = a});

-- | The minimum size for the Auto Scaling group.
psugarqMinSize :: Lens' PutScheduledUpdateGroupAction (Maybe Int)
psugarqMinSize = lens _psugarqMinSize (\ s a -> s{_psugarqMinSize = a});

-- | The time for this action to end.
psugarqEndTime :: Lens' PutScheduledUpdateGroupAction (Maybe UTCTime)
psugarqEndTime = lens _psugarqEndTime (\ s a -> s{_psugarqEndTime = a}) . mapping _Time;

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
psugarqAutoScalingGroupName :: Lens' PutScheduledUpdateGroupAction Text
psugarqAutoScalingGroupName = lens _psugarqAutoScalingGroupName (\ s a -> s{_psugarqAutoScalingGroupName = a});

-- | The name of this scaling action.
psugarqScheduledActionName :: Lens' PutScheduledUpdateGroupAction Text
psugarqScheduledActionName = lens _psugarqScheduledActionName (\ s a -> s{_psugarqScheduledActionName = a});

instance AWSRequest PutScheduledUpdateGroupAction
         where
        type Sv PutScheduledUpdateGroupAction = AutoScaling
        type Rs PutScheduledUpdateGroupAction =
             PutScheduledUpdateGroupActionResponse
        request = post
        response
          = receiveNull PutScheduledUpdateGroupActionResponse'

instance ToHeaders PutScheduledUpdateGroupAction
         where
        toHeaders = const mempty

instance ToPath PutScheduledUpdateGroupAction where
        toPath = const "/"

instance ToQuery PutScheduledUpdateGroupAction where
        toQuery PutScheduledUpdateGroupAction'{..}
          = mconcat
              ["Action" =:
                 ("PutScheduledUpdateGroupAction" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "Time" =: _psugarqTime,
               "StartTime" =: _psugarqStartTime,
               "MaxSize" =: _psugarqMaxSize,
               "DesiredCapacity" =: _psugarqDesiredCapacity,
               "Recurrence" =: _psugarqRecurrence,
               "MinSize" =: _psugarqMinSize,
               "EndTime" =: _psugarqEndTime,
               "AutoScalingGroupName" =:
                 _psugarqAutoScalingGroupName,
               "ScheduledActionName" =: _psugarqScheduledActionName]

-- | /See:/ 'putScheduledUpdateGroupActionResponse' smart constructor.
data PutScheduledUpdateGroupActionResponse =
    PutScheduledUpdateGroupActionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutScheduledUpdateGroupActionResponse' smart constructor.
putScheduledUpdateGroupActionResponse :: PutScheduledUpdateGroupActionResponse
putScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse'
