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
-- Module      : Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scheduled scaling action for an Auto Scaling group. If you leave a parameter unspecified when updating a scheduled scaling action, the corresponding value remains unchanged.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/schedule_time.html Scheduled Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
    (
    -- * Creating a Request
      putScheduledUpdateGroupAction
    , PutScheduledUpdateGroupAction
    -- * Request Lenses
    , psugaStartTime
    , psugaTime
    , psugaMaxSize
    , psugaRecurrence
    , psugaDesiredCapacity
    , psugaMinSize
    , psugaEndTime
    , psugaAutoScalingGroupName
    , psugaScheduledActionName

    -- * Destructuring the Response
    , putScheduledUpdateGroupActionResponse
    , PutScheduledUpdateGroupActionResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putScheduledUpdateGroupAction' smart constructor.
data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction'
  { _psugaStartTime            :: !(Maybe ISO8601)
  , _psugaTime                 :: !(Maybe ISO8601)
  , _psugaMaxSize              :: !(Maybe Int)
  , _psugaRecurrence           :: !(Maybe Text)
  , _psugaDesiredCapacity      :: !(Maybe Int)
  , _psugaMinSize              :: !(Maybe Int)
  , _psugaEndTime              :: !(Maybe ISO8601)
  , _psugaAutoScalingGroupName :: !Text
  , _psugaScheduledActionName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutScheduledUpdateGroupAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psugaStartTime' - The time for this action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ). If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence. If you try to schedule your action in the past, Amazon EC2 Auto Scaling returns an error message.
--
-- * 'psugaTime' - This parameter is deprecated.
--
-- * 'psugaMaxSize' - The maximum size for the Auto Scaling group.
--
-- * 'psugaRecurrence' - The recurring schedule for this action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> .
--
-- * 'psugaDesiredCapacity' - The number of EC2 instances that should be running in the group.
--
-- * 'psugaMinSize' - The minimum size for the Auto Scaling group.
--
-- * 'psugaEndTime' - The time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
--
-- * 'psugaAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'psugaScheduledActionName' - The name of this scaling action.
putScheduledUpdateGroupAction
    :: Text -- ^ 'psugaAutoScalingGroupName'
    -> Text -- ^ 'psugaScheduledActionName'
    -> PutScheduledUpdateGroupAction
putScheduledUpdateGroupAction pAutoScalingGroupName_ pScheduledActionName_ =
  PutScheduledUpdateGroupAction'
    { _psugaStartTime = Nothing
    , _psugaTime = Nothing
    , _psugaMaxSize = Nothing
    , _psugaRecurrence = Nothing
    , _psugaDesiredCapacity = Nothing
    , _psugaMinSize = Nothing
    , _psugaEndTime = Nothing
    , _psugaAutoScalingGroupName = pAutoScalingGroupName_
    , _psugaScheduledActionName = pScheduledActionName_
    }


-- | The time for this action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ). If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence. If you try to schedule your action in the past, Amazon EC2 Auto Scaling returns an error message.
psugaStartTime :: Lens' PutScheduledUpdateGroupAction (Maybe UTCTime)
psugaStartTime = lens _psugaStartTime (\ s a -> s{_psugaStartTime = a}) . mapping _Time

-- | This parameter is deprecated.
psugaTime :: Lens' PutScheduledUpdateGroupAction (Maybe UTCTime)
psugaTime = lens _psugaTime (\ s a -> s{_psugaTime = a}) . mapping _Time

-- | The maximum size for the Auto Scaling group.
psugaMaxSize :: Lens' PutScheduledUpdateGroupAction (Maybe Int)
psugaMaxSize = lens _psugaMaxSize (\ s a -> s{_psugaMaxSize = a})

-- | The recurring schedule for this action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> .
psugaRecurrence :: Lens' PutScheduledUpdateGroupAction (Maybe Text)
psugaRecurrence = lens _psugaRecurrence (\ s a -> s{_psugaRecurrence = a})

-- | The number of EC2 instances that should be running in the group.
psugaDesiredCapacity :: Lens' PutScheduledUpdateGroupAction (Maybe Int)
psugaDesiredCapacity = lens _psugaDesiredCapacity (\ s a -> s{_psugaDesiredCapacity = a})

-- | The minimum size for the Auto Scaling group.
psugaMinSize :: Lens' PutScheduledUpdateGroupAction (Maybe Int)
psugaMinSize = lens _psugaMinSize (\ s a -> s{_psugaMinSize = a})

-- | The time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
psugaEndTime :: Lens' PutScheduledUpdateGroupAction (Maybe UTCTime)
psugaEndTime = lens _psugaEndTime (\ s a -> s{_psugaEndTime = a}) . mapping _Time

-- | The name of the Auto Scaling group.
psugaAutoScalingGroupName :: Lens' PutScheduledUpdateGroupAction Text
psugaAutoScalingGroupName = lens _psugaAutoScalingGroupName (\ s a -> s{_psugaAutoScalingGroupName = a})

-- | The name of this scaling action.
psugaScheduledActionName :: Lens' PutScheduledUpdateGroupAction Text
psugaScheduledActionName = lens _psugaScheduledActionName (\ s a -> s{_psugaScheduledActionName = a})

instance AWSRequest PutScheduledUpdateGroupAction
         where
        type Rs PutScheduledUpdateGroupAction =
             PutScheduledUpdateGroupActionResponse
        request = postQuery autoScaling
        response
          = receiveNull PutScheduledUpdateGroupActionResponse'

instance Hashable PutScheduledUpdateGroupAction where

instance NFData PutScheduledUpdateGroupAction where

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
               "StartTime" =: _psugaStartTime, "Time" =: _psugaTime,
               "MaxSize" =: _psugaMaxSize,
               "Recurrence" =: _psugaRecurrence,
               "DesiredCapacity" =: _psugaDesiredCapacity,
               "MinSize" =: _psugaMinSize,
               "EndTime" =: _psugaEndTime,
               "AutoScalingGroupName" =: _psugaAutoScalingGroupName,
               "ScheduledActionName" =: _psugaScheduledActionName]

-- | /See:/ 'putScheduledUpdateGroupActionResponse' smart constructor.
data PutScheduledUpdateGroupActionResponse =
  PutScheduledUpdateGroupActionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutScheduledUpdateGroupActionResponse' with the minimum fields required to make a request.
--
putScheduledUpdateGroupActionResponse
    :: PutScheduledUpdateGroupActionResponse
putScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse'


instance NFData PutScheduledUpdateGroupActionResponse
         where
