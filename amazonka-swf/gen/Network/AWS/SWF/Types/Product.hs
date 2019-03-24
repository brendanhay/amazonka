{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.Sum

-- | Provides the details of the @ActivityTaskCancelRequested@ event.
--
--
--
-- /See:/ 'activityTaskCancelRequestedEventAttributes' smart constructor.
data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes'
  { _atcreaDecisionTaskCompletedEventId :: !Integer
  , _atcreaActivityId                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityTaskCancelRequestedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atcreaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelActivityTask@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'atcreaActivityId' - The unique ID of the task.
activityTaskCancelRequestedEventAttributes
    :: Integer -- ^ 'atcreaDecisionTaskCompletedEventId'
    -> Text -- ^ 'atcreaActivityId'
    -> ActivityTaskCancelRequestedEventAttributes
activityTaskCancelRequestedEventAttributes pDecisionTaskCompletedEventId_ pActivityId_ =
  ActivityTaskCancelRequestedEventAttributes'
    { _atcreaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId_
    , _atcreaActivityId = pActivityId_
    }


-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelActivityTask@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atcreaDecisionTaskCompletedEventId :: Lens' ActivityTaskCancelRequestedEventAttributes Integer
atcreaDecisionTaskCompletedEventId = lens _atcreaDecisionTaskCompletedEventId (\ s a -> s{_atcreaDecisionTaskCompletedEventId = a})

-- | The unique ID of the task.
atcreaActivityId :: Lens' ActivityTaskCancelRequestedEventAttributes Text
atcreaActivityId = lens _atcreaActivityId (\ s a -> s{_atcreaActivityId = a})

instance FromJSON
           ActivityTaskCancelRequestedEventAttributes
         where
        parseJSON
          = withObject
              "ActivityTaskCancelRequestedEventAttributes"
              (\ x ->
                 ActivityTaskCancelRequestedEventAttributes' <$>
                   (x .: "decisionTaskCompletedEventId") <*>
                     (x .: "activityId"))

instance Hashable
           ActivityTaskCancelRequestedEventAttributes
         where

instance NFData
           ActivityTaskCancelRequestedEventAttributes
         where

-- | Provides the details of the @ActivityTaskCanceled@ event.
--
--
--
-- /See:/ 'activityTaskCanceledEventAttributes' smart constructor.
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes'
  { _aLatestCancelRequestedEventId :: !(Maybe Integer)
  , _aDetails                      :: !(Maybe Text)
  , _aScheduledEventId             :: !Integer
  , _aStartedEventId               :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityTaskCanceledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aLatestCancelRequestedEventId' - If set, contains the ID of the last @ActivityTaskCancelRequested@ event recorded for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'aDetails' - Details of the cancellation.
--
-- * 'aScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'aStartedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskCanceledEventAttributes
    :: Integer -- ^ 'aScheduledEventId'
    -> Integer -- ^ 'aStartedEventId'
    -> ActivityTaskCanceledEventAttributes
activityTaskCanceledEventAttributes pScheduledEventId_ pStartedEventId_ =
  ActivityTaskCanceledEventAttributes'
    { _aLatestCancelRequestedEventId = Nothing
    , _aDetails = Nothing
    , _aScheduledEventId = pScheduledEventId_
    , _aStartedEventId = pStartedEventId_
    }


-- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event recorded for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
aLatestCancelRequestedEventId :: Lens' ActivityTaskCanceledEventAttributes (Maybe Integer)
aLatestCancelRequestedEventId = lens _aLatestCancelRequestedEventId (\ s a -> s{_aLatestCancelRequestedEventId = a})

-- | Details of the cancellation.
aDetails :: Lens' ActivityTaskCanceledEventAttributes (Maybe Text)
aDetails = lens _aDetails (\ s a -> s{_aDetails = a})

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
aScheduledEventId :: Lens' ActivityTaskCanceledEventAttributes Integer
aScheduledEventId = lens _aScheduledEventId (\ s a -> s{_aScheduledEventId = a})

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
aStartedEventId :: Lens' ActivityTaskCanceledEventAttributes Integer
aStartedEventId = lens _aStartedEventId (\ s a -> s{_aStartedEventId = a})

instance FromJSON ActivityTaskCanceledEventAttributes
         where
        parseJSON
          = withObject "ActivityTaskCanceledEventAttributes"
              (\ x ->
                 ActivityTaskCanceledEventAttributes' <$>
                   (x .:? "latestCancelRequestedEventId") <*>
                     (x .:? "details")
                     <*> (x .: "scheduledEventId")
                     <*> (x .: "startedEventId"))

instance Hashable ActivityTaskCanceledEventAttributes
         where

instance NFData ActivityTaskCanceledEventAttributes
         where

-- | Provides the details of the @ActivityTaskCompleted@ event.
--
--
--
-- /See:/ 'activityTaskCompletedEventAttributes' smart constructor.
data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes'
  { _atceaResult           :: !(Maybe Text)
  , _atceaScheduledEventId :: !Integer
  , _atceaStartedEventId   :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityTaskCompletedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atceaResult' - The results of the activity task.
--
-- * 'atceaScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'atceaStartedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskCompletedEventAttributes
    :: Integer -- ^ 'atceaScheduledEventId'
    -> Integer -- ^ 'atceaStartedEventId'
    -> ActivityTaskCompletedEventAttributes
activityTaskCompletedEventAttributes pScheduledEventId_ pStartedEventId_ =
  ActivityTaskCompletedEventAttributes'
    { _atceaResult = Nothing
    , _atceaScheduledEventId = pScheduledEventId_
    , _atceaStartedEventId = pStartedEventId_
    }


-- | The results of the activity task.
atceaResult :: Lens' ActivityTaskCompletedEventAttributes (Maybe Text)
atceaResult = lens _atceaResult (\ s a -> s{_atceaResult = a})

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atceaScheduledEventId :: Lens' ActivityTaskCompletedEventAttributes Integer
atceaScheduledEventId = lens _atceaScheduledEventId (\ s a -> s{_atceaScheduledEventId = a})

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atceaStartedEventId :: Lens' ActivityTaskCompletedEventAttributes Integer
atceaStartedEventId = lens _atceaStartedEventId (\ s a -> s{_atceaStartedEventId = a})

instance FromJSON
           ActivityTaskCompletedEventAttributes
         where
        parseJSON
          = withObject "ActivityTaskCompletedEventAttributes"
              (\ x ->
                 ActivityTaskCompletedEventAttributes' <$>
                   (x .:? "result") <*> (x .: "scheduledEventId") <*>
                     (x .: "startedEventId"))

instance Hashable
           ActivityTaskCompletedEventAttributes
         where

instance NFData ActivityTaskCompletedEventAttributes
         where

-- | Provides the details of the @ActivityTaskFailed@ event.
--
--
--
-- /See:/ 'activityTaskFailedEventAttributes' smart constructor.
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes'
  { _atfeaReason           :: !(Maybe Text)
  , _atfeaDetails          :: !(Maybe Text)
  , _atfeaScheduledEventId :: !Integer
  , _atfeaStartedEventId   :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityTaskFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atfeaReason' - The reason provided for the failure.
--
-- * 'atfeaDetails' - The details of the failure.
--
-- * 'atfeaScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'atfeaStartedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskFailedEventAttributes
    :: Integer -- ^ 'atfeaScheduledEventId'
    -> Integer -- ^ 'atfeaStartedEventId'
    -> ActivityTaskFailedEventAttributes
activityTaskFailedEventAttributes pScheduledEventId_ pStartedEventId_ =
  ActivityTaskFailedEventAttributes'
    { _atfeaReason = Nothing
    , _atfeaDetails = Nothing
    , _atfeaScheduledEventId = pScheduledEventId_
    , _atfeaStartedEventId = pStartedEventId_
    }


-- | The reason provided for the failure.
atfeaReason :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaReason = lens _atfeaReason (\ s a -> s{_atfeaReason = a})

-- | The details of the failure.
atfeaDetails :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaDetails = lens _atfeaDetails (\ s a -> s{_atfeaDetails = a})

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atfeaScheduledEventId :: Lens' ActivityTaskFailedEventAttributes Integer
atfeaScheduledEventId = lens _atfeaScheduledEventId (\ s a -> s{_atfeaScheduledEventId = a})

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atfeaStartedEventId :: Lens' ActivityTaskFailedEventAttributes Integer
atfeaStartedEventId = lens _atfeaStartedEventId (\ s a -> s{_atfeaStartedEventId = a})

instance FromJSON ActivityTaskFailedEventAttributes
         where
        parseJSON
          = withObject "ActivityTaskFailedEventAttributes"
              (\ x ->
                 ActivityTaskFailedEventAttributes' <$>
                   (x .:? "reason") <*> (x .:? "details") <*>
                     (x .: "scheduledEventId")
                     <*> (x .: "startedEventId"))

instance Hashable ActivityTaskFailedEventAttributes
         where

instance NFData ActivityTaskFailedEventAttributes
         where

-- | Provides the details of the @ActivityTaskScheduled@ event.
--
--
--
-- /See:/ 'activityTaskScheduledEventAttributes' smart constructor.
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes'
  { _atseaControl                      :: !(Maybe Text)
  , _atseaHeartbeatTimeout             :: !(Maybe Text)
  , _atseaScheduleToCloseTimeout       :: !(Maybe Text)
  , _atseaInput                        :: !(Maybe Text)
  , _atseaTaskPriority                 :: !(Maybe Text)
  , _atseaScheduleToStartTimeout       :: !(Maybe Text)
  , _atseaStartToCloseTimeout          :: !(Maybe Text)
  , _atseaActivityType                 :: !ActivityType
  , _atseaActivityId                   :: !Text
  , _atseaTaskList                     :: !TaskList
  , _atseaDecisionTaskCompletedEventId :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityTaskScheduledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atseaControl' - Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
--
-- * 'atseaHeartbeatTimeout' - The maximum time before which the worker processing this task must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or return a result, it is ignored.
--
-- * 'atseaScheduleToCloseTimeout' - The maximum amount of time for this activity task.
--
-- * 'atseaInput' - The input provided to the activity task.
--
-- * 'atseaTaskPriority' - The priority to assign to the scheduled activity task. If set, this overrides any default priority value that was assigned when the activity type was registered. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'atseaScheduleToStartTimeout' - The maximum amount of time the activity task can wait to be assigned to a worker.
--
-- * 'atseaStartToCloseTimeout' - The maximum amount of time a worker may take to process the activity task.
--
-- * 'atseaActivityType' - The type of the activity task.
--
-- * 'atseaActivityId' - The unique ID of the activity task.
--
-- * 'atseaTaskList' - The task list in which the activity task has been scheduled.
--
-- * 'atseaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision that resulted in the scheduling of this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskScheduledEventAttributes
    :: ActivityType -- ^ 'atseaActivityType'
    -> Text -- ^ 'atseaActivityId'
    -> TaskList -- ^ 'atseaTaskList'
    -> Integer -- ^ 'atseaDecisionTaskCompletedEventId'
    -> ActivityTaskScheduledEventAttributes
activityTaskScheduledEventAttributes pActivityType_ pActivityId_ pTaskList_ pDecisionTaskCompletedEventId_ =
  ActivityTaskScheduledEventAttributes'
    { _atseaControl = Nothing
    , _atseaHeartbeatTimeout = Nothing
    , _atseaScheduleToCloseTimeout = Nothing
    , _atseaInput = Nothing
    , _atseaTaskPriority = Nothing
    , _atseaScheduleToStartTimeout = Nothing
    , _atseaStartToCloseTimeout = Nothing
    , _atseaActivityType = pActivityType_
    , _atseaActivityId = pActivityId_
    , _atseaTaskList = pTaskList_
    , _atseaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId_
    }


-- | Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
atseaControl :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaControl = lens _atseaControl (\ s a -> s{_atseaControl = a})

-- | The maximum time before which the worker processing this task must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or return a result, it is ignored.
atseaHeartbeatTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaHeartbeatTimeout = lens _atseaHeartbeatTimeout (\ s a -> s{_atseaHeartbeatTimeout = a})

-- | The maximum amount of time for this activity task.
atseaScheduleToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToCloseTimeout = lens _atseaScheduleToCloseTimeout (\ s a -> s{_atseaScheduleToCloseTimeout = a})

-- | The input provided to the activity task.
atseaInput :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaInput = lens _atseaInput (\ s a -> s{_atseaInput = a})

-- | The priority to assign to the scheduled activity task. If set, this overrides any default priority value that was assigned when the activity type was registered. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
atseaTaskPriority :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaTaskPriority = lens _atseaTaskPriority (\ s a -> s{_atseaTaskPriority = a})

-- | The maximum amount of time the activity task can wait to be assigned to a worker.
atseaScheduleToStartTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToStartTimeout = lens _atseaScheduleToStartTimeout (\ s a -> s{_atseaScheduleToStartTimeout = a})

-- | The maximum amount of time a worker may take to process the activity task.
atseaStartToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaStartToCloseTimeout = lens _atseaStartToCloseTimeout (\ s a -> s{_atseaStartToCloseTimeout = a})

-- | The type of the activity task.
atseaActivityType :: Lens' ActivityTaskScheduledEventAttributes ActivityType
atseaActivityType = lens _atseaActivityType (\ s a -> s{_atseaActivityType = a})

-- | The unique ID of the activity task.
atseaActivityId :: Lens' ActivityTaskScheduledEventAttributes Text
atseaActivityId = lens _atseaActivityId (\ s a -> s{_atseaActivityId = a})

-- | The task list in which the activity task has been scheduled.
atseaTaskList :: Lens' ActivityTaskScheduledEventAttributes TaskList
atseaTaskList = lens _atseaTaskList (\ s a -> s{_atseaTaskList = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision that resulted in the scheduling of this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atseaDecisionTaskCompletedEventId :: Lens' ActivityTaskScheduledEventAttributes Integer
atseaDecisionTaskCompletedEventId = lens _atseaDecisionTaskCompletedEventId (\ s a -> s{_atseaDecisionTaskCompletedEventId = a})

instance FromJSON
           ActivityTaskScheduledEventAttributes
         where
        parseJSON
          = withObject "ActivityTaskScheduledEventAttributes"
              (\ x ->
                 ActivityTaskScheduledEventAttributes' <$>
                   (x .:? "control") <*> (x .:? "heartbeatTimeout") <*>
                     (x .:? "scheduleToCloseTimeout")
                     <*> (x .:? "input")
                     <*> (x .:? "taskPriority")
                     <*> (x .:? "scheduleToStartTimeout")
                     <*> (x .:? "startToCloseTimeout")
                     <*> (x .: "activityType")
                     <*> (x .: "activityId")
                     <*> (x .: "taskList")
                     <*> (x .: "decisionTaskCompletedEventId"))

instance Hashable
           ActivityTaskScheduledEventAttributes
         where

instance NFData ActivityTaskScheduledEventAttributes
         where

-- | Provides the details of the @ActivityTaskStarted@ event.
--
--
--
-- /See:/ 'activityTaskStartedEventAttributes' smart constructor.
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes'
  { _atseaIdentity         :: !(Maybe Text)
  , _atseaScheduledEventId :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityTaskStartedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atseaIdentity' - Identity of the worker that was assigned this task. This aids diagnostics when problems arise. The form of this identity is user defined.
--
-- * 'atseaScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskStartedEventAttributes
    :: Integer -- ^ 'atseaScheduledEventId'
    -> ActivityTaskStartedEventAttributes
activityTaskStartedEventAttributes pScheduledEventId_ =
  ActivityTaskStartedEventAttributes'
    {_atseaIdentity = Nothing, _atseaScheduledEventId = pScheduledEventId_}


-- | Identity of the worker that was assigned this task. This aids diagnostics when problems arise. The form of this identity is user defined.
atseaIdentity :: Lens' ActivityTaskStartedEventAttributes (Maybe Text)
atseaIdentity = lens _atseaIdentity (\ s a -> s{_atseaIdentity = a})

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atseaScheduledEventId :: Lens' ActivityTaskStartedEventAttributes Integer
atseaScheduledEventId = lens _atseaScheduledEventId (\ s a -> s{_atseaScheduledEventId = a})

instance FromJSON ActivityTaskStartedEventAttributes
         where
        parseJSON
          = withObject "ActivityTaskStartedEventAttributes"
              (\ x ->
                 ActivityTaskStartedEventAttributes' <$>
                   (x .:? "identity") <*> (x .: "scheduledEventId"))

instance Hashable ActivityTaskStartedEventAttributes
         where

instance NFData ActivityTaskStartedEventAttributes
         where

-- | Provides the details of the @ActivityTaskTimedOut@ event.
--
--
--
-- /See:/ 'activityTaskTimedOutEventAttributes' smart constructor.
data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes'
  { _attoeaDetails          :: !(Maybe Text)
  , _attoeaTimeoutType      :: !ActivityTaskTimeoutType
  , _attoeaScheduledEventId :: !Integer
  , _attoeaStartedEventId   :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityTaskTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attoeaDetails' - Contains the content of the @details@ parameter for the last call made by the activity to @RecordActivityTaskHeartbeat@ .
--
-- * 'attoeaTimeoutType' - The type of the timeout that caused this event.
--
-- * 'attoeaScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'attoeaStartedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskTimedOutEventAttributes
    :: ActivityTaskTimeoutType -- ^ 'attoeaTimeoutType'
    -> Integer -- ^ 'attoeaScheduledEventId'
    -> Integer -- ^ 'attoeaStartedEventId'
    -> ActivityTaskTimedOutEventAttributes
activityTaskTimedOutEventAttributes pTimeoutType_ pScheduledEventId_ pStartedEventId_ =
  ActivityTaskTimedOutEventAttributes'
    { _attoeaDetails = Nothing
    , _attoeaTimeoutType = pTimeoutType_
    , _attoeaScheduledEventId = pScheduledEventId_
    , _attoeaStartedEventId = pStartedEventId_
    }


-- | Contains the content of the @details@ parameter for the last call made by the activity to @RecordActivityTaskHeartbeat@ .
attoeaDetails :: Lens' ActivityTaskTimedOutEventAttributes (Maybe Text)
attoeaDetails = lens _attoeaDetails (\ s a -> s{_attoeaDetails = a})

-- | The type of the timeout that caused this event.
attoeaTimeoutType :: Lens' ActivityTaskTimedOutEventAttributes ActivityTaskTimeoutType
attoeaTimeoutType = lens _attoeaTimeoutType (\ s a -> s{_attoeaTimeoutType = a})

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
attoeaScheduledEventId :: Lens' ActivityTaskTimedOutEventAttributes Integer
attoeaScheduledEventId = lens _attoeaScheduledEventId (\ s a -> s{_attoeaScheduledEventId = a})

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
attoeaStartedEventId :: Lens' ActivityTaskTimedOutEventAttributes Integer
attoeaStartedEventId = lens _attoeaStartedEventId (\ s a -> s{_attoeaStartedEventId = a})

instance FromJSON ActivityTaskTimedOutEventAttributes
         where
        parseJSON
          = withObject "ActivityTaskTimedOutEventAttributes"
              (\ x ->
                 ActivityTaskTimedOutEventAttributes' <$>
                   (x .:? "details") <*> (x .: "timeoutType") <*>
                     (x .: "scheduledEventId")
                     <*> (x .: "startedEventId"))

instance Hashable ActivityTaskTimedOutEventAttributes
         where

instance NFData ActivityTaskTimedOutEventAttributes
         where

-- | Represents an activity type.
--
--
--
-- /See:/ 'activityType' smart constructor.
data ActivityType = ActivityType'
  { _atName    :: !Text
  , _atVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atName' - The name of this activity.
--
-- * 'atVersion' - The version of this activity.
activityType
    :: Text -- ^ 'atName'
    -> Text -- ^ 'atVersion'
    -> ActivityType
activityType pName_ pVersion_ =
  ActivityType' {_atName = pName_, _atVersion = pVersion_}


-- | The name of this activity.
atName :: Lens' ActivityType Text
atName = lens _atName (\ s a -> s{_atName = a})

-- | The version of this activity.
atVersion :: Lens' ActivityType Text
atVersion = lens _atVersion (\ s a -> s{_atVersion = a})

instance FromJSON ActivityType where
        parseJSON
          = withObject "ActivityType"
              (\ x ->
                 ActivityType' <$> (x .: "name") <*> (x .: "version"))

instance Hashable ActivityType where

instance NFData ActivityType where

instance ToJSON ActivityType where
        toJSON ActivityType'{..}
          = object
              (catMaybes
                 [Just ("name" .= _atName),
                  Just ("version" .= _atVersion)])

-- | Configuration settings registered with the activity type.
--
--
--
-- /See:/ 'activityTypeConfiguration' smart constructor.
data ActivityTypeConfiguration = ActivityTypeConfiguration'
  { _atcDefaultTaskScheduleToStartTimeout :: !(Maybe Text)
  , _atcDefaultTaskList                   :: !(Maybe TaskList)
  , _atcDefaultTaskPriority               :: !(Maybe Text)
  , _atcDefaultTaskHeartbeatTimeout       :: !(Maybe Text)
  , _atcDefaultTaskScheduleToCloseTimeout :: !(Maybe Text)
  , _atcDefaultTaskStartToCloseTimeout    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityTypeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atcDefaultTaskScheduleToStartTimeout' - The default maximum duration, specified when registering the activity type, that a task of an activity type can wait before being assigned to a worker. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'atcDefaultTaskList' - The default task list specified for this activity type at registration. This default is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' . You can override the default registered task list when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- * 'atcDefaultTaskPriority' - The default task priority for tasks of this activity type, specified at registration. If not set, then @0@ is used as the default priority. This default can be overridden when scheduling an activity task. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'atcDefaultTaskHeartbeatTimeout' - The default maximum time, in seconds, before which a worker processing a task must report progress by calling 'RecordActivityTaskHeartbeat' . You can specify this value only when /registering/ an activity type. The registered default value can be overridden when you schedule a task through the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'atcDefaultTaskScheduleToCloseTimeout' - The default maximum duration, specified when registering the activity type, for tasks of this activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'atcDefaultTaskStartToCloseTimeout' - The default maximum duration for tasks of an activity type specified when registering the activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
activityTypeConfiguration
    :: ActivityTypeConfiguration
activityTypeConfiguration =
  ActivityTypeConfiguration'
    { _atcDefaultTaskScheduleToStartTimeout = Nothing
    , _atcDefaultTaskList = Nothing
    , _atcDefaultTaskPriority = Nothing
    , _atcDefaultTaskHeartbeatTimeout = Nothing
    , _atcDefaultTaskScheduleToCloseTimeout = Nothing
    , _atcDefaultTaskStartToCloseTimeout = Nothing
    }


-- | The default maximum duration, specified when registering the activity type, that a task of an activity type can wait before being assigned to a worker. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
atcDefaultTaskScheduleToStartTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToStartTimeout = lens _atcDefaultTaskScheduleToStartTimeout (\ s a -> s{_atcDefaultTaskScheduleToStartTimeout = a})

-- | The default task list specified for this activity type at registration. This default is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' . You can override the default registered task list when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
atcDefaultTaskList :: Lens' ActivityTypeConfiguration (Maybe TaskList)
atcDefaultTaskList = lens _atcDefaultTaskList (\ s a -> s{_atcDefaultTaskList = a})

-- | The default task priority for tasks of this activity type, specified at registration. If not set, then @0@ is used as the default priority. This default can be overridden when scheduling an activity task. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
atcDefaultTaskPriority :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskPriority = lens _atcDefaultTaskPriority (\ s a -> s{_atcDefaultTaskPriority = a})

-- | The default maximum time, in seconds, before which a worker processing a task must report progress by calling 'RecordActivityTaskHeartbeat' . You can specify this value only when /registering/ an activity type. The registered default value can be overridden when you schedule a task through the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
atcDefaultTaskHeartbeatTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskHeartbeatTimeout = lens _atcDefaultTaskHeartbeatTimeout (\ s a -> s{_atcDefaultTaskHeartbeatTimeout = a})

-- | The default maximum duration, specified when registering the activity type, for tasks of this activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
atcDefaultTaskScheduleToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToCloseTimeout = lens _atcDefaultTaskScheduleToCloseTimeout (\ s a -> s{_atcDefaultTaskScheduleToCloseTimeout = a})

-- | The default maximum duration for tasks of an activity type specified when registering the activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
atcDefaultTaskStartToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskStartToCloseTimeout = lens _atcDefaultTaskStartToCloseTimeout (\ s a -> s{_atcDefaultTaskStartToCloseTimeout = a})

instance FromJSON ActivityTypeConfiguration where
        parseJSON
          = withObject "ActivityTypeConfiguration"
              (\ x ->
                 ActivityTypeConfiguration' <$>
                   (x .:? "defaultTaskScheduleToStartTimeout") <*>
                     (x .:? "defaultTaskList")
                     <*> (x .:? "defaultTaskPriority")
                     <*> (x .:? "defaultTaskHeartbeatTimeout")
                     <*> (x .:? "defaultTaskScheduleToCloseTimeout")
                     <*> (x .:? "defaultTaskStartToCloseTimeout"))

instance Hashable ActivityTypeConfiguration where

instance NFData ActivityTypeConfiguration where

-- | Detailed information about an activity type.
--
--
--
-- /See:/ 'activityTypeInfo' smart constructor.
data ActivityTypeInfo = ActivityTypeInfo'
  { _atiDeprecationDate :: !(Maybe POSIX)
  , _atiDescription     :: !(Maybe Text)
  , _atiActivityType    :: !ActivityType
  , _atiStatus          :: !RegistrationStatus
  , _atiCreationDate    :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityTypeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atiDeprecationDate' - If DEPRECATED, the date and time 'DeprecateActivityType' was called.
--
-- * 'atiDescription' - The description of the activity type provided in 'RegisterActivityType' .
--
-- * 'atiActivityType' - The 'ActivityType' type structure representing the activity type.
--
-- * 'atiStatus' - The current status of the activity type.
--
-- * 'atiCreationDate' - The date and time this activity type was created through 'RegisterActivityType' .
activityTypeInfo
    :: ActivityType -- ^ 'atiActivityType'
    -> RegistrationStatus -- ^ 'atiStatus'
    -> UTCTime -- ^ 'atiCreationDate'
    -> ActivityTypeInfo
activityTypeInfo pActivityType_ pStatus_ pCreationDate_ =
  ActivityTypeInfo'
    { _atiDeprecationDate = Nothing
    , _atiDescription = Nothing
    , _atiActivityType = pActivityType_
    , _atiStatus = pStatus_
    , _atiCreationDate = _Time # pCreationDate_
    }


-- | If DEPRECATED, the date and time 'DeprecateActivityType' was called.
atiDeprecationDate :: Lens' ActivityTypeInfo (Maybe UTCTime)
atiDeprecationDate = lens _atiDeprecationDate (\ s a -> s{_atiDeprecationDate = a}) . mapping _Time

-- | The description of the activity type provided in 'RegisterActivityType' .
atiDescription :: Lens' ActivityTypeInfo (Maybe Text)
atiDescription = lens _atiDescription (\ s a -> s{_atiDescription = a})

-- | The 'ActivityType' type structure representing the activity type.
atiActivityType :: Lens' ActivityTypeInfo ActivityType
atiActivityType = lens _atiActivityType (\ s a -> s{_atiActivityType = a})

-- | The current status of the activity type.
atiStatus :: Lens' ActivityTypeInfo RegistrationStatus
atiStatus = lens _atiStatus (\ s a -> s{_atiStatus = a})

-- | The date and time this activity type was created through 'RegisterActivityType' .
atiCreationDate :: Lens' ActivityTypeInfo UTCTime
atiCreationDate = lens _atiCreationDate (\ s a -> s{_atiCreationDate = a}) . _Time

instance FromJSON ActivityTypeInfo where
        parseJSON
          = withObject "ActivityTypeInfo"
              (\ x ->
                 ActivityTypeInfo' <$>
                   (x .:? "deprecationDate") <*> (x .:? "description")
                     <*> (x .: "activityType")
                     <*> (x .: "status")
                     <*> (x .: "creationDate"))

instance Hashable ActivityTypeInfo where

instance NFData ActivityTypeInfo where

-- | Provides the details of the @CancelTimer@ decision.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
--
-- /See:/ 'cancelTimerDecisionAttributes' smart constructor.
newtype CancelTimerDecisionAttributes = CancelTimerDecisionAttributes'
  { _ctdaTimerId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelTimerDecisionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctdaTimerId' - The unique ID of the timer to cancel.
cancelTimerDecisionAttributes
    :: Text -- ^ 'ctdaTimerId'
    -> CancelTimerDecisionAttributes
cancelTimerDecisionAttributes pTimerId_ =
  CancelTimerDecisionAttributes' {_ctdaTimerId = pTimerId_}


-- | The unique ID of the timer to cancel.
ctdaTimerId :: Lens' CancelTimerDecisionAttributes Text
ctdaTimerId = lens _ctdaTimerId (\ s a -> s{_ctdaTimerId = a})

instance Hashable CancelTimerDecisionAttributes where

instance NFData CancelTimerDecisionAttributes where

instance ToJSON CancelTimerDecisionAttributes where
        toJSON CancelTimerDecisionAttributes'{..}
          = object
              (catMaybes [Just ("timerId" .= _ctdaTimerId)])

-- | Provides the details of the @CancelTimerFailed@ event.
--
--
--
-- /See:/ 'cancelTimerFailedEventAttributes' smart constructor.
data CancelTimerFailedEventAttributes = CancelTimerFailedEventAttributes'
  { _ctfeaTimerId                      :: !Text
  , _ctfeaCause                        :: !CancelTimerFailedCause
  , _ctfeaDecisionTaskCompletedEventId :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelTimerFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctfeaTimerId' - The timerId provided in the @CancelTimer@ decision that failed.
--
-- * 'ctfeaCause' - The cause of the failure. This information is generated by the system and can be useful for diagnostic purposes.
--
-- * 'ctfeaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelTimer@ decision to cancel this timer. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cancelTimerFailedEventAttributes
    :: Text -- ^ 'ctfeaTimerId'
    -> CancelTimerFailedCause -- ^ 'ctfeaCause'
    -> Integer -- ^ 'ctfeaDecisionTaskCompletedEventId'
    -> CancelTimerFailedEventAttributes
cancelTimerFailedEventAttributes pTimerId_ pCause_ pDecisionTaskCompletedEventId_ =
  CancelTimerFailedEventAttributes'
    { _ctfeaTimerId = pTimerId_
    , _ctfeaCause = pCause_
    , _ctfeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId_
    }


-- | The timerId provided in the @CancelTimer@ decision that failed.
ctfeaTimerId :: Lens' CancelTimerFailedEventAttributes Text
ctfeaTimerId = lens _ctfeaTimerId (\ s a -> s{_ctfeaTimerId = a})

-- | The cause of the failure. This information is generated by the system and can be useful for diagnostic purposes.
ctfeaCause :: Lens' CancelTimerFailedEventAttributes CancelTimerFailedCause
ctfeaCause = lens _ctfeaCause (\ s a -> s{_ctfeaCause = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelTimer@ decision to cancel this timer. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
ctfeaDecisionTaskCompletedEventId :: Lens' CancelTimerFailedEventAttributes Integer
ctfeaDecisionTaskCompletedEventId = lens _ctfeaDecisionTaskCompletedEventId (\ s a -> s{_ctfeaDecisionTaskCompletedEventId = a})

instance FromJSON CancelTimerFailedEventAttributes
         where
        parseJSON
          = withObject "CancelTimerFailedEventAttributes"
              (\ x ->
                 CancelTimerFailedEventAttributes' <$>
                   (x .: "timerId") <*> (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

instance Hashable CancelTimerFailedEventAttributes
         where

instance NFData CancelTimerFailedEventAttributes
         where

-- | Provides the details of the @CancelWorkflowExecution@ decision.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
--
-- /See:/ 'cancelWorkflowExecutionDecisionAttributes' smart constructor.
newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes'
  { _cwedaDetails :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwedaDetails' - Details of the cancellation.
cancelWorkflowExecutionDecisionAttributes
    :: CancelWorkflowExecutionDecisionAttributes
cancelWorkflowExecutionDecisionAttributes =
  CancelWorkflowExecutionDecisionAttributes' {_cwedaDetails = Nothing}


-- | Details of the cancellation.
cwedaDetails :: Lens' CancelWorkflowExecutionDecisionAttributes (Maybe Text)
cwedaDetails = lens _cwedaDetails (\ s a -> s{_cwedaDetails = a})

instance Hashable
           CancelWorkflowExecutionDecisionAttributes
         where

instance NFData
           CancelWorkflowExecutionDecisionAttributes
         where

instance ToJSON
           CancelWorkflowExecutionDecisionAttributes
         where
        toJSON CancelWorkflowExecutionDecisionAttributes'{..}
          = object
              (catMaybes [("details" .=) <$> _cwedaDetails])

-- | Provides the details of the @CancelWorkflowExecutionFailed@ event.
--
--
--
-- /See:/ 'cancelWorkflowExecutionFailedEventAttributes' smart constructor.
data CancelWorkflowExecutionFailedEventAttributes = CancelWorkflowExecutionFailedEventAttributes'
  { _cCause                        :: !CancelWorkflowExecutionFailedCause
  , _cDecisionTaskCompletedEventId :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelWorkflowExecutionFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCause' - The cause of the failure. This information is generated by the system and can be useful for diagnostic purposes.
--
-- * 'cDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cancelWorkflowExecutionFailedEventAttributes
    :: CancelWorkflowExecutionFailedCause -- ^ 'cCause'
    -> Integer -- ^ 'cDecisionTaskCompletedEventId'
    -> CancelWorkflowExecutionFailedEventAttributes
cancelWorkflowExecutionFailedEventAttributes pCause_ pDecisionTaskCompletedEventId_ =
  CancelWorkflowExecutionFailedEventAttributes'
    { _cCause = pCause_
    , _cDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId_
    }


-- | The cause of the failure. This information is generated by the system and can be useful for diagnostic purposes.
cCause :: Lens' CancelWorkflowExecutionFailedEventAttributes CancelWorkflowExecutionFailedCause
cCause = lens _cCause (\ s a -> s{_cCause = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cDecisionTaskCompletedEventId :: Lens' CancelWorkflowExecutionFailedEventAttributes Integer
cDecisionTaskCompletedEventId = lens _cDecisionTaskCompletedEventId (\ s a -> s{_cDecisionTaskCompletedEventId = a})

instance FromJSON
           CancelWorkflowExecutionFailedEventAttributes
         where
        parseJSON
          = withObject
              "CancelWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 CancelWorkflowExecutionFailedEventAttributes' <$>
                   (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

instance Hashable
           CancelWorkflowExecutionFailedEventAttributes
         where

instance NFData
           CancelWorkflowExecutionFailedEventAttributes
         where

-- | Provide details of the @ChildWorkflowExecutionCanceled@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionCanceledEventAttributes' smart constructor.
data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes'
  { _cDetails           :: !(Maybe Text)
  , _cWorkflowExecution :: !WorkflowExecution
  , _cWorkflowType      :: !WorkflowType
  , _cInitiatedEventId  :: !Integer
  , _cStartedEventId    :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChildWorkflowExecutionCanceledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cDetails' - Details of the cancellation (if provided).
--
-- * 'cWorkflowExecution' - The child workflow execution that was canceled.
--
-- * 'cWorkflowType' - The type of the child workflow execution.
--
-- * 'cInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'cStartedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionCanceledEventAttributes
    :: WorkflowExecution -- ^ 'cWorkflowExecution'
    -> WorkflowType -- ^ 'cWorkflowType'
    -> Integer -- ^ 'cInitiatedEventId'
    -> Integer -- ^ 'cStartedEventId'
    -> ChildWorkflowExecutionCanceledEventAttributes
childWorkflowExecutionCanceledEventAttributes pWorkflowExecution_ pWorkflowType_ pInitiatedEventId_ pStartedEventId_ =
  ChildWorkflowExecutionCanceledEventAttributes'
    { _cDetails = Nothing
    , _cWorkflowExecution = pWorkflowExecution_
    , _cWorkflowType = pWorkflowType_
    , _cInitiatedEventId = pInitiatedEventId_
    , _cStartedEventId = pStartedEventId_
    }


-- | Details of the cancellation (if provided).
cDetails :: Lens' ChildWorkflowExecutionCanceledEventAttributes (Maybe Text)
cDetails = lens _cDetails (\ s a -> s{_cDetails = a})

-- | The child workflow execution that was canceled.
cWorkflowExecution :: Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowExecution
cWorkflowExecution = lens _cWorkflowExecution (\ s a -> s{_cWorkflowExecution = a})

-- | The type of the child workflow execution.
cWorkflowType :: Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowType
cWorkflowType = lens _cWorkflowType (\ s a -> s{_cWorkflowType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cInitiatedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes Integer
cInitiatedEventId = lens _cInitiatedEventId (\ s a -> s{_cInitiatedEventId = a})

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cStartedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes Integer
cStartedEventId = lens _cStartedEventId (\ s a -> s{_cStartedEventId = a})

instance FromJSON
           ChildWorkflowExecutionCanceledEventAttributes
         where
        parseJSON
          = withObject
              "ChildWorkflowExecutionCanceledEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionCanceledEventAttributes' <$>
                   (x .:? "details") <*> (x .: "workflowExecution") <*>
                     (x .: "workflowType")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "startedEventId"))

instance Hashable
           ChildWorkflowExecutionCanceledEventAttributes
         where

instance NFData
           ChildWorkflowExecutionCanceledEventAttributes
         where

-- | Provides the details of the @ChildWorkflowExecutionCompleted@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionCompletedEventAttributes' smart constructor.
data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes'
  { _cweceaResult            :: !(Maybe Text)
  , _cweceaWorkflowExecution :: !WorkflowExecution
  , _cweceaWorkflowType      :: !WorkflowType
  , _cweceaInitiatedEventId  :: !Integer
  , _cweceaStartedEventId    :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChildWorkflowExecutionCompletedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cweceaResult' - The result of the child workflow execution.
--
-- * 'cweceaWorkflowExecution' - The child workflow execution that was completed.
--
-- * 'cweceaWorkflowType' - The type of the child workflow execution.
--
-- * 'cweceaInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'cweceaStartedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionCompletedEventAttributes
    :: WorkflowExecution -- ^ 'cweceaWorkflowExecution'
    -> WorkflowType -- ^ 'cweceaWorkflowType'
    -> Integer -- ^ 'cweceaInitiatedEventId'
    -> Integer -- ^ 'cweceaStartedEventId'
    -> ChildWorkflowExecutionCompletedEventAttributes
childWorkflowExecutionCompletedEventAttributes pWorkflowExecution_ pWorkflowType_ pInitiatedEventId_ pStartedEventId_ =
  ChildWorkflowExecutionCompletedEventAttributes'
    { _cweceaResult = Nothing
    , _cweceaWorkflowExecution = pWorkflowExecution_
    , _cweceaWorkflowType = pWorkflowType_
    , _cweceaInitiatedEventId = pInitiatedEventId_
    , _cweceaStartedEventId = pStartedEventId_
    }


-- | The result of the child workflow execution.
cweceaResult :: Lens' ChildWorkflowExecutionCompletedEventAttributes (Maybe Text)
cweceaResult = lens _cweceaResult (\ s a -> s{_cweceaResult = a})

-- | The child workflow execution that was completed.
cweceaWorkflowExecution :: Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowExecution
cweceaWorkflowExecution = lens _cweceaWorkflowExecution (\ s a -> s{_cweceaWorkflowExecution = a})

-- | The type of the child workflow execution.
cweceaWorkflowType :: Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowType
cweceaWorkflowType = lens _cweceaWorkflowType (\ s a -> s{_cweceaWorkflowType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cweceaInitiatedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes Integer
cweceaInitiatedEventId = lens _cweceaInitiatedEventId (\ s a -> s{_cweceaInitiatedEventId = a})

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cweceaStartedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes Integer
cweceaStartedEventId = lens _cweceaStartedEventId (\ s a -> s{_cweceaStartedEventId = a})

instance FromJSON
           ChildWorkflowExecutionCompletedEventAttributes
         where
        parseJSON
          = withObject
              "ChildWorkflowExecutionCompletedEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionCompletedEventAttributes' <$>
                   (x .:? "result") <*> (x .: "workflowExecution") <*>
                     (x .: "workflowType")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "startedEventId"))

instance Hashable
           ChildWorkflowExecutionCompletedEventAttributes
         where

instance NFData
           ChildWorkflowExecutionCompletedEventAttributes
         where

-- | Provides the details of the @ChildWorkflowExecutionFailed@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionFailedEventAttributes' smart constructor.
data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes'
  { _cwefeaReason            :: !(Maybe Text)
  , _cwefeaDetails           :: !(Maybe Text)
  , _cwefeaWorkflowExecution :: !WorkflowExecution
  , _cwefeaWorkflowType      :: !WorkflowType
  , _cwefeaInitiatedEventId  :: !Integer
  , _cwefeaStartedEventId    :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChildWorkflowExecutionFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwefeaReason' - The reason for the failure (if provided).
--
-- * 'cwefeaDetails' - The details of the failure (if provided).
--
-- * 'cwefeaWorkflowExecution' - The child workflow execution that failed.
--
-- * 'cwefeaWorkflowType' - The type of the child workflow execution.
--
-- * 'cwefeaInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'cwefeaStartedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionFailedEventAttributes
    :: WorkflowExecution -- ^ 'cwefeaWorkflowExecution'
    -> WorkflowType -- ^ 'cwefeaWorkflowType'
    -> Integer -- ^ 'cwefeaInitiatedEventId'
    -> Integer -- ^ 'cwefeaStartedEventId'
    -> ChildWorkflowExecutionFailedEventAttributes
childWorkflowExecutionFailedEventAttributes pWorkflowExecution_ pWorkflowType_ pInitiatedEventId_ pStartedEventId_ =
  ChildWorkflowExecutionFailedEventAttributes'
    { _cwefeaReason = Nothing
    , _cwefeaDetails = Nothing
    , _cwefeaWorkflowExecution = pWorkflowExecution_
    , _cwefeaWorkflowType = pWorkflowType_
    , _cwefeaInitiatedEventId = pInitiatedEventId_
    , _cwefeaStartedEventId = pStartedEventId_
    }


-- | The reason for the failure (if provided).
cwefeaReason :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefeaReason = lens _cwefeaReason (\ s a -> s{_cwefeaReason = a})

-- | The details of the failure (if provided).
cwefeaDetails :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefeaDetails = lens _cwefeaDetails (\ s a -> s{_cwefeaDetails = a})

-- | The child workflow execution that failed.
cwefeaWorkflowExecution :: Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowExecution
cwefeaWorkflowExecution = lens _cwefeaWorkflowExecution (\ s a -> s{_cwefeaWorkflowExecution = a})

-- | The type of the child workflow execution.
cwefeaWorkflowType :: Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowType
cwefeaWorkflowType = lens _cwefeaWorkflowType (\ s a -> s{_cwefeaWorkflowType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cwefeaInitiatedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes Integer
cwefeaInitiatedEventId = lens _cwefeaInitiatedEventId (\ s a -> s{_cwefeaInitiatedEventId = a})

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cwefeaStartedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes Integer
cwefeaStartedEventId = lens _cwefeaStartedEventId (\ s a -> s{_cwefeaStartedEventId = a})

instance FromJSON
           ChildWorkflowExecutionFailedEventAttributes
         where
        parseJSON
          = withObject
              "ChildWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionFailedEventAttributes' <$>
                   (x .:? "reason") <*> (x .:? "details") <*>
                     (x .: "workflowExecution")
                     <*> (x .: "workflowType")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "startedEventId"))

instance Hashable
           ChildWorkflowExecutionFailedEventAttributes
         where

instance NFData
           ChildWorkflowExecutionFailedEventAttributes
         where

-- | Provides the details of the @ChildWorkflowExecutionStarted@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionStartedEventAttributes' smart constructor.
data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes'
  { _cweseaWorkflowExecution :: !WorkflowExecution
  , _cweseaWorkflowType      :: !WorkflowType
  , _cweseaInitiatedEventId  :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChildWorkflowExecutionStartedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cweseaWorkflowExecution' - The child workflow execution that was started.
--
-- * 'cweseaWorkflowType' - The type of the child workflow execution.
--
-- * 'cweseaInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionStartedEventAttributes
    :: WorkflowExecution -- ^ 'cweseaWorkflowExecution'
    -> WorkflowType -- ^ 'cweseaWorkflowType'
    -> Integer -- ^ 'cweseaInitiatedEventId'
    -> ChildWorkflowExecutionStartedEventAttributes
childWorkflowExecutionStartedEventAttributes pWorkflowExecution_ pWorkflowType_ pInitiatedEventId_ =
  ChildWorkflowExecutionStartedEventAttributes'
    { _cweseaWorkflowExecution = pWorkflowExecution_
    , _cweseaWorkflowType = pWorkflowType_
    , _cweseaInitiatedEventId = pInitiatedEventId_
    }


-- | The child workflow execution that was started.
cweseaWorkflowExecution :: Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowExecution
cweseaWorkflowExecution = lens _cweseaWorkflowExecution (\ s a -> s{_cweseaWorkflowExecution = a})

-- | The type of the child workflow execution.
cweseaWorkflowType :: Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowType
cweseaWorkflowType = lens _cweseaWorkflowType (\ s a -> s{_cweseaWorkflowType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cweseaInitiatedEventId :: Lens' ChildWorkflowExecutionStartedEventAttributes Integer
cweseaInitiatedEventId = lens _cweseaInitiatedEventId (\ s a -> s{_cweseaInitiatedEventId = a})

instance FromJSON
           ChildWorkflowExecutionStartedEventAttributes
         where
        parseJSON
          = withObject
              "ChildWorkflowExecutionStartedEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionStartedEventAttributes' <$>
                   (x .: "workflowExecution") <*> (x .: "workflowType")
                     <*> (x .: "initiatedEventId"))

instance Hashable
           ChildWorkflowExecutionStartedEventAttributes
         where

instance NFData
           ChildWorkflowExecutionStartedEventAttributes
         where

-- | Provides the details of the @ChildWorkflowExecutionTerminated@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionTerminatedEventAttributes' smart constructor.
data ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes'
  { _cweteaWorkflowExecution :: !WorkflowExecution
  , _cweteaWorkflowType      :: !WorkflowType
  , _cweteaInitiatedEventId  :: !Integer
  , _cweteaStartedEventId    :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChildWorkflowExecutionTerminatedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cweteaWorkflowExecution' - The child workflow execution that was terminated.
--
-- * 'cweteaWorkflowType' - The type of the child workflow execution.
--
-- * 'cweteaInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'cweteaStartedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionTerminatedEventAttributes
    :: WorkflowExecution -- ^ 'cweteaWorkflowExecution'
    -> WorkflowType -- ^ 'cweteaWorkflowType'
    -> Integer -- ^ 'cweteaInitiatedEventId'
    -> Integer -- ^ 'cweteaStartedEventId'
    -> ChildWorkflowExecutionTerminatedEventAttributes
childWorkflowExecutionTerminatedEventAttributes pWorkflowExecution_ pWorkflowType_ pInitiatedEventId_ pStartedEventId_ =
  ChildWorkflowExecutionTerminatedEventAttributes'
    { _cweteaWorkflowExecution = pWorkflowExecution_
    , _cweteaWorkflowType = pWorkflowType_
    , _cweteaInitiatedEventId = pInitiatedEventId_
    , _cweteaStartedEventId = pStartedEventId_
    }


-- | The child workflow execution that was terminated.
cweteaWorkflowExecution :: Lens' ChildWorkflowExecutionTerminatedEventAttributes WorkflowExecution
cweteaWorkflowExecution = lens _cweteaWorkflowExecution (\ s a -> s{_cweteaWorkflowExecution = a})

-- | The type of the child workflow execution.
cweteaWorkflowType :: Lens' ChildWorkflowExecutionTerminatedEventAttributes WorkflowType
cweteaWorkflowType = lens _cweteaWorkflowType (\ s a -> s{_cweteaWorkflowType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cweteaInitiatedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes Integer
cweteaInitiatedEventId = lens _cweteaInitiatedEventId (\ s a -> s{_cweteaInitiatedEventId = a})

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cweteaStartedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes Integer
cweteaStartedEventId = lens _cweteaStartedEventId (\ s a -> s{_cweteaStartedEventId = a})

instance FromJSON
           ChildWorkflowExecutionTerminatedEventAttributes
         where
        parseJSON
          = withObject
              "ChildWorkflowExecutionTerminatedEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionTerminatedEventAttributes' <$>
                   (x .: "workflowExecution") <*> (x .: "workflowType")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "startedEventId"))

instance Hashable
           ChildWorkflowExecutionTerminatedEventAttributes
         where

instance NFData
           ChildWorkflowExecutionTerminatedEventAttributes
         where

-- | Provides the details of the @ChildWorkflowExecutionTimedOut@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionTimedOutEventAttributes' smart constructor.
data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes'
  { _cwetoeaWorkflowExecution :: !WorkflowExecution
  , _cwetoeaWorkflowType      :: !WorkflowType
  , _cwetoeaTimeoutType       :: !WorkflowExecutionTimeoutType
  , _cwetoeaInitiatedEventId  :: !Integer
  , _cwetoeaStartedEventId    :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChildWorkflowExecutionTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwetoeaWorkflowExecution' - The child workflow execution that timed out.
--
-- * 'cwetoeaWorkflowType' - The type of the child workflow execution.
--
-- * 'cwetoeaTimeoutType' - The type of the timeout that caused the child workflow execution to time out.
--
-- * 'cwetoeaInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'cwetoeaStartedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionTimedOutEventAttributes
    :: WorkflowExecution -- ^ 'cwetoeaWorkflowExecution'
    -> WorkflowType -- ^ 'cwetoeaWorkflowType'
    -> WorkflowExecutionTimeoutType -- ^ 'cwetoeaTimeoutType'
    -> Integer -- ^ 'cwetoeaInitiatedEventId'
    -> Integer -- ^ 'cwetoeaStartedEventId'
    -> ChildWorkflowExecutionTimedOutEventAttributes
childWorkflowExecutionTimedOutEventAttributes pWorkflowExecution_ pWorkflowType_ pTimeoutType_ pInitiatedEventId_ pStartedEventId_ =
  ChildWorkflowExecutionTimedOutEventAttributes'
    { _cwetoeaWorkflowExecution = pWorkflowExecution_
    , _cwetoeaWorkflowType = pWorkflowType_
    , _cwetoeaTimeoutType = pTimeoutType_
    , _cwetoeaInitiatedEventId = pInitiatedEventId_
    , _cwetoeaStartedEventId = pStartedEventId_
    }


-- | The child workflow execution that timed out.
cwetoeaWorkflowExecution :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecution
cwetoeaWorkflowExecution = lens _cwetoeaWorkflowExecution (\ s a -> s{_cwetoeaWorkflowExecution = a})

-- | The type of the child workflow execution.
cwetoeaWorkflowType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowType
cwetoeaWorkflowType = lens _cwetoeaWorkflowType (\ s a -> s{_cwetoeaWorkflowType = a})

-- | The type of the timeout that caused the child workflow execution to time out.
cwetoeaTimeoutType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
cwetoeaTimeoutType = lens _cwetoeaTimeoutType (\ s a -> s{_cwetoeaTimeoutType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cwetoeaInitiatedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes Integer
cwetoeaInitiatedEventId = lens _cwetoeaInitiatedEventId (\ s a -> s{_cwetoeaInitiatedEventId = a})

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cwetoeaStartedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes Integer
cwetoeaStartedEventId = lens _cwetoeaStartedEventId (\ s a -> s{_cwetoeaStartedEventId = a})

instance FromJSON
           ChildWorkflowExecutionTimedOutEventAttributes
         where
        parseJSON
          = withObject
              "ChildWorkflowExecutionTimedOutEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionTimedOutEventAttributes' <$>
                   (x .: "workflowExecution") <*> (x .: "workflowType")
                     <*> (x .: "timeoutType")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "startedEventId"))

instance Hashable
           ChildWorkflowExecutionTimedOutEventAttributes
         where

instance NFData
           ChildWorkflowExecutionTimedOutEventAttributes
         where

-- | Used to filter the closed workflow executions in visibility APIs by their close status.
--
--
--
-- /See:/ 'closeStatusFilter' smart constructor.
newtype CloseStatusFilter = CloseStatusFilter'
  { _csfStatus :: CloseStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloseStatusFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfStatus' - The close status that must match the close status of an execution for it to meet the criteria of this filter.
closeStatusFilter
    :: CloseStatus -- ^ 'csfStatus'
    -> CloseStatusFilter
closeStatusFilter pStatus_ = CloseStatusFilter' {_csfStatus = pStatus_}


-- | The close status that must match the close status of an execution for it to meet the criteria of this filter.
csfStatus :: Lens' CloseStatusFilter CloseStatus
csfStatus = lens _csfStatus (\ s a -> s{_csfStatus = a})

instance Hashable CloseStatusFilter where

instance NFData CloseStatusFilter where

instance ToJSON CloseStatusFilter where
        toJSON CloseStatusFilter'{..}
          = object (catMaybes [Just ("status" .= _csfStatus)])

-- | Provides the details of the @CompleteWorkflowExecution@ decision.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
--
-- /See:/ 'completeWorkflowExecutionDecisionAttributes' smart constructor.
newtype CompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes'
  { _cwedaResult :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompleteWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwedaResult' - The result of the workflow execution. The form of the result is implementation defined.
completeWorkflowExecutionDecisionAttributes
    :: CompleteWorkflowExecutionDecisionAttributes
completeWorkflowExecutionDecisionAttributes =
  CompleteWorkflowExecutionDecisionAttributes' {_cwedaResult = Nothing}


-- | The result of the workflow execution. The form of the result is implementation defined.
cwedaResult :: Lens' CompleteWorkflowExecutionDecisionAttributes (Maybe Text)
cwedaResult = lens _cwedaResult (\ s a -> s{_cwedaResult = a})

instance Hashable
           CompleteWorkflowExecutionDecisionAttributes
         where

instance NFData
           CompleteWorkflowExecutionDecisionAttributes
         where

instance ToJSON
           CompleteWorkflowExecutionDecisionAttributes
         where
        toJSON
          CompleteWorkflowExecutionDecisionAttributes'{..}
          = object (catMaybes [("result" .=) <$> _cwedaResult])

-- | Provides the details of the @CompleteWorkflowExecutionFailed@ event.
--
--
--
-- /See:/ 'completeWorkflowExecutionFailedEventAttributes' smart constructor.
data CompleteWorkflowExecutionFailedEventAttributes = CompleteWorkflowExecutionFailedEventAttributes'
  { _cwefeaCause                        :: !CompleteWorkflowExecutionFailedCause
  , _cwefeaDecisionTaskCompletedEventId :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompleteWorkflowExecutionFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwefeaCause' - The cause of the failure. This information is generated by the system and can be useful for diagnostic purposes.
--
-- * 'cwefeaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CompleteWorkflowExecution@ decision to complete this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
completeWorkflowExecutionFailedEventAttributes
    :: CompleteWorkflowExecutionFailedCause -- ^ 'cwefeaCause'
    -> Integer -- ^ 'cwefeaDecisionTaskCompletedEventId'
    -> CompleteWorkflowExecutionFailedEventAttributes
completeWorkflowExecutionFailedEventAttributes pCause_ pDecisionTaskCompletedEventId_ =
  CompleteWorkflowExecutionFailedEventAttributes'
    { _cwefeaCause = pCause_
    , _cwefeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId_
    }


-- | The cause of the failure. This information is generated by the system and can be useful for diagnostic purposes.
cwefeaCause :: Lens' CompleteWorkflowExecutionFailedEventAttributes CompleteWorkflowExecutionFailedCause
cwefeaCause = lens _cwefeaCause (\ s a -> s{_cwefeaCause = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CompleteWorkflowExecution@ decision to complete this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cwefeaDecisionTaskCompletedEventId :: Lens' CompleteWorkflowExecutionFailedEventAttributes Integer
cwefeaDecisionTaskCompletedEventId = lens _cwefeaDecisionTaskCompletedEventId (\ s a -> s{_cwefeaDecisionTaskCompletedEventId = a})

instance FromJSON
           CompleteWorkflowExecutionFailedEventAttributes
         where
        parseJSON
          = withObject
              "CompleteWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 CompleteWorkflowExecutionFailedEventAttributes' <$>
                   (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

instance Hashable
           CompleteWorkflowExecutionFailedEventAttributes
         where

instance NFData
           CompleteWorkflowExecutionFailedEventAttributes
         where

-- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @tag@
