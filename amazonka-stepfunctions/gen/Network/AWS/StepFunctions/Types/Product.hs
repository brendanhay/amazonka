{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.Sum

-- | Contains details about an activity which failed during an execution.
--
--
--
-- /See:/ 'activityFailedEventDetails' smart constructor.
data ActivityFailedEventDetails = ActivityFailedEventDetails'
  { _afedError :: !(Maybe Text)
  , _afedCause :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afedError' - The error code of the failure.
--
-- * 'afedCause' - A more detailed explanation of the cause of the failure.
activityFailedEventDetails
    :: ActivityFailedEventDetails
activityFailedEventDetails =
  ActivityFailedEventDetails' {_afedError = Nothing, _afedCause = Nothing}


-- | The error code of the failure.
afedError :: Lens' ActivityFailedEventDetails (Maybe Text)
afedError = lens _afedError (\ s a -> s{_afedError = a})

-- | A more detailed explanation of the cause of the failure.
afedCause :: Lens' ActivityFailedEventDetails (Maybe Text)
afedCause = lens _afedCause (\ s a -> s{_afedCause = a})

instance FromJSON ActivityFailedEventDetails where
        parseJSON
          = withObject "ActivityFailedEventDetails"
              (\ x ->
                 ActivityFailedEventDetails' <$>
                   (x .:? "error") <*> (x .:? "cause"))

instance Hashable ActivityFailedEventDetails where

instance NFData ActivityFailedEventDetails where

-- | Contains details about an activity.
--
--
--
-- /See:/ 'activityListItem' smart constructor.
data ActivityListItem = ActivityListItem'
  { _aliActivityARN  :: !Text
  , _aliName         :: !Text
  , _aliCreationDate :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aliActivityARN' - The Amazon Resource Name (ARN) that identifies the activity.
--
-- * 'aliName' - The name of the activity. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
-- * 'aliCreationDate' - The date the activity is created.
activityListItem
    :: Text -- ^ 'aliActivityARN'
    -> Text -- ^ 'aliName'
    -> UTCTime -- ^ 'aliCreationDate'
    -> ActivityListItem
activityListItem pActivityARN_ pName_ pCreationDate_ =
  ActivityListItem'
    { _aliActivityARN = pActivityARN_
    , _aliName = pName_
    , _aliCreationDate = _Time # pCreationDate_
    }


-- | The Amazon Resource Name (ARN) that identifies the activity.
aliActivityARN :: Lens' ActivityListItem Text
aliActivityARN = lens _aliActivityARN (\ s a -> s{_aliActivityARN = a})

-- | The name of the activity. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
aliName :: Lens' ActivityListItem Text
aliName = lens _aliName (\ s a -> s{_aliName = a})

-- | The date the activity is created.
aliCreationDate :: Lens' ActivityListItem UTCTime
aliCreationDate = lens _aliCreationDate (\ s a -> s{_aliCreationDate = a}) . _Time

instance FromJSON ActivityListItem where
        parseJSON
          = withObject "ActivityListItem"
              (\ x ->
                 ActivityListItem' <$>
                   (x .: "activityArn") <*> (x .: "name") <*>
                     (x .: "creationDate"))

instance Hashable ActivityListItem where

instance NFData ActivityListItem where

-- | Contains details about an activity schedule failure which occurred during an execution.
--
--
--
-- /See:/ 'activityScheduleFailedEventDetails' smart constructor.
data ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails'
  { _asfedError :: !(Maybe Text)
  , _asfedCause :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityScheduleFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asfedError' - The error code of the failure.
--
-- * 'asfedCause' - A more detailed explanation of the cause of the failure.
activityScheduleFailedEventDetails
    :: ActivityScheduleFailedEventDetails
activityScheduleFailedEventDetails =
  ActivityScheduleFailedEventDetails'
    {_asfedError = Nothing, _asfedCause = Nothing}


-- | The error code of the failure.
asfedError :: Lens' ActivityScheduleFailedEventDetails (Maybe Text)
asfedError = lens _asfedError (\ s a -> s{_asfedError = a})

-- | A more detailed explanation of the cause of the failure.
asfedCause :: Lens' ActivityScheduleFailedEventDetails (Maybe Text)
asfedCause = lens _asfedCause (\ s a -> s{_asfedCause = a})

instance FromJSON ActivityScheduleFailedEventDetails
         where
        parseJSON
          = withObject "ActivityScheduleFailedEventDetails"
              (\ x ->
                 ActivityScheduleFailedEventDetails' <$>
                   (x .:? "error") <*> (x .:? "cause"))

instance Hashable ActivityScheduleFailedEventDetails
         where

instance NFData ActivityScheduleFailedEventDetails
         where

-- | Contains details about an activity scheduled during an execution.
--
--
--
-- /See:/ 'activityScheduledEventDetails' smart constructor.
data ActivityScheduledEventDetails = ActivityScheduledEventDetails'
  { _asedHeartbeatInSeconds :: !(Maybe Integer)
  , _asedInput              :: !(Maybe Text)
  , _asedTimeoutInSeconds   :: !(Maybe Integer)
  , _asedResource           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityScheduledEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asedHeartbeatInSeconds' - The maximum allowed duration between two heartbeats for the activity task.
--
-- * 'asedInput' - The JSON data input to the activity task.
--
-- * 'asedTimeoutInSeconds' - The maximum allowed duration of the activity task.
--
-- * 'asedResource' - The Amazon Resource Name (ARN) of the scheduled activity.
activityScheduledEventDetails
    :: Text -- ^ 'asedResource'
    -> ActivityScheduledEventDetails
activityScheduledEventDetails pResource_ =
  ActivityScheduledEventDetails'
    { _asedHeartbeatInSeconds = Nothing
    , _asedInput = Nothing
    , _asedTimeoutInSeconds = Nothing
    , _asedResource = pResource_
    }


-- | The maximum allowed duration between two heartbeats for the activity task.
asedHeartbeatInSeconds :: Lens' ActivityScheduledEventDetails (Maybe Integer)
asedHeartbeatInSeconds = lens _asedHeartbeatInSeconds (\ s a -> s{_asedHeartbeatInSeconds = a})

-- | The JSON data input to the activity task.
asedInput :: Lens' ActivityScheduledEventDetails (Maybe Text)
asedInput = lens _asedInput (\ s a -> s{_asedInput = a})

-- | The maximum allowed duration of the activity task.
asedTimeoutInSeconds :: Lens' ActivityScheduledEventDetails (Maybe Integer)
asedTimeoutInSeconds = lens _asedTimeoutInSeconds (\ s a -> s{_asedTimeoutInSeconds = a})

-- | The Amazon Resource Name (ARN) of the scheduled activity.
asedResource :: Lens' ActivityScheduledEventDetails Text
asedResource = lens _asedResource (\ s a -> s{_asedResource = a})

instance FromJSON ActivityScheduledEventDetails where
        parseJSON
          = withObject "ActivityScheduledEventDetails"
              (\ x ->
                 ActivityScheduledEventDetails' <$>
                   (x .:? "heartbeatInSeconds") <*> (x .:? "input") <*>
                     (x .:? "timeoutInSeconds")
                     <*> (x .: "resource"))

instance Hashable ActivityScheduledEventDetails where

instance NFData ActivityScheduledEventDetails where

-- | Contains details about the start of an activity during an execution.
--
--
--
-- /See:/ 'activityStartedEventDetails' smart constructor.
newtype ActivityStartedEventDetails = ActivityStartedEventDetails'
  { _asedWorkerName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityStartedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asedWorkerName' - The name of the worker that the task is assigned to. These names are provided by the workers when calling 'GetActivityTask' .
activityStartedEventDetails
    :: ActivityStartedEventDetails
activityStartedEventDetails =
  ActivityStartedEventDetails' {_asedWorkerName = Nothing}


-- | The name of the worker that the task is assigned to. These names are provided by the workers when calling 'GetActivityTask' .
asedWorkerName :: Lens' ActivityStartedEventDetails (Maybe Text)
asedWorkerName = lens _asedWorkerName (\ s a -> s{_asedWorkerName = a})

instance FromJSON ActivityStartedEventDetails where
        parseJSON
          = withObject "ActivityStartedEventDetails"
              (\ x ->
                 ActivityStartedEventDetails' <$>
                   (x .:? "workerName"))

instance Hashable ActivityStartedEventDetails where

instance NFData ActivityStartedEventDetails where

-- | Contains details about an activity which successfully terminated during an execution.
--
--
--
-- /See:/ 'activitySucceededEventDetails' smart constructor.
newtype ActivitySucceededEventDetails = ActivitySucceededEventDetails'
  { _asedOutput :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivitySucceededEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asedOutput' - The JSON data output by the activity task.
activitySucceededEventDetails
    :: ActivitySucceededEventDetails
activitySucceededEventDetails =
  ActivitySucceededEventDetails' {_asedOutput = Nothing}


-- | The JSON data output by the activity task.
asedOutput :: Lens' ActivitySucceededEventDetails (Maybe Text)
asedOutput = lens _asedOutput (\ s a -> s{_asedOutput = a})

instance FromJSON ActivitySucceededEventDetails where
        parseJSON
          = withObject "ActivitySucceededEventDetails"
              (\ x ->
                 ActivitySucceededEventDetails' <$> (x .:? "output"))

instance Hashable ActivitySucceededEventDetails where

instance NFData ActivitySucceededEventDetails where

-- | Contains details about an activity timeout which occurred during an execution.
--
--
--
-- /See:/ 'activityTimedOutEventDetails' smart constructor.
data ActivityTimedOutEventDetails = ActivityTimedOutEventDetails'
  { _atoedError :: !(Maybe Text)
  , _atoedCause :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityTimedOutEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atoedError' - The error code of the failure.
--
-- * 'atoedCause' - A more detailed explanation of the cause of the timeout.
activityTimedOutEventDetails
    :: ActivityTimedOutEventDetails
activityTimedOutEventDetails =
  ActivityTimedOutEventDetails' {_atoedError = Nothing, _atoedCause = Nothing}


-- | The error code of the failure.
atoedError :: Lens' ActivityTimedOutEventDetails (Maybe Text)
atoedError = lens _atoedError (\ s a -> s{_atoedError = a})

-- | A more detailed explanation of the cause of the timeout.
atoedCause :: Lens' ActivityTimedOutEventDetails (Maybe Text)
atoedCause = lens _atoedCause (\ s a -> s{_atoedCause = a})

instance FromJSON ActivityTimedOutEventDetails where
        parseJSON
          = withObject "ActivityTimedOutEventDetails"
              (\ x ->
                 ActivityTimedOutEventDetails' <$>
                   (x .:? "error") <*> (x .:? "cause"))

instance Hashable ActivityTimedOutEventDetails where

instance NFData ActivityTimedOutEventDetails where

-- | Contains details about an abort of an execution.
--
--
--
-- /See:/ 'executionAbortedEventDetails' smart constructor.
data ExecutionAbortedEventDetails = ExecutionAbortedEventDetails'
  { _eaedError :: !(Maybe Text)
  , _eaedCause :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecutionAbortedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eaedError' - The error code of the failure.
--
-- * 'eaedCause' - A more detailed explanation of the cause of the failure.
executionAbortedEventDetails
    :: ExecutionAbortedEventDetails
executionAbortedEventDetails =
  ExecutionAbortedEventDetails' {_eaedError = Nothing, _eaedCause = Nothing}


-- | The error code of the failure.
eaedError :: Lens' ExecutionAbortedEventDetails (Maybe Text)
eaedError = lens _eaedError (\ s a -> s{_eaedError = a})

-- | A more detailed explanation of the cause of the failure.
eaedCause :: Lens' ExecutionAbortedEventDetails (Maybe Text)
eaedCause = lens _eaedCause (\ s a -> s{_eaedCause = a})

instance FromJSON ExecutionAbortedEventDetails where
        parseJSON
          = withObject "ExecutionAbortedEventDetails"
              (\ x ->
                 ExecutionAbortedEventDetails' <$>
                   (x .:? "error") <*> (x .:? "cause"))

instance Hashable ExecutionAbortedEventDetails where

instance NFData ExecutionAbortedEventDetails where

-- | Contains details about an execution failure event.
--
--
--
-- /See:/ 'executionFailedEventDetails' smart constructor.
data ExecutionFailedEventDetails = ExecutionFailedEventDetails'
  { _efedError :: !(Maybe Text)
  , _efedCause :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecutionFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efedError' - The error code of the failure.
--
-- * 'efedCause' - A more detailed explanation of the cause of the failure.
executionFailedEventDetails
    :: ExecutionFailedEventDetails
executionFailedEventDetails =
  ExecutionFailedEventDetails' {_efedError = Nothing, _efedCause = Nothing}


-- | The error code of the failure.
efedError :: Lens' ExecutionFailedEventDetails (Maybe Text)
efedError = lens _efedError (\ s a -> s{_efedError = a})

-- | A more detailed explanation of the cause of the failure.
efedCause :: Lens' ExecutionFailedEventDetails (Maybe Text)
efedCause = lens _efedCause (\ s a -> s{_efedCause = a})

instance FromJSON ExecutionFailedEventDetails where
        parseJSON
          = withObject "ExecutionFailedEventDetails"
              (\ x ->
                 ExecutionFailedEventDetails' <$>
                   (x .:? "error") <*> (x .:? "cause"))

instance Hashable ExecutionFailedEventDetails where

instance NFData ExecutionFailedEventDetails where

-- | Contains details about an execution.
--
--
--
-- /See:/ 'executionListItem' smart constructor.
data ExecutionListItem = ExecutionListItem'
  { _eliStopDate        :: !(Maybe POSIX)
  , _eliExecutionARN    :: !Text
  , _eliStateMachineARN :: !Text
  , _eliName            :: !Text
  , _eliStatus          :: !ExecutionStatus
  , _eliStartDate       :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecutionListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eliStopDate' - If the execution already ended, the date the execution stopped.
--
-- * 'eliExecutionARN' - The Amazon Resource Name (ARN) that identifies the execution.
--
-- * 'eliStateMachineARN' - The Amazon Resource Name (ARN) of the executed state machine.
--
-- * 'eliName' - The name of the execution. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
-- * 'eliStatus' - The current status of the execution.
--
-- * 'eliStartDate' - The date the execution started.
executionListItem
    :: Text -- ^ 'eliExecutionARN'
    -> Text -- ^ 'eliStateMachineARN'
    -> Text -- ^ 'eliName'
    -> ExecutionStatus -- ^ 'eliStatus'
    -> UTCTime -- ^ 'eliStartDate'
    -> ExecutionListItem
executionListItem pExecutionARN_ pStateMachineARN_ pName_ pStatus_ pStartDate_ =
  ExecutionListItem'
    { _eliStopDate = Nothing
    , _eliExecutionARN = pExecutionARN_
    , _eliStateMachineARN = pStateMachineARN_
    , _eliName = pName_
    , _eliStatus = pStatus_
    , _eliStartDate = _Time # pStartDate_
    }


-- | If the execution already ended, the date the execution stopped.
eliStopDate :: Lens' ExecutionListItem (Maybe UTCTime)
eliStopDate = lens _eliStopDate (\ s a -> s{_eliStopDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) that identifies the execution.
eliExecutionARN :: Lens' ExecutionListItem Text
eliExecutionARN = lens _eliExecutionARN (\ s a -> s{_eliExecutionARN = a})

-- | The Amazon Resource Name (ARN) of the executed state machine.
eliStateMachineARN :: Lens' ExecutionListItem Text
eliStateMachineARN = lens _eliStateMachineARN (\ s a -> s{_eliStateMachineARN = a})

-- | The name of the execution. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
eliName :: Lens' ExecutionListItem Text
eliName = lens _eliName (\ s a -> s{_eliName = a})

-- | The current status of the execution.
eliStatus :: Lens' ExecutionListItem ExecutionStatus
eliStatus = lens _eliStatus (\ s a -> s{_eliStatus = a})

-- | The date the execution started.
eliStartDate :: Lens' ExecutionListItem UTCTime
eliStartDate = lens _eliStartDate (\ s a -> s{_eliStartDate = a}) . _Time

instance FromJSON ExecutionListItem where
        parseJSON
          = withObject "ExecutionListItem"
              (\ x ->
                 ExecutionListItem' <$>
                   (x .:? "stopDate") <*> (x .: "executionArn") <*>
                     (x .: "stateMachineArn")
                     <*> (x .: "name")
                     <*> (x .: "status")
                     <*> (x .: "startDate"))

instance Hashable ExecutionListItem where

instance NFData ExecutionListItem where

-- | Contains details about the start of the execution.
--
--
--
-- /See:/ 'executionStartedEventDetails' smart constructor.
data ExecutionStartedEventDetails = ExecutionStartedEventDetails'
  { _esedInput   :: !(Maybe Text)
  , _esedRoleARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecutionStartedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esedInput' - The JSON data input to the execution.
--
-- * 'esedRoleARN' - The Amazon Resource Name (ARN) of the IAM role used for executing AWS Lambda tasks.
executionStartedEventDetails
    :: ExecutionStartedEventDetails
executionStartedEventDetails =
  ExecutionStartedEventDetails' {_esedInput = Nothing, _esedRoleARN = Nothing}


-- | The JSON data input to the execution.
esedInput :: Lens' ExecutionStartedEventDetails (Maybe Text)
esedInput = lens _esedInput (\ s a -> s{_esedInput = a})

-- | The Amazon Resource Name (ARN) of the IAM role used for executing AWS Lambda tasks.
esedRoleARN :: Lens' ExecutionStartedEventDetails (Maybe Text)
esedRoleARN = lens _esedRoleARN (\ s a -> s{_esedRoleARN = a})

instance FromJSON ExecutionStartedEventDetails where
        parseJSON
          = withObject "ExecutionStartedEventDetails"
              (\ x ->
                 ExecutionStartedEventDetails' <$>
                   (x .:? "input") <*> (x .:? "roleArn"))

instance Hashable ExecutionStartedEventDetails where

instance NFData ExecutionStartedEventDetails where

-- | Contains details about the successful termination of the execution.
--
--
--
-- /See:/ 'executionSucceededEventDetails' smart constructor.
newtype ExecutionSucceededEventDetails = ExecutionSucceededEventDetails'
  { _esedOutput :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecutionSucceededEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esedOutput' - The JSON data output by the execution.
executionSucceededEventDetails
    :: ExecutionSucceededEventDetails
executionSucceededEventDetails =
  ExecutionSucceededEventDetails' {_esedOutput = Nothing}


-- | The JSON data output by the execution.
esedOutput :: Lens' ExecutionSucceededEventDetails (Maybe Text)
esedOutput = lens _esedOutput (\ s a -> s{_esedOutput = a})

instance FromJSON ExecutionSucceededEventDetails
         where
        parseJSON
          = withObject "ExecutionSucceededEventDetails"
              (\ x ->
                 ExecutionSucceededEventDetails' <$> (x .:? "output"))

instance Hashable ExecutionSucceededEventDetails
         where

instance NFData ExecutionSucceededEventDetails where

-- | Contains details about the execution timeout which occurred during the execution.
--
--
--
-- /See:/ 'executionTimedOutEventDetails' smart constructor.
data ExecutionTimedOutEventDetails = ExecutionTimedOutEventDetails'
  { _etoedError :: !(Maybe Text)
  , _etoedCause :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecutionTimedOutEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etoedError' - The error code of the failure.
--
-- * 'etoedCause' - A more detailed explanation of the cause of the timeout.
executionTimedOutEventDetails
    :: ExecutionTimedOutEventDetails
executionTimedOutEventDetails =
  ExecutionTimedOutEventDetails' {_etoedError = Nothing, _etoedCause = Nothing}


-- | The error code of the failure.
etoedError :: Lens' ExecutionTimedOutEventDetails (Maybe Text)
etoedError = lens _etoedError (\ s a -> s{_etoedError = a})

-- | A more detailed explanation of the cause of the timeout.
etoedCause :: Lens' ExecutionTimedOutEventDetails (Maybe Text)
etoedCause = lens _etoedCause (\ s a -> s{_etoedCause = a})

instance FromJSON ExecutionTimedOutEventDetails where
        parseJSON
          = withObject "ExecutionTimedOutEventDetails"
              (\ x ->
                 ExecutionTimedOutEventDetails' <$>
                   (x .:? "error") <*> (x .:? "cause"))

instance Hashable ExecutionTimedOutEventDetails where

instance NFData ExecutionTimedOutEventDetails where

-- | Contains details about the events of an execution.
--
--
--
-- /See:/ 'historyEvent' smart constructor.
data HistoryEvent = HistoryEvent'
  { _heActivityStartedEventDetails :: !(Maybe ActivityStartedEventDetails)
  , _heLambdaFunctionStartFailedEventDetails :: !(Maybe LambdaFunctionStartFailedEventDetails)
  , _heStateExitedEventDetails :: !(Maybe StateExitedEventDetails)
  , _heLambdaFunctionSucceededEventDetails :: !(Maybe LambdaFunctionSucceededEventDetails)
  , _heActivitySucceededEventDetails :: !(Maybe ActivitySucceededEventDetails)
  , _heLambdaFunctionTimedOutEventDetails :: !(Maybe LambdaFunctionTimedOutEventDetails)
  , _heActivityTimedOutEventDetails :: !(Maybe ActivityTimedOutEventDetails)
  , _heExecutionFailedEventDetails :: !(Maybe ExecutionFailedEventDetails)
  , _heExecutionAbortedEventDetails :: !(Maybe ExecutionAbortedEventDetails)
  , _heExecutionSucceededEventDetails :: !(Maybe ExecutionSucceededEventDetails)
  , _heLambdaFunctionScheduledEventDetails :: !(Maybe LambdaFunctionScheduledEventDetails)
  , _heActivityScheduledEventDetails :: !(Maybe ActivityScheduledEventDetails)
  , _heExecutionStartedEventDetails :: !(Maybe ExecutionStartedEventDetails)
  , _heActivityScheduleFailedEventDetails :: !(Maybe ActivityScheduleFailedEventDetails)
  , _heLambdaFunctionScheduleFailedEventDetails :: !(Maybe LambdaFunctionScheduleFailedEventDetails)
  , _heStateEnteredEventDetails :: !(Maybe StateEnteredEventDetails)
  , _hePreviousEventId :: !(Maybe Integer)
  , _heActivityFailedEventDetails :: !(Maybe ActivityFailedEventDetails)
  , _heLambdaFunctionFailedEventDetails :: !(Maybe LambdaFunctionFailedEventDetails)
  , _heExecutionTimedOutEventDetails :: !(Maybe ExecutionTimedOutEventDetails)
  , _heTimestamp :: !POSIX
  , _heType :: !HistoryEventType
  , _heId :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HistoryEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'heActivityStartedEventDetails' - Undocumented member.
--
-- * 'heLambdaFunctionStartFailedEventDetails' - Contains details about a lambda function which failed to start during an execution.
--
-- * 'heStateExitedEventDetails' - Undocumented member.
--
-- * 'heLambdaFunctionSucceededEventDetails' - Contains details about a lambda function which terminated successfully during an execution.
--
-- * 'heActivitySucceededEventDetails' - Undocumented member.
--
-- * 'heLambdaFunctionTimedOutEventDetails' - Undocumented member.
--
-- * 'heActivityTimedOutEventDetails' - Undocumented member.
--
-- * 'heExecutionFailedEventDetails' - Undocumented member.
--
-- * 'heExecutionAbortedEventDetails' - Undocumented member.
--
-- * 'heExecutionSucceededEventDetails' - Undocumented member.
--
-- * 'heLambdaFunctionScheduledEventDetails' - Undocumented member.
--
-- * 'heActivityScheduledEventDetails' - Undocumented member.
--
-- * 'heExecutionStartedEventDetails' - Undocumented member.
--
-- * 'heActivityScheduleFailedEventDetails' - Contains details about an activity schedule event which failed during an execution.
--
-- * 'heLambdaFunctionScheduleFailedEventDetails' - Undocumented member.
--
-- * 'heStateEnteredEventDetails' - Undocumented member.
--
-- * 'hePreviousEventId' - The id of the previous event.
--
-- * 'heActivityFailedEventDetails' - Undocumented member.
--
-- * 'heLambdaFunctionFailedEventDetails' - Undocumented member.
--
-- * 'heExecutionTimedOutEventDetails' - Undocumented member.
--
-- * 'heTimestamp' - The date the event occurred.
--
-- * 'heType' - The type of the event.
--
-- * 'heId' - The id of the event. Events are numbered sequentially, starting at one.
historyEvent
    :: UTCTime -- ^ 'heTimestamp'
    -> HistoryEventType -- ^ 'heType'
    -> Integer -- ^ 'heId'
    -> HistoryEvent
historyEvent pTimestamp_ pType_ pId_ =
  HistoryEvent'
    { _heActivityStartedEventDetails = Nothing
    , _heLambdaFunctionStartFailedEventDetails = Nothing
    , _heStateExitedEventDetails = Nothing
    , _heLambdaFunctionSucceededEventDetails = Nothing
    , _heActivitySucceededEventDetails = Nothing
    , _heLambdaFunctionTimedOutEventDetails = Nothing
    , _heActivityTimedOutEventDetails = Nothing
    , _heExecutionFailedEventDetails = Nothing
    , _heExecutionAbortedEventDetails = Nothing
    , _heExecutionSucceededEventDetails = Nothing
    , _heLambdaFunctionScheduledEventDetails = Nothing
    , _heActivityScheduledEventDetails = Nothing
    , _heExecutionStartedEventDetails = Nothing
    , _heActivityScheduleFailedEventDetails = Nothing
    , _heLambdaFunctionScheduleFailedEventDetails = Nothing
    , _heStateEnteredEventDetails = Nothing
    , _hePreviousEventId = Nothing
    , _heActivityFailedEventDetails = Nothing
    , _heLambdaFunctionFailedEventDetails = Nothing
    , _heExecutionTimedOutEventDetails = Nothing
    , _heTimestamp = _Time # pTimestamp_
    , _heType = pType_
    , _heId = pId_
    }


-- | Undocumented member.
heActivityStartedEventDetails :: Lens' HistoryEvent (Maybe ActivityStartedEventDetails)
heActivityStartedEventDetails = lens _heActivityStartedEventDetails (\ s a -> s{_heActivityStartedEventDetails = a})

-- | Contains details about a lambda function which failed to start during an execution.
heLambdaFunctionStartFailedEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionStartFailedEventDetails)
heLambdaFunctionStartFailedEventDetails = lens _heLambdaFunctionStartFailedEventDetails (\ s a -> s{_heLambdaFunctionStartFailedEventDetails = a})

-- | Undocumented member.
heStateExitedEventDetails :: Lens' HistoryEvent (Maybe StateExitedEventDetails)
heStateExitedEventDetails = lens _heStateExitedEventDetails (\ s a -> s{_heStateExitedEventDetails = a})

-- | Contains details about a lambda function which terminated successfully during an execution.
heLambdaFunctionSucceededEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionSucceededEventDetails)
heLambdaFunctionSucceededEventDetails = lens _heLambdaFunctionSucceededEventDetails (\ s a -> s{_heLambdaFunctionSucceededEventDetails = a})

-- | Undocumented member.
heActivitySucceededEventDetails :: Lens' HistoryEvent (Maybe ActivitySucceededEventDetails)
heActivitySucceededEventDetails = lens _heActivitySucceededEventDetails (\ s a -> s{_heActivitySucceededEventDetails = a})

-- | Undocumented member.
heLambdaFunctionTimedOutEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionTimedOutEventDetails)
heLambdaFunctionTimedOutEventDetails = lens _heLambdaFunctionTimedOutEventDetails (\ s a -> s{_heLambdaFunctionTimedOutEventDetails = a})

-- | Undocumented member.
heActivityTimedOutEventDetails :: Lens' HistoryEvent (Maybe ActivityTimedOutEventDetails)
heActivityTimedOutEventDetails = lens _heActivityTimedOutEventDetails (\ s a -> s{_heActivityTimedOutEventDetails = a})

-- | Undocumented member.
heExecutionFailedEventDetails :: Lens' HistoryEvent (Maybe ExecutionFailedEventDetails)
heExecutionFailedEventDetails = lens _heExecutionFailedEventDetails (\ s a -> s{_heExecutionFailedEventDetails = a})

-- | Undocumented member.
heExecutionAbortedEventDetails :: Lens' HistoryEvent (Maybe ExecutionAbortedEventDetails)
heExecutionAbortedEventDetails = lens _heExecutionAbortedEventDetails (\ s a -> s{_heExecutionAbortedEventDetails = a})

-- | Undocumented member.
heExecutionSucceededEventDetails :: Lens' HistoryEvent (Maybe ExecutionSucceededEventDetails)
heExecutionSucceededEventDetails = lens _heExecutionSucceededEventDetails (\ s a -> s{_heExecutionSucceededEventDetails = a})

-- | Undocumented member.
heLambdaFunctionScheduledEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionScheduledEventDetails)
heLambdaFunctionScheduledEventDetails = lens _heLambdaFunctionScheduledEventDetails (\ s a -> s{_heLambdaFunctionScheduledEventDetails = a})

-- | Undocumented member.
heActivityScheduledEventDetails :: Lens' HistoryEvent (Maybe ActivityScheduledEventDetails)
heActivityScheduledEventDetails = lens _heActivityScheduledEventDetails (\ s a -> s{_heActivityScheduledEventDetails = a})

-- | Undocumented member.
heExecutionStartedEventDetails :: Lens' HistoryEvent (Maybe ExecutionStartedEventDetails)
heExecutionStartedEventDetails = lens _heExecutionStartedEventDetails (\ s a -> s{_heExecutionStartedEventDetails = a})

-- | Contains details about an activity schedule event which failed during an execution.
heActivityScheduleFailedEventDetails :: Lens' HistoryEvent (Maybe ActivityScheduleFailedEventDetails)
heActivityScheduleFailedEventDetails = lens _heActivityScheduleFailedEventDetails (\ s a -> s{_heActivityScheduleFailedEventDetails = a})

-- | Undocumented member.
heLambdaFunctionScheduleFailedEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionScheduleFailedEventDetails)
heLambdaFunctionScheduleFailedEventDetails = lens _heLambdaFunctionScheduleFailedEventDetails (\ s a -> s{_heLambdaFunctionScheduleFailedEventDetails = a})

-- | Undocumented member.
heStateEnteredEventDetails :: Lens' HistoryEvent (Maybe StateEnteredEventDetails)
heStateEnteredEventDetails = lens _heStateEnteredEventDetails (\ s a -> s{_heStateEnteredEventDetails = a})

-- | The id of the previous event.
hePreviousEventId :: Lens' HistoryEvent (Maybe Integer)
hePreviousEventId = lens _hePreviousEventId (\ s a -> s{_hePreviousEventId = a})

-- | Undocumented member.
heActivityFailedEventDetails :: Lens' HistoryEvent (Maybe ActivityFailedEventDetails)
heActivityFailedEventDetails = lens _heActivityFailedEventDetails (\ s a -> s{_heActivityFailedEventDetails = a})

-- | Undocumented member.
heLambdaFunctionFailedEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionFailedEventDetails)
heLambdaFunctionFailedEventDetails = lens _heLambdaFunctionFailedEventDetails (\ s a -> s{_heLambdaFunctionFailedEventDetails = a})

-- | Undocumented member.
heExecutionTimedOutEventDetails :: Lens' HistoryEvent (Maybe ExecutionTimedOutEventDetails)
heExecutionTimedOutEventDetails = lens _heExecutionTimedOutEventDetails (\ s a -> s{_heExecutionTimedOutEventDetails = a})

-- | The date the event occurred.
heTimestamp :: Lens' HistoryEvent UTCTime
heTimestamp = lens _heTimestamp (\ s a -> s{_heTimestamp = a}) . _Time

-- | The type of the event.
heType :: Lens' HistoryEvent HistoryEventType
heType = lens _heType (\ s a -> s{_heType = a})

-- | The id of the event. Events are numbered sequentially, starting at one.
heId :: Lens' HistoryEvent Integer
heId = lens _heId (\ s a -> s{_heId = a})

instance FromJSON HistoryEvent where
        parseJSON
          = withObject "HistoryEvent"
              (\ x ->
                 HistoryEvent' <$>
                   (x .:? "activityStartedEventDetails") <*>
                     (x .:? "lambdaFunctionStartFailedEventDetails")
                     <*> (x .:? "stateExitedEventDetails")
                     <*> (x .:? "lambdaFunctionSucceededEventDetails")
                     <*> (x .:? "activitySucceededEventDetails")
                     <*> (x .:? "lambdaFunctionTimedOutEventDetails")
                     <*> (x .:? "activityTimedOutEventDetails")
                     <*> (x .:? "executionFailedEventDetails")
                     <*> (x .:? "executionAbortedEventDetails")
                     <*> (x .:? "executionSucceededEventDetails")
                     <*> (x .:? "lambdaFunctionScheduledEventDetails")
                     <*> (x .:? "activityScheduledEventDetails")
                     <*> (x .:? "executionStartedEventDetails")
                     <*> (x .:? "activityScheduleFailedEventDetails")
                     <*>
                     (x .:? "lambdaFunctionScheduleFailedEventDetails")
                     <*> (x .:? "stateEnteredEventDetails")
                     <*> (x .:? "previousEventId")
                     <*> (x .:? "activityFailedEventDetails")
                     <*> (x .:? "lambdaFunctionFailedEventDetails")
                     <*> (x .:? "executionTimedOutEventDetails")
                     <*> (x .: "timestamp")
                     <*> (x .: "type")
                     <*> (x .: "id"))

instance Hashable HistoryEvent where

instance NFData HistoryEvent where

-- | Contains details about a lambda function which failed during an execution.
--
--
--
-- /See:/ 'lambdaFunctionFailedEventDetails' smart constructor.
data LambdaFunctionFailedEventDetails = LambdaFunctionFailedEventDetails'
  { _lffedError :: !(Maybe Text)
  , _lffedCause :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaFunctionFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lffedError' - The error code of the failure.
--
-- * 'lffedCause' - A more detailed explanation of the cause of the failure.
lambdaFunctionFailedEventDetails
    :: LambdaFunctionFailedEventDetails
lambdaFunctionFailedEventDetails =
  LambdaFunctionFailedEventDetails'
    {_lffedError = Nothing, _lffedCause = Nothing}


-- | The error code of the failure.
lffedError :: Lens' LambdaFunctionFailedEventDetails (Maybe Text)
lffedError = lens _lffedError (\ s a -> s{_lffedError = a})

-- | A more detailed explanation of the cause of the failure.
lffedCause :: Lens' LambdaFunctionFailedEventDetails (Maybe Text)
lffedCause = lens _lffedCause (\ s a -> s{_lffedCause = a})

instance FromJSON LambdaFunctionFailedEventDetails
         where
        parseJSON
          = withObject "LambdaFunctionFailedEventDetails"
              (\ x ->
                 LambdaFunctionFailedEventDetails' <$>
                   (x .:? "error") <*> (x .:? "cause"))

instance Hashable LambdaFunctionFailedEventDetails
         where

instance NFData LambdaFunctionFailedEventDetails
         where

-- | Contains details about a failed lambda function schedule event which occurred during an execution.
--
--
--
-- /See:/ 'lambdaFunctionScheduleFailedEventDetails' smart constructor.
data LambdaFunctionScheduleFailedEventDetails = LambdaFunctionScheduleFailedEventDetails'
  { _lError :: !(Maybe Text)
  , _lCause :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaFunctionScheduleFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lError' - The error code of the failure.
--
-- * 'lCause' - A more detailed explanation of the cause of the failure.
lambdaFunctionScheduleFailedEventDetails
    :: LambdaFunctionScheduleFailedEventDetails
lambdaFunctionScheduleFailedEventDetails =
  LambdaFunctionScheduleFailedEventDetails'
    {_lError = Nothing, _lCause = Nothing}


-- | The error code of the failure.
lError :: Lens' LambdaFunctionScheduleFailedEventDetails (Maybe Text)
lError = lens _lError (\ s a -> s{_lError = a})

-- | A more detailed explanation of the cause of the failure.
lCause :: Lens' LambdaFunctionScheduleFailedEventDetails (Maybe Text)
lCause = lens _lCause (\ s a -> s{_lCause = a})

instance FromJSON
           LambdaFunctionScheduleFailedEventDetails
         where
        parseJSON
          = withObject
              "LambdaFunctionScheduleFailedEventDetails"
              (\ x ->
                 LambdaFunctionScheduleFailedEventDetails' <$>
                   (x .:? "error") <*> (x .:? "cause"))

instance Hashable
           LambdaFunctionScheduleFailedEventDetails
         where

instance NFData
           LambdaFunctionScheduleFailedEventDetails
         where

-- | Contains details about a lambda function scheduled during an execution.
--
--
--
-- /See:/ 'lambdaFunctionScheduledEventDetails' smart constructor.
data LambdaFunctionScheduledEventDetails = LambdaFunctionScheduledEventDetails'
  { _lfsedInput            :: !(Maybe Text)
  , _lfsedTimeoutInSeconds :: !(Maybe Integer)
  , _lfsedResource         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaFunctionScheduledEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfsedInput' - The JSON data input to the lambda function.
--
-- * 'lfsedTimeoutInSeconds' - The maximum allowed duration of the lambda function.
--
-- * 'lfsedResource' - The Amazon Resource Name (ARN) of the scheduled lambda function.
lambdaFunctionScheduledEventDetails
    :: Text -- ^ 'lfsedResource'
    -> LambdaFunctionScheduledEventDetails
lambdaFunctionScheduledEventDetails pResource_ =
  LambdaFunctionScheduledEventDetails'
    { _lfsedInput = Nothing
    , _lfsedTimeoutInSeconds = Nothing
    , _lfsedResource = pResource_
    }


-- | The JSON data input to the lambda function.
lfsedInput :: Lens' LambdaFunctionScheduledEventDetails (Maybe Text)
lfsedInput = lens _lfsedInput (\ s a -> s{_lfsedInput = a})

-- | The maximum allowed duration of the lambda function.
lfsedTimeoutInSeconds :: Lens' LambdaFunctionScheduledEventDetails (Maybe Integer)
lfsedTimeoutInSeconds = lens _lfsedTimeoutInSeconds (\ s a -> s{_lfsedTimeoutInSeconds = a})

-- | The Amazon Resource Name (ARN) of the scheduled lambda function.
lfsedResource :: Lens' LambdaFunctionScheduledEventDetails Text
lfsedResource = lens _lfsedResource (\ s a -> s{_lfsedResource = a})

instance FromJSON LambdaFunctionScheduledEventDetails
         where
        parseJSON
          = withObject "LambdaFunctionScheduledEventDetails"
              (\ x ->
                 LambdaFunctionScheduledEventDetails' <$>
                   (x .:? "input") <*> (x .:? "timeoutInSeconds") <*>
                     (x .: "resource"))

instance Hashable LambdaFunctionScheduledEventDetails
         where

instance NFData LambdaFunctionScheduledEventDetails
         where

-- | Contains details about a lambda function which failed to start during an execution.
--
--
--
-- /See:/ 'lambdaFunctionStartFailedEventDetails' smart constructor.
data LambdaFunctionStartFailedEventDetails = LambdaFunctionStartFailedEventDetails'
  { _lfsfedError :: !(Maybe Text)
  , _lfsfedCause :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaFunctionStartFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfsfedError' - The error code of the failure.
--
-- * 'lfsfedCause' - A more detailed explanation of the cause of the failure.
lambdaFunctionStartFailedEventDetails
    :: LambdaFunctionStartFailedEventDetails
lambdaFunctionStartFailedEventDetails =
  LambdaFunctionStartFailedEventDetails'
    {_lfsfedError = Nothing, _lfsfedCause = Nothing}


-- | The error code of the failure.
lfsfedError :: Lens' LambdaFunctionStartFailedEventDetails (Maybe Text)
lfsfedError = lens _lfsfedError (\ s a -> s{_lfsfedError = a})

-- | A more detailed explanation of the cause of the failure.
lfsfedCause :: Lens' LambdaFunctionStartFailedEventDetails (Maybe Text)
lfsfedCause = lens _lfsfedCause (\ s a -> s{_lfsfedCause = a})

instance FromJSON
           LambdaFunctionStartFailedEventDetails
         where
        parseJSON
          = withObject "LambdaFunctionStartFailedEventDetails"
              (\ x ->
                 LambdaFunctionStartFailedEventDetails' <$>
                   (x .:? "error") <*> (x .:? "cause"))

instance Hashable
           LambdaFunctionStartFailedEventDetails
         where

instance NFData LambdaFunctionStartFailedEventDetails
         where

-- | Contains details about a lambda function which successfully terminated during an execution.
--
--
--
-- /See:/ 'lambdaFunctionSucceededEventDetails' smart constructor.
newtype LambdaFunctionSucceededEventDetails = LambdaFunctionSucceededEventDetails'
  { _lfsedOutput :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaFunctionSucceededEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfsedOutput' - The JSON data output by the lambda function.
lambdaFunctionSucceededEventDetails
    :: LambdaFunctionSucceededEventDetails
lambdaFunctionSucceededEventDetails =
  LambdaFunctionSucceededEventDetails' {_lfsedOutput = Nothing}


-- | The JSON data output by the lambda function.
lfsedOutput :: Lens' LambdaFunctionSucceededEventDetails (Maybe Text)
lfsedOutput = lens _lfsedOutput (\ s a -> s{_lfsedOutput = a})

instance FromJSON LambdaFunctionSucceededEventDetails
         where
        parseJSON
          = withObject "LambdaFunctionSucceededEventDetails"
              (\ x ->
                 LambdaFunctionSucceededEventDetails' <$>
                   (x .:? "output"))

instance Hashable LambdaFunctionSucceededEventDetails
         where

instance NFData LambdaFunctionSucceededEventDetails
         where

-- | Contains details about a lambda function timeout which occurred during an execution.
--
--
--
-- /See:/ 'lambdaFunctionTimedOutEventDetails' smart constructor.
data LambdaFunctionTimedOutEventDetails = LambdaFunctionTimedOutEventDetails'
  { _lftoedError :: !(Maybe Text)
  , _lftoedCause :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaFunctionTimedOutEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lftoedError' - The error code of the failure.
--
-- * 'lftoedCause' - A more detailed explanation of the cause of the timeout.
lambdaFunctionTimedOutEventDetails
    :: LambdaFunctionTimedOutEventDetails
lambdaFunctionTimedOutEventDetails =
  LambdaFunctionTimedOutEventDetails'
    {_lftoedError = Nothing, _lftoedCause = Nothing}


-- | The error code of the failure.
lftoedError :: Lens' LambdaFunctionTimedOutEventDetails (Maybe Text)
lftoedError = lens _lftoedError (\ s a -> s{_lftoedError = a})

-- | A more detailed explanation of the cause of the timeout.
lftoedCause :: Lens' LambdaFunctionTimedOutEventDetails (Maybe Text)
lftoedCause = lens _lftoedCause (\ s a -> s{_lftoedCause = a})

instance FromJSON LambdaFunctionTimedOutEventDetails
         where
        parseJSON
          = withObject "LambdaFunctionTimedOutEventDetails"
              (\ x ->
                 LambdaFunctionTimedOutEventDetails' <$>
                   (x .:? "error") <*> (x .:? "cause"))

instance Hashable LambdaFunctionTimedOutEventDetails
         where

instance NFData LambdaFunctionTimedOutEventDetails
         where

-- | Contains details about a state entered during an execution.
--
--
--
-- /See:/ 'stateEnteredEventDetails' smart constructor.
data StateEnteredEventDetails = StateEnteredEventDetails'
  { _sInput :: !(Maybe Text)
  , _sName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StateEnteredEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sInput' - The string that contains the JSON input data for the state.
--
-- * 'sName' - The name of the state.
stateEnteredEventDetails
    :: Text -- ^ 'sName'
    -> StateEnteredEventDetails
stateEnteredEventDetails pName_ =
  StateEnteredEventDetails' {_sInput = Nothing, _sName = pName_}


-- | The string that contains the JSON input data for the state.
sInput :: Lens' StateEnteredEventDetails (Maybe Text)
sInput = lens _sInput (\ s a -> s{_sInput = a})

-- | The name of the state.
sName :: Lens' StateEnteredEventDetails Text
sName = lens _sName (\ s a -> s{_sName = a})

instance FromJSON StateEnteredEventDetails where
        parseJSON
          = withObject "StateEnteredEventDetails"
              (\ x ->
                 StateEnteredEventDetails' <$>
                   (x .:? "input") <*> (x .: "name"))

instance Hashable StateEnteredEventDetails where

instance NFData StateEnteredEventDetails where

-- | Contains details about an exit from a state during an execution.
--
--
--
-- /See:/ 'stateExitedEventDetails' smart constructor.
data StateExitedEventDetails = StateExitedEventDetails'
  { _seedOutput :: !(Maybe Text)
  , _seedName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StateExitedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seedOutput' - The JSON output data of the state.
--
-- * 'seedName' - The name of the state. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
stateExitedEventDetails
    :: Text -- ^ 'seedName'
    -> StateExitedEventDetails
stateExitedEventDetails pName_ =
  StateExitedEventDetails' {_seedOutput = Nothing, _seedName = pName_}


-- | The JSON output data of the state.
seedOutput :: Lens' StateExitedEventDetails (Maybe Text)
seedOutput = lens _seedOutput (\ s a -> s{_seedOutput = a})

-- | The name of the state. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
seedName :: Lens' StateExitedEventDetails Text
seedName = lens _seedName (\ s a -> s{_seedName = a})

instance FromJSON StateExitedEventDetails where
        parseJSON
          = withObject "StateExitedEventDetails"
              (\ x ->
                 StateExitedEventDetails' <$>
                   (x .:? "output") <*> (x .: "name"))

instance Hashable StateExitedEventDetails where

instance NFData StateExitedEventDetails where

-- | Contains details about the state machine.
--
--
--
-- /See:/ 'stateMachineListItem' smart constructor.
data StateMachineListItem = StateMachineListItem'
  { _smliStateMachineARN :: !Text
  , _smliName            :: !Text
  , _smliCreationDate    :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StateMachineListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smliStateMachineARN' - The Amazon Resource Name (ARN) that identifies the state machine.
--
-- * 'smliName' - The name of the state machine. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
-- * 'smliCreationDate' - The date the state machine is created.
stateMachineListItem
    :: Text -- ^ 'smliStateMachineARN'
    -> Text -- ^ 'smliName'
    -> UTCTime -- ^ 'smliCreationDate'
    -> StateMachineListItem
stateMachineListItem pStateMachineARN_ pName_ pCreationDate_ =
  StateMachineListItem'
    { _smliStateMachineARN = pStateMachineARN_
    , _smliName = pName_
    , _smliCreationDate = _Time # pCreationDate_
    }


-- | The Amazon Resource Name (ARN) that identifies the state machine.
smliStateMachineARN :: Lens' StateMachineListItem Text
smliStateMachineARN = lens _smliStateMachineARN (\ s a -> s{_smliStateMachineARN = a})

-- | The name of the state machine. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
smliName :: Lens' StateMachineListItem Text
smliName = lens _smliName (\ s a -> s{_smliName = a})

-- | The date the state machine is created.
smliCreationDate :: Lens' StateMachineListItem UTCTime
smliCreationDate = lens _smliCreationDate (\ s a -> s{_smliCreationDate = a}) . _Time

instance FromJSON StateMachineListItem where
        parseJSON
          = withObject "StateMachineListItem"
              (\ x ->
                 StateMachineListItem' <$>
                   (x .: "stateMachineArn") <*> (x .: "name") <*>
                     (x .: "creationDate"))

instance Hashable StateMachineListItem where

instance NFData StateMachineListItem where
