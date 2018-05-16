{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.Product where

import Network.AWS.Inspector.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an Amazon Inspector agent. This data type is used as a request parameter in the 'ListAssessmentRunAgents' action.
--
--
--
-- /See:/ 'agentFilter' smart constructor.
data AgentFilter = AgentFilter'
  { _afAgentHealths     :: ![AgentHealth]
  , _afAgentHealthCodes :: ![AgentHealthCode]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AgentFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afAgentHealths' - The current health state of the agent. Values can be set to __HEALTHY__ or __UNHEALTHY__ .
--
-- * 'afAgentHealthCodes' - The detailed health state of the agent. Values can be set to __IDLE__ , __RUNNING__ , __SHUTDOWN__ , __UNHEALTHY__ , __THROTTLED__ , and __UNKNOWN__ .
agentFilter
    :: AgentFilter
agentFilter =
  AgentFilter' {_afAgentHealths = mempty, _afAgentHealthCodes = mempty}


-- | The current health state of the agent. Values can be set to __HEALTHY__ or __UNHEALTHY__ .
afAgentHealths :: Lens' AgentFilter [AgentHealth]
afAgentHealths = lens _afAgentHealths (\ s a -> s{_afAgentHealths = a}) . _Coerce

-- | The detailed health state of the agent. Values can be set to __IDLE__ , __RUNNING__ , __SHUTDOWN__ , __UNHEALTHY__ , __THROTTLED__ , and __UNKNOWN__ .
afAgentHealthCodes :: Lens' AgentFilter [AgentHealthCode]
afAgentHealthCodes = lens _afAgentHealthCodes (\ s a -> s{_afAgentHealthCodes = a}) . _Coerce

instance Hashable AgentFilter where

instance NFData AgentFilter where

instance ToJSON AgentFilter where
        toJSON AgentFilter'{..}
          = object
              (catMaybes
                 [Just ("agentHealths" .= _afAgentHealths),
                  Just ("agentHealthCodes" .= _afAgentHealthCodes)])

-- | Used as a response element in the 'PreviewAgents' action.
--
--
--
-- /See:/ 'agentPreview' smart constructor.
data AgentPreview = AgentPreview'
  { _apHostname         :: !(Maybe Text)
  , _apAutoScalingGroup :: !(Maybe Text)
  , _apOperatingSystem  :: !(Maybe Text)
  , _apAgentVersion     :: !(Maybe Text)
  , _apKernelVersion    :: !(Maybe Text)
  , _apAgentHealth      :: !(Maybe AgentHealth)
  , _apIpv4Address      :: !(Maybe Text)
  , _apAgentId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AgentPreview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apHostname' - The hostname of the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- * 'apAutoScalingGroup' - The Auto Scaling group for the EC2 instance where the agent is installed.
--
-- * 'apOperatingSystem' - The operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- * 'apAgentVersion' - The version of the Amazon Inspector Agent.
--
-- * 'apKernelVersion' - The kernel version of the operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- * 'apAgentHealth' - The health status of the Amazon Inspector Agent.
--
-- * 'apIpv4Address' - The IP address of the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- * 'apAgentId' - The ID of the EC2 instance where the agent is installed.
agentPreview
    :: Text -- ^ 'apAgentId'
    -> AgentPreview
agentPreview pAgentId_ =
  AgentPreview'
    { _apHostname = Nothing
    , _apAutoScalingGroup = Nothing
    , _apOperatingSystem = Nothing
    , _apAgentVersion = Nothing
    , _apKernelVersion = Nothing
    , _apAgentHealth = Nothing
    , _apIpv4Address = Nothing
    , _apAgentId = pAgentId_
    }


-- | The hostname of the EC2 instance on which the Amazon Inspector Agent is installed.
apHostname :: Lens' AgentPreview (Maybe Text)
apHostname = lens _apHostname (\ s a -> s{_apHostname = a})

-- | The Auto Scaling group for the EC2 instance where the agent is installed.
apAutoScalingGroup :: Lens' AgentPreview (Maybe Text)
apAutoScalingGroup = lens _apAutoScalingGroup (\ s a -> s{_apAutoScalingGroup = a})

-- | The operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
apOperatingSystem :: Lens' AgentPreview (Maybe Text)
apOperatingSystem = lens _apOperatingSystem (\ s a -> s{_apOperatingSystem = a})

-- | The version of the Amazon Inspector Agent.
apAgentVersion :: Lens' AgentPreview (Maybe Text)
apAgentVersion = lens _apAgentVersion (\ s a -> s{_apAgentVersion = a})

-- | The kernel version of the operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
apKernelVersion :: Lens' AgentPreview (Maybe Text)
apKernelVersion = lens _apKernelVersion (\ s a -> s{_apKernelVersion = a})

-- | The health status of the Amazon Inspector Agent.
apAgentHealth :: Lens' AgentPreview (Maybe AgentHealth)
apAgentHealth = lens _apAgentHealth (\ s a -> s{_apAgentHealth = a})

-- | The IP address of the EC2 instance on which the Amazon Inspector Agent is installed.
apIpv4Address :: Lens' AgentPreview (Maybe Text)
apIpv4Address = lens _apIpv4Address (\ s a -> s{_apIpv4Address = a})

-- | The ID of the EC2 instance where the agent is installed.
apAgentId :: Lens' AgentPreview Text
apAgentId = lens _apAgentId (\ s a -> s{_apAgentId = a})

instance FromJSON AgentPreview where
        parseJSON
          = withObject "AgentPreview"
              (\ x ->
                 AgentPreview' <$>
                   (x .:? "hostname") <*> (x .:? "autoScalingGroup") <*>
                     (x .:? "operatingSystem")
                     <*> (x .:? "agentVersion")
                     <*> (x .:? "kernelVersion")
                     <*> (x .:? "agentHealth")
                     <*> (x .:? "ipv4Address")
                     <*> (x .: "agentId"))

instance Hashable AgentPreview where

instance NFData AgentPreview where

-- | A snapshot of an Amazon Inspector assessment run that contains the findings of the assessment run .
--
--
-- Used as the response element in the 'DescribeAssessmentRuns' action.
--
--
-- /See:/ 'assessmentRun' smart constructor.
data AssessmentRun = AssessmentRun'
  { _arStartedAt                 :: !(Maybe POSIX)
  , _arCompletedAt               :: !(Maybe POSIX)
  , _arArn                       :: !Text
  , _arName                      :: !Text
  , _arAssessmentTemplateARN     :: !Text
  , _arState                     :: !AssessmentRunState
  , _arDurationInSeconds         :: !Nat
  , _arRulesPackageARNs          :: !(List1 Text)
  , _arUserAttributesForFindings :: ![Attribute]
  , _arCreatedAt                 :: !POSIX
  , _arStateChangedAt            :: !POSIX
  , _arDataCollected             :: !Bool
  , _arStateChanges              :: ![AssessmentRunStateChange]
  , _arNotifications             :: ![AssessmentRunNotification]
  , _arFindingCounts             :: !(Map Severity Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssessmentRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arStartedAt' - The time when 'StartAssessmentRun' was called.
--
-- * 'arCompletedAt' - The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
--
-- * 'arArn' - The ARN of the assessment run.
--
-- * 'arName' - The auto-generated name for the assessment run.
--
-- * 'arAssessmentTemplateARN' - The ARN of the assessment template that is associated with the assessment run.
--
-- * 'arState' - The state of the assessment run.
--
-- * 'arDurationInSeconds' - The duration of the assessment run.
--
-- * 'arRulesPackageARNs' - The rules packages selected for the assessment run.
--
-- * 'arUserAttributesForFindings' - The user-defined attributes that are assigned to every generated finding.
--
-- * 'arCreatedAt' - The time when 'StartAssessmentRun' was called.
--
-- * 'arStateChangedAt' - The last time when the assessment run's state changed.
--
-- * 'arDataCollected' - A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
--
-- * 'arStateChanges' - A list of the assessment run state changes.
--
-- * 'arNotifications' - A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
--
-- * 'arFindingCounts' - Provides a total count of generated findings per severity.
assessmentRun
    :: Text -- ^ 'arArn'
    -> Text -- ^ 'arName'
    -> Text -- ^ 'arAssessmentTemplateARN'
    -> AssessmentRunState -- ^ 'arState'
    -> Natural -- ^ 'arDurationInSeconds'
    -> NonEmpty Text -- ^ 'arRulesPackageARNs'
    -> UTCTime -- ^ 'arCreatedAt'
    -> UTCTime -- ^ 'arStateChangedAt'
    -> Bool -- ^ 'arDataCollected'
    -> AssessmentRun
assessmentRun pArn_ pName_ pAssessmentTemplateARN_ pState_ pDurationInSeconds_ pRulesPackageARNs_ pCreatedAt_ pStateChangedAt_ pDataCollected_ =
  AssessmentRun'
    { _arStartedAt = Nothing
    , _arCompletedAt = Nothing
    , _arArn = pArn_
    , _arName = pName_
    , _arAssessmentTemplateARN = pAssessmentTemplateARN_
    , _arState = pState_
    , _arDurationInSeconds = _Nat # pDurationInSeconds_
    , _arRulesPackageARNs = _List1 # pRulesPackageARNs_
    , _arUserAttributesForFindings = mempty
    , _arCreatedAt = _Time # pCreatedAt_
    , _arStateChangedAt = _Time # pStateChangedAt_
    , _arDataCollected = pDataCollected_
    , _arStateChanges = mempty
    , _arNotifications = mempty
    , _arFindingCounts = mempty
    }


-- | The time when 'StartAssessmentRun' was called.
arStartedAt :: Lens' AssessmentRun (Maybe UTCTime)
arStartedAt = lens _arStartedAt (\ s a -> s{_arStartedAt = a}) . mapping _Time

-- | The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
arCompletedAt :: Lens' AssessmentRun (Maybe UTCTime)
arCompletedAt = lens _arCompletedAt (\ s a -> s{_arCompletedAt = a}) . mapping _Time

-- | The ARN of the assessment run.
arArn :: Lens' AssessmentRun Text
arArn = lens _arArn (\ s a -> s{_arArn = a})

-- | The auto-generated name for the assessment run.
arName :: Lens' AssessmentRun Text
arName = lens _arName (\ s a -> s{_arName = a})

-- | The ARN of the assessment template that is associated with the assessment run.
arAssessmentTemplateARN :: Lens' AssessmentRun Text
arAssessmentTemplateARN = lens _arAssessmentTemplateARN (\ s a -> s{_arAssessmentTemplateARN = a})

-- | The state of the assessment run.
arState :: Lens' AssessmentRun AssessmentRunState
arState = lens _arState (\ s a -> s{_arState = a})

-- | The duration of the assessment run.
arDurationInSeconds :: Lens' AssessmentRun Natural
arDurationInSeconds = lens _arDurationInSeconds (\ s a -> s{_arDurationInSeconds = a}) . _Nat

-- | The rules packages selected for the assessment run.
arRulesPackageARNs :: Lens' AssessmentRun (NonEmpty Text)
arRulesPackageARNs = lens _arRulesPackageARNs (\ s a -> s{_arRulesPackageARNs = a}) . _List1

-- | The user-defined attributes that are assigned to every generated finding.
arUserAttributesForFindings :: Lens' AssessmentRun [Attribute]
arUserAttributesForFindings = lens _arUserAttributesForFindings (\ s a -> s{_arUserAttributesForFindings = a}) . _Coerce

-- | The time when 'StartAssessmentRun' was called.
arCreatedAt :: Lens' AssessmentRun UTCTime
arCreatedAt = lens _arCreatedAt (\ s a -> s{_arCreatedAt = a}) . _Time

-- | The last time when the assessment run's state changed.
arStateChangedAt :: Lens' AssessmentRun UTCTime
arStateChangedAt = lens _arStateChangedAt (\ s a -> s{_arStateChangedAt = a}) . _Time

-- | A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
arDataCollected :: Lens' AssessmentRun Bool
arDataCollected = lens _arDataCollected (\ s a -> s{_arDataCollected = a})

-- | A list of the assessment run state changes.
arStateChanges :: Lens' AssessmentRun [AssessmentRunStateChange]
arStateChanges = lens _arStateChanges (\ s a -> s{_arStateChanges = a}) . _Coerce

-- | A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
arNotifications :: Lens' AssessmentRun [AssessmentRunNotification]
arNotifications = lens _arNotifications (\ s a -> s{_arNotifications = a}) . _Coerce

-- | Provides a total count of generated findings per severity.
arFindingCounts :: Lens' AssessmentRun (HashMap Severity Int)
arFindingCounts = lens _arFindingCounts (\ s a -> s{_arFindingCounts = a}) . _Map

instance FromJSON AssessmentRun where
        parseJSON
          = withObject "AssessmentRun"
              (\ x ->
                 AssessmentRun' <$>
                   (x .:? "startedAt") <*> (x .:? "completedAt") <*>
                     (x .: "arn")
                     <*> (x .: "name")
                     <*> (x .: "assessmentTemplateArn")
                     <*> (x .: "state")
                     <*> (x .: "durationInSeconds")
                     <*> (x .: "rulesPackageArns")
                     <*> (x .:? "userAttributesForFindings" .!= mempty)
                     <*> (x .: "createdAt")
                     <*> (x .: "stateChangedAt")
                     <*> (x .: "dataCollected")
                     <*> (x .:? "stateChanges" .!= mempty)
                     <*> (x .:? "notifications" .!= mempty)
                     <*> (x .:? "findingCounts" .!= mempty))

instance Hashable AssessmentRun where

instance NFData AssessmentRun where

-- | Contains information about an Amazon Inspector agent. This data type is used as a response element in the 'ListAssessmentRunAgents' action.
--
--
--
-- /See:/ 'assessmentRunAgent' smart constructor.
data AssessmentRunAgent = AssessmentRunAgent'
  { _araAutoScalingGroup   :: !(Maybe Text)
  , _araAgentHealthDetails :: !(Maybe Text)
  , _araAgentId            :: !Text
  , _araAssessmentRunARN   :: !Text
  , _araAgentHealth        :: !AgentHealth
  , _araAgentHealthCode    :: !AgentHealthCode
  , _araTelemetryMetadata  :: ![TelemetryMetadata]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssessmentRunAgent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'araAutoScalingGroup' - The Auto Scaling group of the EC2 instance that is specified by the agent ID.
--
-- * 'araAgentHealthDetails' - The description for the agent health code.
--
-- * 'araAgentId' - The AWS account of the EC2 instance where the agent is installed.
--
-- * 'araAssessmentRunARN' - The ARN of the assessment run that is associated with the agent.
--
-- * 'araAgentHealth' - The current health state of the agent.
--
-- * 'araAgentHealthCode' - The detailed health state of the agent.
--
-- * 'araTelemetryMetadata' - The Amazon Inspector application data metrics that are collected by the agent.
assessmentRunAgent
    :: Text -- ^ 'araAgentId'
    -> Text -- ^ 'araAssessmentRunARN'
    -> AgentHealth -- ^ 'araAgentHealth'
    -> AgentHealthCode -- ^ 'araAgentHealthCode'
    -> AssessmentRunAgent
assessmentRunAgent pAgentId_ pAssessmentRunARN_ pAgentHealth_ pAgentHealthCode_ =
  AssessmentRunAgent'
    { _araAutoScalingGroup = Nothing
    , _araAgentHealthDetails = Nothing
    , _araAgentId = pAgentId_
    , _araAssessmentRunARN = pAssessmentRunARN_
    , _araAgentHealth = pAgentHealth_
    , _araAgentHealthCode = pAgentHealthCode_
    , _araTelemetryMetadata = mempty
    }


-- | The Auto Scaling group of the EC2 instance that is specified by the agent ID.
araAutoScalingGroup :: Lens' AssessmentRunAgent (Maybe Text)
araAutoScalingGroup = lens _araAutoScalingGroup (\ s a -> s{_araAutoScalingGroup = a})

-- | The description for the agent health code.
araAgentHealthDetails :: Lens' AssessmentRunAgent (Maybe Text)
araAgentHealthDetails = lens _araAgentHealthDetails (\ s a -> s{_araAgentHealthDetails = a})

-- | The AWS account of the EC2 instance where the agent is installed.
araAgentId :: Lens' AssessmentRunAgent Text
araAgentId = lens _araAgentId (\ s a -> s{_araAgentId = a})

-- | The ARN of the assessment run that is associated with the agent.
araAssessmentRunARN :: Lens' AssessmentRunAgent Text
araAssessmentRunARN = lens _araAssessmentRunARN (\ s a -> s{_araAssessmentRunARN = a})

-- | The current health state of the agent.
araAgentHealth :: Lens' AssessmentRunAgent AgentHealth
araAgentHealth = lens _araAgentHealth (\ s a -> s{_araAgentHealth = a})

-- | The detailed health state of the agent.
araAgentHealthCode :: Lens' AssessmentRunAgent AgentHealthCode
araAgentHealthCode = lens _araAgentHealthCode (\ s a -> s{_araAgentHealthCode = a})

-- | The Amazon Inspector application data metrics that are collected by the agent.
araTelemetryMetadata :: Lens' AssessmentRunAgent [TelemetryMetadata]
araTelemetryMetadata = lens _araTelemetryMetadata (\ s a -> s{_araTelemetryMetadata = a}) . _Coerce

instance FromJSON AssessmentRunAgent where
        parseJSON
          = withObject "AssessmentRunAgent"
              (\ x ->
                 AssessmentRunAgent' <$>
                   (x .:? "autoScalingGroup") <*>
                     (x .:? "agentHealthDetails")
                     <*> (x .: "agentId")
                     <*> (x .: "assessmentRunArn")
                     <*> (x .: "agentHealth")
                     <*> (x .: "agentHealthCode")
                     <*> (x .:? "telemetryMetadata" .!= mempty))

instance Hashable AssessmentRunAgent where

instance NFData AssessmentRunAgent where

-- | Used as the request parameter in the 'ListAssessmentRuns' action.
--
--
--
-- /See:/ 'assessmentRunFilter' smart constructor.
data AssessmentRunFilter = AssessmentRunFilter'
  { _arfStates               :: !(Maybe [AssessmentRunState])
  , _arfNamePattern          :: !(Maybe Text)
  , _arfStartTimeRange       :: !(Maybe TimestampRange)
  , _arfStateChangeTimeRange :: !(Maybe TimestampRange)
  , _arfRulesPackageARNs     :: !(Maybe [Text])
  , _arfCompletionTimeRange  :: !(Maybe TimestampRange)
  , _arfDurationRange        :: !(Maybe DurationRange)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssessmentRunFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arfStates' - For a record to match a filter, one of the values specified for this data type property must be the exact match of the value of the __assessmentRunState__ property of the 'AssessmentRun' data type.
--
-- * 'arfNamePattern' - For a record to match a filter, an explicit value or a string containing a wildcard that is specified for this data type property must match the value of the __assessmentRunName__ property of the 'AssessmentRun' data type.
--
-- * 'arfStartTimeRange' - For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __startTime__ property of the 'AssessmentRun' data type.
--
-- * 'arfStateChangeTimeRange' - For a record to match a filter, the value that is specified for this data type property must match the __stateChangedAt__ property of the 'AssessmentRun' data type.
--
-- * 'arfRulesPackageARNs' - For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __rulesPackages__ property of the 'AssessmentRun' data type.
--
-- * 'arfCompletionTimeRange' - For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __completedAt__ property of the 'AssessmentRun' data type.
--
-- * 'arfDurationRange' - For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentRun' data type.
assessmentRunFilter
    :: AssessmentRunFilter
assessmentRunFilter =
  AssessmentRunFilter'
    { _arfStates = Nothing
    , _arfNamePattern = Nothing
    , _arfStartTimeRange = Nothing
    , _arfStateChangeTimeRange = Nothing
    , _arfRulesPackageARNs = Nothing
    , _arfCompletionTimeRange = Nothing
    , _arfDurationRange = Nothing
    }


-- | For a record to match a filter, one of the values specified for this data type property must be the exact match of the value of the __assessmentRunState__ property of the 'AssessmentRun' data type.
arfStates :: Lens' AssessmentRunFilter [AssessmentRunState]
arfStates = lens _arfStates (\ s a -> s{_arfStates = a}) . _Default . _Coerce

-- | For a record to match a filter, an explicit value or a string containing a wildcard that is specified for this data type property must match the value of the __assessmentRunName__ property of the 'AssessmentRun' data type.
arfNamePattern :: Lens' AssessmentRunFilter (Maybe Text)
arfNamePattern = lens _arfNamePattern (\ s a -> s{_arfNamePattern = a})

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __startTime__ property of the 'AssessmentRun' data type.
arfStartTimeRange :: Lens' AssessmentRunFilter (Maybe TimestampRange)
arfStartTimeRange = lens _arfStartTimeRange (\ s a -> s{_arfStartTimeRange = a})

-- | For a record to match a filter, the value that is specified for this data type property must match the __stateChangedAt__ property of the 'AssessmentRun' data type.
arfStateChangeTimeRange :: Lens' AssessmentRunFilter (Maybe TimestampRange)
arfStateChangeTimeRange = lens _arfStateChangeTimeRange (\ s a -> s{_arfStateChangeTimeRange = a})

-- | For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __rulesPackages__ property of the 'AssessmentRun' data type.
arfRulesPackageARNs :: Lens' AssessmentRunFilter [Text]
arfRulesPackageARNs = lens _arfRulesPackageARNs (\ s a -> s{_arfRulesPackageARNs = a}) . _Default . _Coerce

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __completedAt__ property of the 'AssessmentRun' data type.
arfCompletionTimeRange :: Lens' AssessmentRunFilter (Maybe TimestampRange)
arfCompletionTimeRange = lens _arfCompletionTimeRange (\ s a -> s{_arfCompletionTimeRange = a})

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentRun' data type.
arfDurationRange :: Lens' AssessmentRunFilter (Maybe DurationRange)
arfDurationRange = lens _arfDurationRange (\ s a -> s{_arfDurationRange = a})

instance Hashable AssessmentRunFilter where

instance NFData AssessmentRunFilter where

instance ToJSON AssessmentRunFilter where
        toJSON AssessmentRunFilter'{..}
          = object
              (catMaybes
                 [("states" .=) <$> _arfStates,
                  ("namePattern" .=) <$> _arfNamePattern,
                  ("startTimeRange" .=) <$> _arfStartTimeRange,
                  ("stateChangeTimeRange" .=) <$>
                    _arfStateChangeTimeRange,
                  ("rulesPackageArns" .=) <$> _arfRulesPackageARNs,
                  ("completionTimeRange" .=) <$>
                    _arfCompletionTimeRange,
                  ("durationRange" .=) <$> _arfDurationRange])

-- | Used as one of the elements of the 'AssessmentRun' data type.
--
--
--
-- /See:/ 'assessmentRunNotification' smart constructor.
data AssessmentRunNotification = AssessmentRunNotification'
  { _arnSnsTopicARN          :: !(Maybe Text)
  , _arnSnsPublishStatusCode :: !(Maybe AssessmentRunNotificationSNSStatusCode)
  , _arnMessage              :: !(Maybe Text)
  , _arnDate                 :: !POSIX
  , _arnEvent                :: !InspectorEvent
  , _arnError                :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssessmentRunNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arnSnsTopicARN' - The SNS topic to which the SNS notification is sent.
--
-- * 'arnSnsPublishStatusCode' - The status code of the SNS notification.
--
-- * 'arnMessage' - The message included in the notification.
--
-- * 'arnDate' - The date of the notification.
--
-- * 'arnEvent' - The event for which a notification is sent.
--
-- * 'arnError' - The Boolean value that specifies whether the notification represents an error.
assessmentRunNotification
    :: UTCTime -- ^ 'arnDate'
    -> InspectorEvent -- ^ 'arnEvent'
    -> Bool -- ^ 'arnError'
    -> AssessmentRunNotification
assessmentRunNotification pDate_ pEvent_ pError_ =
  AssessmentRunNotification'
    { _arnSnsTopicARN = Nothing
    , _arnSnsPublishStatusCode = Nothing
    , _arnMessage = Nothing
    , _arnDate = _Time # pDate_
    , _arnEvent = pEvent_
    , _arnError = pError_
    }


-- | The SNS topic to which the SNS notification is sent.
arnSnsTopicARN :: Lens' AssessmentRunNotification (Maybe Text)
arnSnsTopicARN = lens _arnSnsTopicARN (\ s a -> s{_arnSnsTopicARN = a})

-- | The status code of the SNS notification.
arnSnsPublishStatusCode :: Lens' AssessmentRunNotification (Maybe AssessmentRunNotificationSNSStatusCode)
arnSnsPublishStatusCode = lens _arnSnsPublishStatusCode (\ s a -> s{_arnSnsPublishStatusCode = a})

-- | The message included in the notification.
arnMessage :: Lens' AssessmentRunNotification (Maybe Text)
arnMessage = lens _arnMessage (\ s a -> s{_arnMessage = a})

-- | The date of the notification.
arnDate :: Lens' AssessmentRunNotification UTCTime
arnDate = lens _arnDate (\ s a -> s{_arnDate = a}) . _Time

-- | The event for which a notification is sent.
arnEvent :: Lens' AssessmentRunNotification InspectorEvent
arnEvent = lens _arnEvent (\ s a -> s{_arnEvent = a})

-- | The Boolean value that specifies whether the notification represents an error.
arnError :: Lens' AssessmentRunNotification Bool
arnError = lens _arnError (\ s a -> s{_arnError = a})

instance FromJSON AssessmentRunNotification where
        parseJSON
          = withObject "AssessmentRunNotification"
              (\ x ->
                 AssessmentRunNotification' <$>
                   (x .:? "snsTopicArn") <*>
                     (x .:? "snsPublishStatusCode")
                     <*> (x .:? "message")
                     <*> (x .: "date")
                     <*> (x .: "event")
                     <*> (x .: "error"))

instance Hashable AssessmentRunNotification where

instance NFData AssessmentRunNotification where

-- | Used as one of the elements of the 'AssessmentRun' data type.
--
--
--
-- /See:/ 'assessmentRunStateChange' smart constructor.
data AssessmentRunStateChange = AssessmentRunStateChange'
  { _arscStateChangedAt :: !POSIX
  , _arscState          :: !AssessmentRunState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssessmentRunStateChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arscStateChangedAt' - The last time the assessment run state changed.
--
-- * 'arscState' - The assessment run state.
assessmentRunStateChange
    :: UTCTime -- ^ 'arscStateChangedAt'
    -> AssessmentRunState -- ^ 'arscState'
    -> AssessmentRunStateChange
assessmentRunStateChange pStateChangedAt_ pState_ =
  AssessmentRunStateChange'
    {_arscStateChangedAt = _Time # pStateChangedAt_, _arscState = pState_}


-- | The last time the assessment run state changed.
arscStateChangedAt :: Lens' AssessmentRunStateChange UTCTime
arscStateChangedAt = lens _arscStateChangedAt (\ s a -> s{_arscStateChangedAt = a}) . _Time

-- | The assessment run state.
arscState :: Lens' AssessmentRunStateChange AssessmentRunState
arscState = lens _arscState (\ s a -> s{_arscState = a})

instance FromJSON AssessmentRunStateChange where
        parseJSON
          = withObject "AssessmentRunStateChange"
              (\ x ->
                 AssessmentRunStateChange' <$>
                   (x .: "stateChangedAt") <*> (x .: "state"))

instance Hashable AssessmentRunStateChange where

instance NFData AssessmentRunStateChange where

-- | Contains information about an Amazon Inspector application. This data type is used as the response element in the 'DescribeAssessmentTargets' action.
--
--
--
-- /See:/ 'assessmentTarget' smart constructor.
data AssessmentTarget = AssessmentTarget'
  { _aArn              :: !Text
  , _aName             :: !Text
  , _aResourceGroupARN :: !Text
  , _aCreatedAt        :: !POSIX
  , _aUpdatedAt        :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssessmentTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aArn' - The ARN that specifies the Amazon Inspector assessment target.
--
-- * 'aName' - The name of the Amazon Inspector assessment target.
--
-- * 'aResourceGroupARN' - The ARN that specifies the resource group that is associated with the assessment target.
--
-- * 'aCreatedAt' - The time at which the assessment target is created.
--
-- * 'aUpdatedAt' - The time at which 'UpdateAssessmentTarget' is called.
assessmentTarget
    :: Text -- ^ 'aArn'
    -> Text -- ^ 'aName'
    -> Text -- ^ 'aResourceGroupARN'
    -> UTCTime -- ^ 'aCreatedAt'
    -> UTCTime -- ^ 'aUpdatedAt'
    -> AssessmentTarget
assessmentTarget pArn_ pName_ pResourceGroupARN_ pCreatedAt_ pUpdatedAt_ =
  AssessmentTarget'
    { _aArn = pArn_
    , _aName = pName_
    , _aResourceGroupARN = pResourceGroupARN_
    , _aCreatedAt = _Time # pCreatedAt_
    , _aUpdatedAt = _Time # pUpdatedAt_
    }


-- | The ARN that specifies the Amazon Inspector assessment target.
aArn :: Lens' AssessmentTarget Text
aArn = lens _aArn (\ s a -> s{_aArn = a})

-- | The name of the Amazon Inspector assessment target.
aName :: Lens' AssessmentTarget Text
aName = lens _aName (\ s a -> s{_aName = a})

-- | The ARN that specifies the resource group that is associated with the assessment target.
aResourceGroupARN :: Lens' AssessmentTarget Text
aResourceGroupARN = lens _aResourceGroupARN (\ s a -> s{_aResourceGroupARN = a})

-- | The time at which the assessment target is created.
aCreatedAt :: Lens' AssessmentTarget UTCTime
aCreatedAt = lens _aCreatedAt (\ s a -> s{_aCreatedAt = a}) . _Time

-- | The time at which 'UpdateAssessmentTarget' is called.
aUpdatedAt :: Lens' AssessmentTarget UTCTime
aUpdatedAt = lens _aUpdatedAt (\ s a -> s{_aUpdatedAt = a}) . _Time

instance FromJSON AssessmentTarget where
        parseJSON
          = withObject "AssessmentTarget"
              (\ x ->
                 AssessmentTarget' <$>
                   (x .: "arn") <*> (x .: "name") <*>
                     (x .: "resourceGroupArn")
                     <*> (x .: "createdAt")
                     <*> (x .: "updatedAt"))

instance Hashable AssessmentTarget where

instance NFData AssessmentTarget where

-- | Used as the request parameter in the 'ListAssessmentTargets' action.
--
--
--
-- /See:/ 'assessmentTargetFilter' smart constructor.
newtype AssessmentTargetFilter = AssessmentTargetFilter'
  { _atfAssessmentTargetNamePattern :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssessmentTargetFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atfAssessmentTargetNamePattern' - For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTargetName__ property of the 'AssessmentTarget' data type.
assessmentTargetFilter
    :: AssessmentTargetFilter
assessmentTargetFilter =
  AssessmentTargetFilter' {_atfAssessmentTargetNamePattern = Nothing}


-- | For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTargetName__ property of the 'AssessmentTarget' data type.
atfAssessmentTargetNamePattern :: Lens' AssessmentTargetFilter (Maybe Text)
atfAssessmentTargetNamePattern = lens _atfAssessmentTargetNamePattern (\ s a -> s{_atfAssessmentTargetNamePattern = a})

instance Hashable AssessmentTargetFilter where

instance NFData AssessmentTargetFilter where

instance ToJSON AssessmentTargetFilter where
        toJSON AssessmentTargetFilter'{..}
          = object
              (catMaybes
                 [("assessmentTargetNamePattern" .=) <$>
                    _atfAssessmentTargetNamePattern])

-- | Contains information about an Amazon Inspector assessment template. This data type is used as the response element in the 'DescribeAssessmentTemplates' action.
--
--
--
-- /See:/ 'assessmentTemplate' smart constructor.
data AssessmentTemplate = AssessmentTemplate'
  { _atLastAssessmentRunARN      :: !(Maybe Text)
  , _atArn                       :: !Text
  , _atName                      :: !Text
  , _atAssessmentTargetARN       :: !Text
  , _atDurationInSeconds         :: !Nat
  , _atRulesPackageARNs          :: ![Text]
  , _atUserAttributesForFindings :: ![Attribute]
  , _atAssessmentRunCount        :: !Int
  , _atCreatedAt                 :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssessmentTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atLastAssessmentRunARN' - The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greater than zero.
--
-- * 'atArn' - The ARN of the assessment template.
--
-- * 'atName' - The name of the assessment template.
--
-- * 'atAssessmentTargetARN' - The ARN of the assessment target that corresponds to this assessment template.
--
-- * 'atDurationInSeconds' - The duration in seconds specified for this assessment tempate. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
--
-- * 'atRulesPackageARNs' - The rules packages that are specified for this assessment template.
--
-- * 'atUserAttributesForFindings' - The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
--
-- * 'atAssessmentRunCount' - The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
--
-- * 'atCreatedAt' - The time at which the assessment template is created.
assessmentTemplate
    :: Text -- ^ 'atArn'
    -> Text -- ^ 'atName'
    -> Text -- ^ 'atAssessmentTargetARN'
    -> Natural -- ^ 'atDurationInSeconds'
    -> Int -- ^ 'atAssessmentRunCount'
    -> UTCTime -- ^ 'atCreatedAt'
    -> AssessmentTemplate
assessmentTemplate pArn_ pName_ pAssessmentTargetARN_ pDurationInSeconds_ pAssessmentRunCount_ pCreatedAt_ =
  AssessmentTemplate'
    { _atLastAssessmentRunARN = Nothing
    , _atArn = pArn_
    , _atName = pName_
    , _atAssessmentTargetARN = pAssessmentTargetARN_
    , _atDurationInSeconds = _Nat # pDurationInSeconds_
    , _atRulesPackageARNs = mempty
    , _atUserAttributesForFindings = mempty
    , _atAssessmentRunCount = pAssessmentRunCount_
    , _atCreatedAt = _Time # pCreatedAt_
    }


-- | The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greater than zero.
atLastAssessmentRunARN :: Lens' AssessmentTemplate (Maybe Text)
atLastAssessmentRunARN = lens _atLastAssessmentRunARN (\ s a -> s{_atLastAssessmentRunARN = a})

-- | The ARN of the assessment template.
atArn :: Lens' AssessmentTemplate Text
atArn = lens _atArn (\ s a -> s{_atArn = a})

-- | The name of the assessment template.
atName :: Lens' AssessmentTemplate Text
atName = lens _atName (\ s a -> s{_atName = a})

-- | The ARN of the assessment target that corresponds to this assessment template.
atAssessmentTargetARN :: Lens' AssessmentTemplate Text
atAssessmentTargetARN = lens _atAssessmentTargetARN (\ s a -> s{_atAssessmentTargetARN = a})

-- | The duration in seconds specified for this assessment tempate. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
atDurationInSeconds :: Lens' AssessmentTemplate Natural
atDurationInSeconds = lens _atDurationInSeconds (\ s a -> s{_atDurationInSeconds = a}) . _Nat

-- | The rules packages that are specified for this assessment template.
atRulesPackageARNs :: Lens' AssessmentTemplate [Text]
atRulesPackageARNs = lens _atRulesPackageARNs (\ s a -> s{_atRulesPackageARNs = a}) . _Coerce

-- | The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
atUserAttributesForFindings :: Lens' AssessmentTemplate [Attribute]
atUserAttributesForFindings = lens _atUserAttributesForFindings (\ s a -> s{_atUserAttributesForFindings = a}) . _Coerce

-- | The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
atAssessmentRunCount :: Lens' AssessmentTemplate Int
atAssessmentRunCount = lens _atAssessmentRunCount (\ s a -> s{_atAssessmentRunCount = a})

-- | The time at which the assessment template is created.
atCreatedAt :: Lens' AssessmentTemplate UTCTime
atCreatedAt = lens _atCreatedAt (\ s a -> s{_atCreatedAt = a}) . _Time

instance FromJSON AssessmentTemplate where
        parseJSON
          = withObject "AssessmentTemplate"
              (\ x ->
                 AssessmentTemplate' <$>
                   (x .:? "lastAssessmentRunArn") <*> (x .: "arn") <*>
                     (x .: "name")
                     <*> (x .: "assessmentTargetArn")
                     <*> (x .: "durationInSeconds")
                     <*> (x .:? "rulesPackageArns" .!= mempty)
                     <*> (x .:? "userAttributesForFindings" .!= mempty)
                     <*> (x .: "assessmentRunCount")
                     <*> (x .: "createdAt"))

instance Hashable AssessmentTemplate where

instance NFData AssessmentTemplate where

-- | Used as the request parameter in the 'ListAssessmentTemplates' action.
--
--
--
-- /See:/ 'assessmentTemplateFilter' smart constructor.
data AssessmentTemplateFilter = AssessmentTemplateFilter'
  { _atfNamePattern      :: !(Maybe Text)
  , _atfRulesPackageARNs :: !(Maybe [Text])
  , _atfDurationRange    :: !(Maybe DurationRange)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssessmentTemplateFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atfNamePattern' - For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTemplateName__ property of the 'AssessmentTemplate' data type.
--
-- * 'atfRulesPackageARNs' - For a record to match a filter, the values that are specified for this data type property must be contained in the list of values of the __rulesPackageArns__ property of the 'AssessmentTemplate' data type.
--
-- * 'atfDurationRange' - For a record to match a filter, the value specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentTemplate' data type.
assessmentTemplateFilter
    :: AssessmentTemplateFilter
assessmentTemplateFilter =
  AssessmentTemplateFilter'
    { _atfNamePattern = Nothing
    , _atfRulesPackageARNs = Nothing
    , _atfDurationRange = Nothing
    }


-- | For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTemplateName__ property of the 'AssessmentTemplate' data type.
atfNamePattern :: Lens' AssessmentTemplateFilter (Maybe Text)
atfNamePattern = lens _atfNamePattern (\ s a -> s{_atfNamePattern = a})

-- | For a record to match a filter, the values that are specified for this data type property must be contained in the list of values of the __rulesPackageArns__ property of the 'AssessmentTemplate' data type.
atfRulesPackageARNs :: Lens' AssessmentTemplateFilter [Text]
atfRulesPackageARNs = lens _atfRulesPackageARNs (\ s a -> s{_atfRulesPackageARNs = a}) . _Default . _Coerce

-- | For a record to match a filter, the value specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentTemplate' data type.
atfDurationRange :: Lens' AssessmentTemplateFilter (Maybe DurationRange)
atfDurationRange = lens _atfDurationRange (\ s a -> s{_atfDurationRange = a})

instance Hashable AssessmentTemplateFilter where

instance NFData AssessmentTemplateFilter where

instance ToJSON AssessmentTemplateFilter where
        toJSON AssessmentTemplateFilter'{..}
          = object
              (catMaybes
                 [("namePattern" .=) <$> _atfNamePattern,
                  ("rulesPackageArns" .=) <$> _atfRulesPackageARNs,
                  ("durationRange" .=) <$> _atfDurationRange])

-- | A collection of attributes of the host from which the finding is generated.
--
--
--
-- /See:/ 'assetAttributes' smart constructor.
data AssetAttributes = AssetAttributes'
  { _aaHostname         :: !(Maybe Text)
  , _aaAutoScalingGroup :: !(Maybe Text)
  , _aaIpv4Addresses    :: !(Maybe [Text])
  , _aaAgentId          :: !(Maybe Text)
  , _aaAmiId            :: !(Maybe Text)
  , _aaSchemaVersion    :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaHostname' - The hostname of the EC2 instance where the finding is generated.
--
-- * 'aaAutoScalingGroup' - The Auto Scaling group of the EC2 instance where the finding is generated.
--
-- * 'aaIpv4Addresses' - The list of IP v4 addresses of the EC2 instance where the finding is generated.
--
-- * 'aaAgentId' - The ID of the agent that is installed on the EC2 instance where the finding is generated.
--
-- * 'aaAmiId' - The ID of the Amazon Machine Image (AMI) that is installed on the EC2 instance where the finding is generated.
--
-- * 'aaSchemaVersion' - The schema version of this data type.
assetAttributes
    :: Natural -- ^ 'aaSchemaVersion'
    -> AssetAttributes
assetAttributes pSchemaVersion_ =
  AssetAttributes'
    { _aaHostname = Nothing
    , _aaAutoScalingGroup = Nothing
    , _aaIpv4Addresses = Nothing
    , _aaAgentId = Nothing
    , _aaAmiId = Nothing
    , _aaSchemaVersion = _Nat # pSchemaVersion_
    }


-- | The hostname of the EC2 instance where the finding is generated.
aaHostname :: Lens' AssetAttributes (Maybe Text)
aaHostname = lens _aaHostname (\ s a -> s{_aaHostname = a})

-- | The Auto Scaling group of the EC2 instance where the finding is generated.
aaAutoScalingGroup :: Lens' AssetAttributes (Maybe Text)
aaAutoScalingGroup = lens _aaAutoScalingGroup (\ s a -> s{_aaAutoScalingGroup = a})

-- | The list of IP v4 addresses of the EC2 instance where the finding is generated.
aaIpv4Addresses :: Lens' AssetAttributes [Text]
aaIpv4Addresses = lens _aaIpv4Addresses (\ s a -> s{_aaIpv4Addresses = a}) . _Default . _Coerce

-- | The ID of the agent that is installed on the EC2 instance where the finding is generated.
aaAgentId :: Lens' AssetAttributes (Maybe Text)
aaAgentId = lens _aaAgentId (\ s a -> s{_aaAgentId = a})

-- | The ID of the Amazon Machine Image (AMI) that is installed on the EC2 instance where the finding is generated.
aaAmiId :: Lens' AssetAttributes (Maybe Text)
aaAmiId = lens _aaAmiId (\ s a -> s{_aaAmiId = a})

-- | The schema version of this data type.
aaSchemaVersion :: Lens' AssetAttributes Natural
aaSchemaVersion = lens _aaSchemaVersion (\ s a -> s{_aaSchemaVersion = a}) . _Nat

instance FromJSON AssetAttributes where
        parseJSON
          = withObject "AssetAttributes"
              (\ x ->
                 AssetAttributes' <$>
                   (x .:? "hostname") <*> (x .:? "autoScalingGroup") <*>
                     (x .:? "ipv4Addresses" .!= mempty)
                     <*> (x .:? "agentId")
                     <*> (x .:? "amiId")
                     <*> (x .: "schemaVersion"))

instance Hashable AssetAttributes where

instance NFData AssetAttributes where

-- | This data type is used as a request parameter in the 'AddAttributesToFindings' and 'CreateAssessmentTemplate' actions.
--
--
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
  { _aValue :: !(Maybe Text)
  , _aKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aValue' - The value assigned to the attribute key.
--
-- * 'aKey' - The attribute key.
attribute
    :: Text -- ^ 'aKey'
    -> Attribute
attribute pKey_ = Attribute' {_aValue = Nothing, _aKey = pKey_}


-- | The value assigned to the attribute key.
aValue :: Lens' Attribute (Maybe Text)
aValue = lens _aValue (\ s a -> s{_aValue = a})

-- | The attribute key.
aKey :: Lens' Attribute Text
aKey = lens _aKey (\ s a -> s{_aKey = a})

instance FromJSON Attribute where
        parseJSON
          = withObject "Attribute"
              (\ x ->
                 Attribute' <$> (x .:? "value") <*> (x .: "key"))

instance Hashable Attribute where

instance NFData Attribute where

instance ToJSON Attribute where
        toJSON Attribute'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _aValue, Just ("key" .= _aKey)])

-- | This data type is used in the 'AssessmentTemplateFilter' data type.
--
--
--
-- /See:/ 'durationRange' smart constructor.
data DurationRange = DurationRange'
  { _drMinSeconds :: !(Maybe Nat)
  , _drMaxSeconds :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DurationRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drMinSeconds' - The minimum value of the duration range. Must be greater than zero.
--
-- * 'drMaxSeconds' - The maximum value of the duration range. Must be less than or equal to 604800 seconds (1 week).
durationRange
    :: DurationRange
durationRange =
  DurationRange' {_drMinSeconds = Nothing, _drMaxSeconds = Nothing}


-- | The minimum value of the duration range. Must be greater than zero.
drMinSeconds :: Lens' DurationRange (Maybe Natural)
drMinSeconds = lens _drMinSeconds (\ s a -> s{_drMinSeconds = a}) . mapping _Nat

-- | The maximum value of the duration range. Must be less than or equal to 604800 seconds (1 week).
drMaxSeconds :: Lens' DurationRange (Maybe Natural)
drMaxSeconds = lens _drMaxSeconds (\ s a -> s{_drMaxSeconds = a}) . mapping _Nat

instance Hashable DurationRange where

instance NFData DurationRange where

instance ToJSON DurationRange where
        toJSON DurationRange'{..}
          = object
              (catMaybes
                 [("minSeconds" .=) <$> _drMinSeconds,
                  ("maxSeconds" .=) <$> _drMaxSeconds])

-- | This data type is used in the 'Subscription' data type.
--
--
--
-- /See:/ 'eventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { _esEvent        :: !InspectorEvent
  , _esSubscribedAt :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esEvent' - The event for which Amazon Simple Notification Service (SNS) notifications are sent.
--
-- * 'esSubscribedAt' - The time at which 'SubscribeToEvent' is called.
eventSubscription
    :: InspectorEvent -- ^ 'esEvent'
    -> UTCTime -- ^ 'esSubscribedAt'
    -> EventSubscription
eventSubscription pEvent_ pSubscribedAt_ =
  EventSubscription'
    {_esEvent = pEvent_, _esSubscribedAt = _Time # pSubscribedAt_}


-- | The event for which Amazon Simple Notification Service (SNS) notifications are sent.
esEvent :: Lens' EventSubscription InspectorEvent
esEvent = lens _esEvent (\ s a -> s{_esEvent = a})

-- | The time at which 'SubscribeToEvent' is called.
esSubscribedAt :: Lens' EventSubscription UTCTime
esSubscribedAt = lens _esSubscribedAt (\ s a -> s{_esSubscribedAt = a}) . _Time

instance FromJSON EventSubscription where
        parseJSON
          = withObject "EventSubscription"
              (\ x ->
                 EventSubscription' <$>
                   (x .: "event") <*> (x .: "subscribedAt"))

instance Hashable EventSubscription where

instance NFData EventSubscription where

-- | Includes details about the failed items.
--
--
--
-- /See:/ 'failedItemDetails' smart constructor.
data FailedItemDetails = FailedItemDetails'
  { _fidFailureCode :: !FailedItemErrorCode
  , _fidRetryable   :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FailedItemDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fidFailureCode' - The status code of a failed item.
--
-- * 'fidRetryable' - Indicates whether you can immediately retry a request for this item for a specified resource.
failedItemDetails
    :: FailedItemErrorCode -- ^ 'fidFailureCode'
    -> Bool -- ^ 'fidRetryable'
    -> FailedItemDetails
failedItemDetails pFailureCode_ pRetryable_ =
  FailedItemDetails'
    {_fidFailureCode = pFailureCode_, _fidRetryable = pRetryable_}


-- | The status code of a failed item.
fidFailureCode :: Lens' FailedItemDetails FailedItemErrorCode
fidFailureCode = lens _fidFailureCode (\ s a -> s{_fidFailureCode = a})

-- | Indicates whether you can immediately retry a request for this item for a specified resource.
fidRetryable :: Lens' FailedItemDetails Bool
fidRetryable = lens _fidRetryable (\ s a -> s{_fidRetryable = a})

instance FromJSON FailedItemDetails where
        parseJSON
          = withObject "FailedItemDetails"
              (\ x ->
                 FailedItemDetails' <$>
                   (x .: "failureCode") <*> (x .: "retryable"))

instance Hashable FailedItemDetails where

instance NFData FailedItemDetails where

-- | Contains information about an Amazon Inspector finding. This data type is used as the response element in the 'DescribeFindings' action.
--
--
--
-- /See:/ 'finding' smart constructor.
data Finding = Finding'
  { _fService               :: !(Maybe Text)
  , _fSeverity              :: !(Maybe Severity)
  , _fSchemaVersion         :: !(Maybe Nat)
  , _fConfidence            :: !(Maybe Nat)
  , _fAssetAttributes       :: !(Maybe AssetAttributes)
  , _fServiceAttributes     :: !(Maybe InspectorServiceAttributes)
  , _fId                    :: !(Maybe Text)
  , _fNumericSeverity       :: !(Maybe Double)
  , _fAssetType             :: !(Maybe AssetType)
  , _fTitle                 :: !(Maybe Text)
  , _fIndicatorOfCompromise :: !(Maybe Bool)
  , _fDescription           :: !(Maybe Text)
  , _fRecommendation        :: !(Maybe Text)
  , _fArn                   :: !Text
  , _fAttributes            :: ![Attribute]
  , _fUserAttributes        :: ![Attribute]
  , _fCreatedAt             :: !POSIX
  , _fUpdatedAt             :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Finding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fService' - The data element is set to "Inspector".
--
-- * 'fSeverity' - The finding severity. Values can be set to High, Medium, Low, and Informational.
--
-- * 'fSchemaVersion' - The schema version of this data type.
--
-- * 'fConfidence' - This data element is currently not used.
--
-- * 'fAssetAttributes' - A collection of attributes of the host from which the finding is generated.
--
-- * 'fServiceAttributes' - This data type is used in the 'Finding' data type.
--
-- * 'fId' - The ID of the finding.
--
-- * 'fNumericSeverity' - The numeric value of the finding severity.
--
-- * 'fAssetType' - The type of the host from which the finding is generated.
--
-- * 'fTitle' - The name of the finding.
--
-- * 'fIndicatorOfCompromise' - This data element is currently not used.
--
-- * 'fDescription' - The description of the finding.
--
-- * 'fRecommendation' - The recommendation for the finding.
--
-- * 'fArn' - The ARN that specifies the finding.
--
-- * 'fAttributes' - The system-defined attributes for the finding.
--
-- * 'fUserAttributes' - The user-defined attributes that are assigned to the finding.
--
-- * 'fCreatedAt' - The time when the finding was generated.
--
-- * 'fUpdatedAt' - The time when 'AddAttributesToFindings' is called.
finding
    :: Text -- ^ 'fArn'
    -> UTCTime -- ^ 'fCreatedAt'
    -> UTCTime -- ^ 'fUpdatedAt'
    -> Finding
finding pArn_ pCreatedAt_ pUpdatedAt_ =
  Finding'
    { _fService = Nothing
    , _fSeverity = Nothing
    , _fSchemaVersion = Nothing
    , _fConfidence = Nothing
    , _fAssetAttributes = Nothing
    , _fServiceAttributes = Nothing
    , _fId = Nothing
    , _fNumericSeverity = Nothing
    , _fAssetType = Nothing
    , _fTitle = Nothing
    , _fIndicatorOfCompromise = Nothing
    , _fDescription = Nothing
    , _fRecommendation = Nothing
    , _fArn = pArn_
    , _fAttributes = mempty
    , _fUserAttributes = mempty
    , _fCreatedAt = _Time # pCreatedAt_
    , _fUpdatedAt = _Time # pUpdatedAt_
    }


-- | The data element is set to "Inspector".
fService :: Lens' Finding (Maybe Text)
fService = lens _fService (\ s a -> s{_fService = a})

-- | The finding severity. Values can be set to High, Medium, Low, and Informational.
fSeverity :: Lens' Finding (Maybe Severity)
fSeverity = lens _fSeverity (\ s a -> s{_fSeverity = a})

-- | The schema version of this data type.
fSchemaVersion :: Lens' Finding (Maybe Natural)
fSchemaVersion = lens _fSchemaVersion (\ s a -> s{_fSchemaVersion = a}) . mapping _Nat

-- | This data element is currently not used.
fConfidence :: Lens' Finding (Maybe Natural)
fConfidence = lens _fConfidence (\ s a -> s{_fConfidence = a}) . mapping _Nat

-- | A collection of attributes of the host from which the finding is generated.
fAssetAttributes :: Lens' Finding (Maybe AssetAttributes)
fAssetAttributes = lens _fAssetAttributes (\ s a -> s{_fAssetAttributes = a})

-- | This data type is used in the 'Finding' data type.
fServiceAttributes :: Lens' Finding (Maybe InspectorServiceAttributes)
fServiceAttributes = lens _fServiceAttributes (\ s a -> s{_fServiceAttributes = a})

-- | The ID of the finding.
fId :: Lens' Finding (Maybe Text)
fId = lens _fId (\ s a -> s{_fId = a})

-- | The numeric value of the finding severity.
fNumericSeverity :: Lens' Finding (Maybe Double)
fNumericSeverity = lens _fNumericSeverity (\ s a -> s{_fNumericSeverity = a})

-- | The type of the host from which the finding is generated.
fAssetType :: Lens' Finding (Maybe AssetType)
fAssetType = lens _fAssetType (\ s a -> s{_fAssetType = a})

-- | The name of the finding.
fTitle :: Lens' Finding (Maybe Text)
fTitle = lens _fTitle (\ s a -> s{_fTitle = a})

-- | This data element is currently not used.
fIndicatorOfCompromise :: Lens' Finding (Maybe Bool)
fIndicatorOfCompromise = lens _fIndicatorOfCompromise (\ s a -> s{_fIndicatorOfCompromise = a})

-- | The description of the finding.
fDescription :: Lens' Finding (Maybe Text)
fDescription = lens _fDescription (\ s a -> s{_fDescription = a})

-- | The recommendation for the finding.
fRecommendation :: Lens' Finding (Maybe Text)
fRecommendation = lens _fRecommendation (\ s a -> s{_fRecommendation = a})

-- | The ARN that specifies the finding.
fArn :: Lens' Finding Text
fArn = lens _fArn (\ s a -> s{_fArn = a})

-- | The system-defined attributes for the finding.
fAttributes :: Lens' Finding [Attribute]
fAttributes = lens _fAttributes (\ s a -> s{_fAttributes = a}) . _Coerce

-- | The user-defined attributes that are assigned to the finding.
fUserAttributes :: Lens' Finding [Attribute]
fUserAttributes = lens _fUserAttributes (\ s a -> s{_fUserAttributes = a}) . _Coerce

-- | The time when the finding was generated.
fCreatedAt :: Lens' Finding UTCTime
fCreatedAt = lens _fCreatedAt (\ s a -> s{_fCreatedAt = a}) . _Time

-- | The time when 'AddAttributesToFindings' is called.
fUpdatedAt :: Lens' Finding UTCTime
fUpdatedAt = lens _fUpdatedAt (\ s a -> s{_fUpdatedAt = a}) . _Time

instance FromJSON Finding where
        parseJSON
          = withObject "Finding"
              (\ x ->
                 Finding' <$>
                   (x .:? "service") <*> (x .:? "severity") <*>
                     (x .:? "schemaVersion")
                     <*> (x .:? "confidence")
                     <*> (x .:? "assetAttributes")
                     <*> (x .:? "serviceAttributes")
                     <*> (x .:? "id")
                     <*> (x .:? "numericSeverity")
                     <*> (x .:? "assetType")
                     <*> (x .:? "title")
                     <*> (x .:? "indicatorOfCompromise")
                     <*> (x .:? "description")
                     <*> (x .:? "recommendation")
                     <*> (x .: "arn")
                     <*> (x .:? "attributes" .!= mempty)
                     <*> (x .:? "userAttributes" .!= mempty)
                     <*> (x .: "createdAt")
                     <*> (x .: "updatedAt"))

instance Hashable Finding where

instance NFData Finding where

-- | This data type is used as a request parameter in the 'ListFindings' action.
--
--
--
-- /See:/ 'findingFilter' smart constructor.
data FindingFilter = FindingFilter'
  { _ffAgentIds          :: !(Maybe [Text])
  , _ffRuleNames         :: !(Maybe [Text])
  , _ffUserAttributes    :: !(Maybe [Attribute])
  , _ffRulesPackageARNs  :: !(Maybe [Text])
  , _ffAttributes        :: !(Maybe [Attribute])
  , _ffSeverities        :: !(Maybe [Severity])
  , _ffCreationTimeRange :: !(Maybe TimestampRange)
  , _ffAutoScalingGroups :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FindingFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ffAgentIds' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __agentId__ property of the 'Finding' data type.
--
-- * 'ffRuleNames' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __ruleName__ property of the 'Finding' data type.
--
-- * 'ffUserAttributes' - For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __userAttributes__ property of the 'Finding' data type.
--
-- * 'ffRulesPackageARNs' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __rulesPackageArn__ property of the 'Finding' data type.
--
-- * 'ffAttributes' - For a record to match a filter, the list of values that are specified for this data type property must be contained in the list of values of the __attributes__ property of the 'Finding' data type.
--
-- * 'ffSeverities' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __severity__ property of the 'Finding' data type.
--
-- * 'ffCreationTimeRange' - The time range during which the finding is generated.
--
-- * 'ffAutoScalingGroups' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __autoScalingGroup__ property of the 'Finding' data type.
findingFilter
    :: FindingFilter
findingFilter =
  FindingFilter'
    { _ffAgentIds = Nothing
    , _ffRuleNames = Nothing
    , _ffUserAttributes = Nothing
    , _ffRulesPackageARNs = Nothing
    , _ffAttributes = Nothing
    , _ffSeverities = Nothing
    , _ffCreationTimeRange = Nothing
    , _ffAutoScalingGroups = Nothing
    }


-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __agentId__ property of the 'Finding' data type.
ffAgentIds :: Lens' FindingFilter [Text]
ffAgentIds = lens _ffAgentIds (\ s a -> s{_ffAgentIds = a}) . _Default . _Coerce

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __ruleName__ property of the 'Finding' data type.
ffRuleNames :: Lens' FindingFilter [Text]
ffRuleNames = lens _ffRuleNames (\ s a -> s{_ffRuleNames = a}) . _Default . _Coerce

-- | For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __userAttributes__ property of the 'Finding' data type.
ffUserAttributes :: Lens' FindingFilter [Attribute]
ffUserAttributes = lens _ffUserAttributes (\ s a -> s{_ffUserAttributes = a}) . _Default . _Coerce

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __rulesPackageArn__ property of the 'Finding' data type.
ffRulesPackageARNs :: Lens' FindingFilter [Text]
ffRulesPackageARNs = lens _ffRulesPackageARNs (\ s a -> s{_ffRulesPackageARNs = a}) . _Default . _Coerce

-- | For a record to match a filter, the list of values that are specified for this data type property must be contained in the list of values of the __attributes__ property of the 'Finding' data type.
ffAttributes :: Lens' FindingFilter [Attribute]
ffAttributes = lens _ffAttributes (\ s a -> s{_ffAttributes = a}) . _Default . _Coerce

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __severity__ property of the 'Finding' data type.
ffSeverities :: Lens' FindingFilter [Severity]
ffSeverities = lens _ffSeverities (\ s a -> s{_ffSeverities = a}) . _Default . _Coerce

-- | The time range during which the finding is generated.
ffCreationTimeRange :: Lens' FindingFilter (Maybe TimestampRange)
ffCreationTimeRange = lens _ffCreationTimeRange (\ s a -> s{_ffCreationTimeRange = a})

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __autoScalingGroup__ property of the 'Finding' data type.
ffAutoScalingGroups :: Lens' FindingFilter [Text]
ffAutoScalingGroups = lens _ffAutoScalingGroups (\ s a -> s{_ffAutoScalingGroups = a}) . _Default . _Coerce

instance Hashable FindingFilter where

instance NFData FindingFilter where

instance ToJSON FindingFilter where
        toJSON FindingFilter'{..}
          = object
              (catMaybes
                 [("agentIds" .=) <$> _ffAgentIds,
                  ("ruleNames" .=) <$> _ffRuleNames,
                  ("userAttributes" .=) <$> _ffUserAttributes,
                  ("rulesPackageArns" .=) <$> _ffRulesPackageARNs,
                  ("attributes" .=) <$> _ffAttributes,
                  ("severities" .=) <$> _ffSeverities,
                  ("creationTimeRange" .=) <$> _ffCreationTimeRange,
                  ("autoScalingGroups" .=) <$> _ffAutoScalingGroups])

-- | This data type is used in the 'Finding' data type.
--
--
--
-- /See:/ 'inspectorServiceAttributes' smart constructor.
data InspectorServiceAttributes = InspectorServiceAttributes'
  { _isaRulesPackageARN  :: !(Maybe Text)
  , _isaAssessmentRunARN :: !(Maybe Text)
  , _isaSchemaVersion    :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InspectorServiceAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isaRulesPackageARN' - The ARN of the rules package that is used to generate the finding.
--
-- * 'isaAssessmentRunARN' - The ARN of the assessment run during which the finding is generated.
--
-- * 'isaSchemaVersion' - The schema version of this data type.
inspectorServiceAttributes
    :: Natural -- ^ 'isaSchemaVersion'
    -> InspectorServiceAttributes
inspectorServiceAttributes pSchemaVersion_ =
  InspectorServiceAttributes'
    { _isaRulesPackageARN = Nothing
    , _isaAssessmentRunARN = Nothing
    , _isaSchemaVersion = _Nat # pSchemaVersion_
    }


-- | The ARN of the rules package that is used to generate the finding.
isaRulesPackageARN :: Lens' InspectorServiceAttributes (Maybe Text)
isaRulesPackageARN = lens _isaRulesPackageARN (\ s a -> s{_isaRulesPackageARN = a})

-- | The ARN of the assessment run during which the finding is generated.
isaAssessmentRunARN :: Lens' InspectorServiceAttributes (Maybe Text)
isaAssessmentRunARN = lens _isaAssessmentRunARN (\ s a -> s{_isaAssessmentRunARN = a})

-- | The schema version of this data type.
isaSchemaVersion :: Lens' InspectorServiceAttributes Natural
isaSchemaVersion = lens _isaSchemaVersion (\ s a -> s{_isaSchemaVersion = a}) . _Nat

instance FromJSON InspectorServiceAttributes where
        parseJSON
          = withObject "InspectorServiceAttributes"
              (\ x ->
                 InspectorServiceAttributes' <$>
                   (x .:? "rulesPackageArn") <*>
                     (x .:? "assessmentRunArn")
                     <*> (x .: "schemaVersion"))

instance Hashable InspectorServiceAttributes where

instance NFData InspectorServiceAttributes where

-- | Contains information about a resource group. The resource group defines a set of tags that, when queried, identify the AWS resources that make up the assessment target. This data type is used as the response element in the 'DescribeResourceGroups' action.
--
--
--
-- /See:/ 'resourceGroup' smart constructor.
data ResourceGroup = ResourceGroup'
  { _rgArn       :: !Text
  , _rgTags      :: !(List1 ResourceGroupTag)
  , _rgCreatedAt :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgArn' - The ARN of the resource group.
--
-- * 'rgTags' - The tags (key and value pairs) of the resource group. This data type property is used in the 'CreateResourceGroup' action.
--
-- * 'rgCreatedAt' - The time at which resource group is created.
resourceGroup
    :: Text -- ^ 'rgArn'
    -> NonEmpty ResourceGroupTag -- ^ 'rgTags'
    -> UTCTime -- ^ 'rgCreatedAt'
    -> ResourceGroup
resourceGroup pArn_ pTags_ pCreatedAt_ =
  ResourceGroup'
    { _rgArn = pArn_
    , _rgTags = _List1 # pTags_
    , _rgCreatedAt = _Time # pCreatedAt_
    }


-- | The ARN of the resource group.
rgArn :: Lens' ResourceGroup Text
rgArn = lens _rgArn (\ s a -> s{_rgArn = a})

-- | The tags (key and value pairs) of the resource group. This data type property is used in the 'CreateResourceGroup' action.
rgTags :: Lens' ResourceGroup (NonEmpty ResourceGroupTag)
rgTags = lens _rgTags (\ s a -> s{_rgTags = a}) . _List1

-- | The time at which resource group is created.
rgCreatedAt :: Lens' ResourceGroup UTCTime
rgCreatedAt = lens _rgCreatedAt (\ s a -> s{_rgCreatedAt = a}) . _Time

instance FromJSON ResourceGroup where
        parseJSON
          = withObject "ResourceGroup"
              (\ x ->
                 ResourceGroup' <$>
                   (x .: "arn") <*> (x .: "tags") <*>
                     (x .: "createdAt"))

instance Hashable ResourceGroup where

instance NFData ResourceGroup where

-- | This data type is used as one of the elements of the 'ResourceGroup' data type.
--
--
--
-- /See:/ 'resourceGroupTag' smart constructor.
data ResourceGroupTag = ResourceGroupTag'
  { _rgtValue :: !(Maybe Text)
  , _rgtKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceGroupTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgtValue' - The value assigned to a tag key.
--
-- * 'rgtKey' - A tag key.
resourceGroupTag
    :: Text -- ^ 'rgtKey'
    -> ResourceGroupTag
resourceGroupTag pKey_ =
  ResourceGroupTag' {_rgtValue = Nothing, _rgtKey = pKey_}


-- | The value assigned to a tag key.
rgtValue :: Lens' ResourceGroupTag (Maybe Text)
rgtValue = lens _rgtValue (\ s a -> s{_rgtValue = a})

-- | A tag key.
rgtKey :: Lens' ResourceGroupTag Text
rgtKey = lens _rgtKey (\ s a -> s{_rgtKey = a})

instance FromJSON ResourceGroupTag where
        parseJSON
          = withObject "ResourceGroupTag"
              (\ x ->
                 ResourceGroupTag' <$>
                   (x .:? "value") <*> (x .: "key"))

instance Hashable ResourceGroupTag where

instance NFData ResourceGroupTag where

instance ToJSON ResourceGroupTag where
        toJSON ResourceGroupTag'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _rgtValue,
                  Just ("key" .= _rgtKey)])

-- | Contains information about an Amazon Inspector rules package. This data type is used as the response element in the 'DescribeRulesPackages' action.
--
--
--
-- /See:/ 'rulesPackage' smart constructor.
data RulesPackage = RulesPackage'
  { _rpDescription :: !(Maybe Text)
  , _rpArn         :: !Text
  , _rpName        :: !Text
  , _rpVersion     :: !Text
  , _rpProvider    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RulesPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpDescription' - The description of the rules package.
--
-- * 'rpArn' - The ARN of the rules package.
--
-- * 'rpName' - The name of the rules package.
--
-- * 'rpVersion' - The version ID of the rules package.
--
-- * 'rpProvider' - The provider of the rules package.
rulesPackage
    :: Text -- ^ 'rpArn'
    -> Text -- ^ 'rpName'
    -> Text -- ^ 'rpVersion'
    -> Text -- ^ 'rpProvider'
    -> RulesPackage
rulesPackage pArn_ pName_ pVersion_ pProvider_ =
  RulesPackage'
    { _rpDescription = Nothing
    , _rpArn = pArn_
    , _rpName = pName_
    , _rpVersion = pVersion_
    , _rpProvider = pProvider_
    }


-- | The description of the rules package.
rpDescription :: Lens' RulesPackage (Maybe Text)
rpDescription = lens _rpDescription (\ s a -> s{_rpDescription = a})

-- | The ARN of the rules package.
rpArn :: Lens' RulesPackage Text
rpArn = lens _rpArn (\ s a -> s{_rpArn = a})

-- | The name of the rules package.
rpName :: Lens' RulesPackage Text
rpName = lens _rpName (\ s a -> s{_rpName = a})

-- | The version ID of the rules package.
rpVersion :: Lens' RulesPackage Text
rpVersion = lens _rpVersion (\ s a -> s{_rpVersion = a})

-- | The provider of the rules package.
rpProvider :: Lens' RulesPackage Text
rpProvider = lens _rpProvider (\ s a -> s{_rpProvider = a})

instance FromJSON RulesPackage where
        parseJSON
          = withObject "RulesPackage"
              (\ x ->
                 RulesPackage' <$>
                   (x .:? "description") <*> (x .: "arn") <*>
                     (x .: "name")
                     <*> (x .: "version")
                     <*> (x .: "provider"))

instance Hashable RulesPackage where

instance NFData RulesPackage where

-- | This data type is used as a response element in the 'ListEventSubscriptions' action.
--
--
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sResourceARN        :: !Text
  , _sTopicARN           :: !Text
  , _sEventSubscriptions :: !(List1 EventSubscription)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sResourceARN' - The ARN of the assessment template that is used during the event for which the SNS notification is sent.
--
-- * 'sTopicARN' - The ARN of the Amazon Simple Notification Service (SNS) topic to which the SNS notifications are sent.
--
-- * 'sEventSubscriptions' - The list of existing event subscriptions.
subscription
    :: Text -- ^ 'sResourceARN'
    -> Text -- ^ 'sTopicARN'
    -> NonEmpty EventSubscription -- ^ 'sEventSubscriptions'
    -> Subscription
subscription pResourceARN_ pTopicARN_ pEventSubscriptions_ =
  Subscription'
    { _sResourceARN = pResourceARN_
    , _sTopicARN = pTopicARN_
    , _sEventSubscriptions = _List1 # pEventSubscriptions_
    }


-- | The ARN of the assessment template that is used during the event for which the SNS notification is sent.
sResourceARN :: Lens' Subscription Text
sResourceARN = lens _sResourceARN (\ s a -> s{_sResourceARN = a})

-- | The ARN of the Amazon Simple Notification Service (SNS) topic to which the SNS notifications are sent.
sTopicARN :: Lens' Subscription Text
sTopicARN = lens _sTopicARN (\ s a -> s{_sTopicARN = a})

-- | The list of existing event subscriptions.
sEventSubscriptions :: Lens' Subscription (NonEmpty EventSubscription)
sEventSubscriptions = lens _sEventSubscriptions (\ s a -> s{_sEventSubscriptions = a}) . _List1

instance FromJSON Subscription where
        parseJSON
          = withObject "Subscription"
              (\ x ->
                 Subscription' <$>
                   (x .: "resourceArn") <*> (x .: "topicArn") <*>
                     (x .: "eventSubscriptions"))

instance Hashable Subscription where

instance NFData Subscription where

-- | A key and value pair. This data type is used as a request parameter in the 'SetTagsForResource' action and a response element in the 'ListTagsForResource' action.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - A value assigned to a tag key.
--
-- * 'tagKey' - A tag key.
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | A value assigned to a tag key.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | A tag key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "value") <*> (x .: "key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _tagValue,
                  Just ("key" .= _tagKey)])

-- | The metadata about the Amazon Inspector application data metrics collected by the agent. This data type is used as the response element in the 'GetTelemetryMetadata' action.
--
--
--
-- /See:/ 'telemetryMetadata' smart constructor.
data TelemetryMetadata = TelemetryMetadata'
  { _tmDataSize    :: !(Maybe Integer)
  , _tmMessageType :: !Text
  , _tmCount       :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TelemetryMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmDataSize' - The data size of messages that the agent sends to the Amazon Inspector service.
--
-- * 'tmMessageType' - A specific type of behavioral data that is collected by the agent.
--
-- * 'tmCount' - The count of messages that the agent sends to the Amazon Inspector service.
telemetryMetadata
    :: Text -- ^ 'tmMessageType'
    -> Integer -- ^ 'tmCount'
    -> TelemetryMetadata
telemetryMetadata pMessageType_ pCount_ =
  TelemetryMetadata'
    {_tmDataSize = Nothing, _tmMessageType = pMessageType_, _tmCount = pCount_}


-- | The data size of messages that the agent sends to the Amazon Inspector service.
tmDataSize :: Lens' TelemetryMetadata (Maybe Integer)
tmDataSize = lens _tmDataSize (\ s a -> s{_tmDataSize = a})

-- | A specific type of behavioral data that is collected by the agent.
tmMessageType :: Lens' TelemetryMetadata Text
tmMessageType = lens _tmMessageType (\ s a -> s{_tmMessageType = a})

-- | The count of messages that the agent sends to the Amazon Inspector service.
tmCount :: Lens' TelemetryMetadata Integer
tmCount = lens _tmCount (\ s a -> s{_tmCount = a})

instance FromJSON TelemetryMetadata where
        parseJSON
          = withObject "TelemetryMetadata"
              (\ x ->
                 TelemetryMetadata' <$>
                   (x .:? "dataSize") <*> (x .: "messageType") <*>
                     (x .: "count"))

instance Hashable TelemetryMetadata where

instance NFData TelemetryMetadata where

-- | This data type is used in the 'AssessmentRunFilter' data type.
--
--
--
-- /See:/ 'timestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { _trEndDate   :: !(Maybe POSIX)
  , _trBeginDate :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimestampRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trEndDate' - The maximum value of the timestamp range.
--
-- * 'trBeginDate' - The minimum value of the timestamp range.
timestampRange
    :: TimestampRange
timestampRange = TimestampRange' {_trEndDate = Nothing, _trBeginDate = Nothing}


-- | The maximum value of the timestamp range.
trEndDate :: Lens' TimestampRange (Maybe UTCTime)
trEndDate = lens _trEndDate (\ s a -> s{_trEndDate = a}) . mapping _Time

-- | The minimum value of the timestamp range.
trBeginDate :: Lens' TimestampRange (Maybe UTCTime)
trBeginDate = lens _trBeginDate (\ s a -> s{_trBeginDate = a}) . mapping _Time

instance Hashable TimestampRange where

instance NFData TimestampRange where

instance ToJSON TimestampRange where
        toJSON TimestampRange'{..}
          = object
              (catMaybes
                 [("endDate" .=) <$> _trEndDate,
                  ("beginDate" .=) <$> _trBeginDate])
