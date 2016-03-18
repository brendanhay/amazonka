{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.Product where

import           Network.AWS.Inspector.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Contains information about an Inspector agent. This data type is used as
-- a response element in the < ListAssessmentAgents> action.
--
-- /See:/ 'agent' smart constructor.
data Agent = Agent'
    { _aTelemetry          :: !(Maybe [Telemetry])
    , _aAutoScalingGroup   :: !(Maybe Text)
    , _aAgentHealthCode    :: !(Maybe Text)
    , _aAssessmentARN      :: !(Maybe Text)
    , _aAgentId            :: !(Maybe Text)
    , _aAccountId          :: !(Maybe Text)
    , _aAgentHealthDetails :: !(Maybe Text)
    , _aAgentHealth        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Agent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aTelemetry'
--
-- * 'aAutoScalingGroup'
--
-- * 'aAgentHealthCode'
--
-- * 'aAssessmentARN'
--
-- * 'aAgentId'
--
-- * 'aAccountId'
--
-- * 'aAgentHealthDetails'
--
-- * 'aAgentHealth'
agent
    :: Agent
agent =
    Agent'
    { _aTelemetry = Nothing
    , _aAutoScalingGroup = Nothing
    , _aAgentHealthCode = Nothing
    , _aAssessmentARN = Nothing
    , _aAgentId = Nothing
    , _aAccountId = Nothing
    , _aAgentHealthDetails = Nothing
    , _aAgentHealth = Nothing
    }

-- | The Inspector application data metrics collected by the agent.
aTelemetry :: Lens' Agent [Telemetry]
aTelemetry = lens _aTelemetry (\ s a -> s{_aTelemetry = a}) . _Default . _Coerce;

-- | This data type property is currently not used.
aAutoScalingGroup :: Lens' Agent (Maybe Text)
aAutoScalingGroup = lens _aAutoScalingGroup (\ s a -> s{_aAutoScalingGroup = a});

-- | The detailed health state of the agent. Values can be set to /RUNNING/,
-- /HEALTHY/, /UNHEALTHY/, /UNKNOWN/, /BLACKLISTED/, /SHUTDOWN/,
-- /THROTTLED/.
aAgentHealthCode :: Lens' Agent (Maybe Text)
aAgentHealthCode = lens _aAgentHealthCode (\ s a -> s{_aAgentHealthCode = a});

-- | The ARN of the assessment that is associated with the agent.
aAssessmentARN :: Lens' Agent (Maybe Text)
aAssessmentARN = lens _aAssessmentARN (\ s a -> s{_aAssessmentARN = a});

-- | The EC2 instance ID where the agent is installed.
aAgentId :: Lens' Agent (Maybe Text)
aAgentId = lens _aAgentId (\ s a -> s{_aAgentId = a});

-- | AWS account of the EC2 instance where the agent is installed.
aAccountId :: Lens' Agent (Maybe Text)
aAccountId = lens _aAccountId (\ s a -> s{_aAccountId = a});

-- | The description for the agent health code.
aAgentHealthDetails :: Lens' Agent (Maybe Text)
aAgentHealthDetails = lens _aAgentHealthDetails (\ s a -> s{_aAgentHealthDetails = a});

-- | The current health state of the agent. Values can be set to /HEALTHY/ or
-- /UNHEALTHY/.
aAgentHealth :: Lens' Agent (Maybe Text)
aAgentHealth = lens _aAgentHealth (\ s a -> s{_aAgentHealth = a});

instance FromJSON Agent where
        parseJSON
          = withObject "Agent"
              (\ x ->
                 Agent' <$>
                   (x .:? "telemetry" .!= mempty) <*>
                     (x .:? "autoScalingGroup")
                     <*> (x .:? "agentHealthCode")
                     <*> (x .:? "assessmentArn")
                     <*> (x .:? "agentId")
                     <*> (x .:? "accountId")
                     <*> (x .:? "agentHealthDetails")
                     <*> (x .:? "agentHealth"))

-- | This data type is used as a response element in the
-- < PreviewAgentsForResourceGroup> action.
--
-- /See:/ 'agentPreview' smart constructor.
data AgentPreview = AgentPreview'
    { _apAutoScalingGroup :: !(Maybe Text)
    , _apAgentId          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AgentPreview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apAutoScalingGroup'
--
-- * 'apAgentId'
agentPreview
    :: AgentPreview
agentPreview =
    AgentPreview'
    { _apAutoScalingGroup = Nothing
    , _apAgentId = Nothing
    }

-- | The autoscaling group for the EC2 instance where the agent is installed.
apAutoScalingGroup :: Lens' AgentPreview (Maybe Text)
apAutoScalingGroup = lens _apAutoScalingGroup (\ s a -> s{_apAutoScalingGroup = a});

-- | The id of the EC2 instance where the agent is intalled.
apAgentId :: Lens' AgentPreview (Maybe Text)
apAgentId = lens _apAgentId (\ s a -> s{_apAgentId = a});

instance FromJSON AgentPreview where
        parseJSON
          = withObject "AgentPreview"
              (\ x ->
                 AgentPreview' <$>
                   (x .:? "autoScalingGroup") <*> (x .:? "agentId"))

-- | This data type is used as a response element in the
-- < ListAssessmentAgents> action.
--
-- /See:/ 'agentsFilter' smart constructor.
newtype AgentsFilter = AgentsFilter'
    { _afAgentHealthList :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AgentsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afAgentHealthList'
agentsFilter
    :: AgentsFilter
agentsFilter =
    AgentsFilter'
    { _afAgentHealthList = Nothing
    }

-- | For a record to match a filter, the value specified for this data type
-- property must be the exact match of the value of the __agentHealth__
-- property of the < Agent> data type.
afAgentHealthList :: Lens' AgentsFilter [Text]
afAgentHealthList = lens _afAgentHealthList (\ s a -> s{_afAgentHealthList = a}) . _Default . _Coerce;

instance ToJSON AgentsFilter where
        toJSON AgentsFilter'{..}
          = object
              (catMaybes
                 [("agentHealthList" .=) <$> _afAgentHealthList])

-- | Contains information about an Inspector application.
--
-- This data type is used as the response element in the
-- < DescribeApplication> action.
--
-- /See:/ 'application' smart constructor.
data Application = Application'
    { _aApplicationARN   :: !(Maybe Text)
    , _aResourceGroupARN :: !(Maybe Text)
    , _aApplicationName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Application' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aApplicationARN'
--
-- * 'aResourceGroupARN'
--
-- * 'aApplicationName'
application
    :: Application
application =
    Application'
    { _aApplicationARN = Nothing
    , _aResourceGroupARN = Nothing
    , _aApplicationName = Nothing
    }

-- | The ARN specifying the Inspector application.
aApplicationARN :: Lens' Application (Maybe Text)
aApplicationARN = lens _aApplicationARN (\ s a -> s{_aApplicationARN = a});

-- | The ARN specifying the resource group that is associated with the
-- application.
aResourceGroupARN :: Lens' Application (Maybe Text)
aResourceGroupARN = lens _aResourceGroupARN (\ s a -> s{_aResourceGroupARN = a});

-- | The name of the Inspector application.
aApplicationName :: Lens' Application (Maybe Text)
aApplicationName = lens _aApplicationName (\ s a -> s{_aApplicationName = a});

instance FromJSON Application where
        parseJSON
          = withObject "Application"
              (\ x ->
                 Application' <$>
                   (x .:? "applicationArn") <*>
                     (x .:? "resourceGroupArn")
                     <*> (x .:? "applicationName"))

-- | This data type is used as the request parameter in the
-- < ListApplications> action.
--
-- /See:/ 'applicationsFilter' smart constructor.
newtype ApplicationsFilter = ApplicationsFilter'
    { _afApplicationNamePatterns :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ApplicationsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afApplicationNamePatterns'
applicationsFilter
    :: ApplicationsFilter
applicationsFilter =
    ApplicationsFilter'
    { _afApplicationNamePatterns = Nothing
    }

-- | For a record to match a filter, an explicit value or a string containing
-- a wildcard specified for this data type property must match the value of
-- the __applicationName__ property of the < Application> data type.
afApplicationNamePatterns :: Lens' ApplicationsFilter [Text]
afApplicationNamePatterns = lens _afApplicationNamePatterns (\ s a -> s{_afApplicationNamePatterns = a}) . _Default . _Coerce;

instance ToJSON ApplicationsFilter where
        toJSON ApplicationsFilter'{..}
          = object
              (catMaybes
                 [("applicationNamePatterns" .=) <$>
                    _afApplicationNamePatterns])

-- | Contains information about an Inspector assessment.
--
-- This data type is used as the response element in the
-- < DescribeAssessment> action.
--
-- /See:/ 'assessment' smart constructor.
data Assessment = Assessment'
    { _assDataCollected             :: !(Maybe Bool)
    , _assApplicationARN            :: !(Maybe Text)
    , _assStartTime                 :: !(Maybe POSIX)
    , _assAssessmentARN             :: !(Maybe Text)
    , _assUserAttributesForFindings :: !(Maybe [Attribute])
    , _assFailureMessage            :: !(Maybe Text)
    , _assAssessmentState           :: !(Maybe Text)
    , _assEndTime                   :: !(Maybe POSIX)
    , _assDurationInSeconds         :: !(Maybe Int)
    , _assAssessmentName            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Assessment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assDataCollected'
--
-- * 'assApplicationARN'
--
-- * 'assStartTime'
--
-- * 'assAssessmentARN'
--
-- * 'assUserAttributesForFindings'
--
-- * 'assFailureMessage'
--
-- * 'assAssessmentState'
--
-- * 'assEndTime'
--
-- * 'assDurationInSeconds'
--
-- * 'assAssessmentName'
assessment
    :: Assessment
assessment =
    Assessment'
    { _assDataCollected = Nothing
    , _assApplicationARN = Nothing
    , _assStartTime = Nothing
    , _assAssessmentARN = Nothing
    , _assUserAttributesForFindings = Nothing
    , _assFailureMessage = Nothing
    , _assAssessmentState = Nothing
    , _assEndTime = Nothing
    , _assDurationInSeconds = Nothing
    , _assAssessmentName = Nothing
    }

-- | Boolean value (true or false) specifying whether the data collection
-- process is completed.
assDataCollected :: Lens' Assessment (Maybe Bool)
assDataCollected = lens _assDataCollected (\ s a -> s{_assDataCollected = a});

-- | The ARN of the application that corresponds to this assessment.
assApplicationARN :: Lens' Assessment (Maybe Text)
assApplicationARN = lens _assApplicationARN (\ s a -> s{_assApplicationARN = a});

-- | The assessment start time.
assStartTime :: Lens' Assessment (Maybe UTCTime)
assStartTime = lens _assStartTime (\ s a -> s{_assStartTime = a}) . mapping _Time;

-- | The ARN of the assessment.
assAssessmentARN :: Lens' Assessment (Maybe Text)
assAssessmentARN = lens _assAssessmentARN (\ s a -> s{_assAssessmentARN = a});

-- | The user-defined attributes that are assigned to every generated
-- finding.
assUserAttributesForFindings :: Lens' Assessment [Attribute]
assUserAttributesForFindings = lens _assUserAttributesForFindings (\ s a -> s{_assUserAttributesForFindings = a}) . _Default . _Coerce;

-- | This data type property is not currently used.
assFailureMessage :: Lens' Assessment (Maybe Text)
assFailureMessage = lens _assFailureMessage (\ s a -> s{_assFailureMessage = a});

-- | The state of the assessment. Values can be set to /Created/, /Collecting
-- Data/, /Stopping/, and /Completed/.
assAssessmentState :: Lens' Assessment (Maybe Text)
assAssessmentState = lens _assAssessmentState (\ s a -> s{_assAssessmentState = a});

-- | The assessment end time.
assEndTime :: Lens' Assessment (Maybe UTCTime)
assEndTime = lens _assEndTime (\ s a -> s{_assEndTime = a}) . mapping _Time;

-- | The assessment duration in seconds. The default value is 3600 seconds
-- (one hour). The maximum value is 86400 seconds (one day).
assDurationInSeconds :: Lens' Assessment (Maybe Int)
assDurationInSeconds = lens _assDurationInSeconds (\ s a -> s{_assDurationInSeconds = a});

-- | The name of the assessment.
assAssessmentName :: Lens' Assessment (Maybe Text)
assAssessmentName = lens _assAssessmentName (\ s a -> s{_assAssessmentName = a});

instance FromJSON Assessment where
        parseJSON
          = withObject "Assessment"
              (\ x ->
                 Assessment' <$>
                   (x .:? "dataCollected") <*> (x .:? "applicationArn")
                     <*> (x .:? "startTime")
                     <*> (x .:? "assessmentArn")
                     <*> (x .:? "userAttributesForFindings" .!= mempty)
                     <*> (x .:? "failureMessage")
                     <*> (x .:? "assessmentState")
                     <*> (x .:? "endTime")
                     <*> (x .:? "durationInSeconds")
                     <*> (x .:? "assessmentName"))

-- | This data type is used as the request parameter in the
-- < ListAssessments> and < ListAttachedAssessments> actions.
--
-- /See:/ 'assessmentsFilter' smart constructor.
data AssessmentsFilter = AssessmentsFilter'
    { _afDataCollected          :: !(Maybe Bool)
    , _afAssessmentStates       :: !(Maybe [Text])
    , _afStartTimeRange         :: !(Maybe TimestampRange)
    , _afAssessmentNamePatterns :: !(Maybe [Text])
    , _afEndTimeRange           :: !(Maybe TimestampRange)
    , _afDurationRange          :: !(Maybe DurationRange)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssessmentsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afDataCollected'
--
-- * 'afAssessmentStates'
--
-- * 'afStartTimeRange'
--
-- * 'afAssessmentNamePatterns'
--
-- * 'afEndTimeRange'
--
-- * 'afDurationRange'
assessmentsFilter
    :: AssessmentsFilter
assessmentsFilter =
    AssessmentsFilter'
    { _afDataCollected = Nothing
    , _afAssessmentStates = Nothing
    , _afStartTimeRange = Nothing
    , _afAssessmentNamePatterns = Nothing
    , _afEndTimeRange = Nothing
    , _afDurationRange = Nothing
    }

-- | For a record to match a filter, the value specified for this data type
-- property must be the exact match of the value of the __dataCollected__
-- property of the < Assessment> data type.
afDataCollected :: Lens' AssessmentsFilter (Maybe Bool)
afDataCollected = lens _afDataCollected (\ s a -> s{_afDataCollected = a});

-- | For a record to match a filter, the value specified for this data type
-- property must be the exact match of the value of the __assessmentState__
-- property of the < Assessment> data type.
afAssessmentStates :: Lens' AssessmentsFilter [Text]
afAssessmentStates = lens _afAssessmentStates (\ s a -> s{_afAssessmentStates = a}) . _Default . _Coerce;

-- | For a record to match a filter, the value specified for this data type
-- property must inclusively match any value between the specified minimum
-- and maximum values of the __startTime__ property of the < Assessment>
-- data type.
afStartTimeRange :: Lens' AssessmentsFilter (Maybe TimestampRange)
afStartTimeRange = lens _afStartTimeRange (\ s a -> s{_afStartTimeRange = a});

-- | For a record to match a filter, an explicit value or a string containing
-- a wildcard specified for this data type property must match the value of
-- the __assessmentName__ property of the < Assessment> data type.
afAssessmentNamePatterns :: Lens' AssessmentsFilter [Text]
afAssessmentNamePatterns = lens _afAssessmentNamePatterns (\ s a -> s{_afAssessmentNamePatterns = a}) . _Default . _Coerce;

-- | For a record to match a filter, the value specified for this data type
-- property must inclusively match any value between the specified minimum
-- and maximum values of the __endTime__ property of the < Assessment> data
-- type.
afEndTimeRange :: Lens' AssessmentsFilter (Maybe TimestampRange)
afEndTimeRange = lens _afEndTimeRange (\ s a -> s{_afEndTimeRange = a});

-- | For a record to match a filter, the value specified for this data type
-- property must inclusively match any value between the specified minimum
-- and maximum values of the __durationInSeconds__ property of the
-- < Assessment> data type.
afDurationRange :: Lens' AssessmentsFilter (Maybe DurationRange)
afDurationRange = lens _afDurationRange (\ s a -> s{_afDurationRange = a});

instance ToJSON AssessmentsFilter where
        toJSON AssessmentsFilter'{..}
          = object
              (catMaybes
                 [("dataCollected" .=) <$> _afDataCollected,
                  ("assessmentStates" .=) <$> _afAssessmentStates,
                  ("startTimeRange" .=) <$> _afStartTimeRange,
                  ("assessmentNamePatterns" .=) <$>
                    _afAssessmentNamePatterns,
                  ("endTimeRange" .=) <$> _afEndTimeRange,
                  ("durationRange" .=) <$> _afDurationRange])

-- | This data type is used as a response element in the
-- < AddAttributesToFindings> action and a request parameter in the
-- < CreateAssessment> action.
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
    { _aValue :: !(Maybe Text)
    , _aKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aValue'
--
-- * 'aKey'
attribute
    :: Attribute
attribute =
    Attribute'
    { _aValue = Nothing
    , _aKey = Nothing
    }

-- | The value assigned to the attribute key.
aValue :: Lens' Attribute (Maybe Text)
aValue = lens _aValue (\ s a -> s{_aValue = a});

-- | The attribute key.
aKey :: Lens' Attribute (Maybe Text)
aKey = lens _aKey (\ s a -> s{_aKey = a});

instance FromJSON Attribute where
        parseJSON
          = withObject "Attribute"
              (\ x ->
                 Attribute' <$> (x .:? "value") <*> (x .:? "key"))

instance ToJSON Attribute where
        toJSON Attribute'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _aValue, ("key" .=) <$> _aKey])

-- | This data type is used in the < AssessmentsFilter> data type.
--
-- /See:/ 'durationRange' smart constructor.
data DurationRange = DurationRange'
    { _drMaximum :: !(Maybe Int)
    , _drMinimum :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DurationRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drMaximum'
--
-- * 'drMinimum'
durationRange
    :: DurationRange
durationRange =
    DurationRange'
    { _drMaximum = Nothing
    , _drMinimum = Nothing
    }

-- | The maximum value of the duration range. Must be less than or equal to
-- 604800 seconds (1 week).
drMaximum :: Lens' DurationRange (Maybe Int)
drMaximum = lens _drMaximum (\ s a -> s{_drMaximum = a});

-- | The minimum value of the duration range. Must be greater than zero.
drMinimum :: Lens' DurationRange (Maybe Int)
drMinimum = lens _drMinimum (\ s a -> s{_drMinimum = a});

instance ToJSON DurationRange where
        toJSON DurationRange'{..}
          = object
              (catMaybes
                 [("maximum" .=) <$> _drMaximum,
                  ("minimum" .=) <$> _drMinimum])

-- | Contains information about an Inspector finding.
--
-- This data type is used as the response element in the < DescribeFinding>
-- action.
--
-- /See:/ 'finding' smart constructor.
data Finding = Finding'
    { _fAutoScalingGroup :: !(Maybe Text)
    , _fFinding          :: !(Maybe LocalizedText)
    , _fSeverity         :: !(Maybe Text)
    , _fUserAttributes   :: !(Maybe [Attribute])
    , _fRuleName         :: !(Maybe Text)
    , _fAgentId          :: !(Maybe Text)
    , _fRunARN           :: !(Maybe Text)
    , _fAttributes       :: !(Maybe [Attribute])
    , _fRulesPackageARN  :: !(Maybe Text)
    , _fFindingARN       :: !(Maybe Text)
    , _fDescription      :: !(Maybe LocalizedText)
    , _fRecommendation   :: !(Maybe LocalizedText)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Finding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fAutoScalingGroup'
--
-- * 'fFinding'
--
-- * 'fSeverity'
--
-- * 'fUserAttributes'
--
-- * 'fRuleName'
--
-- * 'fAgentId'
--
-- * 'fRunARN'
--
-- * 'fAttributes'
--
-- * 'fRulesPackageARN'
--
-- * 'fFindingARN'
--
-- * 'fDescription'
--
-- * 'fRecommendation'
finding
    :: Finding
finding =
    Finding'
    { _fAutoScalingGroup = Nothing
    , _fFinding = Nothing
    , _fSeverity = Nothing
    , _fUserAttributes = Nothing
    , _fRuleName = Nothing
    , _fAgentId = Nothing
    , _fRunARN = Nothing
    , _fAttributes = Nothing
    , _fRulesPackageARN = Nothing
    , _fFindingARN = Nothing
    , _fDescription = Nothing
    , _fRecommendation = Nothing
    }

-- | The autoscaling group of the EC2 instance where the agent is installed
-- that is used during the assessment that generates the finding.
fAutoScalingGroup :: Lens' Finding (Maybe Text)
fAutoScalingGroup = lens _fAutoScalingGroup (\ s a -> s{_fAutoScalingGroup = a});

-- | A short description that identifies the finding.
fFinding :: Lens' Finding (Maybe LocalizedText)
fFinding = lens _fFinding (\ s a -> s{_fFinding = a});

-- | The finding severity. Values can be set to /High/, /Medium/, /Low/, and
-- /Informational/.
fSeverity :: Lens' Finding (Maybe Text)
fSeverity = lens _fSeverity (\ s a -> s{_fSeverity = a});

-- | The user-defined attributes that are assigned to the finding.
fUserAttributes :: Lens' Finding [Attribute]
fUserAttributes = lens _fUserAttributes (\ s a -> s{_fUserAttributes = a}) . _Default . _Coerce;

-- | The rule name that is used to generate the finding.
fRuleName :: Lens' Finding (Maybe Text)
fRuleName = lens _fRuleName (\ s a -> s{_fRuleName = a});

-- | The EC2 instance ID where the agent is installed that is used during the
-- assessment that generates the finding.
fAgentId :: Lens' Finding (Maybe Text)
fAgentId = lens _fAgentId (\ s a -> s{_fAgentId = a});

-- | The ARN of the assessment run that generated the finding.
fRunARN :: Lens' Finding (Maybe Text)
fRunARN = lens _fRunARN (\ s a -> s{_fRunARN = a});

-- | The system-defined attributes for the finding.
fAttributes :: Lens' Finding [Attribute]
fAttributes = lens _fAttributes (\ s a -> s{_fAttributes = a}) . _Default . _Coerce;

-- | The ARN of the rules package that is used to generate the finding.
fRulesPackageARN :: Lens' Finding (Maybe Text)
fRulesPackageARN = lens _fRulesPackageARN (\ s a -> s{_fRulesPackageARN = a});

-- | The ARN specifying the finding.
fFindingARN :: Lens' Finding (Maybe Text)
fFindingARN = lens _fFindingARN (\ s a -> s{_fFindingARN = a});

-- | The description of the finding.
fDescription :: Lens' Finding (Maybe LocalizedText)
fDescription = lens _fDescription (\ s a -> s{_fDescription = a});

-- | The recommendation for the finding.
fRecommendation :: Lens' Finding (Maybe LocalizedText)
fRecommendation = lens _fRecommendation (\ s a -> s{_fRecommendation = a});

instance FromJSON Finding where
        parseJSON
          = withObject "Finding"
              (\ x ->
                 Finding' <$>
                   (x .:? "autoScalingGroup") <*> (x .:? "finding") <*>
                     (x .:? "severity")
                     <*> (x .:? "userAttributes" .!= mempty)
                     <*> (x .:? "ruleName")
                     <*> (x .:? "agentId")
                     <*> (x .:? "runArn")
                     <*> (x .:? "attributes" .!= mempty)
                     <*> (x .:? "rulesPackageArn")
                     <*> (x .:? "findingArn")
                     <*> (x .:? "description")
                     <*> (x .:? "recommendation"))

-- | This data type is used as a request parameter in the < ListFindings>
-- action.
--
-- /See:/ 'findingsFilter' smart constructor.
data FindingsFilter = FindingsFilter'
    { _ffRuleNames        :: !(Maybe [Text])
    , _ffUserAttributes   :: !(Maybe [Attribute])
    , _ffRulesPackageARNs :: !(Maybe [Text])
    , _ffAttributes       :: !(Maybe [Attribute])
    , _ffSeverities       :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FindingsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ffRuleNames'
--
-- * 'ffUserAttributes'
--
-- * 'ffRulesPackageARNs'
--
-- * 'ffAttributes'
--
-- * 'ffSeverities'
findingsFilter
    :: FindingsFilter
findingsFilter =
    FindingsFilter'
    { _ffRuleNames = Nothing
    , _ffUserAttributes = Nothing
    , _ffRulesPackageARNs = Nothing
    , _ffAttributes = Nothing
    , _ffSeverities = Nothing
    }

-- | For a record to match a filter, the value specified for this data type
-- property must be the exact match of the value of the __ruleName__
-- property of the < Finding> data type.
ffRuleNames :: Lens' FindingsFilter [Text]
ffRuleNames = lens _ffRuleNames (\ s a -> s{_ffRuleNames = a}) . _Default . _Coerce;

-- | For a record to match a filter, the value specified for this data type
-- property must be the exact match of the value of the __userAttributes__
-- property of the < Finding> data type.
ffUserAttributes :: Lens' FindingsFilter [Attribute]
ffUserAttributes = lens _ffUserAttributes (\ s a -> s{_ffUserAttributes = a}) . _Default . _Coerce;

-- | For a record to match a filter, the value specified for this data type
-- property must be the exact match of the value of the __rulesPackageArn__
-- property of the < Finding> data type.
ffRulesPackageARNs :: Lens' FindingsFilter [Text]
ffRulesPackageARNs = lens _ffRulesPackageARNs (\ s a -> s{_ffRulesPackageARNs = a}) . _Default . _Coerce;

-- | For a record to match a filter, the value specified for this data type
-- property must be the exact match of the value of the __attributes__
-- property of the < Finding> data type.
ffAttributes :: Lens' FindingsFilter [Attribute]
ffAttributes = lens _ffAttributes (\ s a -> s{_ffAttributes = a}) . _Default . _Coerce;

-- | For a record to match a filter, the value specified for this data type
-- property must be the exact match of the value of the __severity__
-- property of the < Finding> data type.
ffSeverities :: Lens' FindingsFilter [Text]
ffSeverities = lens _ffSeverities (\ s a -> s{_ffSeverities = a}) . _Default . _Coerce;

instance ToJSON FindingsFilter where
        toJSON FindingsFilter'{..}
          = object
              (catMaybes
                 [("ruleNames" .=) <$> _ffRuleNames,
                  ("userAttributes" .=) <$> _ffUserAttributes,
                  ("rulesPackageArns" .=) <$> _ffRulesPackageARNs,
                  ("attributes" .=) <$> _ffAttributes,
                  ("severities" .=) <$> _ffSeverities])

-- | The textual identifier. This data type is used as the request parameter
-- in the < LocalizeText> action.
--
-- /See:/ 'localizedText' smart constructor.
data LocalizedText = LocalizedText'
    { _ltKey        :: !(Maybe LocalizedTextKey)
    , _ltParameters :: !(Maybe [Parameter])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LocalizedText' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltKey'
--
-- * 'ltParameters'
localizedText
    :: LocalizedText
localizedText =
    LocalizedText'
    { _ltKey = Nothing
    , _ltParameters = Nothing
    }

-- | The facility and id properties of the < LocalizedTextKey> data type.
ltKey :: Lens' LocalizedText (Maybe LocalizedTextKey)
ltKey = lens _ltKey (\ s a -> s{_ltKey = a});

-- | Values for the dynamic elements of the string specified by the textual
-- identifier.
ltParameters :: Lens' LocalizedText [Parameter]
ltParameters = lens _ltParameters (\ s a -> s{_ltParameters = a}) . _Default . _Coerce;

instance FromJSON LocalizedText where
        parseJSON
          = withObject "LocalizedText"
              (\ x ->
                 LocalizedText' <$>
                   (x .:? "key") <*> (x .:? "parameters" .!= mempty))

instance ToJSON LocalizedText where
        toJSON LocalizedText'{..}
          = object
              (catMaybes
                 [("key" .=) <$> _ltKey,
                  ("parameters" .=) <$> _ltParameters])

-- | This data type is used in the < LocalizedText> data type.
--
-- /See:/ 'localizedTextKey' smart constructor.
data LocalizedTextKey = LocalizedTextKey'
    { _ltkFacility :: !(Maybe Text)
    , _ltkId       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LocalizedTextKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltkFacility'
--
-- * 'ltkId'
localizedTextKey
    :: LocalizedTextKey
localizedTextKey =
    LocalizedTextKey'
    { _ltkFacility = Nothing
    , _ltkId = Nothing
    }

-- | The module response source of the text.
ltkFacility :: Lens' LocalizedTextKey (Maybe Text)
ltkFacility = lens _ltkFacility (\ s a -> s{_ltkFacility = a});

-- | Part of the module response source of the text.
ltkId :: Lens' LocalizedTextKey (Maybe Text)
ltkId = lens _ltkId (\ s a -> s{_ltkId = a});

instance FromJSON LocalizedTextKey where
        parseJSON
          = withObject "LocalizedTextKey"
              (\ x ->
                 LocalizedTextKey' <$>
                   (x .:? "facility") <*> (x .:? "id"))

instance ToJSON LocalizedTextKey where
        toJSON LocalizedTextKey'{..}
          = object
              (catMaybes
                 [("facility" .=) <$> _ltkFacility,
                  ("id" .=) <$> _ltkId])

-- | This data type is used in the < Telemetry> data type.
--
-- This is metadata about the behavioral data collected by the Inspector
-- agent on your EC2 instances during an assessment and passed to the
-- Inspector service for analysis.
--
-- /See:/ 'messageTypeTelemetry' smart constructor.
data MessageTypeTelemetry = MessageTypeTelemetry'
    { _mttDataSize    :: !(Maybe Integer)
    , _mttMessageType :: !(Maybe Text)
    , _mttCount       :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MessageTypeTelemetry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mttDataSize'
--
-- * 'mttMessageType'
--
-- * 'mttCount'
messageTypeTelemetry
    :: MessageTypeTelemetry
messageTypeTelemetry =
    MessageTypeTelemetry'
    { _mttDataSize = Nothing
    , _mttMessageType = Nothing
    , _mttCount = Nothing
    }

-- | The total size of the behavioral data that is collected by the agent
-- during an assessment.
mttDataSize :: Lens' MessageTypeTelemetry (Maybe Integer)
mttDataSize = lens _mttDataSize (\ s a -> s{_mttDataSize = a});

-- | A specific type of behavioral data that is collected by the agent.
mttMessageType :: Lens' MessageTypeTelemetry (Maybe Text)
mttMessageType = lens _mttMessageType (\ s a -> s{_mttMessageType = a});

-- | The number of times that the behavioral data is collected by the agent
-- during an assessment.
mttCount :: Lens' MessageTypeTelemetry (Maybe Integer)
mttCount = lens _mttCount (\ s a -> s{_mttCount = a});

instance FromJSON MessageTypeTelemetry where
        parseJSON
          = withObject "MessageTypeTelemetry"
              (\ x ->
                 MessageTypeTelemetry' <$>
                   (x .:? "dataSize") <*> (x .:? "messageType") <*>
                     (x .:? "count"))

-- | This data type is used in the < LocalizedText> data type.
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
    { _pValue :: !(Maybe Text)
    , _pName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pValue'
--
-- * 'pName'
parameter
    :: Parameter
parameter =
    Parameter'
    { _pValue = Nothing
    , _pName = Nothing
    }

-- | The value assigned to the variable that is being replaced.
pValue :: Lens' Parameter (Maybe Text)
pValue = lens _pValue (\ s a -> s{_pValue = a});

-- | The name of the variable that is being replaced.
pName :: Lens' Parameter (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a});

instance FromJSON Parameter where
        parseJSON
          = withObject "Parameter"
              (\ x ->
                 Parameter' <$> (x .:? "value") <*> (x .:? "name"))

instance ToJSON Parameter where
        toJSON Parameter'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _pValue, ("name" .=) <$> _pName])

-- | Contains information about a resource group. The resource group defines
-- a set of tags that, when queried, identify the AWS resources that
-- comprise the application.
--
-- This data type is used as the response element in the
-- < DescribeResourceGroup> action.
--
-- /See:/ 'resourceGroup' smart constructor.
data ResourceGroup = ResourceGroup'
    { _rgResourceGroupTags :: !(Maybe Text)
    , _rgResourceGroupARN  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgResourceGroupTags'
--
-- * 'rgResourceGroupARN'
resourceGroup
    :: ResourceGroup
resourceGroup =
    ResourceGroup'
    { _rgResourceGroupTags = Nothing
    , _rgResourceGroupARN = Nothing
    }

-- | The tags (key and value pairs) of the resource group.
--
-- This data type property is used in the < CreateResourceGroup> action.
--
-- A collection of keys and an array of possible values in JSON format.
--
-- For example, [{ \"key1\" : [\"Value1\",\"Value2\"]},{\"Key2\":
-- [\"Value3\"]}]
rgResourceGroupTags :: Lens' ResourceGroup (Maybe Text)
rgResourceGroupTags = lens _rgResourceGroupTags (\ s a -> s{_rgResourceGroupTags = a});

-- | The ARN of the resource group.
rgResourceGroupARN :: Lens' ResourceGroup (Maybe Text)
rgResourceGroupARN = lens _rgResourceGroupARN (\ s a -> s{_rgResourceGroupARN = a});

instance FromJSON ResourceGroup where
        parseJSON
          = withObject "ResourceGroup"
              (\ x ->
                 ResourceGroup' <$>
                   (x .:? "resourceGroupTags") <*>
                     (x .:? "resourceGroupArn"))

-- | Contains information about an Inspector rules package.
--
-- This data type is used as the response element in the
-- < DescribeRulesPackage> action.
--
-- /See:/ 'rulesPackage' smart constructor.
data RulesPackage = RulesPackage'
    { _rpVersion          :: !(Maybe Text)
    , _rpRulesPackageARN  :: !(Maybe Text)
    , _rpRulesPackageName :: !(Maybe Text)
    , _rpDescription      :: !(Maybe LocalizedText)
    , _rpProvider         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RulesPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpVersion'
--
-- * 'rpRulesPackageARN'
--
-- * 'rpRulesPackageName'
--
-- * 'rpDescription'
--
-- * 'rpProvider'
rulesPackage
    :: RulesPackage
rulesPackage =
    RulesPackage'
    { _rpVersion = Nothing
    , _rpRulesPackageARN = Nothing
    , _rpRulesPackageName = Nothing
    , _rpDescription = Nothing
    , _rpProvider = Nothing
    }

-- | The version id of the rules package.
rpVersion :: Lens' RulesPackage (Maybe Text)
rpVersion = lens _rpVersion (\ s a -> s{_rpVersion = a});

-- | The ARN of the rules package.
rpRulesPackageARN :: Lens' RulesPackage (Maybe Text)
rpRulesPackageARN = lens _rpRulesPackageARN (\ s a -> s{_rpRulesPackageARN = a});

-- | The name of the rules package.
rpRulesPackageName :: Lens' RulesPackage (Maybe Text)
rpRulesPackageName = lens _rpRulesPackageName (\ s a -> s{_rpRulesPackageName = a});

-- | The description of the rules package.
rpDescription :: Lens' RulesPackage (Maybe LocalizedText)
rpDescription = lens _rpDescription (\ s a -> s{_rpDescription = a});

-- | The provider of the rules package.
rpProvider :: Lens' RulesPackage (Maybe Text)
rpProvider = lens _rpProvider (\ s a -> s{_rpProvider = a});

instance FromJSON RulesPackage where
        parseJSON
          = withObject "RulesPackage"
              (\ x ->
                 RulesPackage' <$>
                   (x .:? "version") <*> (x .:? "rulesPackageArn") <*>
                     (x .:? "rulesPackageName")
                     <*> (x .:? "description")
                     <*> (x .:? "provider"))

-- | A snapshot of an Inspector assessment that contains the assessment\'s
-- findings.
--
-- This data type is used as the response element in the < DescribeRun>
-- action.
--
-- /See:/ 'run' smart constructor.
data Run = Run'
    { _runCreationTime   :: !(Maybe POSIX)
    , _runRulesPackages  :: !(Maybe [Text])
    , _runAssessmentARN  :: !(Maybe Text)
    , _runRunState       :: !(Maybe Text)
    , _runRunName        :: !(Maybe Text)
    , _runCompletionTime :: !(Maybe POSIX)
    , _runRunARN         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Run' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'runCreationTime'
--
-- * 'runRulesPackages'
--
-- * 'runAssessmentARN'
--
-- * 'runRunState'
--
-- * 'runRunName'
--
-- * 'runCompletionTime'
--
-- * 'runRunARN'
run
    :: Run
run =
    Run'
    { _runCreationTime = Nothing
    , _runRulesPackages = Nothing
    , _runAssessmentARN = Nothing
    , _runRunState = Nothing
    , _runRunName = Nothing
    , _runCompletionTime = Nothing
    , _runRunARN = Nothing
    }

-- | Run creation time that corresponds to the data collection completion
-- time or failure.
runCreationTime :: Lens' Run (Maybe UTCTime)
runCreationTime = lens _runCreationTime (\ s a -> s{_runCreationTime = a}) . mapping _Time;

-- | Rules packages selected for the run of the assessment.
runRulesPackages :: Lens' Run [Text]
runRulesPackages = lens _runRulesPackages (\ s a -> s{_runRulesPackages = a}) . _Default . _Coerce;

-- | The ARN of the assessment that is associated with the run.
runAssessmentARN :: Lens' Run (Maybe Text)
runAssessmentARN = lens _runAssessmentARN (\ s a -> s{_runAssessmentARN = a});

-- | The state of the run. Values can be set to /DataCollectionComplete/,
-- /EvaluatingPolicies/, /EvaluatingPoliciesErrorCanRetry/, /Completed/,
-- /Failed/, /TombStoned/.
runRunState :: Lens' Run (Maybe Text)
runRunState = lens _runRunState (\ s a -> s{_runRunState = a});

-- | The auto-generated name for the run.
runRunName :: Lens' Run (Maybe Text)
runRunName = lens _runRunName (\ s a -> s{_runRunName = a});

-- | Run completion time that corresponds to the rules packages evaluation
-- completion time or failure.
runCompletionTime :: Lens' Run (Maybe UTCTime)
runCompletionTime = lens _runCompletionTime (\ s a -> s{_runCompletionTime = a}) . mapping _Time;

-- | The ARN of the run.
runRunARN :: Lens' Run (Maybe Text)
runRunARN = lens _runRunARN (\ s a -> s{_runRunARN = a});

instance FromJSON Run where
        parseJSON
          = withObject "Run"
              (\ x ->
                 Run' <$>
                   (x .:? "creationTime") <*>
                     (x .:? "rulesPackages" .!= mempty)
                     <*> (x .:? "assessmentArn")
                     <*> (x .:? "runState")
                     <*> (x .:? "runName")
                     <*> (x .:? "completionTime")
                     <*> (x .:? "runArn"))

-- | This data type is used as the request parameter in the < ListRuns>
-- action.
--
-- /See:/ 'runsFilter' smart constructor.
data RunsFilter = RunsFilter'
    { _rfCreationTime    :: !(Maybe TimestampRange)
    , _rfRulesPackages   :: !(Maybe [Text])
    , _rfRunStates       :: !(Maybe [Text])
    , _rfRunNamePatterns :: !(Maybe [Text])
    , _rfCompletionTime  :: !(Maybe TimestampRange)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RunsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfCreationTime'
--
-- * 'rfRulesPackages'
--
-- * 'rfRunStates'
--
-- * 'rfRunNamePatterns'
--
-- * 'rfCompletionTime'
runsFilter
    :: RunsFilter
runsFilter =
    RunsFilter'
    { _rfCreationTime = Nothing
    , _rfRulesPackages = Nothing
    , _rfRunStates = Nothing
    , _rfRunNamePatterns = Nothing
    , _rfCompletionTime = Nothing
    }

-- | For a record to match a filter, the value specified for this data type
-- property must inclusively match any value between the specified minimum
-- and maximum values of the __creationTime__ property of the < Run> data
-- type.
rfCreationTime :: Lens' RunsFilter (Maybe TimestampRange)
rfCreationTime = lens _rfCreationTime (\ s a -> s{_rfCreationTime = a});

-- | For a record to match a filter, the value specified for this data type
-- property must match a list of values of the __rulesPackages__ property
-- of the < Run> data type.
rfRulesPackages :: Lens' RunsFilter [Text]
rfRulesPackages = lens _rfRulesPackages (\ s a -> s{_rfRulesPackages = a}) . _Default . _Coerce;

-- | For a record to match a filter, the value specified for this data type
-- property must be the exact match of the value of the __runState__
-- property of the < Run> data type.
rfRunStates :: Lens' RunsFilter [Text]
rfRunStates = lens _rfRunStates (\ s a -> s{_rfRunStates = a}) . _Default . _Coerce;

-- | For a record to match a filter, an explicit value or a string containing
-- a wildcard specified for this data type property must match the value of
-- the __runName__ property of the < Run> data type.
rfRunNamePatterns :: Lens' RunsFilter [Text]
rfRunNamePatterns = lens _rfRunNamePatterns (\ s a -> s{_rfRunNamePatterns = a}) . _Default . _Coerce;

-- | For a record to match a filter, the value specified for this data type
-- property must inclusively match any value between the specified minimum
-- and maximum values of the __completionTime__ property of the < Run> data
-- type.
rfCompletionTime :: Lens' RunsFilter (Maybe TimestampRange)
rfCompletionTime = lens _rfCompletionTime (\ s a -> s{_rfCompletionTime = a});

instance ToJSON RunsFilter where
        toJSON RunsFilter'{..}
          = object
              (catMaybes
                 [("creationTime" .=) <$> _rfCreationTime,
                  ("rulesPackages" .=) <$> _rfRulesPackages,
                  ("runStates" .=) <$> _rfRunStates,
                  ("runNamePatterns" .=) <$> _rfRunNamePatterns,
                  ("completionTime" .=) <$> _rfCompletionTime])

-- | A key and value pair.
--
-- This data type is used as a request parameter in the
-- < SetTagsForResource> action and a response element in the
-- < ListTagsForResource> action.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue'
--
-- * 'tagKey'
tag
    :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | The value assigned to a tag key.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The tag key.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])

-- | The metadata about the Inspector application data metrics collected by
-- the agent.
--
-- This data type is used as the response element in the
-- < GetAssessmentTelemetry> action.
--
-- /See:/ 'telemetry' smart constructor.
data Telemetry = Telemetry'
    { _tStatus                 :: !(Maybe Text)
    , _tMessageTypeTelemetries :: !(Maybe [MessageTypeTelemetry])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Telemetry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStatus'
--
-- * 'tMessageTypeTelemetries'
telemetry
    :: Telemetry
telemetry =
    Telemetry'
    { _tStatus = Nothing
    , _tMessageTypeTelemetries = Nothing
    }

-- | The category of the individual metrics that together constitute the
-- telemetry that Inspector received from the agent.
tStatus :: Lens' Telemetry (Maybe Text)
tStatus = lens _tStatus (\ s a -> s{_tStatus = a});

-- | Counts of individual metrics received by Inspector from the agent.
tMessageTypeTelemetries :: Lens' Telemetry [MessageTypeTelemetry]
tMessageTypeTelemetries = lens _tMessageTypeTelemetries (\ s a -> s{_tMessageTypeTelemetries = a}) . _Default . _Coerce;

instance FromJSON Telemetry where
        parseJSON
          = withObject "Telemetry"
              (\ x ->
                 Telemetry' <$>
                   (x .:? "status") <*>
                     (x .:? "messageTypeTelemetries" .!= mempty))

-- | This data type is used in the < AssessmentsFilter> and < RunsFilter>
-- data types.
--
-- /See:/ 'timestampRange' smart constructor.
data TimestampRange = TimestampRange'
    { _trMaximum :: !(Maybe POSIX)
    , _trMinimum :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TimestampRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trMaximum'
--
-- * 'trMinimum'
timestampRange
    :: TimestampRange
timestampRange =
    TimestampRange'
    { _trMaximum = Nothing
    , _trMinimum = Nothing
    }

-- | The maximum value of the timestamp range.
trMaximum :: Lens' TimestampRange (Maybe UTCTime)
trMaximum = lens _trMaximum (\ s a -> s{_trMaximum = a}) . mapping _Time;

-- | The minimum value of the timestamp range.
trMinimum :: Lens' TimestampRange (Maybe UTCTime)
trMinimum = lens _trMinimum (\ s a -> s{_trMinimum = a}) . mapping _Time;

instance ToJSON TimestampRange where
        toJSON TimestampRange'{..}
          = object
              (catMaybes
                 [("maximum" .=) <$> _trMaximum,
                  ("minimum" .=) <$> _trMinimum])
