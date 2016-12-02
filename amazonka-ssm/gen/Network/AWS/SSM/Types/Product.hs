{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.SSM.Types.Sum

-- | An activation registers one or more on-premises servers or virtual machines (VMs) with AWS so that you can configure those servers or VMs using Run Command. A server or VM that has been registered with AWS is called a managed instance.
--
--
--
-- /See:/ 'activation' smart constructor.
data Activation = Activation'
    { _aExpired             :: !(Maybe Bool)
    , _aDefaultInstanceName :: !(Maybe Text)
    , _aActivationId        :: !(Maybe Text)
    , _aCreatedDate         :: !(Maybe POSIX)
    , _aRegistrationLimit   :: !(Maybe Nat)
    , _aExpirationDate      :: !(Maybe POSIX)
    , _aDescription         :: !(Maybe Text)
    , _aRegistrationsCount  :: !(Maybe Nat)
    , _aIAMRole             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Activation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aExpired' - Whether or not the activation is expired.
--
-- * 'aDefaultInstanceName' - A name for the managed instance when it is created.
--
-- * 'aActivationId' - The ID created by Systems Manager when you submitted the activation.
--
-- * 'aCreatedDate' - The date the activation was created.
--
-- * 'aRegistrationLimit' - The maximum number of managed instances that can be registered using this activation.
--
-- * 'aExpirationDate' - The date when this activation can no longer be used to register managed instances.
--
-- * 'aDescription' - A user defined description of the activation.
--
-- * 'aRegistrationsCount' - The number of managed instances already registered with this activation.
--
-- * 'aIAMRole' - The Amazon Identity and Access Management (IAM) role to assign to the managed instance.
activation
    :: Activation
activation =
    Activation'
    { _aExpired = Nothing
    , _aDefaultInstanceName = Nothing
    , _aActivationId = Nothing
    , _aCreatedDate = Nothing
    , _aRegistrationLimit = Nothing
    , _aExpirationDate = Nothing
    , _aDescription = Nothing
    , _aRegistrationsCount = Nothing
    , _aIAMRole = Nothing
    }

-- | Whether or not the activation is expired.
aExpired :: Lens' Activation (Maybe Bool)
aExpired = lens _aExpired (\ s a -> s{_aExpired = a});

-- | A name for the managed instance when it is created.
aDefaultInstanceName :: Lens' Activation (Maybe Text)
aDefaultInstanceName = lens _aDefaultInstanceName (\ s a -> s{_aDefaultInstanceName = a});

-- | The ID created by Systems Manager when you submitted the activation.
aActivationId :: Lens' Activation (Maybe Text)
aActivationId = lens _aActivationId (\ s a -> s{_aActivationId = a});

-- | The date the activation was created.
aCreatedDate :: Lens' Activation (Maybe UTCTime)
aCreatedDate = lens _aCreatedDate (\ s a -> s{_aCreatedDate = a}) . mapping _Time;

-- | The maximum number of managed instances that can be registered using this activation.
aRegistrationLimit :: Lens' Activation (Maybe Natural)
aRegistrationLimit = lens _aRegistrationLimit (\ s a -> s{_aRegistrationLimit = a}) . mapping _Nat;

-- | The date when this activation can no longer be used to register managed instances.
aExpirationDate :: Lens' Activation (Maybe UTCTime)
aExpirationDate = lens _aExpirationDate (\ s a -> s{_aExpirationDate = a}) . mapping _Time;

-- | A user defined description of the activation.
aDescription :: Lens' Activation (Maybe Text)
aDescription = lens _aDescription (\ s a -> s{_aDescription = a});

-- | The number of managed instances already registered with this activation.
aRegistrationsCount :: Lens' Activation (Maybe Natural)
aRegistrationsCount = lens _aRegistrationsCount (\ s a -> s{_aRegistrationsCount = a}) . mapping _Nat;

-- | The Amazon Identity and Access Management (IAM) role to assign to the managed instance.
aIAMRole :: Lens' Activation (Maybe Text)
aIAMRole = lens _aIAMRole (\ s a -> s{_aIAMRole = a});

instance FromJSON Activation where
        parseJSON
          = withObject "Activation"
              (\ x ->
                 Activation' <$>
                   (x .:? "Expired") <*> (x .:? "DefaultInstanceName")
                     <*> (x .:? "ActivationId")
                     <*> (x .:? "CreatedDate")
                     <*> (x .:? "RegistrationLimit")
                     <*> (x .:? "ExpirationDate")
                     <*> (x .:? "Description")
                     <*> (x .:? "RegistrationsCount")
                     <*> (x .:? "IamRole"))

instance Hashable Activation

instance NFData Activation

-- | Describes an association of an SSM document and an instance.
--
--
--
-- /See:/ 'association' smart constructor.
data Association = Association'
    { _aAssociationId      :: !(Maybe Text)
    , _aInstanceId         :: !(Maybe Text)
    , _aOverview           :: !(Maybe AssociationOverview)
    , _aLastExecutionDate  :: !(Maybe POSIX)
    , _aScheduleExpression :: !(Maybe Text)
    , _aName               :: !(Maybe Text)
    , _aTargets            :: !(Maybe [Target])
    , _aDocumentVersion    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Association' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAssociationId' - The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
--
-- * 'aInstanceId' - The ID of the instance.
--
-- * 'aOverview' - Information about the association.
--
-- * 'aLastExecutionDate' - The date on which the association was last run.
--
-- * 'aScheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- * 'aName' - The name of the SSM document.
--
-- * 'aTargets' - The instances targeted by the request to create an association.
--
-- * 'aDocumentVersion' - The version of the document used in the association.
association
    :: Association
association =
    Association'
    { _aAssociationId = Nothing
    , _aInstanceId = Nothing
    , _aOverview = Nothing
    , _aLastExecutionDate = Nothing
    , _aScheduleExpression = Nothing
    , _aName = Nothing
    , _aTargets = Nothing
    , _aDocumentVersion = Nothing
    }

-- | The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
aAssociationId :: Lens' Association (Maybe Text)
aAssociationId = lens _aAssociationId (\ s a -> s{_aAssociationId = a});

-- | The ID of the instance.
aInstanceId :: Lens' Association (Maybe Text)
aInstanceId = lens _aInstanceId (\ s a -> s{_aInstanceId = a});

-- | Information about the association.
aOverview :: Lens' Association (Maybe AssociationOverview)
aOverview = lens _aOverview (\ s a -> s{_aOverview = a});

-- | The date on which the association was last run.
aLastExecutionDate :: Lens' Association (Maybe UTCTime)
aLastExecutionDate = lens _aLastExecutionDate (\ s a -> s{_aLastExecutionDate = a}) . mapping _Time;

-- | A cron expression that specifies a schedule when the association runs.
aScheduleExpression :: Lens' Association (Maybe Text)
aScheduleExpression = lens _aScheduleExpression (\ s a -> s{_aScheduleExpression = a});

-- | The name of the SSM document.
aName :: Lens' Association (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | The instances targeted by the request to create an association.
aTargets :: Lens' Association [Target]
aTargets = lens _aTargets (\ s a -> s{_aTargets = a}) . _Default . _Coerce;

-- | The version of the document used in the association.
aDocumentVersion :: Lens' Association (Maybe Text)
aDocumentVersion = lens _aDocumentVersion (\ s a -> s{_aDocumentVersion = a});

instance FromJSON Association where
        parseJSON
          = withObject "Association"
              (\ x ->
                 Association' <$>
                   (x .:? "AssociationId") <*> (x .:? "InstanceId") <*>
                     (x .:? "Overview")
                     <*> (x .:? "LastExecutionDate")
                     <*> (x .:? "ScheduleExpression")
                     <*> (x .:? "Name")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "DocumentVersion"))

instance Hashable Association

instance NFData Association

-- | Describes the parameters for a document.
--
--
--
-- /See:/ 'associationDescription' smart constructor.
data AssociationDescription = AssociationDescription'
    { _adAssociationId               :: !(Maybe Text)
    , _adInstanceId                  :: !(Maybe Text)
    , _adStatus                      :: !(Maybe AssociationStatus)
    , _adLastSuccessfulExecutionDate :: !(Maybe POSIX)
    , _adOverview                    :: !(Maybe AssociationOverview)
    , _adLastUpdateAssociationDate   :: !(Maybe POSIX)
    , _adDate                        :: !(Maybe POSIX)
    , _adLastExecutionDate           :: !(Maybe POSIX)
    , _adScheduleExpression          :: !(Maybe Text)
    , _adName                        :: !(Maybe Text)
    , _adOutputLocation              :: !(Maybe InstanceAssociationOutputLocation)
    , _adTargets                     :: !(Maybe [Target])
    , _adParameters                  :: !(Maybe (Map Text [Text]))
    , _adDocumentVersion             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAssociationId' - The association ID.
--
-- * 'adInstanceId' - The ID of the instance.
--
-- * 'adStatus' - The association status.
--
-- * 'adLastSuccessfulExecutionDate' - The last date on which the association was successfully run.
--
-- * 'adOverview' - Information about the association.
--
-- * 'adLastUpdateAssociationDate' - The date when the association was last updated.
--
-- * 'adDate' - The date when the association was made.
--
-- * 'adLastExecutionDate' - The date on which the association was last run.
--
-- * 'adScheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- * 'adName' - The name of the SSM document.
--
-- * 'adOutputLocation' - An Amazon S3 bucket where you want to store the output details of the request.
--
-- * 'adTargets' - The instances targeted by the request.
--
-- * 'adParameters' - A description of the parameters for a document.
--
-- * 'adDocumentVersion' - The document version.
associationDescription
    :: AssociationDescription
associationDescription =
    AssociationDescription'
    { _adAssociationId = Nothing
    , _adInstanceId = Nothing
    , _adStatus = Nothing
    , _adLastSuccessfulExecutionDate = Nothing
    , _adOverview = Nothing
    , _adLastUpdateAssociationDate = Nothing
    , _adDate = Nothing
    , _adLastExecutionDate = Nothing
    , _adScheduleExpression = Nothing
    , _adName = Nothing
    , _adOutputLocation = Nothing
    , _adTargets = Nothing
    , _adParameters = Nothing
    , _adDocumentVersion = Nothing
    }

-- | The association ID.
adAssociationId :: Lens' AssociationDescription (Maybe Text)
adAssociationId = lens _adAssociationId (\ s a -> s{_adAssociationId = a});

-- | The ID of the instance.
adInstanceId :: Lens' AssociationDescription (Maybe Text)
adInstanceId = lens _adInstanceId (\ s a -> s{_adInstanceId = a});

-- | The association status.
adStatus :: Lens' AssociationDescription (Maybe AssociationStatus)
adStatus = lens _adStatus (\ s a -> s{_adStatus = a});

-- | The last date on which the association was successfully run.
adLastSuccessfulExecutionDate :: Lens' AssociationDescription (Maybe UTCTime)
adLastSuccessfulExecutionDate = lens _adLastSuccessfulExecutionDate (\ s a -> s{_adLastSuccessfulExecutionDate = a}) . mapping _Time;

-- | Information about the association.
adOverview :: Lens' AssociationDescription (Maybe AssociationOverview)
adOverview = lens _adOverview (\ s a -> s{_adOverview = a});

-- | The date when the association was last updated.
adLastUpdateAssociationDate :: Lens' AssociationDescription (Maybe UTCTime)
adLastUpdateAssociationDate = lens _adLastUpdateAssociationDate (\ s a -> s{_adLastUpdateAssociationDate = a}) . mapping _Time;

-- | The date when the association was made.
adDate :: Lens' AssociationDescription (Maybe UTCTime)
adDate = lens _adDate (\ s a -> s{_adDate = a}) . mapping _Time;

-- | The date on which the association was last run.
adLastExecutionDate :: Lens' AssociationDescription (Maybe UTCTime)
adLastExecutionDate = lens _adLastExecutionDate (\ s a -> s{_adLastExecutionDate = a}) . mapping _Time;

-- | A cron expression that specifies a schedule when the association runs.
adScheduleExpression :: Lens' AssociationDescription (Maybe Text)
adScheduleExpression = lens _adScheduleExpression (\ s a -> s{_adScheduleExpression = a});

-- | The name of the SSM document.
adName :: Lens' AssociationDescription (Maybe Text)
adName = lens _adName (\ s a -> s{_adName = a});

-- | An Amazon S3 bucket where you want to store the output details of the request.
adOutputLocation :: Lens' AssociationDescription (Maybe InstanceAssociationOutputLocation)
adOutputLocation = lens _adOutputLocation (\ s a -> s{_adOutputLocation = a});

-- | The instances targeted by the request.
adTargets :: Lens' AssociationDescription [Target]
adTargets = lens _adTargets (\ s a -> s{_adTargets = a}) . _Default . _Coerce;

-- | A description of the parameters for a document.
adParameters :: Lens' AssociationDescription (HashMap Text [Text])
adParameters = lens _adParameters (\ s a -> s{_adParameters = a}) . _Default . _Map;

-- | The document version.
adDocumentVersion :: Lens' AssociationDescription (Maybe Text)
adDocumentVersion = lens _adDocumentVersion (\ s a -> s{_adDocumentVersion = a});

instance FromJSON AssociationDescription where
        parseJSON
          = withObject "AssociationDescription"
              (\ x ->
                 AssociationDescription' <$>
                   (x .:? "AssociationId") <*> (x .:? "InstanceId") <*>
                     (x .:? "Status")
                     <*> (x .:? "LastSuccessfulExecutionDate")
                     <*> (x .:? "Overview")
                     <*> (x .:? "LastUpdateAssociationDate")
                     <*> (x .:? "Date")
                     <*> (x .:? "LastExecutionDate")
                     <*> (x .:? "ScheduleExpression")
                     <*> (x .:? "Name")
                     <*> (x .:? "OutputLocation")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion"))

instance Hashable AssociationDescription

instance NFData AssociationDescription

-- | Describes a filter.
--
--
--
-- /See:/ 'associationFilter' smart constructor.
data AssociationFilter = AssociationFilter'
    { _afKey   :: !AssociationFilterKey
    , _afValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afKey' - The name of the filter.
--
-- * 'afValue' - The filter value.
associationFilter
    :: AssociationFilterKey -- ^ 'afKey'
    -> Text -- ^ 'afValue'
    -> AssociationFilter
associationFilter pKey_ pValue_ =
    AssociationFilter'
    { _afKey = pKey_
    , _afValue = pValue_
    }

-- | The name of the filter.
afKey :: Lens' AssociationFilter AssociationFilterKey
afKey = lens _afKey (\ s a -> s{_afKey = a});

-- | The filter value.
afValue :: Lens' AssociationFilter Text
afValue = lens _afValue (\ s a -> s{_afValue = a});

instance Hashable AssociationFilter

instance NFData AssociationFilter

instance ToJSON AssociationFilter where
        toJSON AssociationFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _afKey), Just ("value" .= _afValue)])

-- | Information about the association.
--
--
--
-- /See:/ 'associationOverview' smart constructor.
data AssociationOverview = AssociationOverview'
    { _aoDetailedStatus                   :: !(Maybe Text)
    , _aoStatus                           :: !(Maybe Text)
    , _aoAssociationStatusAggregatedCount :: !(Maybe (Map Text Int))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociationOverview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aoDetailedStatus' - A detailed status of the association.
--
-- * 'aoStatus' - The status of the association. Status can be: @Pending@ , @Success@ , or @Failed@ .
--
-- * 'aoAssociationStatusAggregatedCount' - Returns the number of targets for the association status. For example, if you created an association with two instances, and one of them was successful, this would return the count of instances by status.
associationOverview
    :: AssociationOverview
associationOverview =
    AssociationOverview'
    { _aoDetailedStatus = Nothing
    , _aoStatus = Nothing
    , _aoAssociationStatusAggregatedCount = Nothing
    }

-- | A detailed status of the association.
aoDetailedStatus :: Lens' AssociationOverview (Maybe Text)
aoDetailedStatus = lens _aoDetailedStatus (\ s a -> s{_aoDetailedStatus = a});

-- | The status of the association. Status can be: @Pending@ , @Success@ , or @Failed@ .
aoStatus :: Lens' AssociationOverview (Maybe Text)
aoStatus = lens _aoStatus (\ s a -> s{_aoStatus = a});

-- | Returns the number of targets for the association status. For example, if you created an association with two instances, and one of them was successful, this would return the count of instances by status.
aoAssociationStatusAggregatedCount :: Lens' AssociationOverview (HashMap Text Int)
aoAssociationStatusAggregatedCount = lens _aoAssociationStatusAggregatedCount (\ s a -> s{_aoAssociationStatusAggregatedCount = a}) . _Default . _Map;

instance FromJSON AssociationOverview where
        parseJSON
          = withObject "AssociationOverview"
              (\ x ->
                 AssociationOverview' <$>
                   (x .:? "DetailedStatus") <*> (x .:? "Status") <*>
                     (x .:? "AssociationStatusAggregatedCount" .!=
                        mempty))

instance Hashable AssociationOverview

instance NFData AssociationOverview

-- | Describes an association status.
--
--
--
-- /See:/ 'associationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
    { _asAdditionalInfo :: !(Maybe Text)
    , _asDate           :: !POSIX
    , _asName           :: !AssociationStatusName
    , _asMessage        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAdditionalInfo' - A user-defined string.
--
-- * 'asDate' - The date when the status changed.
--
-- * 'asName' - The status.
--
-- * 'asMessage' - The reason for the status.
associationStatus
    :: UTCTime -- ^ 'asDate'
    -> AssociationStatusName -- ^ 'asName'
    -> Text -- ^ 'asMessage'
    -> AssociationStatus
associationStatus pDate_ pName_ pMessage_ =
    AssociationStatus'
    { _asAdditionalInfo = Nothing
    , _asDate = _Time # pDate_
    , _asName = pName_
    , _asMessage = pMessage_
    }

-- | A user-defined string.
asAdditionalInfo :: Lens' AssociationStatus (Maybe Text)
asAdditionalInfo = lens _asAdditionalInfo (\ s a -> s{_asAdditionalInfo = a});

-- | The date when the status changed.
asDate :: Lens' AssociationStatus UTCTime
asDate = lens _asDate (\ s a -> s{_asDate = a}) . _Time;

-- | The status.
asName :: Lens' AssociationStatus AssociationStatusName
asName = lens _asName (\ s a -> s{_asName = a});

-- | The reason for the status.
asMessage :: Lens' AssociationStatus Text
asMessage = lens _asMessage (\ s a -> s{_asMessage = a});

instance FromJSON AssociationStatus where
        parseJSON
          = withObject "AssociationStatus"
              (\ x ->
                 AssociationStatus' <$>
                   (x .:? "AdditionalInfo") <*> (x .: "Date") <*>
                     (x .: "Name")
                     <*> (x .: "Message"))

instance Hashable AssociationStatus

instance NFData AssociationStatus

instance ToJSON AssociationStatus where
        toJSON AssociationStatus'{..}
          = object
              (catMaybes
                 [("AdditionalInfo" .=) <$> _asAdditionalInfo,
                  Just ("Date" .= _asDate), Just ("Name" .= _asName),
                  Just ("Message" .= _asMessage)])

-- | Detailed information about the current state of an individual Automation execution.
--
--
--
-- /See:/ 'automationExecution' smart constructor.
data AutomationExecution = AutomationExecution'
    { _aeDocumentName              :: !(Maybe Text)
    , _aeExecutionEndTime          :: !(Maybe POSIX)
    , _aeFailureMessage            :: !(Maybe Text)
    , _aeAutomationExecutionStatus :: !(Maybe AutomationExecutionStatus)
    , _aeOutputs                   :: !(Maybe (Map Text [Text]))
    , _aeExecutionStartTime        :: !(Maybe POSIX)
    , _aeParameters                :: !(Maybe (Map Text [Text]))
    , _aeDocumentVersion           :: !(Maybe Text)
    , _aeAutomationExecutionId     :: !(Maybe Text)
    , _aeStepExecutions            :: !(Maybe [StepExecution])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AutomationExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeDocumentName' - The name of the Automation document used during the execution.
--
-- * 'aeExecutionEndTime' - The time the execution finished.
--
-- * 'aeFailureMessage' - A message describing why an execution has failed, if the status is set to Failed.
--
-- * 'aeAutomationExecutionStatus' - The execution status of the Automation.
--
-- * 'aeOutputs' - The list of execution outputs as defined in the automation document.
--
-- * 'aeExecutionStartTime' - The time the execution started.
--
-- * 'aeParameters' - The key-value map of execution parameters, which were supplied when calling @StartAutomationExecution@ .
--
-- * 'aeDocumentVersion' - The version of the document to use during execution.
--
-- * 'aeAutomationExecutionId' - The execution ID.
--
-- * 'aeStepExecutions' - A list of details about the current state of all steps that comprise an execution. An Automation document contains a list of steps that are executed in order.
automationExecution
    :: AutomationExecution
automationExecution =
    AutomationExecution'
    { _aeDocumentName = Nothing
    , _aeExecutionEndTime = Nothing
    , _aeFailureMessage = Nothing
    , _aeAutomationExecutionStatus = Nothing
    , _aeOutputs = Nothing
    , _aeExecutionStartTime = Nothing
    , _aeParameters = Nothing
    , _aeDocumentVersion = Nothing
    , _aeAutomationExecutionId = Nothing
    , _aeStepExecutions = Nothing
    }

-- | The name of the Automation document used during the execution.
aeDocumentName :: Lens' AutomationExecution (Maybe Text)
aeDocumentName = lens _aeDocumentName (\ s a -> s{_aeDocumentName = a});

-- | The time the execution finished.
aeExecutionEndTime :: Lens' AutomationExecution (Maybe UTCTime)
aeExecutionEndTime = lens _aeExecutionEndTime (\ s a -> s{_aeExecutionEndTime = a}) . mapping _Time;

-- | A message describing why an execution has failed, if the status is set to Failed.
aeFailureMessage :: Lens' AutomationExecution (Maybe Text)
aeFailureMessage = lens _aeFailureMessage (\ s a -> s{_aeFailureMessage = a});

-- | The execution status of the Automation.
aeAutomationExecutionStatus :: Lens' AutomationExecution (Maybe AutomationExecutionStatus)
aeAutomationExecutionStatus = lens _aeAutomationExecutionStatus (\ s a -> s{_aeAutomationExecutionStatus = a});

-- | The list of execution outputs as defined in the automation document.
aeOutputs :: Lens' AutomationExecution (HashMap Text [Text])
aeOutputs = lens _aeOutputs (\ s a -> s{_aeOutputs = a}) . _Default . _Map;

-- | The time the execution started.
aeExecutionStartTime :: Lens' AutomationExecution (Maybe UTCTime)
aeExecutionStartTime = lens _aeExecutionStartTime (\ s a -> s{_aeExecutionStartTime = a}) . mapping _Time;

-- | The key-value map of execution parameters, which were supplied when calling @StartAutomationExecution@ .
aeParameters :: Lens' AutomationExecution (HashMap Text [Text])
aeParameters = lens _aeParameters (\ s a -> s{_aeParameters = a}) . _Default . _Map;

-- | The version of the document to use during execution.
aeDocumentVersion :: Lens' AutomationExecution (Maybe Text)
aeDocumentVersion = lens _aeDocumentVersion (\ s a -> s{_aeDocumentVersion = a});

-- | The execution ID.
aeAutomationExecutionId :: Lens' AutomationExecution (Maybe Text)
aeAutomationExecutionId = lens _aeAutomationExecutionId (\ s a -> s{_aeAutomationExecutionId = a});

-- | A list of details about the current state of all steps that comprise an execution. An Automation document contains a list of steps that are executed in order.
aeStepExecutions :: Lens' AutomationExecution [StepExecution]
aeStepExecutions = lens _aeStepExecutions (\ s a -> s{_aeStepExecutions = a}) . _Default . _Coerce;

instance FromJSON AutomationExecution where
        parseJSON
          = withObject "AutomationExecution"
              (\ x ->
                 AutomationExecution' <$>
                   (x .:? "DocumentName") <*> (x .:? "ExecutionEndTime")
                     <*> (x .:? "FailureMessage")
                     <*> (x .:? "AutomationExecutionStatus")
                     <*> (x .:? "Outputs" .!= mempty)
                     <*> (x .:? "ExecutionStartTime")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "AutomationExecutionId")
                     <*> (x .:? "StepExecutions" .!= mempty))

instance Hashable AutomationExecution

instance NFData AutomationExecution

-- | A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.
--
--
--
-- /See:/ 'automationExecutionFilter' smart constructor.
data AutomationExecutionFilter = AutomationExecutionFilter'
    { _aefKey    :: !AutomationExecutionFilterKey
    , _aefValues :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AutomationExecutionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aefKey' - The aspect of the Automation execution information that should be limited.
--
-- * 'aefValues' - The values used to limit the execution information associated with the filter's key.
automationExecutionFilter
    :: AutomationExecutionFilterKey -- ^ 'aefKey'
    -> NonEmpty Text -- ^ 'aefValues'
    -> AutomationExecutionFilter
automationExecutionFilter pKey_ pValues_ =
    AutomationExecutionFilter'
    { _aefKey = pKey_
    , _aefValues = _List1 # pValues_
    }

-- | The aspect of the Automation execution information that should be limited.
aefKey :: Lens' AutomationExecutionFilter AutomationExecutionFilterKey
aefKey = lens _aefKey (\ s a -> s{_aefKey = a});

-- | The values used to limit the execution information associated with the filter's key.
aefValues :: Lens' AutomationExecutionFilter (NonEmpty Text)
aefValues = lens _aefValues (\ s a -> s{_aefValues = a}) . _List1;

instance Hashable AutomationExecutionFilter

instance NFData AutomationExecutionFilter

instance ToJSON AutomationExecutionFilter where
        toJSON AutomationExecutionFilter'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _aefKey),
                  Just ("Values" .= _aefValues)])

-- | Details about a specific Automation execution.
--
--
--
-- /See:/ 'automationExecutionMetadata' smart constructor.
data AutomationExecutionMetadata = AutomationExecutionMetadata'
    { _aemLogFile                   :: !(Maybe Text)
    , _aemExecutedBy                :: !(Maybe Text)
    , _aemDocumentName              :: !(Maybe Text)
    , _aemExecutionEndTime          :: !(Maybe POSIX)
    , _aemAutomationExecutionStatus :: !(Maybe AutomationExecutionStatus)
    , _aemOutputs                   :: !(Maybe (Map Text [Text]))
    , _aemExecutionStartTime        :: !(Maybe POSIX)
    , _aemDocumentVersion           :: !(Maybe Text)
    , _aemAutomationExecutionId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AutomationExecutionMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aemLogFile' - An Amazon S3 bucket where execution information is stored.
--
-- * 'aemExecutedBy' - The IAM role ARN of the user who executed the Automation.
--
-- * 'aemDocumentName' - The name of the Automation document used during execution.
--
-- * 'aemExecutionEndTime' - The time the execution finished. This is not populated if the execution is still in progress.
--
-- * 'aemAutomationExecutionStatus' - The status of the execution. Valid values include: Running, Succeeded, Failed, Timed out, or Cancelled.
--
-- * 'aemOutputs' - The list of execution outputs as defined in the Automation document.
--
-- * 'aemExecutionStartTime' - The time the execution started.>
--
-- * 'aemDocumentVersion' - The document version used during the execution.
--
-- * 'aemAutomationExecutionId' - The execution ID.
automationExecutionMetadata
    :: AutomationExecutionMetadata
automationExecutionMetadata =
    AutomationExecutionMetadata'
    { _aemLogFile = Nothing
    , _aemExecutedBy = Nothing
    , _aemDocumentName = Nothing
    , _aemExecutionEndTime = Nothing
    , _aemAutomationExecutionStatus = Nothing
    , _aemOutputs = Nothing
    , _aemExecutionStartTime = Nothing
    , _aemDocumentVersion = Nothing
    , _aemAutomationExecutionId = Nothing
    }

-- | An Amazon S3 bucket where execution information is stored.
aemLogFile :: Lens' AutomationExecutionMetadata (Maybe Text)
aemLogFile = lens _aemLogFile (\ s a -> s{_aemLogFile = a});

-- | The IAM role ARN of the user who executed the Automation.
aemExecutedBy :: Lens' AutomationExecutionMetadata (Maybe Text)
aemExecutedBy = lens _aemExecutedBy (\ s a -> s{_aemExecutedBy = a});

-- | The name of the Automation document used during execution.
aemDocumentName :: Lens' AutomationExecutionMetadata (Maybe Text)
aemDocumentName = lens _aemDocumentName (\ s a -> s{_aemDocumentName = a});

-- | The time the execution finished. This is not populated if the execution is still in progress.
aemExecutionEndTime :: Lens' AutomationExecutionMetadata (Maybe UTCTime)
aemExecutionEndTime = lens _aemExecutionEndTime (\ s a -> s{_aemExecutionEndTime = a}) . mapping _Time;

-- | The status of the execution. Valid values include: Running, Succeeded, Failed, Timed out, or Cancelled.
aemAutomationExecutionStatus :: Lens' AutomationExecutionMetadata (Maybe AutomationExecutionStatus)
aemAutomationExecutionStatus = lens _aemAutomationExecutionStatus (\ s a -> s{_aemAutomationExecutionStatus = a});

-- | The list of execution outputs as defined in the Automation document.
aemOutputs :: Lens' AutomationExecutionMetadata (HashMap Text [Text])
aemOutputs = lens _aemOutputs (\ s a -> s{_aemOutputs = a}) . _Default . _Map;

-- | The time the execution started.>
aemExecutionStartTime :: Lens' AutomationExecutionMetadata (Maybe UTCTime)
aemExecutionStartTime = lens _aemExecutionStartTime (\ s a -> s{_aemExecutionStartTime = a}) . mapping _Time;

-- | The document version used during the execution.
aemDocumentVersion :: Lens' AutomationExecutionMetadata (Maybe Text)
aemDocumentVersion = lens _aemDocumentVersion (\ s a -> s{_aemDocumentVersion = a});

-- | The execution ID.
aemAutomationExecutionId :: Lens' AutomationExecutionMetadata (Maybe Text)
aemAutomationExecutionId = lens _aemAutomationExecutionId (\ s a -> s{_aemAutomationExecutionId = a});

instance FromJSON AutomationExecutionMetadata where
        parseJSON
          = withObject "AutomationExecutionMetadata"
              (\ x ->
                 AutomationExecutionMetadata' <$>
                   (x .:? "LogFile") <*> (x .:? "ExecutedBy") <*>
                     (x .:? "DocumentName")
                     <*> (x .:? "ExecutionEndTime")
                     <*> (x .:? "AutomationExecutionStatus")
                     <*> (x .:? "Outputs" .!= mempty)
                     <*> (x .:? "ExecutionStartTime")
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "AutomationExecutionId"))

instance Hashable AutomationExecutionMetadata

instance NFData AutomationExecutionMetadata

-- | Describes a command request.
--
--
--
-- /See:/ 'command' smart constructor.
data Command = Command'
    { _cStatus             :: !(Maybe CommandStatus)
    , _cExpiresAfter       :: !(Maybe POSIX)
    , _cNotificationConfig :: !(Maybe NotificationConfig)
    , _cTargetCount        :: !(Maybe Int)
    , _cOutputS3KeyPrefix  :: !(Maybe Text)
    , _cDocumentName       :: !(Maybe Text)
    , _cErrorCount         :: !(Maybe Int)
    , _cStatusDetails      :: !(Maybe Text)
    , _cMaxErrors          :: !(Maybe Text)
    , _cInstanceIds        :: !(Maybe [Text])
    , _cOutputS3Region     :: !(Maybe Text)
    , _cTargets            :: !(Maybe [Target])
    , _cCommandId          :: !(Maybe Text)
    , _cParameters         :: !(Maybe (Map Text [Text]))
    , _cComment            :: !(Maybe Text)
    , _cCompletedCount     :: !(Maybe Int)
    , _cOutputS3BucketName :: !(Maybe Text)
    , _cMaxConcurrency     :: !(Maybe Text)
    , _cRequestedDateTime  :: !(Maybe POSIX)
    , _cServiceRole        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The status of the command.
--
-- * 'cExpiresAfter' - If this time is reached and the command has not already started executing, it will not execute. Calculated based on the ExpiresAfter user input provided as part of the SendCommand API.
--
-- * 'cNotificationConfig' - Configurations for sending notifications about command status changes.
--
-- * 'cTargetCount' - The number of targets for the command.
--
-- * 'cOutputS3KeyPrefix' - The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
--
-- * 'cDocumentName' - The name of the SSM document requested for execution.
--
-- * 'cErrorCount' - The number of targets for which the status is @Failed@ or @Execution Timed Out@ .
--
-- * 'cStatusDetails' - A detailed status of the command execution. @StatusDetails@ includes more information than @Status@ because it includes states resulting from error and concurrency control parameters. @StatusDetails@ can show different results than @Status@ . For more information about these statuses, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitor-commands.html Monitor Commands> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/monitor-commands.html Monitor Commands> (Windows). @StatusDetails@ can be one of the following values:     * Pending – The command has not been sent to any instances.     * In Progress – The command has been sent to at least one instance but has not reached a final state on all instances.     * Success – The command successfully executed on all invocations. This is a terminal state.     * Delivery Timed Out – The value of @MaxErrors@ or more command invocations shows a status of @Delivery Timed Out@ . This is a terminal state.     * Execution Timed Out – The value of @MaxErrors@ or more command invocations shows a status of @Execution Timed Out@ . This is a terminal state.     * Failed – The value of @MaxErrors@ or more command invocations shows a status of @Failed@ . This is a terminal state.     * Incomplete – The command was attempted on all instances and one or more invocations does not have a value of @Success@ but not enough invocations failed for the status to be @Failed@ . This is a terminal state.     * Canceled – The command was terminated before it was completed. This is a terminal state.     * Rate Exceeded – The number of instances targeted by the command exceeded the account limit for pending invocations. The system has canceled the command before executing it on any instance. This is a terminal state.
--
-- * 'cMaxErrors' - The maximum number of errors allowed before the system stops sending the command to additional targets. You can specify a number of errors, such as 10, or a percentage or errors, such as 10%. The default value is 50. For more information about how to use @MaxErrors@ , see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Windows).
--
-- * 'cInstanceIds' - The instance IDs against which this command was requested.
--
-- * 'cOutputS3Region' - The region where the Amazon Simple Storage Service (Amazon S3) output bucket is located. The default value is the region where Run Command is being called.
--
-- * 'cTargets' - An array of search criteria that targets instances using a @Key@ ;@Value@ combination that you specify. @Targets@ is required if you don't provide one or more instance IDs in the call.
--
-- * 'cCommandId' - A unique identifier for this command.
--
-- * 'cParameters' - The parameter values to be inserted in the SSM document when executing the command.
--
-- * 'cComment' - User-specified information about the command, such as a brief description of what the command should do.
--
-- * 'cCompletedCount' - The number of targets for which the command invocation reached a terminal state. Terminal states include the following: @Success@ , @Failed@ , @Execution Timed Out@ , @Delivery Timed Out@ , @Canceled@ , @Terminated@ , or @Undeliverable@ .
--
-- * 'cOutputS3BucketName' - The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
--
-- * 'cMaxConcurrency' - The maximum number of instances that are allowed to execute the command at the same time. You can specify a number of instances, such as 10, or a percentage of instances, such as 10%. The default value is 50. For more information about how to use @MaxConcurrency@ , see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Windows).
--
-- * 'cRequestedDateTime' - The date and time the command was requested.
--
-- * 'cServiceRole' - The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes.
command
    :: Command
command =
    Command'
    { _cStatus = Nothing
    , _cExpiresAfter = Nothing
    , _cNotificationConfig = Nothing
    , _cTargetCount = Nothing
    , _cOutputS3KeyPrefix = Nothing
    , _cDocumentName = Nothing
    , _cErrorCount = Nothing
    , _cStatusDetails = Nothing
    , _cMaxErrors = Nothing
    , _cInstanceIds = Nothing
    , _cOutputS3Region = Nothing
    , _cTargets = Nothing
    , _cCommandId = Nothing
    , _cParameters = Nothing
    , _cComment = Nothing
    , _cCompletedCount = Nothing
    , _cOutputS3BucketName = Nothing
    , _cMaxConcurrency = Nothing
    , _cRequestedDateTime = Nothing
    , _cServiceRole = Nothing
    }

-- | The status of the command.
cStatus :: Lens' Command (Maybe CommandStatus)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});

-- | If this time is reached and the command has not already started executing, it will not execute. Calculated based on the ExpiresAfter user input provided as part of the SendCommand API.
cExpiresAfter :: Lens' Command (Maybe UTCTime)
cExpiresAfter = lens _cExpiresAfter (\ s a -> s{_cExpiresAfter = a}) . mapping _Time;

-- | Configurations for sending notifications about command status changes.
cNotificationConfig :: Lens' Command (Maybe NotificationConfig)
cNotificationConfig = lens _cNotificationConfig (\ s a -> s{_cNotificationConfig = a});

-- | The number of targets for the command.
cTargetCount :: Lens' Command (Maybe Int)
cTargetCount = lens _cTargetCount (\ s a -> s{_cTargetCount = a});

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
cOutputS3KeyPrefix :: Lens' Command (Maybe Text)
cOutputS3KeyPrefix = lens _cOutputS3KeyPrefix (\ s a -> s{_cOutputS3KeyPrefix = a});

-- | The name of the SSM document requested for execution.
cDocumentName :: Lens' Command (Maybe Text)
cDocumentName = lens _cDocumentName (\ s a -> s{_cDocumentName = a});

-- | The number of targets for which the status is @Failed@ or @Execution Timed Out@ .
cErrorCount :: Lens' Command (Maybe Int)
cErrorCount = lens _cErrorCount (\ s a -> s{_cErrorCount = a});

-- | A detailed status of the command execution. @StatusDetails@ includes more information than @Status@ because it includes states resulting from error and concurrency control parameters. @StatusDetails@ can show different results than @Status@ . For more information about these statuses, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitor-commands.html Monitor Commands> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/monitor-commands.html Monitor Commands> (Windows). @StatusDetails@ can be one of the following values:     * Pending – The command has not been sent to any instances.     * In Progress – The command has been sent to at least one instance but has not reached a final state on all instances.     * Success – The command successfully executed on all invocations. This is a terminal state.     * Delivery Timed Out – The value of @MaxErrors@ or more command invocations shows a status of @Delivery Timed Out@ . This is a terminal state.     * Execution Timed Out – The value of @MaxErrors@ or more command invocations shows a status of @Execution Timed Out@ . This is a terminal state.     * Failed – The value of @MaxErrors@ or more command invocations shows a status of @Failed@ . This is a terminal state.     * Incomplete – The command was attempted on all instances and one or more invocations does not have a value of @Success@ but not enough invocations failed for the status to be @Failed@ . This is a terminal state.     * Canceled – The command was terminated before it was completed. This is a terminal state.     * Rate Exceeded – The number of instances targeted by the command exceeded the account limit for pending invocations. The system has canceled the command before executing it on any instance. This is a terminal state.
cStatusDetails :: Lens' Command (Maybe Text)
cStatusDetails = lens _cStatusDetails (\ s a -> s{_cStatusDetails = a});

-- | The maximum number of errors allowed before the system stops sending the command to additional targets. You can specify a number of errors, such as 10, or a percentage or errors, such as 10%. The default value is 50. For more information about how to use @MaxErrors@ , see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Windows).
cMaxErrors :: Lens' Command (Maybe Text)
cMaxErrors = lens _cMaxErrors (\ s a -> s{_cMaxErrors = a});

-- | The instance IDs against which this command was requested.
cInstanceIds :: Lens' Command [Text]
cInstanceIds = lens _cInstanceIds (\ s a -> s{_cInstanceIds = a}) . _Default . _Coerce;

-- | The region where the Amazon Simple Storage Service (Amazon S3) output bucket is located. The default value is the region where Run Command is being called.
cOutputS3Region :: Lens' Command (Maybe Text)
cOutputS3Region = lens _cOutputS3Region (\ s a -> s{_cOutputS3Region = a});

-- | An array of search criteria that targets instances using a @Key@ ;@Value@ combination that you specify. @Targets@ is required if you don't provide one or more instance IDs in the call.
cTargets :: Lens' Command [Target]
cTargets = lens _cTargets (\ s a -> s{_cTargets = a}) . _Default . _Coerce;

-- | A unique identifier for this command.
cCommandId :: Lens' Command (Maybe Text)
cCommandId = lens _cCommandId (\ s a -> s{_cCommandId = a});

-- | The parameter values to be inserted in the SSM document when executing the command.
cParameters :: Lens' Command (HashMap Text [Text])
cParameters = lens _cParameters (\ s a -> s{_cParameters = a}) . _Default . _Map;

-- | User-specified information about the command, such as a brief description of what the command should do.
cComment :: Lens' Command (Maybe Text)
cComment = lens _cComment (\ s a -> s{_cComment = a});

-- | The number of targets for which the command invocation reached a terminal state. Terminal states include the following: @Success@ , @Failed@ , @Execution Timed Out@ , @Delivery Timed Out@ , @Canceled@ , @Terminated@ , or @Undeliverable@ .
cCompletedCount :: Lens' Command (Maybe Int)
cCompletedCount = lens _cCompletedCount (\ s a -> s{_cCompletedCount = a});

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
cOutputS3BucketName :: Lens' Command (Maybe Text)
cOutputS3BucketName = lens _cOutputS3BucketName (\ s a -> s{_cOutputS3BucketName = a});

-- | The maximum number of instances that are allowed to execute the command at the same time. You can specify a number of instances, such as 10, or a percentage of instances, such as 10%. The default value is 50. For more information about how to use @MaxConcurrency@ , see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Windows).
cMaxConcurrency :: Lens' Command (Maybe Text)
cMaxConcurrency = lens _cMaxConcurrency (\ s a -> s{_cMaxConcurrency = a});

-- | The date and time the command was requested.
cRequestedDateTime :: Lens' Command (Maybe UTCTime)
cRequestedDateTime = lens _cRequestedDateTime (\ s a -> s{_cRequestedDateTime = a}) . mapping _Time;

-- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes.
cServiceRole :: Lens' Command (Maybe Text)
cServiceRole = lens _cServiceRole (\ s a -> s{_cServiceRole = a});

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "Status") <*> (x .:? "ExpiresAfter") <*>
                     (x .:? "NotificationConfig")
                     <*> (x .:? "TargetCount")
                     <*> (x .:? "OutputS3KeyPrefix")
                     <*> (x .:? "DocumentName")
                     <*> (x .:? "ErrorCount")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "MaxErrors")
                     <*> (x .:? "InstanceIds" .!= mempty)
                     <*> (x .:? "OutputS3Region")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "CommandId")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "Comment")
                     <*> (x .:? "CompletedCount")
                     <*> (x .:? "OutputS3BucketName")
                     <*> (x .:? "MaxConcurrency")
                     <*> (x .:? "RequestedDateTime")
                     <*> (x .:? "ServiceRole"))

instance Hashable Command

instance NFData Command

-- | Describes a command filter.
--
--
--
-- /See:/ 'commandFilter' smart constructor.
data CommandFilter = CommandFilter'
    { _cfKey   :: !CommandFilterKey
    , _cfValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommandFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfKey' - The name of the filter. For example, requested date and time.
--
-- * 'cfValue' - The filter value. For example: June 30, 2015.
commandFilter
    :: CommandFilterKey -- ^ 'cfKey'
    -> Text -- ^ 'cfValue'
    -> CommandFilter
commandFilter pKey_ pValue_ =
    CommandFilter'
    { _cfKey = pKey_
    , _cfValue = pValue_
    }

-- | The name of the filter. For example, requested date and time.
cfKey :: Lens' CommandFilter CommandFilterKey
cfKey = lens _cfKey (\ s a -> s{_cfKey = a});

-- | The filter value. For example: June 30, 2015.
cfValue :: Lens' CommandFilter Text
cfValue = lens _cfValue (\ s a -> s{_cfValue = a});

instance Hashable CommandFilter

instance NFData CommandFilter

instance ToJSON CommandFilter where
        toJSON CommandFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _cfKey), Just ("value" .= _cfValue)])

-- | An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user executes SendCommand against three instances, then a command invocation is created for each requested instance ID. A command invocation returns status and detail information about a command you executed.
--
--
--
-- /See:/ 'commandInvocation' smart constructor.
data CommandInvocation = CommandInvocation'
    { _ciInstanceId         :: !(Maybe Text)
    , _ciStatus             :: !(Maybe CommandInvocationStatus)
    , _ciNotificationConfig :: !(Maybe NotificationConfig)
    , _ciCommandPlugins     :: !(Maybe [CommandPlugin])
    , _ciDocumentName       :: !(Maybe Text)
    , _ciStandardErrorURL   :: !(Maybe Text)
    , _ciStatusDetails      :: !(Maybe Text)
    , _ciStandardOutputURL  :: !(Maybe Text)
    , _ciCommandId          :: !(Maybe Text)
    , _ciComment            :: !(Maybe Text)
    , _ciTraceOutput        :: !(Maybe Text)
    , _ciInstanceName       :: !(Maybe Text)
    , _ciRequestedDateTime  :: !(Maybe POSIX)
    , _ciServiceRole        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommandInvocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciInstanceId' - The instance ID in which this invocation was requested.
--
-- * 'ciStatus' - Whether or not the invocation succeeded, failed, or is pending.
--
-- * 'ciNotificationConfig' - Configurations for sending notifications about command status changes on a per instance basis.
--
-- * 'ciCommandPlugins' - Undocumented member.
--
-- * 'ciDocumentName' - The document name that was requested for execution.
--
-- * 'ciStandardErrorURL' - The URL to the plugin’s StdErr file in Amazon S3, if the Amazon S3 bucket was defined for the parent command. For an invocation, @StandardErrorUrl@ is populated if there is just one plugin defined for the command, and the Amazon S3 bucket was defined for the command.
--
-- * 'ciStatusDetails' - A detailed status of the command execution for each invocation (each instance targeted by the command). @StatusDetails@ includes more information than @Status@ because it includes states resulting from error and concurrency control parameters. @StatusDetails@ can show different results than @Status@ . For more information about these statuses, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitor-commands.html Monitor Commands> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/monitor-commands.html Monitor Commands> (Windows). @StatusDetails@ can be one of the following values:      * Pending – The command has not been sent to the instance.     * In Progress – The command has been sent to the instance but has not reached a terminal state.     * Success – The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out – The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command’s @MaxErrors@ limit, but they do contribute to whether the parent command status is @Success@ or @Incomplete@ . This is a terminal state.     * Execution Timed Out – Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the @MaxErrors@ limit of the parent command. This is a terminal state.     * Failed – The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the @MaxErrors@ limit of the parent command. This is a terminal state.     * Canceled – The command was terminated before it was completed. This is a terminal state.     * Undeliverable – The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command’s @MaxErrors@ limit and don't contribute to whether the parent command status is @Success@ or @Incomplete@ . This is a terminal state.     * Terminated – The parent command exceeded its @MaxErrors@ limit and subsequent command invocations were canceled by the system. This is a terminal state.
--
-- * 'ciStandardOutputURL' - The URL to the plugin’s StdOut file in Amazon S3, if the Amazon S3 bucket was defined for the parent command. For an invocation, @StandardOutputUrl@ is populated if there is just one plugin defined for the command, and the Amazon S3 bucket was defined for the command.
--
-- * 'ciCommandId' - The command against which this invocation was requested.
--
-- * 'ciComment' - User-specified information about the command, such as a brief description of what the command should do.
--
-- * 'ciTraceOutput' - Gets the trace output sent by the agent.
--
-- * 'ciInstanceName' - The name of the invocation target. For Amazon EC2 instances this is the value for the @aws:Name@ tag. For on-premises instances, this is the name of the instance.
--
-- * 'ciRequestedDateTime' - The time and date the request was sent to this instance.
--
-- * 'ciServiceRole' - The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
commandInvocation
    :: CommandInvocation
commandInvocation =
    CommandInvocation'
    { _ciInstanceId = Nothing
    , _ciStatus = Nothing
    , _ciNotificationConfig = Nothing
    , _ciCommandPlugins = Nothing
    , _ciDocumentName = Nothing
    , _ciStandardErrorURL = Nothing
    , _ciStatusDetails = Nothing
    , _ciStandardOutputURL = Nothing
    , _ciCommandId = Nothing
    , _ciComment = Nothing
    , _ciTraceOutput = Nothing
    , _ciInstanceName = Nothing
    , _ciRequestedDateTime = Nothing
    , _ciServiceRole = Nothing
    }

-- | The instance ID in which this invocation was requested.
ciInstanceId :: Lens' CommandInvocation (Maybe Text)
ciInstanceId = lens _ciInstanceId (\ s a -> s{_ciInstanceId = a});

-- | Whether or not the invocation succeeded, failed, or is pending.
ciStatus :: Lens' CommandInvocation (Maybe CommandInvocationStatus)
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a});

-- | Configurations for sending notifications about command status changes on a per instance basis.
ciNotificationConfig :: Lens' CommandInvocation (Maybe NotificationConfig)
ciNotificationConfig = lens _ciNotificationConfig (\ s a -> s{_ciNotificationConfig = a});

-- | Undocumented member.
ciCommandPlugins :: Lens' CommandInvocation [CommandPlugin]
ciCommandPlugins = lens _ciCommandPlugins (\ s a -> s{_ciCommandPlugins = a}) . _Default . _Coerce;

-- | The document name that was requested for execution.
ciDocumentName :: Lens' CommandInvocation (Maybe Text)
ciDocumentName = lens _ciDocumentName (\ s a -> s{_ciDocumentName = a});

-- | The URL to the plugin’s StdErr file in Amazon S3, if the Amazon S3 bucket was defined for the parent command. For an invocation, @StandardErrorUrl@ is populated if there is just one plugin defined for the command, and the Amazon S3 bucket was defined for the command.
ciStandardErrorURL :: Lens' CommandInvocation (Maybe Text)
ciStandardErrorURL = lens _ciStandardErrorURL (\ s a -> s{_ciStandardErrorURL = a});

-- | A detailed status of the command execution for each invocation (each instance targeted by the command). @StatusDetails@ includes more information than @Status@ because it includes states resulting from error and concurrency control parameters. @StatusDetails@ can show different results than @Status@ . For more information about these statuses, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitor-commands.html Monitor Commands> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/monitor-commands.html Monitor Commands> (Windows). @StatusDetails@ can be one of the following values:      * Pending – The command has not been sent to the instance.     * In Progress – The command has been sent to the instance but has not reached a terminal state.     * Success – The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out – The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command’s @MaxErrors@ limit, but they do contribute to whether the parent command status is @Success@ or @Incomplete@ . This is a terminal state.     * Execution Timed Out – Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the @MaxErrors@ limit of the parent command. This is a terminal state.     * Failed – The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the @MaxErrors@ limit of the parent command. This is a terminal state.     * Canceled – The command was terminated before it was completed. This is a terminal state.     * Undeliverable – The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command’s @MaxErrors@ limit and don't contribute to whether the parent command status is @Success@ or @Incomplete@ . This is a terminal state.     * Terminated – The parent command exceeded its @MaxErrors@ limit and subsequent command invocations were canceled by the system. This is a terminal state.
ciStatusDetails :: Lens' CommandInvocation (Maybe Text)
ciStatusDetails = lens _ciStatusDetails (\ s a -> s{_ciStatusDetails = a});

-- | The URL to the plugin’s StdOut file in Amazon S3, if the Amazon S3 bucket was defined for the parent command. For an invocation, @StandardOutputUrl@ is populated if there is just one plugin defined for the command, and the Amazon S3 bucket was defined for the command.
ciStandardOutputURL :: Lens' CommandInvocation (Maybe Text)
ciStandardOutputURL = lens _ciStandardOutputURL (\ s a -> s{_ciStandardOutputURL = a});

-- | The command against which this invocation was requested.
ciCommandId :: Lens' CommandInvocation (Maybe Text)
ciCommandId = lens _ciCommandId (\ s a -> s{_ciCommandId = a});

-- | User-specified information about the command, such as a brief description of what the command should do.
ciComment :: Lens' CommandInvocation (Maybe Text)
ciComment = lens _ciComment (\ s a -> s{_ciComment = a});

-- | Gets the trace output sent by the agent.
ciTraceOutput :: Lens' CommandInvocation (Maybe Text)
ciTraceOutput = lens _ciTraceOutput (\ s a -> s{_ciTraceOutput = a});

-- | The name of the invocation target. For Amazon EC2 instances this is the value for the @aws:Name@ tag. For on-premises instances, this is the name of the instance.
ciInstanceName :: Lens' CommandInvocation (Maybe Text)
ciInstanceName = lens _ciInstanceName (\ s a -> s{_ciInstanceName = a});

-- | The time and date the request was sent to this instance.
ciRequestedDateTime :: Lens' CommandInvocation (Maybe UTCTime)
ciRequestedDateTime = lens _ciRequestedDateTime (\ s a -> s{_ciRequestedDateTime = a}) . mapping _Time;

-- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
ciServiceRole :: Lens' CommandInvocation (Maybe Text)
ciServiceRole = lens _ciServiceRole (\ s a -> s{_ciServiceRole = a});

instance FromJSON CommandInvocation where
        parseJSON
          = withObject "CommandInvocation"
              (\ x ->
                 CommandInvocation' <$>
                   (x .:? "InstanceId") <*> (x .:? "Status") <*>
                     (x .:? "NotificationConfig")
                     <*> (x .:? "CommandPlugins" .!= mempty)
                     <*> (x .:? "DocumentName")
                     <*> (x .:? "StandardErrorUrl")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "StandardOutputUrl")
                     <*> (x .:? "CommandId")
                     <*> (x .:? "Comment")
                     <*> (x .:? "TraceOutput")
                     <*> (x .:? "InstanceName")
                     <*> (x .:? "RequestedDateTime")
                     <*> (x .:? "ServiceRole"))

instance Hashable CommandInvocation

instance NFData CommandInvocation

-- | Describes plugin details.
--
--
--
-- /See:/ 'commandPlugin' smart constructor.
data CommandPlugin = CommandPlugin'
    { _cpStatus                 :: !(Maybe CommandPluginStatus)
    , _cpResponseStartDateTime  :: !(Maybe POSIX)
    , _cpOutputS3KeyPrefix      :: !(Maybe Text)
    , _cpStandardErrorURL       :: !(Maybe Text)
    , _cpResponseCode           :: !(Maybe Int)
    , _cpStatusDetails          :: !(Maybe Text)
    , _cpOutput                 :: !(Maybe Text)
    , _cpStandardOutputURL      :: !(Maybe Text)
    , _cpName                   :: !(Maybe Text)
    , _cpOutputS3Region         :: !(Maybe Text)
    , _cpOutputS3BucketName     :: !(Maybe Text)
    , _cpResponseFinishDateTime :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommandPlugin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpStatus' - The status of this plugin. You can execute a document with multiple plugins.
--
-- * 'cpResponseStartDateTime' - The time the plugin started executing.
--
-- * 'cpOutputS3KeyPrefix' - The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: @test_folder/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-1234567876543/awsrunShellScript@  @test_folder@ is the name of the Amazon S3 bucket; @ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix@ is the name of the S3 prefix; @i-1234567876543@ is the instance ID; @awsrunShellScript@ is the name of the plugin.
--
-- * 'cpStandardErrorURL' - The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
--
-- * 'cpResponseCode' - A numeric response code generated after executing the plugin.
--
-- * 'cpStatusDetails' - A detailed status of the plugin execution. @StatusDetails@ includes more information than @Status@ because it includes states resulting from error and concurrency control parameters. @StatusDetails@ can show different results than @Status@ . For more information about these statuses, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitor-commands.html Monitor Commands> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/monitor-commands.html Monitor Commands> (Windows). @StatusDetails@ can be one of the following values:     * Pending – The command has not been sent to the instance.     * In Progress – The command has been sent to the instance but has not reached a terminal state.     * Success – The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out – The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command’s @MaxErrors@ limit, but they do contribute to whether the parent command status is @Success@ or @Incomplete@ . This is a terminal state.     * Execution Timed Out – Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the @MaxErrors@ limit of the parent command. This is a terminal state.     * Failed – The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the @MaxErrors@ limit of the parent command. This is a terminal state.     * Canceled – The command was terminated before it was completed. This is a terminal state.     * Undeliverable – The command can't be delivered to the instance. The instance might not exist, or it might not be responding. Undeliverable invocations don't count against the parent command’s @MaxErrors@ limit, and they don't contribute to whether the parent command status is @Success@ or @Incomplete@ . This is a terminal state.     * Terminated – The parent command exceeded its @MaxErrors@ limit and subsequent command invocations were canceled by the system. This is a terminal state.
--
-- * 'cpOutput' - Output of the plugin execution.
--
-- * 'cpStandardOutputURL' - The URL for the complete text written by the plugin to stdout in Amazon S3. If the Amazon S3 bucket for the command was not specified, then this string is empty.
--
-- * 'cpName' - The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
--
-- * 'cpOutputS3Region' - The name of the region where the output is stored in Amazon S3.
--
-- * 'cpOutputS3BucketName' - The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: @test_folder/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-1234567876543/awsrunShellScript@  @test_folder@ is the name of the Amazon S3 bucket; @ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix@ is the name of the S3 prefix; @i-1234567876543@ is the instance ID; @awsrunShellScript@ is the name of the plugin.
--
-- * 'cpResponseFinishDateTime' - The time the plugin stopped executing. Could stop prematurely if, for example, a cancel command was sent.
commandPlugin
    :: CommandPlugin
commandPlugin =
    CommandPlugin'
    { _cpStatus = Nothing
    , _cpResponseStartDateTime = Nothing
    , _cpOutputS3KeyPrefix = Nothing
    , _cpStandardErrorURL = Nothing
    , _cpResponseCode = Nothing
    , _cpStatusDetails = Nothing
    , _cpOutput = Nothing
    , _cpStandardOutputURL = Nothing
    , _cpName = Nothing
    , _cpOutputS3Region = Nothing
    , _cpOutputS3BucketName = Nothing
    , _cpResponseFinishDateTime = Nothing
    }

-- | The status of this plugin. You can execute a document with multiple plugins.
cpStatus :: Lens' CommandPlugin (Maybe CommandPluginStatus)
cpStatus = lens _cpStatus (\ s a -> s{_cpStatus = a});

-- | The time the plugin started executing.
cpResponseStartDateTime :: Lens' CommandPlugin (Maybe UTCTime)
cpResponseStartDateTime = lens _cpResponseStartDateTime (\ s a -> s{_cpResponseStartDateTime = a}) . mapping _Time;

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: @test_folder/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-1234567876543/awsrunShellScript@  @test_folder@ is the name of the Amazon S3 bucket; @ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix@ is the name of the S3 prefix; @i-1234567876543@ is the instance ID; @awsrunShellScript@ is the name of the plugin.
cpOutputS3KeyPrefix :: Lens' CommandPlugin (Maybe Text)
cpOutputS3KeyPrefix = lens _cpOutputS3KeyPrefix (\ s a -> s{_cpOutputS3KeyPrefix = a});

-- | The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
cpStandardErrorURL :: Lens' CommandPlugin (Maybe Text)
cpStandardErrorURL = lens _cpStandardErrorURL (\ s a -> s{_cpStandardErrorURL = a});

-- | A numeric response code generated after executing the plugin.
cpResponseCode :: Lens' CommandPlugin (Maybe Int)
cpResponseCode = lens _cpResponseCode (\ s a -> s{_cpResponseCode = a});

-- | A detailed status of the plugin execution. @StatusDetails@ includes more information than @Status@ because it includes states resulting from error and concurrency control parameters. @StatusDetails@ can show different results than @Status@ . For more information about these statuses, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitor-commands.html Monitor Commands> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/monitor-commands.html Monitor Commands> (Windows). @StatusDetails@ can be one of the following values:     * Pending – The command has not been sent to the instance.     * In Progress – The command has been sent to the instance but has not reached a terminal state.     * Success – The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out – The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command’s @MaxErrors@ limit, but they do contribute to whether the parent command status is @Success@ or @Incomplete@ . This is a terminal state.     * Execution Timed Out – Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the @MaxErrors@ limit of the parent command. This is a terminal state.     * Failed – The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the @MaxErrors@ limit of the parent command. This is a terminal state.     * Canceled – The command was terminated before it was completed. This is a terminal state.     * Undeliverable – The command can't be delivered to the instance. The instance might not exist, or it might not be responding. Undeliverable invocations don't count against the parent command’s @MaxErrors@ limit, and they don't contribute to whether the parent command status is @Success@ or @Incomplete@ . This is a terminal state.     * Terminated – The parent command exceeded its @MaxErrors@ limit and subsequent command invocations were canceled by the system. This is a terminal state.
cpStatusDetails :: Lens' CommandPlugin (Maybe Text)
cpStatusDetails = lens _cpStatusDetails (\ s a -> s{_cpStatusDetails = a});

-- | Output of the plugin execution.
cpOutput :: Lens' CommandPlugin (Maybe Text)
cpOutput = lens _cpOutput (\ s a -> s{_cpOutput = a});

-- | The URL for the complete text written by the plugin to stdout in Amazon S3. If the Amazon S3 bucket for the command was not specified, then this string is empty.
cpStandardOutputURL :: Lens' CommandPlugin (Maybe Text)
cpStandardOutputURL = lens _cpStandardOutputURL (\ s a -> s{_cpStandardOutputURL = a});

-- | The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
cpName :: Lens' CommandPlugin (Maybe Text)
cpName = lens _cpName (\ s a -> s{_cpName = a});

-- | The name of the region where the output is stored in Amazon S3.
cpOutputS3Region :: Lens' CommandPlugin (Maybe Text)
cpOutputS3Region = lens _cpOutputS3Region (\ s a -> s{_cpOutputS3Region = a});

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: @test_folder/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-1234567876543/awsrunShellScript@  @test_folder@ is the name of the Amazon S3 bucket; @ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix@ is the name of the S3 prefix; @i-1234567876543@ is the instance ID; @awsrunShellScript@ is the name of the plugin.
cpOutputS3BucketName :: Lens' CommandPlugin (Maybe Text)
cpOutputS3BucketName = lens _cpOutputS3BucketName (\ s a -> s{_cpOutputS3BucketName = a});

-- | The time the plugin stopped executing. Could stop prematurely if, for example, a cancel command was sent.
cpResponseFinishDateTime :: Lens' CommandPlugin (Maybe UTCTime)
cpResponseFinishDateTime = lens _cpResponseFinishDateTime (\ s a -> s{_cpResponseFinishDateTime = a}) . mapping _Time;

instance FromJSON CommandPlugin where
        parseJSON
          = withObject "CommandPlugin"
              (\ x ->
                 CommandPlugin' <$>
                   (x .:? "Status") <*> (x .:? "ResponseStartDateTime")
                     <*> (x .:? "OutputS3KeyPrefix")
                     <*> (x .:? "StandardErrorUrl")
                     <*> (x .:? "ResponseCode")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "Output")
                     <*> (x .:? "StandardOutputUrl")
                     <*> (x .:? "Name")
                     <*> (x .:? "OutputS3Region")
                     <*> (x .:? "OutputS3BucketName")
                     <*> (x .:? "ResponseFinishDateTime"))

instance Hashable CommandPlugin

instance NFData CommandPlugin

-- | Describes the association of an SSM document and an instance.
--
--
--
-- /See:/ 'createAssociationBatchRequestEntry' smart constructor.
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
    { _cabreInstanceId         :: !(Maybe Text)
    , _cabreScheduleExpression :: !(Maybe Text)
    , _cabreOutputLocation     :: !(Maybe InstanceAssociationOutputLocation)
    , _cabreTargets            :: !(Maybe [Target])
    , _cabreParameters         :: !(Maybe (Map Text [Text]))
    , _cabreDocumentVersion    :: !(Maybe Text)
    , _cabreName               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAssociationBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cabreInstanceId' - The ID of the instance.
--
-- * 'cabreScheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- * 'cabreOutputLocation' - An Amazon S3 bucket where you want to store the results of this request.
--
-- * 'cabreTargets' - The instances targeted by the request.
--
-- * 'cabreParameters' - A description of the parameters for a document.
--
-- * 'cabreDocumentVersion' - The document version.
--
-- * 'cabreName' - The name of the configuration document.
createAssociationBatchRequestEntry
    :: Text -- ^ 'cabreName'
    -> CreateAssociationBatchRequestEntry
createAssociationBatchRequestEntry pName_ =
    CreateAssociationBatchRequestEntry'
    { _cabreInstanceId = Nothing
    , _cabreScheduleExpression = Nothing
    , _cabreOutputLocation = Nothing
    , _cabreTargets = Nothing
    , _cabreParameters = Nothing
    , _cabreDocumentVersion = Nothing
    , _cabreName = pName_
    }

-- | The ID of the instance.
cabreInstanceId :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreInstanceId = lens _cabreInstanceId (\ s a -> s{_cabreInstanceId = a});

-- | A cron expression that specifies a schedule when the association runs.
cabreScheduleExpression :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreScheduleExpression = lens _cabreScheduleExpression (\ s a -> s{_cabreScheduleExpression = a});

-- | An Amazon S3 bucket where you want to store the results of this request.
cabreOutputLocation :: Lens' CreateAssociationBatchRequestEntry (Maybe InstanceAssociationOutputLocation)
cabreOutputLocation = lens _cabreOutputLocation (\ s a -> s{_cabreOutputLocation = a});

-- | The instances targeted by the request.
cabreTargets :: Lens' CreateAssociationBatchRequestEntry [Target]
cabreTargets = lens _cabreTargets (\ s a -> s{_cabreTargets = a}) . _Default . _Coerce;

-- | A description of the parameters for a document.
cabreParameters :: Lens' CreateAssociationBatchRequestEntry (HashMap Text [Text])
cabreParameters = lens _cabreParameters (\ s a -> s{_cabreParameters = a}) . _Default . _Map;

-- | The document version.
cabreDocumentVersion :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreDocumentVersion = lens _cabreDocumentVersion (\ s a -> s{_cabreDocumentVersion = a});

-- | The name of the configuration document.
cabreName :: Lens' CreateAssociationBatchRequestEntry Text
cabreName = lens _cabreName (\ s a -> s{_cabreName = a});

instance FromJSON CreateAssociationBatchRequestEntry
         where
        parseJSON
          = withObject "CreateAssociationBatchRequestEntry"
              (\ x ->
                 CreateAssociationBatchRequestEntry' <$>
                   (x .:? "InstanceId") <*> (x .:? "ScheduleExpression")
                     <*> (x .:? "OutputLocation")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .: "Name"))

instance Hashable CreateAssociationBatchRequestEntry

instance NFData CreateAssociationBatchRequestEntry

instance ToJSON CreateAssociationBatchRequestEntry
         where
        toJSON CreateAssociationBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _cabreInstanceId,
                  ("ScheduleExpression" .=) <$>
                    _cabreScheduleExpression,
                  ("OutputLocation" .=) <$> _cabreOutputLocation,
                  ("Targets" .=) <$> _cabreTargets,
                  ("Parameters" .=) <$> _cabreParameters,
                  ("DocumentVersion" .=) <$> _cabreDocumentVersion,
                  Just ("Name" .= _cabreName)])

-- | Filter for the DescribeActivation API.
--
--
--
-- /See:/ 'describeActivationsFilter' smart constructor.
data DescribeActivationsFilter = DescribeActivationsFilter'
    { _dafFilterKey    :: !(Maybe DescribeActivationsFilterKeys)
    , _dafFilterValues :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeActivationsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dafFilterKey' - The name of the filter.
--
-- * 'dafFilterValues' - The filter values.
describeActivationsFilter
    :: DescribeActivationsFilter
describeActivationsFilter =
    DescribeActivationsFilter'
    { _dafFilterKey = Nothing
    , _dafFilterValues = Nothing
    }

-- | The name of the filter.
dafFilterKey :: Lens' DescribeActivationsFilter (Maybe DescribeActivationsFilterKeys)
dafFilterKey = lens _dafFilterKey (\ s a -> s{_dafFilterKey = a});

-- | The filter values.
dafFilterValues :: Lens' DescribeActivationsFilter [Text]
dafFilterValues = lens _dafFilterValues (\ s a -> s{_dafFilterValues = a}) . _Default . _Coerce;

instance Hashable DescribeActivationsFilter

instance NFData DescribeActivationsFilter

instance ToJSON DescribeActivationsFilter where
        toJSON DescribeActivationsFilter'{..}
          = object
              (catMaybes
                 [("FilterKey" .=) <$> _dafFilterKey,
                  ("FilterValues" .=) <$> _dafFilterValues])

-- | A default version of a document.
--
--
--
-- /See:/ 'documentDefaultVersionDescription' smart constructor.
data DocumentDefaultVersionDescription = DocumentDefaultVersionDescription'
    { _ddvdDefaultVersion :: !(Maybe Text)
    , _ddvdName           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentDefaultVersionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddvdDefaultVersion' - The default version of the document.
--
-- * 'ddvdName' - The name of the document.
documentDefaultVersionDescription
    :: DocumentDefaultVersionDescription
documentDefaultVersionDescription =
    DocumentDefaultVersionDescription'
    { _ddvdDefaultVersion = Nothing
    , _ddvdName = Nothing
    }

-- | The default version of the document.
ddvdDefaultVersion :: Lens' DocumentDefaultVersionDescription (Maybe Text)
ddvdDefaultVersion = lens _ddvdDefaultVersion (\ s a -> s{_ddvdDefaultVersion = a});

-- | The name of the document.
ddvdName :: Lens' DocumentDefaultVersionDescription (Maybe Text)
ddvdName = lens _ddvdName (\ s a -> s{_ddvdName = a});

instance FromJSON DocumentDefaultVersionDescription
         where
        parseJSON
          = withObject "DocumentDefaultVersionDescription"
              (\ x ->
                 DocumentDefaultVersionDescription' <$>
                   (x .:? "DefaultVersion") <*> (x .:? "Name"))

instance Hashable DocumentDefaultVersionDescription

instance NFData DocumentDefaultVersionDescription

-- | Describes an SSM document.
--
--
--
-- /See:/ 'documentDescription' smart constructor.
data DocumentDescription = DocumentDescription'
    { _dStatus          :: !(Maybe DocumentStatus)
    , _dDocumentType    :: !(Maybe DocumentType)
    , _dHash            :: !(Maybe Text)
    , _dSchemaVersion   :: !(Maybe Text)
    , _dSha1            :: !(Maybe Text)
    , _dDefaultVersion  :: !(Maybe Text)
    , _dOwner           :: !(Maybe Text)
    , _dPlatformTypes   :: !(Maybe [PlatformType])
    , _dCreatedDate     :: !(Maybe POSIX)
    , _dName            :: !(Maybe Text)
    , _dHashType        :: !(Maybe DocumentHashType)
    , _dParameters      :: !(Maybe [DocumentParameter])
    , _dDocumentVersion :: !(Maybe Text)
    , _dDescription     :: !(Maybe Text)
    , _dLatestVersion   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStatus' - The status of the SSM document.
--
-- * 'dDocumentType' - The type of document.
--
-- * 'dHash' - The Sha256 or Sha1 hash created by the system when the document was created.
--
-- * 'dSchemaVersion' - The schema version.
--
-- * 'dSha1' - The SHA1 hash of the document, which you can use for verification purposes.
--
-- * 'dDefaultVersion' - The default version.
--
-- * 'dOwner' - The AWS user account of the person who created the document.
--
-- * 'dPlatformTypes' - The list of OS platforms compatible with this SSM document.
--
-- * 'dCreatedDate' - The date when the SSM document was created.
--
-- * 'dName' - The name of the SSM document.
--
-- * 'dHashType' - Sha256 or Sha1.
--
-- * 'dParameters' - A description of the parameters for a document.
--
-- * 'dDocumentVersion' - The document version.
--
-- * 'dDescription' - A description of the document.
--
-- * 'dLatestVersion' - The latest version of the document.
documentDescription
    :: DocumentDescription
documentDescription =
    DocumentDescription'
    { _dStatus = Nothing
    , _dDocumentType = Nothing
    , _dHash = Nothing
    , _dSchemaVersion = Nothing
    , _dSha1 = Nothing
    , _dDefaultVersion = Nothing
    , _dOwner = Nothing
    , _dPlatformTypes = Nothing
    , _dCreatedDate = Nothing
    , _dName = Nothing
    , _dHashType = Nothing
    , _dParameters = Nothing
    , _dDocumentVersion = Nothing
    , _dDescription = Nothing
    , _dLatestVersion = Nothing
    }

-- | The status of the SSM document.
dStatus :: Lens' DocumentDescription (Maybe DocumentStatus)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | The type of document.
dDocumentType :: Lens' DocumentDescription (Maybe DocumentType)
dDocumentType = lens _dDocumentType (\ s a -> s{_dDocumentType = a});

-- | The Sha256 or Sha1 hash created by the system when the document was created.
dHash :: Lens' DocumentDescription (Maybe Text)
dHash = lens _dHash (\ s a -> s{_dHash = a});

-- | The schema version.
dSchemaVersion :: Lens' DocumentDescription (Maybe Text)
dSchemaVersion = lens _dSchemaVersion (\ s a -> s{_dSchemaVersion = a});

-- | The SHA1 hash of the document, which you can use for verification purposes.
dSha1 :: Lens' DocumentDescription (Maybe Text)
dSha1 = lens _dSha1 (\ s a -> s{_dSha1 = a});

-- | The default version.
dDefaultVersion :: Lens' DocumentDescription (Maybe Text)
dDefaultVersion = lens _dDefaultVersion (\ s a -> s{_dDefaultVersion = a});

-- | The AWS user account of the person who created the document.
dOwner :: Lens' DocumentDescription (Maybe Text)
dOwner = lens _dOwner (\ s a -> s{_dOwner = a});

-- | The list of OS platforms compatible with this SSM document.
dPlatformTypes :: Lens' DocumentDescription [PlatformType]
dPlatformTypes = lens _dPlatformTypes (\ s a -> s{_dPlatformTypes = a}) . _Default . _Coerce;

-- | The date when the SSM document was created.
dCreatedDate :: Lens' DocumentDescription (Maybe UTCTime)
dCreatedDate = lens _dCreatedDate (\ s a -> s{_dCreatedDate = a}) . mapping _Time;

-- | The name of the SSM document.
dName :: Lens' DocumentDescription (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a});

-- | Sha256 or Sha1.
dHashType :: Lens' DocumentDescription (Maybe DocumentHashType)
dHashType = lens _dHashType (\ s a -> s{_dHashType = a});

-- | A description of the parameters for a document.
dParameters :: Lens' DocumentDescription [DocumentParameter]
dParameters = lens _dParameters (\ s a -> s{_dParameters = a}) . _Default . _Coerce;

-- | The document version.
dDocumentVersion :: Lens' DocumentDescription (Maybe Text)
dDocumentVersion = lens _dDocumentVersion (\ s a -> s{_dDocumentVersion = a});

-- | A description of the document.
dDescription :: Lens' DocumentDescription (Maybe Text)
dDescription = lens _dDescription (\ s a -> s{_dDescription = a});

-- | The latest version of the document.
dLatestVersion :: Lens' DocumentDescription (Maybe Text)
dLatestVersion = lens _dLatestVersion (\ s a -> s{_dLatestVersion = a});

instance FromJSON DocumentDescription where
        parseJSON
          = withObject "DocumentDescription"
              (\ x ->
                 DocumentDescription' <$>
                   (x .:? "Status") <*> (x .:? "DocumentType") <*>
                     (x .:? "Hash")
                     <*> (x .:? "SchemaVersion")
                     <*> (x .:? "Sha1")
                     <*> (x .:? "DefaultVersion")
                     <*> (x .:? "Owner")
                     <*> (x .:? "PlatformTypes" .!= mempty)
                     <*> (x .:? "CreatedDate")
                     <*> (x .:? "Name")
                     <*> (x .:? "HashType")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "Description")
                     <*> (x .:? "LatestVersion"))

instance Hashable DocumentDescription

instance NFData DocumentDescription

-- | Describes a filter.
--
--
--
-- /See:/ 'documentFilter' smart constructor.
data DocumentFilter = DocumentFilter'
    { _dfKey   :: !DocumentFilterKey
    , _dfValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfKey' - The name of the filter.
--
-- * 'dfValue' - The value of the filter.
documentFilter
    :: DocumentFilterKey -- ^ 'dfKey'
    -> Text -- ^ 'dfValue'
    -> DocumentFilter
documentFilter pKey_ pValue_ =
    DocumentFilter'
    { _dfKey = pKey_
    , _dfValue = pValue_
    }

-- | The name of the filter.
dfKey :: Lens' DocumentFilter DocumentFilterKey
dfKey = lens _dfKey (\ s a -> s{_dfKey = a});

-- | The value of the filter.
dfValue :: Lens' DocumentFilter Text
dfValue = lens _dfValue (\ s a -> s{_dfValue = a});

instance Hashable DocumentFilter

instance NFData DocumentFilter

instance ToJSON DocumentFilter where
        toJSON DocumentFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _dfKey), Just ("value" .= _dfValue)])

-- | Describes the name of an SSM document.
--
--
--
-- /See:/ 'documentIdentifier' smart constructor.
data DocumentIdentifier = DocumentIdentifier'
    { _diDocumentType    :: !(Maybe DocumentType)
    , _diSchemaVersion   :: !(Maybe Text)
    , _diOwner           :: !(Maybe Text)
    , _diPlatformTypes   :: !(Maybe [PlatformType])
    , _diName            :: !(Maybe Text)
    , _diDocumentVersion :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diDocumentType' - The document type.
--
-- * 'diSchemaVersion' - The schema version.
--
-- * 'diOwner' - The AWS user account of the person who created the document.
--
-- * 'diPlatformTypes' - The operating system platform.
--
-- * 'diName' - The name of the SSM document.
--
-- * 'diDocumentVersion' - The document version.
documentIdentifier
    :: DocumentIdentifier
documentIdentifier =
    DocumentIdentifier'
    { _diDocumentType = Nothing
    , _diSchemaVersion = Nothing
    , _diOwner = Nothing
    , _diPlatformTypes = Nothing
    , _diName = Nothing
    , _diDocumentVersion = Nothing
    }

-- | The document type.
diDocumentType :: Lens' DocumentIdentifier (Maybe DocumentType)
diDocumentType = lens _diDocumentType (\ s a -> s{_diDocumentType = a});

-- | The schema version.
diSchemaVersion :: Lens' DocumentIdentifier (Maybe Text)
diSchemaVersion = lens _diSchemaVersion (\ s a -> s{_diSchemaVersion = a});

-- | The AWS user account of the person who created the document.
diOwner :: Lens' DocumentIdentifier (Maybe Text)
diOwner = lens _diOwner (\ s a -> s{_diOwner = a});

-- | The operating system platform.
diPlatformTypes :: Lens' DocumentIdentifier [PlatformType]
diPlatformTypes = lens _diPlatformTypes (\ s a -> s{_diPlatformTypes = a}) . _Default . _Coerce;

-- | The name of the SSM document.
diName :: Lens' DocumentIdentifier (Maybe Text)
diName = lens _diName (\ s a -> s{_diName = a});

-- | The document version.
diDocumentVersion :: Lens' DocumentIdentifier (Maybe Text)
diDocumentVersion = lens _diDocumentVersion (\ s a -> s{_diDocumentVersion = a});

instance FromJSON DocumentIdentifier where
        parseJSON
          = withObject "DocumentIdentifier"
              (\ x ->
                 DocumentIdentifier' <$>
                   (x .:? "DocumentType") <*> (x .:? "SchemaVersion")
                     <*> (x .:? "Owner")
                     <*> (x .:? "PlatformTypes" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "DocumentVersion"))

instance Hashable DocumentIdentifier

instance NFData DocumentIdentifier

-- | Parameters specified in the SSM document that execute on the server when the command is run.
--
--
--
-- /See:/ 'documentParameter' smart constructor.
data DocumentParameter = DocumentParameter'
    { _dpName         :: !(Maybe Text)
    , _dpDefaultValue :: !(Maybe Text)
    , _dpType         :: !(Maybe DocumentParameterType)
    , _dpDescription  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpName' - The name of the parameter.
--
-- * 'dpDefaultValue' - If specified, the default values for the parameters. Parameters without a default value are required. Parameters with a default value are optional.
--
-- * 'dpType' - The type of parameter. The type can be either “String” or “StringList”.
--
-- * 'dpDescription' - A description of what the parameter does, how to use it, the default value, and whether or not the parameter is optional.
documentParameter
    :: DocumentParameter
documentParameter =
    DocumentParameter'
    { _dpName = Nothing
    , _dpDefaultValue = Nothing
    , _dpType = Nothing
    , _dpDescription = Nothing
    }

-- | The name of the parameter.
dpName :: Lens' DocumentParameter (Maybe Text)
dpName = lens _dpName (\ s a -> s{_dpName = a});

-- | If specified, the default values for the parameters. Parameters without a default value are required. Parameters with a default value are optional.
dpDefaultValue :: Lens' DocumentParameter (Maybe Text)
dpDefaultValue = lens _dpDefaultValue (\ s a -> s{_dpDefaultValue = a});

-- | The type of parameter. The type can be either “String” or “StringList”.
dpType :: Lens' DocumentParameter (Maybe DocumentParameterType)
dpType = lens _dpType (\ s a -> s{_dpType = a});

-- | A description of what the parameter does, how to use it, the default value, and whether or not the parameter is optional.
dpDescription :: Lens' DocumentParameter (Maybe Text)
dpDescription = lens _dpDescription (\ s a -> s{_dpDescription = a});

instance FromJSON DocumentParameter where
        parseJSON
          = withObject "DocumentParameter"
              (\ x ->
                 DocumentParameter' <$>
                   (x .:? "Name") <*> (x .:? "DefaultValue") <*>
                     (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable DocumentParameter

instance NFData DocumentParameter

-- | Version information about the document.
--
--
--
-- /See:/ 'documentVersionInfo' smart constructor.
data DocumentVersionInfo = DocumentVersionInfo'
    { _dviCreatedDate      :: !(Maybe POSIX)
    , _dviName             :: !(Maybe Text)
    , _dviDocumentVersion  :: !(Maybe Text)
    , _dviIsDefaultVersion :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentVersionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dviCreatedDate' - The date the document was created.
--
-- * 'dviName' - The document name.
--
-- * 'dviDocumentVersion' - The document version.
--
-- * 'dviIsDefaultVersion' - An identifier for the default version of the document.
documentVersionInfo
    :: DocumentVersionInfo
documentVersionInfo =
    DocumentVersionInfo'
    { _dviCreatedDate = Nothing
    , _dviName = Nothing
    , _dviDocumentVersion = Nothing
    , _dviIsDefaultVersion = Nothing
    }

-- | The date the document was created.
dviCreatedDate :: Lens' DocumentVersionInfo (Maybe UTCTime)
dviCreatedDate = lens _dviCreatedDate (\ s a -> s{_dviCreatedDate = a}) . mapping _Time;

-- | The document name.
dviName :: Lens' DocumentVersionInfo (Maybe Text)
dviName = lens _dviName (\ s a -> s{_dviName = a});

-- | The document version.
dviDocumentVersion :: Lens' DocumentVersionInfo (Maybe Text)
dviDocumentVersion = lens _dviDocumentVersion (\ s a -> s{_dviDocumentVersion = a});

-- | An identifier for the default version of the document.
dviIsDefaultVersion :: Lens' DocumentVersionInfo (Maybe Bool)
dviIsDefaultVersion = lens _dviIsDefaultVersion (\ s a -> s{_dviIsDefaultVersion = a});

instance FromJSON DocumentVersionInfo where
        parseJSON
          = withObject "DocumentVersionInfo"
              (\ x ->
                 DocumentVersionInfo' <$>
                   (x .:? "CreatedDate") <*> (x .:? "Name") <*>
                     (x .:? "DocumentVersion")
                     <*> (x .:? "IsDefaultVersion"))

instance Hashable DocumentVersionInfo

instance NFData DocumentVersionInfo

-- | Describes a failed association.
--
--
--
-- /See:/ 'failedCreateAssociation' smart constructor.
data FailedCreateAssociation = FailedCreateAssociation'
    { _fcaEntry   :: !(Maybe CreateAssociationBatchRequestEntry)
    , _fcaFault   :: !(Maybe Fault)
    , _fcaMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FailedCreateAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcaEntry' - The association.
--
-- * 'fcaFault' - The source of the failure.
--
-- * 'fcaMessage' - A description of the failure.
failedCreateAssociation
    :: FailedCreateAssociation
failedCreateAssociation =
    FailedCreateAssociation'
    { _fcaEntry = Nothing
    , _fcaFault = Nothing
    , _fcaMessage = Nothing
    }

-- | The association.
fcaEntry :: Lens' FailedCreateAssociation (Maybe CreateAssociationBatchRequestEntry)
fcaEntry = lens _fcaEntry (\ s a -> s{_fcaEntry = a});

-- | The source of the failure.
fcaFault :: Lens' FailedCreateAssociation (Maybe Fault)
fcaFault = lens _fcaFault (\ s a -> s{_fcaFault = a});

-- | A description of the failure.
fcaMessage :: Lens' FailedCreateAssociation (Maybe Text)
fcaMessage = lens _fcaMessage (\ s a -> s{_fcaMessage = a});

instance FromJSON FailedCreateAssociation where
        parseJSON
          = withObject "FailedCreateAssociation"
              (\ x ->
                 FailedCreateAssociation' <$>
                   (x .:? "Entry") <*> (x .:? "Fault") <*>
                     (x .:? "Message"))

instance Hashable FailedCreateAssociation

instance NFData FailedCreateAssociation

-- | Status information about the aggregated associations.
--
--
--
-- /See:/ 'instanceAggregatedAssociationOverview' smart constructor.
data InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview'
    { _iaaoDetailedStatus                           :: !(Maybe Text)
    , _iaaoInstanceAssociationStatusAggregatedCount :: !(Maybe (Map Text Int))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceAggregatedAssociationOverview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaaoDetailedStatus' - Detailed status information about the aggregated associations.
--
-- * 'iaaoInstanceAssociationStatusAggregatedCount' - The number of associations for the instance(s).
instanceAggregatedAssociationOverview
    :: InstanceAggregatedAssociationOverview
instanceAggregatedAssociationOverview =
    InstanceAggregatedAssociationOverview'
    { _iaaoDetailedStatus = Nothing
    , _iaaoInstanceAssociationStatusAggregatedCount = Nothing
    }

-- | Detailed status information about the aggregated associations.
iaaoDetailedStatus :: Lens' InstanceAggregatedAssociationOverview (Maybe Text)
iaaoDetailedStatus = lens _iaaoDetailedStatus (\ s a -> s{_iaaoDetailedStatus = a});

-- | The number of associations for the instance(s).
iaaoInstanceAssociationStatusAggregatedCount :: Lens' InstanceAggregatedAssociationOverview (HashMap Text Int)
iaaoInstanceAssociationStatusAggregatedCount = lens _iaaoInstanceAssociationStatusAggregatedCount (\ s a -> s{_iaaoInstanceAssociationStatusAggregatedCount = a}) . _Default . _Map;

instance FromJSON
         InstanceAggregatedAssociationOverview where
        parseJSON
          = withObject "InstanceAggregatedAssociationOverview"
              (\ x ->
                 InstanceAggregatedAssociationOverview' <$>
                   (x .:? "DetailedStatus") <*>
                     (x .:? "InstanceAssociationStatusAggregatedCount" .!=
                        mempty))

instance Hashable
         InstanceAggregatedAssociationOverview

instance NFData InstanceAggregatedAssociationOverview

-- | One or more association documents on the instance.
--
--
--
-- /See:/ 'instanceAssociation' smart constructor.
data InstanceAssociation = InstanceAssociation'
    { _iaAssociationId :: !(Maybe Text)
    , _iaInstanceId    :: !(Maybe Text)
    , _iaContent       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaAssociationId' - The association ID.
--
-- * 'iaInstanceId' - The instance ID.
--
-- * 'iaContent' - The content of the association document for the instance(s).
instanceAssociation
    :: InstanceAssociation
instanceAssociation =
    InstanceAssociation'
    { _iaAssociationId = Nothing
    , _iaInstanceId = Nothing
    , _iaContent = Nothing
    }

-- | The association ID.
iaAssociationId :: Lens' InstanceAssociation (Maybe Text)
iaAssociationId = lens _iaAssociationId (\ s a -> s{_iaAssociationId = a});

-- | The instance ID.
iaInstanceId :: Lens' InstanceAssociation (Maybe Text)
iaInstanceId = lens _iaInstanceId (\ s a -> s{_iaInstanceId = a});

-- | The content of the association document for the instance(s).
iaContent :: Lens' InstanceAssociation (Maybe Text)
iaContent = lens _iaContent (\ s a -> s{_iaContent = a});

instance FromJSON InstanceAssociation where
        parseJSON
          = withObject "InstanceAssociation"
              (\ x ->
                 InstanceAssociation' <$>
                   (x .:? "AssociationId") <*> (x .:? "InstanceId") <*>
                     (x .:? "Content"))

instance Hashable InstanceAssociation

instance NFData InstanceAssociation

-- | An Amazon S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 'instanceAssociationOutputLocation' smart constructor.
newtype InstanceAssociationOutputLocation = InstanceAssociationOutputLocation'
    { _iaolS3Location :: Maybe S3OutputLocation
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceAssociationOutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaolS3Location' - An Amazon S3 bucket where you want to store the results of this request.
instanceAssociationOutputLocation
    :: InstanceAssociationOutputLocation
instanceAssociationOutputLocation =
    InstanceAssociationOutputLocation'
    { _iaolS3Location = Nothing
    }

-- | An Amazon S3 bucket where you want to store the results of this request.
iaolS3Location :: Lens' InstanceAssociationOutputLocation (Maybe S3OutputLocation)
iaolS3Location = lens _iaolS3Location (\ s a -> s{_iaolS3Location = a});

instance FromJSON InstanceAssociationOutputLocation
         where
        parseJSON
          = withObject "InstanceAssociationOutputLocation"
              (\ x ->
                 InstanceAssociationOutputLocation' <$>
                   (x .:? "S3Location"))

instance Hashable InstanceAssociationOutputLocation

instance NFData InstanceAssociationOutputLocation

instance ToJSON InstanceAssociationOutputLocation
         where
        toJSON InstanceAssociationOutputLocation'{..}
          = object
              (catMaybes [("S3Location" .=) <$> _iaolS3Location])

-- | The URL of Amazon S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 'instanceAssociationOutputURL' smart constructor.
newtype InstanceAssociationOutputURL = InstanceAssociationOutputURL'
    { _iaouS3OutputURL :: Maybe S3OutputURL
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceAssociationOutputURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaouS3OutputURL' - The URL of Amazon S3 bucket where you want to store the results of this request.
instanceAssociationOutputURL
    :: InstanceAssociationOutputURL
instanceAssociationOutputURL =
    InstanceAssociationOutputURL'
    { _iaouS3OutputURL = Nothing
    }

-- | The URL of Amazon S3 bucket where you want to store the results of this request.
iaouS3OutputURL :: Lens' InstanceAssociationOutputURL (Maybe S3OutputURL)
iaouS3OutputURL = lens _iaouS3OutputURL (\ s a -> s{_iaouS3OutputURL = a});

instance FromJSON InstanceAssociationOutputURL where
        parseJSON
          = withObject "InstanceAssociationOutputURL"
              (\ x ->
                 InstanceAssociationOutputURL' <$>
                   (x .:? "S3OutputUrl"))

instance Hashable InstanceAssociationOutputURL

instance NFData InstanceAssociationOutputURL

-- | Status information about the instance association.
--
--
--
-- /See:/ 'instanceAssociationStatusInfo' smart constructor.
data InstanceAssociationStatusInfo = InstanceAssociationStatusInfo'
    { _iasiAssociationId    :: !(Maybe Text)
    , _iasiInstanceId       :: !(Maybe Text)
    , _iasiDetailedStatus   :: !(Maybe Text)
    , _iasiStatus           :: !(Maybe Text)
    , _iasiOutputURL        :: !(Maybe InstanceAssociationOutputURL)
    , _iasiExecutionSummary :: !(Maybe Text)
    , _iasiName             :: !(Maybe Text)
    , _iasiErrorCode        :: !(Maybe Text)
    , _iasiDocumentVersion  :: !(Maybe Text)
    , _iasiExecutionDate    :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceAssociationStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iasiAssociationId' - The association ID.
--
-- * 'iasiInstanceId' - The instance ID where the association was created.
--
-- * 'iasiDetailedStatus' - Detailed status information about the instance association.
--
-- * 'iasiStatus' - Status information about the instance association.
--
-- * 'iasiOutputURL' - A URL for an Amazon S3 bucket where you want to store the results of this request.
--
-- * 'iasiExecutionSummary' - Summary information about association execution.
--
-- * 'iasiName' - The name of the association.
--
-- * 'iasiErrorCode' - An error code returned by the request to create the association.
--
-- * 'iasiDocumentVersion' - The association document verions.
--
-- * 'iasiExecutionDate' - The date the instance association executed.
instanceAssociationStatusInfo
    :: InstanceAssociationStatusInfo
instanceAssociationStatusInfo =
    InstanceAssociationStatusInfo'
    { _iasiAssociationId = Nothing
    , _iasiInstanceId = Nothing
    , _iasiDetailedStatus = Nothing
    , _iasiStatus = Nothing
    , _iasiOutputURL = Nothing
    , _iasiExecutionSummary = Nothing
    , _iasiName = Nothing
    , _iasiErrorCode = Nothing
    , _iasiDocumentVersion = Nothing
    , _iasiExecutionDate = Nothing
    }

-- | The association ID.
iasiAssociationId :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiAssociationId = lens _iasiAssociationId (\ s a -> s{_iasiAssociationId = a});

-- | The instance ID where the association was created.
iasiInstanceId :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiInstanceId = lens _iasiInstanceId (\ s a -> s{_iasiInstanceId = a});

-- | Detailed status information about the instance association.
iasiDetailedStatus :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiDetailedStatus = lens _iasiDetailedStatus (\ s a -> s{_iasiDetailedStatus = a});

-- | Status information about the instance association.
iasiStatus :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiStatus = lens _iasiStatus (\ s a -> s{_iasiStatus = a});

-- | A URL for an Amazon S3 bucket where you want to store the results of this request.
iasiOutputURL :: Lens' InstanceAssociationStatusInfo (Maybe InstanceAssociationOutputURL)
iasiOutputURL = lens _iasiOutputURL (\ s a -> s{_iasiOutputURL = a});

-- | Summary information about association execution.
iasiExecutionSummary :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiExecutionSummary = lens _iasiExecutionSummary (\ s a -> s{_iasiExecutionSummary = a});

-- | The name of the association.
iasiName :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiName = lens _iasiName (\ s a -> s{_iasiName = a});

-- | An error code returned by the request to create the association.
iasiErrorCode :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiErrorCode = lens _iasiErrorCode (\ s a -> s{_iasiErrorCode = a});

-- | The association document verions.
iasiDocumentVersion :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiDocumentVersion = lens _iasiDocumentVersion (\ s a -> s{_iasiDocumentVersion = a});

-- | The date the instance association executed.
iasiExecutionDate :: Lens' InstanceAssociationStatusInfo (Maybe UTCTime)
iasiExecutionDate = lens _iasiExecutionDate (\ s a -> s{_iasiExecutionDate = a}) . mapping _Time;

instance FromJSON InstanceAssociationStatusInfo where
        parseJSON
          = withObject "InstanceAssociationStatusInfo"
              (\ x ->
                 InstanceAssociationStatusInfo' <$>
                   (x .:? "AssociationId") <*> (x .:? "InstanceId") <*>
                     (x .:? "DetailedStatus")
                     <*> (x .:? "Status")
                     <*> (x .:? "OutputUrl")
                     <*> (x .:? "ExecutionSummary")
                     <*> (x .:? "Name")
                     <*> (x .:? "ErrorCode")
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "ExecutionDate"))

instance Hashable InstanceAssociationStatusInfo

instance NFData InstanceAssociationStatusInfo

-- | Describes a filter for a specific list of instances.
--
--
--
-- /See:/ 'instanceInformation' smart constructor.
data InstanceInformation = InstanceInformation'
    { _iiInstanceId                             :: !(Maybe Text)
    , _iiPingStatus                             :: !(Maybe PingStatus)
    , _iiIPAddress                              :: !(Maybe Text)
    , _iiResourceType                           :: !(Maybe ResourceType)
    , _iiRegistrationDate                       :: !(Maybe POSIX)
    , _iiPlatformVersion                        :: !(Maybe Text)
    , _iiIsLatestVersion                        :: !(Maybe Bool)
    , _iiAgentVersion                           :: !(Maybe Text)
    , _iiLastPingDateTime                       :: !(Maybe POSIX)
    , _iiLastSuccessfulAssociationExecutionDate :: !(Maybe POSIX)
    , _iiActivationId                           :: !(Maybe Text)
    , _iiName                                   :: !(Maybe Text)
    , _iiPlatformType                           :: !(Maybe PlatformType)
    , _iiAssociationOverview                    :: !(Maybe InstanceAggregatedAssociationOverview)
    , _iiAssociationStatus                      :: !(Maybe Text)
    , _iiLastAssociationExecutionDate           :: !(Maybe POSIX)
    , _iiPlatformName                           :: !(Maybe Text)
    , _iiComputerName                           :: !(Maybe Text)
    , _iiIAMRole                                :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiInstanceId' - The instance ID.
--
-- * 'iiPingStatus' - Connection status of the SSM agent.
--
-- * 'iiIPAddress' - The IP address of the managed instance.
--
-- * 'iiResourceType' - The type of instance. Instances are either EC2 instances or managed instances.
--
-- * 'iiRegistrationDate' - The date the server or VM was registered with AWS as a managed instance.
--
-- * 'iiPlatformVersion' - The version of the OS platform running on your instance.
--
-- * 'iiIsLatestVersion' - Indicates whether latest version of the SSM agent is running on your instance.
--
-- * 'iiAgentVersion' - The version of the SSM agent running on your Linux instance.
--
-- * 'iiLastPingDateTime' - The date and time when agent last pinged Systems Manager service.
--
-- * 'iiLastSuccessfulAssociationExecutionDate' - The last date the association was successfully run.
--
-- * 'iiActivationId' - The activation ID created by Systems Manager when the server or VM was registered.
--
-- * 'iiName' - The name of the managed instance.
--
-- * 'iiPlatformType' - The operating system platform type.
--
-- * 'iiAssociationOverview' - Information about the association.
--
-- * 'iiAssociationStatus' - The status of the association.
--
-- * 'iiLastAssociationExecutionDate' - The date the association was last executed.
--
-- * 'iiPlatformName' - The name of the operating system platform running on your instance.
--
-- * 'iiComputerName' - The fully qualified host name of the managed instance.
--
-- * 'iiIAMRole' - The Amazon Identity and Access Management (IAM) role assigned to EC2 instances or managed instances.
instanceInformation
    :: InstanceInformation
instanceInformation =
    InstanceInformation'
    { _iiInstanceId = Nothing
    , _iiPingStatus = Nothing
    , _iiIPAddress = Nothing
    , _iiResourceType = Nothing
    , _iiRegistrationDate = Nothing
    , _iiPlatformVersion = Nothing
    , _iiIsLatestVersion = Nothing
    , _iiAgentVersion = Nothing
    , _iiLastPingDateTime = Nothing
    , _iiLastSuccessfulAssociationExecutionDate = Nothing
    , _iiActivationId = Nothing
    , _iiName = Nothing
    , _iiPlatformType = Nothing
    , _iiAssociationOverview = Nothing
    , _iiAssociationStatus = Nothing
    , _iiLastAssociationExecutionDate = Nothing
    , _iiPlatformName = Nothing
    , _iiComputerName = Nothing
    , _iiIAMRole = Nothing
    }

-- | The instance ID.
iiInstanceId :: Lens' InstanceInformation (Maybe Text)
iiInstanceId = lens _iiInstanceId (\ s a -> s{_iiInstanceId = a});

-- | Connection status of the SSM agent.
iiPingStatus :: Lens' InstanceInformation (Maybe PingStatus)
iiPingStatus = lens _iiPingStatus (\ s a -> s{_iiPingStatus = a});

-- | The IP address of the managed instance.
iiIPAddress :: Lens' InstanceInformation (Maybe Text)
iiIPAddress = lens _iiIPAddress (\ s a -> s{_iiIPAddress = a});

-- | The type of instance. Instances are either EC2 instances or managed instances.
iiResourceType :: Lens' InstanceInformation (Maybe ResourceType)
iiResourceType = lens _iiResourceType (\ s a -> s{_iiResourceType = a});

-- | The date the server or VM was registered with AWS as a managed instance.
iiRegistrationDate :: Lens' InstanceInformation (Maybe UTCTime)
iiRegistrationDate = lens _iiRegistrationDate (\ s a -> s{_iiRegistrationDate = a}) . mapping _Time;

-- | The version of the OS platform running on your instance.
iiPlatformVersion :: Lens' InstanceInformation (Maybe Text)
iiPlatformVersion = lens _iiPlatformVersion (\ s a -> s{_iiPlatformVersion = a});

-- | Indicates whether latest version of the SSM agent is running on your instance.
iiIsLatestVersion :: Lens' InstanceInformation (Maybe Bool)
iiIsLatestVersion = lens _iiIsLatestVersion (\ s a -> s{_iiIsLatestVersion = a});

-- | The version of the SSM agent running on your Linux instance.
iiAgentVersion :: Lens' InstanceInformation (Maybe Text)
iiAgentVersion = lens _iiAgentVersion (\ s a -> s{_iiAgentVersion = a});

-- | The date and time when agent last pinged Systems Manager service.
iiLastPingDateTime :: Lens' InstanceInformation (Maybe UTCTime)
iiLastPingDateTime = lens _iiLastPingDateTime (\ s a -> s{_iiLastPingDateTime = a}) . mapping _Time;

-- | The last date the association was successfully run.
iiLastSuccessfulAssociationExecutionDate :: Lens' InstanceInformation (Maybe UTCTime)
iiLastSuccessfulAssociationExecutionDate = lens _iiLastSuccessfulAssociationExecutionDate (\ s a -> s{_iiLastSuccessfulAssociationExecutionDate = a}) . mapping _Time;

-- | The activation ID created by Systems Manager when the server or VM was registered.
iiActivationId :: Lens' InstanceInformation (Maybe Text)
iiActivationId = lens _iiActivationId (\ s a -> s{_iiActivationId = a});

-- | The name of the managed instance.
iiName :: Lens' InstanceInformation (Maybe Text)
iiName = lens _iiName (\ s a -> s{_iiName = a});

-- | The operating system platform type.
iiPlatformType :: Lens' InstanceInformation (Maybe PlatformType)
iiPlatformType = lens _iiPlatformType (\ s a -> s{_iiPlatformType = a});

-- | Information about the association.
iiAssociationOverview :: Lens' InstanceInformation (Maybe InstanceAggregatedAssociationOverview)
iiAssociationOverview = lens _iiAssociationOverview (\ s a -> s{_iiAssociationOverview = a});

-- | The status of the association.
iiAssociationStatus :: Lens' InstanceInformation (Maybe Text)
iiAssociationStatus = lens _iiAssociationStatus (\ s a -> s{_iiAssociationStatus = a});

-- | The date the association was last executed.
iiLastAssociationExecutionDate :: Lens' InstanceInformation (Maybe UTCTime)
iiLastAssociationExecutionDate = lens _iiLastAssociationExecutionDate (\ s a -> s{_iiLastAssociationExecutionDate = a}) . mapping _Time;

-- | The name of the operating system platform running on your instance.
iiPlatformName :: Lens' InstanceInformation (Maybe Text)
iiPlatformName = lens _iiPlatformName (\ s a -> s{_iiPlatformName = a});

-- | The fully qualified host name of the managed instance.
iiComputerName :: Lens' InstanceInformation (Maybe Text)
iiComputerName = lens _iiComputerName (\ s a -> s{_iiComputerName = a});

-- | The Amazon Identity and Access Management (IAM) role assigned to EC2 instances or managed instances.
iiIAMRole :: Lens' InstanceInformation (Maybe Text)
iiIAMRole = lens _iiIAMRole (\ s a -> s{_iiIAMRole = a});

instance FromJSON InstanceInformation where
        parseJSON
          = withObject "InstanceInformation"
              (\ x ->
                 InstanceInformation' <$>
                   (x .:? "InstanceId") <*> (x .:? "PingStatus") <*>
                     (x .:? "IPAddress")
                     <*> (x .:? "ResourceType")
                     <*> (x .:? "RegistrationDate")
                     <*> (x .:? "PlatformVersion")
                     <*> (x .:? "IsLatestVersion")
                     <*> (x .:? "AgentVersion")
                     <*> (x .:? "LastPingDateTime")
                     <*> (x .:? "LastSuccessfulAssociationExecutionDate")
                     <*> (x .:? "ActivationId")
                     <*> (x .:? "Name")
                     <*> (x .:? "PlatformType")
                     <*> (x .:? "AssociationOverview")
                     <*> (x .:? "AssociationStatus")
                     <*> (x .:? "LastAssociationExecutionDate")
                     <*> (x .:? "PlatformName")
                     <*> (x .:? "ComputerName")
                     <*> (x .:? "IamRole"))

instance Hashable InstanceInformation

instance NFData InstanceInformation

-- | Describes a filter for a specific list of instances.
--
--
--
-- /See:/ 'instanceInformationFilter' smart constructor.
data InstanceInformationFilter = InstanceInformationFilter'
    { _iifKey      :: !InstanceInformationFilterKey
    , _iifValueSet :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceInformationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iifKey' - The name of the filter.
--
-- * 'iifValueSet' - The filter values.
instanceInformationFilter
    :: InstanceInformationFilterKey -- ^ 'iifKey'
    -> NonEmpty Text -- ^ 'iifValueSet'
    -> InstanceInformationFilter
instanceInformationFilter pKey_ pValueSet_ =
    InstanceInformationFilter'
    { _iifKey = pKey_
    , _iifValueSet = _List1 # pValueSet_
    }

-- | The name of the filter.
iifKey :: Lens' InstanceInformationFilter InstanceInformationFilterKey
iifKey = lens _iifKey (\ s a -> s{_iifKey = a});

-- | The filter values.
iifValueSet :: Lens' InstanceInformationFilter (NonEmpty Text)
iifValueSet = lens _iifValueSet (\ s a -> s{_iifValueSet = a}) . _List1;

instance Hashable InstanceInformationFilter

instance NFData InstanceInformationFilter

instance ToJSON InstanceInformationFilter where
        toJSON InstanceInformationFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _iifKey),
                  Just ("valueSet" .= _iifValueSet)])

-- | The filters to describe or get information about your managed instances.
--
--
--
-- /See:/ 'instanceInformationStringFilter' smart constructor.
data InstanceInformationStringFilter = InstanceInformationStringFilter'
    { _iisfKey    :: !Text
    , _iisfValues :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceInformationStringFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iisfKey' - The filter key name to describe your instances. For example: "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|”AssociationStatus”|”Tag Key”
--
-- * 'iisfValues' - The filter values.
instanceInformationStringFilter
    :: Text -- ^ 'iisfKey'
    -> NonEmpty Text -- ^ 'iisfValues'
    -> InstanceInformationStringFilter
instanceInformationStringFilter pKey_ pValues_ =
    InstanceInformationStringFilter'
    { _iisfKey = pKey_
    , _iisfValues = _List1 # pValues_
    }

-- | The filter key name to describe your instances. For example: "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|”AssociationStatus”|”Tag Key”
iisfKey :: Lens' InstanceInformationStringFilter Text
iisfKey = lens _iisfKey (\ s a -> s{_iisfKey = a});

-- | The filter values.
iisfValues :: Lens' InstanceInformationStringFilter (NonEmpty Text)
iisfValues = lens _iisfValues (\ s a -> s{_iisfValues = a}) . _List1;

instance Hashable InstanceInformationStringFilter

instance NFData InstanceInformationStringFilter

instance ToJSON InstanceInformationStringFilter where
        toJSON InstanceInformationStringFilter'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _iisfKey),
                  Just ("Values" .= _iisfValues)])

-- | One or more filters. Use a filter to return a more specific list of results.
--
--
--
-- /See:/ 'inventoryFilter' smart constructor.
data InventoryFilter = InventoryFilter'
    { _ifType   :: !(Maybe InventoryQueryOperatorType)
    , _ifKey    :: !Text
    , _ifValues :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifType' - The type of filter. Valid values include the following: "Equal"|"NotEqual"|"BeginWith"|"LessThan"|"GreaterThan"
--
-- * 'ifKey' - The name of the filter key.
--
-- * 'ifValues' - Inventory filter values. Example: inventory filter where instance IDs are specified as values Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
inventoryFilter
    :: Text -- ^ 'ifKey'
    -> NonEmpty Text -- ^ 'ifValues'
    -> InventoryFilter
inventoryFilter pKey_ pValues_ =
    InventoryFilter'
    { _ifType = Nothing
    , _ifKey = pKey_
    , _ifValues = _List1 # pValues_
    }

-- | The type of filter. Valid values include the following: "Equal"|"NotEqual"|"BeginWith"|"LessThan"|"GreaterThan"
ifType :: Lens' InventoryFilter (Maybe InventoryQueryOperatorType)
ifType = lens _ifType (\ s a -> s{_ifType = a});

-- | The name of the filter key.
ifKey :: Lens' InventoryFilter Text
ifKey = lens _ifKey (\ s a -> s{_ifKey = a});

-- | Inventory filter values. Example: inventory filter where instance IDs are specified as values Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
ifValues :: Lens' InventoryFilter (NonEmpty Text)
ifValues = lens _ifValues (\ s a -> s{_ifValues = a}) . _List1;

instance Hashable InventoryFilter

instance NFData InventoryFilter

instance ToJSON InventoryFilter where
        toJSON InventoryFilter'{..}
          = object
              (catMaybes
                 [("Type" .=) <$> _ifType, Just ("Key" .= _ifKey),
                  Just ("Values" .= _ifValues)])

-- | Information collected from managed instances based on your inventory policy document
--
--
--
-- /See:/ 'inventoryItem' smart constructor.
data InventoryItem = InventoryItem'
    { _iiContentHash   :: !(Maybe Text)
    , _iiContent       :: !(Maybe [Map Text Text])
    , _iiTypeName      :: !Text
    , _iiSchemaVersion :: !Text
    , _iiCaptureTime   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiContentHash' - MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The @PutInventory@ API does not update the inventory item type contents if the MD5 hash has not changed since last update.
--
-- * 'iiContent' - The inventory data of the inventory type.
--
-- * 'iiTypeName' - The name of the inventory type. Default inventory item type names start with @AWS@ . Custom inventory type names will start with @Custom@ . Default inventory item types include the following: @AWS:AWSComponent@ , @AWS:Application@ , @AWS:InstanceInformation@ , @AWS:Network@ , and @AWS:WindowsUpdate@ .
--
-- * 'iiSchemaVersion' - The schema version for the inventory item.
--
-- * 'iiCaptureTime' - The time the inventory information was collected.
inventoryItem
    :: Text -- ^ 'iiTypeName'
    -> Text -- ^ 'iiSchemaVersion'
    -> Text -- ^ 'iiCaptureTime'
    -> InventoryItem
inventoryItem pTypeName_ pSchemaVersion_ pCaptureTime_ =
    InventoryItem'
    { _iiContentHash = Nothing
    , _iiContent = Nothing
    , _iiTypeName = pTypeName_
    , _iiSchemaVersion = pSchemaVersion_
    , _iiCaptureTime = pCaptureTime_
    }

-- | MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The @PutInventory@ API does not update the inventory item type contents if the MD5 hash has not changed since last update.
iiContentHash :: Lens' InventoryItem (Maybe Text)
iiContentHash = lens _iiContentHash (\ s a -> s{_iiContentHash = a});

-- | The inventory data of the inventory type.
iiContent :: Lens' InventoryItem [HashMap Text Text]
iiContent = lens _iiContent (\ s a -> s{_iiContent = a}) . _Default . _Coerce;

-- | The name of the inventory type. Default inventory item type names start with @AWS@ . Custom inventory type names will start with @Custom@ . Default inventory item types include the following: @AWS:AWSComponent@ , @AWS:Application@ , @AWS:InstanceInformation@ , @AWS:Network@ , and @AWS:WindowsUpdate@ .
iiTypeName :: Lens' InventoryItem Text
iiTypeName = lens _iiTypeName (\ s a -> s{_iiTypeName = a});

-- | The schema version for the inventory item.
iiSchemaVersion :: Lens' InventoryItem Text
iiSchemaVersion = lens _iiSchemaVersion (\ s a -> s{_iiSchemaVersion = a});

-- | The time the inventory information was collected.
iiCaptureTime :: Lens' InventoryItem Text
iiCaptureTime = lens _iiCaptureTime (\ s a -> s{_iiCaptureTime = a});

instance Hashable InventoryItem

instance NFData InventoryItem

instance ToJSON InventoryItem where
        toJSON InventoryItem'{..}
          = object
              (catMaybes
                 [("ContentHash" .=) <$> _iiContentHash,
                  ("Content" .=) <$> _iiContent,
                  Just ("TypeName" .= _iiTypeName),
                  Just ("SchemaVersion" .= _iiSchemaVersion),
                  Just ("CaptureTime" .= _iiCaptureTime)])

-- | Attributes are the entries within the inventory item content. It contains name and value.
--
--
--
-- /See:/ 'inventoryItemAttribute' smart constructor.
data InventoryItemAttribute = InventoryItemAttribute'
    { _iiaName     :: !Text
    , _iiaDataType :: !InventoryAttributeDataType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryItemAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiaName' - Name of the inventory item attribute.
--
-- * 'iiaDataType' - The data type of the inventory item attribute.
inventoryItemAttribute
    :: Text -- ^ 'iiaName'
    -> InventoryAttributeDataType -- ^ 'iiaDataType'
    -> InventoryItemAttribute
inventoryItemAttribute pName_ pDataType_ =
    InventoryItemAttribute'
    { _iiaName = pName_
    , _iiaDataType = pDataType_
    }

-- | Name of the inventory item attribute.
iiaName :: Lens' InventoryItemAttribute Text
iiaName = lens _iiaName (\ s a -> s{_iiaName = a});

-- | The data type of the inventory item attribute.
iiaDataType :: Lens' InventoryItemAttribute InventoryAttributeDataType
iiaDataType = lens _iiaDataType (\ s a -> s{_iiaDataType = a});

instance FromJSON InventoryItemAttribute where
        parseJSON
          = withObject "InventoryItemAttribute"
              (\ x ->
                 InventoryItemAttribute' <$>
                   (x .: "Name") <*> (x .: "DataType"))

instance Hashable InventoryItemAttribute

instance NFData InventoryItemAttribute

-- | The inventory item schema definition. Users can use this to compose inventory query filters.
--
--
--
-- /See:/ 'inventoryItemSchema' smart constructor.
data InventoryItemSchema = InventoryItemSchema'
    { _iisVersion    :: !(Maybe Text)
    , _iisTypeName   :: !Text
    , _iisAttributes :: !(List1 InventoryItemAttribute)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryItemSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iisVersion' - The schema version for the inventory item.
--
-- * 'iisTypeName' - The name of the inventory type. Default inventory item type names start with @AWS@ . Custom inventory type names will start with @Custom@ . Default inventory item types include the following: @AWS:AWSComponent@ , @AWS:Application@ , @AWS:InstanceInformation@ , @AWS:Network@ , and @AWS:WindowsUpdate@ .
--
-- * 'iisAttributes' - The schema attributes for inventory. This contains data type and attribute name.
inventoryItemSchema
    :: Text -- ^ 'iisTypeName'
    -> NonEmpty InventoryItemAttribute -- ^ 'iisAttributes'
    -> InventoryItemSchema
inventoryItemSchema pTypeName_ pAttributes_ =
    InventoryItemSchema'
    { _iisVersion = Nothing
    , _iisTypeName = pTypeName_
    , _iisAttributes = _List1 # pAttributes_
    }

-- | The schema version for the inventory item.
iisVersion :: Lens' InventoryItemSchema (Maybe Text)
iisVersion = lens _iisVersion (\ s a -> s{_iisVersion = a});

-- | The name of the inventory type. Default inventory item type names start with @AWS@ . Custom inventory type names will start with @Custom@ . Default inventory item types include the following: @AWS:AWSComponent@ , @AWS:Application@ , @AWS:InstanceInformation@ , @AWS:Network@ , and @AWS:WindowsUpdate@ .
iisTypeName :: Lens' InventoryItemSchema Text
iisTypeName = lens _iisTypeName (\ s a -> s{_iisTypeName = a});

-- | The schema attributes for inventory. This contains data type and attribute name.
iisAttributes :: Lens' InventoryItemSchema (NonEmpty InventoryItemAttribute)
iisAttributes = lens _iisAttributes (\ s a -> s{_iisAttributes = a}) . _List1;

instance FromJSON InventoryItemSchema where
        parseJSON
          = withObject "InventoryItemSchema"
              (\ x ->
                 InventoryItemSchema' <$>
                   (x .:? "Version") <*> (x .: "TypeName") <*>
                     (x .: "Attributes"))

instance Hashable InventoryItemSchema

instance NFData InventoryItemSchema

-- | Inventory query results.
--
--
--
-- /See:/ 'inventoryResultEntity' smart constructor.
data InventoryResultEntity = InventoryResultEntity'
    { _ireData :: !(Maybe (Map Text InventoryResultItem))
    , _ireId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryResultEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ireData' - The data section in the inventory result entity json.
--
-- * 'ireId' - ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID.
inventoryResultEntity
    :: InventoryResultEntity
inventoryResultEntity =
    InventoryResultEntity'
    { _ireData = Nothing
    , _ireId = Nothing
    }

-- | The data section in the inventory result entity json.
ireData :: Lens' InventoryResultEntity (HashMap Text InventoryResultItem)
ireData = lens _ireData (\ s a -> s{_ireData = a}) . _Default . _Map;

-- | ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID.
ireId :: Lens' InventoryResultEntity (Maybe Text)
ireId = lens _ireId (\ s a -> s{_ireId = a});

instance FromJSON InventoryResultEntity where
        parseJSON
          = withObject "InventoryResultEntity"
              (\ x ->
                 InventoryResultEntity' <$>
                   (x .:? "Data" .!= mempty) <*> (x .:? "Id"))

instance Hashable InventoryResultEntity

instance NFData InventoryResultEntity

-- | The inventory result item.
--
--
--
-- /See:/ 'inventoryResultItem' smart constructor.
data InventoryResultItem = InventoryResultItem'
    { _iriContentHash   :: !(Maybe Text)
    , _iriCaptureTime   :: !(Maybe Text)
    , _iriTypeName      :: !Text
    , _iriSchemaVersion :: !Text
    , _iriContent       :: ![Map Text Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryResultItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iriContentHash' - MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The @PutInventory@ API does not update the inventory item type contents if the MD5 hash has not changed since last update.
--
-- * 'iriCaptureTime' - The time inventory item data was captured.
--
-- * 'iriTypeName' - The name of the inventory result item type.
--
-- * 'iriSchemaVersion' - The schema version for the inventory result item/
--
-- * 'iriContent' - Contains all the inventory data of the item type. Results include attribute names and values.
inventoryResultItem
    :: Text -- ^ 'iriTypeName'
    -> Text -- ^ 'iriSchemaVersion'
    -> InventoryResultItem
inventoryResultItem pTypeName_ pSchemaVersion_ =
    InventoryResultItem'
    { _iriContentHash = Nothing
    , _iriCaptureTime = Nothing
    , _iriTypeName = pTypeName_
    , _iriSchemaVersion = pSchemaVersion_
    , _iriContent = mempty
    }

-- | MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The @PutInventory@ API does not update the inventory item type contents if the MD5 hash has not changed since last update.
iriContentHash :: Lens' InventoryResultItem (Maybe Text)
iriContentHash = lens _iriContentHash (\ s a -> s{_iriContentHash = a});

-- | The time inventory item data was captured.
iriCaptureTime :: Lens' InventoryResultItem (Maybe Text)
iriCaptureTime = lens _iriCaptureTime (\ s a -> s{_iriCaptureTime = a});

-- | The name of the inventory result item type.
iriTypeName :: Lens' InventoryResultItem Text
iriTypeName = lens _iriTypeName (\ s a -> s{_iriTypeName = a});

-- | The schema version for the inventory result item/
iriSchemaVersion :: Lens' InventoryResultItem Text
iriSchemaVersion = lens _iriSchemaVersion (\ s a -> s{_iriSchemaVersion = a});

-- | Contains all the inventory data of the item type. Results include attribute names and values.
iriContent :: Lens' InventoryResultItem [HashMap Text Text]
iriContent = lens _iriContent (\ s a -> s{_iriContent = a}) . _Coerce;

instance FromJSON InventoryResultItem where
        parseJSON
          = withObject "InventoryResultItem"
              (\ x ->
                 InventoryResultItem' <$>
                   (x .:? "ContentHash") <*> (x .:? "CaptureTime") <*>
                     (x .: "TypeName")
                     <*> (x .: "SchemaVersion")
                     <*> (x .:? "Content" .!= mempty))

instance Hashable InventoryResultItem

instance NFData InventoryResultItem

-- | Information about an Amazon S3 bucket to write instance-level logs to.
--
--
--
-- /See:/ 'loggingInfo' smart constructor.
data LoggingInfo = LoggingInfo'
    { _liS3KeyPrefix  :: !(Maybe Text)
    , _liS3BucketName :: !Text
    , _liS3Region     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoggingInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liS3KeyPrefix' - (Optional) The Amazon S3 bucket subfolder.
--
-- * 'liS3BucketName' - The name of an Amazon S3 bucket where execution logs are stored .
--
-- * 'liS3Region' - The region where the Amazon S3 bucket is located.
loggingInfo
    :: Text -- ^ 'liS3BucketName'
    -> Text -- ^ 'liS3Region'
    -> LoggingInfo
loggingInfo pS3BucketName_ pS3Region_ =
    LoggingInfo'
    { _liS3KeyPrefix = Nothing
    , _liS3BucketName = pS3BucketName_
    , _liS3Region = pS3Region_
    }

-- | (Optional) The Amazon S3 bucket subfolder.
liS3KeyPrefix :: Lens' LoggingInfo (Maybe Text)
liS3KeyPrefix = lens _liS3KeyPrefix (\ s a -> s{_liS3KeyPrefix = a});

-- | The name of an Amazon S3 bucket where execution logs are stored .
liS3BucketName :: Lens' LoggingInfo Text
liS3BucketName = lens _liS3BucketName (\ s a -> s{_liS3BucketName = a});

-- | The region where the Amazon S3 bucket is located.
liS3Region :: Lens' LoggingInfo Text
liS3Region = lens _liS3Region (\ s a -> s{_liS3Region = a});

instance FromJSON LoggingInfo where
        parseJSON
          = withObject "LoggingInfo"
              (\ x ->
                 LoggingInfo' <$>
                   (x .:? "S3KeyPrefix") <*> (x .: "S3BucketName") <*>
                     (x .: "S3Region"))

instance Hashable LoggingInfo

instance NFData LoggingInfo

instance ToJSON LoggingInfo where
        toJSON LoggingInfo'{..}
          = object
              (catMaybes
                 [("S3KeyPrefix" .=) <$> _liS3KeyPrefix,
                  Just ("S3BucketName" .= _liS3BucketName),
                  Just ("S3Region" .= _liS3Region)])

-- | Describes the information about an execution of a Maintenance Window.
--
--
--
-- /See:/ 'maintenanceWindowExecution' smart constructor.
data MaintenanceWindowExecution = MaintenanceWindowExecution'
    { _mweStatus            :: !(Maybe MaintenanceWindowExecutionStatus)
    , _mweStartTime         :: !(Maybe POSIX)
    , _mweWindowExecutionId :: !(Maybe Text)
    , _mweStatusDetails     :: !(Maybe Text)
    , _mweEndTime           :: !(Maybe POSIX)
    , _mweWindowId          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MaintenanceWindowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mweStatus' - The status of the execution.
--
-- * 'mweStartTime' - The time the execution started.
--
-- * 'mweWindowExecutionId' - The ID of the Maintenance Window execution.
--
-- * 'mweStatusDetails' - The details explaining the Status. Only available for certain status values.
--
-- * 'mweEndTime' - The time the execution finished.
--
-- * 'mweWindowId' - The ID of the Maintenance Window.
maintenanceWindowExecution
    :: MaintenanceWindowExecution
maintenanceWindowExecution =
    MaintenanceWindowExecution'
    { _mweStatus = Nothing
    , _mweStartTime = Nothing
    , _mweWindowExecutionId = Nothing
    , _mweStatusDetails = Nothing
    , _mweEndTime = Nothing
    , _mweWindowId = Nothing
    }

-- | The status of the execution.
mweStatus :: Lens' MaintenanceWindowExecution (Maybe MaintenanceWindowExecutionStatus)
mweStatus = lens _mweStatus (\ s a -> s{_mweStatus = a});

-- | The time the execution started.
mweStartTime :: Lens' MaintenanceWindowExecution (Maybe UTCTime)
mweStartTime = lens _mweStartTime (\ s a -> s{_mweStartTime = a}) . mapping _Time;

-- | The ID of the Maintenance Window execution.
mweWindowExecutionId :: Lens' MaintenanceWindowExecution (Maybe Text)
mweWindowExecutionId = lens _mweWindowExecutionId (\ s a -> s{_mweWindowExecutionId = a});

-- | The details explaining the Status. Only available for certain status values.
mweStatusDetails :: Lens' MaintenanceWindowExecution (Maybe Text)
mweStatusDetails = lens _mweStatusDetails (\ s a -> s{_mweStatusDetails = a});

-- | The time the execution finished.
mweEndTime :: Lens' MaintenanceWindowExecution (Maybe UTCTime)
mweEndTime = lens _mweEndTime (\ s a -> s{_mweEndTime = a}) . mapping _Time;

-- | The ID of the Maintenance Window.
mweWindowId :: Lens' MaintenanceWindowExecution (Maybe Text)
mweWindowId = lens _mweWindowId (\ s a -> s{_mweWindowId = a});

instance FromJSON MaintenanceWindowExecution where
        parseJSON
          = withObject "MaintenanceWindowExecution"
              (\ x ->
                 MaintenanceWindowExecution' <$>
                   (x .:? "Status") <*> (x .:? "StartTime") <*>
                     (x .:? "WindowExecutionId")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "WindowId"))

instance Hashable MaintenanceWindowExecution

instance NFData MaintenanceWindowExecution

-- | Information about a task execution performed as part of a Maintenance Window execution.
--
--
--
-- /See:/ 'maintenanceWindowExecutionTaskIdentity' smart constructor.
data MaintenanceWindowExecutionTaskIdentity = MaintenanceWindowExecutionTaskIdentity'
    { _mwetiStatus            :: !(Maybe MaintenanceWindowExecutionStatus)
    , _mwetiTaskExecutionId   :: !(Maybe Text)
    , _mwetiStartTime         :: !(Maybe POSIX)
    , _mwetiTaskType          :: !(Maybe MaintenanceWindowTaskType)
    , _mwetiTaskARN           :: !(Maybe Text)
    , _mwetiWindowExecutionId :: !(Maybe Text)
    , _mwetiStatusDetails     :: !(Maybe Text)
    , _mwetiEndTime           :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MaintenanceWindowExecutionTaskIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwetiStatus' - The status of the task execution.
--
-- * 'mwetiTaskExecutionId' - The ID of the specific task execution in the Maintenance Window execution.
--
-- * 'mwetiStartTime' - The time the task execution started.
--
-- * 'mwetiTaskType' - The type of executed task.
--
-- * 'mwetiTaskARN' - The ARN of the executed task.
--
-- * 'mwetiWindowExecutionId' - The ID of the Maintenance Window execution that ran the task.
--
-- * 'mwetiStatusDetails' - The details explaining the status of the task execution. Only available for certain status values.
--
-- * 'mwetiEndTime' - The time the task execution finished.
maintenanceWindowExecutionTaskIdentity
    :: MaintenanceWindowExecutionTaskIdentity
maintenanceWindowExecutionTaskIdentity =
    MaintenanceWindowExecutionTaskIdentity'
    { _mwetiStatus = Nothing
    , _mwetiTaskExecutionId = Nothing
    , _mwetiStartTime = Nothing
    , _mwetiTaskType = Nothing
    , _mwetiTaskARN = Nothing
    , _mwetiWindowExecutionId = Nothing
    , _mwetiStatusDetails = Nothing
    , _mwetiEndTime = Nothing
    }

-- | The status of the task execution.
mwetiStatus :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe MaintenanceWindowExecutionStatus)
mwetiStatus = lens _mwetiStatus (\ s a -> s{_mwetiStatus = a});

-- | The ID of the specific task execution in the Maintenance Window execution.
mwetiTaskExecutionId :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe Text)
mwetiTaskExecutionId = lens _mwetiTaskExecutionId (\ s a -> s{_mwetiTaskExecutionId = a});

-- | The time the task execution started.
mwetiStartTime :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe UTCTime)
mwetiStartTime = lens _mwetiStartTime (\ s a -> s{_mwetiStartTime = a}) . mapping _Time;

-- | The type of executed task.
mwetiTaskType :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe MaintenanceWindowTaskType)
mwetiTaskType = lens _mwetiTaskType (\ s a -> s{_mwetiTaskType = a});

-- | The ARN of the executed task.
mwetiTaskARN :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe Text)
mwetiTaskARN = lens _mwetiTaskARN (\ s a -> s{_mwetiTaskARN = a});

-- | The ID of the Maintenance Window execution that ran the task.
mwetiWindowExecutionId :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe Text)
mwetiWindowExecutionId = lens _mwetiWindowExecutionId (\ s a -> s{_mwetiWindowExecutionId = a});

-- | The details explaining the status of the task execution. Only available for certain status values.
mwetiStatusDetails :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe Text)
mwetiStatusDetails = lens _mwetiStatusDetails (\ s a -> s{_mwetiStatusDetails = a});

-- | The time the task execution finished.
mwetiEndTime :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe UTCTime)
mwetiEndTime = lens _mwetiEndTime (\ s a -> s{_mwetiEndTime = a}) . mapping _Time;

instance FromJSON
         MaintenanceWindowExecutionTaskIdentity where
        parseJSON
          = withObject "MaintenanceWindowExecutionTaskIdentity"
              (\ x ->
                 MaintenanceWindowExecutionTaskIdentity' <$>
                   (x .:? "Status") <*> (x .:? "TaskExecutionId") <*>
                     (x .:? "StartTime")
                     <*> (x .:? "TaskType")
                     <*> (x .:? "TaskArn")
                     <*> (x .:? "WindowExecutionId")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "EndTime"))

instance Hashable
         MaintenanceWindowExecutionTaskIdentity

instance NFData
         MaintenanceWindowExecutionTaskIdentity

-- | Describes the information about a task invocation for a particular target as part of a task execution performed as part of a Maintenance Window execution.
--
--
--
-- /See:/ 'maintenanceWindowExecutionTaskInvocationIdentity' smart constructor.
data MaintenanceWindowExecutionTaskInvocationIdentity = MaintenanceWindowExecutionTaskInvocationIdentity'
    { _mwetiiStatus            :: !(Maybe MaintenanceWindowExecutionStatus)
    , _mwetiiExecutionId       :: !(Maybe Text)
    , _mwetiiTaskExecutionId   :: !(Maybe Text)
    , _mwetiiStartTime         :: !(Maybe POSIX)
    , _mwetiiInvocationId      :: !(Maybe Text)
    , _mwetiiOwnerInformation  :: !(Maybe (Sensitive Text))
    , _mwetiiWindowTargetId    :: !(Maybe Text)
    , _mwetiiWindowExecutionId :: !(Maybe Text)
    , _mwetiiStatusDetails     :: !(Maybe Text)
    , _mwetiiEndTime           :: !(Maybe POSIX)
    , _mwetiiParameters        :: !(Maybe (Sensitive Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MaintenanceWindowExecutionTaskInvocationIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwetiiStatus' - The status of the task invocation.
--
-- * 'mwetiiExecutionId' - The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
--
-- * 'mwetiiTaskExecutionId' - The ID of the specific task execution in the Maintenance Window execution.
--
-- * 'mwetiiStartTime' - The time the invocation started.
--
-- * 'mwetiiInvocationId' - The ID of the task invocation.
--
-- * 'mwetiiOwnerInformation' - User-provided value that was specified when the target was registered with the Maintenance Window. This was also included in any CloudWatch events raised during the task invocation.
--
-- * 'mwetiiWindowTargetId' - The ID of the target definition in this Maintenance Window the invocation was performed for.
--
-- * 'mwetiiWindowExecutionId' - The ID of the Maintenance Window execution that ran the task.
--
-- * 'mwetiiStatusDetails' - The details explaining the status of the task invocation. Only available for certain Status values.
--
-- * 'mwetiiEndTime' - The time the invocation finished.
--
-- * 'mwetiiParameters' - The parameters that were provided for the invocation when it was executed.
maintenanceWindowExecutionTaskInvocationIdentity
    :: MaintenanceWindowExecutionTaskInvocationIdentity
maintenanceWindowExecutionTaskInvocationIdentity =
    MaintenanceWindowExecutionTaskInvocationIdentity'
    { _mwetiiStatus = Nothing
    , _mwetiiExecutionId = Nothing
    , _mwetiiTaskExecutionId = Nothing
    , _mwetiiStartTime = Nothing
    , _mwetiiInvocationId = Nothing
    , _mwetiiOwnerInformation = Nothing
    , _mwetiiWindowTargetId = Nothing
    , _mwetiiWindowExecutionId = Nothing
    , _mwetiiStatusDetails = Nothing
    , _mwetiiEndTime = Nothing
    , _mwetiiParameters = Nothing
    }

-- | The status of the task invocation.
mwetiiStatus :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe MaintenanceWindowExecutionStatus)
mwetiiStatus = lens _mwetiiStatus (\ s a -> s{_mwetiiStatus = a});

-- | The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
mwetiiExecutionId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiExecutionId = lens _mwetiiExecutionId (\ s a -> s{_mwetiiExecutionId = a});

-- | The ID of the specific task execution in the Maintenance Window execution.
mwetiiTaskExecutionId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiTaskExecutionId = lens _mwetiiTaskExecutionId (\ s a -> s{_mwetiiTaskExecutionId = a});

-- | The time the invocation started.
mwetiiStartTime :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe UTCTime)
mwetiiStartTime = lens _mwetiiStartTime (\ s a -> s{_mwetiiStartTime = a}) . mapping _Time;

-- | The ID of the task invocation.
mwetiiInvocationId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiInvocationId = lens _mwetiiInvocationId (\ s a -> s{_mwetiiInvocationId = a});

-- | User-provided value that was specified when the target was registered with the Maintenance Window. This was also included in any CloudWatch events raised during the task invocation.
mwetiiOwnerInformation :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiOwnerInformation = lens _mwetiiOwnerInformation (\ s a -> s{_mwetiiOwnerInformation = a}) . mapping _Sensitive;

-- | The ID of the target definition in this Maintenance Window the invocation was performed for.
mwetiiWindowTargetId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiWindowTargetId = lens _mwetiiWindowTargetId (\ s a -> s{_mwetiiWindowTargetId = a});

-- | The ID of the Maintenance Window execution that ran the task.
mwetiiWindowExecutionId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiWindowExecutionId = lens _mwetiiWindowExecutionId (\ s a -> s{_mwetiiWindowExecutionId = a});

-- | The details explaining the status of the task invocation. Only available for certain Status values.
mwetiiStatusDetails :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiStatusDetails = lens _mwetiiStatusDetails (\ s a -> s{_mwetiiStatusDetails = a});

-- | The time the invocation finished.
mwetiiEndTime :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe UTCTime)
mwetiiEndTime = lens _mwetiiEndTime (\ s a -> s{_mwetiiEndTime = a}) . mapping _Time;

-- | The parameters that were provided for the invocation when it was executed.
mwetiiParameters :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiParameters = lens _mwetiiParameters (\ s a -> s{_mwetiiParameters = a}) . mapping _Sensitive;

instance FromJSON
         MaintenanceWindowExecutionTaskInvocationIdentity
         where
        parseJSON
          = withObject
              "MaintenanceWindowExecutionTaskInvocationIdentity"
              (\ x ->
                 MaintenanceWindowExecutionTaskInvocationIdentity' <$>
                   (x .:? "Status") <*> (x .:? "ExecutionId") <*>
                     (x .:? "TaskExecutionId")
                     <*> (x .:? "StartTime")
                     <*> (x .:? "InvocationId")
                     <*> (x .:? "OwnerInformation")
                     <*> (x .:? "WindowTargetId")
                     <*> (x .:? "WindowExecutionId")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "Parameters"))

instance Hashable
         MaintenanceWindowExecutionTaskInvocationIdentity

instance NFData
         MaintenanceWindowExecutionTaskInvocationIdentity

-- | Filter used in the request.
--
--
--
-- /See:/ 'maintenanceWindowFilter' smart constructor.
data MaintenanceWindowFilter = MaintenanceWindowFilter'
    { _mwfValues :: !(Maybe [Text])
    , _mwfKey    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MaintenanceWindowFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwfValues' - The filter values.
--
-- * 'mwfKey' - The name of the filter.
maintenanceWindowFilter
    :: MaintenanceWindowFilter
maintenanceWindowFilter =
    MaintenanceWindowFilter'
    { _mwfValues = Nothing
    , _mwfKey = Nothing
    }

-- | The filter values.
mwfValues :: Lens' MaintenanceWindowFilter [Text]
mwfValues = lens _mwfValues (\ s a -> s{_mwfValues = a}) . _Default . _Coerce;

-- | The name of the filter.
mwfKey :: Lens' MaintenanceWindowFilter (Maybe Text)
mwfKey = lens _mwfKey (\ s a -> s{_mwfKey = a});

instance Hashable MaintenanceWindowFilter

instance NFData MaintenanceWindowFilter

instance ToJSON MaintenanceWindowFilter where
        toJSON MaintenanceWindowFilter'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _mwfValues,
                  ("Key" .=) <$> _mwfKey])

-- | Information about the Maintenance Window.
--
--
--
-- /See:/ 'maintenanceWindowIdentity' smart constructor.
data MaintenanceWindowIdentity = MaintenanceWindowIdentity'
    { _mwiEnabled  :: !(Maybe Bool)
    , _mwiName     :: !(Maybe Text)
    , _mwiCutoff   :: !(Maybe Nat)
    , _mwiDuration :: !(Maybe Nat)
    , _mwiWindowId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MaintenanceWindowIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwiEnabled' - Whether the Maintenance Window is enabled.
--
-- * 'mwiName' - The name of the Maintenance Window.
--
-- * 'mwiCutoff' - The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
--
-- * 'mwiDuration' - The duration of the Maintenance Window in hours.
--
-- * 'mwiWindowId' - The ID of the Maintenance Window.
maintenanceWindowIdentity
    :: MaintenanceWindowIdentity
maintenanceWindowIdentity =
    MaintenanceWindowIdentity'
    { _mwiEnabled = Nothing
    , _mwiName = Nothing
    , _mwiCutoff = Nothing
    , _mwiDuration = Nothing
    , _mwiWindowId = Nothing
    }

-- | Whether the Maintenance Window is enabled.
mwiEnabled :: Lens' MaintenanceWindowIdentity (Maybe Bool)
mwiEnabled = lens _mwiEnabled (\ s a -> s{_mwiEnabled = a});

-- | The name of the Maintenance Window.
mwiName :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiName = lens _mwiName (\ s a -> s{_mwiName = a});

-- | The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
mwiCutoff :: Lens' MaintenanceWindowIdentity (Maybe Natural)
mwiCutoff = lens _mwiCutoff (\ s a -> s{_mwiCutoff = a}) . mapping _Nat;

-- | The duration of the Maintenance Window in hours.
mwiDuration :: Lens' MaintenanceWindowIdentity (Maybe Natural)
mwiDuration = lens _mwiDuration (\ s a -> s{_mwiDuration = a}) . mapping _Nat;

-- | The ID of the Maintenance Window.
mwiWindowId :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiWindowId = lens _mwiWindowId (\ s a -> s{_mwiWindowId = a});

instance FromJSON MaintenanceWindowIdentity where
        parseJSON
          = withObject "MaintenanceWindowIdentity"
              (\ x ->
                 MaintenanceWindowIdentity' <$>
                   (x .:? "Enabled") <*> (x .:? "Name") <*>
                     (x .:? "Cutoff")
                     <*> (x .:? "Duration")
                     <*> (x .:? "WindowId"))

instance Hashable MaintenanceWindowIdentity

instance NFData MaintenanceWindowIdentity

-- | The target registered with the Maintenance Window.
--
--
--
-- /See:/ 'maintenanceWindowTarget' smart constructor.
data MaintenanceWindowTarget = MaintenanceWindowTarget'
    { _mResourceType     :: !(Maybe MaintenanceWindowResourceType)
    , _mOwnerInformation :: !(Maybe (Sensitive Text))
    , _mWindowTargetId   :: !(Maybe Text)
    , _mTargets          :: !(Maybe [Target])
    , _mWindowId         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MaintenanceWindowTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mResourceType' - The type of target.
--
-- * 'mOwnerInformation' - User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this Maintenance Window.
--
-- * 'mWindowTargetId' - The ID of the target.
--
-- * 'mTargets' - The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
--
-- * 'mWindowId' - The Maintenance Window ID where the target is registered.
maintenanceWindowTarget
    :: MaintenanceWindowTarget
maintenanceWindowTarget =
    MaintenanceWindowTarget'
    { _mResourceType = Nothing
    , _mOwnerInformation = Nothing
    , _mWindowTargetId = Nothing
    , _mTargets = Nothing
    , _mWindowId = Nothing
    }

-- | The type of target.
mResourceType :: Lens' MaintenanceWindowTarget (Maybe MaintenanceWindowResourceType)
mResourceType = lens _mResourceType (\ s a -> s{_mResourceType = a});

-- | User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this Maintenance Window.
mOwnerInformation :: Lens' MaintenanceWindowTarget (Maybe Text)
mOwnerInformation = lens _mOwnerInformation (\ s a -> s{_mOwnerInformation = a}) . mapping _Sensitive;

-- | The ID of the target.
mWindowTargetId :: Lens' MaintenanceWindowTarget (Maybe Text)
mWindowTargetId = lens _mWindowTargetId (\ s a -> s{_mWindowTargetId = a});

-- | The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
mTargets :: Lens' MaintenanceWindowTarget [Target]
mTargets = lens _mTargets (\ s a -> s{_mTargets = a}) . _Default . _Coerce;

-- | The Maintenance Window ID where the target is registered.
mWindowId :: Lens' MaintenanceWindowTarget (Maybe Text)
mWindowId = lens _mWindowId (\ s a -> s{_mWindowId = a});

instance FromJSON MaintenanceWindowTarget where
        parseJSON
          = withObject "MaintenanceWindowTarget"
              (\ x ->
                 MaintenanceWindowTarget' <$>
                   (x .:? "ResourceType") <*> (x .:? "OwnerInformation")
                     <*> (x .:? "WindowTargetId")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "WindowId"))

instance Hashable MaintenanceWindowTarget

instance NFData MaintenanceWindowTarget

-- | Information about a task defined for a Maintenance Window.
--
--
--
-- /See:/ 'maintenanceWindowTask' smart constructor.
data MaintenanceWindowTask = MaintenanceWindowTask'
    { _mwtServiceRoleARN :: !(Maybe Text)
    , _mwtWindowTaskId   :: !(Maybe Text)
    , _mwtTaskParameters :: !(Maybe (Sensitive (Map Text (Sensitive MaintenanceWindowTaskParameterValueExpression))))
    , _mwtPriority       :: !(Maybe Nat)
    , _mwtTaskARN        :: !(Maybe Text)
    , _mwtMaxErrors      :: !(Maybe Text)
    , _mwtTargets        :: !(Maybe [Target])
    , _mwtLoggingInfo    :: !(Maybe LoggingInfo)
    , _mwtType           :: !(Maybe MaintenanceWindowTaskType)
    , _mwtMaxConcurrency :: !(Maybe Text)
    , _mwtWindowId       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MaintenanceWindowTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwtServiceRoleARN' - The role that should be assumed when executing the task
--
-- * 'mwtWindowTaskId' - The task ID.
--
-- * 'mwtTaskParameters' - The parameters that should be passed to the task when it is executed.
--
-- * 'mwtPriority' - The priority of the task in the Maintenance Window, the lower the number the higher the priority. Tasks in a Maintenance Window are scheduled in priority order with tasks that have the same priority scheduled in parallel.
--
-- * 'mwtTaskARN' - The ARN of the task to execute.
--
-- * 'mwtMaxErrors' - The maximum number of errors allowed before this task stops being scheduled.
--
-- * 'mwtTargets' - The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
--
-- * 'mwtLoggingInfo' - Information about an Amazon S3 bucket to write task-level logs to.
--
-- * 'mwtType' - The type of task.
--
-- * 'mwtMaxConcurrency' - The maximum number of targets this task can be run for in parallel.
--
-- * 'mwtWindowId' - The Maintenance Window ID where the task is registered.
maintenanceWindowTask
    :: MaintenanceWindowTask
maintenanceWindowTask =
    MaintenanceWindowTask'
    { _mwtServiceRoleARN = Nothing
    , _mwtWindowTaskId = Nothing
    , _mwtTaskParameters = Nothing
    , _mwtPriority = Nothing
    , _mwtTaskARN = Nothing
    , _mwtMaxErrors = Nothing
    , _mwtTargets = Nothing
    , _mwtLoggingInfo = Nothing
    , _mwtType = Nothing
    , _mwtMaxConcurrency = Nothing
    , _mwtWindowId = Nothing
    }

-- | The role that should be assumed when executing the task
mwtServiceRoleARN :: Lens' MaintenanceWindowTask (Maybe Text)
mwtServiceRoleARN = lens _mwtServiceRoleARN (\ s a -> s{_mwtServiceRoleARN = a});

-- | The task ID.
mwtWindowTaskId :: Lens' MaintenanceWindowTask (Maybe Text)
mwtWindowTaskId = lens _mwtWindowTaskId (\ s a -> s{_mwtWindowTaskId = a});

-- | The parameters that should be passed to the task when it is executed.
mwtTaskParameters :: Lens' MaintenanceWindowTask (Maybe (HashMap Text MaintenanceWindowTaskParameterValueExpression))
mwtTaskParameters = lens _mwtTaskParameters (\ s a -> s{_mwtTaskParameters = a}) . mapping (_Sensitive . _Map);

-- | The priority of the task in the Maintenance Window, the lower the number the higher the priority. Tasks in a Maintenance Window are scheduled in priority order with tasks that have the same priority scheduled in parallel.
mwtPriority :: Lens' MaintenanceWindowTask (Maybe Natural)
mwtPriority = lens _mwtPriority (\ s a -> s{_mwtPriority = a}) . mapping _Nat;

-- | The ARN of the task to execute.
mwtTaskARN :: Lens' MaintenanceWindowTask (Maybe Text)
mwtTaskARN = lens _mwtTaskARN (\ s a -> s{_mwtTaskARN = a});

-- | The maximum number of errors allowed before this task stops being scheduled.
mwtMaxErrors :: Lens' MaintenanceWindowTask (Maybe Text)
mwtMaxErrors = lens _mwtMaxErrors (\ s a -> s{_mwtMaxErrors = a});

-- | The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
mwtTargets :: Lens' MaintenanceWindowTask [Target]
mwtTargets = lens _mwtTargets (\ s a -> s{_mwtTargets = a}) . _Default . _Coerce;

-- | Information about an Amazon S3 bucket to write task-level logs to.
mwtLoggingInfo :: Lens' MaintenanceWindowTask (Maybe LoggingInfo)
mwtLoggingInfo = lens _mwtLoggingInfo (\ s a -> s{_mwtLoggingInfo = a});

-- | The type of task.
mwtType :: Lens' MaintenanceWindowTask (Maybe MaintenanceWindowTaskType)
mwtType = lens _mwtType (\ s a -> s{_mwtType = a});

-- | The maximum number of targets this task can be run for in parallel.
mwtMaxConcurrency :: Lens' MaintenanceWindowTask (Maybe Text)
mwtMaxConcurrency = lens _mwtMaxConcurrency (\ s a -> s{_mwtMaxConcurrency = a});

-- | The Maintenance Window ID where the task is registered.
mwtWindowId :: Lens' MaintenanceWindowTask (Maybe Text)
mwtWindowId = lens _mwtWindowId (\ s a -> s{_mwtWindowId = a});

instance FromJSON MaintenanceWindowTask where
        parseJSON
          = withObject "MaintenanceWindowTask"
              (\ x ->
                 MaintenanceWindowTask' <$>
                   (x .:? "ServiceRoleArn") <*> (x .:? "WindowTaskId")
                     <*> (x .:? "TaskParameters" .!= mempty)
                     <*> (x .:? "Priority")
                     <*> (x .:? "TaskArn")
                     <*> (x .:? "MaxErrors")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "LoggingInfo")
                     <*> (x .:? "Type")
                     <*> (x .:? "MaxConcurrency")
                     <*> (x .:? "WindowId"))

instance Hashable MaintenanceWindowTask

instance NFData MaintenanceWindowTask

-- | Defines the values for a task parameter.
--
--
--
-- /See:/ 'maintenanceWindowTaskParameterValueExpression' smart constructor.
newtype MaintenanceWindowTaskParameterValueExpression = MaintenanceWindowTaskParameterValueExpression'
    { _mwtpveValues :: Maybe (Sensitive [Sensitive Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MaintenanceWindowTaskParameterValueExpression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwtpveValues' - This field contains an array of 0 or more strings, each 1 to 255 characters in length.
maintenanceWindowTaskParameterValueExpression
    :: MaintenanceWindowTaskParameterValueExpression
maintenanceWindowTaskParameterValueExpression =
    MaintenanceWindowTaskParameterValueExpression'
    { _mwtpveValues = Nothing
    }

-- | This field contains an array of 0 or more strings, each 1 to 255 characters in length.
mwtpveValues :: Lens' MaintenanceWindowTaskParameterValueExpression (Maybe [Text])
mwtpveValues = lens _mwtpveValues (\ s a -> s{_mwtpveValues = a}) . mapping (_Sensitive . _Coerce);

instance FromJSON
         MaintenanceWindowTaskParameterValueExpression where
        parseJSON
          = withObject
              "MaintenanceWindowTaskParameterValueExpression"
              (\ x ->
                 MaintenanceWindowTaskParameterValueExpression' <$>
                   (x .:? "Values" .!= mempty))

instance Hashable
         MaintenanceWindowTaskParameterValueExpression

instance NFData
         MaintenanceWindowTaskParameterValueExpression

instance ToJSON
         MaintenanceWindowTaskParameterValueExpression where
        toJSON
          MaintenanceWindowTaskParameterValueExpression'{..}
          = object
              (catMaybes [("Values" .=) <$> _mwtpveValues])

-- | Configurations for sending notifications.
--
--
--
-- /See:/ 'notificationConfig' smart constructor.
data NotificationConfig = NotificationConfig'
    { _ncNotificationEvents :: !(Maybe [NotificationEvent])
    , _ncNotificationType   :: !(Maybe NotificationType)
    , _ncNotificationARN    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NotificationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncNotificationEvents' - The different events for which you can receive notifications. These events include the following: All (events), InProgress, Success, TimedOut, Cancelled, Failed. To learn more about these events, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitor-commands.html Monitoring Commands> in the /Amazon Elastic Compute Cloud User Guide / .
--
-- * 'ncNotificationType' - Command: Receive notification when the status of a command changes. Invocation: For commands sent to multiple instances, receive notification on a per-instance basis when the status of a command changes.
--
-- * 'ncNotificationARN' - An Amazon Resource Name (ARN) for a Simple Notification Service (SNS) topic. Run Command pushes notifications about command status changes to this topic.
notificationConfig
    :: NotificationConfig
notificationConfig =
    NotificationConfig'
    { _ncNotificationEvents = Nothing
    , _ncNotificationType = Nothing
    , _ncNotificationARN = Nothing
    }

-- | The different events for which you can receive notifications. These events include the following: All (events), InProgress, Success, TimedOut, Cancelled, Failed. To learn more about these events, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitor-commands.html Monitoring Commands> in the /Amazon Elastic Compute Cloud User Guide / .
ncNotificationEvents :: Lens' NotificationConfig [NotificationEvent]
ncNotificationEvents = lens _ncNotificationEvents (\ s a -> s{_ncNotificationEvents = a}) . _Default . _Coerce;

-- | Command: Receive notification when the status of a command changes. Invocation: For commands sent to multiple instances, receive notification on a per-instance basis when the status of a command changes.
ncNotificationType :: Lens' NotificationConfig (Maybe NotificationType)
ncNotificationType = lens _ncNotificationType (\ s a -> s{_ncNotificationType = a});

-- | An Amazon Resource Name (ARN) for a Simple Notification Service (SNS) topic. Run Command pushes notifications about command status changes to this topic.
ncNotificationARN :: Lens' NotificationConfig (Maybe Text)
ncNotificationARN = lens _ncNotificationARN (\ s a -> s{_ncNotificationARN = a});

instance FromJSON NotificationConfig where
        parseJSON
          = withObject "NotificationConfig"
              (\ x ->
                 NotificationConfig' <$>
                   (x .:? "NotificationEvents" .!= mempty) <*>
                     (x .:? "NotificationType")
                     <*> (x .:? "NotificationArn"))

instance Hashable NotificationConfig

instance NFData NotificationConfig

instance ToJSON NotificationConfig where
        toJSON NotificationConfig'{..}
          = object
              (catMaybes
                 [("NotificationEvents" .=) <$> _ncNotificationEvents,
                  ("NotificationType" .=) <$> _ncNotificationType,
                  ("NotificationArn" .=) <$> _ncNotificationARN])

-- | An Amazon EC2 Systems Manager parameter in Parameter Store.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
    { _pValue :: !(Maybe Text)
    , _pName  :: !(Maybe Text)
    , _pType  :: !(Maybe ParameterType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pValue' - The parameter value.
--
-- * 'pName' - The name of the parameter.
--
-- * 'pType' - The type of parameter. Valid values include the following: String, String list, Secure string.
parameter
    :: Parameter
parameter =
    Parameter'
    { _pValue = Nothing
    , _pName = Nothing
    , _pType = Nothing
    }

-- | The parameter value.
pValue :: Lens' Parameter (Maybe Text)
pValue = lens _pValue (\ s a -> s{_pValue = a});

-- | The name of the parameter.
pName :: Lens' Parameter (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a});

-- | The type of parameter. Valid values include the following: String, String list, Secure string.
pType :: Lens' Parameter (Maybe ParameterType)
pType = lens _pType (\ s a -> s{_pType = a});

instance FromJSON Parameter where
        parseJSON
          = withObject "Parameter"
              (\ x ->
                 Parameter' <$>
                   (x .:? "Value") <*> (x .:? "Name") <*>
                     (x .:? "Type"))

instance Hashable Parameter

instance NFData Parameter

-- | Information about parameter usage.
--
--
--
-- /See:/ 'parameterHistory' smart constructor.
data ParameterHistory = ParameterHistory'
    { _phLastModifiedDate :: !(Maybe POSIX)
    , _phKeyId            :: !(Maybe Text)
    , _phValue            :: !(Maybe Text)
    , _phName             :: !(Maybe Text)
    , _phLastModifiedUser :: !(Maybe Text)
    , _phType             :: !(Maybe ParameterType)
    , _phDescription      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ParameterHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phLastModifiedDate' - Date the parameter was last changed or updated.
--
-- * 'phKeyId' - The ID of the query key used for this parameter.
--
-- * 'phValue' - The parameter value.
--
-- * 'phName' - The name of the parameter.
--
-- * 'phLastModifiedUser' - Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
--
-- * 'phType' - The type of parameter used.
--
-- * 'phDescription' - Information about the parameter.
parameterHistory
    :: ParameterHistory
parameterHistory =
    ParameterHistory'
    { _phLastModifiedDate = Nothing
    , _phKeyId = Nothing
    , _phValue = Nothing
    , _phName = Nothing
    , _phLastModifiedUser = Nothing
    , _phType = Nothing
    , _phDescription = Nothing
    }

-- | Date the parameter was last changed or updated.
phLastModifiedDate :: Lens' ParameterHistory (Maybe UTCTime)
phLastModifiedDate = lens _phLastModifiedDate (\ s a -> s{_phLastModifiedDate = a}) . mapping _Time;

-- | The ID of the query key used for this parameter.
phKeyId :: Lens' ParameterHistory (Maybe Text)
phKeyId = lens _phKeyId (\ s a -> s{_phKeyId = a});

-- | The parameter value.
phValue :: Lens' ParameterHistory (Maybe Text)
phValue = lens _phValue (\ s a -> s{_phValue = a});

-- | The name of the parameter.
phName :: Lens' ParameterHistory (Maybe Text)
phName = lens _phName (\ s a -> s{_phName = a});

-- | Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
phLastModifiedUser :: Lens' ParameterHistory (Maybe Text)
phLastModifiedUser = lens _phLastModifiedUser (\ s a -> s{_phLastModifiedUser = a});

-- | The type of parameter used.
phType :: Lens' ParameterHistory (Maybe ParameterType)
phType = lens _phType (\ s a -> s{_phType = a});

-- | Information about the parameter.
phDescription :: Lens' ParameterHistory (Maybe Text)
phDescription = lens _phDescription (\ s a -> s{_phDescription = a});

instance FromJSON ParameterHistory where
        parseJSON
          = withObject "ParameterHistory"
              (\ x ->
                 ParameterHistory' <$>
                   (x .:? "LastModifiedDate") <*> (x .:? "KeyId") <*>
                     (x .:? "Value")
                     <*> (x .:? "Name")
                     <*> (x .:? "LastModifiedUser")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable ParameterHistory

instance NFData ParameterHistory

-- | Metada includes information like the ARN of the last user and the date/time the parameter was last used.
--
--
--
-- /See:/ 'parameterMetadata' smart constructor.
data ParameterMetadata = ParameterMetadata'
    { _pmLastModifiedDate :: !(Maybe POSIX)
    , _pmKeyId            :: !(Maybe Text)
    , _pmName             :: !(Maybe Text)
    , _pmLastModifiedUser :: !(Maybe Text)
    , _pmType             :: !(Maybe ParameterType)
    , _pmDescription      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ParameterMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmLastModifiedDate' - Date the parameter was last changed or updated.
--
-- * 'pmKeyId' - The ID of the query key used for this parameter.
--
-- * 'pmName' - The parameter name.
--
-- * 'pmLastModifiedUser' - Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
--
-- * 'pmType' - The type of parameter. Valid parameter types include the following: String, String list, Secure string.
--
-- * 'pmDescription' - Description of the parameter actions.
parameterMetadata
    :: ParameterMetadata
parameterMetadata =
    ParameterMetadata'
    { _pmLastModifiedDate = Nothing
    , _pmKeyId = Nothing
    , _pmName = Nothing
    , _pmLastModifiedUser = Nothing
    , _pmType = Nothing
    , _pmDescription = Nothing
    }

-- | Date the parameter was last changed or updated.
pmLastModifiedDate :: Lens' ParameterMetadata (Maybe UTCTime)
pmLastModifiedDate = lens _pmLastModifiedDate (\ s a -> s{_pmLastModifiedDate = a}) . mapping _Time;

-- | The ID of the query key used for this parameter.
pmKeyId :: Lens' ParameterMetadata (Maybe Text)
pmKeyId = lens _pmKeyId (\ s a -> s{_pmKeyId = a});

-- | The parameter name.
pmName :: Lens' ParameterMetadata (Maybe Text)
pmName = lens _pmName (\ s a -> s{_pmName = a});

-- | Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
pmLastModifiedUser :: Lens' ParameterMetadata (Maybe Text)
pmLastModifiedUser = lens _pmLastModifiedUser (\ s a -> s{_pmLastModifiedUser = a});

-- | The type of parameter. Valid parameter types include the following: String, String list, Secure string.
pmType :: Lens' ParameterMetadata (Maybe ParameterType)
pmType = lens _pmType (\ s a -> s{_pmType = a});

-- | Description of the parameter actions.
pmDescription :: Lens' ParameterMetadata (Maybe Text)
pmDescription = lens _pmDescription (\ s a -> s{_pmDescription = a});

instance FromJSON ParameterMetadata where
        parseJSON
          = withObject "ParameterMetadata"
              (\ x ->
                 ParameterMetadata' <$>
                   (x .:? "LastModifiedDate") <*> (x .:? "KeyId") <*>
                     (x .:? "Name")
                     <*> (x .:? "LastModifiedUser")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable ParameterMetadata

instance NFData ParameterMetadata

-- | One or more filters. Use a filter to return a more specific list of results.
--
--
--
-- /See:/ 'parametersFilter' smart constructor.
data ParametersFilter = ParametersFilter'
    { _pfKey    :: !(Maybe ParametersFilterKey)
    , _pfValues :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ParametersFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfKey' - The name of the filter.
--
-- * 'pfValues' - The filter values.
parametersFilter
    :: NonEmpty Text -- ^ 'pfValues'
    -> ParametersFilter
parametersFilter pValues_ =
    ParametersFilter'
    { _pfKey = Nothing
    , _pfValues = _List1 # pValues_
    }

-- | The name of the filter.
pfKey :: Lens' ParametersFilter (Maybe ParametersFilterKey)
pfKey = lens _pfKey (\ s a -> s{_pfKey = a});

-- | The filter values.
pfValues :: Lens' ParametersFilter (NonEmpty Text)
pfValues = lens _pfValues (\ s a -> s{_pfValues = a}) . _List1;

instance Hashable ParametersFilter

instance NFData ParametersFilter

instance ToJSON ParametersFilter where
        toJSON ParametersFilter'{..}
          = object
              (catMaybes
                 [("Key" .=) <$> _pfKey,
                  Just ("Values" .= _pfValues)])

-- | The inventory item result attribute.
--
--
--
-- /See:/ 'resultAttribute' smart constructor.
newtype ResultAttribute = ResultAttribute'
    { _raTypeName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResultAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raTypeName' - Name of the inventory item type. Valid value: “AWS:InstanceInformation”. Default Value: “AWS:InstanceInformation”.
resultAttribute
    :: Text -- ^ 'raTypeName'
    -> ResultAttribute
resultAttribute pTypeName_ =
    ResultAttribute'
    { _raTypeName = pTypeName_
    }

-- | Name of the inventory item type. Valid value: “AWS:InstanceInformation”. Default Value: “AWS:InstanceInformation”.
raTypeName :: Lens' ResultAttribute Text
raTypeName = lens _raTypeName (\ s a -> s{_raTypeName = a});

instance Hashable ResultAttribute

instance NFData ResultAttribute

instance ToJSON ResultAttribute where
        toJSON ResultAttribute'{..}
          = object
              (catMaybes [Just ("TypeName" .= _raTypeName)])

-- | An Amazon S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 's3OutputLocation' smart constructor.
data S3OutputLocation = S3OutputLocation'
    { _solOutputS3KeyPrefix  :: !(Maybe Text)
    , _solOutputS3Region     :: !(Maybe Text)
    , _solOutputS3BucketName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3OutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'solOutputS3KeyPrefix' - The Amazon S3 bucket subfolder.
--
-- * 'solOutputS3Region' - The Amazon S3 region where the association information is stored.
--
-- * 'solOutputS3BucketName' - The name of the Amazon S3 bucket.
s3OutputLocation
    :: S3OutputLocation
s3OutputLocation =
    S3OutputLocation'
    { _solOutputS3KeyPrefix = Nothing
    , _solOutputS3Region = Nothing
    , _solOutputS3BucketName = Nothing
    }

-- | The Amazon S3 bucket subfolder.
solOutputS3KeyPrefix :: Lens' S3OutputLocation (Maybe Text)
solOutputS3KeyPrefix = lens _solOutputS3KeyPrefix (\ s a -> s{_solOutputS3KeyPrefix = a});

-- | The Amazon S3 region where the association information is stored.
solOutputS3Region :: Lens' S3OutputLocation (Maybe Text)
solOutputS3Region = lens _solOutputS3Region (\ s a -> s{_solOutputS3Region = a});

-- | The name of the Amazon S3 bucket.
solOutputS3BucketName :: Lens' S3OutputLocation (Maybe Text)
solOutputS3BucketName = lens _solOutputS3BucketName (\ s a -> s{_solOutputS3BucketName = a});

instance FromJSON S3OutputLocation where
        parseJSON
          = withObject "S3OutputLocation"
              (\ x ->
                 S3OutputLocation' <$>
                   (x .:? "OutputS3KeyPrefix") <*>
                     (x .:? "OutputS3Region")
                     <*> (x .:? "OutputS3BucketName"))

instance Hashable S3OutputLocation

instance NFData S3OutputLocation

instance ToJSON S3OutputLocation where
        toJSON S3OutputLocation'{..}
          = object
              (catMaybes
                 [("OutputS3KeyPrefix" .=) <$> _solOutputS3KeyPrefix,
                  ("OutputS3Region" .=) <$> _solOutputS3Region,
                  ("OutputS3BucketName" .=) <$>
                    _solOutputS3BucketName])

-- | A URL for the Amazon S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 's3OutputURL' smart constructor.
newtype S3OutputURL = S3OutputURL'
    { _souOutputURL :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3OutputURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'souOutputURL' - A URL for an Amazon S3 bucket where you want to store the results of this request.
s3OutputURL
    :: S3OutputURL
s3OutputURL =
    S3OutputURL'
    { _souOutputURL = Nothing
    }

-- | A URL for an Amazon S3 bucket where you want to store the results of this request.
souOutputURL :: Lens' S3OutputURL (Maybe Text)
souOutputURL = lens _souOutputURL (\ s a -> s{_souOutputURL = a});

instance FromJSON S3OutputURL where
        parseJSON
          = withObject "S3OutputURL"
              (\ x -> S3OutputURL' <$> (x .:? "OutputUrl"))

instance Hashable S3OutputURL

instance NFData S3OutputURL

-- | Detailed information about an the execution state of an Automation step.
--
--
--
-- /See:/ 'stepExecution' smart constructor.
data StepExecution = StepExecution'
    { _seInputs             :: !(Maybe (Map Text Text))
    , _seStepName           :: !(Maybe Text)
    , _seExecutionEndTime   :: !(Maybe POSIX)
    , _seFailureMessage     :: !(Maybe Text)
    , _seResponse           :: !(Maybe Text)
    , _seAction             :: !(Maybe Text)
    , _seResponseCode       :: !(Maybe Text)
    , _seStepStatus         :: !(Maybe AutomationExecutionStatus)
    , _seOutputs            :: !(Maybe (Map Text [Text]))
    , _seExecutionStartTime :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StepExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seInputs' - Fully-resolved values passed into the step before execution.
--
-- * 'seStepName' - The name of this execution step.
--
-- * 'seExecutionEndTime' - If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
--
-- * 'seFailureMessage' - If a step failed, this message explains why the execution failed.
--
-- * 'seResponse' - A message associated with the response code for an execution.
--
-- * 'seAction' - The action this step performs. The action determines the behavior of the step.
--
-- * 'seResponseCode' - The response code returned by the execution of the step.
--
-- * 'seStepStatus' - The execution status for this step. Valid values include: @Pending@ , @InProgress@ , @Success@ , @Cancelled@ , @Failed@ , and @TimedOut@ .
--
-- * 'seOutputs' - Returned values from the execution of the step.
--
-- * 'seExecutionStartTime' - If a step has begun execution, this contains the time the step started. If the step is in @Pending@ status, this field is not populated.
stepExecution
    :: StepExecution
stepExecution =
    StepExecution'
    { _seInputs = Nothing
    , _seStepName = Nothing
    , _seExecutionEndTime = Nothing
    , _seFailureMessage = Nothing
    , _seResponse = Nothing
    , _seAction = Nothing
    , _seResponseCode = Nothing
    , _seStepStatus = Nothing
    , _seOutputs = Nothing
    , _seExecutionStartTime = Nothing
    }

-- | Fully-resolved values passed into the step before execution.
seInputs :: Lens' StepExecution (HashMap Text Text)
seInputs = lens _seInputs (\ s a -> s{_seInputs = a}) . _Default . _Map;

-- | The name of this execution step.
seStepName :: Lens' StepExecution (Maybe Text)
seStepName = lens _seStepName (\ s a -> s{_seStepName = a});

-- | If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
seExecutionEndTime :: Lens' StepExecution (Maybe UTCTime)
seExecutionEndTime = lens _seExecutionEndTime (\ s a -> s{_seExecutionEndTime = a}) . mapping _Time;

-- | If a step failed, this message explains why the execution failed.
seFailureMessage :: Lens' StepExecution (Maybe Text)
seFailureMessage = lens _seFailureMessage (\ s a -> s{_seFailureMessage = a});

-- | A message associated with the response code for an execution.
seResponse :: Lens' StepExecution (Maybe Text)
seResponse = lens _seResponse (\ s a -> s{_seResponse = a});

-- | The action this step performs. The action determines the behavior of the step.
seAction :: Lens' StepExecution (Maybe Text)
seAction = lens _seAction (\ s a -> s{_seAction = a});

-- | The response code returned by the execution of the step.
seResponseCode :: Lens' StepExecution (Maybe Text)
seResponseCode = lens _seResponseCode (\ s a -> s{_seResponseCode = a});

-- | The execution status for this step. Valid values include: @Pending@ , @InProgress@ , @Success@ , @Cancelled@ , @Failed@ , and @TimedOut@ .
seStepStatus :: Lens' StepExecution (Maybe AutomationExecutionStatus)
seStepStatus = lens _seStepStatus (\ s a -> s{_seStepStatus = a});

-- | Returned values from the execution of the step.
seOutputs :: Lens' StepExecution (HashMap Text [Text])
seOutputs = lens _seOutputs (\ s a -> s{_seOutputs = a}) . _Default . _Map;

-- | If a step has begun execution, this contains the time the step started. If the step is in @Pending@ status, this field is not populated.
seExecutionStartTime :: Lens' StepExecution (Maybe UTCTime)
seExecutionStartTime = lens _seExecutionStartTime (\ s a -> s{_seExecutionStartTime = a}) . mapping _Time;

instance FromJSON StepExecution where
        parseJSON
          = withObject "StepExecution"
              (\ x ->
                 StepExecution' <$>
                   (x .:? "Inputs" .!= mempty) <*> (x .:? "StepName")
                     <*> (x .:? "ExecutionEndTime")
                     <*> (x .:? "FailureMessage")
                     <*> (x .:? "Response")
                     <*> (x .:? "Action")
                     <*> (x .:? "ResponseCode")
                     <*> (x .:? "StepStatus")
                     <*> (x .:? "Outputs" .!= mempty)
                     <*> (x .:? "ExecutionStartTime"))

instance Hashable StepExecution

instance NFData StepExecution

-- | Metadata that you assign to your managed instances. Tags enable you to categorize your managed instances in different ways, for example, by purpose, owner, or environment.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagKey   :: !Text
    , _tagValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The name of the tag.
--
-- * 'tagValue' - The value of the tag.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ =
    Tag'
    { _tagKey = pKey_
    , _tagValue = pValue_
    }

-- | The name of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | The value of the tag.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag

instance NFData Tag

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])

-- | An array of search criteria that targets instances using a @Key@ ;@Value@ combination that you specify. @Targets@ is required if you don't provide one or more instance IDs in the call.
--
--
--
-- /See:/ 'target' smart constructor.
data Target = Target'
    { _tValues :: !(Maybe [Text])
    , _tKey    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tValues' - User-defined criteria that maps to @Key@ . For example, if you specified @tag:ServerRole@ , you could specify @value:WebServer@ to execute a command on instances that include Amazon EC2 tags of ServerRole;WebServer. For more information about how to send commands that target instances using @Key@ ;@Value@ parameters, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Windows).
--
-- * 'tKey' - User-defined criteria for sending commands that target instances that meet the criteria. @Key@ can be @tag:<Amazon EC2 tag>@ or @name:<Amazon EC2 instance ID>@ . For example, @tag:ServerRole@ or @name:0123456789012345@ . For more information about how to send commands that target instances using @Key@ ;@Value@ parameters, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Windows).
target
    :: Target
target =
    Target'
    { _tValues = Nothing
    , _tKey = Nothing
    }

-- | User-defined criteria that maps to @Key@ . For example, if you specified @tag:ServerRole@ , you could specify @value:WebServer@ to execute a command on instances that include Amazon EC2 tags of ServerRole;WebServer. For more information about how to send commands that target instances using @Key@ ;@Value@ parameters, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Windows).
tValues :: Lens' Target [Text]
tValues = lens _tValues (\ s a -> s{_tValues = a}) . _Default . _Coerce;

-- | User-defined criteria for sending commands that target instances that meet the criteria. @Key@ can be @tag:<Amazon EC2 tag>@ or @name:<Amazon EC2 instance ID>@ . For example, @tag:ServerRole@ or @name:0123456789012345@ . For more information about how to send commands that target instances using @Key@ ;@Value@ parameters, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Linux) or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/run-command.html Executing a Command Using Amazon EC2 Run Command> (Windows).
tKey :: Lens' Target (Maybe Text)
tKey = lens _tKey (\ s a -> s{_tKey = a});

instance FromJSON Target where
        parseJSON
          = withObject "Target"
              (\ x ->
                 Target' <$>
                   (x .:? "Values" .!= mempty) <*> (x .:? "Key"))

instance Hashable Target

instance NFData Target

instance ToJSON Target where
        toJSON Target'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _tValues, ("Key" .=) <$> _tKey])
