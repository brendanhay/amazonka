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
-- /Note:/ This module is auto-generated and exported for convenience, but should
-- not be considered stable as the internal representations and naming is subject
-- to change per release.
module Network.AWS.SSM.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.SSM.Types.Sum

-- | An activation registers one or more on-premises servers or virtual machines (VMs) with AWS so that you can configure those servers or VMs using Run Command. A server or VM that has been registered with AWS is called a managed instance.
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
-- * 'aExpired'
--
-- * 'aDefaultInstanceName'
--
-- * 'aActivationId'
--
-- * 'aCreatedDate'
--
-- * 'aRegistrationLimit'
--
-- * 'aExpirationDate'
--
-- * 'aDescription'
--
-- * 'aRegistrationsCount'
--
-- * 'aIAMRole'
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

-- | The ID created by SSM when you submitted the activation.
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
-- /See:/ 'association' smart constructor.
data Association = Association'
    { _aInstanceId :: !(Maybe Text)
    , _aName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Association' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aInstanceId'
--
-- * 'aName'
association
    :: Association
association =
    Association'
    { _aInstanceId = Nothing
    , _aName = Nothing
    }

-- | The ID of the instance.
aInstanceId :: Lens' Association (Maybe Text)
aInstanceId = lens _aInstanceId (\ s a -> s{_aInstanceId = a});

-- | The name of the SSM document.
aName :: Lens' Association (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

instance FromJSON Association where
        parseJSON
          = withObject "Association"
              (\ x ->
                 Association' <$>
                   (x .:? "InstanceId") <*> (x .:? "Name"))

instance Hashable Association

instance NFData Association

-- | Describes the parameters for a document.
--
-- /See:/ 'associationDescription' smart constructor.
data AssociationDescription = AssociationDescription'
    { _adInstanceId :: !(Maybe Text)
    , _adStatus     :: !(Maybe AssociationStatus)
    , _adDate       :: !(Maybe POSIX)
    , _adName       :: !(Maybe Text)
    , _adParameters :: !(Maybe (Map Text [Text]))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adInstanceId'
--
-- * 'adStatus'
--
-- * 'adDate'
--
-- * 'adName'
--
-- * 'adParameters'
associationDescription
    :: AssociationDescription
associationDescription =
    AssociationDescription'
    { _adInstanceId = Nothing
    , _adStatus = Nothing
    , _adDate = Nothing
    , _adName = Nothing
    , _adParameters = Nothing
    }

-- | The ID of the instance.
adInstanceId :: Lens' AssociationDescription (Maybe Text)
adInstanceId = lens _adInstanceId (\ s a -> s{_adInstanceId = a});

-- | The association status.
adStatus :: Lens' AssociationDescription (Maybe AssociationStatus)
adStatus = lens _adStatus (\ s a -> s{_adStatus = a});

-- | The date when the association was made.
adDate :: Lens' AssociationDescription (Maybe UTCTime)
adDate = lens _adDate (\ s a -> s{_adDate = a}) . mapping _Time;

-- | The name of the SSM document.
adName :: Lens' AssociationDescription (Maybe Text)
adName = lens _adName (\ s a -> s{_adName = a});

-- | A description of the parameters for a document.
adParameters :: Lens' AssociationDescription (HashMap Text [Text])
adParameters = lens _adParameters (\ s a -> s{_adParameters = a}) . _Default . _Map;

instance FromJSON AssociationDescription where
        parseJSON
          = withObject "AssociationDescription"
              (\ x ->
                 AssociationDescription' <$>
                   (x .:? "InstanceId") <*> (x .:? "Status") <*>
                     (x .:? "Date")
                     <*> (x .:? "Name")
                     <*> (x .:? "Parameters" .!= mempty))

instance Hashable AssociationDescription

instance NFData AssociationDescription

-- | Describes a filter.
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
-- * 'afKey'
--
-- * 'afValue'
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

-- | Describes an association status.
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
-- * 'asAdditionalInfo'
--
-- * 'asDate'
--
-- * 'asName'
--
-- * 'asMessage'
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

-- | Describes a command request.
--
-- /See:/ 'command' smart constructor.
data Command = Command'
    { _cStatus             :: !(Maybe CommandStatus)
    , _cExpiresAfter       :: !(Maybe POSIX)
    , _cNotificationConfig :: !(Maybe NotificationConfig)
    , _cOutputS3KeyPrefix  :: !(Maybe Text)
    , _cDocumentName       :: !(Maybe Text)
    , _cInstanceIds        :: !(Maybe (List1 Text))
    , _cCommandId          :: !(Maybe Text)
    , _cParameters         :: !(Maybe (Map Text [Text]))
    , _cComment            :: !(Maybe Text)
    , _cOutputS3BucketName :: !(Maybe Text)
    , _cRequestedDateTime  :: !(Maybe POSIX)
    , _cServiceRole        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus'
--
-- * 'cExpiresAfter'
--
-- * 'cNotificationConfig'
--
-- * 'cOutputS3KeyPrefix'
--
-- * 'cDocumentName'
--
-- * 'cInstanceIds'
--
-- * 'cCommandId'
--
-- * 'cParameters'
--
-- * 'cComment'
--
-- * 'cOutputS3BucketName'
--
-- * 'cRequestedDateTime'
--
-- * 'cServiceRole'
command
    :: Command
command =
    Command'
    { _cStatus = Nothing
    , _cExpiresAfter = Nothing
    , _cNotificationConfig = Nothing
    , _cOutputS3KeyPrefix = Nothing
    , _cDocumentName = Nothing
    , _cInstanceIds = Nothing
    , _cCommandId = Nothing
    , _cParameters = Nothing
    , _cComment = Nothing
    , _cOutputS3BucketName = Nothing
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

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
cOutputS3KeyPrefix :: Lens' Command (Maybe Text)
cOutputS3KeyPrefix = lens _cOutputS3KeyPrefix (\ s a -> s{_cOutputS3KeyPrefix = a});

-- | The name of the SSM document requested for execution.
cDocumentName :: Lens' Command (Maybe Text)
cDocumentName = lens _cDocumentName (\ s a -> s{_cDocumentName = a});

-- | The instance IDs against which this command was requested.
cInstanceIds :: Lens' Command (Maybe (NonEmpty Text))
cInstanceIds = lens _cInstanceIds (\ s a -> s{_cInstanceIds = a}) . mapping _List1;

-- | A unique identifier for this command.
cCommandId :: Lens' Command (Maybe Text)
cCommandId = lens _cCommandId (\ s a -> s{_cCommandId = a});

-- | The parameter values to be inserted in the SSM document when executing the command.
cParameters :: Lens' Command (HashMap Text [Text])
cParameters = lens _cParameters (\ s a -> s{_cParameters = a}) . _Default . _Map;

-- | User-specified information about the command, such as a brief description of what the command should do.
cComment :: Lens' Command (Maybe Text)
cComment = lens _cComment (\ s a -> s{_cComment = a});

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
cOutputS3BucketName :: Lens' Command (Maybe Text)
cOutputS3BucketName = lens _cOutputS3BucketName (\ s a -> s{_cOutputS3BucketName = a});

-- | The date and time the command was requested.
cRequestedDateTime :: Lens' Command (Maybe UTCTime)
cRequestedDateTime = lens _cRequestedDateTime (\ s a -> s{_cRequestedDateTime = a}) . mapping _Time;

-- | The IAM service role that SSM uses to act on your behalf when sending notifications about command status changes.
cServiceRole :: Lens' Command (Maybe Text)
cServiceRole = lens _cServiceRole (\ s a -> s{_cServiceRole = a});

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "Status") <*> (x .:? "ExpiresAfter") <*>
                     (x .:? "NotificationConfig")
                     <*> (x .:? "OutputS3KeyPrefix")
                     <*> (x .:? "DocumentName")
                     <*> (x .:? "InstanceIds")
                     <*> (x .:? "CommandId")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "Comment")
                     <*> (x .:? "OutputS3BucketName")
                     <*> (x .:? "RequestedDateTime")
                     <*> (x .:? "ServiceRole"))

instance Hashable Command

instance NFData Command

-- | Describes a command filter.
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
-- * 'cfKey'
--
-- * 'cfValue'
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
-- /See:/ 'commandInvocation' smart constructor.
data CommandInvocation = CommandInvocation'
    { _ciInstanceId         :: !(Maybe Text)
    , _ciStatus             :: !(Maybe CommandInvocationStatus)
    , _ciNotificationConfig :: !(Maybe NotificationConfig)
    , _ciCommandPlugins     :: !(Maybe [CommandPlugin])
    , _ciDocumentName       :: !(Maybe Text)
    , _ciCommandId          :: !(Maybe Text)
    , _ciComment            :: !(Maybe Text)
    , _ciTraceOutput        :: !(Maybe Text)
    , _ciRequestedDateTime  :: !(Maybe POSIX)
    , _ciServiceRole        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommandInvocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciInstanceId'
--
-- * 'ciStatus'
--
-- * 'ciNotificationConfig'
--
-- * 'ciCommandPlugins'
--
-- * 'ciDocumentName'
--
-- * 'ciCommandId'
--
-- * 'ciComment'
--
-- * 'ciTraceOutput'
--
-- * 'ciRequestedDateTime'
--
-- * 'ciServiceRole'
commandInvocation
    :: CommandInvocation
commandInvocation =
    CommandInvocation'
    { _ciInstanceId = Nothing
    , _ciStatus = Nothing
    , _ciNotificationConfig = Nothing
    , _ciCommandPlugins = Nothing
    , _ciDocumentName = Nothing
    , _ciCommandId = Nothing
    , _ciComment = Nothing
    , _ciTraceOutput = Nothing
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

-- | The command against which this invocation was requested.
ciCommandId :: Lens' CommandInvocation (Maybe Text)
ciCommandId = lens _ciCommandId (\ s a -> s{_ciCommandId = a});

-- | User-specified information about the command, such as a brief description of what the command should do.
ciComment :: Lens' CommandInvocation (Maybe Text)
ciComment = lens _ciComment (\ s a -> s{_ciComment = a});

-- | Gets the trace output sent by the agent.
ciTraceOutput :: Lens' CommandInvocation (Maybe Text)
ciTraceOutput = lens _ciTraceOutput (\ s a -> s{_ciTraceOutput = a});

-- | The time and date the request was sent to this instance.
ciRequestedDateTime :: Lens' CommandInvocation (Maybe UTCTime)
ciRequestedDateTime = lens _ciRequestedDateTime (\ s a -> s{_ciRequestedDateTime = a}) . mapping _Time;

-- | The IAM service role that SSM uses to act on your behalf when sending notifications about command status changes on a per instance basis.
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
                     <*> (x .:? "CommandId")
                     <*> (x .:? "Comment")
                     <*> (x .:? "TraceOutput")
                     <*> (x .:? "RequestedDateTime")
                     <*> (x .:? "ServiceRole"))

instance Hashable CommandInvocation

instance NFData CommandInvocation

-- | Describes plugin details.
--
-- /See:/ 'commandPlugin' smart constructor.
data CommandPlugin = CommandPlugin'
    { _cpStatus                 :: !(Maybe CommandPluginStatus)
    , _cpResponseStartDateTime  :: !(Maybe POSIX)
    , _cpOutputS3KeyPrefix      :: !(Maybe Text)
    , _cpResponseCode           :: !(Maybe Int)
    , _cpOutput                 :: !(Maybe Text)
    , _cpName                   :: !(Maybe Text)
    , _cpOutputS3BucketName     :: !(Maybe Text)
    , _cpResponseFinishDateTime :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommandPlugin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpStatus'
--
-- * 'cpResponseStartDateTime'
--
-- * 'cpOutputS3KeyPrefix'
--
-- * 'cpResponseCode'
--
-- * 'cpOutput'
--
-- * 'cpName'
--
-- * 'cpOutputS3BucketName'
--
-- * 'cpResponseFinishDateTime'
commandPlugin
    :: CommandPlugin
commandPlugin =
    CommandPlugin'
    { _cpStatus = Nothing
    , _cpResponseStartDateTime = Nothing
    , _cpOutputS3KeyPrefix = Nothing
    , _cpResponseCode = Nothing
    , _cpOutput = Nothing
    , _cpName = Nothing
    , _cpOutputS3BucketName = Nothing
    , _cpResponseFinishDateTime = Nothing
    }

-- | The status of this plugin. You can execute a document with multiple plugins.
cpStatus :: Lens' CommandPlugin (Maybe CommandPluginStatus)
cpStatus = lens _cpStatus (\ s a -> s{_cpStatus = a});

-- | The time the plugin started executing.
cpResponseStartDateTime :: Lens' CommandPlugin (Maybe UTCTime)
cpResponseStartDateTime = lens _cpResponseStartDateTime (\ s a -> s{_cpResponseStartDateTime = a}) . mapping _Time;

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
cpOutputS3KeyPrefix :: Lens' CommandPlugin (Maybe Text)
cpOutputS3KeyPrefix = lens _cpOutputS3KeyPrefix (\ s a -> s{_cpOutputS3KeyPrefix = a});

-- | A numeric response code generated after executing the plugin.
cpResponseCode :: Lens' CommandPlugin (Maybe Int)
cpResponseCode = lens _cpResponseCode (\ s a -> s{_cpResponseCode = a});

-- | Output of the plugin execution.
cpOutput :: Lens' CommandPlugin (Maybe Text)
cpOutput = lens _cpOutput (\ s a -> s{_cpOutput = a});

-- | The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
cpName :: Lens' CommandPlugin (Maybe Text)
cpName = lens _cpName (\ s a -> s{_cpName = a});

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
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
                     <*> (x .:? "ResponseCode")
                     <*> (x .:? "Output")
                     <*> (x .:? "Name")
                     <*> (x .:? "OutputS3BucketName")
                     <*> (x .:? "ResponseFinishDateTime"))

instance Hashable CommandPlugin

instance NFData CommandPlugin

-- | Describes the association of an SSM document and an instance.
--
-- /See:/ 'createAssociationBatchRequestEntry' smart constructor.
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
    { _cabreInstanceId :: !(Maybe Text)
    , _cabreName       :: !(Maybe Text)
    , _cabreParameters :: !(Maybe (Map Text [Text]))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAssociationBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cabreInstanceId'
--
-- * 'cabreName'
--
-- * 'cabreParameters'
createAssociationBatchRequestEntry
    :: CreateAssociationBatchRequestEntry
createAssociationBatchRequestEntry =
    CreateAssociationBatchRequestEntry'
    { _cabreInstanceId = Nothing
    , _cabreName = Nothing
    , _cabreParameters = Nothing
    }

-- | The ID of the instance.
cabreInstanceId :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreInstanceId = lens _cabreInstanceId (\ s a -> s{_cabreInstanceId = a});

-- | The name of the configuration document.
cabreName :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreName = lens _cabreName (\ s a -> s{_cabreName = a});

-- | A description of the parameters for a document.
cabreParameters :: Lens' CreateAssociationBatchRequestEntry (HashMap Text [Text])
cabreParameters = lens _cabreParameters (\ s a -> s{_cabreParameters = a}) . _Default . _Map;

instance FromJSON CreateAssociationBatchRequestEntry
         where
        parseJSON
          = withObject "CreateAssociationBatchRequestEntry"
              (\ x ->
                 CreateAssociationBatchRequestEntry' <$>
                   (x .:? "InstanceId") <*> (x .:? "Name") <*>
                     (x .:? "Parameters" .!= mempty))

instance Hashable CreateAssociationBatchRequestEntry

instance NFData CreateAssociationBatchRequestEntry

instance ToJSON CreateAssociationBatchRequestEntry
         where
        toJSON CreateAssociationBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _cabreInstanceId,
                  ("Name" .=) <$> _cabreName,
                  ("Parameters" .=) <$> _cabreParameters])

-- | Filter for the DescribeActivation API.
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
-- * 'dafFilterKey'
--
-- * 'dafFilterValues'
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

-- | Describes an SSM document.
--
-- /See:/ 'documentDescription' smart constructor.
data DocumentDescription = DocumentDescription'
    { _dStatus        :: !(Maybe DocumentStatus)
    , _dHash          :: !(Maybe Text)
    , _dSha1          :: !(Maybe Text)
    , _dOwner         :: !(Maybe Text)
    , _dPlatformTypes :: !(Maybe [PlatformType])
    , _dCreatedDate   :: !(Maybe POSIX)
    , _dName          :: !(Maybe Text)
    , _dHashType      :: !(Maybe DocumentHashType)
    , _dParameters    :: !(Maybe [DocumentParameter])
    , _dDescription   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStatus'
--
-- * 'dHash'
--
-- * 'dSha1'
--
-- * 'dOwner'
--
-- * 'dPlatformTypes'
--
-- * 'dCreatedDate'
--
-- * 'dName'
--
-- * 'dHashType'
--
-- * 'dParameters'
--
-- * 'dDescription'
documentDescription
    :: DocumentDescription
documentDescription =
    DocumentDescription'
    { _dStatus = Nothing
    , _dHash = Nothing
    , _dSha1 = Nothing
    , _dOwner = Nothing
    , _dPlatformTypes = Nothing
    , _dCreatedDate = Nothing
    , _dName = Nothing
    , _dHashType = Nothing
    , _dParameters = Nothing
    , _dDescription = Nothing
    }

-- | The status of the SSM document.
dStatus :: Lens' DocumentDescription (Maybe DocumentStatus)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | The Sha256 or Sha1 hash created by the system when the document was created.
--
-- Sha1 hashes have been deprecated.
dHash :: Lens' DocumentDescription (Maybe Text)
dHash = lens _dHash (\ s a -> s{_dHash = a});

-- | The SHA1 hash of the document, which you can use for verification purposes.
dSha1 :: Lens' DocumentDescription (Maybe Text)
dSha1 = lens _dSha1 (\ s a -> s{_dSha1 = a});

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
--
-- Sha1 hashes have been deprecated.
dHashType :: Lens' DocumentDescription (Maybe DocumentHashType)
dHashType = lens _dHashType (\ s a -> s{_dHashType = a});

-- | A description of the parameters for a document.
dParameters :: Lens' DocumentDescription [DocumentParameter]
dParameters = lens _dParameters (\ s a -> s{_dParameters = a}) . _Default . _Coerce;

-- | A description of the document.
dDescription :: Lens' DocumentDescription (Maybe Text)
dDescription = lens _dDescription (\ s a -> s{_dDescription = a});

instance FromJSON DocumentDescription where
        parseJSON
          = withObject "DocumentDescription"
              (\ x ->
                 DocumentDescription' <$>
                   (x .:? "Status") <*> (x .:? "Hash") <*>
                     (x .:? "Sha1")
                     <*> (x .:? "Owner")
                     <*> (x .:? "PlatformTypes" .!= mempty)
                     <*> (x .:? "CreatedDate")
                     <*> (x .:? "Name")
                     <*> (x .:? "HashType")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "Description"))

instance Hashable DocumentDescription

instance NFData DocumentDescription

-- | Describes a filter.
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
-- * 'dfKey'
--
-- * 'dfValue'
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
-- /See:/ 'documentIdentifier' smart constructor.
data DocumentIdentifier = DocumentIdentifier'
    { _diOwner         :: !(Maybe Text)
    , _diPlatformTypes :: !(Maybe [PlatformType])
    , _diName          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diOwner'
--
-- * 'diPlatformTypes'
--
-- * 'diName'
documentIdentifier
    :: DocumentIdentifier
documentIdentifier =
    DocumentIdentifier'
    { _diOwner = Nothing
    , _diPlatformTypes = Nothing
    , _diName = Nothing
    }

-- | The AWS user account of the person who created the document.
diOwner :: Lens' DocumentIdentifier (Maybe Text)
diOwner = lens _diOwner (\ s a -> s{_diOwner = a});

-- | The operating system platform.
diPlatformTypes :: Lens' DocumentIdentifier [PlatformType]
diPlatformTypes = lens _diPlatformTypes (\ s a -> s{_diPlatformTypes = a}) . _Default . _Coerce;

-- | The name of the SSM document.
diName :: Lens' DocumentIdentifier (Maybe Text)
diName = lens _diName (\ s a -> s{_diName = a});

instance FromJSON DocumentIdentifier where
        parseJSON
          = withObject "DocumentIdentifier"
              (\ x ->
                 DocumentIdentifier' <$>
                   (x .:? "Owner") <*>
                     (x .:? "PlatformTypes" .!= mempty)
                     <*> (x .:? "Name"))

instance Hashable DocumentIdentifier

instance NFData DocumentIdentifier

-- | Parameters specified in the SSM document that execute on the server when the command is run.
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
-- * 'dpName'
--
-- * 'dpDefaultValue'
--
-- * 'dpType'
--
-- * 'dpDescription'
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

-- | Describes a failed association.
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
-- * 'fcaEntry'
--
-- * 'fcaFault'
--
-- * 'fcaMessage'
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

-- | Describes a filter for a specific list of instances.
--
-- /See:/ 'instanceInformation' smart constructor.
data InstanceInformation = InstanceInformation'
    { _iiInstanceId       :: !(Maybe Text)
    , _iiPingStatus       :: !(Maybe PingStatus)
    , _iiIPAddress        :: !(Maybe Text)
    , _iiResourceType     :: !(Maybe ResourceType)
    , _iiRegistrationDate :: !(Maybe POSIX)
    , _iiPlatformVersion  :: !(Maybe Text)
    , _iiIsLatestVersion  :: !(Maybe Bool)
    , _iiAgentVersion     :: !(Maybe Text)
    , _iiLastPingDateTime :: !(Maybe POSIX)
    , _iiActivationId     :: !(Maybe Text)
    , _iiName             :: !(Maybe Text)
    , _iiPlatformType     :: !(Maybe PlatformType)
    , _iiPlatformName     :: !(Maybe Text)
    , _iiComputerName     :: !(Maybe Text)
    , _iiIAMRole          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiInstanceId'
--
-- * 'iiPingStatus'
--
-- * 'iiIPAddress'
--
-- * 'iiResourceType'
--
-- * 'iiRegistrationDate'
--
-- * 'iiPlatformVersion'
--
-- * 'iiIsLatestVersion'
--
-- * 'iiAgentVersion'
--
-- * 'iiLastPingDateTime'
--
-- * 'iiActivationId'
--
-- * 'iiName'
--
-- * 'iiPlatformType'
--
-- * 'iiPlatformName'
--
-- * 'iiComputerName'
--
-- * 'iiIAMRole'
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
    , _iiActivationId = Nothing
    , _iiName = Nothing
    , _iiPlatformType = Nothing
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

-- | The version of the SSM agent running on your instance.
iiAgentVersion :: Lens' InstanceInformation (Maybe Text)
iiAgentVersion = lens _iiAgentVersion (\ s a -> s{_iiAgentVersion = a});

-- | The date and time when agent last pinged SSM service.
iiLastPingDateTime :: Lens' InstanceInformation (Maybe UTCTime)
iiLastPingDateTime = lens _iiLastPingDateTime (\ s a -> s{_iiLastPingDateTime = a}) . mapping _Time;

-- | The activation ID created by SSM when the server or VM was registered.
iiActivationId :: Lens' InstanceInformation (Maybe Text)
iiActivationId = lens _iiActivationId (\ s a -> s{_iiActivationId = a});

-- | The name of the managed instance.
iiName :: Lens' InstanceInformation (Maybe Text)
iiName = lens _iiName (\ s a -> s{_iiName = a});

-- | The operating system platform type.
iiPlatformType :: Lens' InstanceInformation (Maybe PlatformType)
iiPlatformType = lens _iiPlatformType (\ s a -> s{_iiPlatformType = a});

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
                     <*> (x .:? "ActivationId")
                     <*> (x .:? "Name")
                     <*> (x .:? "PlatformType")
                     <*> (x .:? "PlatformName")
                     <*> (x .:? "ComputerName")
                     <*> (x .:? "IamRole"))

instance Hashable InstanceInformation

instance NFData InstanceInformation

-- | Describes a filter for a specific list of instances.
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
-- * 'iifKey'
--
-- * 'iifValueSet'
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

-- | Configurations for sending notifications.
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
-- * 'ncNotificationEvents'
--
-- * 'ncNotificationType'
--
-- * 'ncNotificationARN'
notificationConfig
    :: NotificationConfig
notificationConfig =
    NotificationConfig'
    { _ncNotificationEvents = Nothing
    , _ncNotificationType = Nothing
    , _ncNotificationARN = Nothing
    }

-- | The different events for which you can receive notifications. These events include the following: All (events), InProgress, Success, TimedOut, Cancelled, Failed. To learn more about these events, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitor-commands.html Monitoring Commands> in the /Amazon Elastic Compute Cloud User Guide/ .
ncNotificationEvents :: Lens' NotificationConfig [NotificationEvent]
ncNotificationEvents = lens _ncNotificationEvents (\ s a -> s{_ncNotificationEvents = a}) . _Default . _Coerce;

-- | Command: Receive notification when the status of a command changes. Invocation: For commands sent to multiple instances, receive notification on a per-instance basis when the status of a command changes.
ncNotificationType :: Lens' NotificationConfig (Maybe NotificationType)
ncNotificationType = lens _ncNotificationType (\ s a -> s{_ncNotificationType = a});

-- | An Amazon Resource Name (ARN) for a Simple Notification Service (SNS) topic. SSM pushes notifications about command status changes to this topic.
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

-- | Metadata that you assign to your managed instances. Tags enable you to categorize your managed instances in different ways, for example, by purpose, owner, or environment.
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
-- * 'tagKey'
--
-- * 'tagValue'
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
