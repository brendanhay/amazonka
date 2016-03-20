{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.SSM.Types.Sum

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
    , _cOutputS3KeyPrefix  :: !(Maybe Text)
    , _cDocumentName       :: !(Maybe Text)
    , _cInstanceIds        :: !(Maybe (List1 Text))
    , _cCommandId          :: !(Maybe Text)
    , _cParameters         :: !(Maybe (Map Text [Text]))
    , _cComment            :: !(Maybe Text)
    , _cOutputS3BucketName :: !(Maybe Text)
    , _cRequestedDateTime  :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus'
--
-- * 'cExpiresAfter'
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
command
    :: Command
command =
    Command'
    { _cStatus = Nothing
    , _cExpiresAfter = Nothing
    , _cOutputS3KeyPrefix = Nothing
    , _cDocumentName = Nothing
    , _cInstanceIds = Nothing
    , _cCommandId = Nothing
    , _cParameters = Nothing
    , _cComment = Nothing
    , _cOutputS3BucketName = Nothing
    , _cRequestedDateTime = Nothing
    }

-- | The status of the command.
cStatus :: Lens' Command (Maybe CommandStatus)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});

-- | If this time is reached and the command has not already started
-- executing, it will not execute. Calculated based on the ExpiresAfter
-- user input provided as part of the SendCommand API.
cExpiresAfter :: Lens' Command (Maybe UTCTime)
cExpiresAfter = lens _cExpiresAfter (\ s a -> s{_cExpiresAfter = a}) . mapping _Time;

-- | The S3 directory path inside the bucket where the responses to the
-- command executions should be stored. This was requested when issuing the
-- command.
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

-- | The parameter values to be inserted in the SSM document when executing
-- the command.
cParameters :: Lens' Command (HashMap Text [Text])
cParameters = lens _cParameters (\ s a -> s{_cParameters = a}) . _Default . _Map;

-- | User-specified information about the command, such as a brief
-- description of what the command should do.
cComment :: Lens' Command (Maybe Text)
cComment = lens _cComment (\ s a -> s{_cComment = a});

-- | The S3 bucket where the responses to the command executions should be
-- stored. This was requested when issuing the command.
cOutputS3BucketName :: Lens' Command (Maybe Text)
cOutputS3BucketName = lens _cOutputS3BucketName (\ s a -> s{_cOutputS3BucketName = a});

-- | The date and time the command was requested.
cRequestedDateTime :: Lens' Command (Maybe UTCTime)
cRequestedDateTime = lens _cRequestedDateTime (\ s a -> s{_cRequestedDateTime = a}) . mapping _Time;

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "Status") <*> (x .:? "ExpiresAfter") <*>
                     (x .:? "OutputS3KeyPrefix")
                     <*> (x .:? "DocumentName")
                     <*> (x .:? "InstanceIds")
                     <*> (x .:? "CommandId")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "Comment")
                     <*> (x .:? "OutputS3BucketName")
                     <*> (x .:? "RequestedDateTime"))

instance Hashable Command

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

instance ToJSON CommandFilter where
        toJSON CommandFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _cfKey), Just ("value" .= _cfValue)])

-- | An invocation is copy of a command sent to a specific instance. A
-- command can apply to one or more instances. A command invocation applies
-- to one instance. For example, if a user executes SendCommand against
-- three instances, then a command invocation is created for each requested
-- instance ID. A command invocation returns status and detail information
-- about a command you executed.
--
-- /See:/ 'commandInvocation' smart constructor.
data CommandInvocation = CommandInvocation'
    { _ciInstanceId        :: !(Maybe Text)
    , _ciStatus            :: !(Maybe CommandInvocationStatus)
    , _ciCommandPlugins    :: !(Maybe [CommandPlugin])
    , _ciDocumentName      :: !(Maybe Text)
    , _ciCommandId         :: !(Maybe Text)
    , _ciComment           :: !(Maybe Text)
    , _ciTraceOutput       :: !(Maybe Text)
    , _ciRequestedDateTime :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommandInvocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciInstanceId'
--
-- * 'ciStatus'
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
commandInvocation
    :: CommandInvocation
commandInvocation =
    CommandInvocation'
    { _ciInstanceId = Nothing
    , _ciStatus = Nothing
    , _ciCommandPlugins = Nothing
    , _ciDocumentName = Nothing
    , _ciCommandId = Nothing
    , _ciComment = Nothing
    , _ciTraceOutput = Nothing
    , _ciRequestedDateTime = Nothing
    }

-- | The instance ID in which this invocation was requested.
ciInstanceId :: Lens' CommandInvocation (Maybe Text)
ciInstanceId = lens _ciInstanceId (\ s a -> s{_ciInstanceId = a});

-- | Whether or not the invocation succeeded, failed, or is pending.
ciStatus :: Lens' CommandInvocation (Maybe CommandInvocationStatus)
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a});

-- | Undocumented member.
ciCommandPlugins :: Lens' CommandInvocation [CommandPlugin]
ciCommandPlugins = lens _ciCommandPlugins (\ s a -> s{_ciCommandPlugins = a}) . _Default . _Coerce;

-- | The document name that was requested for execution.
ciDocumentName :: Lens' CommandInvocation (Maybe Text)
ciDocumentName = lens _ciDocumentName (\ s a -> s{_ciDocumentName = a});

-- | The command against which this invocation was requested.
ciCommandId :: Lens' CommandInvocation (Maybe Text)
ciCommandId = lens _ciCommandId (\ s a -> s{_ciCommandId = a});

-- | User-specified information about the command, such as a brief
-- description of what the command should do.
ciComment :: Lens' CommandInvocation (Maybe Text)
ciComment = lens _ciComment (\ s a -> s{_ciComment = a});

-- | Gets the trace output sent by the agent.
ciTraceOutput :: Lens' CommandInvocation (Maybe Text)
ciTraceOutput = lens _ciTraceOutput (\ s a -> s{_ciTraceOutput = a});

-- | The time and date the request was sent to this instance.
ciRequestedDateTime :: Lens' CommandInvocation (Maybe UTCTime)
ciRequestedDateTime = lens _ciRequestedDateTime (\ s a -> s{_ciRequestedDateTime = a}) . mapping _Time;

instance FromJSON CommandInvocation where
        parseJSON
          = withObject "CommandInvocation"
              (\ x ->
                 CommandInvocation' <$>
                   (x .:? "InstanceId") <*> (x .:? "Status") <*>
                     (x .:? "CommandPlugins" .!= mempty)
                     <*> (x .:? "DocumentName")
                     <*> (x .:? "CommandId")
                     <*> (x .:? "Comment")
                     <*> (x .:? "TraceOutput")
                     <*> (x .:? "RequestedDateTime"))

instance Hashable CommandInvocation

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

-- | The status of this plugin. You can execute a document with multiple
-- plugins.
cpStatus :: Lens' CommandPlugin (Maybe CommandPluginStatus)
cpStatus = lens _cpStatus (\ s a -> s{_cpStatus = a});

-- | The time the plugin started executing.
cpResponseStartDateTime :: Lens' CommandPlugin (Maybe UTCTime)
cpResponseStartDateTime = lens _cpResponseStartDateTime (\ s a -> s{_cpResponseStartDateTime = a}) . mapping _Time;

-- | The S3 directory path inside the bucket where the responses to the
-- command executions should be stored. This was requested when issuing the
-- command.
cpOutputS3KeyPrefix :: Lens' CommandPlugin (Maybe Text)
cpOutputS3KeyPrefix = lens _cpOutputS3KeyPrefix (\ s a -> s{_cpOutputS3KeyPrefix = a});

-- | A numeric response code generated after executing the plugin.
cpResponseCode :: Lens' CommandPlugin (Maybe Int)
cpResponseCode = lens _cpResponseCode (\ s a -> s{_cpResponseCode = a});

-- | Output of the plugin execution.
cpOutput :: Lens' CommandPlugin (Maybe Text)
cpOutput = lens _cpOutput (\ s a -> s{_cpOutput = a});

-- | The name of the plugin. Must be one of the following: aws:updateAgent,
-- aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule,
-- aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
cpName :: Lens' CommandPlugin (Maybe Text)
cpName = lens _cpName (\ s a -> s{_cpName = a});

-- | The S3 bucket where the responses to the command executions should be
-- stored. This was requested when issuing the command.
cpOutputS3BucketName :: Lens' CommandPlugin (Maybe Text)
cpOutputS3BucketName = lens _cpOutputS3BucketName (\ s a -> s{_cpOutputS3BucketName = a});

-- | The time the plugin stopped executing. Could stop prematurely if, for
-- example, a cancel command was sent.
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

instance ToJSON CreateAssociationBatchRequestEntry
         where
        toJSON CreateAssociationBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _cabreInstanceId,
                  ("Name" .=) <$> _cabreName,
                  ("Parameters" .=) <$> _cabreParameters])

-- | Describes an SSM document.
--
-- /See:/ 'documentDescription' smart constructor.
data DocumentDescription = DocumentDescription'
    { _dStatus        :: !(Maybe DocumentStatus)
    , _dSha1          :: !(Maybe Text)
    , _dPlatformTypes :: !(Maybe [PlatformType])
    , _dCreatedDate   :: !(Maybe POSIX)
    , _dName          :: !(Maybe Text)
    , _dParameters    :: !(Maybe [DocumentParameter])
    , _dDescription   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStatus'
--
-- * 'dSha1'
--
-- * 'dPlatformTypes'
--
-- * 'dCreatedDate'
--
-- * 'dName'
--
-- * 'dParameters'
--
-- * 'dDescription'
documentDescription
    :: DocumentDescription
documentDescription =
    DocumentDescription'
    { _dStatus = Nothing
    , _dSha1 = Nothing
    , _dPlatformTypes = Nothing
    , _dCreatedDate = Nothing
    , _dName = Nothing
    , _dParameters = Nothing
    , _dDescription = Nothing
    }

-- | The status of the SSM document.
dStatus :: Lens' DocumentDescription (Maybe DocumentStatus)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | The SHA1 hash of the document, which you can use for verification
-- purposes.
dSha1 :: Lens' DocumentDescription (Maybe Text)
dSha1 = lens _dSha1 (\ s a -> s{_dSha1 = a});

-- | The list of OS platforms compatible with this SSM document.
dPlatformTypes :: Lens' DocumentDescription [PlatformType]
dPlatformTypes = lens _dPlatformTypes (\ s a -> s{_dPlatformTypes = a}) . _Default . _Coerce;

-- | The date when the SSM document was created.
dCreatedDate :: Lens' DocumentDescription (Maybe UTCTime)
dCreatedDate = lens _dCreatedDate (\ s a -> s{_dCreatedDate = a}) . mapping _Time;

-- | The name of the SSM document.
dName :: Lens' DocumentDescription (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a});

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
                   (x .:? "Status") <*> (x .:? "Sha1") <*>
                     (x .:? "PlatformTypes" .!= mempty)
                     <*> (x .:? "CreatedDate")
                     <*> (x .:? "Name")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "Description"))

instance Hashable DocumentDescription

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

instance ToJSON DocumentFilter where
        toJSON DocumentFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _dfKey), Just ("value" .= _dfValue)])

-- | Describes the name of an SSM document.
--
-- /See:/ 'documentIdentifier' smart constructor.
data DocumentIdentifier = DocumentIdentifier'
    { _diPlatformTypes :: !(Maybe [PlatformType])
    , _diName          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diPlatformTypes'
--
-- * 'diName'
documentIdentifier
    :: DocumentIdentifier
documentIdentifier =
    DocumentIdentifier'
    { _diPlatformTypes = Nothing
    , _diName = Nothing
    }

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
                   (x .:? "PlatformTypes" .!= mempty) <*>
                     (x .:? "Name"))

instance Hashable DocumentIdentifier

-- | /See:/ 'documentParameter' smart constructor.
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

-- | If specified, the default values for the parameters. Parameters without
-- a default value are required. Parameters with a default value are
-- optional.
dpDefaultValue :: Lens' DocumentParameter (Maybe Text)
dpDefaultValue = lens _dpDefaultValue (\ s a -> s{_dpDefaultValue = a});

-- | The type of parameter. The type can be either “String” or “StringList”.
dpType :: Lens' DocumentParameter (Maybe DocumentParameterType)
dpType = lens _dpType (\ s a -> s{_dpType = a});

-- | A description of what the parameter does, how to use it, the default
-- value, and whether or not the parameter is optional.
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

-- | Describes a filter for a specific list of instances.
--
-- /See:/ 'instanceInformation' smart constructor.
data InstanceInformation = InstanceInformation'
    { _iiInstanceId       :: !(Maybe Text)
    , _iiPingStatus       :: !(Maybe PingStatus)
    , _iiPlatformVersion  :: !(Maybe Text)
    , _iiIsLatestVersion  :: !(Maybe Bool)
    , _iiAgentVersion     :: !(Maybe Text)
    , _iiLastPingDateTime :: !(Maybe POSIX)
    , _iiPlatformType     :: !(Maybe PlatformType)
    , _iiPlatformName     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiInstanceId'
--
-- * 'iiPingStatus'
--
-- * 'iiPlatformVersion'
--
-- * 'iiIsLatestVersion'
--
-- * 'iiAgentVersion'
--
-- * 'iiLastPingDateTime'
--
-- * 'iiPlatformType'
--
-- * 'iiPlatformName'
instanceInformation
    :: InstanceInformation
instanceInformation =
    InstanceInformation'
    { _iiInstanceId = Nothing
    , _iiPingStatus = Nothing
    , _iiPlatformVersion = Nothing
    , _iiIsLatestVersion = Nothing
    , _iiAgentVersion = Nothing
    , _iiLastPingDateTime = Nothing
    , _iiPlatformType = Nothing
    , _iiPlatformName = Nothing
    }

-- | The instance ID.
iiInstanceId :: Lens' InstanceInformation (Maybe Text)
iiInstanceId = lens _iiInstanceId (\ s a -> s{_iiInstanceId = a});

-- | Connection status of the SSM agent.
iiPingStatus :: Lens' InstanceInformation (Maybe PingStatus)
iiPingStatus = lens _iiPingStatus (\ s a -> s{_iiPingStatus = a});

-- | The version of the OS platform running on your instance.
iiPlatformVersion :: Lens' InstanceInformation (Maybe Text)
iiPlatformVersion = lens _iiPlatformVersion (\ s a -> s{_iiPlatformVersion = a});

-- | Indicates whether latest version of the SSM agent is running on your
-- instance.
iiIsLatestVersion :: Lens' InstanceInformation (Maybe Bool)
iiIsLatestVersion = lens _iiIsLatestVersion (\ s a -> s{_iiIsLatestVersion = a});

-- | The version of the SSM agent running on your instance.
iiAgentVersion :: Lens' InstanceInformation (Maybe Text)
iiAgentVersion = lens _iiAgentVersion (\ s a -> s{_iiAgentVersion = a});

-- | The date and time when agent last pinged SSM service.
iiLastPingDateTime :: Lens' InstanceInformation (Maybe UTCTime)
iiLastPingDateTime = lens _iiLastPingDateTime (\ s a -> s{_iiLastPingDateTime = a}) . mapping _Time;

-- | The operating system platform type.
iiPlatformType :: Lens' InstanceInformation (Maybe PlatformType)
iiPlatformType = lens _iiPlatformType (\ s a -> s{_iiPlatformType = a});

-- | The name of the operating system platform running on your instance.
iiPlatformName :: Lens' InstanceInformation (Maybe Text)
iiPlatformName = lens _iiPlatformName (\ s a -> s{_iiPlatformName = a});

instance FromJSON InstanceInformation where
        parseJSON
          = withObject "InstanceInformation"
              (\ x ->
                 InstanceInformation' <$>
                   (x .:? "InstanceId") <*> (x .:? "PingStatus") <*>
                     (x .:? "PlatformVersion")
                     <*> (x .:? "IsLatestVersion")
                     <*> (x .:? "AgentVersion")
                     <*> (x .:? "LastPingDateTime")
                     <*> (x .:? "PlatformType")
                     <*> (x .:? "PlatformName"))

instance Hashable InstanceInformation

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

instance ToJSON InstanceInformationFilter where
        toJSON InstanceInformationFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _iifKey),
                  Just ("valueSet" .= _iifValueSet)])
