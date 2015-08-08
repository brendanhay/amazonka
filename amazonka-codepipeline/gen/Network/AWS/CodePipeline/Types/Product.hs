{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.Product where

import           Network.AWS.CodePipeline.Types.Sum
import           Network.AWS.Prelude

-- | Represents an AWS session credentials object. These credentials are
-- temporary credentials that are issued by AWS Secure Token Service (STS).
-- They can be used to access input and output artifacts in the Amazon S3
-- bucket used to store artifact for the pipeline in AWS CodePipeline.
--
-- /See:/ 'awsSessionCredentials' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ascAccessKeyId'
--
-- * 'ascSecretAccessKey'
--
-- * 'ascSessionToken'
data AWSSessionCredentials = AWSSessionCredentials'
    { _ascAccessKeyId     :: !Text
    , _ascSecretAccessKey :: !Text
    , _ascSessionToken    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AWSSessionCredentials' smart constructor.
awsSessionCredentials :: Text -> Text -> Text -> AWSSessionCredentials
awsSessionCredentials pAccessKeyId_ pSecretAccessKey_ pSessionToken_ =
    AWSSessionCredentials'
    { _ascAccessKeyId = pAccessKeyId_
    , _ascSecretAccessKey = pSecretAccessKey_
    , _ascSessionToken = pSessionToken_
    }

-- | The access key for the session.
ascAccessKeyId :: Lens' AWSSessionCredentials Text
ascAccessKeyId = lens _ascAccessKeyId (\ s a -> s{_ascAccessKeyId = a});

-- | The secret access key for the session.
ascSecretAccessKey :: Lens' AWSSessionCredentials Text
ascSecretAccessKey = lens _ascSecretAccessKey (\ s a -> s{_ascSecretAccessKey = a});

-- | The token for the session.
ascSessionToken :: Lens' AWSSessionCredentials Text
ascSessionToken = lens _ascSessionToken (\ s a -> s{_ascSessionToken = a});

instance FromJSON AWSSessionCredentials where
        parseJSON
          = withObject "AWSSessionCredentials"
              (\ x ->
                 AWSSessionCredentials' <$>
                   (x .: "accessKeyId") <*> (x .: "secretAccessKey") <*>
                     (x .: "sessionToken"))

-- | Represents information about an action configuration.
--
-- /See:/ 'actionConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acConfiguration'
newtype ActionConfiguration = ActionConfiguration'
    { _acConfiguration :: Maybe (Map Text Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActionConfiguration' smart constructor.
actionConfiguration :: ActionConfiguration
actionConfiguration =
    ActionConfiguration'
    { _acConfiguration = Nothing
    }

-- | The configuration data for the action.
acConfiguration :: Lens' ActionConfiguration (HashMap Text Text)
acConfiguration = lens _acConfiguration (\ s a -> s{_acConfiguration = a}) . _Default . _Map;

instance FromJSON ActionConfiguration where
        parseJSON
          = withObject "ActionConfiguration"
              (\ x ->
                 ActionConfiguration' <$>
                   (x .:? "configuration" .!= mempty))

-- | Represents information about an action configuration property.
--
-- /See:/ 'actionConfigurationProperty' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acpQueryable'
--
-- * 'acpType'
--
-- * 'acpDescription'
--
-- * 'acpName'
--
-- * 'acpRequired'
--
-- * 'acpKey'
--
-- * 'acpSecret'
data ActionConfigurationProperty = ActionConfigurationProperty'
    { _acpQueryable   :: !(Maybe Bool)
    , _acpType        :: !(Maybe ActionConfigurationPropertyType)
    , _acpDescription :: !(Maybe Text)
    , _acpName        :: !Text
    , _acpRequired    :: !Bool
    , _acpKey         :: !Bool
    , _acpSecret      :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActionConfigurationProperty' smart constructor.
actionConfigurationProperty :: Text -> Bool -> Bool -> Bool -> ActionConfigurationProperty
actionConfigurationProperty pName_ pRequired_ pKey_ pSecret_ =
    ActionConfigurationProperty'
    { _acpQueryable = Nothing
    , _acpType = Nothing
    , _acpDescription = Nothing
    , _acpName = pName_
    , _acpRequired = pRequired_
    , _acpKey = pKey_
    , _acpSecret = pSecret_
    }

-- | Indicates that the proprety will be used in conjunction with
-- PollForJobs. When creating a custom action, an action can have up to one
-- queryable property. If it has one, that property must be both required
-- and not secret.
--
-- If you create a pipeline with a custom action type, and that custom
-- action contains a queryable property, the value for that configuration
-- property is subject to additional restrictions. The value must be less
-- than or equal to twenty (20) characters. The value can contain only
-- alphanumeric characters, underscores, and hyphens.
acpQueryable :: Lens' ActionConfigurationProperty (Maybe Bool)
acpQueryable = lens _acpQueryable (\ s a -> s{_acpQueryable = a});

-- | The type of the configuration property.
acpType :: Lens' ActionConfigurationProperty (Maybe ActionConfigurationPropertyType)
acpType = lens _acpType (\ s a -> s{_acpType = a});

-- | The description of the action configuration property that will be
-- displayed to users.
acpDescription :: Lens' ActionConfigurationProperty (Maybe Text)
acpDescription = lens _acpDescription (\ s a -> s{_acpDescription = a});

-- | The name of the action configuration property.
acpName :: Lens' ActionConfigurationProperty Text
acpName = lens _acpName (\ s a -> s{_acpName = a});

-- | Whether the configuration property is a required value.
acpRequired :: Lens' ActionConfigurationProperty Bool
acpRequired = lens _acpRequired (\ s a -> s{_acpRequired = a});

-- | Whether the configuration property is a key.
acpKey :: Lens' ActionConfigurationProperty Bool
acpKey = lens _acpKey (\ s a -> s{_acpKey = a});

-- | Whether the configuration property is secret. Secrets are hidden from
-- all calls except for GetJobDetails, GetThirdPartyJobDetails,
-- PollForJobs, and PollForThirdPartyJobs.
--
-- When updating a pipeline, passing * * * * * without changing any other
-- values of the action will preserve the prior value of the secret.
acpSecret :: Lens' ActionConfigurationProperty Bool
acpSecret = lens _acpSecret (\ s a -> s{_acpSecret = a});

instance FromJSON ActionConfigurationProperty where
        parseJSON
          = withObject "ActionConfigurationProperty"
              (\ x ->
                 ActionConfigurationProperty' <$>
                   (x .:? "queryable") <*> (x .:? "type") <*>
                     (x .:? "description")
                     <*> (x .: "name")
                     <*> (x .: "required")
                     <*> (x .: "key")
                     <*> (x .: "secret"))

instance ToJSON ActionConfigurationProperty where
        toJSON ActionConfigurationProperty'{..}
          = object
              ["queryable" .= _acpQueryable, "type" .= _acpType,
               "description" .= _acpDescription, "name" .= _acpName,
               "required" .= _acpRequired, "key" .= _acpKey,
               "secret" .= _acpSecret]

-- | Represents the context of an action within the stage of a pipeline to a
-- job worker.
--
-- /See:/ 'actionContext' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acName'
newtype ActionContext = ActionContext'
    { _acName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActionContext' smart constructor.
actionContext :: ActionContext
actionContext =
    ActionContext'
    { _acName = Nothing
    }

-- | The name of the action within the context of a job.
acName :: Lens' ActionContext (Maybe Text)
acName = lens _acName (\ s a -> s{_acName = a});

instance FromJSON ActionContext where
        parseJSON
          = withObject "ActionContext"
              (\ x -> ActionContext' <$> (x .:? "name"))

-- | Represents information about an action declaration.
--
-- /See:/ 'actionDeclaration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adOutputArtifacts'
--
-- * 'adRunOrder'
--
-- * 'adConfiguration'
--
-- * 'adInputArtifacts'
--
-- * 'adRoleARN'
--
-- * 'adName'
--
-- * 'adActionTypeId'
data ActionDeclaration = ActionDeclaration'
    { _adOutputArtifacts :: !(Maybe [OutputArtifact])
    , _adRunOrder        :: !(Maybe Nat)
    , _adConfiguration   :: !(Maybe (Map Text Text))
    , _adInputArtifacts  :: !(Maybe [InputArtifact])
    , _adRoleARN         :: !(Maybe Text)
    , _adName            :: !Text
    , _adActionTypeId    :: !ActionTypeId
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActionDeclaration' smart constructor.
actionDeclaration :: Text -> ActionTypeId -> ActionDeclaration
actionDeclaration pName_ pActionTypeId_ =
    ActionDeclaration'
    { _adOutputArtifacts = Nothing
    , _adRunOrder = Nothing
    , _adConfiguration = Nothing
    , _adInputArtifacts = Nothing
    , _adRoleARN = Nothing
    , _adName = pName_
    , _adActionTypeId = pActionTypeId_
    }

-- | The name or ID of the result of the action declaration, such as a test
-- or build artifact.
adOutputArtifacts :: Lens' ActionDeclaration [OutputArtifact]
adOutputArtifacts = lens _adOutputArtifacts (\ s a -> s{_adOutputArtifacts = a}) . _Default . _Coerce;

-- | The order in which actions are run.
adRunOrder :: Lens' ActionDeclaration (Maybe Natural)
adRunOrder = lens _adRunOrder (\ s a -> s{_adRunOrder = a}) . mapping _Nat;

-- | The action declaration\'s configuration.
adConfiguration :: Lens' ActionDeclaration (HashMap Text Text)
adConfiguration = lens _adConfiguration (\ s a -> s{_adConfiguration = a}) . _Default . _Map;

-- | The name or ID of the artifact consumed by the action, such as a test or
-- build artifact.
adInputArtifacts :: Lens' ActionDeclaration [InputArtifact]
adInputArtifacts = lens _adInputArtifacts (\ s a -> s{_adInputArtifacts = a}) . _Default . _Coerce;

-- | The ARN of the IAM service role that will perform the declared action.
-- This is assumed through the roleArn for the pipeline.
adRoleARN :: Lens' ActionDeclaration (Maybe Text)
adRoleARN = lens _adRoleARN (\ s a -> s{_adRoleARN = a});

-- | The action declaration\'s name.
adName :: Lens' ActionDeclaration Text
adName = lens _adName (\ s a -> s{_adName = a});

-- | The configuration information for the action type.
adActionTypeId :: Lens' ActionDeclaration ActionTypeId
adActionTypeId = lens _adActionTypeId (\ s a -> s{_adActionTypeId = a});

instance FromJSON ActionDeclaration where
        parseJSON
          = withObject "ActionDeclaration"
              (\ x ->
                 ActionDeclaration' <$>
                   (x .:? "outputArtifacts" .!= mempty) <*>
                     (x .:? "runOrder")
                     <*> (x .:? "configuration" .!= mempty)
                     <*> (x .:? "inputArtifacts" .!= mempty)
                     <*> (x .:? "roleArn")
                     <*> (x .: "name")
                     <*> (x .: "actionTypeId"))

instance ToJSON ActionDeclaration where
        toJSON ActionDeclaration'{..}
          = object
              ["outputArtifacts" .= _adOutputArtifacts,
               "runOrder" .= _adRunOrder,
               "configuration" .= _adConfiguration,
               "inputArtifacts" .= _adInputArtifacts,
               "roleArn" .= _adRoleARN, "name" .= _adName,
               "actionTypeId" .= _adActionTypeId]

-- | Represents information about how an action runs.
--
-- /See:/ 'actionExecution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aeSummary'
--
-- * 'aeStatus'
--
-- * 'aeLastStatusChange'
--
-- * 'aeExternalExecutionURL'
--
-- * 'aePercentComplete'
--
-- * 'aeErrorDetails'
--
-- * 'aeExternalExecutionId'
data ActionExecution = ActionExecution'
    { _aeSummary              :: !(Maybe Text)
    , _aeStatus               :: !(Maybe ActionExecutionStatus)
    , _aeLastStatusChange     :: !(Maybe POSIX)
    , _aeExternalExecutionURL :: !(Maybe Text)
    , _aePercentComplete      :: !(Maybe Nat)
    , _aeErrorDetails         :: !(Maybe ErrorDetails)
    , _aeExternalExecutionId  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActionExecution' smart constructor.
actionExecution :: ActionExecution
actionExecution =
    ActionExecution'
    { _aeSummary = Nothing
    , _aeStatus = Nothing
    , _aeLastStatusChange = Nothing
    , _aeExternalExecutionURL = Nothing
    , _aePercentComplete = Nothing
    , _aeErrorDetails = Nothing
    , _aeExternalExecutionId = Nothing
    }

-- | A summary of the run of the action.
aeSummary :: Lens' ActionExecution (Maybe Text)
aeSummary = lens _aeSummary (\ s a -> s{_aeSummary = a});

-- | The status of the action, or for a completed action, the last status of
-- the action.
aeStatus :: Lens' ActionExecution (Maybe ActionExecutionStatus)
aeStatus = lens _aeStatus (\ s a -> s{_aeStatus = a});

-- | The last status change of the action.
aeLastStatusChange :: Lens' ActionExecution (Maybe UTCTime)
aeLastStatusChange = lens _aeLastStatusChange (\ s a -> s{_aeLastStatusChange = a}) . mapping _Time;

-- | The URL of a resource external to AWS that will be used when running the
-- action, for example an external repository URL.
aeExternalExecutionURL :: Lens' ActionExecution (Maybe Text)
aeExternalExecutionURL = lens _aeExternalExecutionURL (\ s a -> s{_aeExternalExecutionURL = a});

-- | A percentage of completeness of the action as it runs.
aePercentComplete :: Lens' ActionExecution (Maybe Natural)
aePercentComplete = lens _aePercentComplete (\ s a -> s{_aePercentComplete = a}) . mapping _Nat;

-- | The details of an error returned by a URL external to AWS.
aeErrorDetails :: Lens' ActionExecution (Maybe ErrorDetails)
aeErrorDetails = lens _aeErrorDetails (\ s a -> s{_aeErrorDetails = a});

-- | The external ID of the run of the action.
aeExternalExecutionId :: Lens' ActionExecution (Maybe Text)
aeExternalExecutionId = lens _aeExternalExecutionId (\ s a -> s{_aeExternalExecutionId = a});

instance FromJSON ActionExecution where
        parseJSON
          = withObject "ActionExecution"
              (\ x ->
                 ActionExecution' <$>
                   (x .:? "summary") <*> (x .:? "status") <*>
                     (x .:? "lastStatusChange")
                     <*> (x .:? "externalExecutionUrl")
                     <*> (x .:? "percentComplete")
                     <*> (x .:? "errorDetails")
                     <*> (x .:? "externalExecutionId"))

-- | Represents information about the version (or revision) of an action.
--
-- /See:/ 'actionRevision' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arRevisionChangeId'
--
-- * 'arRevisionId'
--
-- * 'arCreated'
data ActionRevision = ActionRevision'
    { _arRevisionChangeId :: !(Maybe Text)
    , _arRevisionId       :: !Text
    , _arCreated          :: !POSIX
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActionRevision' smart constructor.
actionRevision :: Text -> UTCTime -> ActionRevision
actionRevision pRevisionId_ pCreated_ =
    ActionRevision'
    { _arRevisionChangeId = Nothing
    , _arRevisionId = pRevisionId_
    , _arCreated = _Time # pCreated_
    }

-- | The unique identifier of the change that set the state to this revision,
-- for example a deployment ID or timestamp.
arRevisionChangeId :: Lens' ActionRevision (Maybe Text)
arRevisionChangeId = lens _arRevisionChangeId (\ s a -> s{_arRevisionChangeId = a});

-- | The system-generated unique ID that identifies the revision number of
-- the action.
arRevisionId :: Lens' ActionRevision Text
arRevisionId = lens _arRevisionId (\ s a -> s{_arRevisionId = a});

-- | The date and time when the most recent version of the action was
-- created, in timestamp format.
arCreated :: Lens' ActionRevision UTCTime
arCreated = lens _arCreated (\ s a -> s{_arCreated = a}) . _Time;

instance FromJSON ActionRevision where
        parseJSON
          = withObject "ActionRevision"
              (\ x ->
                 ActionRevision' <$>
                   (x .:? "revisionChangeId") <*> (x .: "revisionId")
                     <*> (x .: "created"))

instance ToJSON ActionRevision where
        toJSON ActionRevision'{..}
          = object
              ["revisionChangeId" .= _arRevisionChangeId,
               "revisionId" .= _arRevisionId,
               "created" .= _arCreated]

-- | Represents information about the state of an action.
--
-- /See:/ 'actionState' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asEntityURL'
--
-- * 'asRevisionURL'
--
-- * 'asActionName'
--
-- * 'asCurrentRevision'
--
-- * 'asLatestExecution'
data ActionState = ActionState'
    { _asEntityURL       :: !(Maybe Text)
    , _asRevisionURL     :: !(Maybe Text)
    , _asActionName      :: !(Maybe Text)
    , _asCurrentRevision :: !(Maybe ActionRevision)
    , _asLatestExecution :: !(Maybe ActionExecution)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActionState' smart constructor.
actionState :: ActionState
actionState =
    ActionState'
    { _asEntityURL = Nothing
    , _asRevisionURL = Nothing
    , _asActionName = Nothing
    , _asCurrentRevision = Nothing
    , _asLatestExecution = Nothing
    }

-- | A URL link for more information about the state of the action, such as a
-- deployment group details page.
asEntityURL :: Lens' ActionState (Maybe Text)
asEntityURL = lens _asEntityURL (\ s a -> s{_asEntityURL = a});

-- | A URL link for more information about the revision, such as a commit
-- details page.
asRevisionURL :: Lens' ActionState (Maybe Text)
asRevisionURL = lens _asRevisionURL (\ s a -> s{_asRevisionURL = a});

-- | The name of the action.
asActionName :: Lens' ActionState (Maybe Text)
asActionName = lens _asActionName (\ s a -> s{_asActionName = a});

-- | Undocumented member.
asCurrentRevision :: Lens' ActionState (Maybe ActionRevision)
asCurrentRevision = lens _asCurrentRevision (\ s a -> s{_asCurrentRevision = a});

-- | Undocumented member.
asLatestExecution :: Lens' ActionState (Maybe ActionExecution)
asLatestExecution = lens _asLatestExecution (\ s a -> s{_asLatestExecution = a});

instance FromJSON ActionState where
        parseJSON
          = withObject "ActionState"
              (\ x ->
                 ActionState' <$>
                   (x .:? "entityUrl") <*> (x .:? "revisionUrl") <*>
                     (x .:? "actionName")
                     <*> (x .:? "currentRevision")
                     <*> (x .:? "latestExecution"))

-- | Returns information about the details of an action type.
--
-- /See:/ 'actionType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atSettings'
--
-- * 'atActionConfigurationProperties'
--
-- * 'atId'
--
-- * 'atInputArtifactDetails'
--
-- * 'atOutputArtifactDetails'
data ActionType = ActionType'
    { _atSettings                      :: !(Maybe ActionTypeSettings)
    , _atActionConfigurationProperties :: !(Maybe [ActionConfigurationProperty])
    , _atId                            :: !ActionTypeId
    , _atInputArtifactDetails          :: !ArtifactDetails
    , _atOutputArtifactDetails         :: !ArtifactDetails
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActionType' smart constructor.
actionType :: ActionTypeId -> ArtifactDetails -> ArtifactDetails -> ActionType
actionType pId_ pInputArtifactDetails_ pOutputArtifactDetails_ =
    ActionType'
    { _atSettings = Nothing
    , _atActionConfigurationProperties = Nothing
    , _atId = pId_
    , _atInputArtifactDetails = pInputArtifactDetails_
    , _atOutputArtifactDetails = pOutputArtifactDetails_
    }

-- | The settings for the action type.
atSettings :: Lens' ActionType (Maybe ActionTypeSettings)
atSettings = lens _atSettings (\ s a -> s{_atSettings = a});

-- | The configuration properties for the action type.
atActionConfigurationProperties :: Lens' ActionType [ActionConfigurationProperty]
atActionConfigurationProperties = lens _atActionConfigurationProperties (\ s a -> s{_atActionConfigurationProperties = a}) . _Default . _Coerce;

-- | Undocumented member.
atId :: Lens' ActionType ActionTypeId
atId = lens _atId (\ s a -> s{_atId = a});

-- | The details of the input artifact for the action, such as its commit ID.
atInputArtifactDetails :: Lens' ActionType ArtifactDetails
atInputArtifactDetails = lens _atInputArtifactDetails (\ s a -> s{_atInputArtifactDetails = a});

-- | The details of the output artifact of the action, such as its commit ID.
atOutputArtifactDetails :: Lens' ActionType ArtifactDetails
atOutputArtifactDetails = lens _atOutputArtifactDetails (\ s a -> s{_atOutputArtifactDetails = a});

instance FromJSON ActionType where
        parseJSON
          = withObject "ActionType"
              (\ x ->
                 ActionType' <$>
                   (x .:? "settings") <*>
                     (x .:? "actionConfigurationProperties" .!= mempty)
                     <*> (x .: "id")
                     <*> (x .: "inputArtifactDetails")
                     <*> (x .: "outputArtifactDetails"))

-- | Represents information about an action type.
--
-- /See:/ 'actionTypeId' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atiCategory'
--
-- * 'atiOwner'
--
-- * 'atiProvider'
--
-- * 'atiVersion'
data ActionTypeId = ActionTypeId'
    { _atiCategory :: !ActionCategory
    , _atiOwner    :: !ActionOwner
    , _atiProvider :: !Text
    , _atiVersion  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActionTypeId' smart constructor.
actionTypeId :: ActionCategory -> ActionOwner -> Text -> Text -> ActionTypeId
actionTypeId pCategory_ pOwner_ pProvider_ pVersion_ =
    ActionTypeId'
    { _atiCategory = pCategory_
    , _atiOwner = pOwner_
    , _atiProvider = pProvider_
    , _atiVersion = pVersion_
    }

-- | A category defines what kind of action can be taken in the stage, and
-- constrains the provider type for the action. Valid categories are
-- limited to one of the values below.
atiCategory :: Lens' ActionTypeId ActionCategory
atiCategory = lens _atiCategory (\ s a -> s{_atiCategory = a});

-- | The creator of the action being called.
atiOwner :: Lens' ActionTypeId ActionOwner
atiOwner = lens _atiOwner (\ s a -> s{_atiOwner = a});

-- | The provider of the service being called by the action. Valid providers
-- are determined by the action category. For example, an action in the
-- Deploy category type might have a provider of AWS CodeDeploy, which
-- would be specified as CodeDeploy.
atiProvider :: Lens' ActionTypeId Text
atiProvider = lens _atiProvider (\ s a -> s{_atiProvider = a});

-- | A string that identifies the action type.
atiVersion :: Lens' ActionTypeId Text
atiVersion = lens _atiVersion (\ s a -> s{_atiVersion = a});

instance FromJSON ActionTypeId where
        parseJSON
          = withObject "ActionTypeId"
              (\ x ->
                 ActionTypeId' <$>
                   (x .: "category") <*> (x .: "owner") <*>
                     (x .: "provider")
                     <*> (x .: "version"))

instance ToJSON ActionTypeId where
        toJSON ActionTypeId'{..}
          = object
              ["category" .= _atiCategory, "owner" .= _atiOwner,
               "provider" .= _atiProvider, "version" .= _atiVersion]

-- | Returns information about the settings for an action type.
--
-- /See:/ 'actionTypeSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atsThirdPartyConfigurationURL'
--
-- * 'atsExecutionURLTemplate'
--
-- * 'atsEntityURLTemplate'
--
-- * 'atsRevisionURLTemplate'
data ActionTypeSettings = ActionTypeSettings'
    { _atsThirdPartyConfigurationURL :: !(Maybe Text)
    , _atsExecutionURLTemplate       :: !(Maybe Text)
    , _atsEntityURLTemplate          :: !(Maybe Text)
    , _atsRevisionURLTemplate        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActionTypeSettings' smart constructor.
actionTypeSettings :: ActionTypeSettings
actionTypeSettings =
    ActionTypeSettings'
    { _atsThirdPartyConfigurationURL = Nothing
    , _atsExecutionURLTemplate = Nothing
    , _atsEntityURLTemplate = Nothing
    , _atsRevisionURLTemplate = Nothing
    }

-- | The URL of a sign-up page where users can sign up for an external
-- service and perform initial configuration of the action provided by that
-- service.
atsThirdPartyConfigurationURL :: Lens' ActionTypeSettings (Maybe Text)
atsThirdPartyConfigurationURL = lens _atsThirdPartyConfigurationURL (\ s a -> s{_atsThirdPartyConfigurationURL = a});

-- | The URL returned to the AWS CodePipeline console that contains a link to
-- the top-level landing page for the external system, such as console page
-- for AWS CodeDeploy. This link is shown on the pipeline view page in the
-- AWS CodePipeline console and provides a link to the execution entity of
-- the external action.
atsExecutionURLTemplate :: Lens' ActionTypeSettings (Maybe Text)
atsExecutionURLTemplate = lens _atsExecutionURLTemplate (\ s a -> s{_atsExecutionURLTemplate = a});

-- | The URL returned to the AWS CodePipeline console that provides a deep
-- link to the resources of the external system, such as the configuration
-- page for an AWS CodeDeploy deployment group. This link is provided as
-- part of the action display within the pipeline.
atsEntityURLTemplate :: Lens' ActionTypeSettings (Maybe Text)
atsEntityURLTemplate = lens _atsEntityURLTemplate (\ s a -> s{_atsEntityURLTemplate = a});

-- | The URL returned to the AWS CodePipeline console that contains a link to
-- the page where customers can update or change the configuration of the
-- external action.
atsRevisionURLTemplate :: Lens' ActionTypeSettings (Maybe Text)
atsRevisionURLTemplate = lens _atsRevisionURLTemplate (\ s a -> s{_atsRevisionURLTemplate = a});

instance FromJSON ActionTypeSettings where
        parseJSON
          = withObject "ActionTypeSettings"
              (\ x ->
                 ActionTypeSettings' <$>
                   (x .:? "thirdPartyConfigurationUrl") <*>
                     (x .:? "executionUrlTemplate")
                     <*> (x .:? "entityUrlTemplate")
                     <*> (x .:? "revisionUrlTemplate"))

instance ToJSON ActionTypeSettings where
        toJSON ActionTypeSettings'{..}
          = object
              ["thirdPartyConfigurationUrl" .=
                 _atsThirdPartyConfigurationURL,
               "executionUrlTemplate" .= _atsExecutionURLTemplate,
               "entityUrlTemplate" .= _atsEntityURLTemplate,
               "revisionUrlTemplate" .= _atsRevisionURLTemplate]

-- | Represents information about an artifact that will be worked upon by
-- actions in the pipeline.
--
-- /See:/ 'artifact' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aLocation'
--
-- * 'aName'
--
-- * 'aRevision'
data Artifact = Artifact'
    { _aLocation :: !(Maybe ArtifactLocation)
    , _aName     :: !(Maybe Text)
    , _aRevision :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Artifact' smart constructor.
artifact :: Artifact
artifact =
    Artifact'
    { _aLocation = Nothing
    , _aName = Nothing
    , _aRevision = Nothing
    }

-- | The location of an artifact.
aLocation :: Lens' Artifact (Maybe ArtifactLocation)
aLocation = lens _aLocation (\ s a -> s{_aLocation = a});

-- | The artifact\'s name.
aName :: Lens' Artifact (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | The artifact\'s revision ID. Depending on the type of object, this could
-- be a commit ID (GitHub) or a revision ID (Amazon S3).
aRevision :: Lens' Artifact (Maybe Text)
aRevision = lens _aRevision (\ s a -> s{_aRevision = a});

instance FromJSON Artifact where
        parseJSON
          = withObject "Artifact"
              (\ x ->
                 Artifact' <$>
                   (x .:? "location") <*> (x .:? "name") <*>
                     (x .:? "revision"))

-- | Returns information about the details of an artifact.
--
-- /See:/ 'artifactDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adMinimumCount'
--
-- * 'adMaximumCount'
data ArtifactDetails = ArtifactDetails'
    { _adMinimumCount :: !Nat
    , _adMaximumCount :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ArtifactDetails' smart constructor.
artifactDetails :: Natural -> Natural -> ArtifactDetails
artifactDetails pMinimumCount_ pMaximumCount_ =
    ArtifactDetails'
    { _adMinimumCount = _Nat # pMinimumCount_
    , _adMaximumCount = _Nat # pMaximumCount_
    }

-- | The minimum number of artifacts allowed for the action type.
adMinimumCount :: Lens' ArtifactDetails Natural
adMinimumCount = lens _adMinimumCount (\ s a -> s{_adMinimumCount = a}) . _Nat;

-- | The maximum number of artifacts allowed for the action type.
adMaximumCount :: Lens' ArtifactDetails Natural
adMaximumCount = lens _adMaximumCount (\ s a -> s{_adMaximumCount = a}) . _Nat;

instance FromJSON ArtifactDetails where
        parseJSON
          = withObject "ArtifactDetails"
              (\ x ->
                 ArtifactDetails' <$>
                   (x .: "minimumCount") <*> (x .: "maximumCount"))

instance ToJSON ArtifactDetails where
        toJSON ArtifactDetails'{..}
          = object
              ["minimumCount" .= _adMinimumCount,
               "maximumCount" .= _adMaximumCount]

-- | Represents information about the location of an artifact.
--
-- /See:/ 'artifactLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'alS3Location'
--
-- * 'alType'
data ArtifactLocation = ArtifactLocation'
    { _alS3Location :: !(Maybe S3ArtifactLocation)
    , _alType       :: !(Maybe ArtifactLocationType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ArtifactLocation' smart constructor.
artifactLocation :: ArtifactLocation
artifactLocation =
    ArtifactLocation'
    { _alS3Location = Nothing
    , _alType = Nothing
    }

-- | The Amazon S3 bucket that contains the artifact.
alS3Location :: Lens' ArtifactLocation (Maybe S3ArtifactLocation)
alS3Location = lens _alS3Location (\ s a -> s{_alS3Location = a});

-- | The type of artifact in the location.
alType :: Lens' ArtifactLocation (Maybe ArtifactLocationType)
alType = lens _alType (\ s a -> s{_alType = a});

instance FromJSON ArtifactLocation where
        parseJSON
          = withObject "ArtifactLocation"
              (\ x ->
                 ArtifactLocation' <$>
                   (x .:? "s3Location") <*> (x .:? "type"))

-- | The Amazon S3 location where artifacts are stored for the pipeline. If
-- this Amazon S3 bucket is created manually, it must meet the requirements
-- for AWS CodePipeline. For more information, see the Concepts.
--
-- /See:/ 'artifactStore' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asType'
--
-- * 'asLocation'
data ArtifactStore = ArtifactStore'
    { _asType     :: !ArtifactStoreType
    , _asLocation :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ArtifactStore' smart constructor.
artifactStore :: ArtifactStoreType -> Text -> ArtifactStore
artifactStore pType_ pLocation_ =
    ArtifactStore'
    { _asType = pType_
    , _asLocation = pLocation_
    }

-- | The type of the artifact store, such as S3.
asType :: Lens' ArtifactStore ArtifactStoreType
asType = lens _asType (\ s a -> s{_asType = a});

-- | The location for storing the artifacts for a pipeline, such as an S3
-- bucket or folder.
asLocation :: Lens' ArtifactStore Text
asLocation = lens _asLocation (\ s a -> s{_asLocation = a});

instance FromJSON ArtifactStore where
        parseJSON
          = withObject "ArtifactStore"
              (\ x ->
                 ArtifactStore' <$>
                   (x .: "type") <*> (x .: "location"))

instance ToJSON ArtifactStore where
        toJSON ArtifactStore'{..}
          = object
              ["type" .= _asType, "location" .= _asLocation]

-- | Represents information about a gate declaration.
--
-- /See:/ 'blockerDeclaration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bdName'
--
-- * 'bdType'
data BlockerDeclaration = BlockerDeclaration'
    { _bdName :: !Text
    , _bdType :: !BlockerType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BlockerDeclaration' smart constructor.
blockerDeclaration :: Text -> BlockerType -> BlockerDeclaration
blockerDeclaration pName_ pType_ =
    BlockerDeclaration'
    { _bdName = pName_
    , _bdType = pType_
    }

-- | The name of the gate declaration.
bdName :: Lens' BlockerDeclaration Text
bdName = lens _bdName (\ s a -> s{_bdName = a});

-- | The type of the gate declaration.
bdType :: Lens' BlockerDeclaration BlockerType
bdType = lens _bdType (\ s a -> s{_bdType = a});

instance FromJSON BlockerDeclaration where
        parseJSON
          = withObject "BlockerDeclaration"
              (\ x ->
                 BlockerDeclaration' <$>
                   (x .: "name") <*> (x .: "type"))

instance ToJSON BlockerDeclaration where
        toJSON BlockerDeclaration'{..}
          = object ["name" .= _bdName, "type" .= _bdType]

-- | Represents information about a current revision.
--
-- /See:/ 'currentRevision' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crRevision'
--
-- * 'crChangeIdentifier'
data CurrentRevision = CurrentRevision'
    { _crRevision         :: !Text
    , _crChangeIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CurrentRevision' smart constructor.
currentRevision :: Text -> Text -> CurrentRevision
currentRevision pRevision_ pChangeIdentifier_ =
    CurrentRevision'
    { _crRevision = pRevision_
    , _crChangeIdentifier = pChangeIdentifier_
    }

-- | The revision ID of the current version of an artifact.
crRevision :: Lens' CurrentRevision Text
crRevision = lens _crRevision (\ s a -> s{_crRevision = a});

-- | The change identifier for the current revision.
crChangeIdentifier :: Lens' CurrentRevision Text
crChangeIdentifier = lens _crChangeIdentifier (\ s a -> s{_crChangeIdentifier = a});

instance ToJSON CurrentRevision where
        toJSON CurrentRevision'{..}
          = object
              ["revision" .= _crRevision,
               "changeIdentifier" .= _crChangeIdentifier]

-- | Represents information about an error in AWS CodePipeline.
--
-- /See:/ 'errorDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edCode'
--
-- * 'edMessage'
data ErrorDetails = ErrorDetails'
    { _edCode    :: !(Maybe Text)
    , _edMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ErrorDetails' smart constructor.
errorDetails :: ErrorDetails
errorDetails =
    ErrorDetails'
    { _edCode = Nothing
    , _edMessage = Nothing
    }

-- | The system ID or error number code of the error.
edCode :: Lens' ErrorDetails (Maybe Text)
edCode = lens _edCode (\ s a -> s{_edCode = a});

-- | The text of the error message.
edMessage :: Lens' ErrorDetails (Maybe Text)
edMessage = lens _edMessage (\ s a -> s{_edMessage = a});

instance FromJSON ErrorDetails where
        parseJSON
          = withObject "ErrorDetails"
              (\ x ->
                 ErrorDetails' <$>
                   (x .:? "code") <*> (x .:? "message"))

-- | The details of the actions taken and results produced on an artifact as
-- it passes through stages in the pipeline.
--
-- /See:/ 'executionDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edSummary'
--
-- * 'edPercentComplete'
--
-- * 'edExternalExecutionId'
data ExecutionDetails = ExecutionDetails'
    { _edSummary             :: !(Maybe Text)
    , _edPercentComplete     :: !(Maybe Nat)
    , _edExternalExecutionId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ExecutionDetails' smart constructor.
executionDetails :: ExecutionDetails
executionDetails =
    ExecutionDetails'
    { _edSummary = Nothing
    , _edPercentComplete = Nothing
    , _edExternalExecutionId = Nothing
    }

-- | The summary of the current status of the actions.
edSummary :: Lens' ExecutionDetails (Maybe Text)
edSummary = lens _edSummary (\ s a -> s{_edSummary = a});

-- | The percentage of work completed on the action, represented on a scale
-- of zero to one hundred percent.
edPercentComplete :: Lens' ExecutionDetails (Maybe Natural)
edPercentComplete = lens _edPercentComplete (\ s a -> s{_edPercentComplete = a}) . mapping _Nat;

-- | The system-generated unique ID of this action used to identify this job
-- worker in any external systems, such as AWS CodeDeploy.
edExternalExecutionId :: Lens' ExecutionDetails (Maybe Text)
edExternalExecutionId = lens _edExternalExecutionId (\ s a -> s{_edExternalExecutionId = a});

instance ToJSON ExecutionDetails where
        toJSON ExecutionDetails'{..}
          = object
              ["summary" .= _edSummary,
               "percentComplete" .= _edPercentComplete,
               "externalExecutionId" .= _edExternalExecutionId]

-- | Represents information about failure details.
--
-- /See:/ 'failureDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fdExternalExecutionId'
--
-- * 'fdMessage'
--
-- * 'fdType'
data FailureDetails = FailureDetails'
    { _fdExternalExecutionId :: !(Maybe Text)
    , _fdMessage             :: !(Maybe Text)
    , _fdType                :: !FailureType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'FailureDetails' smart constructor.
failureDetails :: FailureType -> FailureDetails
failureDetails pType_ =
    FailureDetails'
    { _fdExternalExecutionId = Nothing
    , _fdMessage = Nothing
    , _fdType = pType_
    }

-- | The external ID of the run of the action that failed.
fdExternalExecutionId :: Lens' FailureDetails (Maybe Text)
fdExternalExecutionId = lens _fdExternalExecutionId (\ s a -> s{_fdExternalExecutionId = a});

-- | The message about the failure.
fdMessage :: Lens' FailureDetails (Maybe Text)
fdMessage = lens _fdMessage (\ s a -> s{_fdMessage = a});

-- | The type of the failure.
fdType :: Lens' FailureDetails FailureType
fdType = lens _fdType (\ s a -> s{_fdType = a});

instance ToJSON FailureDetails where
        toJSON FailureDetails'{..}
          = object
              ["externalExecutionId" .= _fdExternalExecutionId,
               "message" .= _fdMessage, "type" .= _fdType]

-- | Represents information about an artifact to be worked on, such as a test
-- or build artifact.
--
-- /See:/ 'inputArtifact' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iaName'
newtype InputArtifact = InputArtifact'
    { _iaName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InputArtifact' smart constructor.
inputArtifact :: Text -> InputArtifact
inputArtifact pName_ =
    InputArtifact'
    { _iaName = pName_
    }

-- | The name of the artifact to be worked on, for example, \"My App\".
--
-- The input artifact of an action must exactly match the output artifact
-- declared in a preceding action, but the input artifact does not have to
-- be the next action in strict sequence from the action that provided the
-- output artifact. Actions in parallel can declare different output
-- artifacts, which are in turn consumed by different following actions.
iaName :: Lens' InputArtifact Text
iaName = lens _iaName (\ s a -> s{_iaName = a});

instance FromJSON InputArtifact where
        parseJSON
          = withObject "InputArtifact"
              (\ x -> InputArtifact' <$> (x .: "name"))

instance ToJSON InputArtifact where
        toJSON InputArtifact'{..}
          = object ["name" .= _iaName]

-- | Represents information about a job.
--
-- /See:/ 'job' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jData'
--
-- * 'jAccountId'
--
-- * 'jId'
--
-- * 'jNonce'
data Job = Job'
    { _jData      :: !(Maybe JobData)
    , _jAccountId :: !(Maybe Text)
    , _jId        :: !(Maybe Text)
    , _jNonce     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Job' smart constructor.
job :: Job
job =
    Job'
    { _jData = Nothing
    , _jAccountId = Nothing
    , _jId = Nothing
    , _jNonce = Nothing
    }

-- | Additional data about a job.
jData :: Lens' Job (Maybe JobData)
jData = lens _jData (\ s a -> s{_jData = a});

-- | The ID of the AWS account to use when performing the job.
jAccountId :: Lens' Job (Maybe Text)
jAccountId = lens _jAccountId (\ s a -> s{_jAccountId = a});

-- | The unique system-generated ID of the job.
jId :: Lens' Job (Maybe Text)
jId = lens _jId (\ s a -> s{_jId = a});

-- | A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. This number must
-- be returned in the response.
jNonce :: Lens' Job (Maybe Text)
jNonce = lens _jNonce (\ s a -> s{_jNonce = a});

instance FromJSON Job where
        parseJSON
          = withObject "Job"
              (\ x ->
                 Job' <$>
                   (x .:? "data") <*> (x .:? "accountId") <*>
                     (x .:? "id")
                     <*> (x .:? "nonce"))

-- | Represents additional information about a job required for a job worker
-- to complete the job.
--
-- /See:/ 'jobData' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jdContinuationToken'
--
-- * 'jdOutputArtifacts'
--
-- * 'jdArtifactCredentials'
--
-- * 'jdPipelineContext'
--
-- * 'jdActionTypeId'
--
-- * 'jdInputArtifacts'
--
-- * 'jdActionConfiguration'
data JobData = JobData'
    { _jdContinuationToken   :: !(Maybe Text)
    , _jdOutputArtifacts     :: !(Maybe [Artifact])
    , _jdArtifactCredentials :: !(Maybe (Sensitive AWSSessionCredentials))
    , _jdPipelineContext     :: !(Maybe PipelineContext)
    , _jdActionTypeId        :: !(Maybe ActionTypeId)
    , _jdInputArtifacts      :: !(Maybe [Artifact])
    , _jdActionConfiguration :: !(Maybe ActionConfiguration)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'JobData' smart constructor.
jobData :: JobData
jobData =
    JobData'
    { _jdContinuationToken = Nothing
    , _jdOutputArtifacts = Nothing
    , _jdArtifactCredentials = Nothing
    , _jdPipelineContext = Nothing
    , _jdActionTypeId = Nothing
    , _jdInputArtifacts = Nothing
    , _jdActionConfiguration = Nothing
    }

-- | A system-generated token, such as a AWS CodeDeploy deployment ID, that a
-- job requires in order to continue the job asynchronously.
jdContinuationToken :: Lens' JobData (Maybe Text)
jdContinuationToken = lens _jdContinuationToken (\ s a -> s{_jdContinuationToken = a});

-- | The output of the job.
jdOutputArtifacts :: Lens' JobData [Artifact]
jdOutputArtifacts = lens _jdOutputArtifacts (\ s a -> s{_jdOutputArtifacts = a}) . _Default . _Coerce;

-- | Undocumented member.
jdArtifactCredentials :: Lens' JobData (Maybe AWSSessionCredentials)
jdArtifactCredentials = lens _jdArtifactCredentials (\ s a -> s{_jdArtifactCredentials = a}) . mapping _Sensitive;

-- | Undocumented member.
jdPipelineContext :: Lens' JobData (Maybe PipelineContext)
jdPipelineContext = lens _jdPipelineContext (\ s a -> s{_jdPipelineContext = a});

-- | Undocumented member.
jdActionTypeId :: Lens' JobData (Maybe ActionTypeId)
jdActionTypeId = lens _jdActionTypeId (\ s a -> s{_jdActionTypeId = a});

-- | The artifact supplied to the job.
jdInputArtifacts :: Lens' JobData [Artifact]
jdInputArtifacts = lens _jdInputArtifacts (\ s a -> s{_jdInputArtifacts = a}) . _Default . _Coerce;

-- | Undocumented member.
jdActionConfiguration :: Lens' JobData (Maybe ActionConfiguration)
jdActionConfiguration = lens _jdActionConfiguration (\ s a -> s{_jdActionConfiguration = a});

instance FromJSON JobData where
        parseJSON
          = withObject "JobData"
              (\ x ->
                 JobData' <$>
                   (x .:? "continuationToken") <*>
                     (x .:? "outputArtifacts" .!= mempty)
                     <*> (x .:? "artifactCredentials")
                     <*> (x .:? "pipelineContext")
                     <*> (x .:? "actionTypeId")
                     <*> (x .:? "inputArtifacts" .!= mempty)
                     <*> (x .:? "actionConfiguration"))

-- | Represents information about the details of a job.
--
-- /See:/ 'jobDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jdData'
--
-- * 'jdAccountId'
--
-- * 'jdId'
data JobDetails = JobDetails'
    { _jdData      :: !(Maybe JobData)
    , _jdAccountId :: !(Maybe Text)
    , _jdId        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'JobDetails' smart constructor.
jobDetails :: JobDetails
jobDetails =
    JobDetails'
    { _jdData = Nothing
    , _jdAccountId = Nothing
    , _jdId = Nothing
    }

-- | Undocumented member.
jdData :: Lens' JobDetails (Maybe JobData)
jdData = lens _jdData (\ s a -> s{_jdData = a});

-- | The AWS account ID associated with the job.
jdAccountId :: Lens' JobDetails (Maybe Text)
jdAccountId = lens _jdAccountId (\ s a -> s{_jdAccountId = a});

-- | The unique system-generated ID of the job.
jdId :: Lens' JobDetails (Maybe Text)
jdId = lens _jdId (\ s a -> s{_jdId = a});

instance FromJSON JobDetails where
        parseJSON
          = withObject "JobDetails"
              (\ x ->
                 JobDetails' <$>
                   (x .:? "data") <*> (x .:? "accountId") <*>
                     (x .:? "id"))

-- | Represents information about the output of an action.
--
-- /See:/ 'outputArtifact' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oaName'
newtype OutputArtifact = OutputArtifact'
    { _oaName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'OutputArtifact' smart constructor.
outputArtifact :: Text -> OutputArtifact
outputArtifact pName_ =
    OutputArtifact'
    { _oaName = pName_
    }

-- | The name of the output of an artifact, such as \"My App\".
--
-- The input artifact of an action must exactly match the output artifact
-- declared in a preceding action, but the input artifact does not have to
-- be the next action in strict sequence from the action that provided the
-- output artifact. Actions in parallel can declare different output
-- artifacts, which are in turn consumed by different following actions.
--
-- Output artifact names must be unique within a pipeline.
oaName :: Lens' OutputArtifact Text
oaName = lens _oaName (\ s a -> s{_oaName = a});

instance FromJSON OutputArtifact where
        parseJSON
          = withObject "OutputArtifact"
              (\ x -> OutputArtifact' <$> (x .: "name"))

instance ToJSON OutputArtifact where
        toJSON OutputArtifact'{..}
          = object ["name" .= _oaName]

-- | Represents information about a pipeline to a job worker.
--
-- /See:/ 'pipelineContext' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pcStage'
--
-- * 'pcPipelineName'
--
-- * 'pcAction'
data PipelineContext = PipelineContext'
    { _pcStage        :: !(Maybe StageContext)
    , _pcPipelineName :: !(Maybe Text)
    , _pcAction       :: !(Maybe ActionContext)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PipelineContext' smart constructor.
pipelineContext :: PipelineContext
pipelineContext =
    PipelineContext'
    { _pcStage = Nothing
    , _pcPipelineName = Nothing
    , _pcAction = Nothing
    }

-- | The stage of the pipeline.
pcStage :: Lens' PipelineContext (Maybe StageContext)
pcStage = lens _pcStage (\ s a -> s{_pcStage = a});

-- | The name of the pipeline. This is a user-specified value. Pipeline names
-- must be unique across all pipeline names under an Amazon Web Services
-- account.
pcPipelineName :: Lens' PipelineContext (Maybe Text)
pcPipelineName = lens _pcPipelineName (\ s a -> s{_pcPipelineName = a});

-- | Undocumented member.
pcAction :: Lens' PipelineContext (Maybe ActionContext)
pcAction = lens _pcAction (\ s a -> s{_pcAction = a});

instance FromJSON PipelineContext where
        parseJSON
          = withObject "PipelineContext"
              (\ x ->
                 PipelineContext' <$>
                   (x .:? "stage") <*> (x .:? "pipelineName") <*>
                     (x .:? "action"))

-- | Represents the structure of actions and stages to be performed in the
-- pipeline.
--
-- /See:/ 'pipelineDeclaration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdVersion'
--
-- * 'pdName'
--
-- * 'pdRoleARN'
--
-- * 'pdArtifactStore'
--
-- * 'pdStages'
data PipelineDeclaration = PipelineDeclaration'
    { _pdVersion       :: !(Maybe Nat)
    , _pdName          :: !Text
    , _pdRoleARN       :: !Text
    , _pdArtifactStore :: !ArtifactStore
    , _pdStages        :: ![StageDeclaration]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PipelineDeclaration' smart constructor.
pipelineDeclaration :: Text -> Text -> ArtifactStore -> PipelineDeclaration
pipelineDeclaration pName_ pRoleARN_ pArtifactStore_ =
    PipelineDeclaration'
    { _pdVersion = Nothing
    , _pdName = pName_
    , _pdRoleARN = pRoleARN_
    , _pdArtifactStore = pArtifactStore_
    , _pdStages = mempty
    }

-- | The version number of the pipeline. A new pipeline always has a version
-- number of 1. This number is automatically incremented when a pipeline is
-- updated.
pdVersion :: Lens' PipelineDeclaration (Maybe Natural)
pdVersion = lens _pdVersion (\ s a -> s{_pdVersion = a}) . mapping _Nat;

-- | The name of the action to be performed.
pdName :: Lens' PipelineDeclaration Text
pdName = lens _pdName (\ s a -> s{_pdName = a});

-- | The Amazon Resource Name (ARN) for AWS CodePipeline to use to either
-- perform actions with no actionRoleArn, or to use to assume roles for
-- actions with an actionRoleArn.
pdRoleARN :: Lens' PipelineDeclaration Text
pdRoleARN = lens _pdRoleARN (\ s a -> s{_pdRoleARN = a});

-- | Undocumented member.
pdArtifactStore :: Lens' PipelineDeclaration ArtifactStore
pdArtifactStore = lens _pdArtifactStore (\ s a -> s{_pdArtifactStore = a});

-- | The stage in which to perform the action.
pdStages :: Lens' PipelineDeclaration [StageDeclaration]
pdStages = lens _pdStages (\ s a -> s{_pdStages = a}) . _Coerce;

instance FromJSON PipelineDeclaration where
        parseJSON
          = withObject "PipelineDeclaration"
              (\ x ->
                 PipelineDeclaration' <$>
                   (x .:? "version") <*> (x .: "name") <*>
                     (x .: "roleArn")
                     <*> (x .: "artifactStore")
                     <*> (x .:? "stages" .!= mempty))

instance ToJSON PipelineDeclaration where
        toJSON PipelineDeclaration'{..}
          = object
              ["version" .= _pdVersion, "name" .= _pdName,
               "roleArn" .= _pdRoleARN,
               "artifactStore" .= _pdArtifactStore,
               "stages" .= _pdStages]

-- | Returns a summary of a pipeline.
--
-- /See:/ 'pipelineSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psCreated'
--
-- * 'psName'
--
-- * 'psVersion'
--
-- * 'psUpdated'
data PipelineSummary = PipelineSummary'
    { _psCreated :: !(Maybe POSIX)
    , _psName    :: !(Maybe Text)
    , _psVersion :: !(Maybe Nat)
    , _psUpdated :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PipelineSummary' smart constructor.
pipelineSummary :: PipelineSummary
pipelineSummary =
    PipelineSummary'
    { _psCreated = Nothing
    , _psName = Nothing
    , _psVersion = Nothing
    , _psUpdated = Nothing
    }

-- | The date and time the pipeline was created, in timestamp format.
psCreated :: Lens' PipelineSummary (Maybe UTCTime)
psCreated = lens _psCreated (\ s a -> s{_psCreated = a}) . mapping _Time;

-- | The name of the pipeline.
psName :: Lens' PipelineSummary (Maybe Text)
psName = lens _psName (\ s a -> s{_psName = a});

-- | The version number of the pipeline.
psVersion :: Lens' PipelineSummary (Maybe Natural)
psVersion = lens _psVersion (\ s a -> s{_psVersion = a}) . mapping _Nat;

-- | The date and time of the last update to the pipeline, in timestamp
-- format.
psUpdated :: Lens' PipelineSummary (Maybe UTCTime)
psUpdated = lens _psUpdated (\ s a -> s{_psUpdated = a}) . mapping _Time;

instance FromJSON PipelineSummary where
        parseJSON
          = withObject "PipelineSummary"
              (\ x ->
                 PipelineSummary' <$>
                   (x .:? "created") <*> (x .:? "name") <*>
                     (x .:? "version")
                     <*> (x .:? "updated"))

-- | The location of the Amazon S3 bucket that contains a revision.
--
-- /See:/ 's3ArtifactLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'salBucketName'
--
-- * 'salObjectKey'
data S3ArtifactLocation = S3ArtifactLocation'
    { _salBucketName :: !Text
    , _salObjectKey  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'S3ArtifactLocation' smart constructor.
s3ArtifactLocation :: Text -> Text -> S3ArtifactLocation
s3ArtifactLocation pBucketName_ pObjectKey_ =
    S3ArtifactLocation'
    { _salBucketName = pBucketName_
    , _salObjectKey = pObjectKey_
    }

-- | The name of the Amazon S3 bucket.
salBucketName :: Lens' S3ArtifactLocation Text
salBucketName = lens _salBucketName (\ s a -> s{_salBucketName = a});

-- | The key of the object in the Amazon S3 bucket, which uniquely identifies
-- the object in the bucket.
salObjectKey :: Lens' S3ArtifactLocation Text
salObjectKey = lens _salObjectKey (\ s a -> s{_salObjectKey = a});

instance FromJSON S3ArtifactLocation where
        parseJSON
          = withObject "S3ArtifactLocation"
              (\ x ->
                 S3ArtifactLocation' <$>
                   (x .: "bucketName") <*> (x .: "objectKey"))

-- | Represents information about a stage to a job worker.
--
-- /See:/ 'stageContext' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scName'
newtype StageContext = StageContext'
    { _scName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StageContext' smart constructor.
stageContext :: StageContext
stageContext =
    StageContext'
    { _scName = Nothing
    }

-- | The name of the stage.
scName :: Lens' StageContext (Maybe Text)
scName = lens _scName (\ s a -> s{_scName = a});

instance FromJSON StageContext where
        parseJSON
          = withObject "StageContext"
              (\ x -> StageContext' <$> (x .:? "name"))

-- | Represents information about a stage and its definition.
--
-- /See:/ 'stageDeclaration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdBlockers'
--
-- * 'sdName'
--
-- * 'sdActions'
data StageDeclaration = StageDeclaration'
    { _sdBlockers :: !(Maybe [BlockerDeclaration])
    , _sdName     :: !Text
    , _sdActions  :: ![ActionDeclaration]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StageDeclaration' smart constructor.
stageDeclaration :: Text -> StageDeclaration
stageDeclaration pName_ =
    StageDeclaration'
    { _sdBlockers = Nothing
    , _sdName = pName_
    , _sdActions = mempty
    }

-- | The gates included in a stage.
sdBlockers :: Lens' StageDeclaration [BlockerDeclaration]
sdBlockers = lens _sdBlockers (\ s a -> s{_sdBlockers = a}) . _Default . _Coerce;

-- | The name of the stage.
sdName :: Lens' StageDeclaration Text
sdName = lens _sdName (\ s a -> s{_sdName = a});

-- | The actions included in a stage.
sdActions :: Lens' StageDeclaration [ActionDeclaration]
sdActions = lens _sdActions (\ s a -> s{_sdActions = a}) . _Coerce;

instance FromJSON StageDeclaration where
        parseJSON
          = withObject "StageDeclaration"
              (\ x ->
                 StageDeclaration' <$>
                   (x .:? "blockers" .!= mempty) <*> (x .: "name") <*>
                     (x .:? "actions" .!= mempty))

instance ToJSON StageDeclaration where
        toJSON StageDeclaration'{..}
          = object
              ["blockers" .= _sdBlockers, "name" .= _sdName,
               "actions" .= _sdActions]

-- | Represents information about the state of the stage.
--
-- /See:/ 'stageState' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssInboundTransitionState'
--
-- * 'ssActionStates'
--
-- * 'ssStageName'
data StageState = StageState'
    { _ssInboundTransitionState :: !(Maybe TransitionState)
    , _ssActionStates           :: !(Maybe [ActionState])
    , _ssStageName              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StageState' smart constructor.
stageState :: StageState
stageState =
    StageState'
    { _ssInboundTransitionState = Nothing
    , _ssActionStates = Nothing
    , _ssStageName = Nothing
    }

-- | The state of the inbound transition, which is either enabled or
-- disabled.
ssInboundTransitionState :: Lens' StageState (Maybe TransitionState)
ssInboundTransitionState = lens _ssInboundTransitionState (\ s a -> s{_ssInboundTransitionState = a});

-- | The state of the stage.
ssActionStates :: Lens' StageState [ActionState]
ssActionStates = lens _ssActionStates (\ s a -> s{_ssActionStates = a}) . _Default . _Coerce;

-- | The name of the stage.
ssStageName :: Lens' StageState (Maybe Text)
ssStageName = lens _ssStageName (\ s a -> s{_ssStageName = a});

instance FromJSON StageState where
        parseJSON
          = withObject "StageState"
              (\ x ->
                 StageState' <$>
                   (x .:? "inboundTransitionState") <*>
                     (x .:? "actionStates" .!= mempty)
                     <*> (x .:? "stageName"))

-- | A response to a PollForThirdPartyJobs request returned by AWS
-- CodePipeline when there is a job to be worked upon by a partner action.
--
-- /See:/ 'thirdPartyJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tpjClientId'
--
-- * 'tpjJobId'
data ThirdPartyJob = ThirdPartyJob'
    { _tpjClientId :: !(Maybe Text)
    , _tpjJobId    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ThirdPartyJob' smart constructor.
thirdPartyJob :: ThirdPartyJob
thirdPartyJob =
    ThirdPartyJob'
    { _tpjClientId = Nothing
    , _tpjJobId = Nothing
    }

-- | The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
tpjClientId :: Lens' ThirdPartyJob (Maybe Text)
tpjClientId = lens _tpjClientId (\ s a -> s{_tpjClientId = a});

-- | The identifier used to identify the job in AWS CodePipeline.
tpjJobId :: Lens' ThirdPartyJob (Maybe Text)
tpjJobId = lens _tpjJobId (\ s a -> s{_tpjJobId = a});

instance FromJSON ThirdPartyJob where
        parseJSON
          = withObject "ThirdPartyJob"
              (\ x ->
                 ThirdPartyJob' <$>
                   (x .:? "clientId") <*> (x .:? "jobId"))

-- | Represents information about the job data for a partner action.
--
-- /See:/ 'thirdPartyJobData' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tpjdContinuationToken'
--
-- * 'tpjdOutputArtifacts'
--
-- * 'tpjdArtifactCredentials'
--
-- * 'tpjdPipelineContext'
--
-- * 'tpjdActionTypeId'
--
-- * 'tpjdInputArtifacts'
--
-- * 'tpjdActionConfiguration'
data ThirdPartyJobData = ThirdPartyJobData'
    { _tpjdContinuationToken   :: !(Maybe Text)
    , _tpjdOutputArtifacts     :: !(Maybe [Artifact])
    , _tpjdArtifactCredentials :: !(Maybe (Sensitive AWSSessionCredentials))
    , _tpjdPipelineContext     :: !(Maybe PipelineContext)
    , _tpjdActionTypeId        :: !(Maybe ActionTypeId)
    , _tpjdInputArtifacts      :: !(Maybe [Artifact])
    , _tpjdActionConfiguration :: !(Maybe ActionConfiguration)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ThirdPartyJobData' smart constructor.
thirdPartyJobData :: ThirdPartyJobData
thirdPartyJobData =
    ThirdPartyJobData'
    { _tpjdContinuationToken = Nothing
    , _tpjdOutputArtifacts = Nothing
    , _tpjdArtifactCredentials = Nothing
    , _tpjdPipelineContext = Nothing
    , _tpjdActionTypeId = Nothing
    , _tpjdInputArtifacts = Nothing
    , _tpjdActionConfiguration = Nothing
    }

-- | A system-generated token, such as a AWS CodeDeploy deployment ID, that a
-- job requires in order to continue the job asynchronously.
tpjdContinuationToken :: Lens' ThirdPartyJobData (Maybe Text)
tpjdContinuationToken = lens _tpjdContinuationToken (\ s a -> s{_tpjdContinuationToken = a});

-- | The name of the artifact that will be the result of the action, if any.
-- This name might be system-generated, such as \"MyBuiltApp\", or might be
-- defined by the user when the action is created.
tpjdOutputArtifacts :: Lens' ThirdPartyJobData [Artifact]
tpjdOutputArtifacts = lens _tpjdOutputArtifacts (\ s a -> s{_tpjdOutputArtifacts = a}) . _Default . _Coerce;

-- | Undocumented member.
tpjdArtifactCredentials :: Lens' ThirdPartyJobData (Maybe AWSSessionCredentials)
tpjdArtifactCredentials = lens _tpjdArtifactCredentials (\ s a -> s{_tpjdArtifactCredentials = a}) . mapping _Sensitive;

-- | Undocumented member.
tpjdPipelineContext :: Lens' ThirdPartyJobData (Maybe PipelineContext)
tpjdPipelineContext = lens _tpjdPipelineContext (\ s a -> s{_tpjdPipelineContext = a});

-- | Undocumented member.
tpjdActionTypeId :: Lens' ThirdPartyJobData (Maybe ActionTypeId)
tpjdActionTypeId = lens _tpjdActionTypeId (\ s a -> s{_tpjdActionTypeId = a});

-- | The name of the artifact that will be worked upon by the action, if any.
-- This name might be system-generated, such as \"MyApp\", or might be
-- defined by the user when the action is created. The input artifact name
-- must match the name of an output artifact generated by an action in an
-- earlier action or stage of the pipeline.
tpjdInputArtifacts :: Lens' ThirdPartyJobData [Artifact]
tpjdInputArtifacts = lens _tpjdInputArtifacts (\ s a -> s{_tpjdInputArtifacts = a}) . _Default . _Coerce;

-- | Undocumented member.
tpjdActionConfiguration :: Lens' ThirdPartyJobData (Maybe ActionConfiguration)
tpjdActionConfiguration = lens _tpjdActionConfiguration (\ s a -> s{_tpjdActionConfiguration = a});

instance FromJSON ThirdPartyJobData where
        parseJSON
          = withObject "ThirdPartyJobData"
              (\ x ->
                 ThirdPartyJobData' <$>
                   (x .:? "continuationToken") <*>
                     (x .:? "outputArtifacts" .!= mempty)
                     <*> (x .:? "artifactCredentials")
                     <*> (x .:? "pipelineContext")
                     <*> (x .:? "actionTypeId")
                     <*> (x .:? "inputArtifacts" .!= mempty)
                     <*> (x .:? "actionConfiguration"))

-- | The details of a job sent in response to a GetThirdPartyJobDetails
-- request.
--
-- /See:/ 'thirdPartyJobDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tpjdData'
--
-- * 'tpjdId'
--
-- * 'tpjdNonce'
data ThirdPartyJobDetails = ThirdPartyJobDetails'
    { _tpjdData  :: !(Maybe ThirdPartyJobData)
    , _tpjdId    :: !(Maybe Text)
    , _tpjdNonce :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ThirdPartyJobDetails' smart constructor.
thirdPartyJobDetails :: ThirdPartyJobDetails
thirdPartyJobDetails =
    ThirdPartyJobDetails'
    { _tpjdData = Nothing
    , _tpjdId = Nothing
    , _tpjdNonce = Nothing
    }

-- | The data to be returned by the third party job worker.
tpjdData :: Lens' ThirdPartyJobDetails (Maybe ThirdPartyJobData)
tpjdData = lens _tpjdData (\ s a -> s{_tpjdData = a});

-- | The identifier used to identify the job details in AWS CodePipeline.
tpjdId :: Lens' ThirdPartyJobDetails (Maybe Text)
tpjdId = lens _tpjdId (\ s a -> s{_tpjdId = a});

-- | A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. This number must
-- be returned in the response.
tpjdNonce :: Lens' ThirdPartyJobDetails (Maybe Text)
tpjdNonce = lens _tpjdNonce (\ s a -> s{_tpjdNonce = a});

instance FromJSON ThirdPartyJobDetails where
        parseJSON
          = withObject "ThirdPartyJobDetails"
              (\ x ->
                 ThirdPartyJobDetails' <$>
                   (x .:? "data") <*> (x .:? "id") <*> (x .:? "nonce"))

-- | Represents information about the state of transitions between one stage
-- and another stage.
--
-- /See:/ 'transitionState' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tsEnabled'
--
-- * 'tsDisabledReason'
--
-- * 'tsLastChangedAt'
--
-- * 'tsLastChangedBy'
data TransitionState = TransitionState'
    { _tsEnabled        :: !(Maybe Bool)
    , _tsDisabledReason :: !(Maybe Text)
    , _tsLastChangedAt  :: !(Maybe POSIX)
    , _tsLastChangedBy  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TransitionState' smart constructor.
transitionState :: TransitionState
transitionState =
    TransitionState'
    { _tsEnabled = Nothing
    , _tsDisabledReason = Nothing
    , _tsLastChangedAt = Nothing
    , _tsLastChangedBy = Nothing
    }

-- | Whether the transition between stages is enabled (true) or disabled
-- (false).
tsEnabled :: Lens' TransitionState (Maybe Bool)
tsEnabled = lens _tsEnabled (\ s a -> s{_tsEnabled = a});

-- | The user-specified reason why the transition between two stages of a
-- pipeline was disabled.
tsDisabledReason :: Lens' TransitionState (Maybe Text)
tsDisabledReason = lens _tsDisabledReason (\ s a -> s{_tsDisabledReason = a});

-- | The timestamp when the transition state was last changed.
tsLastChangedAt :: Lens' TransitionState (Maybe UTCTime)
tsLastChangedAt = lens _tsLastChangedAt (\ s a -> s{_tsLastChangedAt = a}) . mapping _Time;

-- | The ID of the user who last changed the transition state.
tsLastChangedBy :: Lens' TransitionState (Maybe Text)
tsLastChangedBy = lens _tsLastChangedBy (\ s a -> s{_tsLastChangedBy = a});

instance FromJSON TransitionState where
        parseJSON
          = withObject "TransitionState"
              (\ x ->
                 TransitionState' <$>
                   (x .:? "enabled") <*> (x .:? "disabledReason") <*>
                     (x .:? "lastChangedAt")
                     <*> (x .:? "lastChangedBy"))
