{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS CloudFormation gives developers and systems administrators an easy way
-- to create and manage a collection of related AWS resources, provisioning
-- and updating them in an orderly and predictable fashion. You can use AWS
-- CloudFormation’s sample templates or create your own templates to describe
-- the AWS resources, and any associated dependencies or runtime parameters,
-- required to run your application. You don’t need to figure out the order
-- for provisioning AWS services or the subtleties of making those
-- dependencies work. CloudFormation takes care of this for you. After the AWS
-- resources are deployed, you can modify and update them in a controlled and
-- predictable way, in effect applying version control to your AWS
-- infrastructure the same way you do with your software.
module Network.AWS.CloudFormation.V2010_05_15.Types where

import Control.Lens.TH (makeIso, makeLenses)
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2010-05-15@) of the
-- @AWS CloudFormation@ service.
data CloudFormation deriving (Typeable)

instance AWSService CloudFormation where
    type Sg CloudFormation = V4
    data Er CloudFormation
        = AlreadyExistsException
        | CloudFormationClient HttpException
        | CloudFormationSerializer String
        | CloudFormationService String
        | InsufficientCapabilitiesException
        | LimitExceededException

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "cloudformation"
        , _svcVersion  = "2010-05-15"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er CloudFormation)
deriving instance Generic (Er CloudFormation)

instance AWSError (Er CloudFormation) where
    awsError = const "CloudFormationError"

instance AWSServiceError (Er CloudFormation) where
    serviceError    = CloudFormationService
    clientError     = CloudFormationClient
    serializerError = CloudFormationSerializer

instance Exception (Er CloudFormation)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://cloudformation.amazonaws.com/doc/2010-05-15/"
    }

data Capability
    = CapabilityCapabilityIam -- ^ CAPABILITY_IAM
      deriving (Eq, Show, Generic)

instance Hashable Capability

instance FromText Capability where
    parser = match "CAPABILITY_IAM" CapabilityCapabilityIam

instance ToText Capability where
    toText CapabilityCapabilityIam = "CAPABILITY_IAM"

instance ToByteString Capability

instance FromXML Capability where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Capability"

instance ToQuery Capability where
    toQuery = genericQuery def

-- | Determines what action will be taken if stack creation fails. This must be
-- one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either OnFailure
-- or DisableRollback, but not both. Default: ROLLBACK.
data OnFailure
    = OnFailureDelete -- ^ DELETE
    | OnFailureDoNothing -- ^ DO_NOTHING
    | OnFailureRollback -- ^ ROLLBACK
      deriving (Eq, Show, Generic)

instance Hashable OnFailure

instance FromText OnFailure where
    parser = match "DELETE" OnFailureDelete
         <|> match "DO_NOTHING" OnFailureDoNothing
         <|> match "ROLLBACK" OnFailureRollback

instance ToText OnFailure where
    toText OnFailureDelete = "DELETE"
    toText OnFailureDoNothing = "DO_NOTHING"
    toText OnFailureRollback = "ROLLBACK"

instance ToByteString OnFailure

instance ToQuery OnFailure where
    toQuery = genericQuery def

-- | Current status of the resource.
data ResourceStatus
    = ResourceStatusCreateComplete -- ^ CREATE_COMPLETE
    | ResourceStatusCreateFailed -- ^ CREATE_FAILED
    | ResourceStatusCreateInProgress -- ^ CREATE_IN_PROGRESS
    | ResourceStatusDeleteComplete -- ^ DELETE_COMPLETE
    | ResourceStatusDeleteFailed -- ^ DELETE_FAILED
    | ResourceStatusDeleteInProgress -- ^ DELETE_IN_PROGRESS
    | ResourceStatusUpdateComplete -- ^ UPDATE_COMPLETE
    | ResourceStatusUpdateFailed -- ^ UPDATE_FAILED
    | ResourceStatusUpdateInProgress -- ^ UPDATE_IN_PROGRESS
      deriving (Eq, Show, Generic)

instance Hashable ResourceStatus

instance FromText ResourceStatus where
    parser = match "CREATE_COMPLETE" ResourceStatusCreateComplete
         <|> match "CREATE_FAILED" ResourceStatusCreateFailed
         <|> match "CREATE_IN_PROGRESS" ResourceStatusCreateInProgress
         <|> match "DELETE_COMPLETE" ResourceStatusDeleteComplete
         <|> match "DELETE_FAILED" ResourceStatusDeleteFailed
         <|> match "DELETE_IN_PROGRESS" ResourceStatusDeleteInProgress
         <|> match "UPDATE_COMPLETE" ResourceStatusUpdateComplete
         <|> match "UPDATE_FAILED" ResourceStatusUpdateFailed
         <|> match "UPDATE_IN_PROGRESS" ResourceStatusUpdateInProgress

instance ToText ResourceStatus where
    toText ResourceStatusCreateComplete = "CREATE_COMPLETE"
    toText ResourceStatusCreateFailed = "CREATE_FAILED"
    toText ResourceStatusCreateInProgress = "CREATE_IN_PROGRESS"
    toText ResourceStatusDeleteComplete = "DELETE_COMPLETE"
    toText ResourceStatusDeleteFailed = "DELETE_FAILED"
    toText ResourceStatusDeleteInProgress = "DELETE_IN_PROGRESS"
    toText ResourceStatusUpdateComplete = "UPDATE_COMPLETE"
    toText ResourceStatusUpdateFailed = "UPDATE_FAILED"
    toText ResourceStatusUpdateInProgress = "UPDATE_IN_PROGRESS"

instance ToByteString ResourceStatus

instance FromXML ResourceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResourceStatus"

instance ToQuery ResourceStatus where
    toQuery = genericQuery def

-- | Current status of the stack.
data StackStatus
    = StackStatusCreateComplete -- ^ CREATE_COMPLETE
    | StackStatusCreateFailed -- ^ CREATE_FAILED
    | StackStatusCreateInProgress -- ^ CREATE_IN_PROGRESS
    | StackStatusDeleteComplete -- ^ DELETE_COMPLETE
    | StackStatusDeleteFailed -- ^ DELETE_FAILED
    | StackStatusDeleteInProgress -- ^ DELETE_IN_PROGRESS
    | StackStatusRollbackComplete -- ^ ROLLBACK_COMPLETE
    | StackStatusRollbackFailed -- ^ ROLLBACK_FAILED
    | StackStatusRollbackInProgress -- ^ ROLLBACK_IN_PROGRESS
    | StackStatusUpdateComplete -- ^ UPDATE_COMPLETE
    | StackStatusUpdateCompleteCleanupInProgress -- ^ UPDATE_COMPLETE_CLEANUP_IN_PROGRESS
    | StackStatusUpdateInProgress -- ^ UPDATE_IN_PROGRESS
    | StackStatusUpdateRollbackComplete -- ^ UPDATE_ROLLBACK_COMPLETE
    | StackStatusUpdateRollbackCompleteCleanupInProgress -- ^ UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS
    | StackStatusUpdateRollbackFailed -- ^ UPDATE_ROLLBACK_FAILED
    | StackStatusUpdateRollbackInProgress -- ^ UPDATE_ROLLBACK_IN_PROGRESS
      deriving (Eq, Show, Generic)

instance Hashable StackStatus

instance FromText StackStatus where
    parser = match "CREATE_COMPLETE" StackStatusCreateComplete
         <|> match "CREATE_FAILED" StackStatusCreateFailed
         <|> match "CREATE_IN_PROGRESS" StackStatusCreateInProgress
         <|> match "DELETE_COMPLETE" StackStatusDeleteComplete
         <|> match "DELETE_FAILED" StackStatusDeleteFailed
         <|> match "DELETE_IN_PROGRESS" StackStatusDeleteInProgress
         <|> match "ROLLBACK_COMPLETE" StackStatusRollbackComplete
         <|> match "ROLLBACK_FAILED" StackStatusRollbackFailed
         <|> match "ROLLBACK_IN_PROGRESS" StackStatusRollbackInProgress
         <|> match "UPDATE_COMPLETE" StackStatusUpdateComplete
         <|> match "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS" StackStatusUpdateCompleteCleanupInProgress
         <|> match "UPDATE_IN_PROGRESS" StackStatusUpdateInProgress
         <|> match "UPDATE_ROLLBACK_COMPLETE" StackStatusUpdateRollbackComplete
         <|> match "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS" StackStatusUpdateRollbackCompleteCleanupInProgress
         <|> match "UPDATE_ROLLBACK_FAILED" StackStatusUpdateRollbackFailed
         <|> match "UPDATE_ROLLBACK_IN_PROGRESS" StackStatusUpdateRollbackInProgress

instance ToText StackStatus where
    toText StackStatusCreateComplete = "CREATE_COMPLETE"
    toText StackStatusCreateFailed = "CREATE_FAILED"
    toText StackStatusCreateInProgress = "CREATE_IN_PROGRESS"
    toText StackStatusDeleteComplete = "DELETE_COMPLETE"
    toText StackStatusDeleteFailed = "DELETE_FAILED"
    toText StackStatusDeleteInProgress = "DELETE_IN_PROGRESS"
    toText StackStatusRollbackComplete = "ROLLBACK_COMPLETE"
    toText StackStatusRollbackFailed = "ROLLBACK_FAILED"
    toText StackStatusRollbackInProgress = "ROLLBACK_IN_PROGRESS"
    toText StackStatusUpdateComplete = "UPDATE_COMPLETE"
    toText StackStatusUpdateCompleteCleanupInProgress = "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"
    toText StackStatusUpdateInProgress = "UPDATE_IN_PROGRESS"
    toText StackStatusUpdateRollbackComplete = "UPDATE_ROLLBACK_COMPLETE"
    toText StackStatusUpdateRollbackCompleteCleanupInProgress = "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
    toText StackStatusUpdateRollbackFailed = "UPDATE_ROLLBACK_FAILED"
    toText StackStatusUpdateRollbackInProgress = "UPDATE_ROLLBACK_IN_PROGRESS"

instance ToByteString StackStatus

instance FromXML StackStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackStatus"

instance ToQuery StackStatus where
    toQuery = genericQuery def

-- | The Output data type.
data Output = Output
    { _rOutputValue :: Maybe Text
      -- ^ The value associated with the output.
    , _rOutputKey :: Maybe Text
      -- ^ The key associated with the output.
    , _rDescription :: Maybe Text
      -- ^ User defined description associated with the output.
    } deriving (Show, Generic)

instance FromXML Output where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Output"

instance ToQuery Output where
    toQuery = genericQuery def

-- | The Parameter data type.
data Parameter = Parameter
    { _qParameterValue :: Maybe Text
      -- ^ The value associated with the parameter.
    , _qParameterKey :: Maybe Text
      -- ^ The key associated with the parameter.
    , _qUsePreviousValue :: Maybe Bool
      -- ^ During a stack update, use the existing parameter value that is
      -- being used for the stack.
    } deriving (Show, Generic)

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

instance ToQuery Parameter where
    toQuery = genericQuery def

-- | The Stack data type.
data Stack = Stack
    { _uCreationTime :: ISO8601
      -- ^ Time at which the stack was created.
    , _uStackStatus :: StackStatus
      -- ^ Current status of the stack.
    , _uDisableRollback :: Maybe Bool
      -- ^ Boolean to enable or disable rollback on stack creation failures:
      -- true: disable rollback false: enable rollback.
    , _uLastUpdatedTime :: Maybe ISO8601
      -- ^ The time the stack was last updated. This field will only be
      -- returned if the stack has been updated at least once.
    , _uNotificationARNs :: [Text]
      -- ^ SNS topic ARNs to which stack related events are published.
    , _uStackStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the stack status.
    , _uOutputs :: [Output]
      -- ^ A list of output structures.
    , _uParameters :: [Parameter]
      -- ^ A list of Parameter structures.
    , _uStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    , _uDescription :: Maybe Text
      -- ^ User defined description associated with the stack.
    , _uCapabilities :: [Capability]
      -- ^ The capabilities allowed in the stack.
    , _uTags :: [Tag]
      -- ^ A list of Tags that specify cost allocation information for the
      -- stack.
    , _uTimeoutInMinutes :: Maybe Integer
      -- ^ The amount of time within which stack creation should complete.
    , _uStackName :: Text
      -- ^ The name associated with the stack.
    } deriving (Show, Generic)

instance FromXML Stack where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Stack"

-- | The StackEvent data type.
data StackEvent = StackEvent
    { _sfLogicalResourceId :: Maybe Text
      -- ^ The logical name of the resource specified in the template.
    , _sfPhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier associated with the physical
      -- instance of the resource.
    , _sfResourceType :: Maybe Text
      -- ^ Type of resource. (For more information, go to AWS Resource Types
      -- Reference in the AWS CloudFormation User Guide.).
    , _sfResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , _sfResourceProperties :: Maybe Text
      -- ^ BLOB of the properties used to create the resource.
    , _sfResourceStatus :: Maybe ResourceStatus
      -- ^ Current status of the resource.
    , _sfStackId :: Text
      -- ^ The unique ID name of the instance of the stack.
    , _sfTimestamp :: ISO8601
      -- ^ Time the status was updated.
    , _sfEventId :: Text
      -- ^ The unique ID of this event.
    , _sfStackName :: Text
      -- ^ The name associated with a stack.
    } deriving (Show, Generic)

instance FromXML StackEvent where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackEvent"

-- | The StackResource data type.
data StackResource = StackResource
    { _sshLogicalResourceId :: Text
      -- ^ The logical name of the resource specified in the template.
    , _sshPhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier that corresponds to a physical
      -- instance ID of a resource supported by AWS CloudFormation.
    , _sshResourceType :: Text
      -- ^ Type of resource. (For more information, go to AWS Resource Types
      -- Reference in the AWS CloudFormation User Guide.).
    , _sshResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , _sshResourceStatus :: ResourceStatus
      -- ^ Current status of the resource.
    , _sshStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    , _sshDescription :: Maybe Text
      -- ^ User defined description associated with the resource.
    , _sshTimestamp :: ISO8601
      -- ^ Time the status was updated.
    , _sshStackName :: Maybe Text
      -- ^ The name associated with the stack.
    } deriving (Show, Generic)

instance FromXML StackResource where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackResource"

-- | A StackResourceDetail structure containing the description of the specified
-- resource in the specified stack.
data StackResourceDetail = StackResourceDetail
    { _srdLogicalResourceId :: Text
      -- ^ The logical name of the resource specified in the template.
    , _srdPhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier that corresponds to a physical
      -- instance ID of a resource supported by AWS CloudFormation.
    , _srdResourceType :: Text
      -- ^ Type of resource. ((For more information, go to AWS Resource
      -- Types Reference in the AWS CloudFormation User Guide.).
    , _srdResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , _srdResourceStatus :: ResourceStatus
      -- ^ Current status of the resource.
    , _srdMetadata :: Maybe Text
      -- ^ The JSON format content of the Metadata attribute declared for
      -- the resource. For more information, see Metadata Attribute in the
      -- AWS CloudFormation User Guide.
    , _srdStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    , _srdDescription :: Maybe Text
      -- ^ User defined description associated with the resource.
    , _srdLastUpdatedTimestamp :: ISO8601
      -- ^ Time the status was updated.
    , _srdStackName :: Maybe Text
      -- ^ The name associated with the stack.
    } deriving (Show, Generic)

instance FromXML StackResourceDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackResourceDetail"

-- | Contains high-level information about the specified stack resource.
data StackResourceSummary = StackResourceSummary
    { _srtLogicalResourceId :: Text
      -- ^ The logical name of the resource specified in the template.
    , _srtPhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier that corresponds to a physical
      -- instance ID of the resource.
    , _srtResourceType :: Text
      -- ^ Type of resource. (For more information, go to AWS Resource Types
      -- Reference in the AWS CloudFormation User Guide.).
    , _srtResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , _srtResourceStatus :: ResourceStatus
      -- ^ Current status of the resource.
    , _srtLastUpdatedTimestamp :: ISO8601
      -- ^ Time the status was updated.
    } deriving (Show, Generic)

instance FromXML StackResourceSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackResourceSummary"

-- | The StackSummary Data Type.
data StackSummary = StackSummary
    { _ssyCreationTime :: ISO8601
      -- ^ The time the stack was created.
    , _ssyStackStatus :: StackStatus
      -- ^ The current status of the stack.
    , _ssyLastUpdatedTime :: Maybe ISO8601
      -- ^ The time the stack was last updated. This field will only be
      -- returned if the stack has been updated at least once.
    , _ssyStackStatusReason :: Maybe Text
      -- ^ Success/Failure message associated with the stack status.
    , _ssyTemplateDescription :: Maybe Text
      -- ^ The template description of the template used to create the
      -- stack.
    , _ssyDeletionTime :: Maybe ISO8601
      -- ^ The time the stack was deleted.
    , _ssyStackId :: Maybe Text
      -- ^ Unique stack identifier.
    , _ssyStackName :: Text
      -- ^ The name associated with the stack.
    } deriving (Show, Generic)

instance FromXML StackSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackSummary"

-- | The Tag type is used by CreateStack in the Tags parameter. It allows you to
-- specify a key/value pair that can be used to store information related to
-- cost allocation for an AWS CloudFormation stack.
data Tag = Tag
    { _yValue :: Maybe Text
      -- ^ Required. A string containing the value for this tag. You can
      -- specify a maximum of 256 characters for a tag value.
    , _yKey :: Maybe Text
      -- ^ Required. A string used to identify this tag. You can specify a
      -- maximum of 128 characters for a tag key. Tags owned by Amazon Web
      -- Services (AWS) have the reserved prefix: aws:.
    } deriving (Show, Generic)

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToQuery Tag where
    toQuery = genericQuery def

-- | The TemplateParameter data type.
data TemplateParameter = TemplateParameter
    { _tqParameterKey :: Maybe Text
      -- ^ The name associated with the parameter.
    , _tqDefaultValue :: Maybe Text
      -- ^ The default value associated with the parameter.
    , _tqNoEcho :: Maybe Bool
      -- ^ Flag indicating whether the parameter should be displayed as
      -- plain text in logs and UIs.
    , _tqDescription :: Maybe Text
      -- ^ User defined description associated with the parameter.
    } deriving (Show, Generic)

instance FromXML TemplateParameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TemplateParameter"

-- Newtypes

-- Products
makeLenses ''Output
makeLenses ''Parameter
makeLenses ''Stack
makeLenses ''StackEvent
makeLenses ''StackResource
makeLenses ''StackResourceDetail
makeLenses ''StackResourceSummary
makeLenses ''StackSummary
makeLenses ''Tag
makeLenses ''TemplateParameter
