{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.CloudFormation.V2010_05_15.Types
    (
    -- * Service
      CloudFormation
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * Capability
    , Capability (..)

    -- * OnFailure
    , OnFailure (..)

    -- * ResourceStatus
    , ResourceStatus (..)

    -- * StackStatus
    , StackStatus (..)

    -- * Output
    , Output
    , mkOutput
    , otOutputKey
    , otOutputValue
    , otDescription

    -- * Parameter
    , Parameter
    , mkParameter
    , qParameterKey
    , qParameterValue
    , qUsePreviousValue

    -- * Stack
    , Stack
    , yStackId
    , yStackName
    , yDescription
    , yParameters
    , yCreationTime
    , yLastUpdatedTime
    , yStackStatus
    , yStackStatusReason
    , yDisableRollback
    , yNotificationARNs
    , yTimeoutInMinutes
    , yCapabilities
    , yOutputs
    , yTags

    -- * StackEvent
    , StackEvent
    , sfStackId
    , sfEventId
    , sfStackName
    , sfLogicalResourceId
    , sfPhysicalResourceId
    , sfResourceType
    , sfTimestamp
    , sfResourceStatus
    , sfResourceStatusReason
    , sfResourceProperties

    -- * StackResource
    , StackResource
    , swStackName
    , swStackId
    , swLogicalResourceId
    , swPhysicalResourceId
    , swResourceType
    , swTimestamp
    , swResourceStatus
    , swResourceStatusReason
    , swDescription

    -- * StackResourceDetail
    , StackResourceDetail
    , srdStackName
    , srdStackId
    , srdLogicalResourceId
    , srdPhysicalResourceId
    , srdResourceType
    , srdLastUpdatedTimestamp
    , srdResourceStatus
    , srdResourceStatusReason
    , srdDescription
    , srdMetadata

    -- * StackResourceSummary
    , StackResourceSummary
    , srtLogicalResourceId
    , srtPhysicalResourceId
    , srtResourceType
    , srtLastUpdatedTimestamp
    , srtResourceStatus
    , srtResourceStatusReason

    -- * StackSummary
    , StackSummary
    , ssyStackId
    , ssyStackName
    , ssyTemplateDescription
    , ssyCreationTime
    , ssyLastUpdatedTime
    , ssyDeletionTime
    , ssyStackStatus
    , ssyStackStatusReason

    -- * Tag
    , Tag
    , mkTag
    , uKey
    , uValue

    -- * TemplateParameter
    , TemplateParameter
    , trParameterKey
    , trDefaultValue
    , trNoEcho
    , trDescription
    ) where

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
    { _otOutputKey :: Maybe Text
      -- ^ The key associated with the output.
    , _otOutputValue :: Maybe Text
      -- ^ The value associated with the output.
    , _otDescription :: Maybe Text
      -- ^ User defined description associated with the output.
    } deriving (Show, Generic)

-- | The key associated with the output.
otOutputKey :: Lens' Output (Maybe Text)
otOutputKey = lens _otOutputKey (\s a -> s { _otOutputKey = a })
{-# INLINE otOutputKey #-}

-- | The value associated with the output.
otOutputValue :: Lens' Output (Maybe Text)
otOutputValue = lens _otOutputValue (\s a -> s { _otOutputValue = a })
{-# INLINE otOutputValue #-}

-- | User defined description associated with the output.
otDescription :: Lens' Output (Maybe Text)
otDescription = lens _otDescription (\s a -> s { _otDescription = a })
{-# INLINE otDescription #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Output' data type to populate a request.
mkOutput :: Output
mkOutput = Output
    { _otOutputKey = Nothing
    , _otOutputValue = Nothing
    , _otDescription = Nothing
    }
{-# INLINE mkOutput #-}

instance FromXML Output where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Output"

instance ToQuery Output where
    toQuery = genericQuery def

-- | The Parameter data type.
data Parameter = Parameter
    { _qParameterKey :: Maybe Text
      -- ^ The key associated with the parameter.
    , _qParameterValue :: Maybe Text
      -- ^ The value associated with the parameter.
    , _qUsePreviousValue :: Maybe Bool
      -- ^ During a stack update, use the existing parameter value that is
      -- being used for the stack.
    } deriving (Show, Generic)

-- | The key associated with the parameter.
qParameterKey :: Lens' Parameter (Maybe Text)
qParameterKey = lens _qParameterKey (\s a -> s { _qParameterKey = a })
{-# INLINE qParameterKey #-}

-- | The value associated with the parameter.
qParameterValue :: Lens' Parameter (Maybe Text)
qParameterValue = lens _qParameterValue (\s a -> s { _qParameterValue = a })
{-# INLINE qParameterValue #-}

-- | During a stack update, use the existing parameter value that is being used
-- for the stack.
qUsePreviousValue :: Lens' Parameter (Maybe Bool)
qUsePreviousValue = lens _qUsePreviousValue (\s a -> s { _qUsePreviousValue = a })
{-# INLINE qUsePreviousValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Parameter' data type to populate a request.
mkParameter :: Parameter
mkParameter = Parameter
    { _qParameterKey = Nothing
    , _qParameterValue = Nothing
    , _qUsePreviousValue = Nothing
    }
{-# INLINE mkParameter #-}

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

instance ToQuery Parameter where
    toQuery = genericQuery def

-- | The Stack data type.
data Stack = Stack
    { _yStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    , _yStackName :: Text
      -- ^ The name associated with the stack.
    , _yDescription :: Maybe Text
      -- ^ User defined description associated with the stack.
    , _yParameters :: [Parameter]
      -- ^ A list of Parameter structures.
    , _yCreationTime :: ISO8601
      -- ^ Time at which the stack was created.
    , _yLastUpdatedTime :: Maybe ISO8601
      -- ^ The time the stack was last updated. This field will only be
      -- returned if the stack has been updated at least once.
    , _yStackStatus :: StackStatus
      -- ^ Current status of the stack.
    , _yStackStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the stack status.
    , _yDisableRollback :: Maybe Bool
      -- ^ Boolean to enable or disable rollback on stack creation failures:
      -- true: disable rollback false: enable rollback.
    , _yNotificationARNs :: [Text]
      -- ^ SNS topic ARNs to which stack related events are published.
    , _yTimeoutInMinutes :: Maybe Integer
      -- ^ The amount of time within which stack creation should complete.
    , _yCapabilities :: [Capability]
      -- ^ The capabilities allowed in the stack.
    , _yOutputs :: [Output]
      -- ^ A list of output structures.
    , _yTags :: [Tag]
      -- ^ A list of Tags that specify cost allocation information for the
      -- stack.
    } deriving (Show, Generic)

-- | Unique identifier of the stack.
yStackId :: Lens' Stack (Maybe Text)
yStackId = lens _yStackId (\s a -> s { _yStackId = a })
{-# INLINE yStackId #-}

-- | The name associated with the stack.
yStackName :: Lens' Stack (Text)
yStackName = lens _yStackName (\s a -> s { _yStackName = a })
{-# INLINE yStackName #-}

-- | User defined description associated with the stack.
yDescription :: Lens' Stack (Maybe Text)
yDescription = lens _yDescription (\s a -> s { _yDescription = a })
{-# INLINE yDescription #-}

-- | A list of Parameter structures.
yParameters :: Lens' Stack ([Parameter])
yParameters = lens _yParameters (\s a -> s { _yParameters = a })
{-# INLINE yParameters #-}

-- | Time at which the stack was created.
yCreationTime :: Lens' Stack (ISO8601)
yCreationTime = lens _yCreationTime (\s a -> s { _yCreationTime = a })
{-# INLINE yCreationTime #-}

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
yLastUpdatedTime :: Lens' Stack (Maybe ISO8601)
yLastUpdatedTime = lens _yLastUpdatedTime (\s a -> s { _yLastUpdatedTime = a })
{-# INLINE yLastUpdatedTime #-}

-- | Current status of the stack.
yStackStatus :: Lens' Stack (StackStatus)
yStackStatus = lens _yStackStatus (\s a -> s { _yStackStatus = a })
{-# INLINE yStackStatus #-}

-- | Success/failure message associated with the stack status.
yStackStatusReason :: Lens' Stack (Maybe Text)
yStackStatusReason = lens _yStackStatusReason (\s a -> s { _yStackStatusReason = a })
{-# INLINE yStackStatusReason #-}

-- | Boolean to enable or disable rollback on stack creation failures: true:
-- disable rollback false: enable rollback.
yDisableRollback :: Lens' Stack (Maybe Bool)
yDisableRollback = lens _yDisableRollback (\s a -> s { _yDisableRollback = a })
{-# INLINE yDisableRollback #-}

-- | SNS topic ARNs to which stack related events are published.
yNotificationARNs :: Lens' Stack ([Text])
yNotificationARNs = lens _yNotificationARNs (\s a -> s { _yNotificationARNs = a })
{-# INLINE yNotificationARNs #-}

-- | The amount of time within which stack creation should complete.
yTimeoutInMinutes :: Lens' Stack (Maybe Integer)
yTimeoutInMinutes = lens _yTimeoutInMinutes (\s a -> s { _yTimeoutInMinutes = a })
{-# INLINE yTimeoutInMinutes #-}

-- | The capabilities allowed in the stack.
yCapabilities :: Lens' Stack ([Capability])
yCapabilities = lens _yCapabilities (\s a -> s { _yCapabilities = a })
{-# INLINE yCapabilities #-}

-- | A list of output structures.
yOutputs :: Lens' Stack ([Output])
yOutputs = lens _yOutputs (\s a -> s { _yOutputs = a })
{-# INLINE yOutputs #-}

-- | A list of Tags that specify cost allocation information for the stack.
yTags :: Lens' Stack ([Tag])
yTags = lens _yTags (\s a -> s { _yTags = a })
{-# INLINE yTags #-}

instance FromXML Stack where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Stack"

-- | The StackEvent data type.
data StackEvent = StackEvent
    { _sfStackId :: Text
      -- ^ The unique ID name of the instance of the stack.
    , _sfEventId :: Text
      -- ^ The unique ID of this event.
    , _sfStackName :: Text
      -- ^ The name associated with a stack.
    , _sfLogicalResourceId :: Maybe Text
      -- ^ The logical name of the resource specified in the template.
    , _sfPhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier associated with the physical
      -- instance of the resource.
    , _sfResourceType :: Maybe Text
      -- ^ Type of resource. (For more information, go to AWS Resource Types
      -- Reference in the AWS CloudFormation User Guide.).
    , _sfTimestamp :: ISO8601
      -- ^ Time the status was updated.
    , _sfResourceStatus :: Maybe ResourceStatus
      -- ^ Current status of the resource.
    , _sfResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , _sfResourceProperties :: Maybe Text
      -- ^ BLOB of the properties used to create the resource.
    } deriving (Show, Generic)

-- | The unique ID name of the instance of the stack.
sfStackId :: Lens' StackEvent (Text)
sfStackId = lens _sfStackId (\s a -> s { _sfStackId = a })
{-# INLINE sfStackId #-}

-- | The unique ID of this event.
sfEventId :: Lens' StackEvent (Text)
sfEventId = lens _sfEventId (\s a -> s { _sfEventId = a })
{-# INLINE sfEventId #-}

-- | The name associated with a stack.
sfStackName :: Lens' StackEvent (Text)
sfStackName = lens _sfStackName (\s a -> s { _sfStackName = a })
{-# INLINE sfStackName #-}

-- | The logical name of the resource specified in the template.
sfLogicalResourceId :: Lens' StackEvent (Maybe Text)
sfLogicalResourceId = lens _sfLogicalResourceId (\s a -> s { _sfLogicalResourceId = a })
{-# INLINE sfLogicalResourceId #-}

-- | The name or unique identifier associated with the physical instance of the
-- resource.
sfPhysicalResourceId :: Lens' StackEvent (Maybe Text)
sfPhysicalResourceId = lens _sfPhysicalResourceId (\s a -> s { _sfPhysicalResourceId = a })
{-# INLINE sfPhysicalResourceId #-}

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
sfResourceType :: Lens' StackEvent (Maybe Text)
sfResourceType = lens _sfResourceType (\s a -> s { _sfResourceType = a })
{-# INLINE sfResourceType #-}

-- | Time the status was updated.
sfTimestamp :: Lens' StackEvent (ISO8601)
sfTimestamp = lens _sfTimestamp (\s a -> s { _sfTimestamp = a })
{-# INLINE sfTimestamp #-}

-- | Current status of the resource.
sfResourceStatus :: Lens' StackEvent (Maybe ResourceStatus)
sfResourceStatus = lens _sfResourceStatus (\s a -> s { _sfResourceStatus = a })
{-# INLINE sfResourceStatus #-}

-- | Success/failure message associated with the resource.
sfResourceStatusReason :: Lens' StackEvent (Maybe Text)
sfResourceStatusReason = lens _sfResourceStatusReason (\s a -> s { _sfResourceStatusReason = a })
{-# INLINE sfResourceStatusReason #-}

-- | BLOB of the properties used to create the resource.
sfResourceProperties :: Lens' StackEvent (Maybe Text)
sfResourceProperties = lens _sfResourceProperties (\s a -> s { _sfResourceProperties = a })
{-# INLINE sfResourceProperties #-}

instance FromXML StackEvent where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackEvent"

-- | The StackResource data type.
data StackResource = StackResource
    { _swStackName :: Maybe Text
      -- ^ The name associated with the stack.
    , _swStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    , _swLogicalResourceId :: Text
      -- ^ The logical name of the resource specified in the template.
    , _swPhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier that corresponds to a physical
      -- instance ID of a resource supported by AWS CloudFormation.
    , _swResourceType :: Text
      -- ^ Type of resource. (For more information, go to AWS Resource Types
      -- Reference in the AWS CloudFormation User Guide.).
    , _swTimestamp :: ISO8601
      -- ^ Time the status was updated.
    , _swResourceStatus :: ResourceStatus
      -- ^ Current status of the resource.
    , _swResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , _swDescription :: Maybe Text
      -- ^ User defined description associated with the resource.
    } deriving (Show, Generic)

-- | The name associated with the stack.
swStackName :: Lens' StackResource (Maybe Text)
swStackName = lens _swStackName (\s a -> s { _swStackName = a })
{-# INLINE swStackName #-}

-- | Unique identifier of the stack.
swStackId :: Lens' StackResource (Maybe Text)
swStackId = lens _swStackId (\s a -> s { _swStackId = a })
{-# INLINE swStackId #-}

-- | The logical name of the resource specified in the template.
swLogicalResourceId :: Lens' StackResource (Text)
swLogicalResourceId = lens _swLogicalResourceId (\s a -> s { _swLogicalResourceId = a })
{-# INLINE swLogicalResourceId #-}

-- | The name or unique identifier that corresponds to a physical instance ID of
-- a resource supported by AWS CloudFormation.
swPhysicalResourceId :: Lens' StackResource (Maybe Text)
swPhysicalResourceId = lens _swPhysicalResourceId (\s a -> s { _swPhysicalResourceId = a })
{-# INLINE swPhysicalResourceId #-}

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
swResourceType :: Lens' StackResource (Text)
swResourceType = lens _swResourceType (\s a -> s { _swResourceType = a })
{-# INLINE swResourceType #-}

-- | Time the status was updated.
swTimestamp :: Lens' StackResource (ISO8601)
swTimestamp = lens _swTimestamp (\s a -> s { _swTimestamp = a })
{-# INLINE swTimestamp #-}

-- | Current status of the resource.
swResourceStatus :: Lens' StackResource (ResourceStatus)
swResourceStatus = lens _swResourceStatus (\s a -> s { _swResourceStatus = a })
{-# INLINE swResourceStatus #-}

-- | Success/failure message associated with the resource.
swResourceStatusReason :: Lens' StackResource (Maybe Text)
swResourceStatusReason = lens _swResourceStatusReason (\s a -> s { _swResourceStatusReason = a })
{-# INLINE swResourceStatusReason #-}

-- | User defined description associated with the resource.
swDescription :: Lens' StackResource (Maybe Text)
swDescription = lens _swDescription (\s a -> s { _swDescription = a })
{-# INLINE swDescription #-}

instance FromXML StackResource where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackResource"

-- | A StackResourceDetail structure containing the description of the specified
-- resource in the specified stack.
data StackResourceDetail = StackResourceDetail
    { _srdStackName :: Maybe Text
      -- ^ The name associated with the stack.
    , _srdStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    , _srdLogicalResourceId :: Text
      -- ^ The logical name of the resource specified in the template.
    , _srdPhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier that corresponds to a physical
      -- instance ID of a resource supported by AWS CloudFormation.
    , _srdResourceType :: Text
      -- ^ Type of resource. ((For more information, go to AWS Resource
      -- Types Reference in the AWS CloudFormation User Guide.).
    , _srdLastUpdatedTimestamp :: ISO8601
      -- ^ Time the status was updated.
    , _srdResourceStatus :: ResourceStatus
      -- ^ Current status of the resource.
    , _srdResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , _srdDescription :: Maybe Text
      -- ^ User defined description associated with the resource.
    , _srdMetadata :: Maybe Text
      -- ^ The JSON format content of the Metadata attribute declared for
      -- the resource. For more information, see Metadata Attribute in the
      -- AWS CloudFormation User Guide.
    } deriving (Show, Generic)

-- | The name associated with the stack.
srdStackName :: Lens' StackResourceDetail (Maybe Text)
srdStackName = lens _srdStackName (\s a -> s { _srdStackName = a })
{-# INLINE srdStackName #-}

-- | Unique identifier of the stack.
srdStackId :: Lens' StackResourceDetail (Maybe Text)
srdStackId = lens _srdStackId (\s a -> s { _srdStackId = a })
{-# INLINE srdStackId #-}

-- | The logical name of the resource specified in the template.
srdLogicalResourceId :: Lens' StackResourceDetail (Text)
srdLogicalResourceId = lens _srdLogicalResourceId (\s a -> s { _srdLogicalResourceId = a })
{-# INLINE srdLogicalResourceId #-}

-- | The name or unique identifier that corresponds to a physical instance ID of
-- a resource supported by AWS CloudFormation.
srdPhysicalResourceId :: Lens' StackResourceDetail (Maybe Text)
srdPhysicalResourceId = lens _srdPhysicalResourceId (\s a -> s { _srdPhysicalResourceId = a })
{-# INLINE srdPhysicalResourceId #-}

-- | Type of resource. ((For more information, go to AWS Resource Types
-- Reference in the AWS CloudFormation User Guide.).
srdResourceType :: Lens' StackResourceDetail (Text)
srdResourceType = lens _srdResourceType (\s a -> s { _srdResourceType = a })
{-# INLINE srdResourceType #-}

-- | Time the status was updated.
srdLastUpdatedTimestamp :: Lens' StackResourceDetail (ISO8601)
srdLastUpdatedTimestamp = lens _srdLastUpdatedTimestamp (\s a -> s { _srdLastUpdatedTimestamp = a })
{-# INLINE srdLastUpdatedTimestamp #-}

-- | Current status of the resource.
srdResourceStatus :: Lens' StackResourceDetail (ResourceStatus)
srdResourceStatus = lens _srdResourceStatus (\s a -> s { _srdResourceStatus = a })
{-# INLINE srdResourceStatus #-}

-- | Success/failure message associated with the resource.
srdResourceStatusReason :: Lens' StackResourceDetail (Maybe Text)
srdResourceStatusReason = lens _srdResourceStatusReason (\s a -> s { _srdResourceStatusReason = a })
{-# INLINE srdResourceStatusReason #-}

-- | User defined description associated with the resource.
srdDescription :: Lens' StackResourceDetail (Maybe Text)
srdDescription = lens _srdDescription (\s a -> s { _srdDescription = a })
{-# INLINE srdDescription #-}

-- | The JSON format content of the Metadata attribute declared for the
-- resource. For more information, see Metadata Attribute in the AWS
-- CloudFormation User Guide.
srdMetadata :: Lens' StackResourceDetail (Maybe Text)
srdMetadata = lens _srdMetadata (\s a -> s { _srdMetadata = a })
{-# INLINE srdMetadata #-}

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
    , _srtLastUpdatedTimestamp :: ISO8601
      -- ^ Time the status was updated.
    , _srtResourceStatus :: ResourceStatus
      -- ^ Current status of the resource.
    , _srtResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    } deriving (Show, Generic)

-- | The logical name of the resource specified in the template.
srtLogicalResourceId :: Lens' StackResourceSummary (Text)
srtLogicalResourceId = lens _srtLogicalResourceId (\s a -> s { _srtLogicalResourceId = a })
{-# INLINE srtLogicalResourceId #-}

-- | The name or unique identifier that corresponds to a physical instance ID of
-- the resource.
srtPhysicalResourceId :: Lens' StackResourceSummary (Maybe Text)
srtPhysicalResourceId = lens _srtPhysicalResourceId (\s a -> s { _srtPhysicalResourceId = a })
{-# INLINE srtPhysicalResourceId #-}

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
srtResourceType :: Lens' StackResourceSummary (Text)
srtResourceType = lens _srtResourceType (\s a -> s { _srtResourceType = a })
{-# INLINE srtResourceType #-}

-- | Time the status was updated.
srtLastUpdatedTimestamp :: Lens' StackResourceSummary (ISO8601)
srtLastUpdatedTimestamp = lens _srtLastUpdatedTimestamp (\s a -> s { _srtLastUpdatedTimestamp = a })
{-# INLINE srtLastUpdatedTimestamp #-}

-- | Current status of the resource.
srtResourceStatus :: Lens' StackResourceSummary (ResourceStatus)
srtResourceStatus = lens _srtResourceStatus (\s a -> s { _srtResourceStatus = a })
{-# INLINE srtResourceStatus #-}

-- | Success/failure message associated with the resource.
srtResourceStatusReason :: Lens' StackResourceSummary (Maybe Text)
srtResourceStatusReason = lens _srtResourceStatusReason (\s a -> s { _srtResourceStatusReason = a })
{-# INLINE srtResourceStatusReason #-}

instance FromXML StackResourceSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackResourceSummary"

-- | The StackSummary Data Type.
data StackSummary = StackSummary
    { _ssyStackId :: Maybe Text
      -- ^ Unique stack identifier.
    , _ssyStackName :: Text
      -- ^ The name associated with the stack.
    , _ssyTemplateDescription :: Maybe Text
      -- ^ The template description of the template used to create the
      -- stack.
    , _ssyCreationTime :: ISO8601
      -- ^ The time the stack was created.
    , _ssyLastUpdatedTime :: Maybe ISO8601
      -- ^ The time the stack was last updated. This field will only be
      -- returned if the stack has been updated at least once.
    , _ssyDeletionTime :: Maybe ISO8601
      -- ^ The time the stack was deleted.
    , _ssyStackStatus :: StackStatus
      -- ^ The current status of the stack.
    , _ssyStackStatusReason :: Maybe Text
      -- ^ Success/Failure message associated with the stack status.
    } deriving (Show, Generic)

-- | Unique stack identifier.
ssyStackId :: Lens' StackSummary (Maybe Text)
ssyStackId = lens _ssyStackId (\s a -> s { _ssyStackId = a })
{-# INLINE ssyStackId #-}

-- | The name associated with the stack.
ssyStackName :: Lens' StackSummary (Text)
ssyStackName = lens _ssyStackName (\s a -> s { _ssyStackName = a })
{-# INLINE ssyStackName #-}

-- | The template description of the template used to create the stack.
ssyTemplateDescription :: Lens' StackSummary (Maybe Text)
ssyTemplateDescription = lens _ssyTemplateDescription (\s a -> s { _ssyTemplateDescription = a })
{-# INLINE ssyTemplateDescription #-}

-- | The time the stack was created.
ssyCreationTime :: Lens' StackSummary (ISO8601)
ssyCreationTime = lens _ssyCreationTime (\s a -> s { _ssyCreationTime = a })
{-# INLINE ssyCreationTime #-}

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
ssyLastUpdatedTime :: Lens' StackSummary (Maybe ISO8601)
ssyLastUpdatedTime = lens _ssyLastUpdatedTime (\s a -> s { _ssyLastUpdatedTime = a })
{-# INLINE ssyLastUpdatedTime #-}

-- | The time the stack was deleted.
ssyDeletionTime :: Lens' StackSummary (Maybe ISO8601)
ssyDeletionTime = lens _ssyDeletionTime (\s a -> s { _ssyDeletionTime = a })
{-# INLINE ssyDeletionTime #-}

-- | The current status of the stack.
ssyStackStatus :: Lens' StackSummary (StackStatus)
ssyStackStatus = lens _ssyStackStatus (\s a -> s { _ssyStackStatus = a })
{-# INLINE ssyStackStatus #-}

-- | Success/Failure message associated with the stack status.
ssyStackStatusReason :: Lens' StackSummary (Maybe Text)
ssyStackStatusReason = lens _ssyStackStatusReason (\s a -> s { _ssyStackStatusReason = a })
{-# INLINE ssyStackStatusReason #-}

instance FromXML StackSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackSummary"

-- | The Tag type is used by CreateStack in the Tags parameter. It allows you to
-- specify a key/value pair that can be used to store information related to
-- cost allocation for an AWS CloudFormation stack.
data Tag = Tag
    { _uKey :: Maybe Text
      -- ^ Required. A string used to identify this tag. You can specify a
      -- maximum of 128 characters for a tag key. Tags owned by Amazon Web
      -- Services (AWS) have the reserved prefix: aws:.
    , _uValue :: Maybe Text
      -- ^ Required. A string containing the value for this tag. You can
      -- specify a maximum of 256 characters for a tag value.
    } deriving (Show, Generic)

-- | Required. A string used to identify this tag. You can specify a maximum of
-- 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have
-- the reserved prefix: aws:.
uKey :: Lens' Tag (Maybe Text)
uKey = lens _uKey (\s a -> s { _uKey = a })
{-# INLINE uKey #-}

-- | Required. A string containing the value for this tag. You can specify a
-- maximum of 256 characters for a tag value.
uValue :: Lens' Tag (Maybe Text)
uValue = lens _uValue (\s a -> s { _uValue = a })
{-# INLINE uValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
mkTag :: Tag
mkTag = Tag
    { _uKey = Nothing
    , _uValue = Nothing
    }
{-# INLINE mkTag #-}

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToQuery Tag where
    toQuery = genericQuery def

-- | The TemplateParameter data type.
data TemplateParameter = TemplateParameter
    { _trParameterKey :: Maybe Text
      -- ^ The name associated with the parameter.
    , _trDefaultValue :: Maybe Text
      -- ^ The default value associated with the parameter.
    , _trNoEcho :: Maybe Bool
      -- ^ Flag indicating whether the parameter should be displayed as
      -- plain text in logs and UIs.
    , _trDescription :: Maybe Text
      -- ^ User defined description associated with the parameter.
    } deriving (Show, Generic)

-- | The name associated with the parameter.
trParameterKey :: Lens' TemplateParameter (Maybe Text)
trParameterKey = lens _trParameterKey (\s a -> s { _trParameterKey = a })
{-# INLINE trParameterKey #-}

-- | The default value associated with the parameter.
trDefaultValue :: Lens' TemplateParameter (Maybe Text)
trDefaultValue = lens _trDefaultValue (\s a -> s { _trDefaultValue = a })
{-# INLINE trDefaultValue #-}

-- | Flag indicating whether the parameter should be displayed as plain text in
-- logs and UIs.
trNoEcho :: Lens' TemplateParameter (Maybe Bool)
trNoEcho = lens _trNoEcho (\s a -> s { _trNoEcho = a })
{-# INLINE trNoEcho #-}

-- | User defined description associated with the parameter.
trDescription :: Lens' TemplateParameter (Maybe Text)
trDescription = lens _trDescription (\s a -> s { _trDescription = a })
{-# INLINE trDescription #-}

instance FromXML TemplateParameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TemplateParameter"
