{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , Output (..)
    , otOutputKey
    , otOutputValue
    , otDescription

    -- * Parameter
    , Parameter (..)
    , qParameterKey
    , qParameterValue
    , qUsePreviousValue

    -- * Stack
    , Stack (..)
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
    , StackEvent (..)
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
    , StackResource (..)
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
    , StackResourceDetail (..)
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
    , StackResourceSummary (..)
    , srtLogicalResourceId
    , srtPhysicalResourceId
    , srtResourceType
    , srtLastUpdatedTimestamp
    , srtResourceStatus
    , srtResourceStatusReason

    -- * StackSummary
    , StackSummary (..)
    , ssyStackId
    , ssyStackName
    , ssyTemplateDescription
    , ssyCreationTime
    , ssyLastUpdatedTime
    , ssyDeletionTime
    , ssyStackStatus
    , ssyStackStatusReason

    -- * Tag
    , Tag (..)
    , uKey
    , uValue

    -- * TemplateParameter
    , TemplateParameter (..)
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
otOutputKey f x =
    f (_otOutputKey x)
        <&> \y -> x { _otOutputKey = y }
{-# INLINE otOutputKey #-}

-- | The value associated with the output.
otOutputValue :: Lens' Output (Maybe Text)
otOutputValue f x =
    f (_otOutputValue x)
        <&> \y -> x { _otOutputValue = y }
{-# INLINE otOutputValue #-}

-- | User defined description associated with the output.
otDescription :: Lens' Output (Maybe Text)
otDescription f x =
    f (_otDescription x)
        <&> \y -> x { _otDescription = y }
{-# INLINE otDescription #-}

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
qParameterKey f x =
    f (_qParameterKey x)
        <&> \y -> x { _qParameterKey = y }
{-# INLINE qParameterKey #-}

-- | The value associated with the parameter.
qParameterValue :: Lens' Parameter (Maybe Text)
qParameterValue f x =
    f (_qParameterValue x)
        <&> \y -> x { _qParameterValue = y }
{-# INLINE qParameterValue #-}

-- | During a stack update, use the existing parameter value that is being used
-- for the stack.
qUsePreviousValue :: Lens' Parameter (Maybe Bool)
qUsePreviousValue f x =
    f (_qUsePreviousValue x)
        <&> \y -> x { _qUsePreviousValue = y }
{-# INLINE qUsePreviousValue #-}

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
yStackId f x =
    f (_yStackId x)
        <&> \y -> x { _yStackId = y }
{-# INLINE yStackId #-}

-- | The name associated with the stack.
yStackName :: Lens' Stack (Text)
yStackName f x =
    f (_yStackName x)
        <&> \y -> x { _yStackName = y }
{-# INLINE yStackName #-}

-- | User defined description associated with the stack.
yDescription :: Lens' Stack (Maybe Text)
yDescription f x =
    f (_yDescription x)
        <&> \y -> x { _yDescription = y }
{-# INLINE yDescription #-}

-- | A list of Parameter structures.
yParameters :: Lens' Stack ([Parameter])
yParameters f x =
    f (_yParameters x)
        <&> \y -> x { _yParameters = y }
{-# INLINE yParameters #-}

-- | Time at which the stack was created.
yCreationTime :: Lens' Stack (ISO8601)
yCreationTime f x =
    f (_yCreationTime x)
        <&> \y -> x { _yCreationTime = y }
{-# INLINE yCreationTime #-}

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
yLastUpdatedTime :: Lens' Stack (Maybe ISO8601)
yLastUpdatedTime f x =
    f (_yLastUpdatedTime x)
        <&> \y -> x { _yLastUpdatedTime = y }
{-# INLINE yLastUpdatedTime #-}

-- | Current status of the stack.
yStackStatus :: Lens' Stack (StackStatus)
yStackStatus f x =
    f (_yStackStatus x)
        <&> \y -> x { _yStackStatus = y }
{-# INLINE yStackStatus #-}

-- | Success/failure message associated with the stack status.
yStackStatusReason :: Lens' Stack (Maybe Text)
yStackStatusReason f x =
    f (_yStackStatusReason x)
        <&> \y -> x { _yStackStatusReason = y }
{-# INLINE yStackStatusReason #-}

-- | Boolean to enable or disable rollback on stack creation failures: true:
-- disable rollback false: enable rollback.
yDisableRollback :: Lens' Stack (Maybe Bool)
yDisableRollback f x =
    f (_yDisableRollback x)
        <&> \y -> x { _yDisableRollback = y }
{-# INLINE yDisableRollback #-}

-- | SNS topic ARNs to which stack related events are published.
yNotificationARNs :: Lens' Stack ([Text])
yNotificationARNs f x =
    f (_yNotificationARNs x)
        <&> \y -> x { _yNotificationARNs = y }
{-# INLINE yNotificationARNs #-}

-- | The amount of time within which stack creation should complete.
yTimeoutInMinutes :: Lens' Stack (Maybe Integer)
yTimeoutInMinutes f x =
    f (_yTimeoutInMinutes x)
        <&> \y -> x { _yTimeoutInMinutes = y }
{-# INLINE yTimeoutInMinutes #-}

-- | The capabilities allowed in the stack.
yCapabilities :: Lens' Stack ([Capability])
yCapabilities f x =
    f (_yCapabilities x)
        <&> \y -> x { _yCapabilities = y }
{-# INLINE yCapabilities #-}

-- | A list of output structures.
yOutputs :: Lens' Stack ([Output])
yOutputs f x =
    f (_yOutputs x)
        <&> \y -> x { _yOutputs = y }
{-# INLINE yOutputs #-}

-- | A list of Tags that specify cost allocation information for the stack.
yTags :: Lens' Stack ([Tag])
yTags f x =
    f (_yTags x)
        <&> \y -> x { _yTags = y }
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
sfStackId f x =
    f (_sfStackId x)
        <&> \y -> x { _sfStackId = y }
{-# INLINE sfStackId #-}

-- | The unique ID of this event.
sfEventId :: Lens' StackEvent (Text)
sfEventId f x =
    f (_sfEventId x)
        <&> \y -> x { _sfEventId = y }
{-# INLINE sfEventId #-}

-- | The name associated with a stack.
sfStackName :: Lens' StackEvent (Text)
sfStackName f x =
    f (_sfStackName x)
        <&> \y -> x { _sfStackName = y }
{-# INLINE sfStackName #-}

-- | The logical name of the resource specified in the template.
sfLogicalResourceId :: Lens' StackEvent (Maybe Text)
sfLogicalResourceId f x =
    f (_sfLogicalResourceId x)
        <&> \y -> x { _sfLogicalResourceId = y }
{-# INLINE sfLogicalResourceId #-}

-- | The name or unique identifier associated with the physical instance of the
-- resource.
sfPhysicalResourceId :: Lens' StackEvent (Maybe Text)
sfPhysicalResourceId f x =
    f (_sfPhysicalResourceId x)
        <&> \y -> x { _sfPhysicalResourceId = y }
{-# INLINE sfPhysicalResourceId #-}

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
sfResourceType :: Lens' StackEvent (Maybe Text)
sfResourceType f x =
    f (_sfResourceType x)
        <&> \y -> x { _sfResourceType = y }
{-# INLINE sfResourceType #-}

-- | Time the status was updated.
sfTimestamp :: Lens' StackEvent (ISO8601)
sfTimestamp f x =
    f (_sfTimestamp x)
        <&> \y -> x { _sfTimestamp = y }
{-# INLINE sfTimestamp #-}

-- | Current status of the resource.
sfResourceStatus :: Lens' StackEvent (Maybe ResourceStatus)
sfResourceStatus f x =
    f (_sfResourceStatus x)
        <&> \y -> x { _sfResourceStatus = y }
{-# INLINE sfResourceStatus #-}

-- | Success/failure message associated with the resource.
sfResourceStatusReason :: Lens' StackEvent (Maybe Text)
sfResourceStatusReason f x =
    f (_sfResourceStatusReason x)
        <&> \y -> x { _sfResourceStatusReason = y }
{-# INLINE sfResourceStatusReason #-}

-- | BLOB of the properties used to create the resource.
sfResourceProperties :: Lens' StackEvent (Maybe Text)
sfResourceProperties f x =
    f (_sfResourceProperties x)
        <&> \y -> x { _sfResourceProperties = y }
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
swStackName f x =
    f (_swStackName x)
        <&> \y -> x { _swStackName = y }
{-# INLINE swStackName #-}

-- | Unique identifier of the stack.
swStackId :: Lens' StackResource (Maybe Text)
swStackId f x =
    f (_swStackId x)
        <&> \y -> x { _swStackId = y }
{-# INLINE swStackId #-}

-- | The logical name of the resource specified in the template.
swLogicalResourceId :: Lens' StackResource (Text)
swLogicalResourceId f x =
    f (_swLogicalResourceId x)
        <&> \y -> x { _swLogicalResourceId = y }
{-# INLINE swLogicalResourceId #-}

-- | The name or unique identifier that corresponds to a physical instance ID of
-- a resource supported by AWS CloudFormation.
swPhysicalResourceId :: Lens' StackResource (Maybe Text)
swPhysicalResourceId f x =
    f (_swPhysicalResourceId x)
        <&> \y -> x { _swPhysicalResourceId = y }
{-# INLINE swPhysicalResourceId #-}

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
swResourceType :: Lens' StackResource (Text)
swResourceType f x =
    f (_swResourceType x)
        <&> \y -> x { _swResourceType = y }
{-# INLINE swResourceType #-}

-- | Time the status was updated.
swTimestamp :: Lens' StackResource (ISO8601)
swTimestamp f x =
    f (_swTimestamp x)
        <&> \y -> x { _swTimestamp = y }
{-# INLINE swTimestamp #-}

-- | Current status of the resource.
swResourceStatus :: Lens' StackResource (ResourceStatus)
swResourceStatus f x =
    f (_swResourceStatus x)
        <&> \y -> x { _swResourceStatus = y }
{-# INLINE swResourceStatus #-}

-- | Success/failure message associated with the resource.
swResourceStatusReason :: Lens' StackResource (Maybe Text)
swResourceStatusReason f x =
    f (_swResourceStatusReason x)
        <&> \y -> x { _swResourceStatusReason = y }
{-# INLINE swResourceStatusReason #-}

-- | User defined description associated with the resource.
swDescription :: Lens' StackResource (Maybe Text)
swDescription f x =
    f (_swDescription x)
        <&> \y -> x { _swDescription = y }
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
srdStackName f x =
    f (_srdStackName x)
        <&> \y -> x { _srdStackName = y }
{-# INLINE srdStackName #-}

-- | Unique identifier of the stack.
srdStackId :: Lens' StackResourceDetail (Maybe Text)
srdStackId f x =
    f (_srdStackId x)
        <&> \y -> x { _srdStackId = y }
{-# INLINE srdStackId #-}

-- | The logical name of the resource specified in the template.
srdLogicalResourceId :: Lens' StackResourceDetail (Text)
srdLogicalResourceId f x =
    f (_srdLogicalResourceId x)
        <&> \y -> x { _srdLogicalResourceId = y }
{-# INLINE srdLogicalResourceId #-}

-- | The name or unique identifier that corresponds to a physical instance ID of
-- a resource supported by AWS CloudFormation.
srdPhysicalResourceId :: Lens' StackResourceDetail (Maybe Text)
srdPhysicalResourceId f x =
    f (_srdPhysicalResourceId x)
        <&> \y -> x { _srdPhysicalResourceId = y }
{-# INLINE srdPhysicalResourceId #-}

-- | Type of resource. ((For more information, go to AWS Resource Types
-- Reference in the AWS CloudFormation User Guide.).
srdResourceType :: Lens' StackResourceDetail (Text)
srdResourceType f x =
    f (_srdResourceType x)
        <&> \y -> x { _srdResourceType = y }
{-# INLINE srdResourceType #-}

-- | Time the status was updated.
srdLastUpdatedTimestamp :: Lens' StackResourceDetail (ISO8601)
srdLastUpdatedTimestamp f x =
    f (_srdLastUpdatedTimestamp x)
        <&> \y -> x { _srdLastUpdatedTimestamp = y }
{-# INLINE srdLastUpdatedTimestamp #-}

-- | Current status of the resource.
srdResourceStatus :: Lens' StackResourceDetail (ResourceStatus)
srdResourceStatus f x =
    f (_srdResourceStatus x)
        <&> \y -> x { _srdResourceStatus = y }
{-# INLINE srdResourceStatus #-}

-- | Success/failure message associated with the resource.
srdResourceStatusReason :: Lens' StackResourceDetail (Maybe Text)
srdResourceStatusReason f x =
    f (_srdResourceStatusReason x)
        <&> \y -> x { _srdResourceStatusReason = y }
{-# INLINE srdResourceStatusReason #-}

-- | User defined description associated with the resource.
srdDescription :: Lens' StackResourceDetail (Maybe Text)
srdDescription f x =
    f (_srdDescription x)
        <&> \y -> x { _srdDescription = y }
{-# INLINE srdDescription #-}

-- | The JSON format content of the Metadata attribute declared for the
-- resource. For more information, see Metadata Attribute in the AWS
-- CloudFormation User Guide.
srdMetadata :: Lens' StackResourceDetail (Maybe Text)
srdMetadata f x =
    f (_srdMetadata x)
        <&> \y -> x { _srdMetadata = y }
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
srtLogicalResourceId f x =
    f (_srtLogicalResourceId x)
        <&> \y -> x { _srtLogicalResourceId = y }
{-# INLINE srtLogicalResourceId #-}

-- | The name or unique identifier that corresponds to a physical instance ID of
-- the resource.
srtPhysicalResourceId :: Lens' StackResourceSummary (Maybe Text)
srtPhysicalResourceId f x =
    f (_srtPhysicalResourceId x)
        <&> \y -> x { _srtPhysicalResourceId = y }
{-# INLINE srtPhysicalResourceId #-}

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
srtResourceType :: Lens' StackResourceSummary (Text)
srtResourceType f x =
    f (_srtResourceType x)
        <&> \y -> x { _srtResourceType = y }
{-# INLINE srtResourceType #-}

-- | Time the status was updated.
srtLastUpdatedTimestamp :: Lens' StackResourceSummary (ISO8601)
srtLastUpdatedTimestamp f x =
    f (_srtLastUpdatedTimestamp x)
        <&> \y -> x { _srtLastUpdatedTimestamp = y }
{-# INLINE srtLastUpdatedTimestamp #-}

-- | Current status of the resource.
srtResourceStatus :: Lens' StackResourceSummary (ResourceStatus)
srtResourceStatus f x =
    f (_srtResourceStatus x)
        <&> \y -> x { _srtResourceStatus = y }
{-# INLINE srtResourceStatus #-}

-- | Success/failure message associated with the resource.
srtResourceStatusReason :: Lens' StackResourceSummary (Maybe Text)
srtResourceStatusReason f x =
    f (_srtResourceStatusReason x)
        <&> \y -> x { _srtResourceStatusReason = y }
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
ssyStackId f x =
    f (_ssyStackId x)
        <&> \y -> x { _ssyStackId = y }
{-# INLINE ssyStackId #-}

-- | The name associated with the stack.
ssyStackName :: Lens' StackSummary (Text)
ssyStackName f x =
    f (_ssyStackName x)
        <&> \y -> x { _ssyStackName = y }
{-# INLINE ssyStackName #-}

-- | The template description of the template used to create the stack.
ssyTemplateDescription :: Lens' StackSummary (Maybe Text)
ssyTemplateDescription f x =
    f (_ssyTemplateDescription x)
        <&> \y -> x { _ssyTemplateDescription = y }
{-# INLINE ssyTemplateDescription #-}

-- | The time the stack was created.
ssyCreationTime :: Lens' StackSummary (ISO8601)
ssyCreationTime f x =
    f (_ssyCreationTime x)
        <&> \y -> x { _ssyCreationTime = y }
{-# INLINE ssyCreationTime #-}

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
ssyLastUpdatedTime :: Lens' StackSummary (Maybe ISO8601)
ssyLastUpdatedTime f x =
    f (_ssyLastUpdatedTime x)
        <&> \y -> x { _ssyLastUpdatedTime = y }
{-# INLINE ssyLastUpdatedTime #-}

-- | The time the stack was deleted.
ssyDeletionTime :: Lens' StackSummary (Maybe ISO8601)
ssyDeletionTime f x =
    f (_ssyDeletionTime x)
        <&> \y -> x { _ssyDeletionTime = y }
{-# INLINE ssyDeletionTime #-}

-- | The current status of the stack.
ssyStackStatus :: Lens' StackSummary (StackStatus)
ssyStackStatus f x =
    f (_ssyStackStatus x)
        <&> \y -> x { _ssyStackStatus = y }
{-# INLINE ssyStackStatus #-}

-- | Success/Failure message associated with the stack status.
ssyStackStatusReason :: Lens' StackSummary (Maybe Text)
ssyStackStatusReason f x =
    f (_ssyStackStatusReason x)
        <&> \y -> x { _ssyStackStatusReason = y }
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
uKey f x =
    f (_uKey x)
        <&> \y -> x { _uKey = y }
{-# INLINE uKey #-}

-- | Required. A string containing the value for this tag. You can specify a
-- maximum of 256 characters for a tag value.
uValue :: Lens' Tag (Maybe Text)
uValue f x =
    f (_uValue x)
        <&> \y -> x { _uValue = y }
{-# INLINE uValue #-}

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
trParameterKey f x =
    f (_trParameterKey x)
        <&> \y -> x { _trParameterKey = y }
{-# INLINE trParameterKey #-}

-- | The default value associated with the parameter.
trDefaultValue :: Lens' TemplateParameter (Maybe Text)
trDefaultValue f x =
    f (_trDefaultValue x)
        <&> \y -> x { _trDefaultValue = y }
{-# INLINE trDefaultValue #-}

-- | Flag indicating whether the parameter should be displayed as plain text in
-- logs and UIs.
trNoEcho :: Lens' TemplateParameter (Maybe Bool)
trNoEcho f x =
    f (_trNoEcho x)
        <&> \y -> x { _trNoEcho = y }
{-# INLINE trNoEcho #-}

-- | User defined description associated with the parameter.
trDescription :: Lens' TemplateParameter (Maybe Text)
trDescription f x =
    f (_trDescription x)
        <&> \y -> x { _trDescription = y }
{-# INLINE trDescription #-}

instance FromXML TemplateParameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TemplateParameter"
