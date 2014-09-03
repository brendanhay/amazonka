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
otOutputKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Output
    -> f Output
otOutputKey f x =
    (\y -> x { _otOutputKey = y })
       <$> f (_otOutputKey x)
{-# INLINE otOutputKey #-}

-- | The value associated with the output.
otOutputValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Output
    -> f Output
otOutputValue f x =
    (\y -> x { _otOutputValue = y })
       <$> f (_otOutputValue x)
{-# INLINE otOutputValue #-}

-- | User defined description associated with the output.
otDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Output
    -> f Output
otDescription f x =
    (\y -> x { _otDescription = y })
       <$> f (_otDescription x)
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
qParameterKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
qParameterKey f x =
    (\y -> x { _qParameterKey = y })
       <$> f (_qParameterKey x)
{-# INLINE qParameterKey #-}

-- | The value associated with the parameter.
qParameterValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
qParameterValue f x =
    (\y -> x { _qParameterValue = y })
       <$> f (_qParameterValue x)
{-# INLINE qParameterValue #-}

-- | During a stack update, use the existing parameter value that is being used
-- for the stack.
qUsePreviousValue
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Parameter
    -> f Parameter
qUsePreviousValue f x =
    (\y -> x { _qUsePreviousValue = y })
       <$> f (_qUsePreviousValue x)
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
yStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
yStackId f x =
    (\y -> x { _yStackId = y })
       <$> f (_yStackId x)
{-# INLINE yStackId #-}

-- | The name associated with the stack.
yStackName
    :: Functor f
    => (Text
    -> f (Text))
    -> Stack
    -> f Stack
yStackName f x =
    (\y -> x { _yStackName = y })
       <$> f (_yStackName x)
{-# INLINE yStackName #-}

-- | User defined description associated with the stack.
yDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
yDescription f x =
    (\y -> x { _yDescription = y })
       <$> f (_yDescription x)
{-# INLINE yDescription #-}

-- | A list of Parameter structures.
yParameters
    :: Functor f
    => ([Parameter]
    -> f ([Parameter]))
    -> Stack
    -> f Stack
yParameters f x =
    (\y -> x { _yParameters = y })
       <$> f (_yParameters x)
{-# INLINE yParameters #-}

-- | Time at which the stack was created.
yCreationTime
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> Stack
    -> f Stack
yCreationTime f x =
    (\y -> x { _yCreationTime = y })
       <$> f (_yCreationTime x)
{-# INLINE yCreationTime #-}

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
yLastUpdatedTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Stack
    -> f Stack
yLastUpdatedTime f x =
    (\y -> x { _yLastUpdatedTime = y })
       <$> f (_yLastUpdatedTime x)
{-# INLINE yLastUpdatedTime #-}

-- | Current status of the stack.
yStackStatus
    :: Functor f
    => (StackStatus
    -> f (StackStatus))
    -> Stack
    -> f Stack
yStackStatus f x =
    (\y -> x { _yStackStatus = y })
       <$> f (_yStackStatus x)
{-# INLINE yStackStatus #-}

-- | Success/failure message associated with the stack status.
yStackStatusReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
yStackStatusReason f x =
    (\y -> x { _yStackStatusReason = y })
       <$> f (_yStackStatusReason x)
{-# INLINE yStackStatusReason #-}

-- | Boolean to enable or disable rollback on stack creation failures: true:
-- disable rollback false: enable rollback.
yDisableRollback
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Stack
    -> f Stack
yDisableRollback f x =
    (\y -> x { _yDisableRollback = y })
       <$> f (_yDisableRollback x)
{-# INLINE yDisableRollback #-}

-- | SNS topic ARNs to which stack related events are published.
yNotificationARNs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Stack
    -> f Stack
yNotificationARNs f x =
    (\y -> x { _yNotificationARNs = y })
       <$> f (_yNotificationARNs x)
{-# INLINE yNotificationARNs #-}

-- | The amount of time within which stack creation should complete.
yTimeoutInMinutes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Stack
    -> f Stack
yTimeoutInMinutes f x =
    (\y -> x { _yTimeoutInMinutes = y })
       <$> f (_yTimeoutInMinutes x)
{-# INLINE yTimeoutInMinutes #-}

-- | The capabilities allowed in the stack.
yCapabilities
    :: Functor f
    => ([Capability]
    -> f ([Capability]))
    -> Stack
    -> f Stack
yCapabilities f x =
    (\y -> x { _yCapabilities = y })
       <$> f (_yCapabilities x)
{-# INLINE yCapabilities #-}

-- | A list of output structures.
yOutputs
    :: Functor f
    => ([Output]
    -> f ([Output]))
    -> Stack
    -> f Stack
yOutputs f x =
    (\y -> x { _yOutputs = y })
       <$> f (_yOutputs x)
{-# INLINE yOutputs #-}

-- | A list of Tags that specify cost allocation information for the stack.
yTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> Stack
    -> f Stack
yTags f x =
    (\y -> x { _yTags = y })
       <$> f (_yTags x)
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
sfStackId
    :: Functor f
    => (Text
    -> f (Text))
    -> StackEvent
    -> f StackEvent
sfStackId f x =
    (\y -> x { _sfStackId = y })
       <$> f (_sfStackId x)
{-# INLINE sfStackId #-}

-- | The unique ID of this event.
sfEventId
    :: Functor f
    => (Text
    -> f (Text))
    -> StackEvent
    -> f StackEvent
sfEventId f x =
    (\y -> x { _sfEventId = y })
       <$> f (_sfEventId x)
{-# INLINE sfEventId #-}

-- | The name associated with a stack.
sfStackName
    :: Functor f
    => (Text
    -> f (Text))
    -> StackEvent
    -> f StackEvent
sfStackName f x =
    (\y -> x { _sfStackName = y })
       <$> f (_sfStackName x)
{-# INLINE sfStackName #-}

-- | The logical name of the resource specified in the template.
sfLogicalResourceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackEvent
    -> f StackEvent
sfLogicalResourceId f x =
    (\y -> x { _sfLogicalResourceId = y })
       <$> f (_sfLogicalResourceId x)
{-# INLINE sfLogicalResourceId #-}

-- | The name or unique identifier associated with the physical instance of the
-- resource.
sfPhysicalResourceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackEvent
    -> f StackEvent
sfPhysicalResourceId f x =
    (\y -> x { _sfPhysicalResourceId = y })
       <$> f (_sfPhysicalResourceId x)
{-# INLINE sfPhysicalResourceId #-}

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
sfResourceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackEvent
    -> f StackEvent
sfResourceType f x =
    (\y -> x { _sfResourceType = y })
       <$> f (_sfResourceType x)
{-# INLINE sfResourceType #-}

-- | Time the status was updated.
sfTimestamp
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> StackEvent
    -> f StackEvent
sfTimestamp f x =
    (\y -> x { _sfTimestamp = y })
       <$> f (_sfTimestamp x)
{-# INLINE sfTimestamp #-}

-- | Current status of the resource.
sfResourceStatus
    :: Functor f
    => (Maybe ResourceStatus
    -> f (Maybe ResourceStatus))
    -> StackEvent
    -> f StackEvent
sfResourceStatus f x =
    (\y -> x { _sfResourceStatus = y })
       <$> f (_sfResourceStatus x)
{-# INLINE sfResourceStatus #-}

-- | Success/failure message associated with the resource.
sfResourceStatusReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackEvent
    -> f StackEvent
sfResourceStatusReason f x =
    (\y -> x { _sfResourceStatusReason = y })
       <$> f (_sfResourceStatusReason x)
{-# INLINE sfResourceStatusReason #-}

-- | BLOB of the properties used to create the resource.
sfResourceProperties
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackEvent
    -> f StackEvent
sfResourceProperties f x =
    (\y -> x { _sfResourceProperties = y })
       <$> f (_sfResourceProperties x)
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
swStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResource
    -> f StackResource
swStackName f x =
    (\y -> x { _swStackName = y })
       <$> f (_swStackName x)
{-# INLINE swStackName #-}

-- | Unique identifier of the stack.
swStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResource
    -> f StackResource
swStackId f x =
    (\y -> x { _swStackId = y })
       <$> f (_swStackId x)
{-# INLINE swStackId #-}

-- | The logical name of the resource specified in the template.
swLogicalResourceId
    :: Functor f
    => (Text
    -> f (Text))
    -> StackResource
    -> f StackResource
swLogicalResourceId f x =
    (\y -> x { _swLogicalResourceId = y })
       <$> f (_swLogicalResourceId x)
{-# INLINE swLogicalResourceId #-}

-- | The name or unique identifier that corresponds to a physical instance ID of
-- a resource supported by AWS CloudFormation.
swPhysicalResourceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResource
    -> f StackResource
swPhysicalResourceId f x =
    (\y -> x { _swPhysicalResourceId = y })
       <$> f (_swPhysicalResourceId x)
{-# INLINE swPhysicalResourceId #-}

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
swResourceType
    :: Functor f
    => (Text
    -> f (Text))
    -> StackResource
    -> f StackResource
swResourceType f x =
    (\y -> x { _swResourceType = y })
       <$> f (_swResourceType x)
{-# INLINE swResourceType #-}

-- | Time the status was updated.
swTimestamp
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> StackResource
    -> f StackResource
swTimestamp f x =
    (\y -> x { _swTimestamp = y })
       <$> f (_swTimestamp x)
{-# INLINE swTimestamp #-}

-- | Current status of the resource.
swResourceStatus
    :: Functor f
    => (ResourceStatus
    -> f (ResourceStatus))
    -> StackResource
    -> f StackResource
swResourceStatus f x =
    (\y -> x { _swResourceStatus = y })
       <$> f (_swResourceStatus x)
{-# INLINE swResourceStatus #-}

-- | Success/failure message associated with the resource.
swResourceStatusReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResource
    -> f StackResource
swResourceStatusReason f x =
    (\y -> x { _swResourceStatusReason = y })
       <$> f (_swResourceStatusReason x)
{-# INLINE swResourceStatusReason #-}

-- | User defined description associated with the resource.
swDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResource
    -> f StackResource
swDescription f x =
    (\y -> x { _swDescription = y })
       <$> f (_swDescription x)
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
srdStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResourceDetail
    -> f StackResourceDetail
srdStackName f x =
    (\y -> x { _srdStackName = y })
       <$> f (_srdStackName x)
{-# INLINE srdStackName #-}

-- | Unique identifier of the stack.
srdStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResourceDetail
    -> f StackResourceDetail
srdStackId f x =
    (\y -> x { _srdStackId = y })
       <$> f (_srdStackId x)
{-# INLINE srdStackId #-}

-- | The logical name of the resource specified in the template.
srdLogicalResourceId
    :: Functor f
    => (Text
    -> f (Text))
    -> StackResourceDetail
    -> f StackResourceDetail
srdLogicalResourceId f x =
    (\y -> x { _srdLogicalResourceId = y })
       <$> f (_srdLogicalResourceId x)
{-# INLINE srdLogicalResourceId #-}

-- | The name or unique identifier that corresponds to a physical instance ID of
-- a resource supported by AWS CloudFormation.
srdPhysicalResourceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResourceDetail
    -> f StackResourceDetail
srdPhysicalResourceId f x =
    (\y -> x { _srdPhysicalResourceId = y })
       <$> f (_srdPhysicalResourceId x)
{-# INLINE srdPhysicalResourceId #-}

-- | Type of resource. ((For more information, go to AWS Resource Types
-- Reference in the AWS CloudFormation User Guide.).
srdResourceType
    :: Functor f
    => (Text
    -> f (Text))
    -> StackResourceDetail
    -> f StackResourceDetail
srdResourceType f x =
    (\y -> x { _srdResourceType = y })
       <$> f (_srdResourceType x)
{-# INLINE srdResourceType #-}

-- | Time the status was updated.
srdLastUpdatedTimestamp
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> StackResourceDetail
    -> f StackResourceDetail
srdLastUpdatedTimestamp f x =
    (\y -> x { _srdLastUpdatedTimestamp = y })
       <$> f (_srdLastUpdatedTimestamp x)
{-# INLINE srdLastUpdatedTimestamp #-}

-- | Current status of the resource.
srdResourceStatus
    :: Functor f
    => (ResourceStatus
    -> f (ResourceStatus))
    -> StackResourceDetail
    -> f StackResourceDetail
srdResourceStatus f x =
    (\y -> x { _srdResourceStatus = y })
       <$> f (_srdResourceStatus x)
{-# INLINE srdResourceStatus #-}

-- | Success/failure message associated with the resource.
srdResourceStatusReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResourceDetail
    -> f StackResourceDetail
srdResourceStatusReason f x =
    (\y -> x { _srdResourceStatusReason = y })
       <$> f (_srdResourceStatusReason x)
{-# INLINE srdResourceStatusReason #-}

-- | User defined description associated with the resource.
srdDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResourceDetail
    -> f StackResourceDetail
srdDescription f x =
    (\y -> x { _srdDescription = y })
       <$> f (_srdDescription x)
{-# INLINE srdDescription #-}

-- | The JSON format content of the Metadata attribute declared for the
-- resource. For more information, see Metadata Attribute in the AWS
-- CloudFormation User Guide.
srdMetadata
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResourceDetail
    -> f StackResourceDetail
srdMetadata f x =
    (\y -> x { _srdMetadata = y })
       <$> f (_srdMetadata x)
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
srtLogicalResourceId
    :: Functor f
    => (Text
    -> f (Text))
    -> StackResourceSummary
    -> f StackResourceSummary
srtLogicalResourceId f x =
    (\y -> x { _srtLogicalResourceId = y })
       <$> f (_srtLogicalResourceId x)
{-# INLINE srtLogicalResourceId #-}

-- | The name or unique identifier that corresponds to a physical instance ID of
-- the resource.
srtPhysicalResourceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResourceSummary
    -> f StackResourceSummary
srtPhysicalResourceId f x =
    (\y -> x { _srtPhysicalResourceId = y })
       <$> f (_srtPhysicalResourceId x)
{-# INLINE srtPhysicalResourceId #-}

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
srtResourceType
    :: Functor f
    => (Text
    -> f (Text))
    -> StackResourceSummary
    -> f StackResourceSummary
srtResourceType f x =
    (\y -> x { _srtResourceType = y })
       <$> f (_srtResourceType x)
{-# INLINE srtResourceType #-}

-- | Time the status was updated.
srtLastUpdatedTimestamp
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> StackResourceSummary
    -> f StackResourceSummary
srtLastUpdatedTimestamp f x =
    (\y -> x { _srtLastUpdatedTimestamp = y })
       <$> f (_srtLastUpdatedTimestamp x)
{-# INLINE srtLastUpdatedTimestamp #-}

-- | Current status of the resource.
srtResourceStatus
    :: Functor f
    => (ResourceStatus
    -> f (ResourceStatus))
    -> StackResourceSummary
    -> f StackResourceSummary
srtResourceStatus f x =
    (\y -> x { _srtResourceStatus = y })
       <$> f (_srtResourceStatus x)
{-# INLINE srtResourceStatus #-}

-- | Success/failure message associated with the resource.
srtResourceStatusReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackResourceSummary
    -> f StackResourceSummary
srtResourceStatusReason f x =
    (\y -> x { _srtResourceStatusReason = y })
       <$> f (_srtResourceStatusReason x)
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
ssyStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackSummary
    -> f StackSummary
ssyStackId f x =
    (\y -> x { _ssyStackId = y })
       <$> f (_ssyStackId x)
{-# INLINE ssyStackId #-}

-- | The name associated with the stack.
ssyStackName
    :: Functor f
    => (Text
    -> f (Text))
    -> StackSummary
    -> f StackSummary
ssyStackName f x =
    (\y -> x { _ssyStackName = y })
       <$> f (_ssyStackName x)
{-# INLINE ssyStackName #-}

-- | The template description of the template used to create the stack.
ssyTemplateDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackSummary
    -> f StackSummary
ssyTemplateDescription f x =
    (\y -> x { _ssyTemplateDescription = y })
       <$> f (_ssyTemplateDescription x)
{-# INLINE ssyTemplateDescription #-}

-- | The time the stack was created.
ssyCreationTime
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> StackSummary
    -> f StackSummary
ssyCreationTime f x =
    (\y -> x { _ssyCreationTime = y })
       <$> f (_ssyCreationTime x)
{-# INLINE ssyCreationTime #-}

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
ssyLastUpdatedTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> StackSummary
    -> f StackSummary
ssyLastUpdatedTime f x =
    (\y -> x { _ssyLastUpdatedTime = y })
       <$> f (_ssyLastUpdatedTime x)
{-# INLINE ssyLastUpdatedTime #-}

-- | The time the stack was deleted.
ssyDeletionTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> StackSummary
    -> f StackSummary
ssyDeletionTime f x =
    (\y -> x { _ssyDeletionTime = y })
       <$> f (_ssyDeletionTime x)
{-# INLINE ssyDeletionTime #-}

-- | The current status of the stack.
ssyStackStatus
    :: Functor f
    => (StackStatus
    -> f (StackStatus))
    -> StackSummary
    -> f StackSummary
ssyStackStatus f x =
    (\y -> x { _ssyStackStatus = y })
       <$> f (_ssyStackStatus x)
{-# INLINE ssyStackStatus #-}

-- | Success/Failure message associated with the stack status.
ssyStackStatusReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackSummary
    -> f StackSummary
ssyStackStatusReason f x =
    (\y -> x { _ssyStackStatusReason = y })
       <$> f (_ssyStackStatusReason x)
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
uKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tag
    -> f Tag
uKey f x =
    (\y -> x { _uKey = y })
       <$> f (_uKey x)
{-# INLINE uKey #-}

-- | Required. A string containing the value for this tag. You can specify a
-- maximum of 256 characters for a tag value.
uValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tag
    -> f Tag
uValue f x =
    (\y -> x { _uValue = y })
       <$> f (_uValue x)
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
trParameterKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TemplateParameter
    -> f TemplateParameter
trParameterKey f x =
    (\y -> x { _trParameterKey = y })
       <$> f (_trParameterKey x)
{-# INLINE trParameterKey #-}

-- | The default value associated with the parameter.
trDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TemplateParameter
    -> f TemplateParameter
trDefaultValue f x =
    (\y -> x { _trDefaultValue = y })
       <$> f (_trDefaultValue x)
{-# INLINE trDefaultValue #-}

-- | Flag indicating whether the parameter should be displayed as plain text in
-- logs and UIs.
trNoEcho
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TemplateParameter
    -> f TemplateParameter
trNoEcho f x =
    (\y -> x { _trNoEcho = y })
       <$> f (_trNoEcho x)
{-# INLINE trNoEcho #-}

-- | User defined description associated with the parameter.
trDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TemplateParameter
    -> f TemplateParameter
trDescription f x =
    (\y -> x { _trDescription = y })
       <$> f (_trDescription x)
{-# INLINE trDescription #-}

instance FromXML TemplateParameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TemplateParameter"
