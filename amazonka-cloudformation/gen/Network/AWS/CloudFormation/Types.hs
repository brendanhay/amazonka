{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.Types
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
module Network.AWS.CloudFormation.Types
    (
    -- * Service
      CloudFormation
    -- ** Errors
    , CloudFormationError (..)
    , _AlreadyExistsException
    , _CloudFormationClient
    , _CloudFormationSerializer
    , _CloudFormationService
    , _InsufficientCapabilitiesException
    , _LimitExceededException
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
    , output
    , oOutputKey
    , oOutputValue
    , oDescription

    -- * Parameter
    , Parameter
    , parameter
    , pParameterKey
    , pParameterValue
    , pUsePreviousValue

    -- * Stack
    , Stack
    , stack
    , sStackId
    , sStackName
    , sDescription
    , sParameters
    , sCreationTime
    , sLastUpdatedTime
    , sStackStatus
    , sStackStatusReason
    , sDisableRollback
    , sNotificationARNs
    , sTimeoutInMinutes
    , sCapabilities
    , sOutputs
    , sTags

    -- * StackEvent
    , StackEvent
    , stackEvent
    , seStackId
    , seEventId
    , seStackName
    , seLogicalResourceId
    , sePhysicalResourceId
    , seResourceType
    , seTimestamp
    , seResourceStatus
    , seResourceStatusReason
    , seResourceProperties

    -- * StackResource
    , StackResource
    , stackResource
    , srStackName
    , srStackId
    , srLogicalResourceId
    , srPhysicalResourceId
    , srResourceType
    , srTimestamp
    , srResourceStatus
    , srResourceStatusReason
    , srDescription

    -- * StackResourceDetail
    , StackResourceDetail
    , stackResourceDetail
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
    , stackResourceSummary
    , srsLogicalResourceId
    , srsPhysicalResourceId
    , srsResourceType
    , srsLastUpdatedTimestamp
    , srsResourceStatus
    , srsResourceStatusReason

    -- * StackSummary
    , StackSummary
    , stackSummary
    , ssStackId
    , ssStackName
    , ssTemplateDescription
    , ssCreationTime
    , ssLastUpdatedTime
    , ssDeletionTime
    , ssStackStatus
    , ssStackStatusReason

    -- * Tag
    , Tag
    , tag
    , tKey
    , tValue

    -- * TemplateParameter
    , TemplateParameter
    , templateParameter
    , tpParameterKey
    , tpDefaultValue
    , tpNoEcho
    , tpDescription
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2010-05-15@) of the
-- @AWS CloudFormation@ service.
data CloudFormation deriving (Typeable)

instance AWSService CloudFormation where
    type Sg CloudFormation = V4
    type Er CloudFormation = CloudFormationError

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "cloudformation"
        , _svcVersion  = "2010-05-15"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'CloudFormation' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data CloudFormationError
      -- | Resource with the name requested already exists.
    = AlreadyExistsException
    | CloudFormationClient HttpException
    | CloudFormationSerializer String
    | CloudFormationService String
      -- | The template contains resources with capabilities that were not
      -- specified in the Capabilities parameter.
    | InsufficientCapabilitiesException
      -- | Quota for the resource has already been reached.
    | LimitExceededException
    deriving (Show, Generic)

instance AWSError CloudFormationError where
    awsError = const "CloudFormationError"

instance AWSServiceError CloudFormationError where
    serviceError    = CloudFormationService
    clientError     = CloudFormationClient
    serializerError = CloudFormationSerializer

instance Exception CloudFormationError

-- | Resource with the name requested already exists.
--
-- See: 'AlreadyExistsException'
_AlreadyExistsException :: Prism' CloudFormationError ()
_AlreadyExistsException = prism'
    (const AlreadyExistsException)
    (\case
        AlreadyExistsException -> Right ()
        x -> Left x)

-- | See: 'CloudFormationClient'
_CloudFormationClient :: Prism' CloudFormationError HttpException
_CloudFormationClient = prism'
    CloudFormationClient
    (\case
        CloudFormationClient p1 -> Right p1
        x -> Left x)

-- | See: 'CloudFormationSerializer'
_CloudFormationSerializer :: Prism' CloudFormationError String
_CloudFormationSerializer = prism'
    CloudFormationSerializer
    (\case
        CloudFormationSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'CloudFormationService'
_CloudFormationService :: Prism' CloudFormationError String
_CloudFormationService = prism'
    CloudFormationService
    (\case
        CloudFormationService p1 -> Right p1
        x -> Left x)

-- | The template contains resources with capabilities that were not specified
-- in the Capabilities parameter.
--
-- See: 'InsufficientCapabilitiesException'
_InsufficientCapabilitiesException :: Prism' CloudFormationError ()
_InsufficientCapabilitiesException = prism'
    (const InsufficientCapabilitiesException)
    (\case
        InsufficientCapabilitiesException -> Right ()
        x -> Left x)

-- | Quota for the resource has already been reached.
--
-- See: 'LimitExceededException'
_LimitExceededException :: Prism' CloudFormationError ()
_LimitExceededException = prism'
    (const LimitExceededException)
    (\case
        LimitExceededException -> Right ()
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

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
    { _oOutputKey :: Maybe Text
    , _oOutputValue :: Maybe Text
    , _oDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Output' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OutputKey ::@ @Maybe Text@
--
-- * @OutputValue ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
output :: Output
output = Output
    { _oOutputKey = Nothing
    , _oOutputValue = Nothing
    , _oDescription = Nothing
    }

-- | The key associated with the output.
oOutputKey :: Lens' Output (Maybe Text)
oOutputKey = lens _oOutputKey (\s a -> s { _oOutputKey = a })

-- | The value associated with the output.
oOutputValue :: Lens' Output (Maybe Text)
oOutputValue = lens _oOutputValue (\s a -> s { _oOutputValue = a })

-- | User defined description associated with the output.
oDescription :: Lens' Output (Maybe Text)
oDescription = lens _oDescription (\s a -> s { _oDescription = a })

instance FromXML Output where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Output"

instance ToQuery Output where
    toQuery = genericQuery def

-- | The Parameter data type.
data Parameter = Parameter
    { _pParameterKey :: Maybe Text
    , _pParameterValue :: Maybe Text
    , _pUsePreviousValue :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Parameter' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterKey ::@ @Maybe Text@
--
-- * @ParameterValue ::@ @Maybe Text@
--
-- * @UsePreviousValue ::@ @Maybe Bool@
--
parameter :: Parameter
parameter = Parameter
    { _pParameterKey = Nothing
    , _pParameterValue = Nothing
    , _pUsePreviousValue = Nothing
    }

-- | The key associated with the parameter.
pParameterKey :: Lens' Parameter (Maybe Text)
pParameterKey = lens _pParameterKey (\s a -> s { _pParameterKey = a })

-- | The value associated with the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s { _pParameterValue = a })

-- | During a stack update, use the existing parameter value that is being used
-- for the stack.
pUsePreviousValue :: Lens' Parameter (Maybe Bool)
pUsePreviousValue =
    lens _pUsePreviousValue (\s a -> s { _pUsePreviousValue = a })

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

instance ToQuery Parameter where
    toQuery = genericQuery def

-- | The Stack data type.
data Stack = Stack
    { _sStackId :: Maybe Text
    , _sStackName :: Text
    , _sDescription :: Maybe Text
    , _sParameters :: [Parameter]
    , _sCreationTime :: ISO8601
    , _sLastUpdatedTime :: Maybe ISO8601
    , _sStackStatus :: StackStatus
    , _sStackStatusReason :: Maybe Text
    , _sDisableRollback :: Maybe Bool
    , _sNotificationARNs :: [Text]
    , _sTimeoutInMinutes :: Maybe Integer
    , _sCapabilities :: [Capability]
    , _sOutputs :: [Output]
    , _sTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Stack' data type.
--
-- 'Stack' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @StackName ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Parameters ::@ @[Parameter]@
--
-- * @CreationTime ::@ @ISO8601@
--
-- * @LastUpdatedTime ::@ @Maybe ISO8601@
--
-- * @StackStatus ::@ @StackStatus@
--
-- * @StackStatusReason ::@ @Maybe Text@
--
-- * @DisableRollback ::@ @Maybe Bool@
--
-- * @NotificationARNs ::@ @[Text]@
--
-- * @TimeoutInMinutes ::@ @Maybe Integer@
--
-- * @Capabilities ::@ @[Capability]@
--
-- * @Outputs ::@ @[Output]@
--
-- * @Tags ::@ @[Tag]@
--
stack :: Text -- ^ 'sStackName'
        -> ISO8601 -- ^ 'sCreationTime'
        -> StackStatus -- ^ 'sStackStatus'
        -> Stack
stack p2 p5 p7 = Stack
    { _sStackId = Nothing
    , _sStackName = p2
    , _sDescription = Nothing
    , _sParameters = mempty
    , _sCreationTime = p5
    , _sLastUpdatedTime = Nothing
    , _sStackStatus = p7
    , _sStackStatusReason = Nothing
    , _sDisableRollback = Nothing
    , _sNotificationARNs = mempty
    , _sTimeoutInMinutes = Nothing
    , _sCapabilities = mempty
    , _sOutputs = mempty
    , _sTags = mempty
    }

-- | Unique identifier of the stack.
sStackId :: Lens' Stack (Maybe Text)
sStackId = lens _sStackId (\s a -> s { _sStackId = a })

-- | The name associated with the stack.
sStackName :: Lens' Stack Text
sStackName = lens _sStackName (\s a -> s { _sStackName = a })

-- | User defined description associated with the stack.
sDescription :: Lens' Stack (Maybe Text)
sDescription = lens _sDescription (\s a -> s { _sDescription = a })

-- | A list of Parameter structures.
sParameters :: Lens' Stack [Parameter]
sParameters = lens _sParameters (\s a -> s { _sParameters = a })

-- | Time at which the stack was created.
sCreationTime :: Lens' Stack ISO8601
sCreationTime = lens _sCreationTime (\s a -> s { _sCreationTime = a })

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
sLastUpdatedTime :: Lens' Stack (Maybe ISO8601)
sLastUpdatedTime =
    lens _sLastUpdatedTime (\s a -> s { _sLastUpdatedTime = a })

-- | Current status of the stack.
sStackStatus :: Lens' Stack StackStatus
sStackStatus = lens _sStackStatus (\s a -> s { _sStackStatus = a })

-- | Success/failure message associated with the stack status.
sStackStatusReason :: Lens' Stack (Maybe Text)
sStackStatusReason =
    lens _sStackStatusReason (\s a -> s { _sStackStatusReason = a })

-- | Boolean to enable or disable rollback on stack creation failures: true:
-- disable rollback false: enable rollback.
sDisableRollback :: Lens' Stack (Maybe Bool)
sDisableRollback =
    lens _sDisableRollback (\s a -> s { _sDisableRollback = a })

-- | SNS topic ARNs to which stack related events are published.
sNotificationARNs :: Lens' Stack [Text]
sNotificationARNs =
    lens _sNotificationARNs (\s a -> s { _sNotificationARNs = a })

-- | The amount of time within which stack creation should complete.
sTimeoutInMinutes :: Lens' Stack (Maybe Integer)
sTimeoutInMinutes =
    lens _sTimeoutInMinutes (\s a -> s { _sTimeoutInMinutes = a })

-- | The capabilities allowed in the stack.
sCapabilities :: Lens' Stack [Capability]
sCapabilities = lens _sCapabilities (\s a -> s { _sCapabilities = a })

-- | A list of output structures.
sOutputs :: Lens' Stack [Output]
sOutputs = lens _sOutputs (\s a -> s { _sOutputs = a })

-- | A list of Tags that specify cost allocation information for the stack.
sTags :: Lens' Stack [Tag]
sTags = lens _sTags (\s a -> s { _sTags = a })

instance FromXML Stack where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Stack"

-- | The StackEvent data type.
data StackEvent = StackEvent
    { _seStackId :: Text
    , _seEventId :: Text
    , _seStackName :: Text
    , _seLogicalResourceId :: Maybe Text
    , _sePhysicalResourceId :: Maybe Text
    , _seResourceType :: Maybe Text
    , _seTimestamp :: ISO8601
    , _seResourceStatus :: Maybe ResourceStatus
    , _seResourceStatusReason :: Maybe Text
    , _seResourceProperties :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StackEvent' data type.
--
-- 'StackEvent' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Text@
--
-- * @EventId ::@ @Text@
--
-- * @StackName ::@ @Text@
--
-- * @LogicalResourceId ::@ @Maybe Text@
--
-- * @PhysicalResourceId ::@ @Maybe Text@
--
-- * @ResourceType ::@ @Maybe Text@
--
-- * @Timestamp ::@ @ISO8601@
--
-- * @ResourceStatus ::@ @Maybe ResourceStatus@
--
-- * @ResourceStatusReason ::@ @Maybe Text@
--
-- * @ResourceProperties ::@ @Maybe Text@
--
stackEvent :: Text -- ^ 'seStackId'
             -> Text -- ^ 'seEventId'
             -> Text -- ^ 'seStackName'
             -> ISO8601 -- ^ 'seTimestamp'
             -> StackEvent
stackEvent p1 p2 p3 p7 = StackEvent
    { _seStackId = p1
    , _seEventId = p2
    , _seStackName = p3
    , _seLogicalResourceId = Nothing
    , _sePhysicalResourceId = Nothing
    , _seResourceType = Nothing
    , _seTimestamp = p7
    , _seResourceStatus = Nothing
    , _seResourceStatusReason = Nothing
    , _seResourceProperties = Nothing
    }

-- | The unique ID name of the instance of the stack.
seStackId :: Lens' StackEvent Text
seStackId = lens _seStackId (\s a -> s { _seStackId = a })

-- | The unique ID of this event.
seEventId :: Lens' StackEvent Text
seEventId = lens _seEventId (\s a -> s { _seEventId = a })

-- | The name associated with a stack.
seStackName :: Lens' StackEvent Text
seStackName = lens _seStackName (\s a -> s { _seStackName = a })

-- | The logical name of the resource specified in the template.
seLogicalResourceId :: Lens' StackEvent (Maybe Text)
seLogicalResourceId =
    lens _seLogicalResourceId (\s a -> s { _seLogicalResourceId = a })

-- | The name or unique identifier associated with the physical instance of the
-- resource.
sePhysicalResourceId :: Lens' StackEvent (Maybe Text)
sePhysicalResourceId =
    lens _sePhysicalResourceId (\s a -> s { _sePhysicalResourceId = a })

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
seResourceType :: Lens' StackEvent (Maybe Text)
seResourceType = lens _seResourceType (\s a -> s { _seResourceType = a })

-- | Time the status was updated.
seTimestamp :: Lens' StackEvent ISO8601
seTimestamp = lens _seTimestamp (\s a -> s { _seTimestamp = a })

-- | Current status of the resource.
seResourceStatus :: Lens' StackEvent (Maybe ResourceStatus)
seResourceStatus =
    lens _seResourceStatus (\s a -> s { _seResourceStatus = a })

-- | Success/failure message associated with the resource.
seResourceStatusReason :: Lens' StackEvent (Maybe Text)
seResourceStatusReason =
    lens _seResourceStatusReason (\s a -> s { _seResourceStatusReason = a })

-- | BLOB of the properties used to create the resource.
seResourceProperties :: Lens' StackEvent (Maybe Text)
seResourceProperties =
    lens _seResourceProperties (\s a -> s { _seResourceProperties = a })

instance FromXML StackEvent where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackEvent"

-- | The StackResource data type.
data StackResource = StackResource
    { _srStackName :: Maybe Text
    , _srStackId :: Maybe Text
    , _srLogicalResourceId :: Text
    , _srPhysicalResourceId :: Maybe Text
    , _srResourceType :: Text
    , _srTimestamp :: ISO8601
    , _srResourceStatus :: ResourceStatus
    , _srResourceStatusReason :: Maybe Text
    , _srDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StackResource' data type.
--
-- 'StackResource' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackName ::@ @Maybe Text@
--
-- * @StackId ::@ @Maybe Text@
--
-- * @LogicalResourceId ::@ @Text@
--
-- * @PhysicalResourceId ::@ @Maybe Text@
--
-- * @ResourceType ::@ @Text@
--
-- * @Timestamp ::@ @ISO8601@
--
-- * @ResourceStatus ::@ @ResourceStatus@
--
-- * @ResourceStatusReason ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
stackResource :: Text -- ^ 'srLogicalResourceId'
                -> Text -- ^ 'srResourceType'
                -> ISO8601 -- ^ 'srTimestamp'
                -> ResourceStatus -- ^ 'srResourceStatus'
                -> StackResource
stackResource p3 p5 p6 p7 = StackResource
    { _srStackName = Nothing
    , _srStackId = Nothing
    , _srLogicalResourceId = p3
    , _srPhysicalResourceId = Nothing
    , _srResourceType = p5
    , _srTimestamp = p6
    , _srResourceStatus = p7
    , _srResourceStatusReason = Nothing
    , _srDescription = Nothing
    }

-- | The name associated with the stack.
srStackName :: Lens' StackResource (Maybe Text)
srStackName = lens _srStackName (\s a -> s { _srStackName = a })

-- | Unique identifier of the stack.
srStackId :: Lens' StackResource (Maybe Text)
srStackId = lens _srStackId (\s a -> s { _srStackId = a })

-- | The logical name of the resource specified in the template.
srLogicalResourceId :: Lens' StackResource Text
srLogicalResourceId =
    lens _srLogicalResourceId (\s a -> s { _srLogicalResourceId = a })

-- | The name or unique identifier that corresponds to a physical instance ID of
-- a resource supported by AWS CloudFormation.
srPhysicalResourceId :: Lens' StackResource (Maybe Text)
srPhysicalResourceId =
    lens _srPhysicalResourceId (\s a -> s { _srPhysicalResourceId = a })

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
srResourceType :: Lens' StackResource Text
srResourceType = lens _srResourceType (\s a -> s { _srResourceType = a })

-- | Time the status was updated.
srTimestamp :: Lens' StackResource ISO8601
srTimestamp = lens _srTimestamp (\s a -> s { _srTimestamp = a })

-- | Current status of the resource.
srResourceStatus :: Lens' StackResource ResourceStatus
srResourceStatus =
    lens _srResourceStatus (\s a -> s { _srResourceStatus = a })

-- | Success/failure message associated with the resource.
srResourceStatusReason :: Lens' StackResource (Maybe Text)
srResourceStatusReason =
    lens _srResourceStatusReason (\s a -> s { _srResourceStatusReason = a })

-- | User defined description associated with the resource.
srDescription :: Lens' StackResource (Maybe Text)
srDescription = lens _srDescription (\s a -> s { _srDescription = a })

instance FromXML StackResource where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackResource"

-- | A StackResourceDetail structure containing the description of the specified
-- resource in the specified stack.
data StackResourceDetail = StackResourceDetail
    { _srdStackName :: Maybe Text
    , _srdStackId :: Maybe Text
    , _srdLogicalResourceId :: Text
    , _srdPhysicalResourceId :: Maybe Text
    , _srdResourceType :: Text
    , _srdLastUpdatedTimestamp :: ISO8601
    , _srdResourceStatus :: ResourceStatus
    , _srdResourceStatusReason :: Maybe Text
    , _srdDescription :: Maybe Text
    , _srdMetadata :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StackResourceDetail' data type.
--
-- 'StackResourceDetail' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackName ::@ @Maybe Text@
--
-- * @StackId ::@ @Maybe Text@
--
-- * @LogicalResourceId ::@ @Text@
--
-- * @PhysicalResourceId ::@ @Maybe Text@
--
-- * @ResourceType ::@ @Text@
--
-- * @LastUpdatedTimestamp ::@ @ISO8601@
--
-- * @ResourceStatus ::@ @ResourceStatus@
--
-- * @ResourceStatusReason ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Metadata ::@ @Maybe Text@
--
stackResourceDetail :: Text -- ^ 'srdLogicalResourceId'
                      -> Text -- ^ 'srdResourceType'
                      -> ISO8601 -- ^ 'srdLastUpdatedTimestamp'
                      -> ResourceStatus -- ^ 'srdResourceStatus'
                      -> StackResourceDetail
stackResourceDetail p3 p5 p6 p7 = StackResourceDetail
    { _srdStackName = Nothing
    , _srdStackId = Nothing
    , _srdLogicalResourceId = p3
    , _srdPhysicalResourceId = Nothing
    , _srdResourceType = p5
    , _srdLastUpdatedTimestamp = p6
    , _srdResourceStatus = p7
    , _srdResourceStatusReason = Nothing
    , _srdDescription = Nothing
    , _srdMetadata = Nothing
    }

-- | The name associated with the stack.
srdStackName :: Lens' StackResourceDetail (Maybe Text)
srdStackName = lens _srdStackName (\s a -> s { _srdStackName = a })

-- | Unique identifier of the stack.
srdStackId :: Lens' StackResourceDetail (Maybe Text)
srdStackId = lens _srdStackId (\s a -> s { _srdStackId = a })

-- | The logical name of the resource specified in the template.
srdLogicalResourceId :: Lens' StackResourceDetail Text
srdLogicalResourceId =
    lens _srdLogicalResourceId (\s a -> s { _srdLogicalResourceId = a })

-- | The name or unique identifier that corresponds to a physical instance ID of
-- a resource supported by AWS CloudFormation.
srdPhysicalResourceId :: Lens' StackResourceDetail (Maybe Text)
srdPhysicalResourceId =
    lens _srdPhysicalResourceId (\s a -> s { _srdPhysicalResourceId = a })

-- | Type of resource. ((For more information, go to AWS Resource Types
-- Reference in the AWS CloudFormation User Guide.).
srdResourceType :: Lens' StackResourceDetail Text
srdResourceType = lens _srdResourceType (\s a -> s { _srdResourceType = a })

-- | Time the status was updated.
srdLastUpdatedTimestamp :: Lens' StackResourceDetail ISO8601
srdLastUpdatedTimestamp =
    lens _srdLastUpdatedTimestamp
         (\s a -> s { _srdLastUpdatedTimestamp = a })

-- | Current status of the resource.
srdResourceStatus :: Lens' StackResourceDetail ResourceStatus
srdResourceStatus =
    lens _srdResourceStatus (\s a -> s { _srdResourceStatus = a })

-- | Success/failure message associated with the resource.
srdResourceStatusReason :: Lens' StackResourceDetail (Maybe Text)
srdResourceStatusReason =
    lens _srdResourceStatusReason
         (\s a -> s { _srdResourceStatusReason = a })

-- | User defined description associated with the resource.
srdDescription :: Lens' StackResourceDetail (Maybe Text)
srdDescription = lens _srdDescription (\s a -> s { _srdDescription = a })

-- | The JSON format content of the Metadata attribute declared for the
-- resource. For more information, see Metadata Attribute in the AWS
-- CloudFormation User Guide.
srdMetadata :: Lens' StackResourceDetail (Maybe Text)
srdMetadata = lens _srdMetadata (\s a -> s { _srdMetadata = a })

instance FromXML StackResourceDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackResourceDetail"

-- | Contains high-level information about the specified stack resource.
data StackResourceSummary = StackResourceSummary
    { _srsLogicalResourceId :: Text
    , _srsPhysicalResourceId :: Maybe Text
    , _srsResourceType :: Text
    , _srsLastUpdatedTimestamp :: ISO8601
    , _srsResourceStatus :: ResourceStatus
    , _srsResourceStatusReason :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StackResourceSummary' data type.
--
-- 'StackResourceSummary' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LogicalResourceId ::@ @Text@
--
-- * @PhysicalResourceId ::@ @Maybe Text@
--
-- * @ResourceType ::@ @Text@
--
-- * @LastUpdatedTimestamp ::@ @ISO8601@
--
-- * @ResourceStatus ::@ @ResourceStatus@
--
-- * @ResourceStatusReason ::@ @Maybe Text@
--
stackResourceSummary :: Text -- ^ 'srsLogicalResourceId'
                       -> Text -- ^ 'srsResourceType'
                       -> ISO8601 -- ^ 'srsLastUpdatedTimestamp'
                       -> ResourceStatus -- ^ 'srsResourceStatus'
                       -> StackResourceSummary
stackResourceSummary p1 p3 p4 p5 = StackResourceSummary
    { _srsLogicalResourceId = p1
    , _srsPhysicalResourceId = Nothing
    , _srsResourceType = p3
    , _srsLastUpdatedTimestamp = p4
    , _srsResourceStatus = p5
    , _srsResourceStatusReason = Nothing
    }

-- | The logical name of the resource specified in the template.
srsLogicalResourceId :: Lens' StackResourceSummary Text
srsLogicalResourceId =
    lens _srsLogicalResourceId (\s a -> s { _srsLogicalResourceId = a })

-- | The name or unique identifier that corresponds to a physical instance ID of
-- the resource.
srsPhysicalResourceId :: Lens' StackResourceSummary (Maybe Text)
srsPhysicalResourceId =
    lens _srsPhysicalResourceId (\s a -> s { _srsPhysicalResourceId = a })

-- | Type of resource. (For more information, go to AWS Resource Types Reference
-- in the AWS CloudFormation User Guide.).
srsResourceType :: Lens' StackResourceSummary Text
srsResourceType = lens _srsResourceType (\s a -> s { _srsResourceType = a })

-- | Time the status was updated.
srsLastUpdatedTimestamp :: Lens' StackResourceSummary ISO8601
srsLastUpdatedTimestamp =
    lens _srsLastUpdatedTimestamp
         (\s a -> s { _srsLastUpdatedTimestamp = a })

-- | Current status of the resource.
srsResourceStatus :: Lens' StackResourceSummary ResourceStatus
srsResourceStatus =
    lens _srsResourceStatus (\s a -> s { _srsResourceStatus = a })

-- | Success/failure message associated with the resource.
srsResourceStatusReason :: Lens' StackResourceSummary (Maybe Text)
srsResourceStatusReason =
    lens _srsResourceStatusReason
         (\s a -> s { _srsResourceStatusReason = a })

instance FromXML StackResourceSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackResourceSummary"

-- | The StackSummary Data Type.
data StackSummary = StackSummary
    { _ssStackId :: Maybe Text
    , _ssStackName :: Text
    , _ssTemplateDescription :: Maybe Text
    , _ssCreationTime :: ISO8601
    , _ssLastUpdatedTime :: Maybe ISO8601
    , _ssDeletionTime :: Maybe ISO8601
    , _ssStackStatus :: StackStatus
    , _ssStackStatusReason :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StackSummary' data type.
--
-- 'StackSummary' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @StackName ::@ @Text@
--
-- * @TemplateDescription ::@ @Maybe Text@
--
-- * @CreationTime ::@ @ISO8601@
--
-- * @LastUpdatedTime ::@ @Maybe ISO8601@
--
-- * @DeletionTime ::@ @Maybe ISO8601@
--
-- * @StackStatus ::@ @StackStatus@
--
-- * @StackStatusReason ::@ @Maybe Text@
--
stackSummary :: Text -- ^ 'ssStackName'
               -> ISO8601 -- ^ 'ssCreationTime'
               -> StackStatus -- ^ 'ssStackStatus'
               -> StackSummary
stackSummary p2 p4 p7 = StackSummary
    { _ssStackId = Nothing
    , _ssStackName = p2
    , _ssTemplateDescription = Nothing
    , _ssCreationTime = p4
    , _ssLastUpdatedTime = Nothing
    , _ssDeletionTime = Nothing
    , _ssStackStatus = p7
    , _ssStackStatusReason = Nothing
    }

-- | Unique stack identifier.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\s a -> s { _ssStackId = a })

-- | The name associated with the stack.
ssStackName :: Lens' StackSummary Text
ssStackName = lens _ssStackName (\s a -> s { _ssStackName = a })

-- | The template description of the template used to create the stack.
ssTemplateDescription :: Lens' StackSummary (Maybe Text)
ssTemplateDescription =
    lens _ssTemplateDescription (\s a -> s { _ssTemplateDescription = a })

-- | The time the stack was created.
ssCreationTime :: Lens' StackSummary ISO8601
ssCreationTime = lens _ssCreationTime (\s a -> s { _ssCreationTime = a })

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
ssLastUpdatedTime :: Lens' StackSummary (Maybe ISO8601)
ssLastUpdatedTime =
    lens _ssLastUpdatedTime (\s a -> s { _ssLastUpdatedTime = a })

-- | The time the stack was deleted.
ssDeletionTime :: Lens' StackSummary (Maybe ISO8601)
ssDeletionTime = lens _ssDeletionTime (\s a -> s { _ssDeletionTime = a })

-- | The current status of the stack.
ssStackStatus :: Lens' StackSummary StackStatus
ssStackStatus = lens _ssStackStatus (\s a -> s { _ssStackStatus = a })

-- | Success/Failure message associated with the stack status.
ssStackStatusReason :: Lens' StackSummary (Maybe Text)
ssStackStatusReason =
    lens _ssStackStatusReason (\s a -> s { _ssStackStatusReason = a })

instance FromXML StackSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StackSummary"

-- | The Tag type is used by CreateStack in the Tags parameter. It allows you to
-- specify a key/value pair that can be used to store information related to
-- cost allocation for an AWS CloudFormation stack.
data Tag = Tag
    { _tKey :: Maybe Text
    , _tValue :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Maybe Text@
--
-- * @Value ::@ @Maybe Text@
--
tag :: Tag
tag = Tag
    { _tKey = Nothing
    , _tValue = Nothing
    }

-- | Required. A string used to identify this tag. You can specify a maximum of
-- 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have
-- the reserved prefix: aws:.
tKey :: Lens' Tag (Maybe Text)
tKey = lens _tKey (\s a -> s { _tKey = a })

-- | Required. A string containing the value for this tag. You can specify a
-- maximum of 256 characters for a tag value.
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToQuery Tag where
    toQuery = genericQuery def

-- | The TemplateParameter data type.
data TemplateParameter = TemplateParameter
    { _tpParameterKey :: Maybe Text
    , _tpDefaultValue :: Maybe Text
    , _tpNoEcho :: Maybe Bool
    , _tpDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TemplateParameter' data type.
--
-- 'TemplateParameter' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterKey ::@ @Maybe Text@
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @NoEcho ::@ @Maybe Bool@
--
-- * @Description ::@ @Maybe Text@
--
templateParameter :: TemplateParameter
templateParameter = TemplateParameter
    { _tpParameterKey = Nothing
    , _tpDefaultValue = Nothing
    , _tpNoEcho = Nothing
    , _tpDescription = Nothing
    }

-- | The name associated with the parameter.
tpParameterKey :: Lens' TemplateParameter (Maybe Text)
tpParameterKey = lens _tpParameterKey (\s a -> s { _tpParameterKey = a })

-- | The default value associated with the parameter.
tpDefaultValue :: Lens' TemplateParameter (Maybe Text)
tpDefaultValue = lens _tpDefaultValue (\s a -> s { _tpDefaultValue = a })

-- | Flag indicating whether the parameter should be displayed as plain text in
-- logs and UIs.
tpNoEcho :: Lens' TemplateParameter (Maybe Bool)
tpNoEcho = lens _tpNoEcho (\s a -> s { _tpNoEcho = a })

-- | User defined description associated with the parameter.
tpDescription :: Lens' TemplateParameter (Maybe Text)
tpDescription = lens _tpDescription (\s a -> s { _tpDescription = a })

instance FromXML TemplateParameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TemplateParameter"
