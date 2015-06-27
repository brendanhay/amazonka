{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFormation.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.CloudFormation.Types
    (
    -- * Service
      CloudFormation

    -- * Errors
    , _InsufficientCapabilitiesException
    , _LimitExceededException
    , _AlreadyExistsException

    -- * Capability
    , Capability (..)

    -- * OnFailure
    , OnFailure (..)

    -- * ResourceSignalStatus
    , ResourceSignalStatus (..)

    -- * ResourceStatus
    , ResourceStatus (..)

    -- * StackStatus
    , StackStatus (..)

    -- * Output
    , Output
    , output
    , outOutputValue
    , outOutputKey
    , outDescription

    -- * Parameter
    , Parameter
    , parameter
    , parParameterValue
    , parParameterKey
    , parUsePreviousValue

    -- * ParameterConstraints
    , ParameterConstraints
    , parameterConstraints
    , pcAllowedValues

    -- * ParameterDeclaration
    , ParameterDeclaration
    , parameterDeclaration
    , pdParameterKey
    , pdParameterType
    , pdParameterConstraints
    , pdDefaultValue
    , pdNoEcho
    , pdDescription

    -- * Stack
    , Stack
    , stack
    , staDisableRollback
    , staLastUpdatedTime
    , staNotificationARNs
    , staStackStatusReason
    , staOutputs
    , staParameters
    , staStackId
    , staCapabilities
    , staDescription
    , staTags
    , staTimeoutInMinutes
    , staStackName
    , staCreationTime
    , staStackStatus

    -- * StackEvent
    , StackEvent
    , stackEvent
    , seLogicalResourceId
    , seResourceStatusReason
    , seResourceType
    , sePhysicalResourceId
    , seResourceProperties
    , seResourceStatus
    , seStackId
    , seEventId
    , seStackName
    , seTimestamp

    -- * StackResource
    , StackResource
    , stackResource
    , srResourceStatusReason
    , srPhysicalResourceId
    , srStackId
    , srDescription
    , srStackName
    , srLogicalResourceId
    , srResourceType
    , srTimestamp
    , srResourceStatus

    -- * StackResourceDetail
    , StackResourceDetail
    , stackResourceDetail
    , srdResourceStatusReason
    , srdPhysicalResourceId
    , srdMetadata
    , srdStackId
    , srdDescription
    , srdStackName
    , srdLogicalResourceId
    , srdResourceType
    , srdLastUpdatedTimestamp
    , srdResourceStatus

    -- * StackResourceSummary
    , StackResourceSummary
    , stackResourceSummary
    , srsResourceStatusReason
    , srsPhysicalResourceId
    , srsLogicalResourceId
    , srsResourceType
    , srsLastUpdatedTimestamp
    , srsResourceStatus

    -- * StackSummary
    , StackSummary
    , stackSummary
    , ssLastUpdatedTime
    , ssTemplateDescription
    , ssStackStatusReason
    , ssDeletionTime
    , ssStackId
    , ssStackName
    , ssCreationTime
    , ssStackStatus

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TemplateParameter
    , TemplateParameter
    , templateParameter
    , tpParameterKey
    , tpDefaultValue
    , tpNoEcho
    , tpDescription
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2010-05-15@ of the Amazon CloudFormation SDK.
data CloudFormation

instance AWSService CloudFormation where
    type Sg CloudFormation = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CloudFormation"
            , _svcPrefix = "cloudformation"
            , _svcVersion = "2010-05-15"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = 80000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The template contains resources with capabilities that were not
-- specified in the Capabilities parameter.
_InsufficientCapabilitiesException :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientCapabilitiesException =
    _ServiceError . hasStatus 400 . hasCode "InsufficientCapabilitiesException"

-- | Quota for the resource has already been reached.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceededException"

-- | Resource with the name requested already exists.
_AlreadyExistsException :: AWSError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException =
    _ServiceError . hasStatus 400 . hasCode "AlreadyExistsException"

data Capability =
    CapabilityIAM
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText Capability where
    parser = takeLowerText >>= \case
        "CAPABILITY_IAM" -> pure CapabilityIAM
        e -> fail ("Failure parsing Capability from " ++ show e)

instance ToText Capability where
    toText = \case
        CapabilityIAM -> "CAPABILITY_IAM"

instance Hashable Capability
instance ToQuery Capability
instance ToHeader Capability

instance FromXML Capability where
    parseXML = parseXMLText "Capability"

data OnFailure
    = Rollback
    | DONothing
    | Delete
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText OnFailure where
    parser = takeLowerText >>= \case
        "DO_NOTHING" -> pure DONothing
        "DELETE" -> pure Delete
        "ROLLBACK" -> pure Rollback
        e -> fail ("Failure parsing OnFailure from " ++ show e)

instance ToText OnFailure where
    toText = \case
        DONothing -> "DO_NOTHING"
        Delete -> "DELETE"
        Rollback -> "ROLLBACK"

instance Hashable OnFailure
instance ToQuery OnFailure
instance ToHeader OnFailure

data ResourceSignalStatus
    = Success
    | Failure
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ResourceSignalStatus where
    parser = takeLowerText >>= \case
        "FAILURE" -> pure Failure
        "SUCCESS" -> pure Success
        e -> fail ("Failure parsing ResourceSignalStatus from " ++ show e)

instance ToText ResourceSignalStatus where
    toText = \case
        Failure -> "FAILURE"
        Success -> "SUCCESS"

instance Hashable ResourceSignalStatus
instance ToQuery ResourceSignalStatus
instance ToHeader ResourceSignalStatus

data ResourceStatus
    = CreateFailed
    | DeleteFailed
    | UpdateFailed
    | CreateComplete
    | UpdateComplete
    | DeleteComplete
    | UpdateINProgress
    | DeleteINProgress
    | DeleteSkipped
    | CreateINProgress
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ResourceStatus where
    parser = takeLowerText >>= \case
        "CREATE_COMPLETE" -> pure CreateComplete
        "CREATE_FAILED" -> pure CreateFailed
        "CREATE_IN_PROGRESS" -> pure CreateINProgress
        "DELETE_COMPLETE" -> pure DeleteComplete
        "DELETE_FAILED" -> pure DeleteFailed
        "DELETE_IN_PROGRESS" -> pure DeleteINProgress
        "DELETE_SKIPPED" -> pure DeleteSkipped
        "UPDATE_COMPLETE" -> pure UpdateComplete
        "UPDATE_FAILED" -> pure UpdateFailed
        "UPDATE_IN_PROGRESS" -> pure UpdateINProgress
        e -> fail ("Failure parsing ResourceStatus from " ++ show e)

instance ToText ResourceStatus where
    toText = \case
        CreateComplete -> "CREATE_COMPLETE"
        CreateFailed -> "CREATE_FAILED"
        CreateINProgress -> "CREATE_IN_PROGRESS"
        DeleteComplete -> "DELETE_COMPLETE"
        DeleteFailed -> "DELETE_FAILED"
        DeleteINProgress -> "DELETE_IN_PROGRESS"
        DeleteSkipped -> "DELETE_SKIPPED"
        UpdateComplete -> "UPDATE_COMPLETE"
        UpdateFailed -> "UPDATE_FAILED"
        UpdateINProgress -> "UPDATE_IN_PROGRESS"

instance Hashable ResourceStatus
instance ToQuery ResourceStatus
instance ToHeader ResourceStatus

instance FromXML ResourceStatus where
    parseXML = parseXMLText "ResourceStatus"

data StackStatus
    = SSUpdateRollbackFailed
    | SSUpdateCompleteCleanupINProgress
    | SSUpdateRollbackINProgress
    | SSCreateINProgress
    | SSRollbackINProgress
    | SSUpdateRollbackCompleteCleanupINProgress
    | SSCreateFailed
    | SSRollbackComplete
    | SSDeleteFailed
    | SSRollbackFailed
    | SSCreateComplete
    | SSDeleteComplete
    | SSUpdateComplete
    | SSDeleteINProgress
    | SSUpdateINProgress
    | SSUpdateRollbackComplete
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText StackStatus where
    parser = takeLowerText >>= \case
        "CREATE_COMPLETE" -> pure SSCreateComplete
        "CREATE_FAILED" -> pure SSCreateFailed
        "CREATE_IN_PROGRESS" -> pure SSCreateINProgress
        "DELETE_COMPLETE" -> pure SSDeleteComplete
        "DELETE_FAILED" -> pure SSDeleteFailed
        "DELETE_IN_PROGRESS" -> pure SSDeleteINProgress
        "ROLLBACK_COMPLETE" -> pure SSRollbackComplete
        "ROLLBACK_FAILED" -> pure SSRollbackFailed
        "ROLLBACK_IN_PROGRESS" -> pure SSRollbackINProgress
        "UPDATE_COMPLETE" -> pure SSUpdateComplete
        "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS" -> pure SSUpdateCompleteCleanupINProgress
        "UPDATE_IN_PROGRESS" -> pure SSUpdateINProgress
        "UPDATE_ROLLBACK_COMPLETE" -> pure SSUpdateRollbackComplete
        "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS" -> pure SSUpdateRollbackCompleteCleanupINProgress
        "UPDATE_ROLLBACK_FAILED" -> pure SSUpdateRollbackFailed
        "UPDATE_ROLLBACK_IN_PROGRESS" -> pure SSUpdateRollbackINProgress
        e -> fail ("Failure parsing StackStatus from " ++ show e)

instance ToText StackStatus where
    toText = \case
        SSCreateComplete -> "CREATE_COMPLETE"
        SSCreateFailed -> "CREATE_FAILED"
        SSCreateINProgress -> "CREATE_IN_PROGRESS"
        SSDeleteComplete -> "DELETE_COMPLETE"
        SSDeleteFailed -> "DELETE_FAILED"
        SSDeleteINProgress -> "DELETE_IN_PROGRESS"
        SSRollbackComplete -> "ROLLBACK_COMPLETE"
        SSRollbackFailed -> "ROLLBACK_FAILED"
        SSRollbackINProgress -> "ROLLBACK_IN_PROGRESS"
        SSUpdateComplete -> "UPDATE_COMPLETE"
        SSUpdateCompleteCleanupINProgress -> "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"
        SSUpdateINProgress -> "UPDATE_IN_PROGRESS"
        SSUpdateRollbackComplete -> "UPDATE_ROLLBACK_COMPLETE"
        SSUpdateRollbackCompleteCleanupINProgress -> "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
        SSUpdateRollbackFailed -> "UPDATE_ROLLBACK_FAILED"
        SSUpdateRollbackINProgress -> "UPDATE_ROLLBACK_IN_PROGRESS"

instance Hashable StackStatus
instance ToQuery StackStatus
instance ToHeader StackStatus

instance FromXML StackStatus where
    parseXML = parseXMLText "StackStatus"

-- | The Output data type.
--
-- /See:/ 'output' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'outOutputValue'
--
-- * 'outOutputKey'
--
-- * 'outDescription'
data Output = Output'
    { _outOutputValue :: Maybe Text
    , _outOutputKey   :: Maybe Text
    , _outDescription :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'Output' smart constructor.
output :: Output
output =
    Output'
    { _outOutputValue = Nothing
    , _outOutputKey = Nothing
    , _outDescription = Nothing
    }

-- | The value associated with the output.
outOutputValue :: Lens' Output (Maybe Text)
outOutputValue = lens _outOutputValue (\ s a -> s{_outOutputValue = a});

-- | The key associated with the output.
outOutputKey :: Lens' Output (Maybe Text)
outOutputKey = lens _outOutputKey (\ s a -> s{_outOutputKey = a});

-- | User defined description associated with the output.
outDescription :: Lens' Output (Maybe Text)
outDescription = lens _outDescription (\ s a -> s{_outDescription = a});

instance FromXML Output where
        parseXML x
          = Output' <$>
              (x .@? "OutputValue") <*> (x .@? "OutputKey") <*>
                (x .@? "Description")

-- | The Parameter data type.
--
-- /See:/ 'parameter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'parParameterValue'
--
-- * 'parParameterKey'
--
-- * 'parUsePreviousValue'
data Parameter = Parameter'
    { _parParameterValue   :: Maybe Text
    , _parParameterKey     :: Maybe Text
    , _parUsePreviousValue :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'Parameter' smart constructor.
parameter :: Parameter
parameter =
    Parameter'
    { _parParameterValue = Nothing
    , _parParameterKey = Nothing
    , _parUsePreviousValue = Nothing
    }

-- | The value associated with the parameter.
parParameterValue :: Lens' Parameter (Maybe Text)
parParameterValue = lens _parParameterValue (\ s a -> s{_parParameterValue = a});

-- | The key associated with the parameter. If you don\'t specify a key and
-- value for a particular parameter, AWS CloudFormation uses the default
-- value that is specified in your template.
parParameterKey :: Lens' Parameter (Maybe Text)
parParameterKey = lens _parParameterKey (\ s a -> s{_parParameterKey = a});

-- | During a stack update, use the existing parameter value that the stack
-- is using for a given parameter key. If you specify @true@, do not
-- specify a parameter value.
parUsePreviousValue :: Lens' Parameter (Maybe Bool)
parUsePreviousValue = lens _parUsePreviousValue (\ s a -> s{_parUsePreviousValue = a});

instance FromXML Parameter where
        parseXML x
          = Parameter' <$>
              (x .@? "ParameterValue") <*> (x .@? "ParameterKey")
                <*> (x .@? "UsePreviousValue")

instance ToQuery Parameter where
        toQuery Parameter'{..}
          = mconcat
              ["ParameterValue" =: _parParameterValue,
               "ParameterKey" =: _parParameterKey,
               "UsePreviousValue" =: _parUsePreviousValue]

-- | A set of criteria that AWS CloudFormation uses to validate parameter
-- values. Although other constraints might be defined in the stack
-- template, AWS CloudFormation returns only the @AllowedValues@ property.
--
-- /See:/ 'parameterConstraints' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pcAllowedValues'
newtype ParameterConstraints = ParameterConstraints'
    { _pcAllowedValues :: Maybe [Text]
    } deriving (Eq,Read,Show)

-- | 'ParameterConstraints' smart constructor.
parameterConstraints :: ParameterConstraints
parameterConstraints =
    ParameterConstraints'
    { _pcAllowedValues = Nothing
    }

-- | A list of values that are permitted for a parameter.
pcAllowedValues :: Lens' ParameterConstraints [Text]
pcAllowedValues = lens _pcAllowedValues (\ s a -> s{_pcAllowedValues = a}) . _Default;

instance FromXML ParameterConstraints where
        parseXML x
          = ParameterConstraints' <$>
              (x .@? "AllowedValues" .!@ mempty >>=
                 may (parseXMLList "member"))

-- | The ParameterDeclaration data type.
--
-- /See:/ 'parameterDeclaration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdParameterKey'
--
-- * 'pdParameterType'
--
-- * 'pdParameterConstraints'
--
-- * 'pdDefaultValue'
--
-- * 'pdNoEcho'
--
-- * 'pdDescription'
data ParameterDeclaration = ParameterDeclaration'
    { _pdParameterKey         :: Maybe Text
    , _pdParameterType        :: Maybe Text
    , _pdParameterConstraints :: Maybe ParameterConstraints
    , _pdDefaultValue         :: Maybe Text
    , _pdNoEcho               :: Maybe Bool
    , _pdDescription          :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'ParameterDeclaration' smart constructor.
parameterDeclaration :: ParameterDeclaration
parameterDeclaration =
    ParameterDeclaration'
    { _pdParameterKey = Nothing
    , _pdParameterType = Nothing
    , _pdParameterConstraints = Nothing
    , _pdDefaultValue = Nothing
    , _pdNoEcho = Nothing
    , _pdDescription = Nothing
    }

-- | The name that is associated with the parameter.
pdParameterKey :: Lens' ParameterDeclaration (Maybe Text)
pdParameterKey = lens _pdParameterKey (\ s a -> s{_pdParameterKey = a});

-- | The type of parameter.
pdParameterType :: Lens' ParameterDeclaration (Maybe Text)
pdParameterType = lens _pdParameterType (\ s a -> s{_pdParameterType = a});

-- | The criteria that AWS CloudFormation uses to validate parameter values.
pdParameterConstraints :: Lens' ParameterDeclaration (Maybe ParameterConstraints)
pdParameterConstraints = lens _pdParameterConstraints (\ s a -> s{_pdParameterConstraints = a});

-- | The default value of the parameter.
pdDefaultValue :: Lens' ParameterDeclaration (Maybe Text)
pdDefaultValue = lens _pdDefaultValue (\ s a -> s{_pdDefaultValue = a});

-- | Flag that indicates whether the parameter value is shown as plain text
-- in logs and in the AWS Management Console.
pdNoEcho :: Lens' ParameterDeclaration (Maybe Bool)
pdNoEcho = lens _pdNoEcho (\ s a -> s{_pdNoEcho = a});

-- | The description that is associate with the parameter.
pdDescription :: Lens' ParameterDeclaration (Maybe Text)
pdDescription = lens _pdDescription (\ s a -> s{_pdDescription = a});

instance FromXML ParameterDeclaration where
        parseXML x
          = ParameterDeclaration' <$>
              (x .@? "ParameterKey") <*> (x .@? "ParameterType")
                <*> (x .@? "ParameterConstraints")
                <*> (x .@? "DefaultValue")
                <*> (x .@? "NoEcho")
                <*> (x .@? "Description")

-- | The Stack data type.
--
-- /See:/ 'stack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staDisableRollback'
--
-- * 'staLastUpdatedTime'
--
-- * 'staNotificationARNs'
--
-- * 'staStackStatusReason'
--
-- * 'staOutputs'
--
-- * 'staParameters'
--
-- * 'staStackId'
--
-- * 'staCapabilities'
--
-- * 'staDescription'
--
-- * 'staTags'
--
-- * 'staTimeoutInMinutes'
--
-- * 'staStackName'
--
-- * 'staCreationTime'
--
-- * 'staStackStatus'
data Stack = Stack'
    { _staDisableRollback   :: Maybe Bool
    , _staLastUpdatedTime   :: Maybe ISO8601
    , _staNotificationARNs  :: Maybe [Text]
    , _staStackStatusReason :: Maybe Text
    , _staOutputs           :: Maybe [Output]
    , _staParameters        :: Maybe [Parameter]
    , _staStackId           :: Maybe Text
    , _staCapabilities      :: Maybe [Capability]
    , _staDescription       :: Maybe Text
    , _staTags              :: Maybe [Tag]
    , _staTimeoutInMinutes  :: Maybe Nat
    , _staStackName         :: Text
    , _staCreationTime      :: ISO8601
    , _staStackStatus       :: StackStatus
    } deriving (Eq,Read,Show)

-- | 'Stack' smart constructor.
stack :: Text -> UTCTime -> StackStatus -> Stack
stack pStackName pCreationTime pStackStatus =
    Stack'
    { _staDisableRollback = Nothing
    , _staLastUpdatedTime = Nothing
    , _staNotificationARNs = Nothing
    , _staStackStatusReason = Nothing
    , _staOutputs = Nothing
    , _staParameters = Nothing
    , _staStackId = Nothing
    , _staCapabilities = Nothing
    , _staDescription = Nothing
    , _staTags = Nothing
    , _staTimeoutInMinutes = Nothing
    , _staStackName = pStackName
    , _staCreationTime = _Time # pCreationTime
    , _staStackStatus = pStackStatus
    }

-- | Boolean to enable or disable rollback on stack creation failures:
--
-- -   @true@: disable rollback
-- -   @false@: enable rollback
staDisableRollback :: Lens' Stack (Maybe Bool)
staDisableRollback = lens _staDisableRollback (\ s a -> s{_staDisableRollback = a});

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
staLastUpdatedTime :: Lens' Stack (Maybe UTCTime)
staLastUpdatedTime = lens _staLastUpdatedTime (\ s a -> s{_staLastUpdatedTime = a}) . mapping _Time;

-- | SNS topic ARNs to which stack related events are published.
staNotificationARNs :: Lens' Stack [Text]
staNotificationARNs = lens _staNotificationARNs (\ s a -> s{_staNotificationARNs = a}) . _Default;

-- | Success\/failure message associated with the stack status.
staStackStatusReason :: Lens' Stack (Maybe Text)
staStackStatusReason = lens _staStackStatusReason (\ s a -> s{_staStackStatusReason = a});

-- | A list of output structures.
staOutputs :: Lens' Stack [Output]
staOutputs = lens _staOutputs (\ s a -> s{_staOutputs = a}) . _Default;

-- | A list of @Parameter@ structures.
staParameters :: Lens' Stack [Parameter]
staParameters = lens _staParameters (\ s a -> s{_staParameters = a}) . _Default;

-- | Unique identifier of the stack.
staStackId :: Lens' Stack (Maybe Text)
staStackId = lens _staStackId (\ s a -> s{_staStackId = a});

-- | The capabilities allowed in the stack.
staCapabilities :: Lens' Stack [Capability]
staCapabilities = lens _staCapabilities (\ s a -> s{_staCapabilities = a}) . _Default;

-- | User defined description associated with the stack.
staDescription :: Lens' Stack (Maybe Text)
staDescription = lens _staDescription (\ s a -> s{_staDescription = a});

-- | A list of @Tag@s that specify cost allocation information for the stack.
staTags :: Lens' Stack [Tag]
staTags = lens _staTags (\ s a -> s{_staTags = a}) . _Default;

-- | The amount of time within which stack creation should complete.
staTimeoutInMinutes :: Lens' Stack (Maybe Natural)
staTimeoutInMinutes = lens _staTimeoutInMinutes (\ s a -> s{_staTimeoutInMinutes = a}) . mapping _Nat;

-- | The name associated with the stack.
staStackName :: Lens' Stack Text
staStackName = lens _staStackName (\ s a -> s{_staStackName = a});

-- | Time at which the stack was created.
staCreationTime :: Lens' Stack UTCTime
staCreationTime = lens _staCreationTime (\ s a -> s{_staCreationTime = a}) . _Time;

-- | Current status of the stack.
staStackStatus :: Lens' Stack StackStatus
staStackStatus = lens _staStackStatus (\ s a -> s{_staStackStatus = a});

instance FromXML Stack where
        parseXML x
          = Stack' <$>
              (x .@? "DisableRollback") <*>
                (x .@? "LastUpdatedTime")
                <*>
                (x .@? "NotificationARNs" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StackStatusReason")
                <*>
                (x .@? "Outputs" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StackId")
                <*>
                (x .@? "Capabilities" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Description")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "TimeoutInMinutes")
                <*> (x .@ "StackName")
                <*> (x .@ "CreationTime")
                <*> (x .@ "StackStatus")

-- | The StackEvent data type.
--
-- /See:/ 'stackEvent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seLogicalResourceId'
--
-- * 'seResourceStatusReason'
--
-- * 'seResourceType'
--
-- * 'sePhysicalResourceId'
--
-- * 'seResourceProperties'
--
-- * 'seResourceStatus'
--
-- * 'seStackId'
--
-- * 'seEventId'
--
-- * 'seStackName'
--
-- * 'seTimestamp'
data StackEvent = StackEvent'
    { _seLogicalResourceId    :: Maybe Text
    , _seResourceStatusReason :: Maybe Text
    , _seResourceType         :: Maybe Text
    , _sePhysicalResourceId   :: Maybe Text
    , _seResourceProperties   :: Maybe Text
    , _seResourceStatus       :: Maybe ResourceStatus
    , _seStackId              :: Text
    , _seEventId              :: Text
    , _seStackName            :: Text
    , _seTimestamp            :: ISO8601
    } deriving (Eq,Read,Show)

-- | 'StackEvent' smart constructor.
stackEvent :: Text -> Text -> Text -> UTCTime -> StackEvent
stackEvent pStackId pEventId pStackName pTimestamp =
    StackEvent'
    { _seLogicalResourceId = Nothing
    , _seResourceStatusReason = Nothing
    , _seResourceType = Nothing
    , _sePhysicalResourceId = Nothing
    , _seResourceProperties = Nothing
    , _seResourceStatus = Nothing
    , _seStackId = pStackId
    , _seEventId = pEventId
    , _seStackName = pStackName
    , _seTimestamp = _Time # pTimestamp
    }

-- | The logical name of the resource specified in the template.
seLogicalResourceId :: Lens' StackEvent (Maybe Text)
seLogicalResourceId = lens _seLogicalResourceId (\ s a -> s{_seLogicalResourceId = a});

-- | Success\/failure message associated with the resource.
seResourceStatusReason :: Lens' StackEvent (Maybe Text)
seResourceStatusReason = lens _seResourceStatusReason (\ s a -> s{_seResourceStatusReason = a});

-- | Type of resource. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
seResourceType :: Lens' StackEvent (Maybe Text)
seResourceType = lens _seResourceType (\ s a -> s{_seResourceType = a});

-- | The name or unique identifier associated with the physical instance of
-- the resource.
sePhysicalResourceId :: Lens' StackEvent (Maybe Text)
sePhysicalResourceId = lens _sePhysicalResourceId (\ s a -> s{_sePhysicalResourceId = a});

-- | BLOB of the properties used to create the resource.
seResourceProperties :: Lens' StackEvent (Maybe Text)
seResourceProperties = lens _seResourceProperties (\ s a -> s{_seResourceProperties = a});

-- | Current status of the resource.
seResourceStatus :: Lens' StackEvent (Maybe ResourceStatus)
seResourceStatus = lens _seResourceStatus (\ s a -> s{_seResourceStatus = a});

-- | The unique ID name of the instance of the stack.
seStackId :: Lens' StackEvent Text
seStackId = lens _seStackId (\ s a -> s{_seStackId = a});

-- | The unique ID of this event.
seEventId :: Lens' StackEvent Text
seEventId = lens _seEventId (\ s a -> s{_seEventId = a});

-- | The name associated with a stack.
seStackName :: Lens' StackEvent Text
seStackName = lens _seStackName (\ s a -> s{_seStackName = a});

-- | Time the status was updated.
seTimestamp :: Lens' StackEvent UTCTime
seTimestamp = lens _seTimestamp (\ s a -> s{_seTimestamp = a}) . _Time;

instance FromXML StackEvent where
        parseXML x
          = StackEvent' <$>
              (x .@? "LogicalResourceId") <*>
                (x .@? "ResourceStatusReason")
                <*> (x .@? "ResourceType")
                <*> (x .@? "PhysicalResourceId")
                <*> (x .@? "ResourceProperties")
                <*> (x .@? "ResourceStatus")
                <*> (x .@ "StackId")
                <*> (x .@ "EventId")
                <*> (x .@ "StackName")
                <*> (x .@ "Timestamp")

-- | The StackResource data type.
--
-- /See:/ 'stackResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srResourceStatusReason'
--
-- * 'srPhysicalResourceId'
--
-- * 'srStackId'
--
-- * 'srDescription'
--
-- * 'srStackName'
--
-- * 'srLogicalResourceId'
--
-- * 'srResourceType'
--
-- * 'srTimestamp'
--
-- * 'srResourceStatus'
data StackResource = StackResource'
    { _srResourceStatusReason :: Maybe Text
    , _srPhysicalResourceId   :: Maybe Text
    , _srStackId              :: Maybe Text
    , _srDescription          :: Maybe Text
    , _srStackName            :: Maybe Text
    , _srLogicalResourceId    :: Text
    , _srResourceType         :: Text
    , _srTimestamp            :: ISO8601
    , _srResourceStatus       :: ResourceStatus
    } deriving (Eq,Read,Show)

-- | 'StackResource' smart constructor.
stackResource :: Text -> Text -> UTCTime -> ResourceStatus -> StackResource
stackResource pLogicalResourceId pResourceType pTimestamp pResourceStatus =
    StackResource'
    { _srResourceStatusReason = Nothing
    , _srPhysicalResourceId = Nothing
    , _srStackId = Nothing
    , _srDescription = Nothing
    , _srStackName = Nothing
    , _srLogicalResourceId = pLogicalResourceId
    , _srResourceType = pResourceType
    , _srTimestamp = _Time # pTimestamp
    , _srResourceStatus = pResourceStatus
    }

-- | Success\/failure message associated with the resource.
srResourceStatusReason :: Lens' StackResource (Maybe Text)
srResourceStatusReason = lens _srResourceStatusReason (\ s a -> s{_srResourceStatusReason = a});

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
srPhysicalResourceId :: Lens' StackResource (Maybe Text)
srPhysicalResourceId = lens _srPhysicalResourceId (\ s a -> s{_srPhysicalResourceId = a});

-- | Unique identifier of the stack.
srStackId :: Lens' StackResource (Maybe Text)
srStackId = lens _srStackId (\ s a -> s{_srStackId = a});

-- | User defined description associated with the resource.
srDescription :: Lens' StackResource (Maybe Text)
srDescription = lens _srDescription (\ s a -> s{_srDescription = a});

-- | The name associated with the stack.
srStackName :: Lens' StackResource (Maybe Text)
srStackName = lens _srStackName (\ s a -> s{_srStackName = a});

-- | The logical name of the resource specified in the template.
srLogicalResourceId :: Lens' StackResource Text
srLogicalResourceId = lens _srLogicalResourceId (\ s a -> s{_srLogicalResourceId = a});

-- | Type of resource. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
srResourceType :: Lens' StackResource Text
srResourceType = lens _srResourceType (\ s a -> s{_srResourceType = a});

-- | Time the status was updated.
srTimestamp :: Lens' StackResource UTCTime
srTimestamp = lens _srTimestamp (\ s a -> s{_srTimestamp = a}) . _Time;

-- | Current status of the resource.
srResourceStatus :: Lens' StackResource ResourceStatus
srResourceStatus = lens _srResourceStatus (\ s a -> s{_srResourceStatus = a});

instance FromXML StackResource where
        parseXML x
          = StackResource' <$>
              (x .@? "ResourceStatusReason") <*>
                (x .@? "PhysicalResourceId")
                <*> (x .@? "StackId")
                <*> (x .@? "Description")
                <*> (x .@? "StackName")
                <*> (x .@ "LogicalResourceId")
                <*> (x .@ "ResourceType")
                <*> (x .@ "Timestamp")
                <*> (x .@ "ResourceStatus")

-- | Contains detailed information about the specified stack resource.
--
-- /See:/ 'stackResourceDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srdResourceStatusReason'
--
-- * 'srdPhysicalResourceId'
--
-- * 'srdMetadata'
--
-- * 'srdStackId'
--
-- * 'srdDescription'
--
-- * 'srdStackName'
--
-- * 'srdLogicalResourceId'
--
-- * 'srdResourceType'
--
-- * 'srdLastUpdatedTimestamp'
--
-- * 'srdResourceStatus'
data StackResourceDetail = StackResourceDetail'
    { _srdResourceStatusReason :: Maybe Text
    , _srdPhysicalResourceId   :: Maybe Text
    , _srdMetadata             :: Maybe Text
    , _srdStackId              :: Maybe Text
    , _srdDescription          :: Maybe Text
    , _srdStackName            :: Maybe Text
    , _srdLogicalResourceId    :: Text
    , _srdResourceType         :: Text
    , _srdLastUpdatedTimestamp :: ISO8601
    , _srdResourceStatus       :: ResourceStatus
    } deriving (Eq,Read,Show)

-- | 'StackResourceDetail' smart constructor.
stackResourceDetail :: Text -> Text -> UTCTime -> ResourceStatus -> StackResourceDetail
stackResourceDetail pLogicalResourceId pResourceType pLastUpdatedTimestamp pResourceStatus =
    StackResourceDetail'
    { _srdResourceStatusReason = Nothing
    , _srdPhysicalResourceId = Nothing
    , _srdMetadata = Nothing
    , _srdStackId = Nothing
    , _srdDescription = Nothing
    , _srdStackName = Nothing
    , _srdLogicalResourceId = pLogicalResourceId
    , _srdResourceType = pResourceType
    , _srdLastUpdatedTimestamp = _Time # pLastUpdatedTimestamp
    , _srdResourceStatus = pResourceStatus
    }

-- | Success\/failure message associated with the resource.
srdResourceStatusReason :: Lens' StackResourceDetail (Maybe Text)
srdResourceStatusReason = lens _srdResourceStatusReason (\ s a -> s{_srdResourceStatusReason = a});

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
srdPhysicalResourceId :: Lens' StackResourceDetail (Maybe Text)
srdPhysicalResourceId = lens _srdPhysicalResourceId (\ s a -> s{_srdPhysicalResourceId = a});

-- | The JSON format content of the @Metadata@ attribute declared for the
-- resource. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute>
-- in the AWS CloudFormation User Guide.
srdMetadata :: Lens' StackResourceDetail (Maybe Text)
srdMetadata = lens _srdMetadata (\ s a -> s{_srdMetadata = a});

-- | Unique identifier of the stack.
srdStackId :: Lens' StackResourceDetail (Maybe Text)
srdStackId = lens _srdStackId (\ s a -> s{_srdStackId = a});

-- | User defined description associated with the resource.
srdDescription :: Lens' StackResourceDetail (Maybe Text)
srdDescription = lens _srdDescription (\ s a -> s{_srdDescription = a});

-- | The name associated with the stack.
srdStackName :: Lens' StackResourceDetail (Maybe Text)
srdStackName = lens _srdStackName (\ s a -> s{_srdStackName = a});

-- | The logical name of the resource specified in the template.
srdLogicalResourceId :: Lens' StackResourceDetail Text
srdLogicalResourceId = lens _srdLogicalResourceId (\ s a -> s{_srdLogicalResourceId = a});

-- | Type of resource. ((For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
srdResourceType :: Lens' StackResourceDetail Text
srdResourceType = lens _srdResourceType (\ s a -> s{_srdResourceType = a});

-- | Time the status was updated.
srdLastUpdatedTimestamp :: Lens' StackResourceDetail UTCTime
srdLastUpdatedTimestamp = lens _srdLastUpdatedTimestamp (\ s a -> s{_srdLastUpdatedTimestamp = a}) . _Time;

-- | Current status of the resource.
srdResourceStatus :: Lens' StackResourceDetail ResourceStatus
srdResourceStatus = lens _srdResourceStatus (\ s a -> s{_srdResourceStatus = a});

instance FromXML StackResourceDetail where
        parseXML x
          = StackResourceDetail' <$>
              (x .@? "ResourceStatusReason") <*>
                (x .@? "PhysicalResourceId")
                <*> (x .@? "Metadata")
                <*> (x .@? "StackId")
                <*> (x .@? "Description")
                <*> (x .@? "StackName")
                <*> (x .@ "LogicalResourceId")
                <*> (x .@ "ResourceType")
                <*> (x .@ "LastUpdatedTimestamp")
                <*> (x .@ "ResourceStatus")

-- | Contains high-level information about the specified stack resource.
--
-- /See:/ 'stackResourceSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srsResourceStatusReason'
--
-- * 'srsPhysicalResourceId'
--
-- * 'srsLogicalResourceId'
--
-- * 'srsResourceType'
--
-- * 'srsLastUpdatedTimestamp'
--
-- * 'srsResourceStatus'
data StackResourceSummary = StackResourceSummary'
    { _srsResourceStatusReason :: Maybe Text
    , _srsPhysicalResourceId   :: Maybe Text
    , _srsLogicalResourceId    :: Text
    , _srsResourceType         :: Text
    , _srsLastUpdatedTimestamp :: ISO8601
    , _srsResourceStatus       :: ResourceStatus
    } deriving (Eq,Read,Show)

-- | 'StackResourceSummary' smart constructor.
stackResourceSummary :: Text -> Text -> UTCTime -> ResourceStatus -> StackResourceSummary
stackResourceSummary pLogicalResourceId pResourceType pLastUpdatedTimestamp pResourceStatus =
    StackResourceSummary'
    { _srsResourceStatusReason = Nothing
    , _srsPhysicalResourceId = Nothing
    , _srsLogicalResourceId = pLogicalResourceId
    , _srsResourceType = pResourceType
    , _srsLastUpdatedTimestamp = _Time # pLastUpdatedTimestamp
    , _srsResourceStatus = pResourceStatus
    }

-- | Success\/failure message associated with the resource.
srsResourceStatusReason :: Lens' StackResourceSummary (Maybe Text)
srsResourceStatusReason = lens _srsResourceStatusReason (\ s a -> s{_srsResourceStatusReason = a});

-- | The name or unique identifier that corresponds to a physical instance ID
-- of the resource.
srsPhysicalResourceId :: Lens' StackResourceSummary (Maybe Text)
srsPhysicalResourceId = lens _srsPhysicalResourceId (\ s a -> s{_srsPhysicalResourceId = a});

-- | The logical name of the resource specified in the template.
srsLogicalResourceId :: Lens' StackResourceSummary Text
srsLogicalResourceId = lens _srsLogicalResourceId (\ s a -> s{_srsLogicalResourceId = a});

-- | Type of resource. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
srsResourceType :: Lens' StackResourceSummary Text
srsResourceType = lens _srsResourceType (\ s a -> s{_srsResourceType = a});

-- | Time the status was updated.
srsLastUpdatedTimestamp :: Lens' StackResourceSummary UTCTime
srsLastUpdatedTimestamp = lens _srsLastUpdatedTimestamp (\ s a -> s{_srsLastUpdatedTimestamp = a}) . _Time;

-- | Current status of the resource.
srsResourceStatus :: Lens' StackResourceSummary ResourceStatus
srsResourceStatus = lens _srsResourceStatus (\ s a -> s{_srsResourceStatus = a});

instance FromXML StackResourceSummary where
        parseXML x
          = StackResourceSummary' <$>
              (x .@? "ResourceStatusReason") <*>
                (x .@? "PhysicalResourceId")
                <*> (x .@ "LogicalResourceId")
                <*> (x .@ "ResourceType")
                <*> (x .@ "LastUpdatedTimestamp")
                <*> (x .@ "ResourceStatus")

-- | The StackSummary Data Type
--
-- /See:/ 'stackSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssLastUpdatedTime'
--
-- * 'ssTemplateDescription'
--
-- * 'ssStackStatusReason'
--
-- * 'ssDeletionTime'
--
-- * 'ssStackId'
--
-- * 'ssStackName'
--
-- * 'ssCreationTime'
--
-- * 'ssStackStatus'
data StackSummary = StackSummary'
    { _ssLastUpdatedTime     :: Maybe ISO8601
    , _ssTemplateDescription :: Maybe Text
    , _ssStackStatusReason   :: Maybe Text
    , _ssDeletionTime        :: Maybe ISO8601
    , _ssStackId             :: Maybe Text
    , _ssStackName           :: Text
    , _ssCreationTime        :: ISO8601
    , _ssStackStatus         :: StackStatus
    } deriving (Eq,Read,Show)

-- | 'StackSummary' smart constructor.
stackSummary :: Text -> UTCTime -> StackStatus -> StackSummary
stackSummary pStackName pCreationTime pStackStatus =
    StackSummary'
    { _ssLastUpdatedTime = Nothing
    , _ssTemplateDescription = Nothing
    , _ssStackStatusReason = Nothing
    , _ssDeletionTime = Nothing
    , _ssStackId = Nothing
    , _ssStackName = pStackName
    , _ssCreationTime = _Time # pCreationTime
    , _ssStackStatus = pStackStatus
    }

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
ssLastUpdatedTime :: Lens' StackSummary (Maybe UTCTime)
ssLastUpdatedTime = lens _ssLastUpdatedTime (\ s a -> s{_ssLastUpdatedTime = a}) . mapping _Time;

-- | The template description of the template used to create the stack.
ssTemplateDescription :: Lens' StackSummary (Maybe Text)
ssTemplateDescription = lens _ssTemplateDescription (\ s a -> s{_ssTemplateDescription = a});

-- | Success\/Failure message associated with the stack status.
ssStackStatusReason :: Lens' StackSummary (Maybe Text)
ssStackStatusReason = lens _ssStackStatusReason (\ s a -> s{_ssStackStatusReason = a});

-- | The time the stack was deleted.
ssDeletionTime :: Lens' StackSummary (Maybe UTCTime)
ssDeletionTime = lens _ssDeletionTime (\ s a -> s{_ssDeletionTime = a}) . mapping _Time;

-- | Unique stack identifier.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\ s a -> s{_ssStackId = a});

-- | The name associated with the stack.
ssStackName :: Lens' StackSummary Text
ssStackName = lens _ssStackName (\ s a -> s{_ssStackName = a});

-- | The time the stack was created.
ssCreationTime :: Lens' StackSummary UTCTime
ssCreationTime = lens _ssCreationTime (\ s a -> s{_ssCreationTime = a}) . _Time;

-- | The current status of the stack.
ssStackStatus :: Lens' StackSummary StackStatus
ssStackStatus = lens _ssStackStatus (\ s a -> s{_ssStackStatus = a});

instance FromXML StackSummary where
        parseXML x
          = StackSummary' <$>
              (x .@? "LastUpdatedTime") <*>
                (x .@? "TemplateDescription")
                <*> (x .@? "StackStatusReason")
                <*> (x .@? "DeletionTime")
                <*> (x .@? "StackId")
                <*> (x .@ "StackName")
                <*> (x .@ "CreationTime")
                <*> (x .@ "StackStatus")

-- | The Tag type is used by @CreateStack@ in the @Tags@ parameter. It allows
-- you to specify a key\/value pair that can be used to store information
-- related to cost allocation for an AWS CloudFormation stack.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'
    { _tagValue :: Maybe Text
    , _tagKey   :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'Tag' smart constructor.
tag :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | /Required/. A string containing the value for this tag. You can specify
-- a maximum of 256 characters for a tag value.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | /Required/. A string used to identify this tag. You can specify a
-- maximum of 128 characters for a tag key. Tags owned by Amazon Web
-- Services (AWS) have the reserved prefix: @aws:@.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | The TemplateParameter data type.
--
-- /See:/ 'templateParameter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tpParameterKey'
--
-- * 'tpDefaultValue'
--
-- * 'tpNoEcho'
--
-- * 'tpDescription'
data TemplateParameter = TemplateParameter'
    { _tpParameterKey :: Maybe Text
    , _tpDefaultValue :: Maybe Text
    , _tpNoEcho       :: Maybe Bool
    , _tpDescription  :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'TemplateParameter' smart constructor.
templateParameter :: TemplateParameter
templateParameter =
    TemplateParameter'
    { _tpParameterKey = Nothing
    , _tpDefaultValue = Nothing
    , _tpNoEcho = Nothing
    , _tpDescription = Nothing
    }

-- | The name associated with the parameter.
tpParameterKey :: Lens' TemplateParameter (Maybe Text)
tpParameterKey = lens _tpParameterKey (\ s a -> s{_tpParameterKey = a});

-- | The default value associated with the parameter.
tpDefaultValue :: Lens' TemplateParameter (Maybe Text)
tpDefaultValue = lens _tpDefaultValue (\ s a -> s{_tpDefaultValue = a});

-- | Flag indicating whether the parameter should be displayed as plain text
-- in logs and UIs.
tpNoEcho :: Lens' TemplateParameter (Maybe Bool)
tpNoEcho = lens _tpNoEcho (\ s a -> s{_tpNoEcho = a});

-- | User defined description associated with the parameter.
tpDescription :: Lens' TemplateParameter (Maybe Text)
tpDescription = lens _tpDescription (\ s a -> s{_tpDescription = a});

instance FromXML TemplateParameter where
        parseXML x
          = TemplateParameter' <$>
              (x .@? "ParameterKey") <*> (x .@? "DefaultValue") <*>
                (x .@? "NoEcho")
                <*> (x .@? "Description")
