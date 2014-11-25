{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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

module Network.AWS.CloudFormation.Types
    (
    -- * Service
      CloudFormation
    -- ** Error
    , RESTError
    -- ** XML
    , ns

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * StackStatus
    , StackStatus (..)

    -- * StackEvent
    , StackEvent
    , stackEvent
    , seEventId
    , seLogicalResourceId
    , sePhysicalResourceId
    , seResourceProperties
    , seResourceStatus
    , seResourceStatusReason
    , seResourceType
    , seStackId
    , seStackName
    , seTimestamp

    -- * StackSummary
    , StackSummary
    , stackSummary
    , ssCreationTime
    , ssDeletionTime
    , ssLastUpdatedTime
    , ssStackId
    , ssStackName
    , ssStackStatus
    , ssStackStatusReason
    , ssTemplateDescription

    -- * StackResourceDetail
    , StackResourceDetail
    , stackResourceDetail
    , srdDescription
    , srdLastUpdatedTimestamp
    , srdLogicalResourceId
    , srdMetadata
    , srdPhysicalResourceId
    , srdResourceStatus
    , srdResourceStatusReason
    , srdResourceType
    , srdStackId
    , srdStackName

    -- * ResourceStatus
    , ResourceStatus (..)

    -- * TemplateParameter
    , TemplateParameter
    , templateParameter
    , tpDefaultValue
    , tpDescription
    , tpNoEcho
    , tpParameterKey

    -- * ParameterDeclaration
    , ParameterDeclaration
    , parameterDeclaration
    , pdDefaultValue
    , pdDescription
    , pdNoEcho
    , pdParameterKey
    , pdParameterType

    -- * StackResource
    , StackResource
    , stackResource
    , sr1Description
    , sr1LogicalResourceId
    , sr1PhysicalResourceId
    , sr1ResourceStatus
    , sr1ResourceStatusReason
    , sr1ResourceType
    , sr1StackId
    , sr1StackName
    , sr1Timestamp

    -- * Output
    , Output
    , output
    , oDescription
    , oOutputKey
    , oOutputValue

    -- * StackResourceSummary
    , StackResourceSummary
    , stackResourceSummary
    , srsLastUpdatedTimestamp
    , srsLogicalResourceId
    , srsPhysicalResourceId
    , srsResourceStatus
    , srsResourceStatusReason
    , srsResourceType

    -- * Capability
    , Capability (..)

    -- * ResourceSignalStatus
    , ResourceSignalStatus (..)

    -- * Stack
    , Stack
    , stack
    , sCapabilities
    , sCreationTime
    , sDescription
    , sDisableRollback
    , sLastUpdatedTime
    , sNotificationARNs
    , sOutputs
    , sParameters
    , sStackId
    , sStackName
    , sStackStatus
    , sStackStatusReason
    , sTags
    , sTimeoutInMinutes

    -- * OnFailure
    , OnFailure (..)

    -- * Parameter
    , Parameter
    , parameter
    , pParameterKey
    , pParameterValue
    , pUsePreviousValue
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Version @2010-05-15@ of the Amazon CloudFormation service.
data CloudFormation

instance AWSService CloudFormation where
    type Sg CloudFormation = V4
    type Er CloudFormation = RESTError

    service = Service
        { _svcEndpoint     = regional
        , _svcAbbrev       = "CloudFormation"
        , _svcPrefix       = "cloudformation"
        , _svcVersion      = "2010-05-15"
        , _svcTargetPrefix = Nothing
        , _svcJSONVersion  = Nothing
        }

    handle = restError statusSuccess

ns :: Text
ns = "http://cloudformation.amazonaws.com/doc/2010-05-15/"

data Tag = Tag
    { _tagKey   :: Maybe Text
    , _tagValue :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Tag' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey' @::@ 'Maybe' 'Text'
--
-- * 'tagValue' @::@ 'Maybe' 'Text'
--
tag :: Tag
tag = Tag
    { _tagKey   = Nothing
    , _tagValue = Nothing
    }

-- | /Required/. A string used to identify this tag. You can specify a maximum of
-- 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have
-- the reserved prefix: 'aws:'.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | /Required/. A string containing the value for this tag. You can specify a
-- maximum of 256 characters for a tag value.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromXML Tag where
    parseXML x = Tag
        <$> x .@? "Key"
        <*> x .@? "Value"

instance ToQuery Tag where
    toQuery Tag{..} = mconcat
        [ "Key"   =? _tagKey
        , "Value" =? _tagValue
        ]

data StackStatus
    = CreateComplete                          -- ^ CREATE_COMPLETE
    | CreateFailed                            -- ^ CREATE_FAILED
    | CreateInProgress                        -- ^ CREATE_IN_PROGRESS
    | DeleteComplete                          -- ^ DELETE_COMPLETE
    | DeleteFailed                            -- ^ DELETE_FAILED
    | DeleteInProgress                        -- ^ DELETE_IN_PROGRESS
    | RollbackComplete                        -- ^ ROLLBACK_COMPLETE
    | RollbackFailed                          -- ^ ROLLBACK_FAILED
    | RollbackInProgress                      -- ^ ROLLBACK_IN_PROGRESS
    | UpdateComplete                          -- ^ UPDATE_COMPLETE
    | UpdateCompleteCleanupInProgress         -- ^ UPDATE_COMPLETE_CLEANUP_IN_PROGRESS
    | UpdateInProgress                        -- ^ UPDATE_IN_PROGRESS
    | UpdateRollbackComplete                  -- ^ UPDATE_ROLLBACK_COMPLETE
    | UpdateRollbackCompleteCleanupInProgress -- ^ UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS
    | UpdateRollbackFailed                    -- ^ UPDATE_ROLLBACK_FAILED
    | UpdateRollbackInProgress                -- ^ UPDATE_ROLLBACK_IN_PROGRESS
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable StackStatus

instance FromText StackStatus where
    parser = takeText >>= \case
        "CREATE_COMPLETE"                              -> pure CreateComplete
        "CREATE_FAILED"                                -> pure CreateFailed
        "CREATE_IN_PROGRESS"                           -> pure CreateInProgress
        "DELETE_COMPLETE"                              -> pure DeleteComplete
        "DELETE_FAILED"                                -> pure DeleteFailed
        "DELETE_IN_PROGRESS"                           -> pure DeleteInProgress
        "ROLLBACK_COMPLETE"                            -> pure RollbackComplete
        "ROLLBACK_FAILED"                              -> pure RollbackFailed
        "ROLLBACK_IN_PROGRESS"                         -> pure RollbackInProgress
        "UPDATE_COMPLETE"                              -> pure UpdateComplete
        "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"          -> pure UpdateCompleteCleanupInProgress
        "UPDATE_IN_PROGRESS"                           -> pure UpdateInProgress
        "UPDATE_ROLLBACK_COMPLETE"                     -> pure UpdateRollbackComplete
        "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS" -> pure UpdateRollbackCompleteCleanupInProgress
        "UPDATE_ROLLBACK_FAILED"                       -> pure UpdateRollbackFailed
        "UPDATE_ROLLBACK_IN_PROGRESS"                  -> pure UpdateRollbackInProgress
        e                                              -> fail $
            "Failure parsing StackStatus from " ++ show e

instance ToText StackStatus where
    toText = \case
        CreateComplete                          -> "CREATE_COMPLETE"
        CreateFailed                            -> "CREATE_FAILED"
        CreateInProgress                        -> "CREATE_IN_PROGRESS"
        DeleteComplete                          -> "DELETE_COMPLETE"
        DeleteFailed                            -> "DELETE_FAILED"
        DeleteInProgress                        -> "DELETE_IN_PROGRESS"
        RollbackComplete                        -> "ROLLBACK_COMPLETE"
        RollbackFailed                          -> "ROLLBACK_FAILED"
        RollbackInProgress                      -> "ROLLBACK_IN_PROGRESS"
        UpdateComplete                          -> "UPDATE_COMPLETE"
        UpdateCompleteCleanupInProgress         -> "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"
        UpdateInProgress                        -> "UPDATE_IN_PROGRESS"
        UpdateRollbackComplete                  -> "UPDATE_ROLLBACK_COMPLETE"
        UpdateRollbackCompleteCleanupInProgress -> "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
        UpdateRollbackFailed                    -> "UPDATE_ROLLBACK_FAILED"
        UpdateRollbackInProgress                -> "UPDATE_ROLLBACK_IN_PROGRESS"

instance ToByteString StackStatus
instance ToHeader     StackStatus
instance ToQuery      StackStatus

instance FromXML StackStatus where
    parseXML = parseXMLText "StackStatus"

data StackEvent = StackEvent
    { _seEventId              :: Text
    , _seLogicalResourceId    :: Maybe Text
    , _sePhysicalResourceId   :: Maybe Text
    , _seResourceProperties   :: Maybe Text
    , _seResourceStatus       :: Maybe ResourceStatus
    , _seResourceStatusReason :: Maybe Text
    , _seResourceType         :: Maybe Text
    , _seStackId              :: Text
    , _seStackName            :: Text
    , _seTimestamp            :: RFC822
    } deriving (Eq, Show)

-- | 'StackEvent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seEventId' @::@ 'Text'
--
-- * 'seLogicalResourceId' @::@ 'Maybe' 'Text'
--
-- * 'sePhysicalResourceId' @::@ 'Maybe' 'Text'
--
-- * 'seResourceProperties' @::@ 'Maybe' 'Text'
--
-- * 'seResourceStatus' @::@ 'Maybe' 'ResourceStatus'
--
-- * 'seResourceStatusReason' @::@ 'Maybe' 'Text'
--
-- * 'seResourceType' @::@ 'Maybe' 'Text'
--
-- * 'seStackId' @::@ 'Text'
--
-- * 'seStackName' @::@ 'Text'
--
-- * 'seTimestamp' @::@ 'UTCTime'
--
stackEvent :: Text -- ^ 'seStackId'
           -> Text -- ^ 'seEventId'
           -> Text -- ^ 'seStackName'
           -> UTCTime -- ^ 'seTimestamp'
           -> StackEvent
stackEvent p1 p2 p3 p4 = StackEvent
    { _seStackId              = p1
    , _seEventId              = p2
    , _seStackName            = p3
    , _seTimestamp            = withIso _Time (const id) p4
    , _seLogicalResourceId    = Nothing
    , _sePhysicalResourceId   = Nothing
    , _seResourceType         = Nothing
    , _seResourceStatus       = Nothing
    , _seResourceStatusReason = Nothing
    , _seResourceProperties   = Nothing
    }

-- | The unique ID of this event.
seEventId :: Lens' StackEvent Text
seEventId = lens _seEventId (\s a -> s { _seEventId = a })

-- | The logical name of the resource specified in the template.
seLogicalResourceId :: Lens' StackEvent (Maybe Text)
seLogicalResourceId =
    lens _seLogicalResourceId (\s a -> s { _seLogicalResourceId = a })

-- | The name or unique identifier associated with the physical instance of the
-- resource.
sePhysicalResourceId :: Lens' StackEvent (Maybe Text)
sePhysicalResourceId =
    lens _sePhysicalResourceId (\s a -> s { _sePhysicalResourceId = a })

-- | BLOB of the properties used to create the resource.
seResourceProperties :: Lens' StackEvent (Maybe Text)
seResourceProperties =
    lens _seResourceProperties (\s a -> s { _seResourceProperties = a })

-- | Current status of the resource.
seResourceStatus :: Lens' StackEvent (Maybe ResourceStatus)
seResourceStatus = lens _seResourceStatus (\s a -> s { _seResourceStatus = a })

-- | Success/failure message associated with the resource.
seResourceStatusReason :: Lens' StackEvent (Maybe Text)
seResourceStatusReason =
    lens _seResourceStatusReason (\s a -> s { _seResourceStatusReason = a })

-- | Type of resource. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html  AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
seResourceType :: Lens' StackEvent (Maybe Text)
seResourceType = lens _seResourceType (\s a -> s { _seResourceType = a })

-- | The unique ID name of the instance of the stack.
seStackId :: Lens' StackEvent Text
seStackId = lens _seStackId (\s a -> s { _seStackId = a })

-- | The name associated with a stack.
seStackName :: Lens' StackEvent Text
seStackName = lens _seStackName (\s a -> s { _seStackName = a })

-- | Time the status was updated.
seTimestamp :: Lens' StackEvent UTCTime
seTimestamp = lens _seTimestamp (\s a -> s { _seTimestamp = a }) . _Time

instance FromXML StackEvent where
    parseXML x = StackEvent
        <$> x .@  "EventId"
        <*> x .@? "LogicalResourceId"
        <*> x .@? "PhysicalResourceId"
        <*> x .@? "ResourceProperties"
        <*> x .@? "ResourceStatus"
        <*> x .@? "ResourceStatusReason"
        <*> x .@? "ResourceType"
        <*> x .@  "StackId"
        <*> x .@  "StackName"
        <*> x .@  "Timestamp"

instance ToQuery StackEvent where
    toQuery StackEvent{..} = mconcat
        [ "EventId"              =? _seEventId
        , "LogicalResourceId"    =? _seLogicalResourceId
        , "PhysicalResourceId"   =? _sePhysicalResourceId
        , "ResourceProperties"   =? _seResourceProperties
        , "ResourceStatus"       =? _seResourceStatus
        , "ResourceStatusReason" =? _seResourceStatusReason
        , "ResourceType"         =? _seResourceType
        , "StackId"              =? _seStackId
        , "StackName"            =? _seStackName
        , "Timestamp"            =? _seTimestamp
        ]

data StackSummary = StackSummary
    { _ssCreationTime        :: RFC822
    , _ssDeletionTime        :: Maybe RFC822
    , _ssLastUpdatedTime     :: Maybe RFC822
    , _ssStackId             :: Maybe Text
    , _ssStackName           :: Text
    , _ssStackStatus         :: StackStatus
    , _ssStackStatusReason   :: Maybe Text
    , _ssTemplateDescription :: Maybe Text
    } deriving (Eq, Show)

-- | 'StackSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssCreationTime' @::@ 'UTCTime'
--
-- * 'ssDeletionTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'ssLastUpdatedTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'ssStackId' @::@ 'Maybe' 'Text'
--
-- * 'ssStackName' @::@ 'Text'
--
-- * 'ssStackStatus' @::@ 'StackStatus'
--
-- * 'ssStackStatusReason' @::@ 'Maybe' 'Text'
--
-- * 'ssTemplateDescription' @::@ 'Maybe' 'Text'
--
stackSummary :: Text -- ^ 'ssStackName'
             -> UTCTime -- ^ 'ssCreationTime'
             -> StackStatus -- ^ 'ssStackStatus'
             -> StackSummary
stackSummary p1 p2 p3 = StackSummary
    { _ssStackName           = p1
    , _ssCreationTime        = withIso _Time (const id) p2
    , _ssStackStatus         = p3
    , _ssStackId             = Nothing
    , _ssTemplateDescription = Nothing
    , _ssLastUpdatedTime     = Nothing
    , _ssDeletionTime        = Nothing
    , _ssStackStatusReason   = Nothing
    }

-- | The time the stack was created.
ssCreationTime :: Lens' StackSummary UTCTime
ssCreationTime = lens _ssCreationTime (\s a -> s { _ssCreationTime = a }) . _Time

-- | The time the stack was deleted.
ssDeletionTime :: Lens' StackSummary (Maybe UTCTime)
ssDeletionTime = lens _ssDeletionTime (\s a -> s { _ssDeletionTime = a }) . mapping _Time

-- | The time the stack was last updated. This field will only be returned if the
-- stack has been updated at least once.
ssLastUpdatedTime :: Lens' StackSummary (Maybe UTCTime)
ssLastUpdatedTime =
    lens _ssLastUpdatedTime (\s a -> s { _ssLastUpdatedTime = a })
        . mapping _Time

-- | Unique stack identifier.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\s a -> s { _ssStackId = a })

-- | The name associated with the stack.
ssStackName :: Lens' StackSummary Text
ssStackName = lens _ssStackName (\s a -> s { _ssStackName = a })

-- | The current status of the stack.
ssStackStatus :: Lens' StackSummary StackStatus
ssStackStatus = lens _ssStackStatus (\s a -> s { _ssStackStatus = a })

-- | Success/Failure message associated with the stack status.
ssStackStatusReason :: Lens' StackSummary (Maybe Text)
ssStackStatusReason =
    lens _ssStackStatusReason (\s a -> s { _ssStackStatusReason = a })

-- | The template description of the template used to create the stack.
ssTemplateDescription :: Lens' StackSummary (Maybe Text)
ssTemplateDescription =
    lens _ssTemplateDescription (\s a -> s { _ssTemplateDescription = a })

instance FromXML StackSummary where
    parseXML x = StackSummary
        <$> x .@  "CreationTime"
        <*> x .@? "DeletionTime"
        <*> x .@? "LastUpdatedTime"
        <*> x .@? "StackId"
        <*> x .@  "StackName"
        <*> x .@  "StackStatus"
        <*> x .@? "StackStatusReason"
        <*> x .@? "TemplateDescription"

instance ToQuery StackSummary where
    toQuery StackSummary{..} = mconcat
        [ "CreationTime"        =? _ssCreationTime
        , "DeletionTime"        =? _ssDeletionTime
        , "LastUpdatedTime"     =? _ssLastUpdatedTime
        , "StackId"             =? _ssStackId
        , "StackName"           =? _ssStackName
        , "StackStatus"         =? _ssStackStatus
        , "StackStatusReason"   =? _ssStackStatusReason
        , "TemplateDescription" =? _ssTemplateDescription
        ]

data StackResourceDetail = StackResourceDetail
    { _srdDescription          :: Maybe Text
    , _srdLastUpdatedTimestamp :: RFC822
    , _srdLogicalResourceId    :: Text
    , _srdMetadata             :: Maybe Text
    , _srdPhysicalResourceId   :: Maybe Text
    , _srdResourceStatus       :: ResourceStatus
    , _srdResourceStatusReason :: Maybe Text
    , _srdResourceType         :: Text
    , _srdStackId              :: Maybe Text
    , _srdStackName            :: Maybe Text
    } deriving (Eq, Show)

-- | 'StackResourceDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srdDescription' @::@ 'Maybe' 'Text'
--
-- * 'srdLastUpdatedTimestamp' @::@ 'UTCTime'
--
-- * 'srdLogicalResourceId' @::@ 'Text'
--
-- * 'srdMetadata' @::@ 'Maybe' 'Text'
--
-- * 'srdPhysicalResourceId' @::@ 'Maybe' 'Text'
--
-- * 'srdResourceStatus' @::@ 'ResourceStatus'
--
-- * 'srdResourceStatusReason' @::@ 'Maybe' 'Text'
--
-- * 'srdResourceType' @::@ 'Text'
--
-- * 'srdStackId' @::@ 'Maybe' 'Text'
--
-- * 'srdStackName' @::@ 'Maybe' 'Text'
--
stackResourceDetail :: Text -- ^ 'srdLogicalResourceId'
                    -> Text -- ^ 'srdResourceType'
                    -> UTCTime -- ^ 'srdLastUpdatedTimestamp'
                    -> ResourceStatus -- ^ 'srdResourceStatus'
                    -> StackResourceDetail
stackResourceDetail p1 p2 p3 p4 = StackResourceDetail
    { _srdLogicalResourceId    = p1
    , _srdResourceType         = p2
    , _srdLastUpdatedTimestamp = withIso _Time (const id) p3
    , _srdResourceStatus       = p4
    , _srdStackName            = Nothing
    , _srdStackId              = Nothing
    , _srdPhysicalResourceId   = Nothing
    , _srdResourceStatusReason = Nothing
    , _srdDescription          = Nothing
    , _srdMetadata             = Nothing
    }

-- | User defined description associated with the resource.
srdDescription :: Lens' StackResourceDetail (Maybe Text)
srdDescription = lens _srdDescription (\s a -> s { _srdDescription = a })

-- | Time the status was updated.
srdLastUpdatedTimestamp :: Lens' StackResourceDetail UTCTime
srdLastUpdatedTimestamp =
    lens _srdLastUpdatedTimestamp (\s a -> s { _srdLastUpdatedTimestamp = a })
        . _Time

-- | The logical name of the resource specified in the template.
srdLogicalResourceId :: Lens' StackResourceDetail Text
srdLogicalResourceId =
    lens _srdLogicalResourceId (\s a -> s { _srdLogicalResourceId = a })

-- | The JSON format content of the 'Metadata' attribute declared for the resource.
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User
-- Guide.
srdMetadata :: Lens' StackResourceDetail (Maybe Text)
srdMetadata = lens _srdMetadata (\s a -> s { _srdMetadata = a })

-- | The name or unique identifier that corresponds to a physical instance ID of a
-- resource supported by AWS CloudFormation.
srdPhysicalResourceId :: Lens' StackResourceDetail (Maybe Text)
srdPhysicalResourceId =
    lens _srdPhysicalResourceId (\s a -> s { _srdPhysicalResourceId = a })

-- | Current status of the resource.
srdResourceStatus :: Lens' StackResourceDetail ResourceStatus
srdResourceStatus =
    lens _srdResourceStatus (\s a -> s { _srdResourceStatus = a })

-- | Success/failure message associated with the resource.
srdResourceStatusReason :: Lens' StackResourceDetail (Maybe Text)
srdResourceStatusReason =
    lens _srdResourceStatusReason (\s a -> s { _srdResourceStatusReason = a })

-- | Type of resource. ((For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html  AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
srdResourceType :: Lens' StackResourceDetail Text
srdResourceType = lens _srdResourceType (\s a -> s { _srdResourceType = a })

-- | Unique identifier of the stack.
srdStackId :: Lens' StackResourceDetail (Maybe Text)
srdStackId = lens _srdStackId (\s a -> s { _srdStackId = a })

-- | The name associated with the stack.
srdStackName :: Lens' StackResourceDetail (Maybe Text)
srdStackName = lens _srdStackName (\s a -> s { _srdStackName = a })

instance FromXML StackResourceDetail where
    parseXML x = StackResourceDetail
        <$> x .@? "Description"
        <*> x .@  "LastUpdatedTimestamp"
        <*> x .@  "LogicalResourceId"
        <*> x .@? "Metadata"
        <*> x .@? "PhysicalResourceId"
        <*> x .@  "ResourceStatus"
        <*> x .@? "ResourceStatusReason"
        <*> x .@  "ResourceType"
        <*> x .@? "StackId"
        <*> x .@? "StackName"

instance ToQuery StackResourceDetail where
    toQuery StackResourceDetail{..} = mconcat
        [ "Description"          =? _srdDescription
        , "LastUpdatedTimestamp" =? _srdLastUpdatedTimestamp
        , "LogicalResourceId"    =? _srdLogicalResourceId
        , "Metadata"             =? _srdMetadata
        , "PhysicalResourceId"   =? _srdPhysicalResourceId
        , "ResourceStatus"       =? _srdResourceStatus
        , "ResourceStatusReason" =? _srdResourceStatusReason
        , "ResourceType"         =? _srdResourceType
        , "StackId"              =? _srdStackId
        , "StackName"            =? _srdStackName
        ]

data ResourceStatus
    = RSCreateComplete   -- ^ CREATE_COMPLETE
    | RSCreateFailed     -- ^ CREATE_FAILED
    | RSCreateInProgress -- ^ CREATE_IN_PROGRESS
    | RSDeleteComplete   -- ^ DELETE_COMPLETE
    | RSDeleteFailed     -- ^ DELETE_FAILED
    | RSDeleteInProgress -- ^ DELETE_IN_PROGRESS
    | RSDeleteSkipped    -- ^ DELETE_SKIPPED
    | RSUpdateComplete   -- ^ UPDATE_COMPLETE
    | RSUpdateFailed     -- ^ UPDATE_FAILED
    | RSUpdateInProgress -- ^ UPDATE_IN_PROGRESS
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ResourceStatus

instance FromText ResourceStatus where
    parser = takeText >>= \case
        "CREATE_COMPLETE"    -> pure RSCreateComplete
        "CREATE_FAILED"      -> pure RSCreateFailed
        "CREATE_IN_PROGRESS" -> pure RSCreateInProgress
        "DELETE_COMPLETE"    -> pure RSDeleteComplete
        "DELETE_FAILED"      -> pure RSDeleteFailed
        "DELETE_IN_PROGRESS" -> pure RSDeleteInProgress
        "DELETE_SKIPPED"     -> pure RSDeleteSkipped
        "UPDATE_COMPLETE"    -> pure RSUpdateComplete
        "UPDATE_FAILED"      -> pure RSUpdateFailed
        "UPDATE_IN_PROGRESS" -> pure RSUpdateInProgress
        e                    -> fail $
            "Failure parsing ResourceStatus from " ++ show e

instance ToText ResourceStatus where
    toText = \case
        RSCreateComplete   -> "CREATE_COMPLETE"
        RSCreateFailed     -> "CREATE_FAILED"
        RSCreateInProgress -> "CREATE_IN_PROGRESS"
        RSDeleteComplete   -> "DELETE_COMPLETE"
        RSDeleteFailed     -> "DELETE_FAILED"
        RSDeleteInProgress -> "DELETE_IN_PROGRESS"
        RSDeleteSkipped    -> "DELETE_SKIPPED"
        RSUpdateComplete   -> "UPDATE_COMPLETE"
        RSUpdateFailed     -> "UPDATE_FAILED"
        RSUpdateInProgress -> "UPDATE_IN_PROGRESS"

instance ToByteString ResourceStatus
instance ToHeader     ResourceStatus
instance ToQuery      ResourceStatus

instance FromXML ResourceStatus where
    parseXML = parseXMLText "ResourceStatus"

data TemplateParameter = TemplateParameter
    { _tpDefaultValue :: Maybe Text
    , _tpDescription  :: Maybe Text
    , _tpNoEcho       :: Maybe Bool
    , _tpParameterKey :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'TemplateParameter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tpDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'tpDescription' @::@ 'Maybe' 'Text'
--
-- * 'tpNoEcho' @::@ 'Maybe' 'Bool'
--
-- * 'tpParameterKey' @::@ 'Maybe' 'Text'
--
templateParameter :: TemplateParameter
templateParameter = TemplateParameter
    { _tpParameterKey = Nothing
    , _tpDefaultValue = Nothing
    , _tpNoEcho       = Nothing
    , _tpDescription  = Nothing
    }

-- | The default value associated with the parameter.
tpDefaultValue :: Lens' TemplateParameter (Maybe Text)
tpDefaultValue = lens _tpDefaultValue (\s a -> s { _tpDefaultValue = a })

-- | User defined description associated with the parameter.
tpDescription :: Lens' TemplateParameter (Maybe Text)
tpDescription = lens _tpDescription (\s a -> s { _tpDescription = a })

-- | Flag indicating whether the parameter should be displayed as plain text in
-- logs and UIs.
tpNoEcho :: Lens' TemplateParameter (Maybe Bool)
tpNoEcho = lens _tpNoEcho (\s a -> s { _tpNoEcho = a })

-- | The name associated with the parameter.
tpParameterKey :: Lens' TemplateParameter (Maybe Text)
tpParameterKey = lens _tpParameterKey (\s a -> s { _tpParameterKey = a })

instance FromXML TemplateParameter where
    parseXML x = TemplateParameter
        <$> x .@? "DefaultValue"
        <*> x .@? "Description"
        <*> x .@? "NoEcho"
        <*> x .@? "ParameterKey"

instance ToQuery TemplateParameter where
    toQuery TemplateParameter{..} = mconcat
        [ "DefaultValue" =? _tpDefaultValue
        , "Description"  =? _tpDescription
        , "NoEcho"       =? _tpNoEcho
        , "ParameterKey" =? _tpParameterKey
        ]

data ParameterDeclaration = ParameterDeclaration
    { _pdDefaultValue  :: Maybe Text
    , _pdDescription   :: Maybe Text
    , _pdNoEcho        :: Maybe Bool
    , _pdParameterKey  :: Maybe Text
    , _pdParameterType :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ParameterDeclaration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'pdDescription' @::@ 'Maybe' 'Text'
--
-- * 'pdNoEcho' @::@ 'Maybe' 'Bool'
--
-- * 'pdParameterKey' @::@ 'Maybe' 'Text'
--
-- * 'pdParameterType' @::@ 'Maybe' 'Text'
--
parameterDeclaration :: ParameterDeclaration
parameterDeclaration = ParameterDeclaration
    { _pdParameterKey  = Nothing
    , _pdDefaultValue  = Nothing
    , _pdParameterType = Nothing
    , _pdNoEcho        = Nothing
    , _pdDescription   = Nothing
    }

-- | The default value of the parameter.
pdDefaultValue :: Lens' ParameterDeclaration (Maybe Text)
pdDefaultValue = lens _pdDefaultValue (\s a -> s { _pdDefaultValue = a })

-- | The description that is associate with the parameter.
pdDescription :: Lens' ParameterDeclaration (Maybe Text)
pdDescription = lens _pdDescription (\s a -> s { _pdDescription = a })

-- | Flag that indicates whether the parameter value is shown as plain text in
-- logs and in the AWS Management Console.
pdNoEcho :: Lens' ParameterDeclaration (Maybe Bool)
pdNoEcho = lens _pdNoEcho (\s a -> s { _pdNoEcho = a })

-- | The name that is associated with the parameter.
pdParameterKey :: Lens' ParameterDeclaration (Maybe Text)
pdParameterKey = lens _pdParameterKey (\s a -> s { _pdParameterKey = a })

-- | The type of parameter.
pdParameterType :: Lens' ParameterDeclaration (Maybe Text)
pdParameterType = lens _pdParameterType (\s a -> s { _pdParameterType = a })

instance FromXML ParameterDeclaration where
    parseXML x = ParameterDeclaration
        <$> x .@? "DefaultValue"
        <*> x .@? "Description"
        <*> x .@? "NoEcho"
        <*> x .@? "ParameterKey"
        <*> x .@? "ParameterType"

instance ToQuery ParameterDeclaration where
    toQuery ParameterDeclaration{..} = mconcat
        [ "DefaultValue"  =? _pdDefaultValue
        , "Description"   =? _pdDescription
        , "NoEcho"        =? _pdNoEcho
        , "ParameterKey"  =? _pdParameterKey
        , "ParameterType" =? _pdParameterType
        ]

data StackResource = StackResource
    { _sr1Description          :: Maybe Text
    , _sr1LogicalResourceId    :: Text
    , _sr1PhysicalResourceId   :: Maybe Text
    , _sr1ResourceStatus       :: ResourceStatus
    , _sr1ResourceStatusReason :: Maybe Text
    , _sr1ResourceType         :: Text
    , _sr1StackId              :: Maybe Text
    , _sr1StackName            :: Maybe Text
    , _sr1Timestamp            :: RFC822
    } deriving (Eq, Show)

-- | 'StackResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sr1Description' @::@ 'Maybe' 'Text'
--
-- * 'sr1LogicalResourceId' @::@ 'Text'
--
-- * 'sr1PhysicalResourceId' @::@ 'Maybe' 'Text'
--
-- * 'sr1ResourceStatus' @::@ 'ResourceStatus'
--
-- * 'sr1ResourceStatusReason' @::@ 'Maybe' 'Text'
--
-- * 'sr1ResourceType' @::@ 'Text'
--
-- * 'sr1StackId' @::@ 'Maybe' 'Text'
--
-- * 'sr1StackName' @::@ 'Maybe' 'Text'
--
-- * 'sr1Timestamp' @::@ 'UTCTime'
--
stackResource :: Text -- ^ 'sr1LogicalResourceId'
              -> Text -- ^ 'sr1ResourceType'
              -> UTCTime -- ^ 'sr1Timestamp'
              -> ResourceStatus -- ^ 'sr1ResourceStatus'
              -> StackResource
stackResource p1 p2 p3 p4 = StackResource
    { _sr1LogicalResourceId    = p1
    , _sr1ResourceType         = p2
    , _sr1Timestamp            = withIso _Time (const id) p3
    , _sr1ResourceStatus       = p4
    , _sr1StackName            = Nothing
    , _sr1StackId              = Nothing
    , _sr1PhysicalResourceId   = Nothing
    , _sr1ResourceStatusReason = Nothing
    , _sr1Description          = Nothing
    }

-- | User defined description associated with the resource.
sr1Description :: Lens' StackResource (Maybe Text)
sr1Description = lens _sr1Description (\s a -> s { _sr1Description = a })

-- | The logical name of the resource specified in the template.
sr1LogicalResourceId :: Lens' StackResource Text
sr1LogicalResourceId =
    lens _sr1LogicalResourceId (\s a -> s { _sr1LogicalResourceId = a })

-- | The name or unique identifier that corresponds to a physical instance ID of a
-- resource supported by AWS CloudFormation.
sr1PhysicalResourceId :: Lens' StackResource (Maybe Text)
sr1PhysicalResourceId =
    lens _sr1PhysicalResourceId (\s a -> s { _sr1PhysicalResourceId = a })

-- | Current status of the resource.
sr1ResourceStatus :: Lens' StackResource ResourceStatus
sr1ResourceStatus =
    lens _sr1ResourceStatus (\s a -> s { _sr1ResourceStatus = a })

-- | Success/failure message associated with the resource.
sr1ResourceStatusReason :: Lens' StackResource (Maybe Text)
sr1ResourceStatusReason =
    lens _sr1ResourceStatusReason (\s a -> s { _sr1ResourceStatusReason = a })

-- | Type of resource. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html  AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
sr1ResourceType :: Lens' StackResource Text
sr1ResourceType = lens _sr1ResourceType (\s a -> s { _sr1ResourceType = a })

-- | Unique identifier of the stack.
sr1StackId :: Lens' StackResource (Maybe Text)
sr1StackId = lens _sr1StackId (\s a -> s { _sr1StackId = a })

-- | The name associated with the stack.
sr1StackName :: Lens' StackResource (Maybe Text)
sr1StackName = lens _sr1StackName (\s a -> s { _sr1StackName = a })

-- | Time the status was updated.
sr1Timestamp :: Lens' StackResource UTCTime
sr1Timestamp = lens _sr1Timestamp (\s a -> s { _sr1Timestamp = a }) . _Time

instance FromXML StackResource where
    parseXML x = StackResource
        <$> x .@? "Description"
        <*> x .@  "LogicalResourceId"
        <*> x .@? "PhysicalResourceId"
        <*> x .@  "ResourceStatus"
        <*> x .@? "ResourceStatusReason"
        <*> x .@  "ResourceType"
        <*> x .@? "StackId"
        <*> x .@? "StackName"
        <*> x .@  "Timestamp"

instance ToQuery StackResource where
    toQuery StackResource{..} = mconcat
        [ "Description"          =? _sr1Description
        , "LogicalResourceId"    =? _sr1LogicalResourceId
        , "PhysicalResourceId"   =? _sr1PhysicalResourceId
        , "ResourceStatus"       =? _sr1ResourceStatus
        , "ResourceStatusReason" =? _sr1ResourceStatusReason
        , "ResourceType"         =? _sr1ResourceType
        , "StackId"              =? _sr1StackId
        , "StackName"            =? _sr1StackName
        , "Timestamp"            =? _sr1Timestamp
        ]

data Output = Output
    { _oDescription :: Maybe Text
    , _oOutputKey   :: Maybe Text
    , _oOutputValue :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Output' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oDescription' @::@ 'Maybe' 'Text'
--
-- * 'oOutputKey' @::@ 'Maybe' 'Text'
--
-- * 'oOutputValue' @::@ 'Maybe' 'Text'
--
output :: Output
output = Output
    { _oOutputKey   = Nothing
    , _oOutputValue = Nothing
    , _oDescription = Nothing
    }

-- | User defined description associated with the output.
oDescription :: Lens' Output (Maybe Text)
oDescription = lens _oDescription (\s a -> s { _oDescription = a })

-- | The key associated with the output.
oOutputKey :: Lens' Output (Maybe Text)
oOutputKey = lens _oOutputKey (\s a -> s { _oOutputKey = a })

-- | The value associated with the output.
oOutputValue :: Lens' Output (Maybe Text)
oOutputValue = lens _oOutputValue (\s a -> s { _oOutputValue = a })

instance FromXML Output where
    parseXML x = Output
        <$> x .@? "Description"
        <*> x .@? "OutputKey"
        <*> x .@? "OutputValue"

instance ToQuery Output where
    toQuery Output{..} = mconcat
        [ "Description" =? _oDescription
        , "OutputKey"   =? _oOutputKey
        , "OutputValue" =? _oOutputValue
        ]

data StackResourceSummary = StackResourceSummary
    { _srsLastUpdatedTimestamp :: RFC822
    , _srsLogicalResourceId    :: Text
    , _srsPhysicalResourceId   :: Maybe Text
    , _srsResourceStatus       :: ResourceStatus
    , _srsResourceStatusReason :: Maybe Text
    , _srsResourceType         :: Text
    } deriving (Eq, Show)

-- | 'StackResourceSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srsLastUpdatedTimestamp' @::@ 'UTCTime'
--
-- * 'srsLogicalResourceId' @::@ 'Text'
--
-- * 'srsPhysicalResourceId' @::@ 'Maybe' 'Text'
--
-- * 'srsResourceStatus' @::@ 'ResourceStatus'
--
-- * 'srsResourceStatusReason' @::@ 'Maybe' 'Text'
--
-- * 'srsResourceType' @::@ 'Text'
--
stackResourceSummary :: Text -- ^ 'srsLogicalResourceId'
                     -> Text -- ^ 'srsResourceType'
                     -> UTCTime -- ^ 'srsLastUpdatedTimestamp'
                     -> ResourceStatus -- ^ 'srsResourceStatus'
                     -> StackResourceSummary
stackResourceSummary p1 p2 p3 p4 = StackResourceSummary
    { _srsLogicalResourceId    = p1
    , _srsResourceType         = p2
    , _srsLastUpdatedTimestamp = withIso _Time (const id) p3
    , _srsResourceStatus       = p4
    , _srsPhysicalResourceId   = Nothing
    , _srsResourceStatusReason = Nothing
    }

-- | Time the status was updated.
srsLastUpdatedTimestamp :: Lens' StackResourceSummary UTCTime
srsLastUpdatedTimestamp =
    lens _srsLastUpdatedTimestamp (\s a -> s { _srsLastUpdatedTimestamp = a })
        . _Time

-- | The logical name of the resource specified in the template.
srsLogicalResourceId :: Lens' StackResourceSummary Text
srsLogicalResourceId =
    lens _srsLogicalResourceId (\s a -> s { _srsLogicalResourceId = a })

-- | The name or unique identifier that corresponds to a physical instance ID of
-- the resource.
srsPhysicalResourceId :: Lens' StackResourceSummary (Maybe Text)
srsPhysicalResourceId =
    lens _srsPhysicalResourceId (\s a -> s { _srsPhysicalResourceId = a })

-- | Current status of the resource.
srsResourceStatus :: Lens' StackResourceSummary ResourceStatus
srsResourceStatus =
    lens _srsResourceStatus (\s a -> s { _srsResourceStatus = a })

-- | Success/failure message associated with the resource.
srsResourceStatusReason :: Lens' StackResourceSummary (Maybe Text)
srsResourceStatusReason =
    lens _srsResourceStatusReason (\s a -> s { _srsResourceStatusReason = a })

-- | Type of resource. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html  AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
srsResourceType :: Lens' StackResourceSummary Text
srsResourceType = lens _srsResourceType (\s a -> s { _srsResourceType = a })

instance FromXML StackResourceSummary where
    parseXML x = StackResourceSummary
        <$> x .@  "LastUpdatedTimestamp"
        <*> x .@  "LogicalResourceId"
        <*> x .@? "PhysicalResourceId"
        <*> x .@  "ResourceStatus"
        <*> x .@? "ResourceStatusReason"
        <*> x .@  "ResourceType"

instance ToQuery StackResourceSummary where
    toQuery StackResourceSummary{..} = mconcat
        [ "LastUpdatedTimestamp" =? _srsLastUpdatedTimestamp
        , "LogicalResourceId"    =? _srsLogicalResourceId
        , "PhysicalResourceId"   =? _srsPhysicalResourceId
        , "ResourceStatus"       =? _srsResourceStatus
        , "ResourceStatusReason" =? _srsResourceStatusReason
        , "ResourceType"         =? _srsResourceType
        ]

data Capability
    = CapabilityIam -- ^ CAPABILITY_IAM
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable Capability

instance FromText Capability where
    parser = takeText >>= \case
        "CAPABILITY_IAM" -> pure CapabilityIam
        e                -> fail $
            "Failure parsing Capability from " ++ show e

instance ToText Capability where
    toText CapabilityIam = "CAPABILITY_IAM"

instance ToByteString Capability
instance ToHeader     Capability
instance ToQuery      Capability

instance FromXML Capability where
    parseXML = parseXMLText "Capability"

data ResourceSignalStatus
    = Failure -- ^ FAILURE
    | Success -- ^ SUCCESS
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ResourceSignalStatus

instance FromText ResourceSignalStatus where
    parser = takeText >>= \case
        "FAILURE" -> pure Failure
        "SUCCESS" -> pure Success
        e         -> fail $
            "Failure parsing ResourceSignalStatus from " ++ show e

instance ToText ResourceSignalStatus where
    toText = \case
        Failure -> "FAILURE"
        Success -> "SUCCESS"

instance ToByteString ResourceSignalStatus
instance ToHeader     ResourceSignalStatus
instance ToQuery      ResourceSignalStatus

instance FromXML ResourceSignalStatus where
    parseXML = parseXMLText "ResourceSignalStatus"

data Stack = Stack
    { _sCapabilities      :: List "Capabilities" Capability
    , _sCreationTime      :: RFC822
    , _sDescription       :: Maybe Text
    , _sDisableRollback   :: Maybe Bool
    , _sLastUpdatedTime   :: Maybe RFC822
    , _sNotificationARNs  :: List "NotificationARNs" Text
    , _sOutputs           :: List "Outputs" Output
    , _sParameters        :: List "Parameters" Parameter
    , _sStackId           :: Maybe Text
    , _sStackName         :: Text
    , _sStackStatus       :: StackStatus
    , _sStackStatusReason :: Maybe Text
    , _sTags              :: List "Tags" Tag
    , _sTimeoutInMinutes  :: Maybe Nat
    } deriving (Eq, Show)

-- | 'Stack' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sCapabilities' @::@ ['Capability']
--
-- * 'sCreationTime' @::@ 'UTCTime'
--
-- * 'sDescription' @::@ 'Maybe' 'Text'
--
-- * 'sDisableRollback' @::@ 'Maybe' 'Bool'
--
-- * 'sLastUpdatedTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'sNotificationARNs' @::@ ['Text']
--
-- * 'sOutputs' @::@ ['Output']
--
-- * 'sParameters' @::@ ['Parameter']
--
-- * 'sStackId' @::@ 'Maybe' 'Text'
--
-- * 'sStackName' @::@ 'Text'
--
-- * 'sStackStatus' @::@ 'StackStatus'
--
-- * 'sStackStatusReason' @::@ 'Maybe' 'Text'
--
-- * 'sTags' @::@ ['Tag']
--
-- * 'sTimeoutInMinutes' @::@ 'Maybe' 'Natural'
--
stack :: Text -- ^ 'sStackName'
      -> UTCTime -- ^ 'sCreationTime'
      -> StackStatus -- ^ 'sStackStatus'
      -> Stack
stack p1 p2 p3 = Stack
    { _sStackName         = p1
    , _sCreationTime      = withIso _Time (const id) p2
    , _sStackStatus       = p3
    , _sStackId           = Nothing
    , _sDescription       = Nothing
    , _sParameters        = mempty
    , _sLastUpdatedTime   = Nothing
    , _sStackStatusReason = Nothing
    , _sDisableRollback   = Nothing
    , _sNotificationARNs  = mempty
    , _sTimeoutInMinutes  = Nothing
    , _sCapabilities      = mempty
    , _sOutputs           = mempty
    , _sTags              = mempty
    }

-- | The capabilities allowed in the stack.
sCapabilities :: Lens' Stack [Capability]
sCapabilities = lens _sCapabilities (\s a -> s { _sCapabilities = a }) . _List

-- | Time at which the stack was created.
sCreationTime :: Lens' Stack UTCTime
sCreationTime = lens _sCreationTime (\s a -> s { _sCreationTime = a }) . _Time

-- | User defined description associated with the stack.
sDescription :: Lens' Stack (Maybe Text)
sDescription = lens _sDescription (\s a -> s { _sDescription = a })

-- | Boolean to enable or disable rollback on stack creation failures:
--
-- 'true': disable rollback  'false': enable rollback
sDisableRollback :: Lens' Stack (Maybe Bool)
sDisableRollback = lens _sDisableRollback (\s a -> s { _sDisableRollback = a })

-- | The time the stack was last updated. This field will only be returned if the
-- stack has been updated at least once.
sLastUpdatedTime :: Lens' Stack (Maybe UTCTime)
sLastUpdatedTime = lens _sLastUpdatedTime (\s a -> s { _sLastUpdatedTime = a }) . mapping _Time

-- | SNS topic ARNs to which stack related events are published.
sNotificationARNs :: Lens' Stack [Text]
sNotificationARNs =
    lens _sNotificationARNs (\s a -> s { _sNotificationARNs = a })
        . _List

-- | A list of output structures.
sOutputs :: Lens' Stack [Output]
sOutputs = lens _sOutputs (\s a -> s { _sOutputs = a }) . _List

-- | A list of 'Parameter' structures.
sParameters :: Lens' Stack [Parameter]
sParameters = lens _sParameters (\s a -> s { _sParameters = a }) . _List

-- | Unique identifier of the stack.
sStackId :: Lens' Stack (Maybe Text)
sStackId = lens _sStackId (\s a -> s { _sStackId = a })

-- | The name associated with the stack.
sStackName :: Lens' Stack Text
sStackName = lens _sStackName (\s a -> s { _sStackName = a })

-- | Current status of the stack.
sStackStatus :: Lens' Stack StackStatus
sStackStatus = lens _sStackStatus (\s a -> s { _sStackStatus = a })

-- | Success/failure message associated with the stack status.
sStackStatusReason :: Lens' Stack (Maybe Text)
sStackStatusReason =
    lens _sStackStatusReason (\s a -> s { _sStackStatusReason = a })

-- | A list of 'Tag's that specify cost allocation information for the stack.
sTags :: Lens' Stack [Tag]
sTags = lens _sTags (\s a -> s { _sTags = a }) . _List

-- | The amount of time within which stack creation should complete.
sTimeoutInMinutes :: Lens' Stack (Maybe Natural)
sTimeoutInMinutes =
    lens _sTimeoutInMinutes (\s a -> s { _sTimeoutInMinutes = a })
        . mapping _Nat

instance FromXML Stack where
    parseXML x = Stack
        <$> x .@  "Capabilities"
        <*> x .@  "CreationTime"
        <*> x .@? "Description"
        <*> x .@? "DisableRollback"
        <*> x .@? "LastUpdatedTime"
        <*> x .@  "NotificationARNs"
        <*> x .@  "Outputs"
        <*> x .@  "Parameters"
        <*> x .@? "StackId"
        <*> x .@  "StackName"
        <*> x .@  "StackStatus"
        <*> x .@? "StackStatusReason"
        <*> x .@  "Tags"
        <*> x .@? "TimeoutInMinutes"

instance ToQuery Stack where
    toQuery Stack{..} = mconcat
        [ "Capabilities"      =? _sCapabilities
        , "CreationTime"      =? _sCreationTime
        , "Description"       =? _sDescription
        , "DisableRollback"   =? _sDisableRollback
        , "LastUpdatedTime"   =? _sLastUpdatedTime
        , "NotificationARNs"  =? _sNotificationARNs
        , "Outputs"           =? _sOutputs
        , "Parameters"        =? _sParameters
        , "StackId"           =? _sStackId
        , "StackName"         =? _sStackName
        , "StackStatus"       =? _sStackStatus
        , "StackStatusReason" =? _sStackStatusReason
        , "Tags"              =? _sTags
        , "TimeoutInMinutes"  =? _sTimeoutInMinutes
        ]

data OnFailure
    = Delete'   -- ^ DELETE
    | DoNothing -- ^ DO_NOTHING
    | Rollback  -- ^ ROLLBACK
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable OnFailure

instance FromText OnFailure where
    parser = takeText >>= \case
        "DELETE"     -> pure Delete'
        "DO_NOTHING" -> pure DoNothing
        "ROLLBACK"   -> pure Rollback
        e            -> fail $
            "Failure parsing OnFailure from " ++ show e

instance ToText OnFailure where
    toText = \case
        Delete'   -> "DELETE"
        DoNothing -> "DO_NOTHING"
        Rollback  -> "ROLLBACK"

instance ToByteString OnFailure
instance ToHeader     OnFailure
instance ToQuery      OnFailure

instance FromXML OnFailure where
    parseXML = parseXMLText "OnFailure"

data Parameter = Parameter
    { _pParameterKey     :: Maybe Text
    , _pParameterValue   :: Maybe Text
    , _pUsePreviousValue :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'Parameter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pParameterKey' @::@ 'Maybe' 'Text'
--
-- * 'pParameterValue' @::@ 'Maybe' 'Text'
--
-- * 'pUsePreviousValue' @::@ 'Maybe' 'Bool'
--
parameter :: Parameter
parameter = Parameter
    { _pParameterKey     = Nothing
    , _pParameterValue   = Nothing
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
    parseXML x = Parameter
        <$> x .@? "ParameterKey"
        <*> x .@? "ParameterValue"
        <*> x .@? "UsePreviousValue"

instance ToQuery Parameter where
    toQuery Parameter{..} = mconcat
        [ "ParameterKey"     =? _pParameterKey
        , "ParameterValue"   =? _pParameterValue
        , "UsePreviousValue" =? _pUsePreviousValue
        ]
