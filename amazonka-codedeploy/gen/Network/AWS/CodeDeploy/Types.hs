{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.CodeDeploy.Types
    (
    -- * Service
      CodeDeploy
    -- ** Error
    , JSONError

    -- * GenericRevisionInfo
    , GenericRevisionInfo
    , genericRevisionInfo
    , griDeploymentGroups
    , griDescription
    , griFirstUsedTime
    , griLastUsedTime
    , griRegisterTime

    -- * ApplicationInfo
    , ApplicationInfo
    , applicationInfo
    , aiApplicationId
    , aiApplicationName
    , aiCreateTime
    , aiLinkedToGitHub

    -- * BundleType
    , BundleType (..)

    -- * TimeRange
    , TimeRange
    , timeRange
    , trEnd
    , trStart

    -- * DeploymentCreator
    , DeploymentCreator (..)

    -- * InstanceSummary
    , InstanceSummary
    , instanceSummary
    , isDeploymentId
    , isInstanceId
    , isLastUpdatedAt
    , isLifecycleEvents
    , isStatus

    -- * AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgHook
    , asgName

    -- * DeploymentGroupInfo
    , DeploymentGroupInfo
    , deploymentGroupInfo
    , dgiApplicationName
    , dgiAutoScalingGroups
    , dgiDeploymentConfigName
    , dgiDeploymentGroupId
    , dgiDeploymentGroupName
    , dgiEc2TagFilters
    , dgiServiceRoleArn
    , dgiTargetRevision

    -- * ApplicationRevisionSortBy
    , ApplicationRevisionSortBy (..)

    -- * MinimumHealthyHosts
    , MinimumHealthyHosts
    , minimumHealthyHosts
    , mhhType
    , mhhValue

    -- * ListStateFilterAction
    , ListStateFilterAction (..)

    -- * LifecycleErrorCode
    , LifecycleErrorCode (..)

    -- * RevisionLocation
    , RevisionLocation
    , revisionLocation
    , rlGitHubLocation
    , rlRevisionType
    , rlS3Location

    -- * LifecycleEventStatus
    , LifecycleEventStatus (..)

    -- * EC2TagFilter
    , EC2TagFilter
    , ec2TagFilter
    , ectfKey
    , ectfType
    , ectfValue

    -- * Diagnostics
    , Diagnostics
    , diagnostics
    , dErrorCode
    , dLogTail
    , dMessage
    , dScriptName

    -- * StopStatus
    , StopStatus (..)

    -- * ErrorInformation
    , ErrorInformation
    , errorInformation
    , eiCode
    , eiMessage

    -- * SortOrder
    , SortOrder (..)

    -- * DeploymentInfo
    , DeploymentInfo
    , deploymentInfo
    , diApplicationName
    , diCompleteTime
    , diCreateTime
    , diCreator
    , diDeploymentConfigName
    , diDeploymentGroupName
    , diDeploymentId
    , diDeploymentOverview
    , diDescription
    , diErrorInformation
    , diIgnoreApplicationStopFailures
    , diRevision
    , diStartTime
    , diStatus

    -- * LifecycleEvent
    , LifecycleEvent
    , lifecycleEvent
    , leDiagnostics
    , leEndTime
    , leLifecycleEventName
    , leStartTime
    , leStatus

    -- * DeploymentOverview
    , DeploymentOverview
    , deploymentOverview
    , doFailed
    , doInProgress
    , doPending
    , doSkipped
    , doSucceeded

    -- * ErrorCode
    , ErrorCode (..)

    -- * DeploymentConfigInfo
    , DeploymentConfigInfo
    , deploymentConfigInfo
    , dciCreateTime
    , dciDeploymentConfigId
    , dciDeploymentConfigName
    , dciMinimumHealthyHosts

    -- * InstanceStatus
    , InstanceStatus (..)

    -- * DeploymentStatus
    , DeploymentStatus (..)

    -- * S3Location
    , S3Location
    , s3Location
    , slBucket
    , slBundleType
    , slETag
    , slKey
    , slVersion

    -- * MinimumHealthyHostsType
    , MinimumHealthyHostsType (..)

    -- * GitHubLocation
    , GitHubLocation
    , gitHubLocation
    , ghlCommitId
    , ghlRepository

    -- * RevisionLocationType
    , RevisionLocationType (..)

    -- * EC2TagFilterType
    , EC2TagFilterType (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2014-10-06@ of the Amazon CodeDeploy service.
data CodeDeploy

instance AWSService CodeDeploy where
    type Sg CodeDeploy = V4
    type Er CodeDeploy = JSONError

    service = service'
      where
        service' :: Service CodeDeploy
        service' = Service
            { _svcAbbrev       = "CodeDeploy"
            , _svcPrefix       = "codedeploy"
            , _svcVersion      = "2014-10-06"
            , _svcTargetPrefix = Just "CodeDeploy_20141006"
            , _svcJSONVersion  = Just "1.1"
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry CodeDeploy
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data GenericRevisionInfo = GenericRevisionInfo
    { _griDeploymentGroups :: List "deploymentGroups" Text
    , _griDescription      :: Maybe Text
    , _griFirstUsedTime    :: Maybe POSIX
    , _griLastUsedTime     :: Maybe POSIX
    , _griRegisterTime     :: Maybe POSIX
    } deriving (Eq, Ord, Read, Show)

-- | 'GenericRevisionInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'griDeploymentGroups' @::@ ['Text']
--
-- * 'griDescription' @::@ 'Maybe' 'Text'
--
-- * 'griFirstUsedTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'griLastUsedTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'griRegisterTime' @::@ 'Maybe' 'UTCTime'
--
genericRevisionInfo :: GenericRevisionInfo
genericRevisionInfo = GenericRevisionInfo
    { _griDescription      = Nothing
    , _griDeploymentGroups = mempty
    , _griFirstUsedTime    = Nothing
    , _griLastUsedTime     = Nothing
    , _griRegisterTime     = Nothing
    }

-- | A list of deployment groups that use this revision.
griDeploymentGroups :: Lens' GenericRevisionInfo [Text]
griDeploymentGroups =
    lens _griDeploymentGroups (\s a -> s { _griDeploymentGroups = a })
        . _List

-- | A comment about the revision.
griDescription :: Lens' GenericRevisionInfo (Maybe Text)
griDescription = lens _griDescription (\s a -> s { _griDescription = a })

-- | When the revision was first used by AWS CodeDeploy.
griFirstUsedTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griFirstUsedTime = lens _griFirstUsedTime (\s a -> s { _griFirstUsedTime = a }) . mapping _Time

-- | When the revision was last used by AWS CodeDeploy.
griLastUsedTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griLastUsedTime = lens _griLastUsedTime (\s a -> s { _griLastUsedTime = a }) . mapping _Time

-- | When the revision was registered with AWS CodeDeploy.
griRegisterTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griRegisterTime = lens _griRegisterTime (\s a -> s { _griRegisterTime = a }) . mapping _Time

instance FromJSON GenericRevisionInfo where
    parseJSON = withObject "GenericRevisionInfo" $ \o -> GenericRevisionInfo
        <$> o .:? "deploymentGroups" .!= mempty
        <*> o .:? "description"
        <*> o .:? "firstUsedTime"
        <*> o .:? "lastUsedTime"
        <*> o .:? "registerTime"

instance ToJSON GenericRevisionInfo where
    toJSON GenericRevisionInfo{..} = object
        [ "description"      .= _griDescription
        , "deploymentGroups" .= _griDeploymentGroups
        , "firstUsedTime"    .= _griFirstUsedTime
        , "lastUsedTime"     .= _griLastUsedTime
        , "registerTime"     .= _griRegisterTime
        ]

data ApplicationInfo = ApplicationInfo
    { _aiApplicationId   :: Maybe Text
    , _aiApplicationName :: Maybe Text
    , _aiCreateTime      :: Maybe POSIX
    , _aiLinkedToGitHub  :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'ApplicationInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aiApplicationId' @::@ 'Maybe' 'Text'
--
-- * 'aiApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'aiCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'aiLinkedToGitHub' @::@ 'Maybe' 'Bool'
--
applicationInfo :: ApplicationInfo
applicationInfo = ApplicationInfo
    { _aiApplicationId   = Nothing
    , _aiApplicationName = Nothing
    , _aiCreateTime      = Nothing
    , _aiLinkedToGitHub  = Nothing
    }

-- | The application ID.
aiApplicationId :: Lens' ApplicationInfo (Maybe Text)
aiApplicationId = lens _aiApplicationId (\s a -> s { _aiApplicationId = a })

-- | The application name.
aiApplicationName :: Lens' ApplicationInfo (Maybe Text)
aiApplicationName =
    lens _aiApplicationName (\s a -> s { _aiApplicationName = a })

-- | The time that the application was created.
aiCreateTime :: Lens' ApplicationInfo (Maybe UTCTime)
aiCreateTime = lens _aiCreateTime (\s a -> s { _aiCreateTime = a }) . mapping _Time

-- | True if the user has authenticated with GitHub for the specified application;
-- otherwise, false.
aiLinkedToGitHub :: Lens' ApplicationInfo (Maybe Bool)
aiLinkedToGitHub = lens _aiLinkedToGitHub (\s a -> s { _aiLinkedToGitHub = a })

instance FromJSON ApplicationInfo where
    parseJSON = withObject "ApplicationInfo" $ \o -> ApplicationInfo
        <$> o .:? "applicationId"
        <*> o .:? "applicationName"
        <*> o .:? "createTime"
        <*> o .:? "linkedToGitHub"

instance ToJSON ApplicationInfo where
    toJSON ApplicationInfo{..} = object
        [ "applicationId"   .= _aiApplicationId
        , "applicationName" .= _aiApplicationName
        , "createTime"      .= _aiCreateTime
        , "linkedToGitHub"  .= _aiLinkedToGitHub
        ]

data BundleType
    = Tar -- ^ tar
    | Tgz -- ^ tgz
    | Zip -- ^ zip
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable BundleType

instance FromText BundleType where
    parser = takeLowerText >>= \case
        "tar" -> pure Tar
        "tgz" -> pure Tgz
        "zip" -> pure Zip
        e     -> fail $
            "Failure parsing BundleType from " ++ show e

instance ToText BundleType where
    toText = \case
        Tar -> "tar"
        Tgz -> "tgz"
        Zip -> "zip"

instance ToByteString BundleType
instance ToHeader     BundleType
instance ToQuery      BundleType

instance FromJSON BundleType where
    parseJSON = parseJSONText "BundleType"

instance ToJSON BundleType where
    toJSON = toJSONText

data TimeRange = TimeRange
    { _trEnd   :: Maybe POSIX
    , _trStart :: Maybe POSIX
    } deriving (Eq, Ord, Read, Show)

-- | 'TimeRange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trEnd' @::@ 'Maybe' 'UTCTime'
--
-- * 'trStart' @::@ 'Maybe' 'UTCTime'
--
timeRange :: TimeRange
timeRange = TimeRange
    { _trStart = Nothing
    , _trEnd   = Nothing
    }

-- | The time range's end time.
--
-- Specify null to leave the time range's end time open-ended.
trEnd :: Lens' TimeRange (Maybe UTCTime)
trEnd = lens _trEnd (\s a -> s { _trEnd = a }) . mapping _Time

-- | The time range's start time.
--
-- Specify null to leave the time range's start time open-ended.
trStart :: Lens' TimeRange (Maybe UTCTime)
trStart = lens _trStart (\s a -> s { _trStart = a }) . mapping _Time

instance FromJSON TimeRange where
    parseJSON = withObject "TimeRange" $ \o -> TimeRange
        <$> o .:? "end"
        <*> o .:? "start"

instance ToJSON TimeRange where
    toJSON TimeRange{..} = object
        [ "start" .= _trStart
        , "end"   .= _trEnd
        ]

data DeploymentCreator
    = Autoscaling -- ^ autoscaling
    | User        -- ^ user
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DeploymentCreator

instance FromText DeploymentCreator where
    parser = takeLowerText >>= \case
        "autoscaling" -> pure Autoscaling
        "user"        -> pure User
        e             -> fail $
            "Failure parsing DeploymentCreator from " ++ show e

instance ToText DeploymentCreator where
    toText = \case
        Autoscaling -> "autoscaling"
        User        -> "user"

instance ToByteString DeploymentCreator
instance ToHeader     DeploymentCreator
instance ToQuery      DeploymentCreator

instance FromJSON DeploymentCreator where
    parseJSON = parseJSONText "DeploymentCreator"

instance ToJSON DeploymentCreator where
    toJSON = toJSONText

data InstanceSummary = InstanceSummary
    { _isDeploymentId    :: Maybe Text
    , _isInstanceId      :: Maybe Text
    , _isLastUpdatedAt   :: Maybe POSIX
    , _isLifecycleEvents :: List "lifecycleEvents" LifecycleEvent
    , _isStatus          :: Maybe InstanceStatus
    } deriving (Eq, Read, Show)

-- | 'InstanceSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isDeploymentId' @::@ 'Maybe' 'Text'
--
-- * 'isInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'isLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'isLifecycleEvents' @::@ ['LifecycleEvent']
--
-- * 'isStatus' @::@ 'Maybe' 'InstanceStatus'
--
instanceSummary :: InstanceSummary
instanceSummary = InstanceSummary
    { _isDeploymentId    = Nothing
    , _isInstanceId      = Nothing
    , _isStatus          = Nothing
    , _isLastUpdatedAt   = Nothing
    , _isLifecycleEvents = mempty
    }

-- | The deployment ID.
isDeploymentId :: Lens' InstanceSummary (Maybe Text)
isDeploymentId = lens _isDeploymentId (\s a -> s { _isDeploymentId = a })

-- | The instance ID.
isInstanceId :: Lens' InstanceSummary (Maybe Text)
isInstanceId = lens _isInstanceId (\s a -> s { _isInstanceId = a })

-- | A timestamp indicating when the instance information was last updated.
isLastUpdatedAt :: Lens' InstanceSummary (Maybe UTCTime)
isLastUpdatedAt = lens _isLastUpdatedAt (\s a -> s { _isLastUpdatedAt = a }) . mapping _Time

-- | A list of lifecycle events for this instance.
isLifecycleEvents :: Lens' InstanceSummary [LifecycleEvent]
isLifecycleEvents =
    lens _isLifecycleEvents (\s a -> s { _isLifecycleEvents = a })
        . _List

-- | The deployment status for this instance:
--
-- Pending: The deployment is pending for this instance. In Progress: The
-- deployment is in progress for this instance. Succeeded: The deployment has
-- succeeded for this instance. Failed: The deployment has failed for this
-- instance. Skipped: The deployment has been skipped for this instance. Unknown: The deployment status is unknown for this instance.
--
isStatus :: Lens' InstanceSummary (Maybe InstanceStatus)
isStatus = lens _isStatus (\s a -> s { _isStatus = a })

instance FromJSON InstanceSummary where
    parseJSON = withObject "InstanceSummary" $ \o -> InstanceSummary
        <$> o .:? "deploymentId"
        <*> o .:? "instanceId"
        <*> o .:? "lastUpdatedAt"
        <*> o .:? "lifecycleEvents" .!= mempty
        <*> o .:? "status"

instance ToJSON InstanceSummary where
    toJSON InstanceSummary{..} = object
        [ "deploymentId"    .= _isDeploymentId
        , "instanceId"      .= _isInstanceId
        , "status"          .= _isStatus
        , "lastUpdatedAt"   .= _isLastUpdatedAt
        , "lifecycleEvents" .= _isLifecycleEvents
        ]

data AutoScalingGroup = AutoScalingGroup
    { _asgHook :: Maybe Text
    , _asgName :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AutoScalingGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgHook' @::@ 'Maybe' 'Text'
--
-- * 'asgName' @::@ 'Maybe' 'Text'
--
autoScalingGroup :: AutoScalingGroup
autoScalingGroup = AutoScalingGroup
    { _asgName = Nothing
    , _asgHook = Nothing
    }

-- | An Auto Scaling lifecycle event hook name.
asgHook :: Lens' AutoScalingGroup (Maybe Text)
asgHook = lens _asgHook (\s a -> s { _asgHook = a })

-- | The Auto Scaling group name.
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\s a -> s { _asgName = a })

instance FromJSON AutoScalingGroup where
    parseJSON = withObject "AutoScalingGroup" $ \o -> AutoScalingGroup
        <$> o .:? "hook"
        <*> o .:? "name"

instance ToJSON AutoScalingGroup where
    toJSON AutoScalingGroup{..} = object
        [ "name" .= _asgName
        , "hook" .= _asgHook
        ]

data DeploymentGroupInfo = DeploymentGroupInfo
    { _dgiApplicationName      :: Maybe Text
    , _dgiAutoScalingGroups    :: List "autoScalingGroups" AutoScalingGroup
    , _dgiDeploymentConfigName :: Maybe Text
    , _dgiDeploymentGroupId    :: Maybe Text
    , _dgiDeploymentGroupName  :: Maybe Text
    , _dgiEc2TagFilters        :: List "ec2TagFilters" EC2TagFilter
    , _dgiServiceRoleArn       :: Maybe Text
    , _dgiTargetRevision       :: Maybe RevisionLocation
    } deriving (Eq, Read, Show)

-- | 'DeploymentGroupInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgiApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'dgiAutoScalingGroups' @::@ ['AutoScalingGroup']
--
-- * 'dgiDeploymentConfigName' @::@ 'Maybe' 'Text'
--
-- * 'dgiDeploymentGroupId' @::@ 'Maybe' 'Text'
--
-- * 'dgiDeploymentGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dgiEc2TagFilters' @::@ ['EC2TagFilter']
--
-- * 'dgiServiceRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'dgiTargetRevision' @::@ 'Maybe' 'RevisionLocation'
--
deploymentGroupInfo :: DeploymentGroupInfo
deploymentGroupInfo = DeploymentGroupInfo
    { _dgiApplicationName      = Nothing
    , _dgiDeploymentGroupId    = Nothing
    , _dgiDeploymentGroupName  = Nothing
    , _dgiDeploymentConfigName = Nothing
    , _dgiEc2TagFilters        = mempty
    , _dgiAutoScalingGroups    = mempty
    , _dgiServiceRoleArn       = Nothing
    , _dgiTargetRevision       = Nothing
    }

-- | The application name.
dgiApplicationName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiApplicationName =
    lens _dgiApplicationName (\s a -> s { _dgiApplicationName = a })

-- | A list of associated Auto Scaling groups.
dgiAutoScalingGroups :: Lens' DeploymentGroupInfo [AutoScalingGroup]
dgiAutoScalingGroups =
    lens _dgiAutoScalingGroups (\s a -> s { _dgiAutoScalingGroups = a })
        . _List

-- | The deployment configuration name.
dgiDeploymentConfigName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentConfigName =
    lens _dgiDeploymentConfigName (\s a -> s { _dgiDeploymentConfigName = a })

-- | The deployment group ID.
dgiDeploymentGroupId :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentGroupId =
    lens _dgiDeploymentGroupId (\s a -> s { _dgiDeploymentGroupId = a })

-- | The deployment group name.
dgiDeploymentGroupName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentGroupName =
    lens _dgiDeploymentGroupName (\s a -> s { _dgiDeploymentGroupName = a })

-- | The Amazon EC2 tags to filter on.
dgiEc2TagFilters :: Lens' DeploymentGroupInfo [EC2TagFilter]
dgiEc2TagFilters = lens _dgiEc2TagFilters (\s a -> s { _dgiEc2TagFilters = a }) . _List

-- | A service role ARN.
dgiServiceRoleArn :: Lens' DeploymentGroupInfo (Maybe Text)
dgiServiceRoleArn =
    lens _dgiServiceRoleArn (\s a -> s { _dgiServiceRoleArn = a })

-- | Information about the deployment group's target revision, including the
-- revision's type and its location.
dgiTargetRevision :: Lens' DeploymentGroupInfo (Maybe RevisionLocation)
dgiTargetRevision =
    lens _dgiTargetRevision (\s a -> s { _dgiTargetRevision = a })

instance FromJSON DeploymentGroupInfo where
    parseJSON = withObject "DeploymentGroupInfo" $ \o -> DeploymentGroupInfo
        <$> o .:? "applicationName"
        <*> o .:? "autoScalingGroups" .!= mempty
        <*> o .:? "deploymentConfigName"
        <*> o .:? "deploymentGroupId"
        <*> o .:? "deploymentGroupName"
        <*> o .:? "ec2TagFilters" .!= mempty
        <*> o .:? "serviceRoleArn"
        <*> o .:? "targetRevision"

instance ToJSON DeploymentGroupInfo where
    toJSON DeploymentGroupInfo{..} = object
        [ "applicationName"      .= _dgiApplicationName
        , "deploymentGroupId"    .= _dgiDeploymentGroupId
        , "deploymentGroupName"  .= _dgiDeploymentGroupName
        , "deploymentConfigName" .= _dgiDeploymentConfigName
        , "ec2TagFilters"        .= _dgiEc2TagFilters
        , "autoScalingGroups"    .= _dgiAutoScalingGroups
        , "serviceRoleArn"       .= _dgiServiceRoleArn
        , "targetRevision"       .= _dgiTargetRevision
        ]

data ApplicationRevisionSortBy
    = FirstUsedTime -- ^ firstUsedTime
    | LastUsedTime  -- ^ lastUsedTime
    | RegisterTime  -- ^ registerTime
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ApplicationRevisionSortBy

instance FromText ApplicationRevisionSortBy where
    parser = takeLowerText >>= \case
        "firstusedtime" -> pure FirstUsedTime
        "lastusedtime"  -> pure LastUsedTime
        "registertime"  -> pure RegisterTime
        e               -> fail $
            "Failure parsing ApplicationRevisionSortBy from " ++ show e

instance ToText ApplicationRevisionSortBy where
    toText = \case
        FirstUsedTime -> "firstUsedTime"
        LastUsedTime  -> "lastUsedTime"
        RegisterTime  -> "registerTime"

instance ToByteString ApplicationRevisionSortBy
instance ToHeader     ApplicationRevisionSortBy
instance ToQuery      ApplicationRevisionSortBy

instance FromJSON ApplicationRevisionSortBy where
    parseJSON = parseJSONText "ApplicationRevisionSortBy"

instance ToJSON ApplicationRevisionSortBy where
    toJSON = toJSONText

data MinimumHealthyHosts = MinimumHealthyHosts
    { _mhhType  :: Maybe MinimumHealthyHostsType
    , _mhhValue :: Maybe Int
    } deriving (Eq, Read, Show)

-- | 'MinimumHealthyHosts' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhhType' @::@ 'Maybe' 'MinimumHealthyHostsType'
--
-- * 'mhhValue' @::@ 'Maybe' 'Int'
--
minimumHealthyHosts :: MinimumHealthyHosts
minimumHealthyHosts = MinimumHealthyHosts
    { _mhhValue = Nothing
    , _mhhType  = Nothing
    }

-- | The minimum healthy instances type:
--
-- HOST_COUNT: The minimum number of healthy instances, as an absolute value. FLEET_PERCENT: The minimum number of healthy instances, as a percentage of the total number of instances in the deployment.
-- For example, for 9 Amazon EC2 instances, if a HOST_COUNT of 6 is specified,
-- deploy to up to 3 instances at a time. The deployment succeeds if 6 or more
-- instances are successfully deployed to; otherwise, the deployment fails. If a
-- FLEET_PERCENT of 40 is specified, deploy to up to 5 instances at a time. The
-- deployment succeeds if 4 or more instances are successfully deployed to;
-- otherwise, the deployment fails.
--
-- In a call to the get deployment configuration operation,
-- CodeDeployDefault.OneAtATime will return a minimum healthy instances type of
-- MOST_CONCURRENCY and a value of 1. This means a deployment to only one Amazon
-- EC2 instance at a time. (You cannot set the type to MOST_CONCURRENCY, only to
-- HOST_COUNT or FLEET_PERCENT.)
mhhType :: Lens' MinimumHealthyHosts (Maybe MinimumHealthyHostsType)
mhhType = lens _mhhType (\s a -> s { _mhhType = a })

-- | The minimum healthy instances value.
mhhValue :: Lens' MinimumHealthyHosts (Maybe Int)
mhhValue = lens _mhhValue (\s a -> s { _mhhValue = a })

instance FromJSON MinimumHealthyHosts where
    parseJSON = withObject "MinimumHealthyHosts" $ \o -> MinimumHealthyHosts
        <$> o .:? "type"
        <*> o .:? "value"

instance ToJSON MinimumHealthyHosts where
    toJSON MinimumHealthyHosts{..} = object
        [ "value" .= _mhhValue
        , "type"  .= _mhhType
        ]

data ListStateFilterAction
    = Exclude -- ^ exclude
    | Ignore  -- ^ ignore
    | Include -- ^ include
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ListStateFilterAction

instance FromText ListStateFilterAction where
    parser = takeLowerText >>= \case
        "exclude" -> pure Exclude
        "ignore"  -> pure Ignore
        "include" -> pure Include
        e         -> fail $
            "Failure parsing ListStateFilterAction from " ++ show e

instance ToText ListStateFilterAction where
    toText = \case
        Exclude -> "exclude"
        Ignore  -> "ignore"
        Include -> "include"

instance ToByteString ListStateFilterAction
instance ToHeader     ListStateFilterAction
instance ToQuery      ListStateFilterAction

instance FromJSON ListStateFilterAction where
    parseJSON = parseJSONText "ListStateFilterAction"

instance ToJSON ListStateFilterAction where
    toJSON = toJSONText

data LifecycleErrorCode
    = ScriptFailed        -- ^ ScriptFailed
    | ScriptMissing       -- ^ ScriptMissing
    | ScriptNotExecutable -- ^ ScriptNotExecutable
    | ScriptTimedOut      -- ^ ScriptTimedOut
    | Success             -- ^ Success
    | UnknownError        -- ^ UnknownError
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable LifecycleErrorCode

instance FromText LifecycleErrorCode where
    parser = takeLowerText >>= \case
        "scriptfailed"        -> pure ScriptFailed
        "scriptmissing"       -> pure ScriptMissing
        "scriptnotexecutable" -> pure ScriptNotExecutable
        "scripttimedout"      -> pure ScriptTimedOut
        "success"             -> pure Success
        "unknownerror"        -> pure UnknownError
        e                     -> fail $
            "Failure parsing LifecycleErrorCode from " ++ show e

instance ToText LifecycleErrorCode where
    toText = \case
        ScriptFailed        -> "ScriptFailed"
        ScriptMissing       -> "ScriptMissing"
        ScriptNotExecutable -> "ScriptNotExecutable"
        ScriptTimedOut      -> "ScriptTimedOut"
        Success             -> "Success"
        UnknownError        -> "UnknownError"

instance ToByteString LifecycleErrorCode
instance ToHeader     LifecycleErrorCode
instance ToQuery      LifecycleErrorCode

instance FromJSON LifecycleErrorCode where
    parseJSON = parseJSONText "LifecycleErrorCode"

instance ToJSON LifecycleErrorCode where
    toJSON = toJSONText

data RevisionLocation = RevisionLocation
    { _rlGitHubLocation :: Maybe GitHubLocation
    , _rlRevisionType   :: Maybe RevisionLocationType
    , _rlS3Location     :: Maybe S3Location
    } deriving (Eq, Read, Show)

-- | 'RevisionLocation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rlGitHubLocation' @::@ 'Maybe' 'GitHubLocation'
--
-- * 'rlRevisionType' @::@ 'Maybe' 'RevisionLocationType'
--
-- * 'rlS3Location' @::@ 'Maybe' 'S3Location'
--
revisionLocation :: RevisionLocation
revisionLocation = RevisionLocation
    { _rlRevisionType   = Nothing
    , _rlS3Location     = Nothing
    , _rlGitHubLocation = Nothing
    }

rlGitHubLocation :: Lens' RevisionLocation (Maybe GitHubLocation)
rlGitHubLocation = lens _rlGitHubLocation (\s a -> s { _rlGitHubLocation = a })

-- | The application revision's type:
--
-- S3: An application revision stored in Amazon S3. GitHub: An application
-- revision stored in GitHub.
rlRevisionType :: Lens' RevisionLocation (Maybe RevisionLocationType)
rlRevisionType = lens _rlRevisionType (\s a -> s { _rlRevisionType = a })

rlS3Location :: Lens' RevisionLocation (Maybe S3Location)
rlS3Location = lens _rlS3Location (\s a -> s { _rlS3Location = a })

instance FromJSON RevisionLocation where
    parseJSON = withObject "RevisionLocation" $ \o -> RevisionLocation
        <$> o .:? "gitHubLocation"
        <*> o .:? "revisionType"
        <*> o .:? "s3Location"

instance ToJSON RevisionLocation where
    toJSON RevisionLocation{..} = object
        [ "revisionType"   .= _rlRevisionType
        , "s3Location"     .= _rlS3Location
        , "gitHubLocation" .= _rlGitHubLocation
        ]

data LifecycleEventStatus
    = Failed     -- ^ Failed
    | InProgress -- ^ InProgress
    | Pending    -- ^ Pending
    | Skipped    -- ^ Skipped
    | Succeeded  -- ^ Succeeded
    | Unknown    -- ^ Unknown
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable LifecycleEventStatus

instance FromText LifecycleEventStatus where
    parser = takeLowerText >>= \case
        "failed"     -> pure Failed
        "inprogress" -> pure InProgress
        "pending"    -> pure Pending
        "skipped"    -> pure Skipped
        "succeeded"  -> pure Succeeded
        "unknown"    -> pure Unknown
        e            -> fail $
            "Failure parsing LifecycleEventStatus from " ++ show e

instance ToText LifecycleEventStatus where
    toText = \case
        Failed     -> "Failed"
        InProgress -> "InProgress"
        Pending    -> "Pending"
        Skipped    -> "Skipped"
        Succeeded  -> "Succeeded"
        Unknown    -> "Unknown"

instance ToByteString LifecycleEventStatus
instance ToHeader     LifecycleEventStatus
instance ToQuery      LifecycleEventStatus

instance FromJSON LifecycleEventStatus where
    parseJSON = parseJSONText "LifecycleEventStatus"

instance ToJSON LifecycleEventStatus where
    toJSON = toJSONText

data EC2TagFilter = EC2TagFilter
    { _ectfKey   :: Maybe Text
    , _ectfType  :: Maybe EC2TagFilterType
    , _ectfValue :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'EC2TagFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ectfKey' @::@ 'Maybe' 'Text'
--
-- * 'ectfType' @::@ 'Maybe' 'EC2TagFilterType'
--
-- * 'ectfValue' @::@ 'Maybe' 'Text'
--
ec2TagFilter :: EC2TagFilter
ec2TagFilter = EC2TagFilter
    { _ectfKey   = Nothing
    , _ectfValue = Nothing
    , _ectfType  = Nothing
    }

-- | The Amazon EC2 tag filter key.
ectfKey :: Lens' EC2TagFilter (Maybe Text)
ectfKey = lens _ectfKey (\s a -> s { _ectfKey = a })

-- | The Amazon EC2 tag filter type:
--
-- KEY_ONLY: Key only. VALUE_ONLY: Value only. KEY_AND_VALUE: Key and value.
ectfType :: Lens' EC2TagFilter (Maybe EC2TagFilterType)
ectfType = lens _ectfType (\s a -> s { _ectfType = a })

-- | The Amazon EC2 tag filter value.
ectfValue :: Lens' EC2TagFilter (Maybe Text)
ectfValue = lens _ectfValue (\s a -> s { _ectfValue = a })

instance FromJSON EC2TagFilter where
    parseJSON = withObject "EC2TagFilter" $ \o -> EC2TagFilter
        <$> o .:? "Key"
        <*> o .:? "Type"
        <*> o .:? "Value"

instance ToJSON EC2TagFilter where
    toJSON EC2TagFilter{..} = object
        [ "Key"   .= _ectfKey
        , "Value" .= _ectfValue
        , "Type"  .= _ectfType
        ]

data Diagnostics = Diagnostics
    { _dErrorCode  :: Maybe LifecycleErrorCode
    , _dLogTail    :: Maybe Text
    , _dMessage    :: Maybe Text
    , _dScriptName :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Diagnostics' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dErrorCode' @::@ 'Maybe' 'LifecycleErrorCode'
--
-- * 'dLogTail' @::@ 'Maybe' 'Text'
--
-- * 'dMessage' @::@ 'Maybe' 'Text'
--
-- * 'dScriptName' @::@ 'Maybe' 'Text'
--
diagnostics :: Diagnostics
diagnostics = Diagnostics
    { _dErrorCode  = Nothing
    , _dScriptName = Nothing
    , _dMessage    = Nothing
    , _dLogTail    = Nothing
    }

-- | The associated error code:
--
-- Success: The specified script ran. ScriptMissing: The specified script was
-- not found in the specified location. ScriptNotExecutable: The specified
-- script is not a recognized executable file type. ScriptTimedOut: The
-- specified script did not finish running in the specified time period. ScriptFailed: The specified script failed to run as expected.
-- UnknownError: The specified script did not run for an unknown reason.
dErrorCode :: Lens' Diagnostics (Maybe LifecycleErrorCode)
dErrorCode = lens _dErrorCode (\s a -> s { _dErrorCode = a })

-- | The last portion of the associated diagnostic log.
dLogTail :: Lens' Diagnostics (Maybe Text)
dLogTail = lens _dLogTail (\s a -> s { _dLogTail = a })

-- | The message associated with the error.
dMessage :: Lens' Diagnostics (Maybe Text)
dMessage = lens _dMessage (\s a -> s { _dMessage = a })

-- | The name of the script.
dScriptName :: Lens' Diagnostics (Maybe Text)
dScriptName = lens _dScriptName (\s a -> s { _dScriptName = a })

instance FromJSON Diagnostics where
    parseJSON = withObject "Diagnostics" $ \o -> Diagnostics
        <$> o .:? "errorCode"
        <*> o .:? "logTail"
        <*> o .:? "message"
        <*> o .:? "scriptName"

instance ToJSON Diagnostics where
    toJSON Diagnostics{..} = object
        [ "errorCode"  .= _dErrorCode
        , "scriptName" .= _dScriptName
        , "message"    .= _dMessage
        , "logTail"    .= _dLogTail
        ]

data StopStatus
    = SSPending   -- ^ Pending
    | SSSucceeded -- ^ Succeeded
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable StopStatus

instance FromText StopStatus where
    parser = takeLowerText >>= \case
        "pending"   -> pure SSPending
        "succeeded" -> pure SSSucceeded
        e           -> fail $
            "Failure parsing StopStatus from " ++ show e

instance ToText StopStatus where
    toText = \case
        SSPending   -> "Pending"
        SSSucceeded -> "Succeeded"

instance ToByteString StopStatus
instance ToHeader     StopStatus
instance ToQuery      StopStatus

instance FromJSON StopStatus where
    parseJSON = parseJSONText "StopStatus"

instance ToJSON StopStatus where
    toJSON = toJSONText

data ErrorInformation = ErrorInformation
    { _eiCode    :: Maybe ErrorCode
    , _eiMessage :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ErrorInformation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eiCode' @::@ 'Maybe' 'ErrorCode'
--
-- * 'eiMessage' @::@ 'Maybe' 'Text'
--
errorInformation :: ErrorInformation
errorInformation = ErrorInformation
    { _eiCode    = Nothing
    , _eiMessage = Nothing
    }

-- | The error code:
--
-- APPLICATION_MISSING: The application was missing. Note that this error code
-- will most likely be raised if the application is deleted after the deployment
-- is created but before it starts. DEPLOYMENT_GROUP_MISSING: The deployment
-- group was missing. Note that this error code will most likely be raised if
-- the deployment group is deleted after the deployment is created but before it
-- starts. REVISION_MISSING: The revision ID was missing. Note that this error
-- code will most likely be raised if the revision is deleted after the
-- deployment is created but before it starts. IAM_ROLE_MISSING: The service
-- role cannot be accessed. IAM_ROLE_PERMISSIONS: The service role does not have
-- the correct permissions. OVER_MAX_INSTANCES: The maximum number of instances
-- was exceeded. NO_INSTANCES: No instances were specified, or no instances can
-- be found. TIMEOUT: The deployment has timed out. HEALTH_CONSTRAINTS_INVALID:
-- The revision can never successfully deploy under the instance health
-- constraints as specified. HEALTH_CONSTRAINTS: The deployment failed on too
-- many instances to be able to successfully deploy under the specified instance
-- health constraints. INTERNAL_ERROR: There was an internal error.
eiCode :: Lens' ErrorInformation (Maybe ErrorCode)
eiCode = lens _eiCode (\s a -> s { _eiCode = a })

-- | An accompanying error message.
eiMessage :: Lens' ErrorInformation (Maybe Text)
eiMessage = lens _eiMessage (\s a -> s { _eiMessage = a })

instance FromJSON ErrorInformation where
    parseJSON = withObject "ErrorInformation" $ \o -> ErrorInformation
        <$> o .:? "code"
        <*> o .:? "message"

instance ToJSON ErrorInformation where
    toJSON ErrorInformation{..} = object
        [ "code"    .= _eiCode
        , "message" .= _eiMessage
        ]

data SortOrder
    = Ascending  -- ^ ascending
    | Descending -- ^ descending
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SortOrder

instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "ascending"  -> pure Ascending
        "descending" -> pure Descending
        e            -> fail $
            "Failure parsing SortOrder from " ++ show e

instance ToText SortOrder where
    toText = \case
        Ascending  -> "ascending"
        Descending -> "descending"

instance ToByteString SortOrder
instance ToHeader     SortOrder
instance ToQuery      SortOrder

instance FromJSON SortOrder where
    parseJSON = parseJSONText "SortOrder"

instance ToJSON SortOrder where
    toJSON = toJSONText

data DeploymentInfo = DeploymentInfo
    { _diApplicationName               :: Maybe Text
    , _diCompleteTime                  :: Maybe POSIX
    , _diCreateTime                    :: Maybe POSIX
    , _diCreator                       :: Maybe DeploymentCreator
    , _diDeploymentConfigName          :: Maybe Text
    , _diDeploymentGroupName           :: Maybe Text
    , _diDeploymentId                  :: Maybe Text
    , _diDeploymentOverview            :: Maybe DeploymentOverview
    , _diDescription                   :: Maybe Text
    , _diErrorInformation              :: Maybe ErrorInformation
    , _diIgnoreApplicationStopFailures :: Maybe Bool
    , _diRevision                      :: Maybe RevisionLocation
    , _diStartTime                     :: Maybe POSIX
    , _diStatus                        :: Maybe DeploymentStatus
    } deriving (Eq, Read, Show)

-- | 'DeploymentInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'diCompleteTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'diCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'diCreator' @::@ 'Maybe' 'DeploymentCreator'
--
-- * 'diDeploymentConfigName' @::@ 'Maybe' 'Text'
--
-- * 'diDeploymentGroupName' @::@ 'Maybe' 'Text'
--
-- * 'diDeploymentId' @::@ 'Maybe' 'Text'
--
-- * 'diDeploymentOverview' @::@ 'Maybe' 'DeploymentOverview'
--
-- * 'diDescription' @::@ 'Maybe' 'Text'
--
-- * 'diErrorInformation' @::@ 'Maybe' 'ErrorInformation'
--
-- * 'diIgnoreApplicationStopFailures' @::@ 'Maybe' 'Bool'
--
-- * 'diRevision' @::@ 'Maybe' 'RevisionLocation'
--
-- * 'diStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'diStatus' @::@ 'Maybe' 'DeploymentStatus'
--
deploymentInfo :: DeploymentInfo
deploymentInfo = DeploymentInfo
    { _diApplicationName               = Nothing
    , _diDeploymentGroupName           = Nothing
    , _diDeploymentConfigName          = Nothing
    , _diDeploymentId                  = Nothing
    , _diRevision                      = Nothing
    , _diStatus                        = Nothing
    , _diErrorInformation              = Nothing
    , _diCreateTime                    = Nothing
    , _diStartTime                     = Nothing
    , _diCompleteTime                  = Nothing
    , _diDeploymentOverview            = Nothing
    , _diDescription                   = Nothing
    , _diCreator                       = Nothing
    , _diIgnoreApplicationStopFailures = Nothing
    }

-- | The application name.
diApplicationName :: Lens' DeploymentInfo (Maybe Text)
diApplicationName =
    lens _diApplicationName (\s a -> s { _diApplicationName = a })

-- | A timestamp indicating when the deployment was completed.
diCompleteTime :: Lens' DeploymentInfo (Maybe UTCTime)
diCompleteTime = lens _diCompleteTime (\s a -> s { _diCompleteTime = a }) . mapping _Time

-- | A timestamp indicating when the deployment was created.
diCreateTime :: Lens' DeploymentInfo (Maybe UTCTime)
diCreateTime = lens _diCreateTime (\s a -> s { _diCreateTime = a }) . mapping _Time

-- | How the deployment was created:
--
-- user: A user created the deployment. autoscaling: Auto Scaling created the
-- deployment.
diCreator :: Lens' DeploymentInfo (Maybe DeploymentCreator)
diCreator = lens _diCreator (\s a -> s { _diCreator = a })

-- | The deployment configuration name.
diDeploymentConfigName :: Lens' DeploymentInfo (Maybe Text)
diDeploymentConfigName =
    lens _diDeploymentConfigName (\s a -> s { _diDeploymentConfigName = a })

-- | The deployment group name.
diDeploymentGroupName :: Lens' DeploymentInfo (Maybe Text)
diDeploymentGroupName =
    lens _diDeploymentGroupName (\s a -> s { _diDeploymentGroupName = a })

-- | The deployment ID.
diDeploymentId :: Lens' DeploymentInfo (Maybe Text)
diDeploymentId = lens _diDeploymentId (\s a -> s { _diDeploymentId = a })

-- | A summary of the deployment status of the instances in the deployment.
diDeploymentOverview :: Lens' DeploymentInfo (Maybe DeploymentOverview)
diDeploymentOverview =
    lens _diDeploymentOverview (\s a -> s { _diDeploymentOverview = a })

-- | A comment about the deployment.
diDescription :: Lens' DeploymentInfo (Maybe Text)
diDescription = lens _diDescription (\s a -> s { _diDescription = a })

-- | Information about any error associated with this deployment.
diErrorInformation :: Lens' DeploymentInfo (Maybe ErrorInformation)
diErrorInformation =
    lens _diErrorInformation (\s a -> s { _diErrorInformation = a })

-- | If true, then if the deployment causes the ApplicationStop deployment
-- lifecycle event to fail to a specific instance, the deployment will not be
-- considered to have failed to that instance at that point and will continue on
-- to the BeforeInstall deployment lifecycle event.
--
-- If false or not specified, then if the deployment causes the ApplicationStop
-- deployment lifecycle event to fail to a specific instance, the deployment
-- will stop to that instance, and the deployment to that instance will be
-- considered to have failed.
diIgnoreApplicationStopFailures :: Lens' DeploymentInfo (Maybe Bool)
diIgnoreApplicationStopFailures =
    lens _diIgnoreApplicationStopFailures
        (\s a -> s { _diIgnoreApplicationStopFailures = a })

-- | Information about the location of application artifacts that are stored and
-- the service to retrieve them from.
diRevision :: Lens' DeploymentInfo (Maybe RevisionLocation)
diRevision = lens _diRevision (\s a -> s { _diRevision = a })

-- | A timestamp indicating when the deployment began deploying to the deployment
-- group.
--
-- Note that in some cases, the reported value of the start time may be later
-- than the complete time. This is due to differences in the clock settings of
-- various back-end servers that participate in the overall deployment process.
diStartTime :: Lens' DeploymentInfo (Maybe UTCTime)
diStartTime = lens _diStartTime (\s a -> s { _diStartTime = a }) . mapping _Time

-- | The current state of the deployment as a whole.
diStatus :: Lens' DeploymentInfo (Maybe DeploymentStatus)
diStatus = lens _diStatus (\s a -> s { _diStatus = a })

instance FromJSON DeploymentInfo where
    parseJSON = withObject "DeploymentInfo" $ \o -> DeploymentInfo
        <$> o .:? "applicationName"
        <*> o .:? "completeTime"
        <*> o .:? "createTime"
        <*> o .:? "creator"
        <*> o .:? "deploymentConfigName"
        <*> o .:? "deploymentGroupName"
        <*> o .:? "deploymentId"
        <*> o .:? "deploymentOverview"
        <*> o .:? "description"
        <*> o .:? "errorInformation"
        <*> o .:? "ignoreApplicationStopFailures"
        <*> o .:? "revision"
        <*> o .:? "startTime"
        <*> o .:? "status"

instance ToJSON DeploymentInfo where
    toJSON DeploymentInfo{..} = object
        [ "applicationName"               .= _diApplicationName
        , "deploymentGroupName"           .= _diDeploymentGroupName
        , "deploymentConfigName"          .= _diDeploymentConfigName
        , "deploymentId"                  .= _diDeploymentId
        , "revision"                      .= _diRevision
        , "status"                        .= _diStatus
        , "errorInformation"              .= _diErrorInformation
        , "createTime"                    .= _diCreateTime
        , "startTime"                     .= _diStartTime
        , "completeTime"                  .= _diCompleteTime
        , "deploymentOverview"            .= _diDeploymentOverview
        , "description"                   .= _diDescription
        , "creator"                       .= _diCreator
        , "ignoreApplicationStopFailures" .= _diIgnoreApplicationStopFailures
        ]

data LifecycleEvent = LifecycleEvent
    { _leDiagnostics        :: Maybe Diagnostics
    , _leEndTime            :: Maybe POSIX
    , _leLifecycleEventName :: Maybe Text
    , _leStartTime          :: Maybe POSIX
    , _leStatus             :: Maybe LifecycleEventStatus
    } deriving (Eq, Read, Show)

-- | 'LifecycleEvent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'leDiagnostics' @::@ 'Maybe' 'Diagnostics'
--
-- * 'leEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'leLifecycleEventName' @::@ 'Maybe' 'Text'
--
-- * 'leStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'leStatus' @::@ 'Maybe' 'LifecycleEventStatus'
--
lifecycleEvent :: LifecycleEvent
lifecycleEvent = LifecycleEvent
    { _leLifecycleEventName = Nothing
    , _leDiagnostics        = Nothing
    , _leStartTime          = Nothing
    , _leEndTime            = Nothing
    , _leStatus             = Nothing
    }

-- | Diagnostic information about the deployment lifecycle event.
leDiagnostics :: Lens' LifecycleEvent (Maybe Diagnostics)
leDiagnostics = lens _leDiagnostics (\s a -> s { _leDiagnostics = a })

-- | A timestamp indicating when the deployment lifecycle event ended.
leEndTime :: Lens' LifecycleEvent (Maybe UTCTime)
leEndTime = lens _leEndTime (\s a -> s { _leEndTime = a }) . mapping _Time

-- | The deployment lifecycle event name, such as ApplicationStop, BeforeInstall,
-- AfterInstall, ApplicationStart, or ValidateService.
leLifecycleEventName :: Lens' LifecycleEvent (Maybe Text)
leLifecycleEventName =
    lens _leLifecycleEventName (\s a -> s { _leLifecycleEventName = a })

-- | A timestamp indicating when the deployment lifecycle event started.
leStartTime :: Lens' LifecycleEvent (Maybe UTCTime)
leStartTime = lens _leStartTime (\s a -> s { _leStartTime = a }) . mapping _Time

-- | The deployment lifecycle event status:
--
-- Pending: The deployment lifecycle event is pending. InProgress: The
-- deployment lifecycle event is in progress. Succeeded: The deployment
-- lifecycle event has succeeded. Failed: The deployment lifecycle event has
-- failed. Skipped: The deployment lifecycle event has been skipped. Unknown:
-- The deployment lifecycle event is unknown.
leStatus :: Lens' LifecycleEvent (Maybe LifecycleEventStatus)
leStatus = lens _leStatus (\s a -> s { _leStatus = a })

instance FromJSON LifecycleEvent where
    parseJSON = withObject "LifecycleEvent" $ \o -> LifecycleEvent
        <$> o .:? "diagnostics"
        <*> o .:? "endTime"
        <*> o .:? "lifecycleEventName"
        <*> o .:? "startTime"
        <*> o .:? "status"

instance ToJSON LifecycleEvent where
    toJSON LifecycleEvent{..} = object
        [ "lifecycleEventName" .= _leLifecycleEventName
        , "diagnostics"        .= _leDiagnostics
        , "startTime"          .= _leStartTime
        , "endTime"            .= _leEndTime
        , "status"             .= _leStatus
        ]

data DeploymentOverview = DeploymentOverview
    { _doFailed     :: Maybe Integer
    , _doInProgress :: Maybe Integer
    , _doPending    :: Maybe Integer
    , _doSkipped    :: Maybe Integer
    , _doSucceeded  :: Maybe Integer
    } deriving (Eq, Ord, Read, Show)

-- | 'DeploymentOverview' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doFailed' @::@ 'Maybe' 'Integer'
--
-- * 'doInProgress' @::@ 'Maybe' 'Integer'
--
-- * 'doPending' @::@ 'Maybe' 'Integer'
--
-- * 'doSkipped' @::@ 'Maybe' 'Integer'
--
-- * 'doSucceeded' @::@ 'Maybe' 'Integer'
--
deploymentOverview :: DeploymentOverview
deploymentOverview = DeploymentOverview
    { _doPending    = Nothing
    , _doInProgress = Nothing
    , _doSucceeded  = Nothing
    , _doFailed     = Nothing
    , _doSkipped    = Nothing
    }

-- | The number of instances that have failed in the deployment.
doFailed :: Lens' DeploymentOverview (Maybe Integer)
doFailed = lens _doFailed (\s a -> s { _doFailed = a })

-- | The number of instances that are in progress in the deployment.
doInProgress :: Lens' DeploymentOverview (Maybe Integer)
doInProgress = lens _doInProgress (\s a -> s { _doInProgress = a })

-- | The number of instances that are pending in the deployment.
doPending :: Lens' DeploymentOverview (Maybe Integer)
doPending = lens _doPending (\s a -> s { _doPending = a })

-- | The number of instances that have been skipped in the deployment.
doSkipped :: Lens' DeploymentOverview (Maybe Integer)
doSkipped = lens _doSkipped (\s a -> s { _doSkipped = a })

-- | The number of instances that have succeeded in the deployment.
doSucceeded :: Lens' DeploymentOverview (Maybe Integer)
doSucceeded = lens _doSucceeded (\s a -> s { _doSucceeded = a })

instance FromJSON DeploymentOverview where
    parseJSON = withObject "DeploymentOverview" $ \o -> DeploymentOverview
        <$> o .:? "Failed"
        <*> o .:? "InProgress"
        <*> o .:? "Pending"
        <*> o .:? "Skipped"
        <*> o .:? "Succeeded"

instance ToJSON DeploymentOverview where
    toJSON DeploymentOverview{..} = object
        [ "Pending"    .= _doPending
        , "InProgress" .= _doInProgress
        , "Succeeded"  .= _doSucceeded
        , "Failed"     .= _doFailed
        , "Skipped"    .= _doSkipped
        ]

data ErrorCode
    = ApplicationMissing       -- ^ APPLICATION_MISSING
    | DeploymentGroupMissing   -- ^ DEPLOYMENT_GROUP_MISSING
    | HealthConstraints        -- ^ HEALTH_CONSTRAINTS
    | HealthConstraintsInvalid -- ^ HEALTH_CONSTRAINTS_INVALID
    | IamRoleMissing           -- ^ IAM_ROLE_MISSING
    | IamRolePermissions       -- ^ IAM_ROLE_PERMISSIONS
    | InternalError            -- ^ INTERNAL_ERROR
    | NoInstances              -- ^ NO_INSTANCES
    | OverMaxInstances         -- ^ OVER_MAX_INSTANCES
    | RevisionMissing          -- ^ REVISION_MISSING
    | Timeout                  -- ^ TIMEOUT
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ErrorCode

instance FromText ErrorCode where
    parser = takeLowerText >>= \case
        "application_missing"        -> pure ApplicationMissing
        "deployment_group_missing"   -> pure DeploymentGroupMissing
        "health_constraints"         -> pure HealthConstraints
        "health_constraints_invalid" -> pure HealthConstraintsInvalid
        "iam_role_missing"           -> pure IamRoleMissing
        "iam_role_permissions"       -> pure IamRolePermissions
        "internal_error"             -> pure InternalError
        "no_instances"               -> pure NoInstances
        "over_max_instances"         -> pure OverMaxInstances
        "revision_missing"           -> pure RevisionMissing
        "timeout"                    -> pure Timeout
        e                            -> fail $
            "Failure parsing ErrorCode from " ++ show e

instance ToText ErrorCode where
    toText = \case
        ApplicationMissing       -> "APPLICATION_MISSING"
        DeploymentGroupMissing   -> "DEPLOYMENT_GROUP_MISSING"
        HealthConstraints        -> "HEALTH_CONSTRAINTS"
        HealthConstraintsInvalid -> "HEALTH_CONSTRAINTS_INVALID"
        IamRoleMissing           -> "IAM_ROLE_MISSING"
        IamRolePermissions       -> "IAM_ROLE_PERMISSIONS"
        InternalError            -> "INTERNAL_ERROR"
        NoInstances              -> "NO_INSTANCES"
        OverMaxInstances         -> "OVER_MAX_INSTANCES"
        RevisionMissing          -> "REVISION_MISSING"
        Timeout                  -> "TIMEOUT"

instance ToByteString ErrorCode
instance ToHeader     ErrorCode
instance ToQuery      ErrorCode

instance FromJSON ErrorCode where
    parseJSON = parseJSONText "ErrorCode"

instance ToJSON ErrorCode where
    toJSON = toJSONText

data DeploymentConfigInfo = DeploymentConfigInfo
    { _dciCreateTime           :: Maybe POSIX
    , _dciDeploymentConfigId   :: Maybe Text
    , _dciDeploymentConfigName :: Maybe Text
    , _dciMinimumHealthyHosts  :: Maybe MinimumHealthyHosts
    } deriving (Eq, Read, Show)

-- | 'DeploymentConfigInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dciCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dciDeploymentConfigId' @::@ 'Maybe' 'Text'
--
-- * 'dciDeploymentConfigName' @::@ 'Maybe' 'Text'
--
-- * 'dciMinimumHealthyHosts' @::@ 'Maybe' 'MinimumHealthyHosts'
--
deploymentConfigInfo :: DeploymentConfigInfo
deploymentConfigInfo = DeploymentConfigInfo
    { _dciDeploymentConfigId   = Nothing
    , _dciDeploymentConfigName = Nothing
    , _dciMinimumHealthyHosts  = Nothing
    , _dciCreateTime           = Nothing
    }

-- | The time that the deployment configuration was created.
dciCreateTime :: Lens' DeploymentConfigInfo (Maybe UTCTime)
dciCreateTime = lens _dciCreateTime (\s a -> s { _dciCreateTime = a }) . mapping _Time

-- | The deployment configuration ID.
dciDeploymentConfigId :: Lens' DeploymentConfigInfo (Maybe Text)
dciDeploymentConfigId =
    lens _dciDeploymentConfigId (\s a -> s { _dciDeploymentConfigId = a })

-- | The deployment configuration name.
dciDeploymentConfigName :: Lens' DeploymentConfigInfo (Maybe Text)
dciDeploymentConfigName =
    lens _dciDeploymentConfigName (\s a -> s { _dciDeploymentConfigName = a })

-- | Information about the number or percentage of minimum healthy instances.
dciMinimumHealthyHosts :: Lens' DeploymentConfigInfo (Maybe MinimumHealthyHosts)
dciMinimumHealthyHosts =
    lens _dciMinimumHealthyHosts (\s a -> s { _dciMinimumHealthyHosts = a })

instance FromJSON DeploymentConfigInfo where
    parseJSON = withObject "DeploymentConfigInfo" $ \o -> DeploymentConfigInfo
        <$> o .:? "createTime"
        <*> o .:? "deploymentConfigId"
        <*> o .:? "deploymentConfigName"
        <*> o .:? "minimumHealthyHosts"

instance ToJSON DeploymentConfigInfo where
    toJSON DeploymentConfigInfo{..} = object
        [ "deploymentConfigId"   .= _dciDeploymentConfigId
        , "deploymentConfigName" .= _dciDeploymentConfigName
        , "minimumHealthyHosts"  .= _dciMinimumHealthyHosts
        , "createTime"           .= _dciCreateTime
        ]

data InstanceStatus
    = ISFailed     -- ^ Failed
    | ISInProgress -- ^ InProgress
    | ISPending    -- ^ Pending
    | ISSkipped    -- ^ Skipped
    | ISSucceeded  -- ^ Succeeded
    | ISUnknown    -- ^ Unknown
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable InstanceStatus

instance FromText InstanceStatus where
    parser = takeLowerText >>= \case
        "failed"     -> pure ISFailed
        "inprogress" -> pure ISInProgress
        "pending"    -> pure ISPending
        "skipped"    -> pure ISSkipped
        "succeeded"  -> pure ISSucceeded
        "unknown"    -> pure ISUnknown
        e            -> fail $
            "Failure parsing InstanceStatus from " ++ show e

instance ToText InstanceStatus where
    toText = \case
        ISFailed     -> "Failed"
        ISInProgress -> "InProgress"
        ISPending    -> "Pending"
        ISSkipped    -> "Skipped"
        ISSucceeded  -> "Succeeded"
        ISUnknown    -> "Unknown"

instance ToByteString InstanceStatus
instance ToHeader     InstanceStatus
instance ToQuery      InstanceStatus

instance FromJSON InstanceStatus where
    parseJSON = parseJSONText "InstanceStatus"

instance ToJSON InstanceStatus where
    toJSON = toJSONText

data DeploymentStatus
    = DSCreated    -- ^ Created
    | DSFailed     -- ^ Failed
    | DSInProgress -- ^ InProgress
    | DSQueued     -- ^ Queued
    | DSStopped    -- ^ Stopped
    | DSSucceeded  -- ^ Succeeded
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DeploymentStatus

instance FromText DeploymentStatus where
    parser = takeLowerText >>= \case
        "created"    -> pure DSCreated
        "failed"     -> pure DSFailed
        "inprogress" -> pure DSInProgress
        "queued"     -> pure DSQueued
        "stopped"    -> pure DSStopped
        "succeeded"  -> pure DSSucceeded
        e            -> fail $
            "Failure parsing DeploymentStatus from " ++ show e

instance ToText DeploymentStatus where
    toText = \case
        DSCreated    -> "Created"
        DSFailed     -> "Failed"
        DSInProgress -> "InProgress"
        DSQueued     -> "Queued"
        DSStopped    -> "Stopped"
        DSSucceeded  -> "Succeeded"

instance ToByteString DeploymentStatus
instance ToHeader     DeploymentStatus
instance ToQuery      DeploymentStatus

instance FromJSON DeploymentStatus where
    parseJSON = parseJSONText "DeploymentStatus"

instance ToJSON DeploymentStatus where
    toJSON = toJSONText

data S3Location = S3Location
    { _slBucket     :: Maybe Text
    , _slBundleType :: Maybe BundleType
    , _slETag       :: Maybe Text
    , _slKey        :: Maybe Text
    , _slVersion    :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'S3Location' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slBucket' @::@ 'Maybe' 'Text'
--
-- * 'slBundleType' @::@ 'Maybe' 'BundleType'
--
-- * 'slETag' @::@ 'Maybe' 'Text'
--
-- * 'slKey' @::@ 'Maybe' 'Text'
--
-- * 'slVersion' @::@ 'Maybe' 'Text'
--
s3Location :: S3Location
s3Location = S3Location
    { _slBucket     = Nothing
    , _slKey        = Nothing
    , _slBundleType = Nothing
    , _slVersion    = Nothing
    , _slETag       = Nothing
    }

-- | The name of the Amazon S3 bucket where the application revision is stored.
slBucket :: Lens' S3Location (Maybe Text)
slBucket = lens _slBucket (\s a -> s { _slBucket = a })

-- | The file type of the application revision. Must be one of the following:
--
-- tar: A tar archive file. tgz: A compressed tar archive file. zip: A zip
-- archive file.
slBundleType :: Lens' S3Location (Maybe BundleType)
slBundleType = lens _slBundleType (\s a -> s { _slBundleType = a })

-- | The ETag of the Amazon S3 object that represents the bundled artifacts for
-- the application revision.
--
-- If the ETag is not specified as an input parameter, ETag validation of the
-- object will be skipped.
slETag :: Lens' S3Location (Maybe Text)
slETag = lens _slETag (\s a -> s { _slETag = a })

-- | The name of the Amazon S3 object that represents the bundled artifacts for
-- the application revision.
slKey :: Lens' S3Location (Maybe Text)
slKey = lens _slKey (\s a -> s { _slKey = a })

-- | A specific version of the Amazon S3 object that represents the bundled
-- artifacts for the application revision.
--
-- If the version is not specified, the system will use the most recent version
-- by default.
slVersion :: Lens' S3Location (Maybe Text)
slVersion = lens _slVersion (\s a -> s { _slVersion = a })

instance FromJSON S3Location where
    parseJSON = withObject "S3Location" $ \o -> S3Location
        <$> o .:? "bucket"
        <*> o .:? "bundleType"
        <*> o .:? "eTag"
        <*> o .:? "key"
        <*> o .:? "version"

instance ToJSON S3Location where
    toJSON S3Location{..} = object
        [ "bucket"     .= _slBucket
        , "key"        .= _slKey
        , "bundleType" .= _slBundleType
        , "version"    .= _slVersion
        , "eTag"       .= _slETag
        ]

data MinimumHealthyHostsType
    = FleetPercent -- ^ FLEET_PERCENT
    | HostCount    -- ^ HOST_COUNT
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable MinimumHealthyHostsType

instance FromText MinimumHealthyHostsType where
    parser = takeLowerText >>= \case
        "fleet_percent" -> pure FleetPercent
        "host_count"    -> pure HostCount
        e               -> fail $
            "Failure parsing MinimumHealthyHostsType from " ++ show e

instance ToText MinimumHealthyHostsType where
    toText = \case
        FleetPercent -> "FLEET_PERCENT"
        HostCount    -> "HOST_COUNT"

instance ToByteString MinimumHealthyHostsType
instance ToHeader     MinimumHealthyHostsType
instance ToQuery      MinimumHealthyHostsType

instance FromJSON MinimumHealthyHostsType where
    parseJSON = parseJSONText "MinimumHealthyHostsType"

instance ToJSON MinimumHealthyHostsType where
    toJSON = toJSONText

data GitHubLocation = GitHubLocation
    { _ghlCommitId   :: Maybe Text
    , _ghlRepository :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'GitHubLocation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghlCommitId' @::@ 'Maybe' 'Text'
--
-- * 'ghlRepository' @::@ 'Maybe' 'Text'
--
gitHubLocation :: GitHubLocation
gitHubLocation = GitHubLocation
    { _ghlRepository = Nothing
    , _ghlCommitId   = Nothing
    }

-- | The SHA1 commit ID of the GitHub commit that references the that represents
-- the bundled artifacts for the application revision.
ghlCommitId :: Lens' GitHubLocation (Maybe Text)
ghlCommitId = lens _ghlCommitId (\s a -> s { _ghlCommitId = a })

-- | The GitHub account and repository pair that stores a reference to the commit
-- that represents the bundled artifacts for the application revision.
--
-- Specified as account/repository.
ghlRepository :: Lens' GitHubLocation (Maybe Text)
ghlRepository = lens _ghlRepository (\s a -> s { _ghlRepository = a })

instance FromJSON GitHubLocation where
    parseJSON = withObject "GitHubLocation" $ \o -> GitHubLocation
        <$> o .:? "commitId"
        <*> o .:? "repository"

instance ToJSON GitHubLocation where
    toJSON GitHubLocation{..} = object
        [ "repository" .= _ghlRepository
        , "commitId"   .= _ghlCommitId
        ]

data RevisionLocationType
    = GitHub -- ^ GitHub
    | S3     -- ^ S3
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable RevisionLocationType

instance FromText RevisionLocationType where
    parser = takeLowerText >>= \case
        "github" -> pure GitHub
        "s3"     -> pure S3
        e        -> fail $
            "Failure parsing RevisionLocationType from " ++ show e

instance ToText RevisionLocationType where
    toText = \case
        GitHub -> "GitHub"
        S3     -> "S3"

instance ToByteString RevisionLocationType
instance ToHeader     RevisionLocationType
instance ToQuery      RevisionLocationType

instance FromJSON RevisionLocationType where
    parseJSON = parseJSONText "RevisionLocationType"

instance ToJSON RevisionLocationType where
    toJSON = toJSONText

data EC2TagFilterType
    = KeyAndValue -- ^ KEY_AND_VALUE
    | KeyOnly     -- ^ KEY_ONLY
    | ValueOnly   -- ^ VALUE_ONLY
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable EC2TagFilterType

instance FromText EC2TagFilterType where
    parser = takeLowerText >>= \case
        "key_and_value" -> pure KeyAndValue
        "key_only"      -> pure KeyOnly
        "value_only"    -> pure ValueOnly
        e               -> fail $
            "Failure parsing EC2TagFilterType from " ++ show e

instance ToText EC2TagFilterType where
    toText = \case
        KeyAndValue -> "KEY_AND_VALUE"
        KeyOnly     -> "KEY_ONLY"
        ValueOnly   -> "VALUE_ONLY"

instance ToByteString EC2TagFilterType
instance ToHeader     EC2TagFilterType
instance ToQuery      EC2TagFilterType

instance FromJSON EC2TagFilterType where
    parseJSON = parseJSONText "EC2TagFilterType"

instance ToJSON EC2TagFilterType where
    toJSON = toJSONText
