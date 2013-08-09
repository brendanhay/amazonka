{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Network.AWS.AutoScaling.V20110101.Responses
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.AutoScaling.V20110101.Responses where

import Data.Aeson.TH
import Data.Aeson.XML
import Data.Text
import Data.Time
import Network.AWS.AutoScaling.V20110101.Types
import Network.AWS.Internal

data MetadataResponse = MetadataResponse
    { rmrRequestId :: !Text
    } deriving (Show)

$(deriveJSON fieldOptions ''MetadataResponse)

instance FromXML MetadataResponse where
    fromXML = stringToXML 2

data ErrorResponse = ErrorResponse
    { erError :: ![Error]
    , erRequestId :: !Text
    } deriving (Show)

data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { dasgrDescribeAutoScalingGroupsResult :: !DescribeAutoScalingGroupsResult
    , dasgrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingGroupsResult = DescribeAutoScalingGroupsResult
    { dasgrAutoScalingGroups :: !AutoScalingGroups
    , dasgrNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribePoliciesResponse = DescribePoliciesResponse
    { dprDescribePoliciesResult :: !DescribePoliciesResult
    , dprResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribePoliciesResult = DescribePoliciesResult
    { dprScalingPolicies :: !(Maybe ScalingPolicies)
    , dprNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse
    { dsptrDescribeScalingProcessTypesResult :: !DescribeScalingProcessTypesResult
    , dsptrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScalingProcessTypesResult = DescribeScalingProcessTypesResult
    { dsptrProcesses :: !(Maybe Processes)
    } deriving (Show)

data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse
    { dsarDescribeScalingActivitiesResult :: !DescribeScalingActivitiesResult
    , dsarResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScalingActivitiesResult = DescribeScalingActivitiesResult
    { dsarActivities :: !Activities
    , dsarNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { dncrDescribeNotificationConfigurationsResult :: !DescribeNotificationConfigurationsResult
    , dncrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeNotificationConfigurationsResult = DescribeNotificationConfigurationsResult
    { dncrNotificationConfigurations :: !NotificationConfigurations
    , dncrNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
    { dtptrDescribeTerminationPolicyTypesResult :: !DescribeTerminationPolicyTypesResult
    , dtptrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeTerminationPolicyTypesResult = DescribeTerminationPolicyTypesResult
    { dtptrTerminationPolicyTypes :: !(Maybe TerminationPolicies)
    } deriving (Show)

data DescribeTagsResponse = DescribeTagsResponse
    { dtrDescribeTagsResult :: !DescribeTagsResult
    , dtrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeTagsResult = DescribeTagsResult
    { dtrTags :: !(Maybe TagDescriptionList)
    , dtrNextToken :: !(Maybe Text)
    } deriving (Show)

data PutScalingPolicyResponse = PutScalingPolicyResponse
    { psprPutScalingPolicyResult :: !PutScalingPolicyResult
    , psprResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data PutScalingPolicyResult = PutScalingPolicyResult
    { psprPolicyARN :: !(Maybe ResourceName)
    } deriving (Show)

data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { dasntrDescribeAutoScalingNotificationTypesResult :: !DescribeAutoScalingNotificationTypesResult
    , dasntrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingNotificationTypesResult = DescribeAutoScalingNotificationTypesResult
    { dasntrAutoScalingNotificationTypes :: !(Maybe AutoScalingNotificationTypes)
    } deriving (Show)

data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { dasirDescribeAutoScalingInstancesResult :: !DescribeAutoScalingInstancesResult
    , dasirResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingInstancesResult = DescribeAutoScalingInstancesResult
    { dasirAutoScalingInstances :: !(Maybe AutoScalingInstances)
    , dasirNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
    { dlcrDescribeLaunchConfigurationsResult :: !DescribeLaunchConfigurationsResult
    , dlcrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeLaunchConfigurationsResult = DescribeLaunchConfigurationsResult
    { dlcrLaunchConfigurations :: !LaunchConfigurations
    , dlcrNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
    { datrDescribeAdjustmentTypesResult :: !DescribeAdjustmentTypesResult
    , datrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAdjustmentTypesResult = DescribeAdjustmentTypesResult
    { datrAdjustmentTypes :: !(Maybe AdjustmentTypes)
    } deriving (Show)

data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { dsarrDescribeScheduledActionsResult :: !DescribeScheduledActionsResult
    , dsarrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScheduledActionsResult = DescribeScheduledActionsResult
    { dsarrScheduledUpdateGroupActions :: !(Maybe ScheduledUpdateGroupActions)
    , dsarrNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { dmctrDescribeMetricCollectionTypesResult :: !DescribeMetricCollectionTypesResult
    , dmctrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeMetricCollectionTypesResult = DescribeMetricCollectionTypesResult
    { dmctrMetrics :: !(Maybe MetricCollectionTypes)
    , dmctrGranularities :: !(Maybe MetricGranularityTypes)
    } deriving (Show)

data TerminateInstanceInAutoScalingGroupResult = TerminateInstanceInAutoScalingGroupResult
    { tiiasgrActivity :: !(Maybe Activity)
    } deriving (Show)

$(deriveXML ''TerminateInstanceInAutoScalingGroupResult)

data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
    { tiiasgrTerminateInstanceInAutoScalingGroupResult :: !TerminateInstanceInAutoScalingGroupResult
    , tiiasgrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''TerminateInstanceInAutoScalingGroupResponse)

