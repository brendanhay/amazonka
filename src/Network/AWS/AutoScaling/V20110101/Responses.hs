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

$(deriveXML ''ErrorResponse)

data DescribeAutoScalingGroupsResult = DescribeAutoScalingGroupsResult
    { dasgrAutoScalingGroups :: !AutoScalingGroups
    , dasgrNextToken :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeAutoScalingGroupsResult)

data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { dasgrDescribeAutoScalingGroupsResult :: !DescribeAutoScalingGroupsResult
    , dasgrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeAutoScalingGroupsResponse)

data DescribePoliciesResult = DescribePoliciesResult
    { dprScalingPolicies :: !(Maybe ScalingPolicies)
    , dprNextToken :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribePoliciesResult)

data DescribePoliciesResponse = DescribePoliciesResponse
    { dprDescribePoliciesResult :: !DescribePoliciesResult
    , dprResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribePoliciesResponse)

data DescribeScalingProcessTypesResult = DescribeScalingProcessTypesResult
    { dsptrProcesses :: !(Maybe Processes)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeScalingProcessTypesResult)

data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse
    { dsptrDescribeScalingProcessTypesResult :: !DescribeScalingProcessTypesResult
    , dsptrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeScalingProcessTypesResponse)

data DescribeScalingActivitiesResult = DescribeScalingActivitiesResult
    { dsarActivities :: !Activities
    , dsarNextToken :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeScalingActivitiesResult)

data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse
    { dsarDescribeScalingActivitiesResult :: !DescribeScalingActivitiesResult
    , dsarResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeScalingActivitiesResponse)

data DescribeNotificationConfigurationsResult = DescribeNotificationConfigurationsResult
    { dncrNotificationConfigurations :: !NotificationConfigurations
    , dncrNextToken :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeNotificationConfigurationsResult)

data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { dncrDescribeNotificationConfigurationsResult :: !DescribeNotificationConfigurationsResult
    , dncrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeNotificationConfigurationsResponse)

data DescribeTerminationPolicyTypesResult = DescribeTerminationPolicyTypesResult
    { dtptrTerminationPolicyTypes :: !(Maybe TerminationPolicies)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeTerminationPolicyTypesResult)

data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
    { dtptrDescribeTerminationPolicyTypesResult :: !DescribeTerminationPolicyTypesResult
    , dtptrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeTerminationPolicyTypesResponse)

data DescribeTagsResult = DescribeTagsResult
    { dtrTags :: !(Maybe TagDescriptionList)
    , dtrNextToken :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeTagsResult)

data DescribeTagsResponse = DescribeTagsResponse
    { dtrDescribeTagsResult :: !DescribeTagsResult
    , dtrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeTagsResponse)

data PutScalingPolicyResult = PutScalingPolicyResult
    { psprPolicyARN :: !(Maybe ResourceName)
    } deriving (Show)

$(deriveJSON fieldOptions ''PutScalingPolicyResult)

data PutScalingPolicyResponse = PutScalingPolicyResponse
    { psprPutScalingPolicyResult :: !PutScalingPolicyResult
    , psprResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''PutScalingPolicyResponse)

data DescribeAutoScalingNotificationTypesResult = DescribeAutoScalingNotificationTypesResult
    { dasntrAutoScalingNotificationTypes :: !(Maybe AutoScalingNotificationTypes)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeAutoScalingNotificationTypesResult)

data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { dasntrDescribeAutoScalingNotificationTypesResult :: !DescribeAutoScalingNotificationTypesResult
    , dasntrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeAutoScalingNotificationTypesResponse)

data DescribeAutoScalingInstancesResult = DescribeAutoScalingInstancesResult
    { dasirAutoScalingInstances :: !(Maybe AutoScalingInstances)
    , dasirNextToken :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeAutoScalingInstancesResult)

data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { dasirDescribeAutoScalingInstancesResult :: !DescribeAutoScalingInstancesResult
    , dasirResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeAutoScalingInstancesResponse)

data DescribeLaunchConfigurationsResult = DescribeLaunchConfigurationsResult
    { dlcrLaunchConfigurations :: !LaunchConfigurations
    , dlcrNextToken :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeLaunchConfigurationsResult)

data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
    { dlcrDescribeLaunchConfigurationsResult :: !DescribeLaunchConfigurationsResult
    , dlcrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeLaunchConfigurationsResponse)

data DescribeAdjustmentTypesResult = DescribeAdjustmentTypesResult
    { datrAdjustmentTypes :: !(Maybe AdjustmentTypes)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeAdjustmentTypesResult)

data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
    { datrDescribeAdjustmentTypesResult :: !DescribeAdjustmentTypesResult
    , datrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeAdjustmentTypesResponse)

data DescribeScheduledActionsResult = DescribeScheduledActionsResult
    { dsarrScheduledUpdateGroupActions :: !(Maybe ScheduledUpdateGroupActions)
    , dsarrNextToken :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeScheduledActionsResult)

data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { dsarrDescribeScheduledActionsResult :: !DescribeScheduledActionsResult
    , dsarrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeScheduledActionsResponse)

data DescribeMetricCollectionTypesResult = DescribeMetricCollectionTypesResult
    { dmctrMetrics :: !(Maybe MetricCollectionTypes)
    , dmctrGranularities :: !(Maybe MetricGranularityTypes)
    } deriving (Show)

$(deriveJSON fieldOptions ''DescribeMetricCollectionTypesResult)

data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { dmctrDescribeMetricCollectionTypesResult :: !DescribeMetricCollectionTypesResult
    , dmctrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''DescribeMetricCollectionTypesResponse)

data TerminateInstanceInAutoScalingGroupResult = TerminateInstanceInAutoScalingGroupResult
    { tiiasgrActivity :: !(Maybe Activity)
    } deriving (Show)

$(deriveJSON fieldOptions ''TerminateInstanceInAutoScalingGroupResult)

data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
    { tiiasgrTerminateInstanceInAutoScalingGroupResult :: !TerminateInstanceInAutoScalingGroupResult
    , tiiasgrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

$(deriveXML ''TerminateInstanceInAutoScalingGroupResponse)
