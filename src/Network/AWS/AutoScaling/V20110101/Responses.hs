{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

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

import Data.ByteString                         (ByteString)
import Network.AWS.AutoScaling.V20110101.Types
import Network.AWS.Internal
import GHC.Generics

data MetadataResponse = MetadataResponse
    { rmrRequestId :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML MetadataResponse

-- data ErrorResponse = ErrorResponse
--     { erError :: ![Error]
--     , erRequestId :: !ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsXML ErrorResponse

-- data DescribeAutoScalingGroupsResult = DescribeAutoScalingGroupsResult
--     { dasgrAutoScalingGroups :: !AutoScalingGroups
--     , dasgrNextToken :: !(Maybe ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeAutoScalingGroupsResult

-- data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
--     { dasgrDescribeAutoScalingGroupsResult :: !DescribeAutoScalingGroupsResult
--     , dasgrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeAutoScalingGroupsResponse

-- data DescribePoliciesResult = DescribePoliciesResult
--     { dprScalingPolicies :: !(Maybe ScalingPolicies)
--     , dprNextToken :: !(Maybe ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribePoliciesResult

-- data DescribePoliciesResponse = DescribePoliciesResponse
--     { dprDescribePoliciesResult :: !DescribePoliciesResult
--     , dprResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribePoliciesResponse

-- data DescribeScalingProcessTypesResult = DescribeScalingProcessTypesResult
--     { dsptrProcesses :: !(Maybe Processes)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeScalingProcessTypesResult

-- data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse
--     { dsptrDescribeScalingProcessTypesResult :: !DescribeScalingProcessTypesResult
--     , dsptrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeScalingProcessTypesResponse

-- data DescribeScalingActivitiesResult = DescribeScalingActivitiesResult
--     { dsarActivities :: !Activities
--     , dsarNextToken :: !(Maybe ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeScalingActivitiesResult

-- data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse
--     { dsarDescribeScalingActivitiesResult :: !DescribeScalingActivitiesResult
--     , dsarResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeScalingActivitiesResponse

-- data DescribeNotificationConfigurationsResult = DescribeNotificationConfigurationsResult
--     { dncrNotificationConfigurations :: !NotificationConfigurations
--     , dncrNextToken :: !(Maybe ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeNotificationConfigurationsResult

-- data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
--     { dncrDescribeNotificationConfigurationsResult :: !DescribeNotificationConfigurationsResult
--     , dncrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeNotificationConfigurationsResponse

-- data DescribeTerminationPolicyTypesResult = DescribeTerminationPolicyTypesResult
--     { dtptrTerminationPolicyTypes :: !(Maybe TerminationPolicies)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeTerminationPolicyTypesResult

-- data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
--     { dtptrDescribeTerminationPolicyTypesResult :: !DescribeTerminationPolicyTypesResult
--     , dtptrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeTerminationPolicyTypesResponse

-- data DescribeTagsResult = DescribeTagsResult
--     { dtrTags :: !(Maybe TagDescriptionList)
--     , dtrNextToken :: !(Maybe ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeTagsResult

-- data DescribeTagsResponse = DescribeTagsResponse
--     { dtrDescribeTagsResult :: !DescribeTagsResult
--     , dtrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeTagsResponse

-- data PutScalingPolicyResult = PutScalingPolicyResult
--     { psprPolicyARN :: !(Maybe ResourceName)
--     } deriving (Eq, Show, Generic)

-- instance IsXML PutScalingPolicyResult

-- data PutScalingPolicyResponse = PutScalingPolicyResponse
--     { psprPutScalingPolicyResult :: !PutScalingPolicyResult
--     , psprResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML PutScalingPolicyResponse

-- data DescribeAutoScalingNotificationTypesResult = DescribeAutoScalingNotificationTypesResult
--     { dasntrAutoScalingNotificationTypes :: !(Maybe AutoScalingNotificationTypes)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeAutoScalingNotificationTypesResult

-- data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
--     { dasntrDescribeAutoScalingNotificationTypesResult :: !DescribeAutoScalingNotificationTypesResult
--     , dasntrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeAutoScalingNotificationTypesResponse

-- data DescribeAutoScalingInstancesResult = DescribeAutoScalingInstancesResult
--     { dasirAutoScalingInstances :: !(Maybe AutoScalingInstances)
--     , dasirNextToken :: !(Maybe ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeAutoScalingInstancesResult

-- data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
--     { dasirDescribeAutoScalingInstancesResult :: !DescribeAutoScalingInstancesResult
--     , dasirResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeAutoScalingInstancesResponse

-- data DescribeLaunchConfigurationsResult = DescribeLaunchConfigurationsResult
--     { dlcrLaunchConfigurations :: !LaunchConfigurations
--     , dlcrNextToken :: !(Maybe ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeLaunchConfigurationsResult

-- data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
--     { dlcrDescribeLaunchConfigurationsResult :: !DescribeLaunchConfigurationsResult
--     , dlcrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeLaunchConfigurationsResponse

-- data DescribeAdjustmentTypesResult = DescribeAdjustmentTypesResult
--     { datrAdjustmentTypes :: !(Maybe AdjustmentTypes)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeAdjustmentTypesResult

-- data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
--     { datrDescribeAdjustmentTypesResult :: !DescribeAdjustmentTypesResult
--     , datrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeAdjustmentTypesResponse

-- data DescribeScheduledActionsResult = DescribeScheduledActionsResult
--     { dsarrScheduledUpdateGroupActions :: !(Maybe ScheduledUpdateGroupActions)
--     , dsarrNextToken :: !(Maybe ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeScheduledActionsResult

-- data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
--     { dsarrDescribeScheduledActionsResult :: !DescribeScheduledActionsResult
--     , dsarrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeScheduledActionsResponse

-- data DescribeMetricCollectionTypesResult = DescribeMetricCollectionTypesResult
--     { dmctrMetrics :: !(Maybe MetricCollectionTypes)
--     , dmctrGranularities :: !(Maybe MetricGranularityTypes)
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeMetricCollectionTypesResult

-- data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
--     { dmctrDescribeMetricCollectionTypesResult :: !DescribeMetricCollectionTypesResult
--     , dmctrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML DescribeMetricCollectionTypesResponse

-- data TerminateInstanceInAutoScalingGroupResult = TerminateInstanceInAutoScalingGroupResult
--     { tiiasgrActivity :: !(Maybe Activity)
--     } deriving (Eq, Show, Generic)

-- instance IsXML TerminateInstanceInAutoScalingGroupResult

-- data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
--     { tiiasgrTerminateInstanceInAutoScalingGroupResult :: !TerminateInstanceInAutoScalingGroupResult
--     , tiiasgrResponseMetadata :: !ResponseMetadata
--     } deriving (Eq, Show, Generic)

-- instance IsXML TerminateInstanceInAutoScalingGroupResponse
