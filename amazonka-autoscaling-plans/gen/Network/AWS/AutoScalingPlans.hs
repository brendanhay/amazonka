{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Auto Scaling
--
-- Use AWS Auto Scaling to create scaling plans for your applications to
-- automatically scale your scalable AWS resources.
--
-- __API Summary__
--
-- You can use the AWS Auto Scaling service API to accomplish the following
-- tasks:
--
-- -   Create and manage scaling plans
--
-- -   Define target tracking scaling policies to dynamically scale your
--     resources based on utilization
--
-- -   Scale Amazon EC2 Auto Scaling groups using predictive scaling and
--     dynamic scaling to scale your Amazon EC2 capacity faster
--
-- -   Set minimum and maximum capacity limits
--
-- -   Retrieve information on existing scaling plans
--
-- -   Access current forecast data and historical forecast data for up to
--     56 days previous
--
-- To learn more about AWS Auto Scaling, including information about
-- granting IAM users required permissions for AWS Auto Scaling actions,
-- see the
-- <https://docs.aws.amazon.com/autoscaling/plans/userguide/what-is-aws-auto-scaling.html AWS Auto Scaling User Guide>.
module Network.AWS.AutoScalingPlans
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ObjectNotFoundException
    _ObjectNotFoundException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** ValidationException
    _ValidationException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ConcurrentUpdateException
    _ConcurrentUpdateException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateScalingPlan
    UpdateScalingPlan (UpdateScalingPlan'),
    newUpdateScalingPlan,
    UpdateScalingPlanResponse (UpdateScalingPlanResponse'),
    newUpdateScalingPlanResponse,

    -- ** DeleteScalingPlan
    DeleteScalingPlan (DeleteScalingPlan'),
    newDeleteScalingPlan,
    DeleteScalingPlanResponse (DeleteScalingPlanResponse'),
    newDeleteScalingPlanResponse,

    -- ** DescribeScalingPlanResources (Paginated)
    DescribeScalingPlanResources (DescribeScalingPlanResources'),
    newDescribeScalingPlanResources,
    DescribeScalingPlanResourcesResponse (DescribeScalingPlanResourcesResponse'),
    newDescribeScalingPlanResourcesResponse,

    -- ** GetScalingPlanResourceForecastData
    GetScalingPlanResourceForecastData (GetScalingPlanResourceForecastData'),
    newGetScalingPlanResourceForecastData,
    GetScalingPlanResourceForecastDataResponse (GetScalingPlanResourceForecastDataResponse'),
    newGetScalingPlanResourceForecastDataResponse,

    -- ** DescribeScalingPlans (Paginated)
    DescribeScalingPlans (DescribeScalingPlans'),
    newDescribeScalingPlans,
    DescribeScalingPlansResponse (DescribeScalingPlansResponse'),
    newDescribeScalingPlansResponse,

    -- ** CreateScalingPlan
    CreateScalingPlan (CreateScalingPlan'),
    newCreateScalingPlan,
    CreateScalingPlanResponse (CreateScalingPlanResponse'),
    newCreateScalingPlanResponse,

    -- * Types

    -- ** ForecastDataType
    ForecastDataType (..),

    -- ** LoadMetricType
    LoadMetricType (..),

    -- ** MetricStatistic
    MetricStatistic (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** PredictiveScalingMaxCapacityBehavior
    PredictiveScalingMaxCapacityBehavior (..),

    -- ** PredictiveScalingMode
    PredictiveScalingMode (..),

    -- ** ScalableDimension
    ScalableDimension (..),

    -- ** ScalingMetricType
    ScalingMetricType (..),

    -- ** ScalingPlanStatusCode
    ScalingPlanStatusCode (..),

    -- ** ScalingPolicyUpdateBehavior
    ScalingPolicyUpdateBehavior (..),

    -- ** ScalingStatusCode
    ScalingStatusCode (..),

    -- ** ServiceNamespace
    ServiceNamespace (..),

    -- ** ApplicationSource
    ApplicationSource (ApplicationSource'),
    newApplicationSource,

    -- ** CustomizedLoadMetricSpecification
    CustomizedLoadMetricSpecification (CustomizedLoadMetricSpecification'),
    newCustomizedLoadMetricSpecification,

    -- ** CustomizedScalingMetricSpecification
    CustomizedScalingMetricSpecification (CustomizedScalingMetricSpecification'),
    newCustomizedScalingMetricSpecification,

    -- ** Datapoint
    Datapoint (Datapoint'),
    newDatapoint,

    -- ** MetricDimension
    MetricDimension (MetricDimension'),
    newMetricDimension,

    -- ** PredefinedLoadMetricSpecification
    PredefinedLoadMetricSpecification (PredefinedLoadMetricSpecification'),
    newPredefinedLoadMetricSpecification,

    -- ** PredefinedScalingMetricSpecification
    PredefinedScalingMetricSpecification (PredefinedScalingMetricSpecification'),
    newPredefinedScalingMetricSpecification,

    -- ** ScalingInstruction
    ScalingInstruction (ScalingInstruction'),
    newScalingInstruction,

    -- ** ScalingPlan
    ScalingPlan (ScalingPlan'),
    newScalingPlan,

    -- ** ScalingPlanResource
    ScalingPlanResource (ScalingPlanResource'),
    newScalingPlanResource,

    -- ** ScalingPolicy
    ScalingPolicy (ScalingPolicy'),
    newScalingPolicy,

    -- ** TagFilter
    TagFilter (TagFilter'),
    newTagFilter,

    -- ** TargetTrackingConfiguration
    TargetTrackingConfiguration (TargetTrackingConfiguration'),
    newTargetTrackingConfiguration,
  )
where

import Network.AWS.AutoScalingPlans.CreateScalingPlan
import Network.AWS.AutoScalingPlans.DeleteScalingPlan
import Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
import Network.AWS.AutoScalingPlans.DescribeScalingPlans
import Network.AWS.AutoScalingPlans.GetScalingPlanResourceForecastData
import Network.AWS.AutoScalingPlans.Lens
import Network.AWS.AutoScalingPlans.Types
import Network.AWS.AutoScalingPlans.UpdateScalingPlan
import Network.AWS.AutoScalingPlans.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AutoScalingPlans'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
