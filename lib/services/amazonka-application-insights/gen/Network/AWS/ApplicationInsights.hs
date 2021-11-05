{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.ApplicationInsights
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon CloudWatch Application Insights
--
-- Amazon CloudWatch Application Insights is a service that helps you
-- detect common problems with your applications. It enables you to
-- pinpoint the source of issues in your applications (built with
-- technologies such as Microsoft IIS, .NET, and Microsoft SQL Server), by
-- providing key insights into detected problems.
--
-- After you onboard your application, CloudWatch Application Insights
-- identifies, recommends, and sets up metrics and logs. It continuously
-- analyzes and correlates your metrics and logs for unusual behavior to
-- surface actionable problems with your application. For example, if your
-- application is slow and unresponsive and leading to HTTP 500 errors in
-- your Application Load Balancer (ALB), Application Insights informs you
-- that a memory pressure problem with your SQL Server database is
-- occurring. It bases this analysis on impactful metrics and log errors.
module Network.AWS.ApplicationInsights
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** TagsAlreadyExistException
    _TagsAlreadyExistException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeApplication
    DescribeApplication (DescribeApplication'),
    newDescribeApplication,
    DescribeApplicationResponse (DescribeApplicationResponse'),
    newDescribeApplicationResponse,

    -- ** DescribeComponent
    DescribeComponent (DescribeComponent'),
    newDescribeComponent,
    DescribeComponentResponse (DescribeComponentResponse'),
    newDescribeComponentResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    UpdateApplicationResponse (UpdateApplicationResponse'),
    newUpdateApplicationResponse,

    -- ** DescribeComponentConfigurationRecommendation
    DescribeComponentConfigurationRecommendation (DescribeComponentConfigurationRecommendation'),
    newDescribeComponentConfigurationRecommendation,
    DescribeComponentConfigurationRecommendationResponse (DescribeComponentConfigurationRecommendationResponse'),
    newDescribeComponentConfigurationRecommendationResponse,

    -- ** DescribeProblem
    DescribeProblem (DescribeProblem'),
    newDescribeProblem,
    DescribeProblemResponse (DescribeProblemResponse'),
    newDescribeProblemResponse,

    -- ** UpdateComponentConfiguration
    UpdateComponentConfiguration (UpdateComponentConfiguration'),
    newUpdateComponentConfiguration,
    UpdateComponentConfigurationResponse (UpdateComponentConfigurationResponse'),
    newUpdateComponentConfigurationResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** DescribeProblemObservations
    DescribeProblemObservations (DescribeProblemObservations'),
    newDescribeProblemObservations,
    DescribeProblemObservationsResponse (DescribeProblemObservationsResponse'),
    newDescribeProblemObservationsResponse,

    -- ** DescribeObservation
    DescribeObservation (DescribeObservation'),
    newDescribeObservation,
    DescribeObservationResponse (DescribeObservationResponse'),
    newDescribeObservationResponse,

    -- ** ListLogPatternSets
    ListLogPatternSets (ListLogPatternSets'),
    newListLogPatternSets,
    ListLogPatternSetsResponse (ListLogPatternSetsResponse'),
    newListLogPatternSetsResponse,

    -- ** DescribeComponentConfiguration
    DescribeComponentConfiguration (DescribeComponentConfiguration'),
    newDescribeComponentConfiguration,
    DescribeComponentConfigurationResponse (DescribeComponentConfigurationResponse'),
    newDescribeComponentConfigurationResponse,

    -- ** ListProblems
    ListProblems (ListProblems'),
    newListProblems,
    ListProblemsResponse (ListProblemsResponse'),
    newListProblemsResponse,

    -- ** ListLogPatterns
    ListLogPatterns (ListLogPatterns'),
    newListLogPatterns,
    ListLogPatternsResponse (ListLogPatternsResponse'),
    newListLogPatternsResponse,

    -- ** DeleteLogPattern
    DeleteLogPattern (DeleteLogPattern'),
    newDeleteLogPattern,
    DeleteLogPatternResponse (DeleteLogPatternResponse'),
    newDeleteLogPatternResponse,

    -- ** UpdateLogPattern
    UpdateLogPattern (UpdateLogPattern'),
    newUpdateLogPattern,
    UpdateLogPatternResponse (UpdateLogPatternResponse'),
    newUpdateLogPatternResponse,

    -- ** CreateLogPattern
    CreateLogPattern (CreateLogPattern'),
    newCreateLogPattern,
    CreateLogPatternResponse (CreateLogPatternResponse'),
    newCreateLogPatternResponse,

    -- ** ListConfigurationHistory
    ListConfigurationHistory (ListConfigurationHistory'),
    newListConfigurationHistory,
    ListConfigurationHistoryResponse (ListConfigurationHistoryResponse'),
    newListConfigurationHistoryResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListApplications
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateComponent
    CreateComponent (CreateComponent'),
    newCreateComponent,
    CreateComponentResponse (CreateComponentResponse'),
    newCreateComponentResponse,

    -- ** ListComponents
    ListComponents (ListComponents'),
    newListComponents,
    ListComponentsResponse (ListComponentsResponse'),
    newListComponentsResponse,

    -- ** DeleteComponent
    DeleteComponent (DeleteComponent'),
    newDeleteComponent,
    DeleteComponentResponse (DeleteComponentResponse'),
    newDeleteComponentResponse,

    -- ** UpdateComponent
    UpdateComponent (UpdateComponent'),
    newUpdateComponent,
    UpdateComponentResponse (UpdateComponentResponse'),
    newUpdateComponentResponse,

    -- ** DescribeLogPattern
    DescribeLogPattern (DescribeLogPattern'),
    newDescribeLogPattern,
    DescribeLogPatternResponse (DescribeLogPatternResponse'),
    newDescribeLogPatternResponse,

    -- * Types

    -- ** CloudWatchEventSource
    CloudWatchEventSource (..),

    -- ** ConfigurationEventResourceType
    ConfigurationEventResourceType (..),

    -- ** ConfigurationEventStatus
    ConfigurationEventStatus (..),

    -- ** FeedbackKey
    FeedbackKey (..),

    -- ** FeedbackValue
    FeedbackValue (..),

    -- ** LogFilter
    LogFilter (..),

    -- ** OsType
    OsType (..),

    -- ** SeverityLevel
    SeverityLevel (..),

    -- ** Status
    Status (..),

    -- ** Tier
    Tier (..),

    -- ** ApplicationComponent
    ApplicationComponent (ApplicationComponent'),
    newApplicationComponent,

    -- ** ApplicationInfo
    ApplicationInfo (ApplicationInfo'),
    newApplicationInfo,

    -- ** ConfigurationEvent
    ConfigurationEvent (ConfigurationEvent'),
    newConfigurationEvent,

    -- ** LogPattern
    LogPattern (LogPattern'),
    newLogPattern,

    -- ** Observation
    Observation (Observation'),
    newObservation,

    -- ** Problem
    Problem (Problem'),
    newProblem,

    -- ** RelatedObservations
    RelatedObservations (RelatedObservations'),
    newRelatedObservations,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.ApplicationInsights.CreateApplication
import Network.AWS.ApplicationInsights.CreateComponent
import Network.AWS.ApplicationInsights.CreateLogPattern
import Network.AWS.ApplicationInsights.DeleteApplication
import Network.AWS.ApplicationInsights.DeleteComponent
import Network.AWS.ApplicationInsights.DeleteLogPattern
import Network.AWS.ApplicationInsights.DescribeApplication
import Network.AWS.ApplicationInsights.DescribeComponent
import Network.AWS.ApplicationInsights.DescribeComponentConfiguration
import Network.AWS.ApplicationInsights.DescribeComponentConfigurationRecommendation
import Network.AWS.ApplicationInsights.DescribeLogPattern
import Network.AWS.ApplicationInsights.DescribeObservation
import Network.AWS.ApplicationInsights.DescribeProblem
import Network.AWS.ApplicationInsights.DescribeProblemObservations
import Network.AWS.ApplicationInsights.Lens
import Network.AWS.ApplicationInsights.ListApplications
import Network.AWS.ApplicationInsights.ListComponents
import Network.AWS.ApplicationInsights.ListConfigurationHistory
import Network.AWS.ApplicationInsights.ListLogPatternSets
import Network.AWS.ApplicationInsights.ListLogPatterns
import Network.AWS.ApplicationInsights.ListProblems
import Network.AWS.ApplicationInsights.ListTagsForResource
import Network.AWS.ApplicationInsights.TagResource
import Network.AWS.ApplicationInsights.Types
import Network.AWS.ApplicationInsights.UntagResource
import Network.AWS.ApplicationInsights.UpdateApplication
import Network.AWS.ApplicationInsights.UpdateComponent
import Network.AWS.ApplicationInsights.UpdateComponentConfiguration
import Network.AWS.ApplicationInsights.UpdateLogPattern
import Network.AWS.ApplicationInsights.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ApplicationInsights'.

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
