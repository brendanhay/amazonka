{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DeviceFarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-06-23@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the AWS Device Farm API documentation, which contains APIs
-- for:
--
-- -   Testing on desktop browsers
--
--     Device Farm makes it possible for you to test your web applications
--     on desktop browsers using Selenium. The APIs for desktop browser
--     testing contain @TestGrid@ in their names. For more information, see
--     <https://docs.aws.amazon.com/devicefarm/latest/testgrid/ Testing Web Applications on Selenium with Device Farm>.
--
-- -   Testing on real mobile devices
--
--     Device Farm makes it possible for you to test apps on physical
--     phones, tablets, and other devices in the cloud. For more
--     information, see the
--     <https://docs.aws.amazon.com/devicefarm/latest/developerguide/ Device Farm Developer Guide>.
module Amazonka.DeviceFarm
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotEligibleException
    _NotEligibleException,

    -- ** CannotDeleteException
    _CannotDeleteException,

    -- ** IdempotencyException
    _IdempotencyException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ArgumentException
    _ArgumentException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** TagPolicyException
    _TagPolicyException,

    -- ** TagOperationException
    _TagOperationException,

    -- ** ServiceAccountException
    _ServiceAccountException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** UpdateProject
    UpdateProject (UpdateProject'),
    newUpdateProject,
    UpdateProjectResponse (UpdateProjectResponse'),
    newUpdateProjectResponse,

    -- ** UpdateNetworkProfile
    UpdateNetworkProfile (UpdateNetworkProfile'),
    newUpdateNetworkProfile,
    UpdateNetworkProfileResponse (UpdateNetworkProfileResponse'),
    newUpdateNetworkProfileResponse,

    -- ** DeleteNetworkProfile
    DeleteNetworkProfile (DeleteNetworkProfile'),
    newDeleteNetworkProfile,
    DeleteNetworkProfileResponse (DeleteNetworkProfileResponse'),
    newDeleteNetworkProfileResponse,

    -- ** GetDevicePoolCompatibility
    GetDevicePoolCompatibility (GetDevicePoolCompatibility'),
    newGetDevicePoolCompatibility,
    GetDevicePoolCompatibilityResponse (GetDevicePoolCompatibilityResponse'),
    newGetDevicePoolCompatibilityResponse,

    -- ** InstallToRemoteAccessSession
    InstallToRemoteAccessSession (InstallToRemoteAccessSession'),
    newInstallToRemoteAccessSession,
    InstallToRemoteAccessSessionResponse (InstallToRemoteAccessSessionResponse'),
    newInstallToRemoteAccessSessionResponse,

    -- ** ListTests (Paginated)
    ListTests (ListTests'),
    newListTests,
    ListTestsResponse (ListTestsResponse'),
    newListTestsResponse,

    -- ** ListArtifacts (Paginated)
    ListArtifacts (ListArtifacts'),
    newListArtifacts,
    ListArtifactsResponse (ListArtifactsResponse'),
    newListArtifactsResponse,

    -- ** ListTestGridSessionActions
    ListTestGridSessionActions (ListTestGridSessionActions'),
    newListTestGridSessionActions,
    ListTestGridSessionActionsResponse (ListTestGridSessionActionsResponse'),
    newListTestGridSessionActionsResponse,

    -- ** CreateUpload
    CreateUpload (CreateUpload'),
    newCreateUpload,
    CreateUploadResponse (CreateUploadResponse'),
    newCreateUploadResponse,

    -- ** GetDeviceInstance
    GetDeviceInstance (GetDeviceInstance'),
    newGetDeviceInstance,
    GetDeviceInstanceResponse (GetDeviceInstanceResponse'),
    newGetDeviceInstanceResponse,

    -- ** StopJob
    StopJob (StopJob'),
    newStopJob,
    StopJobResponse (StopJobResponse'),
    newStopJobResponse,

    -- ** DeleteRemoteAccessSession
    DeleteRemoteAccessSession (DeleteRemoteAccessSession'),
    newDeleteRemoteAccessSession,
    DeleteRemoteAccessSessionResponse (DeleteRemoteAccessSessionResponse'),
    newDeleteRemoteAccessSessionResponse,

    -- ** ListTestGridSessionArtifacts
    ListTestGridSessionArtifacts (ListTestGridSessionArtifacts'),
    newListTestGridSessionArtifacts,
    ListTestGridSessionArtifactsResponse (ListTestGridSessionArtifactsResponse'),
    newListTestGridSessionArtifactsResponse,

    -- ** ListTestGridProjects
    ListTestGridProjects (ListTestGridProjects'),
    newListTestGridProjects,
    ListTestGridProjectsResponse (ListTestGridProjectsResponse'),
    newListTestGridProjectsResponse,

    -- ** DeleteUpload
    DeleteUpload (DeleteUpload'),
    newDeleteUpload,
    DeleteUploadResponse (DeleteUploadResponse'),
    newDeleteUploadResponse,

    -- ** UpdateUpload
    UpdateUpload (UpdateUpload'),
    newUpdateUpload,
    UpdateUploadResponse (UpdateUploadResponse'),
    newUpdateUploadResponse,

    -- ** DeleteTestGridProject
    DeleteTestGridProject (DeleteTestGridProject'),
    newDeleteTestGridProject,
    DeleteTestGridProjectResponse (DeleteTestGridProjectResponse'),
    newDeleteTestGridProjectResponse,

    -- ** UpdateTestGridProject
    UpdateTestGridProject (UpdateTestGridProject'),
    newUpdateTestGridProject,
    UpdateTestGridProjectResponse (UpdateTestGridProjectResponse'),
    newUpdateTestGridProjectResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetDevicePool
    GetDevicePool (GetDevicePool'),
    newGetDevicePool,
    GetDevicePoolResponse (GetDevicePoolResponse'),
    newGetDevicePoolResponse,

    -- ** ListDevicePools (Paginated)
    ListDevicePools (ListDevicePools'),
    newListDevicePools,
    ListDevicePoolsResponse (ListDevicePoolsResponse'),
    newListDevicePoolsResponse,

    -- ** UpdateDevicePool
    UpdateDevicePool (UpdateDevicePool'),
    newUpdateDevicePool,
    UpdateDevicePoolResponse (UpdateDevicePoolResponse'),
    newUpdateDevicePoolResponse,

    -- ** DeleteDevicePool
    DeleteDevicePool (DeleteDevicePool'),
    newDeleteDevicePool,
    DeleteDevicePoolResponse (DeleteDevicePoolResponse'),
    newDeleteDevicePoolResponse,

    -- ** GetUpload
    GetUpload (GetUpload'),
    newGetUpload,
    GetUploadResponse (GetUploadResponse'),
    newGetUploadResponse,

    -- ** ListOfferingTransactions (Paginated)
    ListOfferingTransactions (ListOfferingTransactions'),
    newListOfferingTransactions,
    ListOfferingTransactionsResponse (ListOfferingTransactionsResponse'),
    newListOfferingTransactionsResponse,

    -- ** CreateDevicePool
    CreateDevicePool (CreateDevicePool'),
    newCreateDevicePool,
    CreateDevicePoolResponse (CreateDevicePoolResponse'),
    newCreateDevicePoolResponse,

    -- ** DeleteRun
    DeleteRun (DeleteRun'),
    newDeleteRun,
    DeleteRunResponse (DeleteRunResponse'),
    newDeleteRunResponse,

    -- ** ListRuns (Paginated)
    ListRuns (ListRuns'),
    newListRuns,
    ListRunsResponse (ListRunsResponse'),
    newListRunsResponse,

    -- ** GetTest
    GetTest (GetTest'),
    newGetTest,
    GetTestResponse (GetTestResponse'),
    newGetTestResponse,

    -- ** UpdateDeviceInstance
    UpdateDeviceInstance (UpdateDeviceInstance'),
    newUpdateDeviceInstance,
    UpdateDeviceInstanceResponse (UpdateDeviceInstanceResponse'),
    newUpdateDeviceInstanceResponse,

    -- ** GetNetworkProfile
    GetNetworkProfile (GetNetworkProfile'),
    newGetNetworkProfile,
    GetNetworkProfileResponse (GetNetworkProfileResponse'),
    newGetNetworkProfileResponse,

    -- ** RenewOffering
    RenewOffering (RenewOffering'),
    newRenewOffering,
    RenewOfferingResponse (RenewOfferingResponse'),
    newRenewOfferingResponse,

    -- ** DeleteInstanceProfile
    DeleteInstanceProfile (DeleteInstanceProfile'),
    newDeleteInstanceProfile,
    DeleteInstanceProfileResponse (DeleteInstanceProfileResponse'),
    newDeleteInstanceProfileResponse,

    -- ** UpdateInstanceProfile
    UpdateInstanceProfile (UpdateInstanceProfile'),
    newUpdateInstanceProfile,
    UpdateInstanceProfileResponse (UpdateInstanceProfileResponse'),
    newUpdateInstanceProfileResponse,

    -- ** CreateInstanceProfile
    CreateInstanceProfile (CreateInstanceProfile'),
    newCreateInstanceProfile,
    CreateInstanceProfileResponse (CreateInstanceProfileResponse'),
    newCreateInstanceProfileResponse,

    -- ** GetDevice
    GetDevice (GetDevice'),
    newGetDevice,
    GetDeviceResponse (GetDeviceResponse'),
    newGetDeviceResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** GetTestGridSession
    GetTestGridSession (GetTestGridSession'),
    newGetTestGridSession,
    GetTestGridSessionResponse (GetTestGridSessionResponse'),
    newGetTestGridSessionResponse,

    -- ** GetVPCEConfiguration
    GetVPCEConfiguration (GetVPCEConfiguration'),
    newGetVPCEConfiguration,
    GetVPCEConfigurationResponse (GetVPCEConfigurationResponse'),
    newGetVPCEConfigurationResponse,

    -- ** StopRemoteAccessSession
    StopRemoteAccessSession (StopRemoteAccessSession'),
    newStopRemoteAccessSession,
    StopRemoteAccessSessionResponse (StopRemoteAccessSessionResponse'),
    newStopRemoteAccessSessionResponse,

    -- ** CreateNetworkProfile
    CreateNetworkProfile (CreateNetworkProfile'),
    newCreateNetworkProfile,
    CreateNetworkProfileResponse (CreateNetworkProfileResponse'),
    newCreateNetworkProfileResponse,

    -- ** DeleteVPCEConfiguration
    DeleteVPCEConfiguration (DeleteVPCEConfiguration'),
    newDeleteVPCEConfiguration,
    DeleteVPCEConfigurationResponse (DeleteVPCEConfigurationResponse'),
    newDeleteVPCEConfigurationResponse,

    -- ** UpdateVPCEConfiguration
    UpdateVPCEConfiguration (UpdateVPCEConfiguration'),
    newUpdateVPCEConfiguration,
    UpdateVPCEConfigurationResponse (UpdateVPCEConfigurationResponse'),
    newUpdateVPCEConfigurationResponse,

    -- ** GetJob
    GetJob (GetJob'),
    newGetJob,
    GetJobResponse (GetJobResponse'),
    newGetJobResponse,

    -- ** GetInstanceProfile
    GetInstanceProfile (GetInstanceProfile'),
    newGetInstanceProfile,
    GetInstanceProfileResponse (GetInstanceProfileResponse'),
    newGetInstanceProfileResponse,

    -- ** ListNetworkProfiles (Paginated)
    ListNetworkProfiles (ListNetworkProfiles'),
    newListNetworkProfiles,
    ListNetworkProfilesResponse (ListNetworkProfilesResponse'),
    newListNetworkProfilesResponse,

    -- ** CreateVPCEConfiguration
    CreateVPCEConfiguration (CreateVPCEConfiguration'),
    newCreateVPCEConfiguration,
    CreateVPCEConfigurationResponse (CreateVPCEConfigurationResponse'),
    newCreateVPCEConfigurationResponse,

    -- ** ScheduleRun
    ScheduleRun (ScheduleRun'),
    newScheduleRun,
    ScheduleRunResponse (ScheduleRunResponse'),
    newScheduleRunResponse,

    -- ** CreateTestGridProject
    CreateTestGridProject (CreateTestGridProject'),
    newCreateTestGridProject,
    CreateTestGridProjectResponse (CreateTestGridProjectResponse'),
    newCreateTestGridProjectResponse,

    -- ** GetRun
    GetRun (GetRun'),
    newGetRun,
    GetRunResponse (GetRunResponse'),
    newGetRunResponse,

    -- ** ListSamples (Paginated)
    ListSamples (ListSamples'),
    newListSamples,
    ListSamplesResponse (ListSamplesResponse'),
    newListSamplesResponse,

    -- ** ListSuites (Paginated)
    ListSuites (ListSuites'),
    newListSuites,
    ListSuitesResponse (ListSuitesResponse'),
    newListSuitesResponse,

    -- ** ListRemoteAccessSessions (Paginated)
    ListRemoteAccessSessions (ListRemoteAccessSessions'),
    newListRemoteAccessSessions,
    ListRemoteAccessSessionsResponse (ListRemoteAccessSessionsResponse'),
    newListRemoteAccessSessionsResponse,

    -- ** GetAccountSettings
    GetAccountSettings (GetAccountSettings'),
    newGetAccountSettings,
    GetAccountSettingsResponse (GetAccountSettingsResponse'),
    newGetAccountSettingsResponse,

    -- ** CreateRemoteAccessSession
    CreateRemoteAccessSession (CreateRemoteAccessSession'),
    newCreateRemoteAccessSession,
    CreateRemoteAccessSessionResponse (CreateRemoteAccessSessionResponse'),
    newCreateRemoteAccessSessionResponse,

    -- ** ListOfferingPromotions (Paginated)
    ListOfferingPromotions (ListOfferingPromotions'),
    newListOfferingPromotions,
    ListOfferingPromotionsResponse (ListOfferingPromotionsResponse'),
    newListOfferingPromotionsResponse,

    -- ** GetOfferingStatus (Paginated)
    GetOfferingStatus (GetOfferingStatus'),
    newGetOfferingStatus,
    GetOfferingStatusResponse (GetOfferingStatusResponse'),
    newGetOfferingStatusResponse,

    -- ** ListUploads (Paginated)
    ListUploads (ListUploads'),
    newListUploads,
    ListUploadsResponse (ListUploadsResponse'),
    newListUploadsResponse,

    -- ** GetTestGridProject
    GetTestGridProject (GetTestGridProject'),
    newGetTestGridProject,
    GetTestGridProjectResponse (GetTestGridProjectResponse'),
    newGetTestGridProjectResponse,

    -- ** GetSuite
    GetSuite (GetSuite'),
    newGetSuite,
    GetSuiteResponse (GetSuiteResponse'),
    newGetSuiteResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetRemoteAccessSession
    GetRemoteAccessSession (GetRemoteAccessSession'),
    newGetRemoteAccessSession,
    GetRemoteAccessSessionResponse (GetRemoteAccessSessionResponse'),
    newGetRemoteAccessSessionResponse,

    -- ** ListDeviceInstances (Paginated)
    ListDeviceInstances (ListDeviceInstances'),
    newListDeviceInstances,
    ListDeviceInstancesResponse (ListDeviceInstancesResponse'),
    newListDeviceInstancesResponse,

    -- ** PurchaseOffering
    PurchaseOffering (PurchaseOffering'),
    newPurchaseOffering,
    PurchaseOfferingResponse (PurchaseOfferingResponse'),
    newPurchaseOfferingResponse,

    -- ** ListInstanceProfiles (Paginated)
    ListInstanceProfiles (ListInstanceProfiles'),
    newListInstanceProfiles,
    ListInstanceProfilesResponse (ListInstanceProfilesResponse'),
    newListInstanceProfilesResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetProject
    GetProject (GetProject'),
    newGetProject,
    GetProjectResponse (GetProjectResponse'),
    newGetProjectResponse,

    -- ** ListUniqueProblems (Paginated)
    ListUniqueProblems (ListUniqueProblems'),
    newListUniqueProblems,
    ListUniqueProblemsResponse (ListUniqueProblemsResponse'),
    newListUniqueProblemsResponse,

    -- ** ListVPCEConfigurations (Paginated)
    ListVPCEConfigurations (ListVPCEConfigurations'),
    newListVPCEConfigurations,
    ListVPCEConfigurationsResponse (ListVPCEConfigurationsResponse'),
    newListVPCEConfigurationsResponse,

    -- ** StopRun
    StopRun (StopRun'),
    newStopRun,
    StopRunResponse (StopRunResponse'),
    newStopRunResponse,

    -- ** ListDevices (Paginated)
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** ListTestGridSessions
    ListTestGridSessions (ListTestGridSessions'),
    newListTestGridSessions,
    ListTestGridSessionsResponse (ListTestGridSessionsResponse'),
    newListTestGridSessionsResponse,

    -- ** CreateTestGridUrl
    CreateTestGridUrl (CreateTestGridUrl'),
    newCreateTestGridUrl,
    CreateTestGridUrlResponse (CreateTestGridUrlResponse'),
    newCreateTestGridUrlResponse,

    -- ** ListOfferings (Paginated)
    ListOfferings (ListOfferings'),
    newListOfferings,
    ListOfferingsResponse (ListOfferingsResponse'),
    newListOfferingsResponse,

    -- * Types

    -- ** ArtifactCategory
    ArtifactCategory (..),

    -- ** ArtifactType
    ArtifactType (..),

    -- ** BillingMethod
    BillingMethod (..),

    -- ** CurrencyCode
    CurrencyCode (..),

    -- ** DeviceAttribute
    DeviceAttribute (..),

    -- ** DeviceAvailability
    DeviceAvailability (..),

    -- ** DeviceFilterAttribute
    DeviceFilterAttribute (..),

    -- ** DeviceFormFactor
    DeviceFormFactor (..),

    -- ** DevicePlatform
    DevicePlatform (..),

    -- ** DevicePoolType
    DevicePoolType (..),

    -- ** ExecutionResult
    ExecutionResult (..),

    -- ** ExecutionResultCode
    ExecutionResultCode (..),

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** InstanceStatus
    InstanceStatus (..),

    -- ** InteractionMode
    InteractionMode (..),

    -- ** NetworkProfileType
    NetworkProfileType (..),

    -- ** OfferingTransactionType
    OfferingTransactionType (..),

    -- ** OfferingType
    OfferingType (..),

    -- ** RecurringChargeFrequency
    RecurringChargeFrequency (..),

    -- ** RuleOperator
    RuleOperator (..),

    -- ** SampleType
    SampleType (..),

    -- ** TestGridSessionArtifactCategory
    TestGridSessionArtifactCategory (..),

    -- ** TestGridSessionArtifactType
    TestGridSessionArtifactType (..),

    -- ** TestGridSessionStatus
    TestGridSessionStatus (..),

    -- ** TestType
    TestType (..),

    -- ** UploadCategory
    UploadCategory (..),

    -- ** UploadStatus
    UploadStatus (..),

    -- ** UploadType
    UploadType (..),

    -- ** AccountSettings
    AccountSettings (AccountSettings'),
    newAccountSettings,

    -- ** Artifact
    Artifact (Artifact'),
    newArtifact,

    -- ** CPU
    CPU (CPU'),
    newCPU,

    -- ** Counters
    Counters (Counters'),
    newCounters,

    -- ** CreateRemoteAccessSessionConfiguration
    CreateRemoteAccessSessionConfiguration (CreateRemoteAccessSessionConfiguration'),
    newCreateRemoteAccessSessionConfiguration,

    -- ** CustomerArtifactPaths
    CustomerArtifactPaths (CustomerArtifactPaths'),
    newCustomerArtifactPaths,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** DeviceFilter
    DeviceFilter (DeviceFilter'),
    newDeviceFilter,

    -- ** DeviceInstance
    DeviceInstance (DeviceInstance'),
    newDeviceInstance,

    -- ** DeviceMinutes
    DeviceMinutes (DeviceMinutes'),
    newDeviceMinutes,

    -- ** DevicePool
    DevicePool (DevicePool'),
    newDevicePool,

    -- ** DevicePoolCompatibilityResult
    DevicePoolCompatibilityResult (DevicePoolCompatibilityResult'),
    newDevicePoolCompatibilityResult,

    -- ** DeviceSelectionConfiguration
    DeviceSelectionConfiguration (DeviceSelectionConfiguration'),
    newDeviceSelectionConfiguration,

    -- ** DeviceSelectionResult
    DeviceSelectionResult (DeviceSelectionResult'),
    newDeviceSelectionResult,

    -- ** ExecutionConfiguration
    ExecutionConfiguration (ExecutionConfiguration'),
    newExecutionConfiguration,

    -- ** IncompatibilityMessage
    IncompatibilityMessage (IncompatibilityMessage'),
    newIncompatibilityMessage,

    -- ** InstanceProfile
    InstanceProfile (InstanceProfile'),
    newInstanceProfile,

    -- ** Job
    Job (Job'),
    newJob,

    -- ** Location
    Location (Location'),
    newLocation,

    -- ** MonetaryAmount
    MonetaryAmount (MonetaryAmount'),
    newMonetaryAmount,

    -- ** NetworkProfile
    NetworkProfile (NetworkProfile'),
    newNetworkProfile,

    -- ** Offering
    Offering (Offering'),
    newOffering,

    -- ** OfferingPromotion
    OfferingPromotion (OfferingPromotion'),
    newOfferingPromotion,

    -- ** OfferingStatus
    OfferingStatus (OfferingStatus'),
    newOfferingStatus,

    -- ** OfferingTransaction
    OfferingTransaction (OfferingTransaction'),
    newOfferingTransaction,

    -- ** Problem
    Problem (Problem'),
    newProblem,

    -- ** ProblemDetail
    ProblemDetail (ProblemDetail'),
    newProblemDetail,

    -- ** Project
    Project (Project'),
    newProject,

    -- ** Radios
    Radios (Radios'),
    newRadios,

    -- ** RecurringCharge
    RecurringCharge (RecurringCharge'),
    newRecurringCharge,

    -- ** RemoteAccessSession
    RemoteAccessSession (RemoteAccessSession'),
    newRemoteAccessSession,

    -- ** Resolution
    Resolution (Resolution'),
    newResolution,

    -- ** Rule
    Rule (Rule'),
    newRule,

    -- ** Run
    Run (Run'),
    newRun,

    -- ** Sample
    Sample (Sample'),
    newSample,

    -- ** ScheduleRunConfiguration
    ScheduleRunConfiguration (ScheduleRunConfiguration'),
    newScheduleRunConfiguration,

    -- ** ScheduleRunTest
    ScheduleRunTest (ScheduleRunTest'),
    newScheduleRunTest,

    -- ** Suite
    Suite (Suite'),
    newSuite,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Test
    Test (Test'),
    newTest,

    -- ** TestGridProject
    TestGridProject (TestGridProject'),
    newTestGridProject,

    -- ** TestGridSession
    TestGridSession (TestGridSession'),
    newTestGridSession,

    -- ** TestGridSessionAction
    TestGridSessionAction (TestGridSessionAction'),
    newTestGridSessionAction,

    -- ** TestGridSessionArtifact
    TestGridSessionArtifact (TestGridSessionArtifact'),
    newTestGridSessionArtifact,

    -- ** TestGridVpcConfig
    TestGridVpcConfig (TestGridVpcConfig'),
    newTestGridVpcConfig,

    -- ** TrialMinutes
    TrialMinutes (TrialMinutes'),
    newTrialMinutes,

    -- ** UniqueProblem
    UniqueProblem (UniqueProblem'),
    newUniqueProblem,

    -- ** Upload
    Upload (Upload'),
    newUpload,

    -- ** VPCEConfiguration
    VPCEConfiguration (VPCEConfiguration'),
    newVPCEConfiguration,
  )
where

import Amazonka.DeviceFarm.CreateDevicePool
import Amazonka.DeviceFarm.CreateInstanceProfile
import Amazonka.DeviceFarm.CreateNetworkProfile
import Amazonka.DeviceFarm.CreateProject
import Amazonka.DeviceFarm.CreateRemoteAccessSession
import Amazonka.DeviceFarm.CreateTestGridProject
import Amazonka.DeviceFarm.CreateTestGridUrl
import Amazonka.DeviceFarm.CreateUpload
import Amazonka.DeviceFarm.CreateVPCEConfiguration
import Amazonka.DeviceFarm.DeleteDevicePool
import Amazonka.DeviceFarm.DeleteInstanceProfile
import Amazonka.DeviceFarm.DeleteNetworkProfile
import Amazonka.DeviceFarm.DeleteProject
import Amazonka.DeviceFarm.DeleteRemoteAccessSession
import Amazonka.DeviceFarm.DeleteRun
import Amazonka.DeviceFarm.DeleteTestGridProject
import Amazonka.DeviceFarm.DeleteUpload
import Amazonka.DeviceFarm.DeleteVPCEConfiguration
import Amazonka.DeviceFarm.GetAccountSettings
import Amazonka.DeviceFarm.GetDevice
import Amazonka.DeviceFarm.GetDeviceInstance
import Amazonka.DeviceFarm.GetDevicePool
import Amazonka.DeviceFarm.GetDevicePoolCompatibility
import Amazonka.DeviceFarm.GetInstanceProfile
import Amazonka.DeviceFarm.GetJob
import Amazonka.DeviceFarm.GetNetworkProfile
import Amazonka.DeviceFarm.GetOfferingStatus
import Amazonka.DeviceFarm.GetProject
import Amazonka.DeviceFarm.GetRemoteAccessSession
import Amazonka.DeviceFarm.GetRun
import Amazonka.DeviceFarm.GetSuite
import Amazonka.DeviceFarm.GetTest
import Amazonka.DeviceFarm.GetTestGridProject
import Amazonka.DeviceFarm.GetTestGridSession
import Amazonka.DeviceFarm.GetUpload
import Amazonka.DeviceFarm.GetVPCEConfiguration
import Amazonka.DeviceFarm.InstallToRemoteAccessSession
import Amazonka.DeviceFarm.Lens
import Amazonka.DeviceFarm.ListArtifacts
import Amazonka.DeviceFarm.ListDeviceInstances
import Amazonka.DeviceFarm.ListDevicePools
import Amazonka.DeviceFarm.ListDevices
import Amazonka.DeviceFarm.ListInstanceProfiles
import Amazonka.DeviceFarm.ListJobs
import Amazonka.DeviceFarm.ListNetworkProfiles
import Amazonka.DeviceFarm.ListOfferingPromotions
import Amazonka.DeviceFarm.ListOfferingTransactions
import Amazonka.DeviceFarm.ListOfferings
import Amazonka.DeviceFarm.ListProjects
import Amazonka.DeviceFarm.ListRemoteAccessSessions
import Amazonka.DeviceFarm.ListRuns
import Amazonka.DeviceFarm.ListSamples
import Amazonka.DeviceFarm.ListSuites
import Amazonka.DeviceFarm.ListTagsForResource
import Amazonka.DeviceFarm.ListTestGridProjects
import Amazonka.DeviceFarm.ListTestGridSessionActions
import Amazonka.DeviceFarm.ListTestGridSessionArtifacts
import Amazonka.DeviceFarm.ListTestGridSessions
import Amazonka.DeviceFarm.ListTests
import Amazonka.DeviceFarm.ListUniqueProblems
import Amazonka.DeviceFarm.ListUploads
import Amazonka.DeviceFarm.ListVPCEConfigurations
import Amazonka.DeviceFarm.PurchaseOffering
import Amazonka.DeviceFarm.RenewOffering
import Amazonka.DeviceFarm.ScheduleRun
import Amazonka.DeviceFarm.StopJob
import Amazonka.DeviceFarm.StopRemoteAccessSession
import Amazonka.DeviceFarm.StopRun
import Amazonka.DeviceFarm.TagResource
import Amazonka.DeviceFarm.Types
import Amazonka.DeviceFarm.UntagResource
import Amazonka.DeviceFarm.UpdateDeviceInstance
import Amazonka.DeviceFarm.UpdateDevicePool
import Amazonka.DeviceFarm.UpdateInstanceProfile
import Amazonka.DeviceFarm.UpdateNetworkProfile
import Amazonka.DeviceFarm.UpdateProject
import Amazonka.DeviceFarm.UpdateTestGridProject
import Amazonka.DeviceFarm.UpdateUpload
import Amazonka.DeviceFarm.UpdateVPCEConfiguration
import Amazonka.DeviceFarm.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DeviceFarm'.

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
