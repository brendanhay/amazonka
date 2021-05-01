{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
module Network.AWS.DeviceFarm
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotEligibleException
    _NotEligibleException,

    -- ** ArgumentException
    _ArgumentException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** ServiceAccountException
    _ServiceAccountException,

    -- ** TagOperationException
    _TagOperationException,

    -- ** IdempotencyException
    _IdempotencyException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** TagPolicyException
    _TagPolicyException,

    -- ** CannotDeleteException
    _CannotDeleteException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateUpload
    CreateUpload (CreateUpload'),
    newCreateUpload,
    CreateUploadResponse (CreateUploadResponse'),
    newCreateUploadResponse,

    -- ** ListTestGridSessionActions
    ListTestGridSessionActions (ListTestGridSessionActions'),
    newListTestGridSessionActions,
    ListTestGridSessionActionsResponse (ListTestGridSessionActionsResponse'),
    newListTestGridSessionActionsResponse,

    -- ** CreateTestGridProject
    CreateTestGridProject (CreateTestGridProject'),
    newCreateTestGridProject,
    CreateTestGridProjectResponse (CreateTestGridProjectResponse'),
    newCreateTestGridProjectResponse,

    -- ** ListSamples (Paginated)
    ListSamples (ListSamples'),
    newListSamples,
    ListSamplesResponse (ListSamplesResponse'),
    newListSamplesResponse,

    -- ** ListTestGridSessionArtifacts
    ListTestGridSessionArtifacts (ListTestGridSessionArtifacts'),
    newListTestGridSessionArtifacts,
    ListTestGridSessionArtifactsResponse (ListTestGridSessionArtifactsResponse'),
    newListTestGridSessionArtifactsResponse,

    -- ** ListNetworkProfiles (Paginated)
    ListNetworkProfiles (ListNetworkProfiles'),
    newListNetworkProfiles,
    ListNetworkProfilesResponse (ListNetworkProfilesResponse'),
    newListNetworkProfilesResponse,

    -- ** ScheduleRun
    ScheduleRun (ScheduleRun'),
    newScheduleRun,
    ScheduleRunResponse (ScheduleRunResponse'),
    newScheduleRunResponse,

    -- ** UpdateNetworkProfile
    UpdateNetworkProfile (UpdateNetworkProfile'),
    newUpdateNetworkProfile,
    UpdateNetworkProfileResponse (UpdateNetworkProfileResponse'),
    newUpdateNetworkProfileResponse,

    -- ** InstallToRemoteAccessSession
    InstallToRemoteAccessSession (InstallToRemoteAccessSession'),
    newInstallToRemoteAccessSession,
    InstallToRemoteAccessSessionResponse (InstallToRemoteAccessSessionResponse'),
    newInstallToRemoteAccessSessionResponse,

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

    -- ** CreateVPCEConfiguration
    CreateVPCEConfiguration (CreateVPCEConfiguration'),
    newCreateVPCEConfiguration,
    CreateVPCEConfigurationResponse (CreateVPCEConfigurationResponse'),
    newCreateVPCEConfigurationResponse,

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

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

    -- ** ListDevices (Paginated)
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

    -- ** StopRun
    StopRun (StopRun'),
    newStopRun,
    StopRunResponse (StopRunResponse'),
    newStopRunResponse,

    -- ** CreateNetworkProfile
    CreateNetworkProfile (CreateNetworkProfile'),
    newCreateNetworkProfile,
    CreateNetworkProfileResponse (CreateNetworkProfileResponse'),
    newCreateNetworkProfileResponse,

    -- ** GetJob
    GetJob (GetJob'),
    newGetJob,
    GetJobResponse (GetJobResponse'),
    newGetJobResponse,

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

    -- ** StopRemoteAccessSession
    StopRemoteAccessSession (StopRemoteAccessSession'),
    newStopRemoteAccessSession,
    StopRemoteAccessSessionResponse (StopRemoteAccessSessionResponse'),
    newStopRemoteAccessSessionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListDeviceInstances (Paginated)
    ListDeviceInstances (ListDeviceInstances'),
    newListDeviceInstances,
    ListDeviceInstancesResponse (ListDeviceInstancesResponse'),
    newListDeviceInstancesResponse,

    -- ** UpdateDeviceInstance
    UpdateDeviceInstance (UpdateDeviceInstance'),
    newUpdateDeviceInstance,
    UpdateDeviceInstanceResponse (UpdateDeviceInstanceResponse'),
    newUpdateDeviceInstanceResponse,

    -- ** CreateDevicePool
    CreateDevicePool (CreateDevicePool'),
    newCreateDevicePool,
    CreateDevicePoolResponse (CreateDevicePoolResponse'),
    newCreateDevicePoolResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListRuns (Paginated)
    ListRuns (ListRuns'),
    newListRuns,
    ListRunsResponse (ListRunsResponse'),
    newListRunsResponse,

    -- ** GetUpload
    GetUpload (GetUpload'),
    newGetUpload,
    GetUploadResponse (GetUploadResponse'),
    newGetUploadResponse,

    -- ** GetTestGridProject
    GetTestGridProject (GetTestGridProject'),
    newGetTestGridProject,
    GetTestGridProjectResponse (GetTestGridProjectResponse'),
    newGetTestGridProjectResponse,

    -- ** ListDevicePools (Paginated)
    ListDevicePools (ListDevicePools'),
    newListDevicePools,
    ListDevicePoolsResponse (ListDevicePoolsResponse'),
    newListDevicePoolsResponse,

    -- ** GetOfferingStatus (Paginated)
    GetOfferingStatus (GetOfferingStatus'),
    newGetOfferingStatus,
    GetOfferingStatusResponse (GetOfferingStatusResponse'),
    newGetOfferingStatusResponse,

    -- ** DeleteUpload
    DeleteUpload (DeleteUpload'),
    newDeleteUpload,
    DeleteUploadResponse (DeleteUploadResponse'),
    newDeleteUploadResponse,

    -- ** ListUploads (Paginated)
    ListUploads (ListUploads'),
    newListUploads,
    ListUploadsResponse (ListUploadsResponse'),
    newListUploadsResponse,

    -- ** CreateRemoteAccessSession
    CreateRemoteAccessSession (CreateRemoteAccessSession'),
    newCreateRemoteAccessSession,
    CreateRemoteAccessSessionResponse (CreateRemoteAccessSessionResponse'),
    newCreateRemoteAccessSessionResponse,

    -- ** UpdateUpload
    UpdateUpload (UpdateUpload'),
    newUpdateUpload,
    UpdateUploadResponse (UpdateUploadResponse'),
    newUpdateUploadResponse,

    -- ** ListTestGridProjects
    ListTestGridProjects (ListTestGridProjects'),
    newListTestGridProjects,
    ListTestGridProjectsResponse (ListTestGridProjectsResponse'),
    newListTestGridProjectsResponse,

    -- ** ListArtifacts (Paginated)
    ListArtifacts (ListArtifacts'),
    newListArtifacts,
    ListArtifactsResponse (ListArtifactsResponse'),
    newListArtifactsResponse,

    -- ** ListRemoteAccessSessions (Paginated)
    ListRemoteAccessSessions (ListRemoteAccessSessions'),
    newListRemoteAccessSessions,
    ListRemoteAccessSessionsResponse (ListRemoteAccessSessionsResponse'),
    newListRemoteAccessSessionsResponse,

    -- ** GetRun
    GetRun (GetRun'),
    newGetRun,
    GetRunResponse (GetRunResponse'),
    newGetRunResponse,

    -- ** GetDeviceInstance
    GetDeviceInstance (GetDeviceInstance'),
    newGetDeviceInstance,
    GetDeviceInstanceResponse (GetDeviceInstanceResponse'),
    newGetDeviceInstanceResponse,

    -- ** ListSuites (Paginated)
    ListSuites (ListSuites'),
    newListSuites,
    ListSuitesResponse (ListSuitesResponse'),
    newListSuitesResponse,

    -- ** StopJob
    StopJob (StopJob'),
    newStopJob,
    StopJobResponse (StopJobResponse'),
    newStopJobResponse,

    -- ** ListTests (Paginated)
    ListTests (ListTests'),
    newListTests,
    ListTestsResponse (ListTestsResponse'),
    newListTestsResponse,

    -- ** DeleteRemoteAccessSession
    DeleteRemoteAccessSession (DeleteRemoteAccessSession'),
    newDeleteRemoteAccessSession,
    DeleteRemoteAccessSessionResponse (DeleteRemoteAccessSessionResponse'),
    newDeleteRemoteAccessSessionResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** GetInstanceProfile
    GetInstanceProfile (GetInstanceProfile'),
    newGetInstanceProfile,
    GetInstanceProfileResponse (GetInstanceProfileResponse'),
    newGetInstanceProfileResponse,

    -- ** UpdateProject
    UpdateProject (UpdateProject'),
    newUpdateProject,
    UpdateProjectResponse (UpdateProjectResponse'),
    newUpdateProjectResponse,

    -- ** ListTestGridSessions
    ListTestGridSessions (ListTestGridSessions'),
    newListTestGridSessions,
    ListTestGridSessionsResponse (ListTestGridSessionsResponse'),
    newListTestGridSessionsResponse,

    -- ** ListUniqueProblems (Paginated)
    ListUniqueProblems (ListUniqueProblems'),
    newListUniqueProblems,
    ListUniqueProblemsResponse (ListUniqueProblemsResponse'),
    newListUniqueProblemsResponse,

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

    -- ** ListVPCEConfigurations (Paginated)
    ListVPCEConfigurations (ListVPCEConfigurations'),
    newListVPCEConfigurations,
    ListVPCEConfigurationsResponse (ListVPCEConfigurationsResponse'),
    newListVPCEConfigurationsResponse,

    -- ** GetVPCEConfiguration
    GetVPCEConfiguration (GetVPCEConfiguration'),
    newGetVPCEConfiguration,
    GetVPCEConfigurationResponse (GetVPCEConfigurationResponse'),
    newGetVPCEConfigurationResponse,

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

    -- ** PurchaseOffering
    PurchaseOffering (PurchaseOffering'),
    newPurchaseOffering,
    PurchaseOfferingResponse (PurchaseOfferingResponse'),
    newPurchaseOfferingResponse,

    -- ** GetProject
    GetProject (GetProject'),
    newGetProject,
    GetProjectResponse (GetProjectResponse'),
    newGetProjectResponse,

    -- ** ListInstanceProfiles (Paginated)
    ListInstanceProfiles (ListInstanceProfiles'),
    newListInstanceProfiles,
    ListInstanceProfilesResponse (ListInstanceProfilesResponse'),
    newListInstanceProfilesResponse,

    -- ** GetNetworkProfile
    GetNetworkProfile (GetNetworkProfile'),
    newGetNetworkProfile,
    GetNetworkProfileResponse (GetNetworkProfileResponse'),
    newGetNetworkProfileResponse,

    -- ** UpdateInstanceProfile
    UpdateInstanceProfile (UpdateInstanceProfile'),
    newUpdateInstanceProfile,
    UpdateInstanceProfileResponse (UpdateInstanceProfileResponse'),
    newUpdateInstanceProfileResponse,

    -- ** DeleteInstanceProfile
    DeleteInstanceProfile (DeleteInstanceProfile'),
    newDeleteInstanceProfile,
    DeleteInstanceProfileResponse (DeleteInstanceProfileResponse'),
    newDeleteInstanceProfileResponse,

    -- ** RenewOffering
    RenewOffering (RenewOffering'),
    newRenewOffering,
    RenewOfferingResponse (RenewOfferingResponse'),
    newRenewOfferingResponse,

    -- ** GetRemoteAccessSession
    GetRemoteAccessSession (GetRemoteAccessSession'),
    newGetRemoteAccessSession,
    GetRemoteAccessSessionResponse (GetRemoteAccessSessionResponse'),
    newGetRemoteAccessSessionResponse,

    -- ** GetSuite
    GetSuite (GetSuite'),
    newGetSuite,
    GetSuiteResponse (GetSuiteResponse'),
    newGetSuiteResponse,

    -- ** DeleteRun
    DeleteRun (DeleteRun'),
    newDeleteRun,
    DeleteRunResponse (DeleteRunResponse'),
    newDeleteRunResponse,

    -- ** GetTest
    GetTest (GetTest'),
    newGetTest,
    GetTestResponse (GetTestResponse'),
    newGetTestResponse,

    -- ** DeleteDevicePool
    DeleteDevicePool (DeleteDevicePool'),
    newDeleteDevicePool,
    DeleteDevicePoolResponse (DeleteDevicePoolResponse'),
    newDeleteDevicePoolResponse,

    -- ** ListOfferingTransactions (Paginated)
    ListOfferingTransactions (ListOfferingTransactions'),
    newListOfferingTransactions,
    ListOfferingTransactionsResponse (ListOfferingTransactionsResponse'),
    newListOfferingTransactionsResponse,

    -- ** UpdateDevicePool
    UpdateDevicePool (UpdateDevicePool'),
    newUpdateDevicePool,
    UpdateDevicePoolResponse (UpdateDevicePoolResponse'),
    newUpdateDevicePoolResponse,

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

    -- ** DeleteTestGridProject
    DeleteTestGridProject (DeleteTestGridProject'),
    newDeleteTestGridProject,
    DeleteTestGridProjectResponse (DeleteTestGridProjectResponse'),
    newDeleteTestGridProjectResponse,

    -- ** ListOfferingPromotions (Paginated)
    ListOfferingPromotions (ListOfferingPromotions'),
    newListOfferingPromotions,
    ListOfferingPromotionsResponse (ListOfferingPromotionsResponse'),
    newListOfferingPromotionsResponse,

    -- ** GetDevicePool
    GetDevicePool (GetDevicePool'),
    newGetDevicePool,
    GetDevicePoolResponse (GetDevicePoolResponse'),
    newGetDevicePoolResponse,

    -- ** GetAccountSettings
    GetAccountSettings (GetAccountSettings'),
    newGetAccountSettings,
    GetAccountSettingsResponse (GetAccountSettingsResponse'),
    newGetAccountSettingsResponse,

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

import Network.AWS.DeviceFarm.CreateDevicePool
import Network.AWS.DeviceFarm.CreateInstanceProfile
import Network.AWS.DeviceFarm.CreateNetworkProfile
import Network.AWS.DeviceFarm.CreateProject
import Network.AWS.DeviceFarm.CreateRemoteAccessSession
import Network.AWS.DeviceFarm.CreateTestGridProject
import Network.AWS.DeviceFarm.CreateTestGridUrl
import Network.AWS.DeviceFarm.CreateUpload
import Network.AWS.DeviceFarm.CreateVPCEConfiguration
import Network.AWS.DeviceFarm.DeleteDevicePool
import Network.AWS.DeviceFarm.DeleteInstanceProfile
import Network.AWS.DeviceFarm.DeleteNetworkProfile
import Network.AWS.DeviceFarm.DeleteProject
import Network.AWS.DeviceFarm.DeleteRemoteAccessSession
import Network.AWS.DeviceFarm.DeleteRun
import Network.AWS.DeviceFarm.DeleteTestGridProject
import Network.AWS.DeviceFarm.DeleteUpload
import Network.AWS.DeviceFarm.DeleteVPCEConfiguration
import Network.AWS.DeviceFarm.GetAccountSettings
import Network.AWS.DeviceFarm.GetDevice
import Network.AWS.DeviceFarm.GetDeviceInstance
import Network.AWS.DeviceFarm.GetDevicePool
import Network.AWS.DeviceFarm.GetDevicePoolCompatibility
import Network.AWS.DeviceFarm.GetInstanceProfile
import Network.AWS.DeviceFarm.GetJob
import Network.AWS.DeviceFarm.GetNetworkProfile
import Network.AWS.DeviceFarm.GetOfferingStatus
import Network.AWS.DeviceFarm.GetProject
import Network.AWS.DeviceFarm.GetRemoteAccessSession
import Network.AWS.DeviceFarm.GetRun
import Network.AWS.DeviceFarm.GetSuite
import Network.AWS.DeviceFarm.GetTest
import Network.AWS.DeviceFarm.GetTestGridProject
import Network.AWS.DeviceFarm.GetTestGridSession
import Network.AWS.DeviceFarm.GetUpload
import Network.AWS.DeviceFarm.GetVPCEConfiguration
import Network.AWS.DeviceFarm.InstallToRemoteAccessSession
import Network.AWS.DeviceFarm.Lens
import Network.AWS.DeviceFarm.ListArtifacts
import Network.AWS.DeviceFarm.ListDeviceInstances
import Network.AWS.DeviceFarm.ListDevicePools
import Network.AWS.DeviceFarm.ListDevices
import Network.AWS.DeviceFarm.ListInstanceProfiles
import Network.AWS.DeviceFarm.ListJobs
import Network.AWS.DeviceFarm.ListNetworkProfiles
import Network.AWS.DeviceFarm.ListOfferingPromotions
import Network.AWS.DeviceFarm.ListOfferingTransactions
import Network.AWS.DeviceFarm.ListOfferings
import Network.AWS.DeviceFarm.ListProjects
import Network.AWS.DeviceFarm.ListRemoteAccessSessions
import Network.AWS.DeviceFarm.ListRuns
import Network.AWS.DeviceFarm.ListSamples
import Network.AWS.DeviceFarm.ListSuites
import Network.AWS.DeviceFarm.ListTagsForResource
import Network.AWS.DeviceFarm.ListTestGridProjects
import Network.AWS.DeviceFarm.ListTestGridSessionActions
import Network.AWS.DeviceFarm.ListTestGridSessionArtifacts
import Network.AWS.DeviceFarm.ListTestGridSessions
import Network.AWS.DeviceFarm.ListTests
import Network.AWS.DeviceFarm.ListUniqueProblems
import Network.AWS.DeviceFarm.ListUploads
import Network.AWS.DeviceFarm.ListVPCEConfigurations
import Network.AWS.DeviceFarm.PurchaseOffering
import Network.AWS.DeviceFarm.RenewOffering
import Network.AWS.DeviceFarm.ScheduleRun
import Network.AWS.DeviceFarm.StopJob
import Network.AWS.DeviceFarm.StopRemoteAccessSession
import Network.AWS.DeviceFarm.StopRun
import Network.AWS.DeviceFarm.TagResource
import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.UntagResource
import Network.AWS.DeviceFarm.UpdateDeviceInstance
import Network.AWS.DeviceFarm.UpdateDevicePool
import Network.AWS.DeviceFarm.UpdateInstanceProfile
import Network.AWS.DeviceFarm.UpdateNetworkProfile
import Network.AWS.DeviceFarm.UpdateProject
import Network.AWS.DeviceFarm.UpdateTestGridProject
import Network.AWS.DeviceFarm.UpdateUpload
import Network.AWS.DeviceFarm.UpdateVPCEConfiguration
import Network.AWS.DeviceFarm.Waiters

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
