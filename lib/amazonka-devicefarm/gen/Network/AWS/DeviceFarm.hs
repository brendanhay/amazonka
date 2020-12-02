{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Welcome to the AWS Device Farm API documentation, which contains APIs for:
--
--
--     * Testing on desktop browsers
--
-- Device Farm makes it possible for you to test your web applications on desktop browsers using Selenium. The APIs for desktop browser testing contain @TestGrid@ in their names. For more information, see <https://docs.aws.amazon.com/devicefarm/latest/testgrid/ Testing Web Applications on Selenium with Device Farm> .
--
--     * Testing on real mobile devices
--
-- Device Farm makes it possible for you to test apps on physical phones, tablets, and other devices in the cloud. For more information, see the <https://docs.aws.amazon.com/devicefarm/latest/developerguide/ Device Farm Developer Guide> .
module Network.AWS.DeviceFarm
  ( -- * Service Configuration
    deviceFarm,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListProjects (Paginated)
    module Network.AWS.DeviceFarm.ListProjects,

    -- ** DeleteProject
    module Network.AWS.DeviceFarm.DeleteProject,

    -- ** UpdateProject
    module Network.AWS.DeviceFarm.UpdateProject,

    -- ** UpdateNetworkProfile
    module Network.AWS.DeviceFarm.UpdateNetworkProfile,

    -- ** DeleteNetworkProfile
    module Network.AWS.DeviceFarm.DeleteNetworkProfile,

    -- ** GetDevicePoolCompatibility
    module Network.AWS.DeviceFarm.GetDevicePoolCompatibility,

    -- ** InstallToRemoteAccessSession
    module Network.AWS.DeviceFarm.InstallToRemoteAccessSession,

    -- ** ListTests (Paginated)
    module Network.AWS.DeviceFarm.ListTests,

    -- ** ListArtifacts (Paginated)
    module Network.AWS.DeviceFarm.ListArtifacts,

    -- ** ListTestGridSessionActions
    module Network.AWS.DeviceFarm.ListTestGridSessionActions,

    -- ** CreateUpload
    module Network.AWS.DeviceFarm.CreateUpload,

    -- ** GetDeviceInstance
    module Network.AWS.DeviceFarm.GetDeviceInstance,

    -- ** StopJob
    module Network.AWS.DeviceFarm.StopJob,

    -- ** DeleteRemoteAccessSession
    module Network.AWS.DeviceFarm.DeleteRemoteAccessSession,

    -- ** ListTestGridSessionArtifacts
    module Network.AWS.DeviceFarm.ListTestGridSessionArtifacts,

    -- ** ListTestGridProjects
    module Network.AWS.DeviceFarm.ListTestGridProjects,

    -- ** DeleteUpload
    module Network.AWS.DeviceFarm.DeleteUpload,

    -- ** UpdateUpload
    module Network.AWS.DeviceFarm.UpdateUpload,

    -- ** DeleteTestGridProject
    module Network.AWS.DeviceFarm.DeleteTestGridProject,

    -- ** UpdateTestGridProject
    module Network.AWS.DeviceFarm.UpdateTestGridProject,

    -- ** ListTagsForResource
    module Network.AWS.DeviceFarm.ListTagsForResource,

    -- ** GetDevicePool
    module Network.AWS.DeviceFarm.GetDevicePool,

    -- ** ListDevicePools (Paginated)
    module Network.AWS.DeviceFarm.ListDevicePools,

    -- ** UpdateDevicePool
    module Network.AWS.DeviceFarm.UpdateDevicePool,

    -- ** DeleteDevicePool
    module Network.AWS.DeviceFarm.DeleteDevicePool,

    -- ** GetUpload
    module Network.AWS.DeviceFarm.GetUpload,

    -- ** ListOfferingTransactions (Paginated)
    module Network.AWS.DeviceFarm.ListOfferingTransactions,

    -- ** CreateDevicePool
    module Network.AWS.DeviceFarm.CreateDevicePool,

    -- ** DeleteRun
    module Network.AWS.DeviceFarm.DeleteRun,

    -- ** ListRuns (Paginated)
    module Network.AWS.DeviceFarm.ListRuns,

    -- ** GetTest
    module Network.AWS.DeviceFarm.GetTest,

    -- ** UpdateDeviceInstance
    module Network.AWS.DeviceFarm.UpdateDeviceInstance,

    -- ** GetNetworkProfile
    module Network.AWS.DeviceFarm.GetNetworkProfile,

    -- ** RenewOffering
    module Network.AWS.DeviceFarm.RenewOffering,

    -- ** DeleteInstanceProfile
    module Network.AWS.DeviceFarm.DeleteInstanceProfile,

    -- ** UpdateInstanceProfile
    module Network.AWS.DeviceFarm.UpdateInstanceProfile,

    -- ** CreateInstanceProfile
    module Network.AWS.DeviceFarm.CreateInstanceProfile,

    -- ** GetDevice
    module Network.AWS.DeviceFarm.GetDevice,

    -- ** ListJobs (Paginated)
    module Network.AWS.DeviceFarm.ListJobs,

    -- ** GetTestGridSession
    module Network.AWS.DeviceFarm.GetTestGridSession,

    -- ** GetVPCEConfiguration
    module Network.AWS.DeviceFarm.GetVPCEConfiguration,

    -- ** StopRemoteAccessSession
    module Network.AWS.DeviceFarm.StopRemoteAccessSession,

    -- ** CreateNetworkProfile
    module Network.AWS.DeviceFarm.CreateNetworkProfile,

    -- ** DeleteVPCEConfiguration
    module Network.AWS.DeviceFarm.DeleteVPCEConfiguration,

    -- ** UpdateVPCEConfiguration
    module Network.AWS.DeviceFarm.UpdateVPCEConfiguration,

    -- ** GetJob
    module Network.AWS.DeviceFarm.GetJob,

    -- ** GetInstanceProfile
    module Network.AWS.DeviceFarm.GetInstanceProfile,

    -- ** ListNetworkProfiles (Paginated)
    module Network.AWS.DeviceFarm.ListNetworkProfiles,

    -- ** CreateVPCEConfiguration
    module Network.AWS.DeviceFarm.CreateVPCEConfiguration,

    -- ** ScheduleRun
    module Network.AWS.DeviceFarm.ScheduleRun,

    -- ** CreateTestGridProject
    module Network.AWS.DeviceFarm.CreateTestGridProject,

    -- ** GetRun
    module Network.AWS.DeviceFarm.GetRun,

    -- ** ListSamples (Paginated)
    module Network.AWS.DeviceFarm.ListSamples,

    -- ** ListSuites (Paginated)
    module Network.AWS.DeviceFarm.ListSuites,

    -- ** ListRemoteAccessSessions (Paginated)
    module Network.AWS.DeviceFarm.ListRemoteAccessSessions,

    -- ** GetAccountSettings
    module Network.AWS.DeviceFarm.GetAccountSettings,

    -- ** CreateRemoteAccessSession
    module Network.AWS.DeviceFarm.CreateRemoteAccessSession,

    -- ** ListOfferingPromotions (Paginated)
    module Network.AWS.DeviceFarm.ListOfferingPromotions,

    -- ** GetOfferingStatus (Paginated)
    module Network.AWS.DeviceFarm.GetOfferingStatus,

    -- ** ListUploads (Paginated)
    module Network.AWS.DeviceFarm.ListUploads,

    -- ** GetTestGridProject
    module Network.AWS.DeviceFarm.GetTestGridProject,

    -- ** GetSuite
    module Network.AWS.DeviceFarm.GetSuite,

    -- ** TagResource
    module Network.AWS.DeviceFarm.TagResource,

    -- ** GetRemoteAccessSession
    module Network.AWS.DeviceFarm.GetRemoteAccessSession,

    -- ** ListDeviceInstances (Paginated)
    module Network.AWS.DeviceFarm.ListDeviceInstances,

    -- ** PurchaseOffering
    module Network.AWS.DeviceFarm.PurchaseOffering,

    -- ** ListInstanceProfiles (Paginated)
    module Network.AWS.DeviceFarm.ListInstanceProfiles,

    -- ** UntagResource
    module Network.AWS.DeviceFarm.UntagResource,

    -- ** GetProject
    module Network.AWS.DeviceFarm.GetProject,

    -- ** ListUniqueProblems (Paginated)
    module Network.AWS.DeviceFarm.ListUniqueProblems,

    -- ** ListVPCEConfigurations (Paginated)
    module Network.AWS.DeviceFarm.ListVPCEConfigurations,

    -- ** StopRun
    module Network.AWS.DeviceFarm.StopRun,

    -- ** ListDevices (Paginated)
    module Network.AWS.DeviceFarm.ListDevices,

    -- ** CreateProject
    module Network.AWS.DeviceFarm.CreateProject,

    -- ** ListTestGridSessions
    module Network.AWS.DeviceFarm.ListTestGridSessions,

    -- ** CreateTestGridURL
    module Network.AWS.DeviceFarm.CreateTestGridURL,

    -- ** ListOfferings (Paginated)
    module Network.AWS.DeviceFarm.ListOfferings,

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
    AccountSettings,
    accountSettings,
    asSkipAppResign,
    asAwsAccountNumber,
    asMaxJobTimeoutMinutes,
    asMaxSlots,
    asTrialMinutes,
    asUnmeteredDevices,
    asUnmeteredRemoteAccessDevices,
    asDefaultJobTimeoutMinutes,

    -- ** Artifact
    Artifact,
    artifact,
    aArn,
    aUrl,
    aExtension,
    aName,
    aType,

    -- ** CPU
    CPU,
    cpu,
    cpuFrequency,
    cpuClock,
    cpuArchitecture,

    -- ** Counters
    Counters,
    counters,
    cPassed,
    cSkipped,
    cWarned,
    cStopped,
    cTotal,
    cFailed,
    cErrored,

    -- ** CreateRemoteAccessSessionConfiguration
    CreateRemoteAccessSessionConfiguration,
    createRemoteAccessSessionConfiguration,
    crascBillingMethod,
    crascVpceConfigurationARNs,

    -- ** CustomerArtifactPaths
    CustomerArtifactPaths,
    customerArtifactPaths,
    capAndroidPaths,
    capDeviceHostPaths,
    capIosPaths,

    -- ** Device
    Device,
    device,
    dCarrier,
    dImage,
    dManufacturer,
    dPlatform,
    dModelId,
    dRemoteAccessEnabled,
    dArn,
    dFormFactor,
    dFleetType,
    dResolution,
    dAvailability,
    dMemory,
    dRadio,
    dOs,
    dName,
    dModel,
    dInstances,
    dRemoteDebugEnabled,
    dCpu,
    dHeapSize,
    dFleetName,

    -- ** DeviceFilter
    DeviceFilter,
    deviceFilter,
    dfAttribute,
    dfOperator,
    dfValues,

    -- ** DeviceInstance
    DeviceInstance,
    deviceInstance,
    diStatus,
    diUdid,
    diInstanceProfile,
    diArn,
    diDeviceARN,
    diLabels,

    -- ** DeviceMinutes
    DeviceMinutes,
    deviceMinutes,
    dmMetered,
    dmTotal,
    dmUnmetered,

    -- ** DevicePool
    DevicePool,
    devicePool,
    devArn,
    devRules,
    devName,
    devMaxDevices,
    devType,
    devDescription,

    -- ** DevicePoolCompatibilityResult
    DevicePoolCompatibilityResult,
    devicePoolCompatibilityResult,
    dpcrDevice,
    dpcrCompatible,
    dpcrIncompatibilityMessages,

    -- ** DeviceSelectionConfiguration
    DeviceSelectionConfiguration,
    deviceSelectionConfiguration,
    dscFilters,
    dscMaxDevices,

    -- ** DeviceSelectionResult
    DeviceSelectionResult,
    deviceSelectionResult,
    dsrMatchedDevicesCount,
    dsrFilters,
    dsrMaxDevices,

    -- ** ExecutionConfiguration
    ExecutionConfiguration,
    executionConfiguration,
    ecSkipAppResign,
    ecAccountsCleanup,
    ecAppPackagesCleanup,
    ecJobTimeoutMinutes,
    ecVideoCapture,

    -- ** IncompatibilityMessage
    IncompatibilityMessage,
    incompatibilityMessage,
    imType,
    imMessage,

    -- ** InstanceProfile
    InstanceProfile,
    instanceProfile,
    ipArn,
    ipRebootAfterUse,
    ipName,
    ipPackageCleanup,
    ipExcludeAppPackagesFromCleanup,
    ipDescription,

    -- ** Job
    Job,
    job,
    jobInstanceARN,
    jobStatus,
    jobCounters,
    jobArn,
    jobCreated,
    jobDevice,
    jobStopped,
    jobResult,
    jobName,
    jobVideoEndpoint,
    jobDeviceMinutes,
    jobVideoCapture,
    jobType,
    jobMessage,
    jobStarted,

    -- ** Location
    Location,
    location,
    lLatitude,
    lLongitude,

    -- ** MonetaryAmount
    MonetaryAmount,
    monetaryAmount,
    maAmount,
    maCurrencyCode,

    -- ** NetworkProfile
    NetworkProfile,
    networkProfile,
    npUplinkJitterMs,
    npArn,
    npUplinkLossPercent,
    npDownlinkJitterMs,
    npName,
    npDownlinkLossPercent,
    npType,
    npUplinkDelayMs,
    npUplinkBandwidthBits,
    npDescription,
    npDownlinkDelayMs,
    npDownlinkBandwidthBits,

    -- ** Offering
    Offering,
    offering,
    oPlatform,
    oId,
    oRecurringCharges,
    oType,
    oDescription,

    -- ** OfferingPromotion
    OfferingPromotion,
    offeringPromotion,
    opId,
    opDescription,

    -- ** OfferingStatus
    OfferingStatus,
    offeringStatus,
    osEffectiveOn,
    osOffering,
    osQuantity,
    osType,

    -- ** OfferingTransaction
    OfferingTransaction,
    offeringTransaction,
    otOfferingStatus,
    otCost,
    otTransactionId,
    otOfferingPromotionId,
    otCreatedOn,

    -- ** Problem
    Problem,
    problem,
    pDevice,
    pTest,
    pResult,
    pRun,
    pJob,
    pMessage,
    pSuite,

    -- ** ProblemDetail
    ProblemDetail,
    problemDetail,
    pdArn,
    pdName,

    -- ** Project
    Project,
    project,
    pArn,
    pCreated,
    pName,
    pDefaultJobTimeoutMinutes,

    -- ** Radios
    Radios,
    radios,
    rNfc,
    rGps,
    rBluetooth,
    rWifi,

    -- ** RecurringCharge
    RecurringCharge,
    recurringCharge,
    rcFrequency,
    rcCost,

    -- ** RemoteAccessSession
    RemoteAccessSession,
    remoteAccessSession,
    rasBillingMethod,
    rasClientId,
    rasDeviceUdid,
    rasSkipAppResign,
    rasInstanceARN,
    rasStatus,
    rasRemoteRecordEnabled,
    rasArn,
    rasRemoteRecordAppARN,
    rasCreated,
    rasDevice,
    rasStopped,
    rasResult,
    rasName,
    rasDeviceMinutes,
    rasRemoteDebugEnabled,
    rasEndpoint,
    rasMessage,
    rasHostAddress,
    rasInteractionMode,
    rasStarted,

    -- ** Resolution
    Resolution,
    resolution,
    rHeight,
    rWidth,

    -- ** Rule
    Rule,
    rule,
    rAttribute,
    rOperator,
    rValue,

    -- ** Run
    Run,
    run,
    runBillingMethod,
    runSkipAppResign,
    runStatus,
    runCustomerArtifactPaths,
    runEventCount,
    runCounters,
    runPlatform,
    runSeed,
    runRadios,
    runArn,
    runLocation,
    runCreated,
    runLocale,
    runTestSpecARN,
    runStopped,
    runResult,
    runJobTimeoutMinutes,
    runCompletedJobs,
    runResultCode,
    runName,
    runAppUpload,
    runParsingResultURL,
    runNetworkProfile,
    runDeviceMinutes,
    runType,
    runMessage,
    runWebURL,
    runTotalJobs,
    runDevicePoolARN,
    runStarted,
    runDeviceSelectionResult,

    -- ** Sample
    Sample,
    sample,
    samArn,
    samUrl,
    samType,

    -- ** ScheduleRunConfiguration
    ScheduleRunConfiguration,
    scheduleRunConfiguration,
    srcBillingMethod,
    srcCustomerArtifactPaths,
    srcRadios,
    srcLocation,
    srcLocale,
    srcNetworkProfileARN,
    srcExtraDataPackageARN,
    srcAuxiliaryApps,
    srcVpceConfigurationARNs,

    -- ** ScheduleRunTest
    ScheduleRunTest,
    scheduleRunTest,
    srtTestSpecARN,
    srtTestPackageARN,
    srtParameters,
    srtFilter,
    srtType,

    -- ** Suite
    Suite,
    suite,
    sStatus,
    sCounters,
    sArn,
    sCreated,
    sStopped,
    sResult,
    sName,
    sDeviceMinutes,
    sType,
    sMessage,
    sStarted,

    -- ** Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- ** Test
    Test,
    test,
    tStatus,
    tCounters,
    tArn,
    tCreated,
    tStopped,
    tResult,
    tName,
    tDeviceMinutes,
    tType,
    tMessage,
    tStarted,

    -- ** TestGridProject
    TestGridProject,
    testGridProject,
    tgpArn,
    tgpCreated,
    tgpName,
    tgpDescription,

    -- ** TestGridSession
    TestGridSession,
    testGridSession,
    tgsStatus,
    tgsArn,
    tgsCreated,
    tgsBillingMinutes,
    tgsEnded,
    tgsSeleniumProperties,

    -- ** TestGridSessionAction
    TestGridSessionAction,
    testGridSessionAction,
    tgsaAction,
    tgsaDuration,
    tgsaRequestMethod,
    tgsaStarted,
    tgsaStatusCode,

    -- ** TestGridSessionArtifact
    TestGridSessionArtifact,
    testGridSessionArtifact,
    tgsaUrl,
    tgsaType,
    tgsaFilename,

    -- ** TrialMinutes
    TrialMinutes,
    trialMinutes,
    tmRemaining,
    tmTotal,

    -- ** UniqueProblem
    UniqueProblem,
    uniqueProblem,
    upProblems,
    upMessage,

    -- ** Upload
    Upload,
    upload,
    uStatus,
    uArn,
    uCreated,
    uCategory,
    uUrl,
    uName,
    uMetadata,
    uType,
    uMessage,
    uContentType,

    -- ** VPCEConfiguration
    VPCEConfiguration,
    vpcEConfiguration,
    vecVpceServiceName,
    vecArn,
    vecVpceConfigurationName,
    vecServiceDNSName,
    vecVpceConfigurationDescription,
  )
where

import Network.AWS.DeviceFarm.CreateDevicePool
import Network.AWS.DeviceFarm.CreateInstanceProfile
import Network.AWS.DeviceFarm.CreateNetworkProfile
import Network.AWS.DeviceFarm.CreateProject
import Network.AWS.DeviceFarm.CreateRemoteAccessSession
import Network.AWS.DeviceFarm.CreateTestGridProject
import Network.AWS.DeviceFarm.CreateTestGridURL
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
