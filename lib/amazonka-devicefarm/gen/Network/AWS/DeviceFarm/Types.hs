{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types
  ( -- * Service Configuration
    deviceFarm,

    -- * Errors

    -- * ArtifactCategory
    ArtifactCategory (..),

    -- * ArtifactType
    ArtifactType (..),

    -- * BillingMethod
    BillingMethod (..),

    -- * CurrencyCode
    CurrencyCode (..),

    -- * DeviceAttribute
    DeviceAttribute (..),

    -- * DeviceAvailability
    DeviceAvailability (..),

    -- * DeviceFilterAttribute
    DeviceFilterAttribute (..),

    -- * DeviceFormFactor
    DeviceFormFactor (..),

    -- * DevicePlatform
    DevicePlatform (..),

    -- * DevicePoolType
    DevicePoolType (..),

    -- * ExecutionResult
    ExecutionResult (..),

    -- * ExecutionResultCode
    ExecutionResultCode (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * InstanceStatus
    InstanceStatus (..),

    -- * InteractionMode
    InteractionMode (..),

    -- * NetworkProfileType
    NetworkProfileType (..),

    -- * OfferingTransactionType
    OfferingTransactionType (..),

    -- * OfferingType
    OfferingType (..),

    -- * RecurringChargeFrequency
    RecurringChargeFrequency (..),

    -- * RuleOperator
    RuleOperator (..),

    -- * SampleType
    SampleType (..),

    -- * TestGridSessionArtifactCategory
    TestGridSessionArtifactCategory (..),

    -- * TestGridSessionArtifactType
    TestGridSessionArtifactType (..),

    -- * TestGridSessionStatus
    TestGridSessionStatus (..),

    -- * TestType
    TestType (..),

    -- * UploadCategory
    UploadCategory (..),

    -- * UploadStatus
    UploadStatus (..),

    -- * UploadType
    UploadType (..),

    -- * AccountSettings
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

    -- * Artifact
    Artifact,
    artifact,
    aArn,
    aUrl,
    aExtension,
    aName,
    aType,

    -- * CPU
    CPU,
    cpu,
    cpuFrequency,
    cpuClock,
    cpuArchitecture,

    -- * Counters
    Counters,
    counters,
    cPassed,
    cSkipped,
    cWarned,
    cStopped,
    cTotal,
    cFailed,
    cErrored,

    -- * CreateRemoteAccessSessionConfiguration
    CreateRemoteAccessSessionConfiguration,
    createRemoteAccessSessionConfiguration,
    crascBillingMethod,
    crascVpceConfigurationARNs,

    -- * CustomerArtifactPaths
    CustomerArtifactPaths,
    customerArtifactPaths,
    capAndroidPaths,
    capDeviceHostPaths,
    capIosPaths,

    -- * Device
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

    -- * DeviceFilter
    DeviceFilter,
    deviceFilter,
    dfAttribute,
    dfOperator,
    dfValues,

    -- * DeviceInstance
    DeviceInstance,
    deviceInstance,
    diStatus,
    diUdid,
    diInstanceProfile,
    diArn,
    diDeviceARN,
    diLabels,

    -- * DeviceMinutes
    DeviceMinutes,
    deviceMinutes,
    dmMetered,
    dmTotal,
    dmUnmetered,

    -- * DevicePool
    DevicePool,
    devicePool,
    devArn,
    devRules,
    devName,
    devMaxDevices,
    devType,
    devDescription,

    -- * DevicePoolCompatibilityResult
    DevicePoolCompatibilityResult,
    devicePoolCompatibilityResult,
    dpcrDevice,
    dpcrCompatible,
    dpcrIncompatibilityMessages,

    -- * DeviceSelectionConfiguration
    DeviceSelectionConfiguration,
    deviceSelectionConfiguration,
    dscFilters,
    dscMaxDevices,

    -- * DeviceSelectionResult
    DeviceSelectionResult,
    deviceSelectionResult,
    dsrMatchedDevicesCount,
    dsrFilters,
    dsrMaxDevices,

    -- * ExecutionConfiguration
    ExecutionConfiguration,
    executionConfiguration,
    ecSkipAppResign,
    ecAccountsCleanup,
    ecAppPackagesCleanup,
    ecJobTimeoutMinutes,
    ecVideoCapture,

    -- * IncompatibilityMessage
    IncompatibilityMessage,
    incompatibilityMessage,
    imType,
    imMessage,

    -- * InstanceProfile
    InstanceProfile,
    instanceProfile,
    ipArn,
    ipRebootAfterUse,
    ipName,
    ipPackageCleanup,
    ipExcludeAppPackagesFromCleanup,
    ipDescription,

    -- * Job
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

    -- * Location
    Location,
    location,
    lLatitude,
    lLongitude,

    -- * MonetaryAmount
    MonetaryAmount,
    monetaryAmount,
    maAmount,
    maCurrencyCode,

    -- * NetworkProfile
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

    -- * Offering
    Offering,
    offering,
    oPlatform,
    oId,
    oRecurringCharges,
    oType,
    oDescription,

    -- * OfferingPromotion
    OfferingPromotion,
    offeringPromotion,
    opId,
    opDescription,

    -- * OfferingStatus
    OfferingStatus,
    offeringStatus,
    osEffectiveOn,
    osOffering,
    osQuantity,
    osType,

    -- * OfferingTransaction
    OfferingTransaction,
    offeringTransaction,
    otOfferingStatus,
    otCost,
    otTransactionId,
    otOfferingPromotionId,
    otCreatedOn,

    -- * Problem
    Problem,
    problem,
    pDevice,
    pTest,
    pResult,
    pRun,
    pJob,
    pMessage,
    pSuite,

    -- * ProblemDetail
    ProblemDetail,
    problemDetail,
    pdArn,
    pdName,

    -- * Project
    Project,
    project,
    pArn,
    pCreated,
    pName,
    pDefaultJobTimeoutMinutes,

    -- * Radios
    Radios,
    radios,
    rNfc,
    rGps,
    rBluetooth,
    rWifi,

    -- * RecurringCharge
    RecurringCharge,
    recurringCharge,
    rcFrequency,
    rcCost,

    -- * RemoteAccessSession
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

    -- * Resolution
    Resolution,
    resolution,
    rHeight,
    rWidth,

    -- * Rule
    Rule,
    rule,
    rAttribute,
    rOperator,
    rValue,

    -- * Run
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

    -- * Sample
    Sample,
    sample,
    samArn,
    samUrl,
    samType,

    -- * ScheduleRunConfiguration
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

    -- * ScheduleRunTest
    ScheduleRunTest,
    scheduleRunTest,
    srtTestSpecARN,
    srtTestPackageARN,
    srtParameters,
    srtFilter,
    srtType,

    -- * Suite
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

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * Test
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

    -- * TestGridProject
    TestGridProject,
    testGridProject,
    tgpArn,
    tgpCreated,
    tgpName,
    tgpDescription,

    -- * TestGridSession
    TestGridSession,
    testGridSession,
    tgsStatus,
    tgsArn,
    tgsCreated,
    tgsBillingMinutes,
    tgsEnded,
    tgsSeleniumProperties,

    -- * TestGridSessionAction
    TestGridSessionAction,
    testGridSessionAction,
    tgsaAction,
    tgsaDuration,
    tgsaRequestMethod,
    tgsaStarted,
    tgsaStatusCode,

    -- * TestGridSessionArtifact
    TestGridSessionArtifact,
    testGridSessionArtifact,
    tgsaUrl,
    tgsaType,
    tgsaFilename,

    -- * TrialMinutes
    TrialMinutes,
    trialMinutes,
    tmRemaining,
    tmTotal,

    -- * UniqueProblem
    UniqueProblem,
    uniqueProblem,
    upProblems,
    upMessage,

    -- * Upload
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

    -- * VPCEConfiguration
    VPCEConfiguration,
    vpcEConfiguration,
    vecVpceServiceName,
    vecArn,
    vecVpceConfigurationName,
    vecServiceDNSName,
    vecVpceConfigurationDescription,
  )
where

import Network.AWS.DeviceFarm.Types.AccountSettings
import Network.AWS.DeviceFarm.Types.Artifact
import Network.AWS.DeviceFarm.Types.ArtifactCategory
import Network.AWS.DeviceFarm.Types.ArtifactType
import Network.AWS.DeviceFarm.Types.BillingMethod
import Network.AWS.DeviceFarm.Types.CPU
import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
import Network.AWS.DeviceFarm.Types.CurrencyCode
import Network.AWS.DeviceFarm.Types.CustomerArtifactPaths
import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.DeviceAttribute
import Network.AWS.DeviceFarm.Types.DeviceAvailability
import Network.AWS.DeviceFarm.Types.DeviceFilter
import Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
import Network.AWS.DeviceFarm.Types.DeviceFormFactor
import Network.AWS.DeviceFarm.Types.DeviceInstance
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.DevicePlatform
import Network.AWS.DeviceFarm.Types.DevicePool
import Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult
import Network.AWS.DeviceFarm.Types.DevicePoolType
import Network.AWS.DeviceFarm.Types.DeviceSelectionConfiguration
import Network.AWS.DeviceFarm.Types.DeviceSelectionResult
import Network.AWS.DeviceFarm.Types.ExecutionConfiguration
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionResultCode
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.IncompatibilityMessage
import Network.AWS.DeviceFarm.Types.InstanceProfile
import Network.AWS.DeviceFarm.Types.InstanceStatus
import Network.AWS.DeviceFarm.Types.InteractionMode
import Network.AWS.DeviceFarm.Types.Job
import Network.AWS.DeviceFarm.Types.Location
import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.NetworkProfile
import Network.AWS.DeviceFarm.Types.NetworkProfileType
import Network.AWS.DeviceFarm.Types.Offering
import Network.AWS.DeviceFarm.Types.OfferingPromotion
import Network.AWS.DeviceFarm.Types.OfferingStatus
import Network.AWS.DeviceFarm.Types.OfferingTransaction
import Network.AWS.DeviceFarm.Types.OfferingTransactionType
import Network.AWS.DeviceFarm.Types.OfferingType
import Network.AWS.DeviceFarm.Types.Problem
import Network.AWS.DeviceFarm.Types.ProblemDetail
import Network.AWS.DeviceFarm.Types.Project
import Network.AWS.DeviceFarm.Types.Radios
import Network.AWS.DeviceFarm.Types.RecurringCharge
import Network.AWS.DeviceFarm.Types.RecurringChargeFrequency
import Network.AWS.DeviceFarm.Types.RemoteAccessSession
import Network.AWS.DeviceFarm.Types.Resolution
import Network.AWS.DeviceFarm.Types.Rule
import Network.AWS.DeviceFarm.Types.RuleOperator
import Network.AWS.DeviceFarm.Types.Run
import Network.AWS.DeviceFarm.Types.Sample
import Network.AWS.DeviceFarm.Types.SampleType
import Network.AWS.DeviceFarm.Types.ScheduleRunConfiguration
import Network.AWS.DeviceFarm.Types.ScheduleRunTest
import Network.AWS.DeviceFarm.Types.Suite
import Network.AWS.DeviceFarm.Types.Tag
import Network.AWS.DeviceFarm.Types.Test
import Network.AWS.DeviceFarm.Types.TestGridProject
import Network.AWS.DeviceFarm.Types.TestGridSession
import Network.AWS.DeviceFarm.Types.TestGridSessionAction
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifact
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifactCategory
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
import Network.AWS.DeviceFarm.Types.TestGridSessionStatus
import Network.AWS.DeviceFarm.Types.TestType
import Network.AWS.DeviceFarm.Types.TrialMinutes
import Network.AWS.DeviceFarm.Types.UniqueProblem
import Network.AWS.DeviceFarm.Types.Upload
import Network.AWS.DeviceFarm.Types.UploadCategory
import Network.AWS.DeviceFarm.Types.UploadStatus
import Network.AWS.DeviceFarm.Types.UploadType
import Network.AWS.DeviceFarm.Types.VPCEConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-06-23@ of the Amazon Device Farm SDK configuration.
deviceFarm :: Service
deviceFarm =
  Service
    { _svcAbbrev = "DeviceFarm",
      _svcSigner = v4,
      _svcPrefix = "devicefarm",
      _svcVersion = "2015-06-23",
      _svcEndpoint = defaultEndpoint deviceFarm,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "DeviceFarm",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
