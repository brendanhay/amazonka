-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types
  ( -- * Service configuration
    deviceFarmService,

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
    AccountSettings (..),
    mkAccountSettings,
    asSkipAppResign,
    asAwsAccountNumber,
    asMaxJobTimeoutMinutes,
    asMaxSlots,
    asTrialMinutes,
    asUnmeteredDevices,
    asUnmeteredRemoteAccessDevices,
    asDefaultJobTimeoutMinutes,

    -- * Artifact
    Artifact (..),
    mkArtifact,
    aArn,
    aUrl,
    aExtension,
    aName,
    aType,

    -- * CPU
    CPU (..),
    mkCPU,
    cpuFrequency,
    cpuClock,
    cpuArchitecture,

    -- * Counters
    Counters (..),
    mkCounters,
    cPassed,
    cSkipped,
    cWarned,
    cStopped,
    cTotal,
    cFailed,
    cErrored,

    -- * CreateRemoteAccessSessionConfiguration
    CreateRemoteAccessSessionConfiguration (..),
    mkCreateRemoteAccessSessionConfiguration,
    crascBillingMethod,
    crascVpceConfigurationARNs,

    -- * CustomerArtifactPaths
    CustomerArtifactPaths (..),
    mkCustomerArtifactPaths,
    capAndroidPaths,
    capDeviceHostPaths,
    capIosPaths,

    -- * Device
    Device (..),
    mkDevice,
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
    DeviceFilter (..),
    mkDeviceFilter,
    dfAttribute,
    dfOperator,
    dfValues,

    -- * DeviceInstance
    DeviceInstance (..),
    mkDeviceInstance,
    diStatus,
    diUdid,
    diInstanceProfile,
    diArn,
    diDeviceARN,
    diLabels,

    -- * DeviceMinutes
    DeviceMinutes (..),
    mkDeviceMinutes,
    dmMetered,
    dmTotal,
    dmUnmetered,

    -- * DevicePool
    DevicePool (..),
    mkDevicePool,
    dpArn,
    dpRules,
    dpName,
    dpMaxDevices,
    dpType,
    dpDescription,

    -- * DevicePoolCompatibilityResult
    DevicePoolCompatibilityResult (..),
    mkDevicePoolCompatibilityResult,
    dpcrDevice,
    dpcrCompatible,
    dpcrIncompatibilityMessages,

    -- * DeviceSelectionConfiguration
    DeviceSelectionConfiguration (..),
    mkDeviceSelectionConfiguration,
    dscFilters,
    dscMaxDevices,

    -- * DeviceSelectionResult
    DeviceSelectionResult (..),
    mkDeviceSelectionResult,
    dsrMatchedDevicesCount,
    dsrFilters,
    dsrMaxDevices,

    -- * ExecutionConfiguration
    ExecutionConfiguration (..),
    mkExecutionConfiguration,
    ecSkipAppResign,
    ecAccountsCleanup,
    ecAppPackagesCleanup,
    ecJobTimeoutMinutes,
    ecVideoCapture,

    -- * IncompatibilityMessage
    IncompatibilityMessage (..),
    mkIncompatibilityMessage,
    imType,
    imMessage,

    -- * InstanceProfile
    InstanceProfile (..),
    mkInstanceProfile,
    ipArn,
    ipRebootAfterUse,
    ipName,
    ipPackageCleanup,
    ipExcludeAppPackagesFromCleanup,
    ipDescription,

    -- * Job
    Job (..),
    mkJob,
    jInstanceARN,
    jStatus,
    jCounters,
    jArn,
    jCreated,
    jDevice,
    jStopped,
    jResult,
    jName,
    jVideoEndpoint,
    jDeviceMinutes,
    jVideoCapture,
    jType,
    jMessage,
    jStarted,

    -- * Location
    Location (..),
    mkLocation,
    lLatitude,
    lLongitude,

    -- * MonetaryAmount
    MonetaryAmount (..),
    mkMonetaryAmount,
    maAmount,
    maCurrencyCode,

    -- * NetworkProfile
    NetworkProfile (..),
    mkNetworkProfile,
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
    Offering (..),
    mkOffering,
    oPlatform,
    oId,
    oRecurringCharges,
    oType,
    oDescription,

    -- * OfferingPromotion
    OfferingPromotion (..),
    mkOfferingPromotion,
    opId,
    opDescription,

    -- * OfferingStatus
    OfferingStatus (..),
    mkOfferingStatus,
    osEffectiveOn,
    osOffering,
    osQuantity,
    osType,

    -- * OfferingTransaction
    OfferingTransaction (..),
    mkOfferingTransaction,
    otOfferingStatus,
    otCost,
    otTransactionId,
    otOfferingPromotionId,
    otCreatedOn,

    -- * Problem
    Problem (..),
    mkProblem,
    pDevice,
    pTest,
    pResult,
    pRun,
    pJob,
    pMessage,
    pSuite,

    -- * ProblemDetail
    ProblemDetail (..),
    mkProblemDetail,
    pdArn,
    pdName,

    -- * Project
    Project (..),
    mkProject,
    pArn,
    pCreated,
    pName,
    pDefaultJobTimeoutMinutes,

    -- * Radios
    Radios (..),
    mkRadios,
    rNfc,
    rGps,
    rBluetooth,
    rWifi,

    -- * RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcFrequency,
    rcCost,

    -- * RemoteAccessSession
    RemoteAccessSession (..),
    mkRemoteAccessSession,
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
    Resolution (..),
    mkResolution,
    rHeight,
    rWidth,

    -- * Rule
    Rule (..),
    mkRule,
    rAttribute,
    rOperator,
    rValue,

    -- * Run
    Run (..),
    mkRun,
    rBillingMethod,
    rSkipAppResign,
    rStatus,
    rCustomerArtifactPaths,
    rEventCount,
    rCounters,
    rPlatform,
    rSeed,
    rRadios,
    rArn,
    rLocation,
    rCreated,
    rLocale,
    rTestSpecARN,
    rStopped,
    rResult,
    rJobTimeoutMinutes,
    rCompletedJobs,
    rResultCode,
    rName,
    rAppUpload,
    rParsingResultURL,
    rNetworkProfile,
    rDeviceMinutes,
    rType,
    rMessage,
    rWebURL,
    rTotalJobs,
    rDevicePoolARN,
    rStarted,
    rDeviceSelectionResult,

    -- * Sample
    Sample (..),
    mkSample,
    sfArn,
    sfUrl,
    sfType,

    -- * ScheduleRunConfiguration
    ScheduleRunConfiguration (..),
    mkScheduleRunConfiguration,
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
    ScheduleRunTest (..),
    mkScheduleRunTest,
    srtTestSpecARN,
    srtTestPackageARN,
    srtParameters,
    srtFilter,
    srtType,

    -- * Suite
    Suite (..),
    mkSuite,
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
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Test
    Test (..),
    mkTest,
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
    TestGridProject (..),
    mkTestGridProject,
    tgpArn,
    tgpCreated,
    tgpName,
    tgpDescription,

    -- * TestGridSession
    TestGridSession (..),
    mkTestGridSession,
    tgsStatus,
    tgsArn,
    tgsCreated,
    tgsBillingMinutes,
    tgsEnded,
    tgsSeleniumProperties,

    -- * TestGridSessionAction
    TestGridSessionAction (..),
    mkTestGridSessionAction,
    tgsaAction,
    tgsaDuration,
    tgsaRequestMethod,
    tgsaStarted,
    tgsaStatusCode,

    -- * TestGridSessionArtifact
    TestGridSessionArtifact (..),
    mkTestGridSessionArtifact,
    tgsaUrl,
    tgsaType,
    tgsaFilename,

    -- * TrialMinutes
    TrialMinutes (..),
    mkTrialMinutes,
    tmRemaining,
    tmTotal,

    -- * UniqueProblem
    UniqueProblem (..),
    mkUniqueProblem,
    upProblems,
    upMessage,

    -- * Upload
    Upload (..),
    mkUpload,
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
    VPCEConfiguration (..),
    mkVPCEConfiguration,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-06-23@ of the Amazon Device Farm SDK configuration.
deviceFarmService :: Lude.Service
deviceFarmService =
  Lude.Service
    { Lude._svcAbbrev = "DeviceFarm",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "devicefarm",
      Lude._svcVersion = "2015-06-23",
      Lude._svcEndpoint = Lude.defaultEndpoint deviceFarmService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "DeviceFarm",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
