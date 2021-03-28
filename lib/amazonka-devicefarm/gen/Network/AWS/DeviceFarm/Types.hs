-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _NotEligibleException
    , _CannotDeleteException
    , _IdempotencyException
    , _TooManyTagsException
    , _ArgumentException
    , _NotFoundException
    , _InternalServiceException
    , _TagPolicyException
    , _TagOperationException
    , _ServiceAccountException
    , _InvalidOperationException
    , _LimitExceededException

    -- * OfferingTransactionType
    , OfferingTransactionType (..)

    -- * AccountSettings
    , AccountSettings (..)
    , mkAccountSettings
    , asAwsAccountNumber
    , asDefaultJobTimeoutMinutes
    , asMaxJobTimeoutMinutes
    , asMaxSlots
    , asSkipAppResign
    , asTrialMinutes
    , asUnmeteredDevices
    , asUnmeteredRemoteAccessDevices

    -- * OfferingStatus
    , OfferingStatus (..)
    , mkOfferingStatus
    , osEffectiveOn
    , osOffering
    , osQuantity
    , osType

    -- * DevicePoolType
    , DevicePoolType (..)

    -- * ScheduleRunConfiguration
    , ScheduleRunConfiguration (..)
    , mkScheduleRunConfiguration
    , srcAuxiliaryApps
    , srcBillingMethod
    , srcCustomerArtifactPaths
    , srcExtraDataPackageArn
    , srcLocale
    , srcLocation
    , srcNetworkProfileArn
    , srcRadios
    , srcVpceConfigurationArns

    -- * TestGridSessionStatus
    , TestGridSessionStatus (..)

    -- * CustomerArtifactPaths
    , CustomerArtifactPaths (..)
    , mkCustomerArtifactPaths
    , capAndroidPaths
    , capDeviceHostPaths
    , capIosPaths

    -- * DeviceAttribute
    , DeviceAttribute (..)

    -- * ClientId
    , ClientId (..)

    -- * BillingMethod
    , BillingMethod (..)

    -- * PaginationToken
    , PaginationToken (..)

    -- * DevicePlatform
    , DevicePlatform (..)

    -- * RecurringChargeFrequency
    , RecurringChargeFrequency (..)

    -- * Counters
    , Counters (..)
    , mkCounters
    , cErrored
    , cFailed
    , cPassed
    , cSkipped
    , cStopped
    , cTotal
    , cWarned

    -- * Radios
    , Radios (..)
    , mkRadios
    , rBluetooth
    , rGps
    , rNfc
    , rWifi

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * CreateRemoteAccessSessionConfiguration
    , CreateRemoteAccessSessionConfiguration (..)
    , mkCreateRemoteAccessSessionConfiguration
    , crascBillingMethod
    , crascVpceConfigurationArns

    -- * ExecutionConfiguration
    , ExecutionConfiguration (..)
    , mkExecutionConfiguration
    , ecAccountsCleanup
    , ecAppPackagesCleanup
    , ecJobTimeoutMinutes
    , ecSkipAppResign
    , ecVideoCapture

    -- * CurrencyCode
    , CurrencyCode (..)

    -- * ProblemDetail
    , ProblemDetail (..)
    , mkProblemDetail
    , pdArn
    , pdName

    -- * VPCEServiceName
    , VPCEServiceName (..)

    -- * InstanceProfile
    , InstanceProfile (..)
    , mkInstanceProfile
    , ipArn
    , ipDescription
    , ipExcludeAppPackagesFromCleanup
    , ipName
    , ipPackageCleanup
    , ipRebootAfterUse

    -- * IncompatibilityMessage
    , IncompatibilityMessage (..)
    , mkIncompatibilityMessage
    , imMessage
    , imType

    -- * ScheduleRunTest
    , ScheduleRunTest (..)
    , mkScheduleRunTest
    , srtType
    , srtFilter
    , srtParameters
    , srtTestPackageArn
    , srtTestSpecArn

    -- * Location
    , Location (..)
    , mkLocation
    , lLatitude
    , lLongitude

    -- * ResourceName
    , ResourceName (..)

    -- * NetworkProfileType
    , NetworkProfileType (..)

    -- * Project
    , Project (..)
    , mkProject
    , pArn
    , pCreated
    , pDefaultJobTimeoutMinutes
    , pName

    -- * UniqueProblem
    , UniqueProblem (..)
    , mkUniqueProblem
    , upMessage
    , upProblems

    -- * VPCEConfiguration
    , VPCEConfiguration (..)
    , mkVPCEConfiguration
    , vpcecArn
    , vpcecServiceDnsName
    , vpcecVpceConfigurationDescription
    , vpcecVpceConfigurationName
    , vpcecVpceServiceName

    -- * Device
    , Device (..)
    , mkDevice
    , dArn
    , dAvailability
    , dCarrier
    , dCpu
    , dFleetName
    , dFleetType
    , dFormFactor
    , dHeapSize
    , dImage
    , dInstances
    , dManufacturer
    , dMemory
    , dModel
    , dModelId
    , dName
    , dOs
    , dPlatform
    , dRadio
    , dRemoteAccessEnabled
    , dRemoteDebugEnabled
    , dResolution

    -- * DeviceSelectionConfiguration
    , DeviceSelectionConfiguration (..)
    , mkDeviceSelectionConfiguration
    , dscFilters
    , dscMaxDevices

    -- * Offering
    , Offering (..)
    , mkOffering
    , oDescription
    , oId
    , oPlatform
    , oRecurringCharges
    , oType

    -- * TestGridSession
    , TestGridSession (..)
    , mkTestGridSession
    , tgsArn
    , tgsBillingMinutes
    , tgsCreated
    , tgsEnded
    , tgsSeleniumProperties
    , tgsStatus

    -- * TestGridProject
    , TestGridProject (..)
    , mkTestGridProject
    , tgpArn
    , tgpCreated
    , tgpDescription
    , tgpName

    -- * OfferingIdentifier
    , OfferingIdentifier (..)

    -- * UploadType
    , UploadType (..)

    -- * DeviceFormFactor
    , DeviceFormFactor (..)

    -- * URL
    , URL (..)

    -- * TestGridSessionArtifactCategory
    , TestGridSessionArtifactCategory (..)

    -- * RuleOperator
    , RuleOperator (..)

    -- * Resolution
    , Resolution (..)
    , mkResolution
    , rHeight
    , rWidth

    -- * ExecutionStatus
    , ExecutionStatus (..)

    -- * Artifact
    , Artifact (..)
    , mkArtifact
    , aArn
    , aExtension
    , aName
    , aType
    , aUrl

    -- * TestGridSessionAction
    , TestGridSessionAction (..)
    , mkTestGridSessionAction
    , tgsaAction
    , tgsaDuration
    , tgsaRequestMethod
    , tgsaStarted
    , tgsaStatusCode

    -- * Rule
    , Rule (..)
    , mkRule
    , rAttribute
    , rOperator
    , rValue

    -- * Test
    , Test (..)
    , mkTest
    , tArn
    , tCounters
    , tCreated
    , tDeviceMinutes
    , tMessage
    , tName
    , tResult
    , tStarted
    , tStatus
    , tStopped
    , tType

    -- * SampleType
    , SampleType (..)

    -- * ArtifactCategory
    , ArtifactCategory (..)

    -- * SshPublicKey
    , SshPublicKey (..)

    -- * TestGridSessionArtifact
    , TestGridSessionArtifact (..)
    , mkTestGridSessionArtifact
    , tgsaFilename
    , tgsaType
    , tgsaUrl

    -- * DeviceAvailability
    , DeviceAvailability (..)

    -- * Run
    , Run (..)
    , mkRun
    , rAppUpload
    , rArn
    , rBillingMethod
    , rCompletedJobs
    , rCounters
    , rCreated
    , rCustomerArtifactPaths
    , rDeviceMinutes
    , rDevicePoolArn
    , rDeviceSelectionResult
    , rEventCount
    , rJobTimeoutMinutes
    , rLocale
    , rLocation
    , rMessage
    , rName
    , rNetworkProfile
    , rParsingResultUrl
    , rPlatform
    , rRadios
    , rResult
    , rResultCode
    , rSeed
    , rSkipAppResign
    , rStarted
    , rStatus
    , rStopped
    , rTestSpecArn
    , rTotalJobs
    , rType
    , rWebUrl

    -- * DeviceFilterAttribute
    , DeviceFilterAttribute (..)

    -- * ResourceDescription
    , ResourceDescription (..)

    -- * DevicePool
    , DevicePool (..)
    , mkDevicePool
    , dpArn
    , dpDescription
    , dpMaxDevices
    , dpName
    , dpRules
    , dpType

    -- * OfferingTransaction
    , OfferingTransaction (..)
    , mkOfferingTransaction
    , otCost
    , otCreatedOn
    , otOfferingPromotionId
    , otOfferingStatus
    , otTransactionId

    -- * UploadStatus
    , UploadStatus (..)

    -- * TrialMinutes
    , TrialMinutes (..)
    , mkTrialMinutes
    , tmRemaining
    , tmTotal

    -- * Job
    , Job (..)
    , mkJob
    , jArn
    , jCounters
    , jCreated
    , jDevice
    , jDeviceMinutes
    , jInstanceArn
    , jMessage
    , jName
    , jResult
    , jStarted
    , jStatus
    , jStopped
    , jType
    , jVideoCapture
    , jVideoEndpoint

    -- * Name
    , Name (..)

    -- * DeviceMinutes
    , DeviceMinutes (..)
    , mkDeviceMinutes
    , dmMetered
    , dmTotal
    , dmUnmetered

    -- * InstanceStatus
    , InstanceStatus (..)

    -- * DeviceFarmArn
    , DeviceFarmArn (..)

    -- * NetworkProfile
    , NetworkProfile (..)
    , mkNetworkProfile
    , npArn
    , npDescription
    , npDownlinkBandwidthBits
    , npDownlinkDelayMs
    , npDownlinkJitterMs
    , npDownlinkLossPercent
    , npName
    , npType
    , npUplinkBandwidthBits
    , npUplinkDelayMs
    , npUplinkJitterMs
    , npUplinkLossPercent

    -- * Metadata
    , Metadata (..)

    -- * TagKey
    , TagKey (..)

    -- * DeviceFilter
    , DeviceFilter (..)
    , mkDeviceFilter
    , dfAttribute
    , dfOperator
    , dfValues

    -- * OfferingType
    , OfferingType (..)

    -- * OfferingPromotionIdentifier
    , OfferingPromotionIdentifier (..)

    -- * Filter
    , Filter (..)

    -- * MonetaryAmount
    , MonetaryAmount (..)
    , mkMonetaryAmount
    , maAmount
    , maCurrencyCode

    -- * RecurringCharge
    , RecurringCharge (..)
    , mkRecurringCharge
    , rcCost
    , rcFrequency

    -- * ExecutionResult
    , ExecutionResult (..)

    -- * OfferingPromotion
    , OfferingPromotion (..)
    , mkOfferingPromotion
    , opDescription
    , opId

    -- * Problem
    , Problem (..)
    , mkProblem
    , pDevice
    , pJob
    , pMessage
    , pResult
    , pRun
    , pSuite
    , pTest

    -- * UploadCategory
    , UploadCategory (..)

    -- * Message
    , Message (..)

    -- * AmazonResourceName
    , AmazonResourceName (..)

    -- * ServiceDnsName
    , ServiceDnsName (..)

    -- * DevicePoolCompatibilityResult
    , DevicePoolCompatibilityResult (..)
    , mkDevicePoolCompatibilityResult
    , dpcrCompatible
    , dpcrDevice
    , dpcrIncompatibilityMessages

    -- * Upload
    , Upload (..)
    , mkUpload
    , uArn
    , uCategory
    , uContentType
    , uCreated
    , uMessage
    , uMetadata
    , uName
    , uStatus
    , uType
    , uUrl

    -- * CPU
    , CPU (..)
    , mkCPU
    , cpuArchitecture
    , cpuClock
    , cpuFrequency

    -- * HostAddress
    , HostAddress (..)

    -- * ArtifactType
    , ArtifactType (..)

    -- * Suite
    , Suite (..)
    , mkSuite
    , sArn
    , sCounters
    , sCreated
    , sDeviceMinutes
    , sMessage
    , sName
    , sResult
    , sStarted
    , sStatus
    , sStopped
    , sType

    -- * TestType
    , TestType (..)

    -- * RemoteAccessSession
    , RemoteAccessSession (..)
    , mkRemoteAccessSession
    , rasArn
    , rasBillingMethod
    , rasClientId
    , rasCreated
    , rasDevice
    , rasDeviceMinutes
    , rasDeviceUdid
    , rasEndpoint
    , rasHostAddress
    , rasInstanceArn
    , rasInteractionMode
    , rasMessage
    , rasName
    , rasRemoteDebugEnabled
    , rasRemoteRecordAppArn
    , rasRemoteRecordEnabled
    , rasResult
    , rasSkipAppResign
    , rasStarted
    , rasStatus
    , rasStopped

    -- * Sample
    , Sample (..)
    , mkSample
    , sfArn
    , sfType
    , sfUrl

    -- * TestGridSessionArtifactType
    , TestGridSessionArtifactType (..)

    -- * ExecutionResultCode
    , ExecutionResultCode (..)

    -- * DeviceSelectionResult
    , DeviceSelectionResult (..)
    , mkDeviceSelectionResult
    , dsrFilters
    , dsrMatchedDevicesCount
    , dsrMaxDevices

    -- * DeviceInstance
    , DeviceInstance (..)
    , mkDeviceInstance
    , diArn
    , diDeviceArn
    , diInstanceProfile
    , diLabels
    , diStatus
    , diUdid

    -- * ContentType
    , ContentType (..)

    -- * InteractionMode
    , InteractionMode (..)

    -- * Arn
    , Arn (..)

    -- * NextToken
    , NextToken (..)

    -- * AwsAccountNumber
    , AwsAccountNumber (..)

    -- * Description
    , Description (..)

    -- * ExtraDataPackageArn
    , ExtraDataPackageArn (..)

    -- * NetworkProfileArn
    , NetworkProfileArn (..)

    -- * ProjectArn
    , ProjectArn (..)

    -- * SessionArn
    , SessionArn (..)

    -- * SessionId
    , SessionId (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * TestPackageArn
    , TestPackageArn (..)

    -- * TestSpecArn
    , TestSpecArn (..)

    -- * OfferingId
    , OfferingId (..)

    -- * VpceConfigurationName
    , VpceConfigurationName (..)

    -- * VpceConfigurationDescription
    , VpceConfigurationDescription (..)

    -- * AppArn
    , AppArn (..)

    -- * DevicePoolArn
    , DevicePoolArn (..)

    -- * Id
    , Id (..)

    -- * DeviceArn
    , DeviceArn (..)

    -- * InstanceArn
    , InstanceArn (..)

    -- * RemoteRecordAppArn
    , RemoteRecordAppArn (..)

    -- * ResourceARN
    , ResourceARN (..)

    -- * AppUpload
    , AppUpload (..)

    -- * OfferingPromotionId
    , OfferingPromotionId (..)

    -- * TransactionId
    , TransactionId (..)

    -- * RemoteAccessSessionArn
    , RemoteAccessSessionArn (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.DeviceFarm.Types.OfferingTransactionType
  
import Network.AWS.DeviceFarm.Types.AccountSettings
  
import Network.AWS.DeviceFarm.Types.OfferingStatus
  
import Network.AWS.DeviceFarm.Types.DevicePoolType
  
import Network.AWS.DeviceFarm.Types.ScheduleRunConfiguration
  
import Network.AWS.DeviceFarm.Types.TestGridSessionStatus
  
import Network.AWS.DeviceFarm.Types.CustomerArtifactPaths
  
import Network.AWS.DeviceFarm.Types.DeviceAttribute
  
import Network.AWS.DeviceFarm.Types.ClientId
  
import Network.AWS.DeviceFarm.Types.BillingMethod
  
import Network.AWS.DeviceFarm.Types.PaginationToken
  
import Network.AWS.DeviceFarm.Types.DevicePlatform
  
  
  
import Network.AWS.DeviceFarm.Types.RecurringChargeFrequency
  
import Network.AWS.DeviceFarm.Types.Counters
  
import Network.AWS.DeviceFarm.Types.Radios
  
import Network.AWS.DeviceFarm.Types.Tag
  
import Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
  
import Network.AWS.DeviceFarm.Types.ExecutionConfiguration
  
import Network.AWS.DeviceFarm.Types.CurrencyCode
  
import Network.AWS.DeviceFarm.Types.ProblemDetail
  
import Network.AWS.DeviceFarm.Types.VPCEServiceName
  
import Network.AWS.DeviceFarm.Types.InstanceProfile
  
import Network.AWS.DeviceFarm.Types.IncompatibilityMessage
  
import Network.AWS.DeviceFarm.Types.ScheduleRunTest
  
import Network.AWS.DeviceFarm.Types.Location
  
import Network.AWS.DeviceFarm.Types.ResourceName
  
import Network.AWS.DeviceFarm.Types.NetworkProfileType
  
import Network.AWS.DeviceFarm.Types.Project
  
  
import Network.AWS.DeviceFarm.Types.UniqueProblem
  
import Network.AWS.DeviceFarm.Types.VPCEConfiguration
  
import Network.AWS.DeviceFarm.Types.Device
  
import Network.AWS.DeviceFarm.Types.DeviceSelectionConfiguration
  
import Network.AWS.DeviceFarm.Types.Offering
  
import Network.AWS.DeviceFarm.Types.TestGridSession
  
import Network.AWS.DeviceFarm.Types.TestGridProject
  
import Network.AWS.DeviceFarm.Types.OfferingIdentifier
  
import Network.AWS.DeviceFarm.Types.UploadType
  
import Network.AWS.DeviceFarm.Types.DeviceFormFactor
  
import Network.AWS.DeviceFarm.Types.URL
  
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifactCategory
  
import Network.AWS.DeviceFarm.Types.RuleOperator
  
import Network.AWS.DeviceFarm.Types.Resolution
  
import Network.AWS.DeviceFarm.Types.ExecutionStatus
  
import Network.AWS.DeviceFarm.Types.Artifact
  
import Network.AWS.DeviceFarm.Types.TestGridSessionAction
  
import Network.AWS.DeviceFarm.Types.Rule
  
import Network.AWS.DeviceFarm.Types.Test
  
import Network.AWS.DeviceFarm.Types.SampleType
  
import Network.AWS.DeviceFarm.Types.ArtifactCategory
  
import Network.AWS.DeviceFarm.Types.SshPublicKey
  
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifact
  
  
import Network.AWS.DeviceFarm.Types.DeviceAvailability
  
import Network.AWS.DeviceFarm.Types.Run
  
import Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
  
import Network.AWS.DeviceFarm.Types.ResourceDescription
  
import Network.AWS.DeviceFarm.Types.DevicePool
  
  
import Network.AWS.DeviceFarm.Types.OfferingTransaction
  
  
import Network.AWS.DeviceFarm.Types.UploadStatus
  
import Network.AWS.DeviceFarm.Types.TrialMinutes
  
import Network.AWS.DeviceFarm.Types.Job
  
import Network.AWS.DeviceFarm.Types.Name
  
import Network.AWS.DeviceFarm.Types.DeviceMinutes
  
import Network.AWS.DeviceFarm.Types.InstanceStatus
  
import Network.AWS.DeviceFarm.Types.DeviceFarmArn
  
import Network.AWS.DeviceFarm.Types.NetworkProfile
  
import Network.AWS.DeviceFarm.Types.Metadata
  
  
  
import Network.AWS.DeviceFarm.Types.TagKey
  
import Network.AWS.DeviceFarm.Types.DeviceFilter
  
import Network.AWS.DeviceFarm.Types.OfferingType
  
import Network.AWS.DeviceFarm.Types.OfferingPromotionIdentifier
  
import Network.AWS.DeviceFarm.Types.Filter
  
  
import Network.AWS.DeviceFarm.Types.MonetaryAmount
  
import Network.AWS.DeviceFarm.Types.RecurringCharge
  
import Network.AWS.DeviceFarm.Types.ExecutionResult
  
import Network.AWS.DeviceFarm.Types.OfferingPromotion
  
import Network.AWS.DeviceFarm.Types.Problem
  
import Network.AWS.DeviceFarm.Types.UploadCategory
  
import Network.AWS.DeviceFarm.Types.Message
  
import Network.AWS.DeviceFarm.Types.AmazonResourceName
  
import Network.AWS.DeviceFarm.Types.ServiceDnsName
  
  
import Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult
  
import Network.AWS.DeviceFarm.Types.Upload
  
  
import Network.AWS.DeviceFarm.Types.CPU
  
import Network.AWS.DeviceFarm.Types.HostAddress
  
import Network.AWS.DeviceFarm.Types.ArtifactType
  
import Network.AWS.DeviceFarm.Types.Suite
  
import Network.AWS.DeviceFarm.Types.TestType
  
import Network.AWS.DeviceFarm.Types.RemoteAccessSession
  
import Network.AWS.DeviceFarm.Types.Sample
  
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
  
import Network.AWS.DeviceFarm.Types.ExecutionResultCode
  
import Network.AWS.DeviceFarm.Types.DeviceSelectionResult
  
import Network.AWS.DeviceFarm.Types.DeviceInstance
  
import Network.AWS.DeviceFarm.Types.ContentType
  
  
import Network.AWS.DeviceFarm.Types.InteractionMode
  
import Network.AWS.DeviceFarm.Types.Arn
  
import Network.AWS.DeviceFarm.Types.NextToken
  
import Network.AWS.DeviceFarm.Types.AwsAccountNumber
  
import Network.AWS.DeviceFarm.Types.Description
  
import Network.AWS.DeviceFarm.Types.ExtraDataPackageArn
  
import Network.AWS.DeviceFarm.Types.NetworkProfileArn
  
import Network.AWS.DeviceFarm.Types.ProjectArn
  
import Network.AWS.DeviceFarm.Types.SessionArn
  
import Network.AWS.DeviceFarm.Types.SessionId
  
import Network.AWS.DeviceFarm.Types.Key
  
import Network.AWS.DeviceFarm.Types.Value
  
import Network.AWS.DeviceFarm.Types.TestPackageArn
  
import Network.AWS.DeviceFarm.Types.TestSpecArn
  
import Network.AWS.DeviceFarm.Types.OfferingId
  
import Network.AWS.DeviceFarm.Types.VpceConfigurationName
  
import Network.AWS.DeviceFarm.Types.VpceConfigurationDescription
  
import Network.AWS.DeviceFarm.Types.AppArn
  
import Network.AWS.DeviceFarm.Types.DevicePoolArn
  
import Network.AWS.DeviceFarm.Types.Id
  
import Network.AWS.DeviceFarm.Types.DeviceArn
  
import Network.AWS.DeviceFarm.Types.InstanceArn
  
import Network.AWS.DeviceFarm.Types.RemoteRecordAppArn
  
import Network.AWS.DeviceFarm.Types.ResourceARN
  
import Network.AWS.DeviceFarm.Types.AppUpload
  
import Network.AWS.DeviceFarm.Types.OfferingPromotionId
  
import Network.AWS.DeviceFarm.Types.TransactionId
  
import Network.AWS.DeviceFarm.Types.RemoteAccessSessionArn
  

-- | API version @2015-06-23@ of the Amazon Device Farm SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "DeviceFarm",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "devicefarm",
                 Core._svcVersion = "2015-06-23", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "DeviceFarm",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | Exception gets thrown when a user is not eligible to perform the specified transaction.
_NotEligibleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotEligibleException
  = Core._MatchServiceError mkServiceConfig "NotEligibleException"
{-# INLINEABLE _NotEligibleException #-}
{-# DEPRECATED _NotEligibleException "Use generic-lens or generic-optics instead"  #-}

-- | The requested object could not be deleted.
_CannotDeleteException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CannotDeleteException
  = Core._MatchServiceError mkServiceConfig "CannotDeleteException"
{-# INLINEABLE _CannotDeleteException #-}
{-# DEPRECATED _CannotDeleteException "Use generic-lens or generic-optics instead"  #-}

-- | An entity with the same name already exists.
_IdempotencyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IdempotencyException
  = Core._MatchServiceError mkServiceConfig "IdempotencyException"
{-# INLINEABLE _IdempotencyException #-}
{-# DEPRECATED _IdempotencyException "Use generic-lens or generic-optics instead"  #-}

-- | The list of tags on the repository is over the limit. The maximum number of tags that can be applied to a repository is 50. 
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | An invalid argument was specified.
_ArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ArgumentException
  = Core._MatchServiceError mkServiceConfig "ArgumentException"
{-# INLINEABLE _ArgumentException #-}
{-# DEPRECATED _ArgumentException "Use generic-lens or generic-optics instead"  #-}

-- | The specified entity was not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException
  = Core._MatchServiceError mkServiceConfig "NotFoundException"
{-# INLINEABLE _NotFoundException #-}
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | An internal exception was raised in the service. Contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> if you see this error. 
_InternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceException
  = Core._MatchServiceError mkServiceConfig
      "InternalServiceException"
{-# INLINEABLE _InternalServiceException #-}
{-# DEPRECATED _InternalServiceException "Use generic-lens or generic-optics instead"  #-}

-- | The request doesn't comply with the AWS Identity and Access Management (IAM) tag policy. Correct your request and then retry it.
_TagPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagPolicyException
  = Core._MatchServiceError mkServiceConfig "TagPolicyException"
{-# INLINEABLE _TagPolicyException #-}
{-# DEPRECATED _TagPolicyException "Use generic-lens or generic-optics instead"  #-}

-- | The operation was not successful. Try again.
_TagOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagOperationException
  = Core._MatchServiceError mkServiceConfig "TagOperationException"
{-# INLINEABLE _TagOperationException #-}
{-# DEPRECATED _TagOperationException "Use generic-lens or generic-optics instead"  #-}

-- | There was a problem with the service account.
_ServiceAccountException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceAccountException
  = Core._MatchServiceError mkServiceConfig "ServiceAccountException"
{-# INLINEABLE _ServiceAccountException #-}
{-# DEPRECATED _ServiceAccountException "Use generic-lens or generic-optics instead"  #-}

-- | There was an error with the update request, or you do not have sufficient permissions to update this VPC endpoint configuration.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidOperationException"
{-# INLINEABLE _InvalidOperationException #-}
{-# DEPRECATED _InvalidOperationException "Use generic-lens or generic-optics instead"  #-}

-- | A limit was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
