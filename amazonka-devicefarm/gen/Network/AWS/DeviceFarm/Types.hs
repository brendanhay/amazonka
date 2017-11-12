{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types
    (
    -- * Service Configuration
      deviceFarm

    -- * Errors
    , _NotEligibleException
    , _IdempotencyException
    , _ArgumentException
    , _NotFoundException
    , _ServiceAccountException
    , _LimitExceededException

    -- * ArtifactCategory
    , ArtifactCategory (..)

    -- * ArtifactType
    , ArtifactType (..)

    -- * BillingMethod
    , BillingMethod (..)

    -- * CurrencyCode
    , CurrencyCode (..)

    -- * DeviceAttribute
    , DeviceAttribute (..)

    -- * DeviceFormFactor
    , DeviceFormFactor (..)

    -- * DevicePlatform
    , DevicePlatform (..)

    -- * DevicePoolType
    , DevicePoolType (..)

    -- * ExecutionResult
    , ExecutionResult (..)

    -- * ExecutionResultCode
    , ExecutionResultCode (..)

    -- * ExecutionStatus
    , ExecutionStatus (..)

    -- * NetworkProfileType
    , NetworkProfileType (..)

    -- * OfferingTransactionType
    , OfferingTransactionType (..)

    -- * OfferingType
    , OfferingType (..)

    -- * RecurringChargeFrequency
    , RecurringChargeFrequency (..)

    -- * RuleOperator
    , RuleOperator (..)

    -- * SampleType
    , SampleType (..)

    -- * TestType
    , TestType (..)

    -- * UploadStatus
    , UploadStatus (..)

    -- * UploadType
    , UploadType (..)

    -- * AccountSettings
    , AccountSettings
    , accountSettings
    , asAwsAccountNumber
    , asMaxJobTimeoutMinutes
    , asMaxSlots
    , asTrialMinutes
    , asUnmeteredDevices
    , asUnmeteredRemoteAccessDevices
    , asDefaultJobTimeoutMinutes

    -- * Artifact
    , Artifact
    , artifact
    , aArn
    , aUrl
    , aExtension
    , aName
    , aType

    -- * CPU
    , CPU
    , cpu
    , cpuFrequency
    , cpuClock
    , cpuArchitecture

    -- * Counters
    , Counters
    , counters
    , cPassed
    , cSkipped
    , cWarned
    , cStopped
    , cTotal
    , cFailed
    , cErrored

    -- * CreateRemoteAccessSessionConfiguration
    , CreateRemoteAccessSessionConfiguration
    , createRemoteAccessSessionConfiguration
    , crascBillingMethod

    -- * CustomerArtifactPaths
    , CustomerArtifactPaths
    , customerArtifactPaths
    , capAndroidPaths
    , capDeviceHostPaths
    , capIosPaths

    -- * Device
    , Device
    , device
    , devCarrier
    , devImage
    , devManufacturer
    , devPlatform
    , devRemoteAccessEnabled
    , devArn
    , devFormFactor
    , devFleetType
    , devResolution
    , devMemory
    , devRadio
    , devOs
    , devName
    , devModel
    , devRemoteDebugEnabled
    , devCpu
    , devHeapSize
    , devFleetName

    -- * DeviceMinutes
    , DeviceMinutes
    , deviceMinutes
    , dmMetered
    , dmTotal
    , dmUnmetered

    -- * DevicePool
    , DevicePool
    , devicePool
    , dArn
    , dRules
    , dName
    , dType
    , dDescription

    -- * DevicePoolCompatibilityResult
    , DevicePoolCompatibilityResult
    , devicePoolCompatibilityResult
    , dpcrDevice
    , dpcrCompatible
    , dpcrIncompatibilityMessages

    -- * ExecutionConfiguration
    , ExecutionConfiguration
    , executionConfiguration
    , ecAccountsCleanup
    , ecAppPackagesCleanup
    , ecJobTimeoutMinutes

    -- * IncompatibilityMessage
    , IncompatibilityMessage
    , incompatibilityMessage
    , imType
    , imMessage

    -- * Job
    , Job
    , job
    , jobStatus
    , jobCounters
    , jobArn
    , jobCreated
    , jobDevice
    , jobStopped
    , jobResult
    , jobName
    , jobDeviceMinutes
    , jobType
    , jobMessage
    , jobStarted

    -- * Location
    , Location
    , location
    , lLatitude
    , lLongitude

    -- * MonetaryAmount
    , MonetaryAmount
    , monetaryAmount
    , maAmount
    , maCurrencyCode

    -- * NetworkProfile
    , NetworkProfile
    , networkProfile
    , npUplinkJitterMs
    , npArn
    , npUplinkLossPercent
    , npDownlinkJitterMs
    , npName
    , npDownlinkLossPercent
    , npType
    , npUplinkDelayMs
    , npUplinkBandwidthBits
    , npDescription
    , npDownlinkDelayMs
    , npDownlinkBandwidthBits

    -- * Offering
    , Offering
    , offering
    , oPlatform
    , oId
    , oRecurringCharges
    , oType
    , oDescription

    -- * OfferingPromotion
    , OfferingPromotion
    , offeringPromotion
    , opId
    , opDescription

    -- * OfferingStatus
    , OfferingStatus
    , offeringStatus
    , osEffectiveOn
    , osOffering
    , osQuantity
    , osType

    -- * OfferingTransaction
    , OfferingTransaction
    , offeringTransaction
    , otOfferingStatus
    , otCost
    , otTransactionId
    , otOfferingPromotionId
    , otCreatedOn

    -- * Problem
    , Problem
    , problem
    , pDevice
    , pTest
    , pResult
    , pRun
    , pJob
    , pMessage
    , pSuite

    -- * ProblemDetail
    , ProblemDetail
    , problemDetail
    , pdArn
    , pdName

    -- * Project
    , Project
    , project
    , pArn
    , pCreated
    , pName
    , pDefaultJobTimeoutMinutes

    -- * Radios
    , Radios
    , radios
    , rNfc
    , rGps
    , rBluetooth
    , rWifi

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcFrequency
    , rcCost

    -- * RemoteAccessSession
    , RemoteAccessSession
    , remoteAccessSession
    , rasBillingMethod
    , rasClientId
    , rasDeviceUdid
    , rasStatus
    , rasArn
    , rasCreated
    , rasDevice
    , rasStopped
    , rasResult
    , rasName
    , rasDeviceMinutes
    , rasRemoteDebugEnabled
    , rasEndpoint
    , rasMessage
    , rasHostAddress
    , rasStarted

    -- * Resolution
    , Resolution
    , resolution
    , rHeight
    , rWidth

    -- * Rule
    , Rule
    , rule
    , rAttribute
    , rOperator
    , rValue

    -- * Run
    , Run
    , run
    , runBillingMethod
    , runStatus
    , runCustomerArtifactPaths
    , runCounters
    , runPlatform
    , runArn
    , runCreated
    , runStopped
    , runResult
    , runCompletedJobs
    , runResultCode
    , runName
    , runParsingResultURL
    , runNetworkProfile
    , runDeviceMinutes
    , runType
    , runMessage
    , runTotalJobs
    , runStarted

    -- * Sample
    , Sample
    , sample
    , samArn
    , samUrl
    , samType

    -- * ScheduleRunConfiguration
    , ScheduleRunConfiguration
    , scheduleRunConfiguration
    , srcBillingMethod
    , srcCustomerArtifactPaths
    , srcRadios
    , srcLocation
    , srcLocale
    , srcNetworkProfileARN
    , srcExtraDataPackageARN
    , srcAuxiliaryApps

    -- * ScheduleRunTest
    , ScheduleRunTest
    , scheduleRunTest
    , srtTestPackageARN
    , srtParameters
    , srtFilter
    , srtType

    -- * Suite
    , Suite
    , suite
    , sStatus
    , sCounters
    , sArn
    , sCreated
    , sStopped
    , sResult
    , sName
    , sDeviceMinutes
    , sType
    , sMessage
    , sStarted

    -- * Test
    , Test
    , test
    , tStatus
    , tCounters
    , tArn
    , tCreated
    , tStopped
    , tResult
    , tName
    , tDeviceMinutes
    , tType
    , tMessage
    , tStarted

    -- * TrialMinutes
    , TrialMinutes
    , trialMinutes
    , tmRemaining
    , tmTotal

    -- * UniqueProblem
    , UniqueProblem
    , uniqueProblem
    , upProblems
    , upMessage

    -- * Upload
    , Upload
    , upload
    , uStatus
    , uArn
    , uCreated
    , uUrl
    , uName
    , uMetadata
    , uType
    , uMessage
    , uContentType
    ) where

import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.DeviceFarm.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-06-23@ of the Amazon Device Farm SDK configuration.
deviceFarm :: Service
deviceFarm =
  Service
  { _svcAbbrev = "DeviceFarm"
  , _svcSigner = v4
  , _svcPrefix = "devicefarm"
  , _svcVersion = "2015-06-23"
  , _svcEndpoint = defaultEndpoint deviceFarm
  , _svcTimeout = Just 70
  , _svcCheck = statusSuccess
  , _svcError = parseJSONError "DeviceFarm"
  , _svcRetry = retry
  }
  where
    retry =
      Exponential
      { _retryBase = 5.0e-2
      , _retryGrowth = 2
      , _retryAttempts = 5
      , _retryCheck = check
      }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | Exception gets thrown when a user is not eligible to perform the specified transaction.
--
--
_NotEligibleException :: AsError a => Getting (First ServiceError) a ServiceError
_NotEligibleException = _MatchServiceError deviceFarm "NotEligibleException"


-- | An entity with the same name already exists.
--
--
_IdempotencyException :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotencyException = _MatchServiceError deviceFarm "IdempotencyException"


-- | An invalid argument was specified.
--
--
_ArgumentException :: AsError a => Getting (First ServiceError) a ServiceError
_ArgumentException = _MatchServiceError deviceFarm "ArgumentException"


-- | The specified entity was not found.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _MatchServiceError deviceFarm "NotFoundException"


-- | There was a problem with the service account.
--
--
_ServiceAccountException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceAccountException =
  _MatchServiceError deviceFarm "ServiceAccountException"


-- | A limit was exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError deviceFarm "LimitExceededException"

