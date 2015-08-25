{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types
    (
    -- * Service Configuration
      deviceFarm

    -- * Errors
    , _IdempotencyException
    , _NotFoundException
    , _ArgumentException
    , _ServiceAccountException
    , _LimitExceededException

    -- * ArtifactCategory
    , ArtifactCategory (..)

    -- * ArtifactType
    , ArtifactType (..)

    -- * BillingMethod
    , BillingMethod (..)

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

    -- * ExecutionStatus
    , ExecutionStatus (..)

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
    , asUnmeteredDevices

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

    -- * Device
    , Device
    , device
    , dCarrier
    , dImage
    , dManufacturer
    , dPlatform
    , dArn
    , dFormFactor
    , dResolution
    , dMemory
    , dRadio
    , dOs
    , dName
    , dModel
    , dCpu
    , dHeapSize

    -- * DevicePool
    , DevicePool
    , devicePool
    , dpArn
    , dpRules
    , dpName
    , dpType
    , dpDescription

    -- * DevicePoolCompatibilityResult
    , DevicePoolCompatibilityResult
    , devicePoolCompatibilityResult
    , dpcrDevice
    , dpcrCompatible
    , dpcrIncompatibilityMessages

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
    , jobType
    , jobMessage
    , jobStarted

    -- * Location
    , Location
    , location
    , lLatitude
    , lLongitude

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

    -- * Radios
    , Radios
    , radios
    , rNfc
    , rGps
    , rBluetooth
    , rWifi

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
    , runStatus
    , runBillingMethod
    , runCounters
    , runPlatform
    , runArn
    , runCreated
    , runCompletedJobs
    , runStopped
    , runResult
    , runName
    , runType
    , runMessage
    , runTotalJobs
    , runStarted

    -- * Sample
    , Sample
    , sample
    , sArn
    , sUrl
    , sType

    -- * ScheduleRunConfiguration
    , ScheduleRunConfiguration
    , scheduleRunConfiguration
    , srcBillingMethod
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
    , suiStatus
    , suiCounters
    , suiArn
    , suiCreated
    , suiStopped
    , suiResult
    , suiName
    , suiType
    , suiMessage
    , suiStarted

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
    , tType
    , tMessage
    , tStarted

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

import           Network.AWS.DeviceFarm.Types.Product
import           Network.AWS.DeviceFarm.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-06-23' of the Amazon Device Farm SDK configuration.
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
    , _svcError = parseJSONError
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
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | An entity with the same name already exists.
_IdempotencyException :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotencyException = _ServiceError . hasCode "IdempotencyException"

-- | The specified entity was not found.
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _ServiceError . hasCode "NotFoundException"

-- | An invalid argument was specified.
_ArgumentException :: AsError a => Getting (First ServiceError) a ServiceError
_ArgumentException = _ServiceError . hasCode "ArgumentException"

-- | There was a problem with the service account.
_ServiceAccountException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceAccountException = _ServiceError . hasCode "ServiceAccountException"

-- | A limit was exceeded.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
