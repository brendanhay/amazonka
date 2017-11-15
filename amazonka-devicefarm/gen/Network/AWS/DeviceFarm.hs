{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Device Farm is a service that enables mobile app developers to test Android, iOS, and Fire OS apps on physical phones, tablets, and other devices in the cloud.
--
--
module Network.AWS.DeviceFarm
    (
    -- * Service Configuration
      deviceFarm

    -- * Errors
    -- $errors

    -- ** NotEligibleException
    , _NotEligibleException

    -- ** IdempotencyException
    , _IdempotencyException

    -- ** ArgumentException
    , _ArgumentException

    -- ** NotFoundException
    , _NotFoundException

    -- ** ServiceAccountException
    , _ServiceAccountException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListProjects (Paginated)
    , module Network.AWS.DeviceFarm.ListProjects

    -- ** DeleteProject
    , module Network.AWS.DeviceFarm.DeleteProject

    -- ** UpdateProject
    , module Network.AWS.DeviceFarm.UpdateProject

    -- ** UpdateNetworkProfile
    , module Network.AWS.DeviceFarm.UpdateNetworkProfile

    -- ** DeleteNetworkProfile
    , module Network.AWS.DeviceFarm.DeleteNetworkProfile

    -- ** GetDevicePoolCompatibility
    , module Network.AWS.DeviceFarm.GetDevicePoolCompatibility

    -- ** InstallToRemoteAccessSession
    , module Network.AWS.DeviceFarm.InstallToRemoteAccessSession

    -- ** ListTests (Paginated)
    , module Network.AWS.DeviceFarm.ListTests

    -- ** ListArtifacts (Paginated)
    , module Network.AWS.DeviceFarm.ListArtifacts

    -- ** CreateUpload
    , module Network.AWS.DeviceFarm.CreateUpload

    -- ** DeleteRemoteAccessSession
    , module Network.AWS.DeviceFarm.DeleteRemoteAccessSession

    -- ** DeleteUpload
    , module Network.AWS.DeviceFarm.DeleteUpload

    -- ** GetDevicePool
    , module Network.AWS.DeviceFarm.GetDevicePool

    -- ** ListDevicePools (Paginated)
    , module Network.AWS.DeviceFarm.ListDevicePools

    -- ** UpdateDevicePool
    , module Network.AWS.DeviceFarm.UpdateDevicePool

    -- ** DeleteDevicePool
    , module Network.AWS.DeviceFarm.DeleteDevicePool

    -- ** GetUpload
    , module Network.AWS.DeviceFarm.GetUpload

    -- ** ListOfferingTransactions (Paginated)
    , module Network.AWS.DeviceFarm.ListOfferingTransactions

    -- ** CreateDevicePool
    , module Network.AWS.DeviceFarm.CreateDevicePool

    -- ** DeleteRun
    , module Network.AWS.DeviceFarm.DeleteRun

    -- ** ListRuns (Paginated)
    , module Network.AWS.DeviceFarm.ListRuns

    -- ** GetTest
    , module Network.AWS.DeviceFarm.GetTest

    -- ** GetNetworkProfile
    , module Network.AWS.DeviceFarm.GetNetworkProfile

    -- ** RenewOffering
    , module Network.AWS.DeviceFarm.RenewOffering

    -- ** GetDevice
    , module Network.AWS.DeviceFarm.GetDevice

    -- ** ListJobs (Paginated)
    , module Network.AWS.DeviceFarm.ListJobs

    -- ** StopRemoteAccessSession
    , module Network.AWS.DeviceFarm.StopRemoteAccessSession

    -- ** CreateNetworkProfile
    , module Network.AWS.DeviceFarm.CreateNetworkProfile

    -- ** GetJob
    , module Network.AWS.DeviceFarm.GetJob

    -- ** ListNetworkProfiles
    , module Network.AWS.DeviceFarm.ListNetworkProfiles

    -- ** ScheduleRun
    , module Network.AWS.DeviceFarm.ScheduleRun

    -- ** GetRun
    , module Network.AWS.DeviceFarm.GetRun

    -- ** ListSamples (Paginated)
    , module Network.AWS.DeviceFarm.ListSamples

    -- ** ListSuites (Paginated)
    , module Network.AWS.DeviceFarm.ListSuites

    -- ** ListRemoteAccessSessions
    , module Network.AWS.DeviceFarm.ListRemoteAccessSessions

    -- ** GetAccountSettings
    , module Network.AWS.DeviceFarm.GetAccountSettings

    -- ** CreateRemoteAccessSession
    , module Network.AWS.DeviceFarm.CreateRemoteAccessSession

    -- ** ListOfferingPromotions
    , module Network.AWS.DeviceFarm.ListOfferingPromotions

    -- ** GetOfferingStatus (Paginated)
    , module Network.AWS.DeviceFarm.GetOfferingStatus

    -- ** ListUploads (Paginated)
    , module Network.AWS.DeviceFarm.ListUploads

    -- ** GetSuite
    , module Network.AWS.DeviceFarm.GetSuite

    -- ** GetRemoteAccessSession
    , module Network.AWS.DeviceFarm.GetRemoteAccessSession

    -- ** PurchaseOffering
    , module Network.AWS.DeviceFarm.PurchaseOffering

    -- ** GetProject
    , module Network.AWS.DeviceFarm.GetProject

    -- ** ListUniqueProblems (Paginated)
    , module Network.AWS.DeviceFarm.ListUniqueProblems

    -- ** StopRun
    , module Network.AWS.DeviceFarm.StopRun

    -- ** ListDevices (Paginated)
    , module Network.AWS.DeviceFarm.ListDevices

    -- ** CreateProject
    , module Network.AWS.DeviceFarm.CreateProject

    -- ** ListOfferings (Paginated)
    , module Network.AWS.DeviceFarm.ListOfferings

    -- * Types

    -- ** ArtifactCategory
    , ArtifactCategory (..)

    -- ** ArtifactType
    , ArtifactType (..)

    -- ** BillingMethod
    , BillingMethod (..)

    -- ** CurrencyCode
    , CurrencyCode (..)

    -- ** DeviceAttribute
    , DeviceAttribute (..)

    -- ** DeviceFormFactor
    , DeviceFormFactor (..)

    -- ** DevicePlatform
    , DevicePlatform (..)

    -- ** DevicePoolType
    , DevicePoolType (..)

    -- ** ExecutionResult
    , ExecutionResult (..)

    -- ** ExecutionResultCode
    , ExecutionResultCode (..)

    -- ** ExecutionStatus
    , ExecutionStatus (..)

    -- ** NetworkProfileType
    , NetworkProfileType (..)

    -- ** OfferingTransactionType
    , OfferingTransactionType (..)

    -- ** OfferingType
    , OfferingType (..)

    -- ** RecurringChargeFrequency
    , RecurringChargeFrequency (..)

    -- ** RuleOperator
    , RuleOperator (..)

    -- ** SampleType
    , SampleType (..)

    -- ** TestType
    , TestType (..)

    -- ** UploadStatus
    , UploadStatus (..)

    -- ** UploadType
    , UploadType (..)

    -- ** AccountSettings
    , AccountSettings
    , accountSettings
    , asAwsAccountNumber
    , asMaxJobTimeoutMinutes
    , asMaxSlots
    , asTrialMinutes
    , asUnmeteredDevices
    , asUnmeteredRemoteAccessDevices
    , asDefaultJobTimeoutMinutes

    -- ** Artifact
    , Artifact
    , artifact
    , aArn
    , aUrl
    , aExtension
    , aName
    , aType

    -- ** CPU
    , CPU
    , cpu
    , cpuFrequency
    , cpuClock
    , cpuArchitecture

    -- ** Counters
    , Counters
    , counters
    , cPassed
    , cSkipped
    , cWarned
    , cStopped
    , cTotal
    , cFailed
    , cErrored

    -- ** CreateRemoteAccessSessionConfiguration
    , CreateRemoteAccessSessionConfiguration
    , createRemoteAccessSessionConfiguration
    , crascBillingMethod

    -- ** CustomerArtifactPaths
    , CustomerArtifactPaths
    , customerArtifactPaths
    , capAndroidPaths
    , capDeviceHostPaths
    , capIosPaths

    -- ** Device
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

    -- ** DeviceMinutes
    , DeviceMinutes
    , deviceMinutes
    , dmMetered
    , dmTotal
    , dmUnmetered

    -- ** DevicePool
    , DevicePool
    , devicePool
    , dArn
    , dRules
    , dName
    , dType
    , dDescription

    -- ** DevicePoolCompatibilityResult
    , DevicePoolCompatibilityResult
    , devicePoolCompatibilityResult
    , dpcrDevice
    , dpcrCompatible
    , dpcrIncompatibilityMessages

    -- ** ExecutionConfiguration
    , ExecutionConfiguration
    , executionConfiguration
    , ecAccountsCleanup
    , ecAppPackagesCleanup
    , ecJobTimeoutMinutes

    -- ** IncompatibilityMessage
    , IncompatibilityMessage
    , incompatibilityMessage
    , imType
    , imMessage

    -- ** Job
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

    -- ** Location
    , Location
    , location
    , lLatitude
    , lLongitude

    -- ** MonetaryAmount
    , MonetaryAmount
    , monetaryAmount
    , maAmount
    , maCurrencyCode

    -- ** NetworkProfile
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

    -- ** Offering
    , Offering
    , offering
    , oPlatform
    , oId
    , oRecurringCharges
    , oType
    , oDescription

    -- ** OfferingPromotion
    , OfferingPromotion
    , offeringPromotion
    , opId
    , opDescription

    -- ** OfferingStatus
    , OfferingStatus
    , offeringStatus
    , osEffectiveOn
    , osOffering
    , osQuantity
    , osType

    -- ** OfferingTransaction
    , OfferingTransaction
    , offeringTransaction
    , otOfferingStatus
    , otCost
    , otTransactionId
    , otOfferingPromotionId
    , otCreatedOn

    -- ** Problem
    , Problem
    , problem
    , pDevice
    , pTest
    , pResult
    , pRun
    , pJob
    , pMessage
    , pSuite

    -- ** ProblemDetail
    , ProblemDetail
    , problemDetail
    , pdArn
    , pdName

    -- ** Project
    , Project
    , project
    , pArn
    , pCreated
    , pName
    , pDefaultJobTimeoutMinutes

    -- ** Radios
    , Radios
    , radios
    , rNfc
    , rGps
    , rBluetooth
    , rWifi

    -- ** RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcFrequency
    , rcCost

    -- ** RemoteAccessSession
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

    -- ** Resolution
    , Resolution
    , resolution
    , rHeight
    , rWidth

    -- ** Rule
    , Rule
    , rule
    , rAttribute
    , rOperator
    , rValue

    -- ** Run
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

    -- ** Sample
    , Sample
    , sample
    , samArn
    , samUrl
    , samType

    -- ** ScheduleRunConfiguration
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

    -- ** ScheduleRunTest
    , ScheduleRunTest
    , scheduleRunTest
    , srtTestPackageARN
    , srtParameters
    , srtFilter
    , srtType

    -- ** Suite
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

    -- ** Test
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

    -- ** TrialMinutes
    , TrialMinutes
    , trialMinutes
    , tmRemaining
    , tmTotal

    -- ** UniqueProblem
    , UniqueProblem
    , uniqueProblem
    , upProblems
    , upMessage

    -- ** Upload
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

import Network.AWS.DeviceFarm.CreateDevicePool
import Network.AWS.DeviceFarm.CreateNetworkProfile
import Network.AWS.DeviceFarm.CreateProject
import Network.AWS.DeviceFarm.CreateRemoteAccessSession
import Network.AWS.DeviceFarm.CreateUpload
import Network.AWS.DeviceFarm.DeleteDevicePool
import Network.AWS.DeviceFarm.DeleteNetworkProfile
import Network.AWS.DeviceFarm.DeleteProject
import Network.AWS.DeviceFarm.DeleteRemoteAccessSession
import Network.AWS.DeviceFarm.DeleteRun
import Network.AWS.DeviceFarm.DeleteUpload
import Network.AWS.DeviceFarm.GetAccountSettings
import Network.AWS.DeviceFarm.GetDevice
import Network.AWS.DeviceFarm.GetDevicePool
import Network.AWS.DeviceFarm.GetDevicePoolCompatibility
import Network.AWS.DeviceFarm.GetJob
import Network.AWS.DeviceFarm.GetNetworkProfile
import Network.AWS.DeviceFarm.GetOfferingStatus
import Network.AWS.DeviceFarm.GetProject
import Network.AWS.DeviceFarm.GetRemoteAccessSession
import Network.AWS.DeviceFarm.GetRun
import Network.AWS.DeviceFarm.GetSuite
import Network.AWS.DeviceFarm.GetTest
import Network.AWS.DeviceFarm.GetUpload
import Network.AWS.DeviceFarm.InstallToRemoteAccessSession
import Network.AWS.DeviceFarm.ListArtifacts
import Network.AWS.DeviceFarm.ListDevicePools
import Network.AWS.DeviceFarm.ListDevices
import Network.AWS.DeviceFarm.ListJobs
import Network.AWS.DeviceFarm.ListNetworkProfiles
import Network.AWS.DeviceFarm.ListOfferingPromotions
import Network.AWS.DeviceFarm.ListOfferings
import Network.AWS.DeviceFarm.ListOfferingTransactions
import Network.AWS.DeviceFarm.ListProjects
import Network.AWS.DeviceFarm.ListRemoteAccessSessions
import Network.AWS.DeviceFarm.ListRuns
import Network.AWS.DeviceFarm.ListSamples
import Network.AWS.DeviceFarm.ListSuites
import Network.AWS.DeviceFarm.ListTests
import Network.AWS.DeviceFarm.ListUniqueProblems
import Network.AWS.DeviceFarm.ListUploads
import Network.AWS.DeviceFarm.PurchaseOffering
import Network.AWS.DeviceFarm.RenewOffering
import Network.AWS.DeviceFarm.ScheduleRun
import Network.AWS.DeviceFarm.StopRemoteAccessSession
import Network.AWS.DeviceFarm.StopRun
import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.UpdateDevicePool
import Network.AWS.DeviceFarm.UpdateNetworkProfile
import Network.AWS.DeviceFarm.UpdateProject
import Network.AWS.DeviceFarm.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DeviceFarm'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
