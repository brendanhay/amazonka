{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types
    (
    -- * Service Configuration
      snowball

    -- * Errors
    , _InvalidResourceException
    , _UnsupportedAddressException
    , _KMSRequestFailedException
    , _InvalidJobStateException
    , _InvalidAddressException

    -- * JobState
    , JobState (..)

    -- * JobType
    , JobType (..)

    -- * ShippingOption
    , ShippingOption (..)

    -- * SnowballCapacity
    , SnowballCapacity (..)

    -- * Address
    , Address
    , address
    , aStreet3
    , aLandmark
    , aPostalCode
    , aCountry
    , aStateOrProvince
    , aStreet2
    , aAddressId
    , aCity
    , aPhoneNumber
    , aCompany
    , aName
    , aPrefectureOrDistrict
    , aStreet1

    -- * DataTransfer
    , DataTransfer
    , dataTransfer
    , dtTotalObjects
    , dtTotalBytes
    , dtObjectsTransferred
    , dtBytesTransferred

    -- * JobListEntry
    , JobListEntry
    , jobListEntry
    , jleJobId
    , jleJobState
    , jleIsMaster

    -- * JobLogs
    , JobLogs
    , jobLogs
    , jlJobFailureLogURI
    , jlJobCompletionReportURI
    , jlJobSuccessLogURI

    -- * JobMetadata
    , JobMetadata
    , jobMetadata
    , jmJobType
    , jmKMSKeyARN
    , jmJobId
    , jmJobLogInfo
    , jmNotification
    , jmJobState
    , jmShippingDetails
    , jmAddressId
    , jmDataTransferProgress
    , jmResources
    , jmCreationDate
    , jmDescription
    , jmRoleARN
    , jmSnowballCapacityPreference

    -- * JobResource
    , JobResource
    , jobResource
    , jrS3Resources

    -- * KeyRange
    , KeyRange
    , keyRange
    , krEndMarker
    , krBeginMarker

    -- * Notification
    , Notification
    , notification
    , nNotifyAll
    , nSNSTopicARN
    , nJobStatesToNotify

    -- * S3Resource
    , S3Resource
    , s3Resource
    , srKeyRange
    , srBucketARN

    -- * Shipment
    , Shipment
    , shipment
    , sStatus
    , sTrackingNumber

    -- * ShippingDetails
    , ShippingDetails
    , shippingDetails
    , sdShippingOption
    , sdOutboundShipment
    , sdInboundShipment
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.Snowball.Types.Product
import           Network.AWS.Snowball.Types.Sum

-- | API version '2016-06-30' of the Amazon Import/Export Snowball SDK configuration.
snowball :: Service
snowball =
    Service
    { _svcAbbrev = "Snowball"
    , _svcSigner = v4
    , _svcPrefix = "snowball"
    , _svcVersion = "2016-06-30"
    , _svcEndpoint = defaultEndpoint snowball
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Snowball"
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

-- | The specified resource can\'t be found. Check the information you provided in your last request, and try again.
_InvalidResourceException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceException = _ServiceError . hasCode "InvalidResourceException"

-- | The address is either outside the serviceable area for your region, or an error occurred. Check the address with your region\'s carrier and try again. If the issue persists, contact AWS Support.
_UnsupportedAddressException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedAddressException =
    _ServiceError . hasCode "UnsupportedAddressException"

-- | The provided AWS Key Management Service key lacks the permissions to perform the specified < CreateJob> or < UpdateJob> action.
_KMSRequestFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSRequestFailedException =
    _ServiceError . hasCode "KMSRequestFailedException"

-- | The action can\'t be performed because the job\'s current state doesn\'t allow that action to be performed.
_InvalidJobStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidJobStateException = _ServiceError . hasCode "InvalidJobStateException"

-- | The address provided was invalid. Check the address with your region\'s carrier, and try again.
_InvalidAddressException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAddressException = _ServiceError . hasCode "InvalidAddressException"
