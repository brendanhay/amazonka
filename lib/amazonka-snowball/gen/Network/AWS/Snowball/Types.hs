{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types
  ( -- * Service Configuration
    snowball,

    -- * Errors

    -- * ClusterState
    ClusterState (..),

    -- * JobState
    JobState (..),

    -- * JobType
    JobType (..),

    -- * ShipmentState
    ShipmentState (..),

    -- * ShippingLabelStatus
    ShippingLabelStatus (..),

    -- * ShippingOption
    ShippingOption (..),

    -- * SnowballCapacity
    SnowballCapacity (..),

    -- * SnowballType
    SnowballType (..),

    -- * Address
    Address,
    address,
    aIsRestricted,
    aStreet3,
    aLandmark,
    aPostalCode,
    aCountry,
    aStateOrProvince,
    aStreet2,
    aAddressId,
    aCity,
    aPhoneNumber,
    aCompany,
    aName,
    aPrefectureOrDistrict,
    aStreet1,

    -- * ClusterListEntry
    ClusterListEntry,
    clusterListEntry,
    cleClusterState,
    cleClusterId,
    cleCreationDate,
    cleDescription,

    -- * ClusterMetadata
    ClusterMetadata,
    clusterMetadata,
    cmJobType,
    cmKMSKeyARN,
    cmClusterState,
    cmNotification,
    cmForwardingAddressId,
    cmAddressId,
    cmSnowballType,
    cmShippingOption,
    cmResources,
    cmClusterId,
    cmCreationDate,
    cmDescription,
    cmTaxDocuments,
    cmRoleARN,

    -- * CompatibleImage
    CompatibleImage,
    compatibleImage,
    ciName,
    ciAMIId,

    -- * DataTransfer
    DataTransfer,
    dataTransfer,
    dtTotalObjects,
    dtTotalBytes,
    dtObjectsTransferred,
    dtBytesTransferred,

    -- * DeviceConfiguration
    DeviceConfiguration,
    deviceConfiguration,
    dcSnowconeDeviceConfiguration,

    -- * EC2AMIResource
    EC2AMIResource,
    ec2AMIResource,
    earSnowballAMIId,
    earAMIId,

    -- * EventTriggerDefinition
    EventTriggerDefinition,
    eventTriggerDefinition,
    etdEventResourceARN,

    -- * INDTaxDocuments
    INDTaxDocuments,
    iNDTaxDocuments,
    indtdGSTIN,

    -- * JobListEntry
    JobListEntry,
    jobListEntry,
    jleJobType,
    jleJobId,
    jleJobState,
    jleSnowballType,
    jleCreationDate,
    jleDescription,
    jleIsMaster,

    -- * JobLogs
    JobLogs,
    jobLogs,
    jlJobFailureLogURI,
    jlJobCompletionReportURI,
    jlJobSuccessLogURI,

    -- * JobMetadata
    JobMetadata,
    jobMetadata,
    jmJobType,
    jmKMSKeyARN,
    jmJobId,
    jmJobLogInfo,
    jmNotification,
    jmJobState,
    jmForwardingAddressId,
    jmShippingDetails,
    jmAddressId,
    jmSnowballType,
    jmDataTransferProgress,
    jmResources,
    jmClusterId,
    jmCreationDate,
    jmDeviceConfiguration,
    jmDescription,
    jmTaxDocuments,
    jmRoleARN,
    jmSnowballCapacityPreference,

    -- * JobResource
    JobResource,
    jobResource,
    jrEC2AMIResources,
    jrLambdaResources,
    jrS3Resources,

    -- * KeyRange
    KeyRange,
    keyRange,
    krEndMarker,
    krBeginMarker,

    -- * LambdaResource
    LambdaResource,
    lambdaResource,
    lrEventTriggers,
    lrLambdaARN,

    -- * Notification
    Notification,
    notification,
    nNotifyAll,
    nSNSTopicARN,
    nJobStatesToNotify,

    -- * S3Resource
    S3Resource,
    s3Resource,
    srKeyRange,
    srBucketARN,

    -- * Shipment
    Shipment,
    shipment,
    sStatus,
    sTrackingNumber,

    -- * ShippingDetails
    ShippingDetails,
    shippingDetails,
    sdShippingOption,
    sdOutboundShipment,
    sdInboundShipment,

    -- * SnowconeDeviceConfiguration
    SnowconeDeviceConfiguration,
    snowconeDeviceConfiguration,
    sdcWirelessConnection,

    -- * TaxDocuments
    TaxDocuments,
    taxDocuments,
    tdIND,

    -- * WirelessConnection
    WirelessConnection,
    wirelessConnection,
    wcIsWifiEnabled,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Snowball.Types.Address
import Network.AWS.Snowball.Types.ClusterListEntry
import Network.AWS.Snowball.Types.ClusterMetadata
import Network.AWS.Snowball.Types.ClusterState
import Network.AWS.Snowball.Types.CompatibleImage
import Network.AWS.Snowball.Types.DataTransfer
import Network.AWS.Snowball.Types.DeviceConfiguration
import Network.AWS.Snowball.Types.EC2AMIResource
import Network.AWS.Snowball.Types.EventTriggerDefinition
import Network.AWS.Snowball.Types.INDTaxDocuments
import Network.AWS.Snowball.Types.JobListEntry
import Network.AWS.Snowball.Types.JobLogs
import Network.AWS.Snowball.Types.JobMetadata
import Network.AWS.Snowball.Types.JobResource
import Network.AWS.Snowball.Types.JobState
import Network.AWS.Snowball.Types.JobType
import Network.AWS.Snowball.Types.KeyRange
import Network.AWS.Snowball.Types.LambdaResource
import Network.AWS.Snowball.Types.Notification
import Network.AWS.Snowball.Types.S3Resource
import Network.AWS.Snowball.Types.Shipment
import Network.AWS.Snowball.Types.ShipmentState
import Network.AWS.Snowball.Types.ShippingDetails
import Network.AWS.Snowball.Types.ShippingLabelStatus
import Network.AWS.Snowball.Types.ShippingOption
import Network.AWS.Snowball.Types.SnowballCapacity
import Network.AWS.Snowball.Types.SnowballType
import Network.AWS.Snowball.Types.SnowconeDeviceConfiguration
import Network.AWS.Snowball.Types.TaxDocuments
import Network.AWS.Snowball.Types.WirelessConnection

-- | API version @2016-06-30@ of the Amazon Import/Export Snowball SDK configuration.
snowball :: Service
snowball =
  Service
    { _svcAbbrev = "Snowball",
      _svcSigner = v4,
      _svcPrefix = "snowball",
      _svcVersion = "2016-06-30",
      _svcEndpoint = defaultEndpoint snowball,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Snowball",
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
