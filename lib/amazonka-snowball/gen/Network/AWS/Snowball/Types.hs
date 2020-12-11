-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types
  ( -- * Service configuration
    snowballService,

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
    Address (..),
    mkAddress,
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
    ClusterListEntry (..),
    mkClusterListEntry,
    cleClusterState,
    cleClusterId,
    cleCreationDate,
    cleDescription,

    -- * ClusterMetadata
    ClusterMetadata (..),
    mkClusterMetadata,
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
    CompatibleImage (..),
    mkCompatibleImage,
    ciName,
    ciAMIId,

    -- * DataTransfer
    DataTransfer (..),
    mkDataTransfer,
    dtTotalObjects,
    dtTotalBytes,
    dtObjectsTransferred,
    dtBytesTransferred,

    -- * DeviceConfiguration
    DeviceConfiguration (..),
    mkDeviceConfiguration,
    dcSnowconeDeviceConfiguration,

    -- * EC2AMIResource
    EC2AMIResource (..),
    mkEC2AMIResource,
    earSnowballAMIId,
    earAMIId,

    -- * EventTriggerDefinition
    EventTriggerDefinition (..),
    mkEventTriggerDefinition,
    etdEventResourceARN,

    -- * INDTaxDocuments
    INDTaxDocuments (..),
    mkINDTaxDocuments,
    indtdGSTIN,

    -- * JobListEntry
    JobListEntry (..),
    mkJobListEntry,
    jleJobType,
    jleJobId,
    jleJobState,
    jleSnowballType,
    jleCreationDate,
    jleDescription,
    jleIsMaster,

    -- * JobLogs
    JobLogs (..),
    mkJobLogs,
    jlJobFailureLogURI,
    jlJobCompletionReportURI,
    jlJobSuccessLogURI,

    -- * JobMetadata
    JobMetadata (..),
    mkJobMetadata,
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
    JobResource (..),
    mkJobResource,
    jrEC2AMIResources,
    jrLambdaResources,
    jrS3Resources,

    -- * KeyRange
    KeyRange (..),
    mkKeyRange,
    krEndMarker,
    krBeginMarker,

    -- * LambdaResource
    LambdaResource (..),
    mkLambdaResource,
    lrEventTriggers,
    lrLambdaARN,

    -- * Notification
    Notification (..),
    mkNotification,
    nNotifyAll,
    nSNSTopicARN,
    nJobStatesToNotify,

    -- * S3Resource
    S3Resource (..),
    mkS3Resource,
    srKeyRange,
    srBucketARN,

    -- * Shipment
    Shipment (..),
    mkShipment,
    sStatus,
    sTrackingNumber,

    -- * ShippingDetails
    ShippingDetails (..),
    mkShippingDetails,
    sdShippingOption,
    sdOutboundShipment,
    sdInboundShipment,

    -- * SnowconeDeviceConfiguration
    SnowconeDeviceConfiguration (..),
    mkSnowconeDeviceConfiguration,
    sdcWirelessConnection,

    -- * TaxDocuments
    TaxDocuments (..),
    mkTaxDocuments,
    tdIND,

    -- * WirelessConnection
    WirelessConnection (..),
    mkWirelessConnection,
    wcIsWifiEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign
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
snowballService :: Lude.Service
snowballService =
  Lude.Service
    { Lude._svcAbbrev = "Snowball",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "snowball",
      Lude._svcVersion = "2016-06-30",
      Lude._svcEndpoint = Lude.defaultEndpoint snowballService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Snowball",
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
