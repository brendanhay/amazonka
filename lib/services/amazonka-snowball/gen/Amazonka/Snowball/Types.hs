{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Snowball.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ClusterLimitExceededException,
    _ConflictException,
    _Ec2RequestFailedException,
    _InvalidAddressException,
    _InvalidInputCombinationException,
    _InvalidJobStateException,
    _InvalidNextTokenException,
    _InvalidResourceException,
    _KMSRequestFailedException,
    _ReturnShippingLabelAlreadyExistsException,
    _UnsupportedAddressException,

    -- * ClusterState
    ClusterState (..),

    -- * DeviceServiceName
    DeviceServiceName (..),

    -- * JobState
    JobState (..),

    -- * JobType
    JobType (..),

    -- * LongTermPricingType
    LongTermPricingType (..),

    -- * RemoteManagement
    RemoteManagement (..),

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

    -- * StorageUnit
    StorageUnit (..),

    -- * TransferOption
    TransferOption (..),

    -- * Address
    Address (..),
    newAddress,
    address_addressId,
    address_city,
    address_company,
    address_country,
    address_isRestricted,
    address_landmark,
    address_name,
    address_phoneNumber,
    address_postalCode,
    address_prefectureOrDistrict,
    address_stateOrProvince,
    address_street1,
    address_street2,
    address_street3,

    -- * ClusterListEntry
    ClusterListEntry (..),
    newClusterListEntry,
    clusterListEntry_clusterId,
    clusterListEntry_clusterState,
    clusterListEntry_creationDate,
    clusterListEntry_description,

    -- * ClusterMetadata
    ClusterMetadata (..),
    newClusterMetadata,
    clusterMetadata_addressId,
    clusterMetadata_clusterId,
    clusterMetadata_clusterState,
    clusterMetadata_creationDate,
    clusterMetadata_description,
    clusterMetadata_forwardingAddressId,
    clusterMetadata_jobType,
    clusterMetadata_kmsKeyARN,
    clusterMetadata_notification,
    clusterMetadata_onDeviceServiceConfiguration,
    clusterMetadata_resources,
    clusterMetadata_roleARN,
    clusterMetadata_shippingOption,
    clusterMetadata_snowballType,
    clusterMetadata_taxDocuments,

    -- * CompatibleImage
    CompatibleImage (..),
    newCompatibleImage,
    compatibleImage_amiId,
    compatibleImage_name,

    -- * DataTransfer
    DataTransfer (..),
    newDataTransfer,
    dataTransfer_bytesTransferred,
    dataTransfer_objectsTransferred,
    dataTransfer_totalBytes,
    dataTransfer_totalObjects,

    -- * DeviceConfiguration
    DeviceConfiguration (..),
    newDeviceConfiguration,
    deviceConfiguration_snowconeDeviceConfiguration,

    -- * Ec2AmiResource
    Ec2AmiResource (..),
    newEc2AmiResource,
    ec2AmiResource_snowballAmiId,
    ec2AmiResource_amiId,

    -- * EventTriggerDefinition
    EventTriggerDefinition (..),
    newEventTriggerDefinition,
    eventTriggerDefinition_eventResourceARN,

    -- * INDTaxDocuments
    INDTaxDocuments (..),
    newINDTaxDocuments,
    iNDTaxDocuments_gstin,

    -- * JobListEntry
    JobListEntry (..),
    newJobListEntry,
    jobListEntry_creationDate,
    jobListEntry_description,
    jobListEntry_isMaster,
    jobListEntry_jobId,
    jobListEntry_jobState,
    jobListEntry_jobType,
    jobListEntry_snowballType,

    -- * JobLogs
    JobLogs (..),
    newJobLogs,
    jobLogs_jobCompletionReportURI,
    jobLogs_jobFailureLogURI,
    jobLogs_jobSuccessLogURI,

    -- * JobMetadata
    JobMetadata (..),
    newJobMetadata,
    jobMetadata_addressId,
    jobMetadata_clusterId,
    jobMetadata_creationDate,
    jobMetadata_dataTransferProgress,
    jobMetadata_description,
    jobMetadata_deviceConfiguration,
    jobMetadata_forwardingAddressId,
    jobMetadata_jobId,
    jobMetadata_jobLogInfo,
    jobMetadata_jobState,
    jobMetadata_jobType,
    jobMetadata_kmsKeyARN,
    jobMetadata_longTermPricingId,
    jobMetadata_notification,
    jobMetadata_onDeviceServiceConfiguration,
    jobMetadata_remoteManagement,
    jobMetadata_resources,
    jobMetadata_roleARN,
    jobMetadata_shippingDetails,
    jobMetadata_snowballCapacityPreference,
    jobMetadata_snowballType,
    jobMetadata_taxDocuments,

    -- * JobResource
    JobResource (..),
    newJobResource,
    jobResource_ec2AmiResources,
    jobResource_lambdaResources,
    jobResource_s3Resources,

    -- * KeyRange
    KeyRange (..),
    newKeyRange,
    keyRange_beginMarker,
    keyRange_endMarker,

    -- * LambdaResource
    LambdaResource (..),
    newLambdaResource,
    lambdaResource_eventTriggers,
    lambdaResource_lambdaArn,

    -- * LongTermPricingListEntry
    LongTermPricingListEntry (..),
    newLongTermPricingListEntry,
    longTermPricingListEntry_currentActiveJob,
    longTermPricingListEntry_isLongTermPricingAutoRenew,
    longTermPricingListEntry_jobIds,
    longTermPricingListEntry_longTermPricingEndDate,
    longTermPricingListEntry_longTermPricingId,
    longTermPricingListEntry_longTermPricingStartDate,
    longTermPricingListEntry_longTermPricingStatus,
    longTermPricingListEntry_longTermPricingType,
    longTermPricingListEntry_replacementJob,
    longTermPricingListEntry_snowballType,

    -- * NFSOnDeviceServiceConfiguration
    NFSOnDeviceServiceConfiguration (..),
    newNFSOnDeviceServiceConfiguration,
    nFSOnDeviceServiceConfiguration_storageLimit,
    nFSOnDeviceServiceConfiguration_storageUnit,

    -- * Notification
    Notification (..),
    newNotification,
    notification_jobStatesToNotify,
    notification_notifyAll,
    notification_snsTopicARN,

    -- * OnDeviceServiceConfiguration
    OnDeviceServiceConfiguration (..),
    newOnDeviceServiceConfiguration,
    onDeviceServiceConfiguration_nFSOnDeviceService,
    onDeviceServiceConfiguration_tGWOnDeviceService,

    -- * S3Resource
    S3Resource (..),
    newS3Resource,
    s3Resource_bucketArn,
    s3Resource_keyRange,
    s3Resource_targetOnDeviceServices,

    -- * Shipment
    Shipment (..),
    newShipment,
    shipment_status,
    shipment_trackingNumber,

    -- * ShippingDetails
    ShippingDetails (..),
    newShippingDetails,
    shippingDetails_inboundShipment,
    shippingDetails_outboundShipment,
    shippingDetails_shippingOption,

    -- * SnowconeDeviceConfiguration
    SnowconeDeviceConfiguration (..),
    newSnowconeDeviceConfiguration,
    snowconeDeviceConfiguration_wirelessConnection,

    -- * TGWOnDeviceServiceConfiguration
    TGWOnDeviceServiceConfiguration (..),
    newTGWOnDeviceServiceConfiguration,
    tGWOnDeviceServiceConfiguration_storageLimit,
    tGWOnDeviceServiceConfiguration_storageUnit,

    -- * TargetOnDeviceService
    TargetOnDeviceService (..),
    newTargetOnDeviceService,
    targetOnDeviceService_serviceName,
    targetOnDeviceService_transferOption,

    -- * TaxDocuments
    TaxDocuments (..),
    newTaxDocuments,
    taxDocuments_ind,

    -- * WirelessConnection
    WirelessConnection (..),
    newWirelessConnection,
    wirelessConnection_isWifiEnabled,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.Snowball.Types.Address
import Amazonka.Snowball.Types.ClusterListEntry
import Amazonka.Snowball.Types.ClusterMetadata
import Amazonka.Snowball.Types.ClusterState
import Amazonka.Snowball.Types.CompatibleImage
import Amazonka.Snowball.Types.DataTransfer
import Amazonka.Snowball.Types.DeviceConfiguration
import Amazonka.Snowball.Types.DeviceServiceName
import Amazonka.Snowball.Types.Ec2AmiResource
import Amazonka.Snowball.Types.EventTriggerDefinition
import Amazonka.Snowball.Types.INDTaxDocuments
import Amazonka.Snowball.Types.JobListEntry
import Amazonka.Snowball.Types.JobLogs
import Amazonka.Snowball.Types.JobMetadata
import Amazonka.Snowball.Types.JobResource
import Amazonka.Snowball.Types.JobState
import Amazonka.Snowball.Types.JobType
import Amazonka.Snowball.Types.KeyRange
import Amazonka.Snowball.Types.LambdaResource
import Amazonka.Snowball.Types.LongTermPricingListEntry
import Amazonka.Snowball.Types.LongTermPricingType
import Amazonka.Snowball.Types.NFSOnDeviceServiceConfiguration
import Amazonka.Snowball.Types.Notification
import Amazonka.Snowball.Types.OnDeviceServiceConfiguration
import Amazonka.Snowball.Types.RemoteManagement
import Amazonka.Snowball.Types.S3Resource
import Amazonka.Snowball.Types.Shipment
import Amazonka.Snowball.Types.ShipmentState
import Amazonka.Snowball.Types.ShippingDetails
import Amazonka.Snowball.Types.ShippingLabelStatus
import Amazonka.Snowball.Types.ShippingOption
import Amazonka.Snowball.Types.SnowballCapacity
import Amazonka.Snowball.Types.SnowballType
import Amazonka.Snowball.Types.SnowconeDeviceConfiguration
import Amazonka.Snowball.Types.StorageUnit
import Amazonka.Snowball.Types.TGWOnDeviceServiceConfiguration
import Amazonka.Snowball.Types.TargetOnDeviceService
import Amazonka.Snowball.Types.TaxDocuments
import Amazonka.Snowball.Types.TransferOption
import Amazonka.Snowball.Types.WirelessConnection

-- | API version @2016-06-30@ of the Amazon Import/Export Snowball SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Snowball",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "snowball",
      Core.signingName = "snowball",
      Core.version = "2016-06-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Snowball",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Job creation failed. Currently, clusters support five nodes. If you have
-- fewer than five nodes for your cluster and you have more nodes to create
-- for this cluster, try again and create jobs until your cluster has
-- exactly five nodes.
_ClusterLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ClusterLimitExceededException"

-- | You get this exception when you call @CreateReturnShippingLabel@ more
-- than once when other requests are not completed.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Your IAM user lacks the necessary Amazon EC2 permissions to perform the
-- attempted action.
_Ec2RequestFailedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_Ec2RequestFailedException =
  Core._MatchServiceError
    defaultService
    "Ec2RequestFailedException"

-- | The address provided was invalid. Check the address with your region\'s
-- carrier, and try again.
_InvalidAddressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidAddressException =
  Core._MatchServiceError
    defaultService
    "InvalidAddressException"

-- | Job or cluster creation failed. One or more inputs were invalid. Confirm
-- that the CreateClusterRequest$SnowballType value supports your
-- CreateJobRequest$JobType, and try again.
_InvalidInputCombinationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInputCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidInputCombinationException"

-- | The action can\'t be performed because the job\'s current state doesn\'t
-- allow that action to be performed.
_InvalidJobStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidJobStateException =
  Core._MatchServiceError
    defaultService
    "InvalidJobStateException"

-- | The @NextToken@ string was altered unexpectedly, and the operation has
-- stopped. Run the operation without changing the @NextToken@ string, and
-- try again.
_InvalidNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The specified resource can\'t be found. Check the information you
-- provided in your last request, and try again.
_InvalidResourceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceException"

-- | The provided Key Management Service key lacks the permissions to perform
-- the specified CreateJob or UpdateJob action.
_KMSRequestFailedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSRequestFailedException =
  Core._MatchServiceError
    defaultService
    "KMSRequestFailedException"

-- | You get this exception if you call @CreateReturnShippingLabel@ and a
-- valid return shipping label already exists. In this case, use
-- @DescribeReturnShippingLabel@ to get the URL.
_ReturnShippingLabelAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReturnShippingLabelAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ReturnShippingLabelAlreadyExistsException"

-- | The address is either outside the serviceable area for your region, or
-- an error occurred. Check the address with your region\'s carrier and try
-- again. If the issue persists, contact Amazon Web Services Support.
_UnsupportedAddressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedAddressException =
  Core._MatchServiceError
    defaultService
    "UnsupportedAddressException"
