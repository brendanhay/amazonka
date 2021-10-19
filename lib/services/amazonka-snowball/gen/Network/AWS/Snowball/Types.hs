{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidResourceException,
    _UnsupportedAddressException,
    _ReturnShippingLabelAlreadyExistsException,
    _KMSRequestFailedException,
    _InvalidJobStateException,
    _InvalidInputCombinationException,
    _ConflictException,
    _Ec2RequestFailedException,
    _InvalidNextTokenException,
    _InvalidAddressException,
    _ClusterLimitExceededException,

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
    address_isRestricted,
    address_street3,
    address_landmark,
    address_postalCode,
    address_country,
    address_stateOrProvince,
    address_street2,
    address_addressId,
    address_city,
    address_phoneNumber,
    address_company,
    address_name,
    address_prefectureOrDistrict,
    address_street1,

    -- * ClusterListEntry
    ClusterListEntry (..),
    newClusterListEntry,
    clusterListEntry_clusterState,
    clusterListEntry_clusterId,
    clusterListEntry_creationDate,
    clusterListEntry_description,

    -- * ClusterMetadata
    ClusterMetadata (..),
    newClusterMetadata,
    clusterMetadata_jobType,
    clusterMetadata_kmsKeyARN,
    clusterMetadata_clusterState,
    clusterMetadata_notification,
    clusterMetadata_forwardingAddressId,
    clusterMetadata_addressId,
    clusterMetadata_snowballType,
    clusterMetadata_shippingOption,
    clusterMetadata_resources,
    clusterMetadata_onDeviceServiceConfiguration,
    clusterMetadata_clusterId,
    clusterMetadata_creationDate,
    clusterMetadata_description,
    clusterMetadata_taxDocuments,
    clusterMetadata_roleARN,

    -- * CompatibleImage
    CompatibleImage (..),
    newCompatibleImage,
    compatibleImage_name,
    compatibleImage_amiId,

    -- * DataTransfer
    DataTransfer (..),
    newDataTransfer,
    dataTransfer_totalObjects,
    dataTransfer_totalBytes,
    dataTransfer_objectsTransferred,
    dataTransfer_bytesTransferred,

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
    jobListEntry_jobType,
    jobListEntry_jobId,
    jobListEntry_jobState,
    jobListEntry_snowballType,
    jobListEntry_creationDate,
    jobListEntry_description,
    jobListEntry_isMaster,

    -- * JobLogs
    JobLogs (..),
    newJobLogs,
    jobLogs_jobFailureLogURI,
    jobLogs_jobCompletionReportURI,
    jobLogs_jobSuccessLogURI,

    -- * JobMetadata
    JobMetadata (..),
    newJobMetadata,
    jobMetadata_jobType,
    jobMetadata_kmsKeyARN,
    jobMetadata_remoteManagement,
    jobMetadata_jobId,
    jobMetadata_jobLogInfo,
    jobMetadata_notification,
    jobMetadata_jobState,
    jobMetadata_forwardingAddressId,
    jobMetadata_shippingDetails,
    jobMetadata_addressId,
    jobMetadata_snowballType,
    jobMetadata_dataTransferProgress,
    jobMetadata_longTermPricingId,
    jobMetadata_resources,
    jobMetadata_onDeviceServiceConfiguration,
    jobMetadata_clusterId,
    jobMetadata_creationDate,
    jobMetadata_deviceConfiguration,
    jobMetadata_description,
    jobMetadata_taxDocuments,
    jobMetadata_roleARN,
    jobMetadata_snowballCapacityPreference,

    -- * JobResource
    JobResource (..),
    newJobResource,
    jobResource_ec2AmiResources,
    jobResource_lambdaResources,
    jobResource_s3Resources,

    -- * KeyRange
    KeyRange (..),
    newKeyRange,
    keyRange_endMarker,
    keyRange_beginMarker,

    -- * LambdaResource
    LambdaResource (..),
    newLambdaResource,
    lambdaResource_eventTriggers,
    lambdaResource_lambdaArn,

    -- * LongTermPricingListEntry
    LongTermPricingListEntry (..),
    newLongTermPricingListEntry,
    longTermPricingListEntry_longTermPricingType,
    longTermPricingListEntry_longTermPricingStartDate,
    longTermPricingListEntry_snowballType,
    longTermPricingListEntry_longTermPricingId,
    longTermPricingListEntry_longTermPricingEndDate,
    longTermPricingListEntry_currentActiveJob,
    longTermPricingListEntry_isLongTermPricingAutoRenew,
    longTermPricingListEntry_longTermPricingStatus,
    longTermPricingListEntry_jobIds,
    longTermPricingListEntry_replacementJob,

    -- * NFSOnDeviceServiceConfiguration
    NFSOnDeviceServiceConfiguration (..),
    newNFSOnDeviceServiceConfiguration,
    nFSOnDeviceServiceConfiguration_storageLimit,
    nFSOnDeviceServiceConfiguration_storageUnit,

    -- * Notification
    Notification (..),
    newNotification,
    notification_notifyAll,
    notification_snsTopicARN,
    notification_jobStatesToNotify,

    -- * OnDeviceServiceConfiguration
    OnDeviceServiceConfiguration (..),
    newOnDeviceServiceConfiguration,
    onDeviceServiceConfiguration_nFSOnDeviceService,

    -- * S3Resource
    S3Resource (..),
    newS3Resource,
    s3Resource_keyRange,
    s3Resource_bucketArn,
    s3Resource_targetOnDeviceServices,

    -- * Shipment
    Shipment (..),
    newShipment,
    shipment_status,
    shipment_trackingNumber,

    -- * ShippingDetails
    ShippingDetails (..),
    newShippingDetails,
    shippingDetails_shippingOption,
    shippingDetails_outboundShipment,
    shippingDetails_inboundShipment,

    -- * SnowconeDeviceConfiguration
    SnowconeDeviceConfiguration (..),
    newSnowconeDeviceConfiguration,
    snowconeDeviceConfiguration_wirelessConnection,

    -- * TargetOnDeviceService
    TargetOnDeviceService (..),
    newTargetOnDeviceService,
    targetOnDeviceService_transferOption,
    targetOnDeviceService_serviceName,

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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Snowball.Types.Address
import Network.AWS.Snowball.Types.ClusterListEntry
import Network.AWS.Snowball.Types.ClusterMetadata
import Network.AWS.Snowball.Types.ClusterState
import Network.AWS.Snowball.Types.CompatibleImage
import Network.AWS.Snowball.Types.DataTransfer
import Network.AWS.Snowball.Types.DeviceConfiguration
import Network.AWS.Snowball.Types.DeviceServiceName
import Network.AWS.Snowball.Types.Ec2AmiResource
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
import Network.AWS.Snowball.Types.LongTermPricingListEntry
import Network.AWS.Snowball.Types.LongTermPricingType
import Network.AWS.Snowball.Types.NFSOnDeviceServiceConfiguration
import Network.AWS.Snowball.Types.Notification
import Network.AWS.Snowball.Types.OnDeviceServiceConfiguration
import Network.AWS.Snowball.Types.RemoteManagement
import Network.AWS.Snowball.Types.S3Resource
import Network.AWS.Snowball.Types.Shipment
import Network.AWS.Snowball.Types.ShipmentState
import Network.AWS.Snowball.Types.ShippingDetails
import Network.AWS.Snowball.Types.ShippingLabelStatus
import Network.AWS.Snowball.Types.ShippingOption
import Network.AWS.Snowball.Types.SnowballCapacity
import Network.AWS.Snowball.Types.SnowballType
import Network.AWS.Snowball.Types.SnowconeDeviceConfiguration
import Network.AWS.Snowball.Types.StorageUnit
import Network.AWS.Snowball.Types.TargetOnDeviceService
import Network.AWS.Snowball.Types.TaxDocuments
import Network.AWS.Snowball.Types.TransferOption
import Network.AWS.Snowball.Types.WirelessConnection

-- | API version @2016-06-30@ of the Amazon Import/Export Snowball SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Snowball",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "snowball",
      Core._serviceSigningName = "snowball",
      Core._serviceVersion = "2016-06-30",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Snowball",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource can\'t be found. Check the information you
-- provided in your last request, and try again.
_InvalidResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceException"

-- | The address is either outside the serviceable area for your region, or
-- an error occurred. Check the address with your region\'s carrier and try
-- again. If the issue persists, contact AWS Support.
_UnsupportedAddressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedAddressException =
  Core._MatchServiceError
    defaultService
    "UnsupportedAddressException"

-- | You get this exception if you call @CreateReturnShippingLabel@ and a
-- valid return shipping label already exists. In this case, use
-- @DescribeReturnShippingLabel@ to get the url.
_ReturnShippingLabelAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReturnShippingLabelAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ReturnShippingLabelAlreadyExistsException"

-- | The provided AWS Key Management Service key lacks the permissions to
-- perform the specified CreateJob or UpdateJob action.
_KMSRequestFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSRequestFailedException =
  Core._MatchServiceError
    defaultService
    "KMSRequestFailedException"

-- | The action can\'t be performed because the job\'s current state doesn\'t
-- allow that action to be performed.
_InvalidJobStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidJobStateException =
  Core._MatchServiceError
    defaultService
    "InvalidJobStateException"

-- | Job or cluster creation failed. One or more inputs were invalid. Confirm
-- that the CreateClusterRequest$SnowballType value supports your
-- CreateJobRequest$JobType, and try again.
_InvalidInputCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidInputCombinationException"

-- | You get this exception when you call @CreateReturnShippingLabel@ more
-- than once when other requests are not completed.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Your IAM user lacks the necessary Amazon EC2 permissions to perform the
-- attempted action.
_Ec2RequestFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_Ec2RequestFailedException =
  Core._MatchServiceError
    defaultService
    "Ec2RequestFailedException"

-- | The @NextToken@ string was altered unexpectedly, and the operation has
-- stopped. Run the operation without changing the @NextToken@ string, and
-- try again.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The address provided was invalid. Check the address with your region\'s
-- carrier, and try again.
_InvalidAddressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAddressException =
  Core._MatchServiceError
    defaultService
    "InvalidAddressException"

-- | Job creation failed. Currently, clusters support five nodes. If you have
-- fewer than five nodes for your cluster and you have more nodes to create
-- for this cluster, try again and create jobs until your cluster has
-- exactly five nodes.
_ClusterLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ClusterLimitExceededException"
