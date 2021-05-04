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
    _InvalidInputCombinationException,
    _ClusterLimitExceededException,
    _InvalidAddressException,
    _InvalidNextTokenException,
    _ReturnShippingLabelAlreadyExistsException,
    _UnsupportedAddressException,
    _ConflictException,
    _Ec2RequestFailedException,
    _InvalidJobStateException,
    _KMSRequestFailedException,

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
    newAddress,
    address_phoneNumber,
    address_company,
    address_isRestricted,
    address_postalCode,
    address_street1,
    address_landmark,
    address_city,
    address_name,
    address_addressId,
    address_street2,
    address_stateOrProvince,
    address_country,
    address_prefectureOrDistrict,
    address_street3,

    -- * ClusterListEntry
    ClusterListEntry (..),
    newClusterListEntry,
    clusterListEntry_clusterId,
    clusterListEntry_creationDate,
    clusterListEntry_description,
    clusterListEntry_clusterState,

    -- * ClusterMetadata
    ClusterMetadata (..),
    newClusterMetadata,
    clusterMetadata_clusterId,
    clusterMetadata_roleARN,
    clusterMetadata_shippingOption,
    clusterMetadata_creationDate,
    clusterMetadata_kmsKeyARN,
    clusterMetadata_jobType,
    clusterMetadata_resources,
    clusterMetadata_taxDocuments,
    clusterMetadata_snowballType,
    clusterMetadata_description,
    clusterMetadata_addressId,
    clusterMetadata_forwardingAddressId,
    clusterMetadata_notification,
    clusterMetadata_clusterState,

    -- * CompatibleImage
    CompatibleImage (..),
    newCompatibleImage,
    compatibleImage_amiId,
    compatibleImage_name,

    -- * DataTransfer
    DataTransfer (..),
    newDataTransfer,
    dataTransfer_totalObjects,
    dataTransfer_bytesTransferred,
    dataTransfer_totalBytes,
    dataTransfer_objectsTransferred,

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
    jobListEntry_isMaster,
    jobListEntry_jobState,
    jobListEntry_creationDate,
    jobListEntry_jobType,
    jobListEntry_snowballType,
    jobListEntry_description,
    jobListEntry_jobId,

    -- * JobLogs
    JobLogs (..),
    newJobLogs,
    jobLogs_jobCompletionReportURI,
    jobLogs_jobSuccessLogURI,
    jobLogs_jobFailureLogURI,

    -- * JobMetadata
    JobMetadata (..),
    newJobMetadata,
    jobMetadata_clusterId,
    jobMetadata_roleARN,
    jobMetadata_jobState,
    jobMetadata_deviceConfiguration,
    jobMetadata_creationDate,
    jobMetadata_kmsKeyARN,
    jobMetadata_jobType,
    jobMetadata_resources,
    jobMetadata_taxDocuments,
    jobMetadata_snowballCapacityPreference,
    jobMetadata_snowballType,
    jobMetadata_dataTransferProgress,
    jobMetadata_description,
    jobMetadata_addressId,
    jobMetadata_forwardingAddressId,
    jobMetadata_shippingDetails,
    jobMetadata_notification,
    jobMetadata_jobLogInfo,
    jobMetadata_jobId,

    -- * JobResource
    JobResource (..),
    newJobResource,
    jobResource_s3Resources,
    jobResource_ec2AmiResources,
    jobResource_lambdaResources,

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

    -- * Notification
    Notification (..),
    newNotification,
    notification_jobStatesToNotify,
    notification_notifyAll,
    notification_snsTopicARN,

    -- * S3Resource
    S3Resource (..),
    newS3Resource,
    s3Resource_bucketArn,
    s3Resource_keyRange,

    -- * Shipment
    Shipment (..),
    newShipment,
    shipment_trackingNumber,
    shipment_status,

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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Snowball",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "snowball",
      Prelude._svcSigningName = "snowball",
      Prelude._svcVersion = "2016-06-30",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "Snowball",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource can\'t be found. Check the information you
-- provided in your last request, and try again.
_InvalidResourceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidResourceException =
  Prelude._MatchServiceError
    defaultService
    "InvalidResourceException"

-- | Job or cluster creation failed. One or more inputs were invalid. Confirm
-- that the CreateClusterRequest$SnowballType value supports your
-- CreateJobRequest$JobType, and try again.
_InvalidInputCombinationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInputCombinationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInputCombinationException"

-- | Job creation failed. Currently, clusters support five nodes. If you have
-- less than five nodes for your cluster and you have more nodes to create
-- for this cluster, try again and create jobs until your cluster has
-- exactly five notes.
_ClusterLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ClusterLimitExceededException"

-- | The address provided was invalid. Check the address with your region\'s
-- carrier, and try again.
_InvalidAddressException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAddressException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAddressException"

-- | The @NextToken@ string was altered unexpectedly, and the operation has
-- stopped. Run the operation without changing the @NextToken@ string, and
-- try again.
_InvalidNextTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNextTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | You get this exception if you call @CreateReturnShippingLabel@ and a
-- valid return shipping label already exists. In this case, use
-- @DescribeReturnShippingLabel@ to get the url.
_ReturnShippingLabelAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReturnShippingLabelAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ReturnShippingLabelAlreadyExistsException"

-- | The address is either outside the serviceable area for your region, or
-- an error occurred. Check the address with your region\'s carrier and try
-- again. If the issue persists, contact AWS Support.
_UnsupportedAddressException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedAddressException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedAddressException"

-- | You get this exception when you call @CreateReturnShippingLabel@ more
-- than once when other requests are not completed.
_ConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConflictException =
  Prelude._MatchServiceError
    defaultService
    "ConflictException"

-- | Your IAM user lacks the necessary Amazon EC2 permissions to perform the
-- attempted action.
_Ec2RequestFailedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_Ec2RequestFailedException =
  Prelude._MatchServiceError
    defaultService
    "Ec2RequestFailedException"

-- | The action can\'t be performed because the job\'s current state doesn\'t
-- allow that action to be performed.
_InvalidJobStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidJobStateException =
  Prelude._MatchServiceError
    defaultService
    "InvalidJobStateException"

-- | The provided AWS Key Management Service key lacks the permissions to
-- perform the specified CreateJob or UpdateJob action.
_KMSRequestFailedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSRequestFailedException =
  Prelude._MatchServiceError
    defaultService
    "KMSRequestFailedException"
