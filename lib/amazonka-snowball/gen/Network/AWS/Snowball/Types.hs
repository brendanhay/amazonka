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
    mkServiceConfig,

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

    -- * JobType
    JobType (..),

    -- * KmsKeyARN
    KmsKeyARN (..),

    -- * GSTIN
    GSTIN (..),

    -- * INDTaxDocuments
    INDTaxDocuments (..),
    mkINDTaxDocuments,
    indtdGSTIN,

    -- * S3Resource
    S3Resource (..),
    mkS3Resource,
    srBucketArn,
    srKeyRange,

    -- * SnsTopicARN
    SnsTopicARN (..),

    -- * JobId
    JobId (..),

    -- * JobMetadata
    JobMetadata (..),
    mkJobMetadata,
    jmAddressId,
    jmClusterId,
    jmCreationDate,
    jmDataTransferProgress,
    jmDescription,
    jmDeviceConfiguration,
    jmForwardingAddressId,
    jmJobId,
    jmJobLogInfo,
    jmJobState,
    jmJobType,
    jmKmsKeyARN,
    jmNotification,
    jmResources,
    jmRoleARN,
    jmShippingDetails,
    jmSnowballCapacityPreference,
    jmSnowballType,
    jmTaxDocuments,

    -- * ClusterState
    ClusterState (..),

    -- * LambdaResource
    LambdaResource (..),
    mkLambdaResource,
    lrEventTriggers,
    lrLambdaArn,

    -- * Notification
    Notification (..),
    mkNotification,
    nJobStatesToNotify,
    nNotifyAll,
    nSnsTopicARN,

    -- * String
    String (..),

    -- * JobResource
    JobResource (..),
    mkJobResource,
    jrEc2AmiResources,
    jrLambdaResources,
    jrS3Resources,

    -- * JobLogs
    JobLogs (..),
    mkJobLogs,
    jlJobCompletionReportURI,
    jlJobFailureLogURI,
    jlJobSuccessLogURI,

    -- * JobState
    JobState (..),

    -- * ShippingDetails
    ShippingDetails (..),
    mkShippingDetails,
    sdInboundShipment,
    sdOutboundShipment,
    sdShippingOption,

    -- * AddressId
    AddressId (..),

    -- * Address
    Address (..),
    mkAddress,
    aAddressId,
    aCity,
    aCompany,
    aCountry,
    aIsRestricted,
    aLandmark,
    aName,
    aPhoneNumber,
    aPostalCode,
    aPrefectureOrDistrict,
    aStateOrProvince,
    aStreet1,
    aStreet2,
    aStreet3,

    -- * SnowballType
    SnowballType (..),

    -- * WirelessConnection
    WirelessConnection (..),
    mkWirelessConnection,
    wcIsWifiEnabled,

    -- * CompatibleImage
    CompatibleImage (..),
    mkCompatibleImage,
    ciAmiId,
    ciName,

    -- * ShippingOption
    ShippingOption (..),

    -- * ClusterListEntry
    ClusterListEntry (..),
    mkClusterListEntry,
    cleClusterId,
    cleClusterState,
    cleCreationDate,
    cleDescription,

    -- * ResourceARN
    ResourceARN (..),

    -- * ClusterMetadata
    ClusterMetadata (..),
    mkClusterMetadata,
    cmAddressId,
    cmClusterId,
    cmClusterState,
    cmCreationDate,
    cmDescription,
    cmForwardingAddressId,
    cmJobType,
    cmKmsKeyARN,
    cmNotification,
    cmResources,
    cmRoleARN,
    cmShippingOption,
    cmSnowballType,
    cmTaxDocuments,

    -- * ShipmentState
    ShipmentState (..),

    -- * ClusterId
    ClusterId (..),

    -- * SnowconeDeviceConfiguration
    SnowconeDeviceConfiguration (..),
    mkSnowconeDeviceConfiguration,
    sdcWirelessConnection,

    -- * SnowballCapacity
    SnowballCapacity (..),

    -- * ShippingLabelStatus
    ShippingLabelStatus (..),

    -- * KeyRange
    KeyRange (..),
    mkKeyRange,
    krBeginMarker,
    krEndMarker,

    -- * AmiId
    AmiId (..),

    -- * DeviceConfiguration
    DeviceConfiguration (..),
    mkDeviceConfiguration,
    dcSnowconeDeviceConfiguration,

    -- * Shipment
    Shipment (..),
    mkShipment,
    sStatus,
    sTrackingNumber,

    -- * Ec2AmiResource
    Ec2AmiResource (..),
    mkEc2AmiResource,
    earAmiId,
    earSnowballAmiId,

    -- * DataTransfer
    DataTransfer (..),
    mkDataTransfer,
    dtBytesTransferred,
    dtObjectsTransferred,
    dtTotalBytes,
    dtTotalObjects,

    -- * TaxDocuments
    TaxDocuments (..),
    mkTaxDocuments,
    tdIND,

    -- * EventTriggerDefinition
    EventTriggerDefinition (..),
    mkEventTriggerDefinition,
    etdEventResourceARN,

    -- * JobListEntry
    JobListEntry (..),
    mkJobListEntry,
    jleCreationDate,
    jleDescription,
    jleIsMaster,
    jleJobId,
    jleJobState,
    jleJobType,
    jleSnowballType,

    -- * RoleARN
    RoleARN (..),

    -- * NextToken
    NextToken (..),

    -- * Description
    Description (..),

    -- * ForwardingAddressId
    ForwardingAddressId (..),

    -- * UpdatesURI
    UpdatesURI (..),

    -- * BucketArn
    BucketArn (..),

    -- * LambdaArn
    LambdaArn (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Snowball.Types.Address
import Network.AWS.Snowball.Types.AddressId
import Network.AWS.Snowball.Types.AmiId
import Network.AWS.Snowball.Types.BucketArn
import Network.AWS.Snowball.Types.ClusterId
import Network.AWS.Snowball.Types.ClusterListEntry
import Network.AWS.Snowball.Types.ClusterMetadata
import Network.AWS.Snowball.Types.ClusterState
import Network.AWS.Snowball.Types.CompatibleImage
import Network.AWS.Snowball.Types.DataTransfer
import Network.AWS.Snowball.Types.Description
import Network.AWS.Snowball.Types.DeviceConfiguration
import Network.AWS.Snowball.Types.Ec2AmiResource
import Network.AWS.Snowball.Types.EventTriggerDefinition
import Network.AWS.Snowball.Types.ForwardingAddressId
import Network.AWS.Snowball.Types.GSTIN
import Network.AWS.Snowball.Types.INDTaxDocuments
import Network.AWS.Snowball.Types.JobId
import Network.AWS.Snowball.Types.JobListEntry
import Network.AWS.Snowball.Types.JobLogs
import Network.AWS.Snowball.Types.JobMetadata
import Network.AWS.Snowball.Types.JobResource
import Network.AWS.Snowball.Types.JobState
import Network.AWS.Snowball.Types.JobType
import Network.AWS.Snowball.Types.KeyRange
import Network.AWS.Snowball.Types.KmsKeyARN
import Network.AWS.Snowball.Types.LambdaArn
import Network.AWS.Snowball.Types.LambdaResource
import Network.AWS.Snowball.Types.NextToken
import Network.AWS.Snowball.Types.Notification
import Network.AWS.Snowball.Types.ResourceARN
import Network.AWS.Snowball.Types.RoleARN
import Network.AWS.Snowball.Types.S3Resource
import Network.AWS.Snowball.Types.Shipment
import Network.AWS.Snowball.Types.ShipmentState
import Network.AWS.Snowball.Types.ShippingDetails
import Network.AWS.Snowball.Types.ShippingLabelStatus
import Network.AWS.Snowball.Types.ShippingOption
import Network.AWS.Snowball.Types.SnowballCapacity
import Network.AWS.Snowball.Types.SnowballType
import Network.AWS.Snowball.Types.SnowconeDeviceConfiguration
import Network.AWS.Snowball.Types.SnsTopicARN
import Network.AWS.Snowball.Types.String
import Network.AWS.Snowball.Types.TaxDocuments
import Network.AWS.Snowball.Types.UpdatesURI
import Network.AWS.Snowball.Types.WirelessConnection

-- | API version @2016-06-30@ of the Amazon Import/Export Snowball SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Snowball",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "snowball",
      Core._svcVersion = "2016-06-30",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Snowball",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The specified resource can't be found. Check the information you provided in your last request, and try again.
_InvalidResourceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResourceException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidResourceException"
{-# DEPRECATED _InvalidResourceException "Use generic-lens or generic-optics instead." #-}

-- | The address is either outside the serviceable area for your region, or an error occurred. Check the address with your region's carrier and try again. If the issue persists, contact AWS Support.
_UnsupportedAddressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedAddressException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedAddressException"
{-# DEPRECATED _UnsupportedAddressException "Use generic-lens or generic-optics instead." #-}

-- | You get this exception if you call @CreateReturnShippingLabel@ and a valid return shipping label already exists. In this case, use @DescribeReturnShippingLabel@ to get the url.
_ReturnShippingLabelAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReturnShippingLabelAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ReturnShippingLabelAlreadyExistsException"
{-# DEPRECATED _ReturnShippingLabelAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The provided AWS Key Management Service key lacks the permissions to perform the specified 'CreateJob' or 'UpdateJob' action.
_KMSRequestFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSRequestFailedException =
  Core._MatchServiceError
    mkServiceConfig
    "KMSRequestFailedException"
{-# DEPRECATED _KMSRequestFailedException "Use generic-lens or generic-optics instead." #-}

-- | The action can't be performed because the job's current state doesn't allow that action to be performed.
_InvalidJobStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidJobStateException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidJobStateException"
{-# DEPRECATED _InvalidJobStateException "Use generic-lens or generic-optics instead." #-}

-- | Job or cluster creation failed. One or more inputs were invalid. Confirm that the 'CreateClusterRequest$SnowballType' value supports your 'CreateJobRequest$JobType' , and try again.
_InvalidInputCombinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputCombinationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidInputCombinationException"
{-# DEPRECATED _InvalidInputCombinationException "Use generic-lens or generic-optics instead." #-}

-- | You get this exception when you call @CreateReturnShippingLabel@ more than once when other requests are not completed.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError mkServiceConfig "ConflictException"
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead." #-}

-- | Your IAM user lacks the necessary Amazon EC2 permissions to perform the attempted action.
_Ec2RequestFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_Ec2RequestFailedException =
  Core._MatchServiceError
    mkServiceConfig
    "Ec2RequestFailedException"
{-# DEPRECATED _Ec2RequestFailedException "Use generic-lens or generic-optics instead." #-}

-- | The @NextToken@ string was altered unexpectedly, and the operation has stopped. Run the operation without changing the @NextToken@ string, and try again.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidNextTokenException"
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead." #-}

-- | The address provided was invalid. Check the address with your region's carrier, and try again.
_InvalidAddressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAddressException =
  Core._MatchServiceError mkServiceConfig "InvalidAddressException"
{-# DEPRECATED _InvalidAddressException "Use generic-lens or generic-optics instead." #-}

-- | Job creation failed. Currently, clusters support five nodes. If you have less than five nodes for your cluster and you have more nodes to create for this cluster, try again and create jobs until your cluster has exactly five notes.
_ClusterLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterLimitExceededException"
{-# DEPRECATED _ClusterLimitExceededException "Use generic-lens or generic-optics instead." #-}
