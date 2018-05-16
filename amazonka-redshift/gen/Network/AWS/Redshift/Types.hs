{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types
    (
    -- * Service Configuration
      redshift

    -- * Errors
    , _ClusterSecurityGroupQuotaExceededFault
    , _InvalidS3KeyPrefixFault
    , _SourceNotFoundFault
    , _AuthorizationQuotaExceededFault
    , _CopyToRegionDisabledFault
    , _LimitExceededFault
    , _InvalidClusterSecurityGroupStateFault
    , _ClusterSecurityGroupAlreadyExistsFault
    , _ClusterSnapshotNotFoundFault
    , _InvalidElasticIPFault
    , _TableRestoreNotFoundFault
    , _HSMConfigurationNotFoundFault
    , _AuthorizationAlreadyExistsFault
    , _SubscriptionCategoryNotFoundFault
    , _HSMConfigurationAlreadyExistsFault
    , _SubscriptionNotFoundFault
    , _InvalidS3BucketNameFault
    , _ClusterSnapshotAlreadyExistsFault
    , _InvalidSubnet
    , _InvalidHSMConfigurationStateFault
    , _SnapshotCopyAlreadyDisabledFault
    , _ClusterQuotaExceededFault
    , _HSMClientCertificateQuotaExceededFault
    , _ClusterParameterGroupNotFoundFault
    , _SnapshotCopyGrantQuotaExceededFault
    , _NumberOfNodesPerClusterLimitExceededFault
    , _SnapshotCopyAlreadyEnabledFault
    , _ClusterParameterGroupAlreadyExistsFault
    , _SnapshotCopyDisabledFault
    , _ResizeNotFoundFault
    , _HSMClientCertificateNotFoundFault
    , _SNSTopicARNNotFoundFault
    , _ClusterNotFoundFault
    , _ClusterParameterGroupQuotaExceededFault
    , _SnapshotCopyGrantAlreadyExistsFault
    , _SNSNoAuthorizationFault
    , _InvalidClusterStateFault
    , _InvalidTableRestoreArgumentFault
    , _SnapshotCopyGrantNotFoundFault
    , _HSMConfigurationQuotaExceededFault
    , _ClusterSnapshotQuotaExceededFault
    , _InsufficientClusterCapacityFault
    , _SNSInvalidTopicFault
    , _DependentServiceUnavailableFault
    , _UnsupportedOptionFault
    , _SubscriptionAlreadyExistFault
    , _InvalidVPCNetworkStateFault
    , _ClusterSubnetGroupNotFoundFault
    , _BucketNotFoundFault
    , _InvalidSubscriptionStateFault
    , _DependentServiceRequestThrottlingFault
    , _AuthorizationNotFoundFault
    , _InvalidClusterSubnetGroupStateFault
    , _UnsupportedOperationFault
    , _ClusterSubnetGroupAlreadyExistsFault
    , _InvalidClusterSnapshotStateFault
    , _ClusterSecurityGroupNotFoundFault
    , _ReservedNodeNotFoundFault
    , _ReservedNodeOfferingNotFoundFault
    , _InvalidClusterSubnetStateFault
    , _IncompatibleOrderableOptions
    , _EventSubscriptionQuotaExceededFault
    , _InvalidClusterParameterGroupStateFault
    , _ReservedNodeAlreadyExistsFault
    , _InProgressTableRestoreQuotaExceededFault
    , _InvalidRestoreFault
    , _ResourceNotFoundFault
    , _SubscriptionEventIdNotFoundFault
    , _InvalidSnapshotCopyGrantStateFault
    , _UnknownSnapshotCopyRegionFault
    , _ReservedNodeQuotaExceededFault
    , _ClusterSubnetQuotaExceededFault
    , _ClusterAlreadyExistsFault
    , _AccessToSnapshotDeniedFault
    , _TagLimitExceededFault
    , _NumberOfNodesQuotaExceededFault
    , _HSMClientCertificateAlreadyExistsFault
    , _InvalidHSMClientCertificateStateFault
    , _SubnetAlreadyInUse
    , _SubscriptionSeverityNotFoundFault
    , _UnauthorizedOperation
    , _InvalidTagFault
    , _InsufficientS3BucketPolicyFault
    , _ClusterSubnetGroupQuotaExceededFault

    -- * Re-exported Types
    , module Network.AWS.Redshift.Internal

    -- * ParameterApplyType
    , ParameterApplyType (..)

    -- * ReservedNodeOfferingType
    , ReservedNodeOfferingType (..)

    -- * SourceType
    , SourceType (..)

    -- * TableRestoreStatusType
    , TableRestoreStatusType (..)

    -- * AccountWithRestoreAccess
    , AccountWithRestoreAccess
    , accountWithRestoreAccess
    , awraAccountAlias
    , awraAccountId

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName
    , azSupportedPlatforms

    -- * Cluster
    , Cluster
    , cluster
    , cRestoreStatus
    , cEnhancedVPCRouting
    , cClusterSnapshotCopyStatus
    , cClusterRevisionNumber
    , cPubliclyAccessible
    , cMasterUsername
    , cVPCId
    , cClusterSecurityGroups
    , cAutomatedSnapshotRetentionPeriod
    , cEncrypted
    , cClusterSubnetGroupName
    , cClusterIdentifier
    , cNumberOfNodes
    , cClusterPublicKey
    , cPreferredMaintenanceWindow
    , cModifyStatus
    , cKMSKeyId
    , cClusterParameterGroups
    , cAvailabilityZone
    , cVPCSecurityGroups
    , cHSMStatus
    , cIAMRoles
    , cElasticIPStatus
    , cClusterVersion
    , cNodeType
    , cClusterCreateTime
    , cEndpoint
    , cAllowVersionUpgrade
    , cClusterStatus
    , cPendingModifiedValues
    , cTags
    , cClusterNodes
    , cDBName

    -- * ClusterIAMRole
    , ClusterIAMRole
    , clusterIAMRole
    , cirIAMRoleARN
    , cirApplyStatus

    -- * ClusterNode
    , ClusterNode
    , clusterNode
    , cnNodeRole
    , cnPrivateIPAddress
    , cnPublicIPAddress

    -- * ClusterParameterGroup
    , ClusterParameterGroup
    , clusterParameterGroup
    , cpgParameterGroupFamily
    , cpgDescription
    , cpgTags
    , cpgParameterGroupName

    -- * ClusterParameterGroupNameMessage
    , ClusterParameterGroupNameMessage
    , clusterParameterGroupNameMessage
    , cpgnmParameterGroupStatus
    , cpgnmParameterGroupName

    -- * ClusterParameterGroupStatus
    , ClusterParameterGroupStatus
    , clusterParameterGroupStatus
    , cpgsClusterParameterStatusList
    , cpgsParameterApplyStatus
    , cpgsParameterGroupName

    -- * ClusterParameterStatus
    , ClusterParameterStatus
    , clusterParameterStatus
    , cpsParameterApplyErrorDescription
    , cpsParameterName
    , cpsParameterApplyStatus

    -- * ClusterSecurityGroup
    , ClusterSecurityGroup
    , clusterSecurityGroup
    , cluClusterSecurityGroupName
    , cluIPRanges
    , cluEC2SecurityGroups
    , cluDescription
    , cluTags

    -- * ClusterSecurityGroupMembership
    , ClusterSecurityGroupMembership
    , clusterSecurityGroupMembership
    , csgmStatus
    , csgmClusterSecurityGroupName

    -- * ClusterSnapshotCopyStatus
    , ClusterSnapshotCopyStatus
    , clusterSnapshotCopyStatus
    , cscsRetentionPeriod
    , cscsDestinationRegion
    , cscsSnapshotCopyGrantName

    -- * ClusterSubnetGroup
    , ClusterSubnetGroup
    , clusterSubnetGroup
    , csgVPCId
    , csgSubnets
    , csgClusterSubnetGroupName
    , csgSubnetGroupStatus
    , csgDescription
    , csgTags

    -- * ClusterVersion
    , ClusterVersion
    , clusterVersion
    , cvClusterParameterGroupFamily
    , cvClusterVersion
    , cvDescription

    -- * DefaultClusterParameters
    , DefaultClusterParameters
    , defaultClusterParameters
    , dcpMarker
    , dcpParameters
    , dcpParameterGroupFamily

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , esgStatus
    , esgEC2SecurityGroupOwnerId
    , esgEC2SecurityGroupName
    , esgTags

    -- * ElasticIPStatus
    , ElasticIPStatus
    , elasticIPStatus
    , eisStatus
    , eisElasticIP

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- * Event
    , Event
    , event
    , eSourceType
    , eSeverity
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage
    , eEventId

    -- * EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmSourceType
    , ecmEvents

    -- * EventInfoMap
    , EventInfoMap
    , eventInfoMap
    , eimEventDescription
    , eimSeverity
    , eimEventCategories
    , eimEventId

    -- * EventSubscription
    , EventSubscription
    , eventSubscription
    , esStatus
    , esCustomerAWSId
    , esCustSubscriptionId
    , esSNSTopicARN
    , esEnabled
    , esSourceType
    , esSeverity
    , esSubscriptionCreationTime
    , esEventCategoriesList
    , esTags
    , esSourceIdsList

    -- * HSMClientCertificate
    , HSMClientCertificate
    , hsmClientCertificate
    , hccHSMClientCertificateIdentifier
    , hccHSMClientCertificatePublicKey
    , hccTags

    -- * HSMConfiguration
    , HSMConfiguration
    , hsmConfiguration
    , hcHSMConfigurationIdentifier
    , hcHSMPartitionName
    , hcDescription
    , hcTags
    , hcHSMIPAddress

    -- * HSMStatus
    , HSMStatus
    , hsmStatus
    , hsStatus
    , hsHSMConfigurationIdentifier
    , hsHSMClientCertificateIdentifier

    -- * IPRange
    , IPRange
    , ipRange
    , irStatus
    , irCIdRIP
    , irTags

    -- * LoggingStatus
    , LoggingStatus
    , loggingStatus
    , lsLastFailureTime
    , lsLastSuccessfulDeliveryTime
    , lsS3KeyPrefix
    , lsBucketName
    , lsLoggingEnabled
    , lsLastFailureMessage

    -- * OrderableClusterOption
    , OrderableClusterOption
    , orderableClusterOption
    , ocoAvailabilityZones
    , ocoClusterType
    , ocoClusterVersion
    , ocoNodeType

    -- * Parameter
    , Parameter
    , parameter
    , pApplyType
    , pParameterValue
    , pMinimumEngineVersion
    , pSource
    , pIsModifiable
    , pDataType
    , pAllowedValues
    , pParameterName
    , pDescription

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEnhancedVPCRouting
    , pmvMasterUserPassword
    , pmvPubliclyAccessible
    , pmvAutomatedSnapshotRetentionPeriod
    , pmvClusterIdentifier
    , pmvNumberOfNodes
    , pmvClusterType
    , pmvClusterVersion
    , pmvNodeType

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- * ReservedNode
    , ReservedNode
    , reservedNode
    , rnReservedNodeOfferingType
    , rnState
    , rnCurrencyCode
    , rnStartTime
    , rnNodeCount
    , rnReservedNodeId
    , rnReservedNodeOfferingId
    , rnRecurringCharges
    , rnOfferingType
    , rnUsagePrice
    , rnNodeType
    , rnFixedPrice
    , rnDuration

    -- * ReservedNodeOffering
    , ReservedNodeOffering
    , reservedNodeOffering
    , rnoReservedNodeOfferingType
    , rnoCurrencyCode
    , rnoReservedNodeOfferingId
    , rnoRecurringCharges
    , rnoOfferingType
    , rnoUsagePrice
    , rnoNodeType
    , rnoFixedPrice
    , rnoDuration

    -- * RestoreStatus
    , RestoreStatus
    , restoreStatus
    , rsStatus
    , rsEstimatedTimeToCompletionInSeconds
    , rsCurrentRestoreRateInMegaBytesPerSecond
    , rsProgressInMegaBytes
    , rsElapsedTimeInSeconds
    , rsSnapshotSizeInMegaBytes

    -- * Snapshot
    , Snapshot
    , snapshot
    , sStatus
    , sRestorableNodeTypes
    , sAccountsWithRestoreAccess
    , sEnhancedVPCRouting
    , sSnapshotIdentifier
    , sEncryptedWithHSM
    , sMasterUsername
    , sSourceRegion
    , sVPCId
    , sBackupProgressInMegaBytes
    , sEncrypted
    , sClusterIdentifier
    , sNumberOfNodes
    , sSnapshotType
    , sKMSKeyId
    , sAvailabilityZone
    , sCurrentBackupRateInMegaBytesPerSecond
    , sSnapshotCreateTime
    , sClusterVersion
    , sOwnerAccount
    , sNodeType
    , sElapsedTimeInSeconds
    , sClusterCreateTime
    , sEstimatedSecondsToCompletion
    , sActualIncrementalBackupSizeInMegaBytes
    , sTags
    , sPort
    , sTotalBackupSizeInMegaBytes
    , sDBName

    -- * SnapshotCopyGrant
    , SnapshotCopyGrant
    , snapshotCopyGrant
    , scgKMSKeyId
    , scgSnapshotCopyGrantName
    , scgTags

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- * SupportedPlatform
    , SupportedPlatform
    , supportedPlatform
    , spName

    -- * TableRestoreStatus
    , TableRestoreStatus
    , tableRestoreStatus
    , trsStatus
    , trsTargetSchemaName
    , trsSnapshotIdentifier
    , trsSourceDatabaseName
    , trsTableRestoreRequestId
    , trsNewTableName
    , trsTargetDatabaseName
    , trsSourceSchemaName
    , trsClusterIdentifier
    , trsRequestTime
    , trsSourceTableName
    , trsTotalDataInMegaBytes
    , trsProgressInMegaBytes
    , trsMessage

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TaggedResource
    , TaggedResource
    , taggedResource
    , trTag
    , trResourceType
    , trResourceName

    -- * VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Product
import Network.AWS.Redshift.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2012-12-01@ of the Amazon Redshift SDK configuration.
redshift :: Service
redshift =
  Service
    { _svcAbbrev = "Redshift"
    , _svcSigner = v4
    , _svcPrefix = "redshift"
    , _svcVersion = "2012-12-01"
    , _svcEndpoint = defaultEndpoint redshift
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "Redshift"
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
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The request would result in the user exceeding the allowed number of cluster security groups. For information about increasing your quota, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
--
_ClusterSecurityGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSecurityGroupQuotaExceededFault =
  _MatchServiceError redshift "QuotaExceeded.ClusterSecurityGroup" .
  hasStatus 400


-- | The string specified for the logging S3 key prefix does not comply with the documented constraints.
--
--
_InvalidS3KeyPrefixFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3KeyPrefixFault =
  _MatchServiceError redshift "InvalidS3KeyPrefixFault" . hasStatus 400


-- | The specified Amazon Redshift event source could not be found.
--
--
_SourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SourceNotFoundFault =
  _MatchServiceError redshift "SourceNotFound" . hasStatus 404


-- | The authorization quota for the cluster security group has been reached.
--
--
_AuthorizationQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationQuotaExceededFault =
  _MatchServiceError redshift "AuthorizationQuotaExceeded" . hasStatus 400


-- | Cross-region snapshot copy was temporarily disabled. Try your request again.
--
--
_CopyToRegionDisabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_CopyToRegionDisabledFault =
  _MatchServiceError redshift "CopyToRegionDisabledFault" . hasStatus 400


-- | The encryption key has exceeded its grant limit in AWS KMS.
--
--
_LimitExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededFault =
  _MatchServiceError redshift "LimitExceededFault" . hasStatus 400


-- | The state of the cluster security group is not @available@ .
--
--
_InvalidClusterSecurityGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSecurityGroupStateFault =
  _MatchServiceError redshift "InvalidClusterSecurityGroupState" . hasStatus 400


-- | A cluster security group with the same name already exists.
--
--
_ClusterSecurityGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSecurityGroupAlreadyExistsFault =
  _MatchServiceError redshift "ClusterSecurityGroupAlreadyExists" .
  hasStatus 400


-- | The snapshot identifier does not refer to an existing cluster snapshot.
--
--
_ClusterSnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSnapshotNotFoundFault =
  _MatchServiceError redshift "ClusterSnapshotNotFound" . hasStatus 404


-- | The Elastic IP (EIP) is invalid or cannot be found.
--
--
_InvalidElasticIPFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidElasticIPFault =
  _MatchServiceError redshift "InvalidElasticIpFault" . hasStatus 400


-- | The specified @TableRestoreRequestId@ value was not found.
--
--
_TableRestoreNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_TableRestoreNotFoundFault =
  _MatchServiceError redshift "TableRestoreNotFoundFault" . hasStatus 400


-- | There is no Amazon Redshift HSM configuration with the specified identifier.
--
--
_HSMConfigurationNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMConfigurationNotFoundFault =
  _MatchServiceError redshift "HsmConfigurationNotFoundFault" . hasStatus 400


-- | The specified CIDR block or EC2 security group is already authorized for the specified cluster security group.
--
--
_AuthorizationAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault =
  _MatchServiceError redshift "AuthorizationAlreadyExists" . hasStatus 400


-- | The value specified for the event category was not one of the allowed values, or it specified a category that does not apply to the specified source type. The allowed values are Configuration, Management, Monitoring, and Security.
--
--
_SubscriptionCategoryNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionCategoryNotFoundFault =
  _MatchServiceError redshift "SubscriptionCategoryNotFound" . hasStatus 404


-- | There is already an existing Amazon Redshift HSM configuration with the specified identifier.
--
--
_HSMConfigurationAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMConfigurationAlreadyExistsFault =
  _MatchServiceError redshift "HsmConfigurationAlreadyExistsFault" .
  hasStatus 400


-- | An Amazon Redshift event notification subscription with the specified name does not exist.
--
--
_SubscriptionNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionNotFoundFault =
  _MatchServiceError redshift "SubscriptionNotFound" . hasStatus 404


-- | The S3 bucket name is invalid. For more information about naming rules, go to <http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations> in the Amazon Simple Storage Service (S3) Developer Guide.
--
--
_InvalidS3BucketNameFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3BucketNameFault =
  _MatchServiceError redshift "InvalidS3BucketNameFault" . hasStatus 400


-- | The value specified as a snapshot identifier is already used by an existing snapshot.
--
--
_ClusterSnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSnapshotAlreadyExistsFault =
  _MatchServiceError redshift "ClusterSnapshotAlreadyExists" . hasStatus 400


-- | The requested subnet is not valid, or not all of the subnets are in the same VPC.
--
--
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _MatchServiceError redshift "InvalidSubnet" . hasStatus 400


-- | The specified HSM configuration is not in the @available@ state, or it is still in use by one or more Amazon Redshift clusters.
--
--
_InvalidHSMConfigurationStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidHSMConfigurationStateFault =
  _MatchServiceError redshift "InvalidHsmConfigurationStateFault" .
  hasStatus 400


-- | The cluster already has cross-region snapshot copy disabled.
--
--
_SnapshotCopyAlreadyDisabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyAlreadyDisabledFault =
  _MatchServiceError redshift "SnapshotCopyAlreadyDisabledFault" . hasStatus 400


-- | The request would exceed the allowed number of cluster instances for this account. For information about increasing your quota, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
--
_ClusterQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterQuotaExceededFault =
  _MatchServiceError redshift "ClusterQuotaExceeded" . hasStatus 400


-- | The quota for HSM client certificates has been reached. For information about increasing your quota, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
--
_HSMClientCertificateQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMClientCertificateQuotaExceededFault =
  _MatchServiceError redshift "HsmClientCertificateQuotaExceededFault" .
  hasStatus 400


-- | The parameter group name does not refer to an existing parameter group.
--
--
_ClusterParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterParameterGroupNotFoundFault =
  _MatchServiceError redshift "ClusterParameterGroupNotFound" . hasStatus 404


-- | The AWS account has exceeded the maximum number of snapshot copy grants in this region.
--
--
_SnapshotCopyGrantQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyGrantQuotaExceededFault =
  _MatchServiceError redshift "SnapshotCopyGrantQuotaExceededFault" .
  hasStatus 400


-- | The operation would exceed the number of nodes allowed for a cluster.
--
--
_NumberOfNodesPerClusterLimitExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NumberOfNodesPerClusterLimitExceededFault =
  _MatchServiceError redshift "NumberOfNodesPerClusterLimitExceeded" .
  hasStatus 400


-- | The cluster already has cross-region snapshot copy enabled.
--
--
_SnapshotCopyAlreadyEnabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyAlreadyEnabledFault =
  _MatchServiceError redshift "SnapshotCopyAlreadyEnabledFault" . hasStatus 400


-- | A cluster parameter group with the same name already exists.
--
--
_ClusterParameterGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterParameterGroupAlreadyExistsFault =
  _MatchServiceError redshift "ClusterParameterGroupAlreadyExists" .
  hasStatus 400


-- | Cross-region snapshot copy was temporarily disabled. Try your request again.
--
--
_SnapshotCopyDisabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyDisabledFault =
  _MatchServiceError redshift "SnapshotCopyDisabledFault" . hasStatus 400


-- | A resize operation for the specified cluster is not found.
--
--
_ResizeNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResizeNotFoundFault =
  _MatchServiceError redshift "ResizeNotFound" . hasStatus 404


-- | There is no Amazon Redshift HSM client certificate with the specified identifier.
--
--
_HSMClientCertificateNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMClientCertificateNotFoundFault =
  _MatchServiceError redshift "HsmClientCertificateNotFoundFault" .
  hasStatus 400


-- | An Amazon SNS topic with the specified Amazon Resource Name (ARN) does not exist.
--
--
_SNSTopicARNNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSTopicARNNotFoundFault =
  _MatchServiceError redshift "SNSTopicArnNotFound" . hasStatus 404


-- | The @ClusterIdentifier@ parameter does not refer to an existing cluster.
--
--
_ClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterNotFoundFault =
  _MatchServiceError redshift "ClusterNotFound" . hasStatus 404


-- | The request would result in the user exceeding the allowed number of cluster parameter groups. For information about increasing your quota, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
--
_ClusterParameterGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterParameterGroupQuotaExceededFault =
  _MatchServiceError redshift "ClusterParameterGroupQuotaExceeded" .
  hasStatus 400


-- | The snapshot copy grant can't be created because a grant with the same name already exists.
--
--
_SnapshotCopyGrantAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyGrantAlreadyExistsFault =
  _MatchServiceError redshift "SnapshotCopyGrantAlreadyExistsFault" .
  hasStatus 400


-- | You do not have permission to publish to the specified Amazon SNS topic.
--
--
_SNSNoAuthorizationFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSNoAuthorizationFault =
  _MatchServiceError redshift "SNSNoAuthorization" . hasStatus 400


-- | The specified cluster is not in the @available@ state.
--
--
_InvalidClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterStateFault =
  _MatchServiceError redshift "InvalidClusterState" . hasStatus 400


-- | The value specified for the @sourceDatabaseName@ , @sourceSchemaName@ , or @sourceTableName@ parameter, or a combination of these, doesn't exist in the snapshot.
--
--
_InvalidTableRestoreArgumentFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTableRestoreArgumentFault =
  _MatchServiceError redshift "InvalidTableRestoreArgument" . hasStatus 400


-- | The specified snapshot copy grant can't be found. Make sure that the name is typed correctly and that the grant exists in the destination region.
--
--
_SnapshotCopyGrantNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyGrantNotFoundFault =
  _MatchServiceError redshift "SnapshotCopyGrantNotFoundFault" . hasStatus 400


-- | The quota for HSM configurations has been reached. For information about increasing your quota, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
--
_HSMConfigurationQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMConfigurationQuotaExceededFault =
  _MatchServiceError redshift "HsmConfigurationQuotaExceededFault" .
  hasStatus 400


-- | The request would result in the user exceeding the allowed number of cluster snapshots.
--
--
_ClusterSnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSnapshotQuotaExceededFault =
  _MatchServiceError redshift "ClusterSnapshotQuotaExceeded" . hasStatus 400


-- | The number of nodes specified exceeds the allotted capacity of the cluster.
--
--
_InsufficientClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientClusterCapacityFault =
  _MatchServiceError redshift "InsufficientClusterCapacity" . hasStatus 400


-- | Amazon SNS has responded that there is a problem with the specified Amazon SNS topic.
--
--
_SNSInvalidTopicFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSInvalidTopicFault =
  _MatchServiceError redshift "SNSInvalidTopic" . hasStatus 400


-- | Your request cannot be completed because a dependent internal service is temporarily unavailable. Wait 30 to 60 seconds and try again.
--
--
_DependentServiceUnavailableFault :: AsError a => Getting (First ServiceError) a ServiceError
_DependentServiceUnavailableFault =
  _MatchServiceError redshift "DependentServiceUnavailableFault" . hasStatus 400


-- | A request option was specified that is not supported.
--
--
_UnsupportedOptionFault :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOptionFault =
  _MatchServiceError redshift "UnsupportedOptionFault" . hasStatus 400


-- | There is already an existing event notification subscription with the specified name.
--
--
_SubscriptionAlreadyExistFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionAlreadyExistFault =
  _MatchServiceError redshift "SubscriptionAlreadyExist" . hasStatus 400


-- | The cluster subnet group does not cover all Availability Zones.
--
--
_InvalidVPCNetworkStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
  _MatchServiceError redshift "InvalidVPCNetworkStateFault" . hasStatus 400


-- | The cluster subnet group name does not refer to an existing cluster subnet group.
--
--
_ClusterSubnetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetGroupNotFoundFault =
  _MatchServiceError redshift "ClusterSubnetGroupNotFoundFault" . hasStatus 400


-- | Could not find the specified S3 bucket.
--
--
_BucketNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_BucketNotFoundFault =
  _MatchServiceError redshift "BucketNotFoundFault" . hasStatus 400


-- | The subscription request is invalid because it is a duplicate request. This subscription request is already in progress.
--
--
_InvalidSubscriptionStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubscriptionStateFault =
  _MatchServiceError redshift "InvalidSubscriptionStateFault" . hasStatus 400


-- | The request cannot be completed because a dependent service is throttling requests made by Amazon Redshift on your behalf. Wait and retry the request.
--
--
_DependentServiceRequestThrottlingFault :: AsError a => Getting (First ServiceError) a ServiceError
_DependentServiceRequestThrottlingFault =
  _MatchServiceError redshift "DependentServiceRequestThrottlingFault" .
  hasStatus 400


-- | The specified CIDR IP range or EC2 security group is not authorized for the specified cluster security group.
--
--
_AuthorizationNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault =
  _MatchServiceError redshift "AuthorizationNotFound" . hasStatus 404


-- | The cluster subnet group cannot be deleted because it is in use.
--
--
_InvalidClusterSubnetGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSubnetGroupStateFault =
  _MatchServiceError redshift "InvalidClusterSubnetGroupStateFault" .
  hasStatus 400


-- | The requested operation isn't supported.
--
--
_UnsupportedOperationFault :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationFault =
  _MatchServiceError redshift "UnsupportedOperation" . hasStatus 400


-- | A /ClusterSubnetGroupName/ is already used by an existing cluster subnet group.
--
--
_ClusterSubnetGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetGroupAlreadyExistsFault =
  _MatchServiceError redshift "ClusterSubnetGroupAlreadyExists" . hasStatus 400


-- | The specified cluster snapshot is not in the @available@ state, or other accounts are authorized to access the snapshot.
--
--
_InvalidClusterSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSnapshotStateFault =
  _MatchServiceError redshift "InvalidClusterSnapshotState" . hasStatus 400


-- | The cluster security group name does not refer to an existing cluster security group.
--
--
_ClusterSecurityGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSecurityGroupNotFoundFault =
  _MatchServiceError redshift "ClusterSecurityGroupNotFound" . hasStatus 404


-- | The specified reserved compute node not found.
--
--
_ReservedNodeNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedNodeNotFoundFault =
  _MatchServiceError redshift "ReservedNodeNotFound" . hasStatus 404


-- | Specified offering does not exist.
--
--
_ReservedNodeOfferingNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedNodeOfferingNotFoundFault =
  _MatchServiceError redshift "ReservedNodeOfferingNotFound" . hasStatus 404


-- | The state of the subnet is invalid.
--
--
_InvalidClusterSubnetStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSubnetStateFault =
  _MatchServiceError redshift "InvalidClusterSubnetStateFault" . hasStatus 400


-- | The specified options are incompatible.
--
--
_IncompatibleOrderableOptions :: AsError a => Getting (First ServiceError) a ServiceError
_IncompatibleOrderableOptions =
  _MatchServiceError redshift "IncompatibleOrderableOptions" . hasStatus 400


-- | The request would exceed the allowed number of event subscriptions for this account. For information about increasing your quota, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
--
_EventSubscriptionQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_EventSubscriptionQuotaExceededFault =
  _MatchServiceError redshift "EventSubscriptionQuotaExceeded" . hasStatus 400


-- | The cluster parameter group action can not be completed because another task is in progress that involves the parameter group. Wait a few moments and try the operation again.
--
--
_InvalidClusterParameterGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterParameterGroupStateFault =
  _MatchServiceError redshift "InvalidClusterParameterGroupState" .
  hasStatus 400


-- | User already has a reservation with the given identifier.
--
--
_ReservedNodeAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedNodeAlreadyExistsFault =
  _MatchServiceError redshift "ReservedNodeAlreadyExists" . hasStatus 404


-- | You have exceeded the allowed number of table restore requests. Wait for your current table restore requests to complete before making a new request.
--
--
_InProgressTableRestoreQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_InProgressTableRestoreQuotaExceededFault =
  _MatchServiceError redshift "InProgressTableRestoreQuotaExceededFault" .
  hasStatus 400


-- | The restore is invalid.
--
--
_InvalidRestoreFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRestoreFault =
  _MatchServiceError redshift "InvalidRestore" . hasStatus 406


-- | The resource could not be found.
--
--
_ResourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault =
  _MatchServiceError redshift "ResourceNotFoundFault" . hasStatus 404


-- | An Amazon Redshift event with the specified event ID does not exist.
--
--
_SubscriptionEventIdNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionEventIdNotFoundFault =
  _MatchServiceError redshift "SubscriptionEventIdNotFound" . hasStatus 404


-- | The snapshot copy grant can't be deleted because it is used by one or more clusters.
--
--
_InvalidSnapshotCopyGrantStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSnapshotCopyGrantStateFault =
  _MatchServiceError redshift "InvalidSnapshotCopyGrantStateFault" .
  hasStatus 400


-- | The specified region is incorrect or does not exist.
--
--
_UnknownSnapshotCopyRegionFault :: AsError a => Getting (First ServiceError) a ServiceError
_UnknownSnapshotCopyRegionFault =
  _MatchServiceError redshift "UnknownSnapshotCopyRegionFault" . hasStatus 404


-- | Request would exceed the user's compute node quota. For information about increasing your quota, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
--
_ReservedNodeQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedNodeQuotaExceededFault =
  _MatchServiceError redshift "ReservedNodeQuotaExceeded" . hasStatus 400


-- | The request would result in user exceeding the allowed number of subnets in a cluster subnet groups. For information about increasing your quota, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
--
_ClusterSubnetQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetQuotaExceededFault =
  _MatchServiceError redshift "ClusterSubnetQuotaExceededFault" . hasStatus 400


-- | The account already has a cluster with the given identifier.
--
--
_ClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterAlreadyExistsFault =
  _MatchServiceError redshift "ClusterAlreadyExists" . hasStatus 400


-- | The owner of the specified snapshot has not authorized your account to access the snapshot.
--
--
_AccessToSnapshotDeniedFault :: AsError a => Getting (First ServiceError) a ServiceError
_AccessToSnapshotDeniedFault =
  _MatchServiceError redshift "AccessToSnapshotDenied" . hasStatus 400


-- | The request exceeds the limit of 10 tags for the resource.
--
--
_TagLimitExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_TagLimitExceededFault =
  _MatchServiceError redshift "TagLimitExceededFault" . hasStatus 400


-- | The operation would exceed the number of nodes allotted to the account. For information about increasing your quota, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
--
_NumberOfNodesQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NumberOfNodesQuotaExceededFault =
  _MatchServiceError redshift "NumberOfNodesQuotaExceeded" . hasStatus 400


-- | There is already an existing Amazon Redshift HSM client certificate with the specified identifier.
--
--
_HSMClientCertificateAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMClientCertificateAlreadyExistsFault =
  _MatchServiceError redshift "HsmClientCertificateAlreadyExistsFault" .
  hasStatus 400


-- | The specified HSM client certificate is not in the @available@ state, or it is still in use by one or more Amazon Redshift clusters.
--
--
_InvalidHSMClientCertificateStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidHSMClientCertificateStateFault =
  _MatchServiceError redshift "InvalidHsmClientCertificateStateFault" .
  hasStatus 400


-- | A specified subnet is already in use by another cluster.
--
--
_SubnetAlreadyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse =
  _MatchServiceError redshift "SubnetAlreadyInUse" . hasStatus 400


-- | The value specified for the event severity was not one of the allowed values, or it specified a severity that does not apply to the specified source type. The allowed values are ERROR and INFO.
--
--
_SubscriptionSeverityNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionSeverityNotFoundFault =
  _MatchServiceError redshift "SubscriptionSeverityNotFound" . hasStatus 404


-- | Your account is not authorized to perform the requested operation.
--
--
_UnauthorizedOperation :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedOperation =
  _MatchServiceError redshift "UnauthorizedOperation" . hasStatus 400


-- | The tag is invalid.
--
--
_InvalidTagFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagFault = _MatchServiceError redshift "InvalidTagFault" . hasStatus 400


-- | The cluster does not have read bucket or put object permissions on the S3 bucket specified when enabling logging.
--
--
_InsufficientS3BucketPolicyFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientS3BucketPolicyFault =
  _MatchServiceError redshift "InsufficientS3BucketPolicyFault" . hasStatus 400


-- | The request would result in user exceeding the allowed number of cluster subnet groups. For information about increasing your quota, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
--
_ClusterSubnetGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetGroupQuotaExceededFault =
  _MatchServiceError redshift "ClusterSubnetGroupQuotaExceeded" . hasStatus 400

