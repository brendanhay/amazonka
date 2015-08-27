{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types
    (
    -- * Service Configuration
      redshift

    -- * Errors
    , _LimitExceededFault
    , _ClusterSecurityGroupQuotaExceededFault
    , _CopyToRegionDisabledFault
    , _AuthorizationQuotaExceededFault
    , _SourceNotFoundFault
    , _InvalidS3KeyPrefixFault
    , _AuthorizationAlreadyExistsFault
    , _InvalidClusterSecurityGroupStateFault
    , _ClusterSecurityGroupAlreadyExistsFault
    , _InvalidElasticIPFault
    , _ClusterSnapshotNotFoundFault
    , _HSMConfigurationNotFoundFault
    , _InvalidHSMConfigurationStateFault
    , _ClusterSnapshotAlreadyExistsFault
    , _HSMConfigurationAlreadyExistsFault
    , _SubscriptionCategoryNotFoundFault
    , _SubscriptionNotFoundFault
    , _InvalidSubnet
    , _InvalidS3BucketNameFault
    , _ClusterQuotaExceededFault
    , _SnapshotCopyAlreadyDisabledFault
    , _ClusterParameterGroupNotFoundFault
    , _HSMClientCertificateQuotaExceededFault
    , _SnapshotCopyGrantQuotaExceededFault
    , _SnapshotCopyAlreadyEnabledFault
    , _ClusterParameterGroupAlreadyExistsFault
    , _NumberOfNodesPerClusterLimitExceededFault
    , _SnapshotCopyDisabledFault
    , _ResizeNotFoundFault
    , _ClusterParameterGroupQuotaExceededFault
    , _SNSTopicARNNotFoundFault
    , _HSMClientCertificateNotFoundFault
    , _SnapshotCopyGrantAlreadyExistsFault
    , _ClusterNotFoundFault
    , _SNSNoAuthorizationFault
    , _SnapshotCopyGrantNotFoundFault
    , _InvalidClusterStateFault
    , _InsufficientClusterCapacityFault
    , _HSMConfigurationQuotaExceededFault
    , _ClusterSnapshotQuotaExceededFault
    , _SNSInvalidTopicFault
    , _UnsupportedOptionFault
    , _SubscriptionAlreadyExistFault
    , _InvalidVPCNetworkStateFault
    , _ClusterSubnetGroupNotFoundFault
    , _BucketNotFoundFault
    , _InvalidSubscriptionStateFault
    , _UnsupportedOperationFault
    , _AuthorizationNotFoundFault
    , _InvalidClusterSubnetGroupStateFault
    , _InvalidClusterSnapshotStateFault
    , _ClusterSubnetGroupAlreadyExistsFault
    , _ClusterSecurityGroupNotFoundFault
    , _IncompatibleOrderableOptions
    , _ReservedNodeOfferingNotFoundFault
    , _InvalidClusterSubnetStateFault
    , _ReservedNodeNotFoundFault
    , _EventSubscriptionQuotaExceededFault
    , _InvalidClusterParameterGroupStateFault
    , _ReservedNodeAlreadyExistsFault
    , _ReservedNodeQuotaExceededFault
    , _SubscriptionEventIdNotFoundFault
    , _ResourceNotFoundFault
    , _InvalidSnapshotCopyGrantStateFault
    , _InvalidRestoreFault
    , _UnknownSnapshotCopyRegionFault
    , _AccessToSnapshotDeniedFault
    , _ClusterAlreadyExistsFault
    , _HSMClientCertificateAlreadyExistsFault
    , _InvalidHSMClientCertificateStateFault
    , _NumberOfNodesQuotaExceededFault
    , _TagLimitExceededFault
    , _ClusterSubnetQuotaExceededFault
    , _SubnetAlreadyInUse
    , _InvalidTagFault
    , _InsufficientS3BucketPolicyFault
    , _ClusterSubnetGroupQuotaExceededFault
    , _SubscriptionSeverityNotFoundFault
    , _UnauthorizedOperation

    -- * ParameterApplyType
    , ParameterApplyType (..)

    -- * SourceType
    , SourceType (..)

    -- * AccountWithRestoreAccess
    , AccountWithRestoreAccess
    , accountWithRestoreAccess
    , awraAccountId

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- * Cluster
    , Cluster
    , cluster
    , cRestoreStatus
    , cClusterSnapshotCopyStatus
    , cClusterRevisionNumber
    , cMasterUsername
    , cPubliclyAccessible
    , cVPCId
    , cClusterSecurityGroups
    , cAutomatedSnapshotRetentionPeriod
    , cEncrypted
    , cClusterIdentifier
    , cNumberOfNodes
    , cClusterSubnetGroupName
    , cPreferredMaintenanceWindow
    , cModifyStatus
    , cClusterPublicKey
    , cClusterParameterGroups
    , cAvailabilityZone
    , cVPCSecurityGroups
    , cKMSKeyId
    , cHSMStatus
    , cElasticIPStatus
    , cClusterVersion
    , cNodeType
    , cEndpoint
    , cClusterCreateTime
    , cAllowVersionUpgrade
    , cPendingModifiedValues
    , cClusterStatus
    , cDBName
    , cTags
    , cClusterNodes

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
    , cpgParameterGroupName
    , cpgTags

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
    , dcpParameters
    , dcpMarker
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
    , esCustomerAWSId
    , esStatus
    , esCustSubscriptionId
    , esSNSTopicARN
    , esEnabled
    , esSourceType
    , esSeverity
    , esSubscriptionCreationTime
    , esEventCategoriesList
    , esSourceIdsList
    , esTags

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
    , hcHSMIPAddress
    , hcTags

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
    , lsLastSuccessfulDeliveryTime
    , lsLastFailureTime
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
    , pAllowedValues
    , pDataType
    , pParameterName
    , pDescription

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvMasterUserPassword
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
    , rnState
    , rnCurrencyCode
    , rnStartTime
    , rnNodeCount
    , rnReservedNodeOfferingId
    , rnReservedNodeId
    , rnOfferingType
    , rnUsagePrice
    , rnNodeType
    , rnRecurringCharges
    , rnFixedPrice
    , rnDuration

    -- * ReservedNodeOffering
    , ReservedNodeOffering
    , reservedNodeOffering
    , rnoCurrencyCode
    , rnoReservedNodeOfferingId
    , rnoOfferingType
    , rnoUsagePrice
    , rnoNodeType
    , rnoRecurringCharges
    , rnoFixedPrice
    , rnoDuration

    -- * RestoreStatus
    , RestoreStatus
    , restoreStatus
    , rsEstimatedTimeToCompletionInSeconds
    , rsStatus
    , rsCurrentRestoreRateInMegaBytesPerSecond
    , rsProgressInMegaBytes
    , rsElapsedTimeInSeconds
    , rsSnapshotSizeInMegaBytes

    -- * Snapshot
    , Snapshot
    , snapshot
    , sRestorableNodeTypes
    , sStatus
    , sAccountsWithRestoreAccess
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
    , sAvailabilityZone
    , sKMSKeyId
    , sCurrentBackupRateInMegaBytesPerSecond
    , sSnapshotCreateTime
    , sClusterVersion
    , sOwnerAccount
    , sNodeType
    , sClusterCreateTime
    , sElapsedTimeInSeconds
    , sEstimatedSecondsToCompletion
    , sTotalBackupSizeInMegaBytes
    , sDBName
    , sTags
    , sActualIncrementalBackupSizeInMegaBytes
    , sPort

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

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TaggedResource
    , TaggedResource
    , taggedResource
    , trResourceType
    , trTag
    , trResourceName

    -- * VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Redshift.Types.Sum
import           Network.AWS.Sign.V4

-- | API version '2012-12-01' of the Amazon Redshift SDK configuration.
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
    , _svcError = parseXMLError
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

-- | The encryption key has exceeded its grant limit in AWS KMS.
_LimitExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededFault =
    _ServiceError . hasStatus 400 . hasCode "LimitExceededFault"

-- | The request would result in the user exceeding the allowed number of
-- cluster security groups. For information about increasing your quota, go
-- to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSecurityGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSecurityGroupQuotaExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "QuotaExceeded.ClusterSecurityGroup"

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
_CopyToRegionDisabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_CopyToRegionDisabledFault =
    _ServiceError . hasStatus 400 . hasCode "CopyToRegionDisabledFault"

-- | The authorization quota for the cluster security group has been reached.
_AuthorizationQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "AuthorizationQuotaExceeded"

-- | The specified Amazon Redshift event source could not be found.
_SourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SourceNotFoundFault = _ServiceError . hasStatus 404 . hasCode "SourceNotFound"

-- | The string specified for the logging S3 key prefix does not comply with
-- the documented constraints.
_InvalidS3KeyPrefixFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3KeyPrefixFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidS3KeyPrefixFault"

-- | The specified CIDR block or EC2 security group is already authorized for
-- the specified cluster security group.
_AuthorizationAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "AuthorizationAlreadyExists"

-- | The state of the cluster security group is not 'available'.
_InvalidClusterSecurityGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSecurityGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidClusterSecurityGroupState"

-- | A cluster security group with the same name already exists.
_ClusterSecurityGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSecurityGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSecurityGroupAlreadyExists"

-- | The Elastic IP (EIP) is invalid or cannot be found.
_InvalidElasticIPFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidElasticIPFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidElasticIpFault"

-- | The snapshot identifier does not refer to an existing cluster snapshot.
_ClusterSnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSnapshotNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ClusterSnapshotNotFound"

-- | There is no Amazon Redshift HSM configuration with the specified
-- identifier.
_HSMConfigurationNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMConfigurationNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "HsmConfigurationNotFoundFault"

-- | The specified HSM configuration is not in the 'available' state, or it
-- is still in use by one or more Amazon Redshift clusters.
_InvalidHSMConfigurationStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidHSMConfigurationStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidHsmConfigurationStateFault"

-- | The value specified as a snapshot identifier is already used by an
-- existing snapshot.
_ClusterSnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSnapshotAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSnapshotAlreadyExists"

-- | There is already an existing Amazon Redshift HSM configuration with the
-- specified identifier.
_HSMConfigurationAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMConfigurationAlreadyExistsFault =
    _ServiceError .
    hasStatus 400 . hasCode "HsmConfigurationAlreadyExistsFault"

-- | The value specified for the event category was not one of the allowed
-- values, or it specified a category that does not apply to the specified
-- source type. The allowed values are Configuration, Management,
-- Monitoring, and Security.
_SubscriptionCategoryNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionCategoryNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionCategoryNotFound"

-- | An Amazon Redshift event notification subscription with the specified
-- name does not exist.
_SubscriptionNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionNotFound"

-- | The requested subnet is not valid, or not all of the subnets are in the
-- same VPC.
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _ServiceError . hasStatus 400 . hasCode "InvalidSubnet"

-- | The S3 bucket name is invalid. For more information about naming rules,
-- go to
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
-- in the Amazon Simple Storage Service (S3) Developer Guide.
_InvalidS3BucketNameFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3BucketNameFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidS3BucketNameFault"

-- | The request would exceed the allowed number of cluster instances for
-- this account. For information about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterQuotaExceeded"

-- | The cluster already has cross-region snapshot copy disabled.
_SnapshotCopyAlreadyDisabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyAlreadyDisabledFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotCopyAlreadyDisabledFault"

-- | The parameter group name does not refer to an existing parameter group.
_ClusterParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterParameterGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ClusterParameterGroupNotFound"

-- | The quota for HSM client certificates has been reached. For information
-- about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_HSMClientCertificateQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMClientCertificateQuotaExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "HsmClientCertificateQuotaExceededFault"

-- | The AWS account has exceeded the maximum number of snapshot copy grants
-- in this region.
_SnapshotCopyGrantQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyGrantQuotaExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "SnapshotCopyGrantQuotaExceededFault"

-- | The cluster already has cross-region snapshot copy enabled.
_SnapshotCopyAlreadyEnabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyAlreadyEnabledFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotCopyAlreadyEnabledFault"

-- | A cluster parameter group with the same name already exists.
_ClusterParameterGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterParameterGroupAlreadyExistsFault =
    _ServiceError .
    hasStatus 400 . hasCode "ClusterParameterGroupAlreadyExists"

-- | The operation would exceed the number of nodes allowed for a cluster.
_NumberOfNodesPerClusterLimitExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NumberOfNodesPerClusterLimitExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "NumberOfNodesPerClusterLimitExceeded"

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
_SnapshotCopyDisabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyDisabledFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotCopyDisabledFault"

-- | A resize operation for the specified cluster is not found.
_ResizeNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResizeNotFoundFault = _ServiceError . hasStatus 404 . hasCode "ResizeNotFound"

-- | The request would result in the user exceeding the allowed number of
-- cluster parameter groups. For information about increasing your quota,
-- go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterParameterGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterParameterGroupQuotaExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "ClusterParameterGroupQuotaExceeded"

-- | An Amazon SNS topic with the specified Amazon Resource Name (ARN) does
-- not exist.
_SNSTopicARNNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSTopicARNNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SNSTopicArnNotFound"

-- | There is no Amazon Redshift HSM client certificate with the specified
-- identifier.
_HSMClientCertificateNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMClientCertificateNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "HsmClientCertificateNotFoundFault"

-- | The snapshot copy grant can\'t be created because a grant with the same
-- name already exists.
_SnapshotCopyGrantAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyGrantAlreadyExistsFault =
    _ServiceError .
    hasStatus 400 . hasCode "SnapshotCopyGrantAlreadyExistsFault"

-- | The /ClusterIdentifier/ parameter does not refer to an existing cluster.
_ClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ClusterNotFound"

-- | You do not have permission to publish to the specified Amazon SNS topic.
_SNSNoAuthorizationFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSNoAuthorizationFault =
    _ServiceError . hasStatus 400 . hasCode "SNSNoAuthorization"

-- | The specified snapshot copy grant can\'t be found. Make sure that the
-- name is typed correctly and that the grant exists in the destination
-- region.
_SnapshotCopyGrantNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyGrantNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotCopyGrantNotFoundFault"

-- | The specified cluster is not in the 'available' state.
_InvalidClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidClusterState"

-- | The number of nodes specified exceeds the allotted capacity of the
-- cluster.
_InsufficientClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientClusterCapacityFault =
    _ServiceError . hasStatus 400 . hasCode "InsufficientClusterCapacity"

-- | The quota for HSM configurations has been reached. For information about
-- increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_HSMConfigurationQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMConfigurationQuotaExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "HsmConfigurationQuotaExceededFault"

-- | The request would result in the user exceeding the allowed number of
-- cluster snapshots.
_ClusterSnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSnapshotQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSnapshotQuotaExceeded"

-- | Amazon SNS has responded that there is a problem with the specified
-- Amazon SNS topic.
_SNSInvalidTopicFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSInvalidTopicFault =
    _ServiceError . hasStatus 400 . hasCode "SNSInvalidTopic"

-- | A request option was specified that is not supported.
_UnsupportedOptionFault :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOptionFault =
    _ServiceError . hasStatus 400 . hasCode "UnsupportedOptionFault"

-- | There is already an existing event notification subscription with the
-- specified name.
_SubscriptionAlreadyExistFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionAlreadyExistFault =
    _ServiceError . hasStatus 400 . hasCode "SubscriptionAlreadyExist"

-- | The cluster subnet group does not cover all Availability Zones.
_InvalidVPCNetworkStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidVPCNetworkStateFault"

-- | The cluster subnet group name does not refer to an existing cluster
-- subnet group.
_ClusterSubnetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetGroupNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSubnetGroupNotFoundFault"

-- | Could not find the specified S3 bucket.
_BucketNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_BucketNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "BucketNotFoundFault"

-- | The subscription request is invalid because it is a duplicate request.
-- This subscription request is already in progress.
_InvalidSubscriptionStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubscriptionStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidSubscriptionStateFault"

-- | The requested operation isn\'t supported.
_UnsupportedOperationFault :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationFault =
    _ServiceError . hasStatus 400 . hasCode "UnsupportedOperation"

-- | The specified CIDR IP range or EC2 security group is not authorized for
-- the specified cluster security group.
_AuthorizationNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "AuthorizationNotFound"

-- | The cluster subnet group cannot be deleted because it is in use.
_InvalidClusterSubnetGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSubnetGroupStateFault =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidClusterSubnetGroupStateFault"

-- | The state of the cluster snapshot is not 'available', or other accounts
-- are authorized to access the snapshot.
_InvalidClusterSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSnapshotStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidClusterSnapshotState"

-- | A /ClusterSubnetGroupName/ is already used by an existing cluster subnet
-- group.
_ClusterSubnetGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSubnetGroupAlreadyExists"

-- | The cluster security group name does not refer to an existing cluster
-- security group.
_ClusterSecurityGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSecurityGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ClusterSecurityGroupNotFound"

-- | The specified options are incompatible.
_IncompatibleOrderableOptions :: AsError a => Getting (First ServiceError) a ServiceError
_IncompatibleOrderableOptions =
    _ServiceError . hasStatus 400 . hasCode "IncompatibleOrderableOptions"

-- | Specified offering does not exist.
_ReservedNodeOfferingNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedNodeOfferingNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedNodeOfferingNotFound"

-- | The state of the subnet is invalid.
_InvalidClusterSubnetStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSubnetStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidClusterSubnetStateFault"

-- | The specified reserved compute node not found.
_ReservedNodeNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedNodeNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedNodeNotFound"

-- | The request would exceed the allowed number of event subscriptions for
-- this account. For information about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_EventSubscriptionQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_EventSubscriptionQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "EventSubscriptionQuotaExceeded"

-- | The cluster parameter group action can not be completed because another
-- task is in progress that involves the parameter group. Wait a few
-- moments and try the operation again.
_InvalidClusterParameterGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterParameterGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidClusterParameterGroupState"

-- | User already has a reservation with the given identifier.
_ReservedNodeAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedNodeAlreadyExistsFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedNodeAlreadyExists"

-- | Request would exceed the user\'s compute node quota. For information
-- about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ReservedNodeQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedNodeQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ReservedNodeQuotaExceeded"

-- | An Amazon Redshift event with the specified event ID does not exist.
_SubscriptionEventIdNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionEventIdNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionEventIdNotFound"

-- | The resource could not be found.
_ResourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundFault"

-- | The snapshot copy grant can\'t be deleted because it is used by one or
-- more clusters.
_InvalidSnapshotCopyGrantStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSnapshotCopyGrantStateFault =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidSnapshotCopyGrantStateFault"

-- | The restore is invalid.
_InvalidRestoreFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRestoreFault = _ServiceError . hasStatus 406 . hasCode "InvalidRestore"

-- | The specified region is incorrect or does not exist.
_UnknownSnapshotCopyRegionFault :: AsError a => Getting (First ServiceError) a ServiceError
_UnknownSnapshotCopyRegionFault =
    _ServiceError . hasStatus 404 . hasCode "UnknownSnapshotCopyRegionFault"

-- | The owner of the specified snapshot has not authorized your account to
-- access the snapshot.
_AccessToSnapshotDeniedFault :: AsError a => Getting (First ServiceError) a ServiceError
_AccessToSnapshotDeniedFault =
    _ServiceError . hasStatus 400 . hasCode "AccessToSnapshotDenied"

-- | The account already has a cluster with the given identifier.
_ClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterAlreadyExists"

-- | There is already an existing Amazon Redshift HSM client certificate with
-- the specified identifier.
_HSMClientCertificateAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_HSMClientCertificateAlreadyExistsFault =
    _ServiceError .
    hasStatus 400 . hasCode "HsmClientCertificateAlreadyExistsFault"

-- | The specified HSM client certificate is not in the 'available' state, or
-- it is still in use by one or more Amazon Redshift clusters.
_InvalidHSMClientCertificateStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidHSMClientCertificateStateFault =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidHsmClientCertificateStateFault"

-- | The operation would exceed the number of nodes allotted to the account.
-- For information about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_NumberOfNodesQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NumberOfNodesQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "NumberOfNodesQuotaExceeded"

-- | The request exceeds the limit of 10 tags for the resource.
_TagLimitExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_TagLimitExceededFault =
    _ServiceError . hasStatus 400 . hasCode "TagLimitExceededFault"

-- | The request would result in user exceeding the allowed number of subnets
-- in a cluster subnet groups. For information about increasing your quota,
-- go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSubnetQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSubnetQuotaExceededFault"

-- | A specified subnet is already in use by another cluster.
_SubnetAlreadyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse =
    _ServiceError . hasStatus 400 . hasCode "SubnetAlreadyInUse"

-- | The tag is invalid.
_InvalidTagFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagFault = _ServiceError . hasStatus 400 . hasCode "InvalidTagFault"

-- | The cluster does not have read bucket or put object permissions on the
-- S3 bucket specified when enabling logging.
_InsufficientS3BucketPolicyFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientS3BucketPolicyFault =
    _ServiceError . hasStatus 400 . hasCode "InsufficientS3BucketPolicyFault"

-- | The request would result in user exceeding the allowed number of cluster
-- subnet groups. For information about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSubnetGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSubnetGroupQuotaExceeded"

-- | The value specified for the event severity was not one of the allowed
-- values, or it specified a severity that does not apply to the specified
-- source type. The allowed values are ERROR and INFO.
_SubscriptionSeverityNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionSeverityNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionSeverityNotFound"

-- | Your account is not authorized to perform the requested operation.
_UnauthorizedOperation :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedOperation =
    _ServiceError . hasStatus 400 . hasCode "UnauthorizedOperation"
