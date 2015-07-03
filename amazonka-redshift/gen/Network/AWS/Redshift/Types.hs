{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.Redshift.Types
    (
    -- * Service
      Redshift

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
    , cluRestoreStatus
    , cluClusterSnapshotCopyStatus
    , cluClusterRevisionNumber
    , cluMasterUsername
    , cluPubliclyAccessible
    , cluVPCId
    , cluClusterSecurityGroups
    , cluAutomatedSnapshotRetentionPeriod
    , cluEncrypted
    , cluClusterIdentifier
    , cluNumberOfNodes
    , cluClusterSubnetGroupName
    , cluPreferredMaintenanceWindow
    , cluModifyStatus
    , cluClusterPublicKey
    , cluClusterParameterGroups
    , cluAvailabilityZone
    , cluVPCSecurityGroups
    , cluKMSKeyId
    , cluHSMStatus
    , cluElasticIPStatus
    , cluClusterVersion
    , cluNodeType
    , cluEndpoint
    , cluClusterCreateTime
    , cluAllowVersionUpgrade
    , cluPendingModifiedValues
    , cluClusterStatus
    , cluDBName
    , cluTags
    , cluClusterNodes

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
    , cClusterSecurityGroupName
    , cIPRanges
    , cEC2SecurityGroups
    , cDescription
    , cTags

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
    , endAddress
    , endPort

    -- * Event
    , Event
    , event
    , eveSourceType
    , eveSeverity
    , eveSourceIdentifier
    , eveDate
    , eveEventCategories
    , eveMessage
    , eveEventId

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
    , irCIDRIP
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
    , parApplyType
    , parParameterValue
    , parMinimumEngineVersion
    , parSource
    , parIsModifiable
    , parAllowedValues
    , parDataType
    , parParameterName
    , parDescription

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
    , snaRestorableNodeTypes
    , snaStatus
    , snaAccountsWithRestoreAccess
    , snaSnapshotIdentifier
    , snaEncryptedWithHSM
    , snaMasterUsername
    , snaSourceRegion
    , snaVPCId
    , snaBackupProgressInMegaBytes
    , snaEncrypted
    , snaClusterIdentifier
    , snaNumberOfNodes
    , snaSnapshotType
    , snaAvailabilityZone
    , snaKMSKeyId
    , snaCurrentBackupRateInMegaBytesPerSecond
    , snaSnapshotCreateTime
    , snaClusterVersion
    , snaOwnerAccount
    , snaNodeType
    , snaClusterCreateTime
    , snaElapsedTimeInSeconds
    , snaEstimatedSecondsToCompletion
    , snaTotalBackupSizeInMegaBytes
    , snaDBName
    , snaTags
    , snaActualIncrementalBackupSizeInMegaBytes
    , snaPort

    -- * SnapshotCopyGrant
    , SnapshotCopyGrant
    , snapshotCopyGrant
    , scgKMSKeyId
    , scgSnapshotCopyGrantName
    , scgTags

    -- * Subnet
    , Subnet
    , subnet
    , subSubnetStatus
    , subSubnetIdentifier
    , subSubnetAvailabilityZone

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
import           Network.AWS.Sign.V4

-- | Version @2012-12-01@ of the Amazon Redshift SDK.
data Redshift

instance AWSService Redshift where
    type Sg Redshift = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Redshift"
            , _svcPrefix = "redshift"
            , _svcVersion = "2012-12-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
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
_LimitExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededFault =
    _ServiceError . hasStatus 400 . hasCode "LimitExceededFault"

-- | The request would result in the user exceeding the allowed number of
-- cluster security groups. For information about increasing your quota, go
-- to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSecurityGroupQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterSecurityGroupQuotaExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "QuotaExceeded.ClusterSecurityGroup"

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
_CopyToRegionDisabledFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CopyToRegionDisabledFault =
    _ServiceError . hasStatus 400 . hasCode "CopyToRegionDisabledFault"

-- | The authorization quota for the cluster security group has been reached.
_AuthorizationQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_AuthorizationQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "AuthorizationQuotaExceeded"

-- | The specified Amazon Redshift event source could not be found.
_SourceNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SourceNotFoundFault = _ServiceError . hasStatus 404 . hasCode "SourceNotFound"

-- | The string specified for the logging S3 key prefix does not comply with
-- the documented constraints.
_InvalidS3KeyPrefixFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidS3KeyPrefixFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidS3KeyPrefixFault"

-- | The specified CIDR block or EC2 security group is already authorized for
-- the specified cluster security group.
_AuthorizationAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "AuthorizationAlreadyExists"

-- | The state of the cluster security group is not @available@.
_InvalidClusterSecurityGroupStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSecurityGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidClusterSecurityGroupState"

-- | A cluster security group with the same name already exists.
_ClusterSecurityGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterSecurityGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSecurityGroupAlreadyExists"

-- | The Elastic IP (EIP) is invalid or cannot be found.
_InvalidElasticIPFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidElasticIPFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidElasticIpFault"

-- | The snapshot identifier does not refer to an existing cluster snapshot.
_ClusterSnapshotNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterSnapshotNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ClusterSnapshotNotFound"

-- | There is no Amazon Redshift HSM configuration with the specified
-- identifier.
_HSMConfigurationNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_HSMConfigurationNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "HsmConfigurationNotFoundFault"

-- | The specified HSM configuration is not in the @available@ state, or it
-- is still in use by one or more Amazon Redshift clusters.
_InvalidHSMConfigurationStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidHSMConfigurationStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidHsmConfigurationStateFault"

-- | The value specified as a snapshot identifier is already used by an
-- existing snapshot.
_ClusterSnapshotAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterSnapshotAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSnapshotAlreadyExists"

-- | There is already an existing Amazon Redshift HSM configuration with the
-- specified identifier.
_HSMConfigurationAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_HSMConfigurationAlreadyExistsFault =
    _ServiceError .
    hasStatus 400 . hasCode "HsmConfigurationAlreadyExistsFault"

-- | The value specified for the event category was not one of the allowed
-- values, or it specified a category that does not apply to the specified
-- source type. The allowed values are Configuration, Management,
-- Monitoring, and Security.
_SubscriptionCategoryNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SubscriptionCategoryNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionCategoryNotFound"

-- | An Amazon Redshift event notification subscription with the specified
-- name does not exist.
_SubscriptionNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SubscriptionNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionNotFound"

-- | The requested subnet is not valid, or not all of the subnets are in the
-- same VPC.
_InvalidSubnet :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _ServiceError . hasStatus 400 . hasCode "InvalidSubnet"

-- | The S3 bucket name is invalid. For more information about naming rules,
-- go to
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
-- in the Amazon Simple Storage Service (S3) Developer Guide.
_InvalidS3BucketNameFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidS3BucketNameFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidS3BucketNameFault"

-- | The request would exceed the allowed number of cluster instances for
-- this account. For information about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterQuotaExceeded"

-- | The cluster already has cross-region snapshot copy disabled.
_SnapshotCopyAlreadyDisabledFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyAlreadyDisabledFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotCopyAlreadyDisabledFault"

-- | The parameter group name does not refer to an existing parameter group.
_ClusterParameterGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterParameterGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ClusterParameterGroupNotFound"

-- | The quota for HSM client certificates has been reached. For information
-- about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_HSMClientCertificateQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_HSMClientCertificateQuotaExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "HsmClientCertificateQuotaExceededFault"

-- | The AWS account has exceeded the maximum number of snapshot copy grants
-- in this region.
_SnapshotCopyGrantQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyGrantQuotaExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "SnapshotCopyGrantQuotaExceededFault"

-- | The cluster already has cross-region snapshot copy enabled.
_SnapshotCopyAlreadyEnabledFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyAlreadyEnabledFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotCopyAlreadyEnabledFault"

-- | A cluster parameter group with the same name already exists.
_ClusterParameterGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterParameterGroupAlreadyExistsFault =
    _ServiceError .
    hasStatus 400 . hasCode "ClusterParameterGroupAlreadyExists"

-- | The operation would exceed the number of nodes allowed for a cluster.
_NumberOfNodesPerClusterLimitExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_NumberOfNodesPerClusterLimitExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "NumberOfNodesPerClusterLimitExceeded"

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
_SnapshotCopyDisabledFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyDisabledFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotCopyDisabledFault"

-- | A resize operation for the specified cluster is not found.
_ResizeNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ResizeNotFoundFault = _ServiceError . hasStatus 404 . hasCode "ResizeNotFound"

-- | The request would result in the user exceeding the allowed number of
-- cluster parameter groups. For information about increasing your quota,
-- go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterParameterGroupQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterParameterGroupQuotaExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "ClusterParameterGroupQuotaExceeded"

-- | An Amazon SNS topic with the specified Amazon Resource Name (ARN) does
-- not exist.
_SNSTopicARNNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SNSTopicARNNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SNSTopicArnNotFound"

-- | There is no Amazon Redshift HSM client certificate with the specified
-- identifier.
_HSMClientCertificateNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_HSMClientCertificateNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "HsmClientCertificateNotFoundFault"

-- | The snapshot copy grant can\'t be created because a grant with the same
-- name already exists.
_SnapshotCopyGrantAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyGrantAlreadyExistsFault =
    _ServiceError .
    hasStatus 400 . hasCode "SnapshotCopyGrantAlreadyExistsFault"

-- | The /ClusterIdentifier/ parameter does not refer to an existing cluster.
_ClusterNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ClusterNotFound"

-- | You do not have permission to publish to the specified Amazon SNS topic.
_SNSNoAuthorizationFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SNSNoAuthorizationFault =
    _ServiceError . hasStatus 400 . hasCode "SNSNoAuthorization"

-- | The specified snapshot copy grant can\'t be found. Make sure that the
-- name is typed correctly and that the grant exists in the destination
-- region.
_SnapshotCopyGrantNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotCopyGrantNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotCopyGrantNotFoundFault"

-- | The specified cluster is not in the @available@ state.
_InvalidClusterStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidClusterStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidClusterState"

-- | The number of nodes specified exceeds the allotted capacity of the
-- cluster.
_InsufficientClusterCapacityFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientClusterCapacityFault =
    _ServiceError . hasStatus 400 . hasCode "InsufficientClusterCapacity"

-- | The quota for HSM configurations has been reached. For information about
-- increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_HSMConfigurationQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_HSMConfigurationQuotaExceededFault =
    _ServiceError .
    hasStatus 400 . hasCode "HsmConfigurationQuotaExceededFault"

-- | The request would result in the user exceeding the allowed number of
-- cluster snapshots.
_ClusterSnapshotQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterSnapshotQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSnapshotQuotaExceeded"

-- | Amazon SNS has responded that there is a problem with the specified
-- Amazon SNS topic.
_SNSInvalidTopicFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SNSInvalidTopicFault =
    _ServiceError . hasStatus 400 . hasCode "SNSInvalidTopic"

-- | A request option was specified that is not supported.
_UnsupportedOptionFault :: AWSError a => Getting (First ServiceError) a ServiceError
_UnsupportedOptionFault =
    _ServiceError . hasStatus 400 . hasCode "UnsupportedOptionFault"

-- | There is already an existing event notification subscription with the
-- specified name.
_SubscriptionAlreadyExistFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SubscriptionAlreadyExistFault =
    _ServiceError . hasStatus 400 . hasCode "SubscriptionAlreadyExist"

-- | The cluster subnet group does not cover all Availability Zones.
_InvalidVPCNetworkStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidVPCNetworkStateFault"

-- | The cluster subnet group name does not refer to an existing cluster
-- subnet group.
_ClusterSubnetGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetGroupNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSubnetGroupNotFoundFault"

-- | Could not find the specified S3 bucket.
_BucketNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_BucketNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "BucketNotFoundFault"

-- | The subscription request is invalid because it is a duplicate request.
-- This subscription request is already in progress.
_InvalidSubscriptionStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidSubscriptionStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidSubscriptionStateFault"

-- | The requested operation isn\'t supported.
_UnsupportedOperationFault :: AWSError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationFault =
    _ServiceError . hasStatus 400 . hasCode "UnsupportedOperation"

-- | The specified CIDR IP range or EC2 security group is not authorized for
-- the specified cluster security group.
_AuthorizationNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "AuthorizationNotFound"

-- | The cluster subnet group cannot be deleted because it is in use.
_InvalidClusterSubnetGroupStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSubnetGroupStateFault =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidClusterSubnetGroupStateFault"

-- | The state of the cluster snapshot is not @available@, or other accounts
-- are authorized to access the snapshot.
_InvalidClusterSnapshotStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSnapshotStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidClusterSnapshotState"

-- | A /ClusterSubnetGroupName/ is already used by an existing cluster subnet
-- group.
_ClusterSubnetGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSubnetGroupAlreadyExists"

-- | The cluster security group name does not refer to an existing cluster
-- security group.
_ClusterSecurityGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterSecurityGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ClusterSecurityGroupNotFound"

-- | The specified options are incompatible.
_IncompatibleOrderableOptions :: AWSError a => Getting (First ServiceError) a ServiceError
_IncompatibleOrderableOptions =
    _ServiceError . hasStatus 400 . hasCode "IncompatibleOrderableOptions"

-- | Specified offering does not exist.
_ReservedNodeOfferingNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedNodeOfferingNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedNodeOfferingNotFound"

-- | The state of the subnet is invalid.
_InvalidClusterSubnetStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidClusterSubnetStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidClusterSubnetStateFault"

-- | The specified reserved compute node not found.
_ReservedNodeNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedNodeNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedNodeNotFound"

-- | The request would exceed the allowed number of event subscriptions for
-- this account. For information about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_EventSubscriptionQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_EventSubscriptionQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "EventSubscriptionQuotaExceeded"

-- | The cluster parameter group action can not be completed because another
-- task is in progress that involves the parameter group. Wait a few
-- moments and try the operation again.
_InvalidClusterParameterGroupStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidClusterParameterGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidClusterParameterGroupState"

-- | User already has a reservation with the given identifier.
_ReservedNodeAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedNodeAlreadyExistsFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedNodeAlreadyExists"

-- | Request would exceed the user\'s compute node quota. For information
-- about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ReservedNodeQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedNodeQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ReservedNodeQuotaExceeded"

-- | An Amazon Redshift event with the specified event ID does not exist.
_SubscriptionEventIdNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SubscriptionEventIdNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionEventIdNotFound"

-- | The resource could not be found.
_ResourceNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundFault"

-- | The snapshot copy grant can\'t be deleted because it is used by one or
-- more clusters.
_InvalidSnapshotCopyGrantStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidSnapshotCopyGrantStateFault =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidSnapshotCopyGrantStateFault"

-- | The restore is invalid.
_InvalidRestoreFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRestoreFault = _ServiceError . hasStatus 406 . hasCode "InvalidRestore"

-- | The specified region is incorrect or does not exist.
_UnknownSnapshotCopyRegionFault :: AWSError a => Getting (First ServiceError) a ServiceError
_UnknownSnapshotCopyRegionFault =
    _ServiceError . hasStatus 404 . hasCode "UnknownSnapshotCopyRegionFault"

-- | The owner of the specified snapshot has not authorized your account to
-- access the snapshot.
_AccessToSnapshotDeniedFault :: AWSError a => Getting (First ServiceError) a ServiceError
_AccessToSnapshotDeniedFault =
    _ServiceError . hasStatus 400 . hasCode "AccessToSnapshotDenied"

-- | The account already has a cluster with the given identifier.
_ClusterAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterAlreadyExists"

-- | There is already an existing Amazon Redshift HSM client certificate with
-- the specified identifier.
_HSMClientCertificateAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_HSMClientCertificateAlreadyExistsFault =
    _ServiceError .
    hasStatus 400 . hasCode "HsmClientCertificateAlreadyExistsFault"

-- | The specified HSM client certificate is not in the @available@ state, or
-- it is still in use by one or more Amazon Redshift clusters.
_InvalidHSMClientCertificateStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidHSMClientCertificateStateFault =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidHsmClientCertificateStateFault"

-- | The operation would exceed the number of nodes allotted to the account.
-- For information about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_NumberOfNodesQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_NumberOfNodesQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "NumberOfNodesQuotaExceeded"

-- | The request exceeds the limit of 10 tags for the resource.
_TagLimitExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_TagLimitExceededFault =
    _ServiceError . hasStatus 400 . hasCode "TagLimitExceededFault"

-- | The request would result in user exceeding the allowed number of subnets
-- in a cluster subnet groups. For information about increasing your quota,
-- go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSubnetQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSubnetQuotaExceededFault"

-- | A specified subnet is already in use by another cluster.
_SubnetAlreadyInUse :: AWSError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse =
    _ServiceError . hasStatus 400 . hasCode "SubnetAlreadyInUse"

-- | The tag is invalid.
_InvalidTagFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidTagFault = _ServiceError . hasStatus 400 . hasCode "InvalidTagFault"

-- | The cluster does not have read bucket or put object permissions on the
-- S3 bucket specified when enabling logging.
_InsufficientS3BucketPolicyFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientS3BucketPolicyFault =
    _ServiceError . hasStatus 400 . hasCode "InsufficientS3BucketPolicyFault"

-- | The request would result in user exceeding the allowed number of cluster
-- subnet groups. For information about increasing your quota, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSubnetGroupQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterSubnetGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterSubnetGroupQuotaExceeded"

-- | The value specified for the event severity was not one of the allowed
-- values, or it specified a severity that does not apply to the specified
-- source type. The allowed values are ERROR and INFO.
_SubscriptionSeverityNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SubscriptionSeverityNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionSeverityNotFound"

-- | Your account is not authorized to perform the requested operation.
_UnauthorizedOperation :: AWSError a => Getting (First ServiceError) a ServiceError
_UnauthorizedOperation =
    _ServiceError . hasStatus 400 . hasCode "UnauthorizedOperation"

data ParameterApplyType
    = Dynamic
    | Static
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ParameterApplyType where
    parser = takeLowerText >>= \case
        "dynamic" -> pure Dynamic
        "static" -> pure Static
        e -> fromTextError $ "Failure parsing ParameterApplyType from value: '" <> e
           <> "'. Accepted values: dynamic, static"

instance ToText ParameterApplyType where
    toText = \case
        Dynamic -> "dynamic"
        Static -> "static"

instance Hashable ParameterApplyType where
    hashWithSalt = hashUsing fromEnum

instance ToQuery ParameterApplyType
instance ToHeader ParameterApplyType

instance FromXML ParameterApplyType where
    parseXML = parseXMLText "ParameterApplyType"

data SourceType
    = ClusterParameterGroup
    | Cluster
    | ClusterSecurityGroup
    | ClusterSnapshot
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText SourceType where
    parser = takeLowerText >>= \case
        "cluster" -> pure Cluster
        "cluster-parameter-group" -> pure ClusterParameterGroup
        "cluster-security-group" -> pure ClusterSecurityGroup
        "cluster-snapshot" -> pure ClusterSnapshot
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: cluster, cluster-parameter-group, cluster-security-group, cluster-snapshot"

instance ToText SourceType where
    toText = \case
        Cluster -> "cluster"
        ClusterParameterGroup -> "cluster-parameter-group"
        ClusterSecurityGroup -> "cluster-security-group"
        ClusterSnapshot -> "cluster-snapshot"

instance Hashable SourceType where
    hashWithSalt = hashUsing fromEnum

instance ToQuery SourceType
instance ToHeader SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"

-- | Describes an AWS customer account authorized to restore a snapshot.
--
-- /See:/ 'accountWithRestoreAccess' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'awraAccountId'
newtype AccountWithRestoreAccess = AccountWithRestoreAccess'
    { _awraAccountId :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'AccountWithRestoreAccess' smart constructor.
accountWithRestoreAccess :: AccountWithRestoreAccess
accountWithRestoreAccess =
    AccountWithRestoreAccess'
    { _awraAccountId = Nothing
    }

-- | The identifier of an AWS customer account authorized to restore a
-- snapshot.
awraAccountId :: Lens' AccountWithRestoreAccess (Maybe Text)
awraAccountId = lens _awraAccountId (\ s a -> s{_awraAccountId = a});

instance FromXML AccountWithRestoreAccess where
        parseXML x
          = AccountWithRestoreAccess' <$> (x .@? "AccountId")

-- | Describes an availability zone.
--
-- /See:/ 'availabilityZone' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azName'
newtype AvailabilityZone = AvailabilityZone'
    { _azName :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'AvailabilityZone' smart constructor.
availabilityZone :: AvailabilityZone
availabilityZone =
    AvailabilityZone'
    { _azName = Nothing
    }

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\ s a -> s{_azName = a});

instance FromXML AvailabilityZone where
        parseXML x = AvailabilityZone' <$> (x .@? "Name")

-- | Describes a cluster.
--
-- /See:/ 'cluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cluRestoreStatus'
--
-- * 'cluClusterSnapshotCopyStatus'
--
-- * 'cluClusterRevisionNumber'
--
-- * 'cluMasterUsername'
--
-- * 'cluPubliclyAccessible'
--
-- * 'cluVPCId'
--
-- * 'cluClusterSecurityGroups'
--
-- * 'cluAutomatedSnapshotRetentionPeriod'
--
-- * 'cluEncrypted'
--
-- * 'cluClusterIdentifier'
--
-- * 'cluNumberOfNodes'
--
-- * 'cluClusterSubnetGroupName'
--
-- * 'cluPreferredMaintenanceWindow'
--
-- * 'cluModifyStatus'
--
-- * 'cluClusterPublicKey'
--
-- * 'cluClusterParameterGroups'
--
-- * 'cluAvailabilityZone'
--
-- * 'cluVPCSecurityGroups'
--
-- * 'cluKMSKeyId'
--
-- * 'cluHSMStatus'
--
-- * 'cluElasticIPStatus'
--
-- * 'cluClusterVersion'
--
-- * 'cluNodeType'
--
-- * 'cluEndpoint'
--
-- * 'cluClusterCreateTime'
--
-- * 'cluAllowVersionUpgrade'
--
-- * 'cluPendingModifiedValues'
--
-- * 'cluClusterStatus'
--
-- * 'cluDBName'
--
-- * 'cluTags'
--
-- * 'cluClusterNodes'
data Cluster = Cluster'
    { _cluRestoreStatus                    :: !(Maybe RestoreStatus)
    , _cluClusterSnapshotCopyStatus        :: !(Maybe ClusterSnapshotCopyStatus)
    , _cluClusterRevisionNumber            :: !(Maybe Text)
    , _cluMasterUsername                   :: !(Maybe Text)
    , _cluPubliclyAccessible               :: !(Maybe Bool)
    , _cluVPCId                            :: !(Maybe Text)
    , _cluClusterSecurityGroups            :: !(Maybe [ClusterSecurityGroupMembership])
    , _cluAutomatedSnapshotRetentionPeriod :: !(Maybe Int)
    , _cluEncrypted                        :: !(Maybe Bool)
    , _cluClusterIdentifier                :: !(Maybe Text)
    , _cluNumberOfNodes                    :: !(Maybe Int)
    , _cluClusterSubnetGroupName           :: !(Maybe Text)
    , _cluPreferredMaintenanceWindow       :: !(Maybe Text)
    , _cluModifyStatus                     :: !(Maybe Text)
    , _cluClusterPublicKey                 :: !(Maybe Text)
    , _cluClusterParameterGroups           :: !(Maybe [ClusterParameterGroupStatus])
    , _cluAvailabilityZone                 :: !(Maybe Text)
    , _cluVPCSecurityGroups                :: !(Maybe [VPCSecurityGroupMembership])
    , _cluKMSKeyId                         :: !(Maybe Text)
    , _cluHSMStatus                        :: !(Maybe HSMStatus)
    , _cluElasticIPStatus                  :: !(Maybe ElasticIPStatus)
    , _cluClusterVersion                   :: !(Maybe Text)
    , _cluNodeType                         :: !(Maybe Text)
    , _cluEndpoint                         :: !(Maybe Endpoint)
    , _cluClusterCreateTime                :: !(Maybe ISO8601)
    , _cluAllowVersionUpgrade              :: !(Maybe Bool)
    , _cluPendingModifiedValues            :: !(Maybe PendingModifiedValues)
    , _cluClusterStatus                    :: !(Maybe Text)
    , _cluDBName                           :: !(Maybe Text)
    , _cluTags                             :: !(Maybe [Tag])
    , _cluClusterNodes                     :: !(Maybe [ClusterNode])
    } deriving (Eq,Read,Show)

-- | 'Cluster' smart constructor.
cluster :: Cluster
cluster =
    Cluster'
    { _cluRestoreStatus = Nothing
    , _cluClusterSnapshotCopyStatus = Nothing
    , _cluClusterRevisionNumber = Nothing
    , _cluMasterUsername = Nothing
    , _cluPubliclyAccessible = Nothing
    , _cluVPCId = Nothing
    , _cluClusterSecurityGroups = Nothing
    , _cluAutomatedSnapshotRetentionPeriod = Nothing
    , _cluEncrypted = Nothing
    , _cluClusterIdentifier = Nothing
    , _cluNumberOfNodes = Nothing
    , _cluClusterSubnetGroupName = Nothing
    , _cluPreferredMaintenanceWindow = Nothing
    , _cluModifyStatus = Nothing
    , _cluClusterPublicKey = Nothing
    , _cluClusterParameterGroups = Nothing
    , _cluAvailabilityZone = Nothing
    , _cluVPCSecurityGroups = Nothing
    , _cluKMSKeyId = Nothing
    , _cluHSMStatus = Nothing
    , _cluElasticIPStatus = Nothing
    , _cluClusterVersion = Nothing
    , _cluNodeType = Nothing
    , _cluEndpoint = Nothing
    , _cluClusterCreateTime = Nothing
    , _cluAllowVersionUpgrade = Nothing
    , _cluPendingModifiedValues = Nothing
    , _cluClusterStatus = Nothing
    , _cluDBName = Nothing
    , _cluTags = Nothing
    , _cluClusterNodes = Nothing
    }

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
cluRestoreStatus :: Lens' Cluster (Maybe RestoreStatus)
cluRestoreStatus = lens _cluRestoreStatus (\ s a -> s{_cluRestoreStatus = a});

-- | Returns the destination region and retention period that are configured
-- for cross-region snapshot copy.
cluClusterSnapshotCopyStatus :: Lens' Cluster (Maybe ClusterSnapshotCopyStatus)
cluClusterSnapshotCopyStatus = lens _cluClusterSnapshotCopyStatus (\ s a -> s{_cluClusterSnapshotCopyStatus = a});

-- | The specific revision number of the database in the cluster.
cluClusterRevisionNumber :: Lens' Cluster (Maybe Text)
cluClusterRevisionNumber = lens _cluClusterRevisionNumber (\ s a -> s{_cluClusterRevisionNumber = a});

-- | The master user name for the cluster. This name is used to connect to
-- the database that is specified in __DBName__.
cluMasterUsername :: Lens' Cluster (Maybe Text)
cluMasterUsername = lens _cluMasterUsername (\ s a -> s{_cluMasterUsername = a});

-- | If @true@, the cluster can be accessed from a public network.
cluPubliclyAccessible :: Lens' Cluster (Maybe Bool)
cluPubliclyAccessible = lens _cluPubliclyAccessible (\ s a -> s{_cluPubliclyAccessible = a});

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
cluVPCId :: Lens' Cluster (Maybe Text)
cluVPCId = lens _cluVPCId (\ s a -> s{_cluVPCId = a});

-- | A list of cluster security group that are associated with the cluster.
-- Each security group is represented by an element that contains
-- @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@
-- subelements.
--
-- Cluster security groups are used when the cluster is not created in a
-- VPC. Clusters that are created in a VPC use VPC security groups, which
-- are listed by the __VpcSecurityGroups__ parameter.
cluClusterSecurityGroups :: Lens' Cluster [ClusterSecurityGroupMembership]
cluClusterSecurityGroups = lens _cluClusterSecurityGroups (\ s a -> s{_cluClusterSecurityGroups = a}) . _Default;

-- | The number of days that automatic cluster snapshots are retained.
cluAutomatedSnapshotRetentionPeriod :: Lens' Cluster (Maybe Int)
cluAutomatedSnapshotRetentionPeriod = lens _cluAutomatedSnapshotRetentionPeriod (\ s a -> s{_cluAutomatedSnapshotRetentionPeriod = a});

-- | If @true@, data in the cluster is encrypted at rest.
cluEncrypted :: Lens' Cluster (Maybe Bool)
cluEncrypted = lens _cluEncrypted (\ s a -> s{_cluEncrypted = a});

-- | The unique identifier of the cluster.
cluClusterIdentifier :: Lens' Cluster (Maybe Text)
cluClusterIdentifier = lens _cluClusterIdentifier (\ s a -> s{_cluClusterIdentifier = a});

-- | The number of compute nodes in the cluster.
cluNumberOfNodes :: Lens' Cluster (Maybe Int)
cluNumberOfNodes = lens _cluNumberOfNodes (\ s a -> s{_cluNumberOfNodes = a});

-- | The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
cluClusterSubnetGroupName :: Lens' Cluster (Maybe Text)
cluClusterSubnetGroupName = lens _cluClusterSubnetGroupName (\ s a -> s{_cluClusterSubnetGroupName = a});

-- | The weekly time range (in UTC) during which system maintenance can
-- occur.
cluPreferredMaintenanceWindow :: Lens' Cluster (Maybe Text)
cluPreferredMaintenanceWindow = lens _cluPreferredMaintenanceWindow (\ s a -> s{_cluPreferredMaintenanceWindow = a});

-- | The status of a modify operation, if any, initiated for the cluster.
cluModifyStatus :: Lens' Cluster (Maybe Text)
cluModifyStatus = lens _cluModifyStatus (\ s a -> s{_cluModifyStatus = a});

-- | The public key for the cluster.
cluClusterPublicKey :: Lens' Cluster (Maybe Text)
cluClusterPublicKey = lens _cluClusterPublicKey (\ s a -> s{_cluClusterPublicKey = a});

-- | The list of cluster parameter groups that are associated with this
-- cluster. Each parameter group in the list is returned with its status.
cluClusterParameterGroups :: Lens' Cluster [ClusterParameterGroupStatus]
cluClusterParameterGroups = lens _cluClusterParameterGroups (\ s a -> s{_cluClusterParameterGroups = a}) . _Default;

-- | The name of the Availability Zone in which the cluster is located.
cluAvailabilityZone :: Lens' Cluster (Maybe Text)
cluAvailabilityZone = lens _cluAvailabilityZone (\ s a -> s{_cluAvailabilityZone = a});

-- | A list of Virtual Private Cloud (VPC) security groups that are
-- associated with the cluster. This parameter is returned only if the
-- cluster is in a VPC.
cluVPCSecurityGroups :: Lens' Cluster [VPCSecurityGroupMembership]
cluVPCSecurityGroups = lens _cluVPCSecurityGroups (\ s a -> s{_cluVPCSecurityGroups = a}) . _Default;

-- | The AWS Key Management Service (KMS) key ID of the encryption key used
-- to encrypt data in the cluster.
cluKMSKeyId :: Lens' Cluster (Maybe Text)
cluKMSKeyId = lens _cluKMSKeyId (\ s a -> s{_cluKMSKeyId = a});

-- | Reports whether the Amazon Redshift cluster has finished applying any
-- HSM settings changes specified in a modify cluster command.
--
-- Values: active, applying
cluHSMStatus :: Lens' Cluster (Maybe HSMStatus)
cluHSMStatus = lens _cluHSMStatus (\ s a -> s{_cluHSMStatus = a});

-- | Describes the status of the elastic IP (EIP) address.
cluElasticIPStatus :: Lens' Cluster (Maybe ElasticIPStatus)
cluElasticIPStatus = lens _cluElasticIPStatus (\ s a -> s{_cluElasticIPStatus = a});

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
cluClusterVersion :: Lens' Cluster (Maybe Text)
cluClusterVersion = lens _cluClusterVersion (\ s a -> s{_cluClusterVersion = a});

-- | The node type for the nodes in the cluster.
cluNodeType :: Lens' Cluster (Maybe Text)
cluNodeType = lens _cluNodeType (\ s a -> s{_cluNodeType = a});

-- | The connection endpoint.
cluEndpoint :: Lens' Cluster (Maybe Endpoint)
cluEndpoint = lens _cluEndpoint (\ s a -> s{_cluEndpoint = a});

-- | The date and time that the cluster was created.
cluClusterCreateTime :: Lens' Cluster (Maybe UTCTime)
cluClusterCreateTime = lens _cluClusterCreateTime (\ s a -> s{_cluClusterCreateTime = a}) . mapping _Time;

-- | If @true@, major version upgrades will be applied automatically to the
-- cluster during the maintenance window.
cluAllowVersionUpgrade :: Lens' Cluster (Maybe Bool)
cluAllowVersionUpgrade = lens _cluAllowVersionUpgrade (\ s a -> s{_cluAllowVersionUpgrade = a});

-- | If present, changes to the cluster are pending. Specific pending changes
-- are identified by subelements.
cluPendingModifiedValues :: Lens' Cluster (Maybe PendingModifiedValues)
cluPendingModifiedValues = lens _cluPendingModifiedValues (\ s a -> s{_cluPendingModifiedValues = a});

-- | The current state of this cluster. Possible values include @available@,
-- @creating@, @deleting@, @rebooting@, @renaming@, and @resizing@.
cluClusterStatus :: Lens' Cluster (Maybe Text)
cluClusterStatus = lens _cluClusterStatus (\ s a -> s{_cluClusterStatus = a});

-- | The name of the initial database that was created when the cluster was
-- created. This same name is returned for the life of the cluster. If an
-- initial database was not specified, a database named \"dev\" was created
-- by default.
cluDBName :: Lens' Cluster (Maybe Text)
cluDBName = lens _cluDBName (\ s a -> s{_cluDBName = a});

-- | The list of tags for the cluster.
cluTags :: Lens' Cluster [Tag]
cluTags = lens _cluTags (\ s a -> s{_cluTags = a}) . _Default;

-- | The nodes in a cluster.
cluClusterNodes :: Lens' Cluster [ClusterNode]
cluClusterNodes = lens _cluClusterNodes (\ s a -> s{_cluClusterNodes = a}) . _Default;

instance FromXML Cluster where
        parseXML x
          = Cluster' <$>
              (x .@? "RestoreStatus") <*>
                (x .@? "ClusterSnapshotCopyStatus")
                <*> (x .@? "ClusterRevisionNumber")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "PubliclyAccessible")
                <*> (x .@? "VpcId")
                <*>
                (x .@? "ClusterSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "ClusterSecurityGroup"))
                <*> (x .@? "AutomatedSnapshotRetentionPeriod")
                <*> (x .@? "Encrypted")
                <*> (x .@? "ClusterIdentifier")
                <*> (x .@? "NumberOfNodes")
                <*> (x .@? "ClusterSubnetGroupName")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*> (x .@? "ModifyStatus")
                <*> (x .@? "ClusterPublicKey")
                <*>
                (x .@? "ClusterParameterGroups" .!@ mempty >>=
                   may (parseXMLList "ClusterParameterGroup"))
                <*> (x .@? "AvailabilityZone")
                <*>
                (x .@? "VpcSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "VpcSecurityGroup"))
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "HsmStatus")
                <*> (x .@? "ElasticIpStatus")
                <*> (x .@? "ClusterVersion")
                <*> (x .@? "NodeType")
                <*> (x .@? "Endpoint")
                <*> (x .@? "ClusterCreateTime")
                <*> (x .@? "AllowVersionUpgrade")
                <*> (x .@? "PendingModifiedValues")
                <*> (x .@? "ClusterStatus")
                <*> (x .@? "DBName")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))
                <*>
                (x .@? "ClusterNodes" .!@ mempty >>=
                   may (parseXMLList "member"))

-- | The identifier of a node in a cluster.
--
-- /See:/ 'clusterNode' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnNodeRole'
--
-- * 'cnPrivateIPAddress'
--
-- * 'cnPublicIPAddress'
data ClusterNode = ClusterNode'
    { _cnNodeRole         :: !(Maybe Text)
    , _cnPrivateIPAddress :: !(Maybe Text)
    , _cnPublicIPAddress  :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ClusterNode' smart constructor.
clusterNode :: ClusterNode
clusterNode =
    ClusterNode'
    { _cnNodeRole = Nothing
    , _cnPrivateIPAddress = Nothing
    , _cnPublicIPAddress = Nothing
    }

-- | Whether the node is a leader node or a compute node.
cnNodeRole :: Lens' ClusterNode (Maybe Text)
cnNodeRole = lens _cnNodeRole (\ s a -> s{_cnNodeRole = a});

-- | The private IP address of a node within a cluster.
cnPrivateIPAddress :: Lens' ClusterNode (Maybe Text)
cnPrivateIPAddress = lens _cnPrivateIPAddress (\ s a -> s{_cnPrivateIPAddress = a});

-- | The public IP address of a node within a cluster.
cnPublicIPAddress :: Lens' ClusterNode (Maybe Text)
cnPublicIPAddress = lens _cnPublicIPAddress (\ s a -> s{_cnPublicIPAddress = a});

instance FromXML ClusterNode where
        parseXML x
          = ClusterNode' <$>
              (x .@? "NodeRole") <*> (x .@? "PrivateIPAddress") <*>
                (x .@? "PublicIPAddress")

-- | Describes a parameter group.
--
-- /See:/ 'clusterParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgParameterGroupFamily'
--
-- * 'cpgDescription'
--
-- * 'cpgParameterGroupName'
--
-- * 'cpgTags'
data ClusterParameterGroup = ClusterParameterGroup'
    { _cpgParameterGroupFamily :: !(Maybe Text)
    , _cpgDescription          :: !(Maybe Text)
    , _cpgParameterGroupName   :: !(Maybe Text)
    , _cpgTags                 :: !(Maybe [Tag])
    } deriving (Eq,Read,Show)

-- | 'ClusterParameterGroup' smart constructor.
clusterParameterGroup :: ClusterParameterGroup
clusterParameterGroup =
    ClusterParameterGroup'
    { _cpgParameterGroupFamily = Nothing
    , _cpgDescription = Nothing
    , _cpgParameterGroupName = Nothing
    , _cpgTags = Nothing
    }

-- | The name of the cluster parameter group family that this cluster
-- parameter group is compatible with.
cpgParameterGroupFamily :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupFamily = lens _cpgParameterGroupFamily (\ s a -> s{_cpgParameterGroupFamily = a});

-- | The description of the parameter group.
cpgDescription :: Lens' ClusterParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\ s a -> s{_cpgDescription = a});

-- | The name of the cluster parameter group.
cpgParameterGroupName :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupName = lens _cpgParameterGroupName (\ s a -> s{_cpgParameterGroupName = a});

-- | The list of tags for the cluster parameter group.
cpgTags :: Lens' ClusterParameterGroup [Tag]
cpgTags = lens _cpgTags (\ s a -> s{_cpgTags = a}) . _Default;

instance FromXML ClusterParameterGroup where
        parseXML x
          = ClusterParameterGroup' <$>
              (x .@? "ParameterGroupFamily") <*>
                (x .@? "Description")
                <*> (x .@? "ParameterGroupName")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

-- | Contains the output from the ModifyClusterParameterGroup and
-- ResetClusterParameterGroup actions and indicate the parameter group
-- involved and the status of the operation on the parameter group.
--
-- /See:/ 'clusterParameterGroupNameMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgnmParameterGroupStatus'
--
-- * 'cpgnmParameterGroupName'
data ClusterParameterGroupNameMessage = ClusterParameterGroupNameMessage'
    { _cpgnmParameterGroupStatus :: !(Maybe Text)
    , _cpgnmParameterGroupName   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ClusterParameterGroupNameMessage' smart constructor.
clusterParameterGroupNameMessage :: ClusterParameterGroupNameMessage
clusterParameterGroupNameMessage =
    ClusterParameterGroupNameMessage'
    { _cpgnmParameterGroupStatus = Nothing
    , _cpgnmParameterGroupName = Nothing
    }

-- | The status of the parameter group. For example, if you made a change to
-- a parameter group name-value pair, then the change could be pending a
-- reboot of an associated cluster.
cpgnmParameterGroupStatus :: Lens' ClusterParameterGroupNameMessage (Maybe Text)
cpgnmParameterGroupStatus = lens _cpgnmParameterGroupStatus (\ s a -> s{_cpgnmParameterGroupStatus = a});

-- | The name of the cluster parameter group.
cpgnmParameterGroupName :: Lens' ClusterParameterGroupNameMessage (Maybe Text)
cpgnmParameterGroupName = lens _cpgnmParameterGroupName (\ s a -> s{_cpgnmParameterGroupName = a});

instance FromXML ClusterParameterGroupNameMessage
         where
        parseXML x
          = ClusterParameterGroupNameMessage' <$>
              (x .@? "ParameterGroupStatus") <*>
                (x .@? "ParameterGroupName")

-- | Describes the status of a parameter group.
--
-- /See:/ 'clusterParameterGroupStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgsClusterParameterStatusList'
--
-- * 'cpgsParameterApplyStatus'
--
-- * 'cpgsParameterGroupName'
data ClusterParameterGroupStatus = ClusterParameterGroupStatus'
    { _cpgsClusterParameterStatusList :: !(Maybe [ClusterParameterStatus])
    , _cpgsParameterApplyStatus       :: !(Maybe Text)
    , _cpgsParameterGroupName         :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ClusterParameterGroupStatus' smart constructor.
clusterParameterGroupStatus :: ClusterParameterGroupStatus
clusterParameterGroupStatus =
    ClusterParameterGroupStatus'
    { _cpgsClusterParameterStatusList = Nothing
    , _cpgsParameterApplyStatus = Nothing
    , _cpgsParameterGroupName = Nothing
    }

-- | The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
cpgsClusterParameterStatusList :: Lens' ClusterParameterGroupStatus [ClusterParameterStatus]
cpgsClusterParameterStatusList = lens _cpgsClusterParameterStatusList (\ s a -> s{_cpgsClusterParameterStatusList = a}) . _Default;

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus = lens _cpgsParameterApplyStatus (\ s a -> s{_cpgsParameterApplyStatus = a});

-- | The name of the cluster parameter group.
cpgsParameterGroupName :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterGroupName = lens _cpgsParameterGroupName (\ s a -> s{_cpgsParameterGroupName = a});

instance FromXML ClusterParameterGroupStatus where
        parseXML x
          = ClusterParameterGroupStatus' <$>
              (x .@? "ClusterParameterStatusList" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "ParameterApplyStatus")
                <*> (x .@? "ParameterGroupName")

-- | Describes the status of a parameter group.
--
-- /See:/ 'clusterParameterStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpsParameterApplyErrorDescription'
--
-- * 'cpsParameterName'
--
-- * 'cpsParameterApplyStatus'
data ClusterParameterStatus = ClusterParameterStatus'
    { _cpsParameterApplyErrorDescription :: !(Maybe Text)
    , _cpsParameterName                  :: !(Maybe Text)
    , _cpsParameterApplyStatus           :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ClusterParameterStatus' smart constructor.
clusterParameterStatus :: ClusterParameterStatus
clusterParameterStatus =
    ClusterParameterStatus'
    { _cpsParameterApplyErrorDescription = Nothing
    , _cpsParameterName = Nothing
    , _cpsParameterApplyStatus = Nothing
    }

-- | The error that prevented the parameter from being applied to the
-- database.
cpsParameterApplyErrorDescription :: Lens' ClusterParameterStatus (Maybe Text)
cpsParameterApplyErrorDescription = lens _cpsParameterApplyErrorDescription (\ s a -> s{_cpsParameterApplyErrorDescription = a});

-- | The name of the parameter.
cpsParameterName :: Lens' ClusterParameterStatus (Maybe Text)
cpsParameterName = lens _cpsParameterName (\ s a -> s{_cpsParameterName = a});

-- | The status of the parameter that indicates whether the parameter is in
-- sync with the database, waiting for a cluster reboot, or encountered an
-- error when being applied.
--
-- The following are possible statuses and descriptions.
--
-- -   @in-sync@: The parameter value is in sync with the database.
-- -   @pending-reboot@: The parameter value will be applied after the
--     cluster reboots.
-- -   @applying@: The parameter value is being applied to the database.
-- -   @invalid-parameter@: Cannot apply the parameter value because it has
--     an invalid value or syntax.
-- -   @apply-deferred@: The parameter contains static property changes.
--     The changes are deferred until the cluster reboots.
-- -   @apply-error@: Cannot connect to the cluster. The parameter change
--     will be applied after the cluster reboots.
-- -   @unknown-error@: Cannot apply the parameter change right now. The
--     change will be applied after the cluster reboots.
cpsParameterApplyStatus :: Lens' ClusterParameterStatus (Maybe Text)
cpsParameterApplyStatus = lens _cpsParameterApplyStatus (\ s a -> s{_cpsParameterApplyStatus = a});

instance FromXML ClusterParameterStatus where
        parseXML x
          = ClusterParameterStatus' <$>
              (x .@? "ParameterApplyErrorDescription") <*>
                (x .@? "ParameterName")
                <*> (x .@? "ParameterApplyStatus")

-- | Describes a security group.
--
-- /See:/ 'clusterSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cClusterSecurityGroupName'
--
-- * 'cIPRanges'
--
-- * 'cEC2SecurityGroups'
--
-- * 'cDescription'
--
-- * 'cTags'
data ClusterSecurityGroup = ClusterSecurityGroup'
    { _cClusterSecurityGroupName :: !(Maybe Text)
    , _cIPRanges                 :: !(Maybe [IPRange])
    , _cEC2SecurityGroups        :: !(Maybe [EC2SecurityGroup])
    , _cDescription              :: !(Maybe Text)
    , _cTags                     :: !(Maybe [Tag])
    } deriving (Eq,Read,Show)

-- | 'ClusterSecurityGroup' smart constructor.
clusterSecurityGroup :: ClusterSecurityGroup
clusterSecurityGroup =
    ClusterSecurityGroup'
    { _cClusterSecurityGroupName = Nothing
    , _cIPRanges = Nothing
    , _cEC2SecurityGroups = Nothing
    , _cDescription = Nothing
    , _cTags = Nothing
    }

-- | The name of the cluster security group to which the operation was
-- applied.
cClusterSecurityGroupName :: Lens' ClusterSecurityGroup (Maybe Text)
cClusterSecurityGroupName = lens _cClusterSecurityGroupName (\ s a -> s{_cClusterSecurityGroupName = a});

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters
-- associated with this cluster security group.
cIPRanges :: Lens' ClusterSecurityGroup [IPRange]
cIPRanges = lens _cIPRanges (\ s a -> s{_cIPRanges = a}) . _Default;

-- | A list of EC2 security groups that are permitted to access clusters
-- associated with this cluster security group.
cEC2SecurityGroups :: Lens' ClusterSecurityGroup [EC2SecurityGroup]
cEC2SecurityGroups = lens _cEC2SecurityGroups (\ s a -> s{_cEC2SecurityGroups = a}) . _Default;

-- | A description of the security group.
cDescription :: Lens' ClusterSecurityGroup (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a});

-- | The list of tags for the cluster security group.
cTags :: Lens' ClusterSecurityGroup [Tag]
cTags = lens _cTags (\ s a -> s{_cTags = a}) . _Default;

instance FromXML ClusterSecurityGroup where
        parseXML x
          = ClusterSecurityGroup' <$>
              (x .@? "ClusterSecurityGroupName") <*>
                (x .@? "IPRanges" .!@ mempty >>=
                   may (parseXMLList "IPRange"))
                <*>
                (x .@? "EC2SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "EC2SecurityGroup"))
                <*> (x .@? "Description")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

-- | Describes a security group.
--
-- /See:/ 'clusterSecurityGroupMembership' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgmStatus'
--
-- * 'csgmClusterSecurityGroupName'
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership'
    { _csgmStatus                   :: !(Maybe Text)
    , _csgmClusterSecurityGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ClusterSecurityGroupMembership' smart constructor.
clusterSecurityGroupMembership :: ClusterSecurityGroupMembership
clusterSecurityGroupMembership =
    ClusterSecurityGroupMembership'
    { _csgmStatus = Nothing
    , _csgmClusterSecurityGroupName = Nothing
    }

-- | The status of the cluster security group.
csgmStatus :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\ s a -> s{_csgmStatus = a});

-- | The name of the cluster security group.
csgmClusterSecurityGroupName :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmClusterSecurityGroupName = lens _csgmClusterSecurityGroupName (\ s a -> s{_csgmClusterSecurityGroupName = a});

instance FromXML ClusterSecurityGroupMembership where
        parseXML x
          = ClusterSecurityGroupMembership' <$>
              (x .@? "Status") <*>
                (x .@? "ClusterSecurityGroupName")

-- | Returns the destination region and retention period that are configured
-- for cross-region snapshot copy.
--
-- /See:/ 'clusterSnapshotCopyStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cscsRetentionPeriod'
--
-- * 'cscsDestinationRegion'
--
-- * 'cscsSnapshotCopyGrantName'
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus'
    { _cscsRetentionPeriod       :: !(Maybe Integer)
    , _cscsDestinationRegion     :: !(Maybe Text)
    , _cscsSnapshotCopyGrantName :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ClusterSnapshotCopyStatus' smart constructor.
clusterSnapshotCopyStatus :: ClusterSnapshotCopyStatus
clusterSnapshotCopyStatus =
    ClusterSnapshotCopyStatus'
    { _cscsRetentionPeriod = Nothing
    , _cscsDestinationRegion = Nothing
    , _cscsSnapshotCopyGrantName = Nothing
    }

-- | The number of days that automated snapshots are retained in the
-- destination region after they are copied from a source region.
cscsRetentionPeriod :: Lens' ClusterSnapshotCopyStatus (Maybe Integer)
cscsRetentionPeriod = lens _cscsRetentionPeriod (\ s a -> s{_cscsRetentionPeriod = a});

-- | The destination region that snapshots are automatically copied to when
-- cross-region snapshot copy is enabled.
cscsDestinationRegion :: Lens' ClusterSnapshotCopyStatus (Maybe Text)
cscsDestinationRegion = lens _cscsDestinationRegion (\ s a -> s{_cscsDestinationRegion = a});

-- | The name of the snapshot copy grant.
cscsSnapshotCopyGrantName :: Lens' ClusterSnapshotCopyStatus (Maybe Text)
cscsSnapshotCopyGrantName = lens _cscsSnapshotCopyGrantName (\ s a -> s{_cscsSnapshotCopyGrantName = a});

instance FromXML ClusterSnapshotCopyStatus where
        parseXML x
          = ClusterSnapshotCopyStatus' <$>
              (x .@? "RetentionPeriod") <*>
                (x .@? "DestinationRegion")
                <*> (x .@? "SnapshotCopyGrantName")

-- | Describes a subnet group.
--
-- /See:/ 'clusterSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgVPCId'
--
-- * 'csgSubnets'
--
-- * 'csgClusterSubnetGroupName'
--
-- * 'csgSubnetGroupStatus'
--
-- * 'csgDescription'
--
-- * 'csgTags'
data ClusterSubnetGroup = ClusterSubnetGroup'
    { _csgVPCId                  :: !(Maybe Text)
    , _csgSubnets                :: !(Maybe [Subnet])
    , _csgClusterSubnetGroupName :: !(Maybe Text)
    , _csgSubnetGroupStatus      :: !(Maybe Text)
    , _csgDescription            :: !(Maybe Text)
    , _csgTags                   :: !(Maybe [Tag])
    } deriving (Eq,Read,Show)

-- | 'ClusterSubnetGroup' smart constructor.
clusterSubnetGroup :: ClusterSubnetGroup
clusterSubnetGroup =
    ClusterSubnetGroup'
    { _csgVPCId = Nothing
    , _csgSubnets = Nothing
    , _csgClusterSubnetGroupName = Nothing
    , _csgSubnetGroupStatus = Nothing
    , _csgDescription = Nothing
    , _csgTags = Nothing
    }

-- | The VPC ID of the cluster subnet group.
csgVPCId :: Lens' ClusterSubnetGroup (Maybe Text)
csgVPCId = lens _csgVPCId (\ s a -> s{_csgVPCId = a});

-- | A list of the VPC Subnet elements.
csgSubnets :: Lens' ClusterSubnetGroup [Subnet]
csgSubnets = lens _csgSubnets (\ s a -> s{_csgSubnets = a}) . _Default;

-- | The name of the cluster subnet group.
csgClusterSubnetGroupName :: Lens' ClusterSubnetGroup (Maybe Text)
csgClusterSubnetGroupName = lens _csgClusterSubnetGroupName (\ s a -> s{_csgClusterSubnetGroupName = a});

-- | The status of the cluster subnet group. Possible values are @Complete@,
-- @Incomplete@ and @Invalid@.
csgSubnetGroupStatus :: Lens' ClusterSubnetGroup (Maybe Text)
csgSubnetGroupStatus = lens _csgSubnetGroupStatus (\ s a -> s{_csgSubnetGroupStatus = a});

-- | The description of the cluster subnet group.
csgDescription :: Lens' ClusterSubnetGroup (Maybe Text)
csgDescription = lens _csgDescription (\ s a -> s{_csgDescription = a});

-- | The list of tags for the cluster subnet group.
csgTags :: Lens' ClusterSubnetGroup [Tag]
csgTags = lens _csgTags (\ s a -> s{_csgTags = a}) . _Default;

instance FromXML ClusterSubnetGroup where
        parseXML x
          = ClusterSubnetGroup' <$>
              (x .@? "VpcId") <*>
                (x .@? "Subnets" .!@ mempty >>=
                   may (parseXMLList "Subnet"))
                <*> (x .@? "ClusterSubnetGroupName")
                <*> (x .@? "SubnetGroupStatus")
                <*> (x .@? "Description")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

-- | Describes a cluster version, including the parameter group family and
-- description of the version.
--
-- /See:/ 'clusterVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvClusterParameterGroupFamily'
--
-- * 'cvClusterVersion'
--
-- * 'cvDescription'
data ClusterVersion = ClusterVersion'
    { _cvClusterParameterGroupFamily :: !(Maybe Text)
    , _cvClusterVersion              :: !(Maybe Text)
    , _cvDescription                 :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ClusterVersion' smart constructor.
clusterVersion :: ClusterVersion
clusterVersion =
    ClusterVersion'
    { _cvClusterParameterGroupFamily = Nothing
    , _cvClusterVersion = Nothing
    , _cvDescription = Nothing
    }

-- | The name of the cluster parameter group family for the cluster.
cvClusterParameterGroupFamily :: Lens' ClusterVersion (Maybe Text)
cvClusterParameterGroupFamily = lens _cvClusterParameterGroupFamily (\ s a -> s{_cvClusterParameterGroupFamily = a});

-- | The version number used by the cluster.
cvClusterVersion :: Lens' ClusterVersion (Maybe Text)
cvClusterVersion = lens _cvClusterVersion (\ s a -> s{_cvClusterVersion = a});

-- | The description of the cluster version.
cvDescription :: Lens' ClusterVersion (Maybe Text)
cvDescription = lens _cvDescription (\ s a -> s{_cvDescription = a});

instance FromXML ClusterVersion where
        parseXML x
          = ClusterVersion' <$>
              (x .@? "ClusterParameterGroupFamily") <*>
                (x .@? "ClusterVersion")
                <*> (x .@? "Description")

-- | Describes the default cluster parameters for a parameter group family.
--
-- /See:/ 'defaultClusterParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpParameters'
--
-- * 'dcpMarker'
--
-- * 'dcpParameterGroupFamily'
data DefaultClusterParameters = DefaultClusterParameters'
    { _dcpParameters           :: !(Maybe [Parameter])
    , _dcpMarker               :: !(Maybe Text)
    , _dcpParameterGroupFamily :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DefaultClusterParameters' smart constructor.
defaultClusterParameters :: DefaultClusterParameters
defaultClusterParameters =
    DefaultClusterParameters'
    { _dcpParameters = Nothing
    , _dcpMarker = Nothing
    , _dcpParameterGroupFamily = Nothing
    }

-- | The list of cluster default parameters.
dcpParameters :: Lens' DefaultClusterParameters [Parameter]
dcpParameters = lens _dcpParameters (\ s a -> s{_dcpParameters = a}) . _Default;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcpMarker :: Lens' DefaultClusterParameters (Maybe Text)
dcpMarker = lens _dcpMarker (\ s a -> s{_dcpMarker = a});

-- | The name of the cluster parameter group family to which the engine
-- default parameters apply.
dcpParameterGroupFamily :: Lens' DefaultClusterParameters (Maybe Text)
dcpParameterGroupFamily = lens _dcpParameterGroupFamily (\ s a -> s{_dcpParameterGroupFamily = a});

instance FromXML DefaultClusterParameters where
        parseXML x
          = DefaultClusterParameters' <$>
              (x .@? "Parameters" .!@ mempty >>=
                 may (parseXMLList "Parameter"))
                <*> (x .@? "Marker")
                <*> (x .@? "ParameterGroupFamily")

-- | Describes an Amazon EC2 security group.
--
-- /See:/ 'ec2SecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esgStatus'
--
-- * 'esgEC2SecurityGroupOwnerId'
--
-- * 'esgEC2SecurityGroupName'
--
-- * 'esgTags'
data EC2SecurityGroup = EC2SecurityGroup'
    { _esgStatus                  :: !(Maybe Text)
    , _esgEC2SecurityGroupOwnerId :: !(Maybe Text)
    , _esgEC2SecurityGroupName    :: !(Maybe Text)
    , _esgTags                    :: !(Maybe [Tag])
    } deriving (Eq,Read,Show)

-- | 'EC2SecurityGroup' smart constructor.
ec2SecurityGroup :: EC2SecurityGroup
ec2SecurityGroup =
    EC2SecurityGroup'
    { _esgStatus = Nothing
    , _esgEC2SecurityGroupOwnerId = Nothing
    , _esgEC2SecurityGroupName = Nothing
    , _esgTags = Nothing
    }

-- | The status of the EC2 security group.
esgStatus :: Lens' EC2SecurityGroup (Maybe Text)
esgStatus = lens _esgStatus (\ s a -> s{_esgStatus = a});

-- | The AWS ID of the owner of the EC2 security group specified in the
-- @EC2SecurityGroupName@ field.
esgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupOwnerId = lens _esgEC2SecurityGroupOwnerId (\ s a -> s{_esgEC2SecurityGroupOwnerId = a});

-- | The name of the EC2 Security Group.
esgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupName = lens _esgEC2SecurityGroupName (\ s a -> s{_esgEC2SecurityGroupName = a});

-- | The list of tags for the EC2 security group.
esgTags :: Lens' EC2SecurityGroup [Tag]
esgTags = lens _esgTags (\ s a -> s{_esgTags = a}) . _Default;

instance FromXML EC2SecurityGroup where
        parseXML x
          = EC2SecurityGroup' <$>
              (x .@? "Status") <*>
                (x .@? "EC2SecurityGroupOwnerId")
                <*> (x .@? "EC2SecurityGroupName")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

-- | Describes the status of the elastic IP (EIP) address.
--
-- /See:/ 'elasticIPStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eisStatus'
--
-- * 'eisElasticIP'
data ElasticIPStatus = ElasticIPStatus'
    { _eisStatus    :: !(Maybe Text)
    , _eisElasticIP :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ElasticIPStatus' smart constructor.
elasticIPStatus :: ElasticIPStatus
elasticIPStatus =
    ElasticIPStatus'
    { _eisStatus = Nothing
    , _eisElasticIP = Nothing
    }

-- | Describes the status of the elastic IP (EIP) address.
eisStatus :: Lens' ElasticIPStatus (Maybe Text)
eisStatus = lens _eisStatus (\ s a -> s{_eisStatus = a});

-- | The elastic IP (EIP) address for the cluster.
eisElasticIP :: Lens' ElasticIPStatus (Maybe Text)
eisElasticIP = lens _eisElasticIP (\ s a -> s{_eisElasticIP = a});

instance FromXML ElasticIPStatus where
        parseXML x
          = ElasticIPStatus' <$>
              (x .@? "Status") <*> (x .@? "ElasticIp")

-- | Describes a connection endpoint.
--
-- /See:/ 'endpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'endAddress'
--
-- * 'endPort'
data Endpoint = Endpoint'
    { _endAddress :: !(Maybe Text)
    , _endPort    :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'Endpoint' smart constructor.
endpoint :: Endpoint
endpoint =
    Endpoint'
    { _endAddress = Nothing
    , _endPort = Nothing
    }

-- | The DNS address of the Cluster.
endAddress :: Lens' Endpoint (Maybe Text)
endAddress = lens _endAddress (\ s a -> s{_endAddress = a});

-- | The port that the database engine is listening on.
endPort :: Lens' Endpoint (Maybe Int)
endPort = lens _endPort (\ s a -> s{_endPort = a});

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$> (x .@? "Address") <*> (x .@? "Port")

-- | Describes an event.
--
-- /See:/ 'event' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eveSourceType'
--
-- * 'eveSeverity'
--
-- * 'eveSourceIdentifier'
--
-- * 'eveDate'
--
-- * 'eveEventCategories'
--
-- * 'eveMessage'
--
-- * 'eveEventId'
data Event = Event'
    { _eveSourceType       :: !(Maybe SourceType)
    , _eveSeverity         :: !(Maybe Text)
    , _eveSourceIdentifier :: !(Maybe Text)
    , _eveDate             :: !(Maybe ISO8601)
    , _eveEventCategories  :: !(Maybe [Text])
    , _eveMessage          :: !(Maybe Text)
    , _eveEventId          :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Event' smart constructor.
event :: Event
event =
    Event'
    { _eveSourceType = Nothing
    , _eveSeverity = Nothing
    , _eveSourceIdentifier = Nothing
    , _eveDate = Nothing
    , _eveEventCategories = Nothing
    , _eveMessage = Nothing
    , _eveEventId = Nothing
    }

-- | The source type for this event.
eveSourceType :: Lens' Event (Maybe SourceType)
eveSourceType = lens _eveSourceType (\ s a -> s{_eveSourceType = a});

-- | The severity of the event.
--
-- Values: ERROR, INFO
eveSeverity :: Lens' Event (Maybe Text)
eveSeverity = lens _eveSeverity (\ s a -> s{_eveSeverity = a});

-- | The identifier for the source of the event.
eveSourceIdentifier :: Lens' Event (Maybe Text)
eveSourceIdentifier = lens _eveSourceIdentifier (\ s a -> s{_eveSourceIdentifier = a});

-- | The date and time of the event.
eveDate :: Lens' Event (Maybe UTCTime)
eveDate = lens _eveDate (\ s a -> s{_eveDate = a}) . mapping _Time;

-- | A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security
eveEventCategories :: Lens' Event [Text]
eveEventCategories = lens _eveEventCategories (\ s a -> s{_eveEventCategories = a}) . _Default;

-- | The text of this event.
eveMessage :: Lens' Event (Maybe Text)
eveMessage = lens _eveMessage (\ s a -> s{_eveMessage = a});

-- | The identifier of the event.
eveEventId :: Lens' Event (Maybe Text)
eveEventId = lens _eveEventId (\ s a -> s{_eveEventId = a});

instance FromXML Event where
        parseXML x
          = Event' <$>
              (x .@? "SourceType") <*> (x .@? "Severity") <*>
                (x .@? "SourceIdentifier")
                <*> (x .@? "Date")
                <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*> (x .@? "Message")
                <*> (x .@? "EventId")

-- | /See:/ 'eventCategoriesMap' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ecmSourceType'
--
-- * 'ecmEvents'
data EventCategoriesMap = EventCategoriesMap'
    { _ecmSourceType :: !(Maybe Text)
    , _ecmEvents     :: !(Maybe [EventInfoMap])
    } deriving (Eq,Read,Show)

-- | 'EventCategoriesMap' smart constructor.
eventCategoriesMap :: EventCategoriesMap
eventCategoriesMap =
    EventCategoriesMap'
    { _ecmSourceType = Nothing
    , _ecmEvents = Nothing
    }

-- | The Amazon Redshift source type, such as cluster or cluster-snapshot,
-- that the returned categories belong to.
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\ s a -> s{_ecmSourceType = a});

-- | The events in the event category.
ecmEvents :: Lens' EventCategoriesMap [EventInfoMap]
ecmEvents = lens _ecmEvents (\ s a -> s{_ecmEvents = a}) . _Default;

instance FromXML EventCategoriesMap where
        parseXML x
          = EventCategoriesMap' <$>
              (x .@? "SourceType") <*>
                (x .@? "Events" .!@ mempty >>=
                   may (parseXMLList "EventInfoMap"))

-- | /See:/ 'eventInfoMap' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eimEventDescription'
--
-- * 'eimSeverity'
--
-- * 'eimEventCategories'
--
-- * 'eimEventId'
data EventInfoMap = EventInfoMap'
    { _eimEventDescription :: !(Maybe Text)
    , _eimSeverity         :: !(Maybe Text)
    , _eimEventCategories  :: !(Maybe [Text])
    , _eimEventId          :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'EventInfoMap' smart constructor.
eventInfoMap :: EventInfoMap
eventInfoMap =
    EventInfoMap'
    { _eimEventDescription = Nothing
    , _eimSeverity = Nothing
    , _eimEventCategories = Nothing
    , _eimEventId = Nothing
    }

-- | The description of an Amazon Redshift event.
eimEventDescription :: Lens' EventInfoMap (Maybe Text)
eimEventDescription = lens _eimEventDescription (\ s a -> s{_eimEventDescription = a});

-- | The severity of the event.
--
-- Values: ERROR, INFO
eimSeverity :: Lens' EventInfoMap (Maybe Text)
eimSeverity = lens _eimSeverity (\ s a -> s{_eimSeverity = a});

-- | The category of an Amazon Redshift event.
eimEventCategories :: Lens' EventInfoMap [Text]
eimEventCategories = lens _eimEventCategories (\ s a -> s{_eimEventCategories = a}) . _Default;

-- | The identifier of an Amazon Redshift event.
eimEventId :: Lens' EventInfoMap (Maybe Text)
eimEventId = lens _eimEventId (\ s a -> s{_eimEventId = a});

instance FromXML EventInfoMap where
        parseXML x
          = EventInfoMap' <$>
              (x .@? "EventDescription") <*> (x .@? "Severity") <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*> (x .@? "EventId")

-- | /See:/ 'eventSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esCustomerAWSId'
--
-- * 'esStatus'
--
-- * 'esCustSubscriptionId'
--
-- * 'esSNSTopicARN'
--
-- * 'esEnabled'
--
-- * 'esSourceType'
--
-- * 'esSeverity'
--
-- * 'esSubscriptionCreationTime'
--
-- * 'esEventCategoriesList'
--
-- * 'esSourceIdsList'
--
-- * 'esTags'
data EventSubscription = EventSubscription'
    { _esCustomerAWSId            :: !(Maybe Text)
    , _esStatus                   :: !(Maybe Text)
    , _esCustSubscriptionId       :: !(Maybe Text)
    , _esSNSTopicARN              :: !(Maybe Text)
    , _esEnabled                  :: !(Maybe Bool)
    , _esSourceType               :: !(Maybe Text)
    , _esSeverity                 :: !(Maybe Text)
    , _esSubscriptionCreationTime :: !(Maybe ISO8601)
    , _esEventCategoriesList      :: !(Maybe [Text])
    , _esSourceIdsList            :: !(Maybe [Text])
    , _esTags                     :: !(Maybe [Tag])
    } deriving (Eq,Read,Show)

-- | 'EventSubscription' smart constructor.
eventSubscription :: EventSubscription
eventSubscription =
    EventSubscription'
    { _esCustomerAWSId = Nothing
    , _esStatus = Nothing
    , _esCustSubscriptionId = Nothing
    , _esSNSTopicARN = Nothing
    , _esEnabled = Nothing
    , _esSourceType = Nothing
    , _esSeverity = Nothing
    , _esSubscriptionCreationTime = Nothing
    , _esEventCategoriesList = Nothing
    , _esSourceIdsList = Nothing
    , _esTags = Nothing
    }

-- | The AWS customer account associated with the Amazon Redshift event
-- notification subscription.
esCustomerAWSId :: Lens' EventSubscription (Maybe Text)
esCustomerAWSId = lens _esCustomerAWSId (\ s a -> s{_esCustomerAWSId = a});

-- | The status of the Amazon Redshift event notification subscription.
--
-- Constraints:
--
-- -   Can be one of the following: active | no-permission |
--     topic-not-exist
-- -   The status \"no-permission\" indicates that Amazon Redshift no
--     longer has permission to post to the Amazon SNS topic. The status
--     \"topic-not-exist\" indicates that the topic was deleted after the
--     subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\ s a -> s{_esStatus = a});

-- | The name of the Amazon Redshift event notification subscription.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId = lens _esCustSubscriptionId (\ s a -> s{_esCustSubscriptionId = a});

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
esSNSTopicARN :: Lens' EventSubscription (Maybe Text)
esSNSTopicARN = lens _esSNSTopicARN (\ s a -> s{_esSNSTopicARN = a});

-- | A Boolean value indicating whether the subscription is enabled. @true@
-- indicates the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\ s a -> s{_esEnabled = a});

-- | The source type of the events returned the Amazon Redshift event
-- notification, such as cluster, or cluster-snapshot.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\ s a -> s{_esSourceType = a});

-- | The event severity specified in the Amazon Redshift event notification
-- subscription.
--
-- Values: ERROR, INFO
esSeverity :: Lens' EventSubscription (Maybe Text)
esSeverity = lens _esSeverity (\ s a -> s{_esSeverity = a});

-- | The date and time the Amazon Redshift event notification subscription
-- was created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe UTCTime)
esSubscriptionCreationTime = lens _esSubscriptionCreationTime (\ s a -> s{_esSubscriptionCreationTime = a}) . mapping _Time;

-- | The list of Amazon Redshift event categories specified in the event
-- notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security
esEventCategoriesList :: Lens' EventSubscription [Text]
esEventCategoriesList = lens _esEventCategoriesList (\ s a -> s{_esEventCategoriesList = a}) . _Default;

-- | A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
esSourceIdsList :: Lens' EventSubscription [Text]
esSourceIdsList = lens _esSourceIdsList (\ s a -> s{_esSourceIdsList = a}) . _Default;

-- | The list of tags for the event subscription.
esTags :: Lens' EventSubscription [Tag]
esTags = lens _esTags (\ s a -> s{_esTags = a}) . _Default;

instance FromXML EventSubscription where
        parseXML x
          = EventSubscription' <$>
              (x .@? "CustomerAwsId") <*> (x .@? "Status") <*>
                (x .@? "CustSubscriptionId")
                <*> (x .@? "SnsTopicArn")
                <*> (x .@? "Enabled")
                <*> (x .@? "SourceType")
                <*> (x .@? "Severity")
                <*> (x .@? "SubscriptionCreationTime")
                <*>
                (x .@? "EventCategoriesList" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*>
                (x .@? "SourceIdsList" .!@ mempty >>=
                   may (parseXMLList "SourceId"))
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

-- | Returns information about an HSM client certificate. The certificate is
-- stored in a secure Hardware Storage Module (HSM), and used by the Amazon
-- Redshift cluster to encrypt data files.
--
-- /See:/ 'hsmClientCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hccHSMClientCertificateIdentifier'
--
-- * 'hccHSMClientCertificatePublicKey'
--
-- * 'hccTags'
data HSMClientCertificate = HSMClientCertificate'
    { _hccHSMClientCertificateIdentifier :: !(Maybe Text)
    , _hccHSMClientCertificatePublicKey  :: !(Maybe Text)
    , _hccTags                           :: !(Maybe [Tag])
    } deriving (Eq,Read,Show)

-- | 'HSMClientCertificate' smart constructor.
hsmClientCertificate :: HSMClientCertificate
hsmClientCertificate =
    HSMClientCertificate'
    { _hccHSMClientCertificateIdentifier = Nothing
    , _hccHSMClientCertificatePublicKey = Nothing
    , _hccTags = Nothing
    }

-- | The identifier of the HSM client certificate.
hccHSMClientCertificateIdentifier :: Lens' HSMClientCertificate (Maybe Text)
hccHSMClientCertificateIdentifier = lens _hccHSMClientCertificateIdentifier (\ s a -> s{_hccHSMClientCertificateIdentifier = a});

-- | The public key that the Amazon Redshift cluster will use to connect to
-- the HSM. You must register the public key in the HSM.
hccHSMClientCertificatePublicKey :: Lens' HSMClientCertificate (Maybe Text)
hccHSMClientCertificatePublicKey = lens _hccHSMClientCertificatePublicKey (\ s a -> s{_hccHSMClientCertificatePublicKey = a});

-- | The list of tags for the HSM client certificate.
hccTags :: Lens' HSMClientCertificate [Tag]
hccTags = lens _hccTags (\ s a -> s{_hccTags = a}) . _Default;

instance FromXML HSMClientCertificate where
        parseXML x
          = HSMClientCertificate' <$>
              (x .@? "HsmClientCertificateIdentifier") <*>
                (x .@? "HsmClientCertificatePublicKey")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

-- | Returns information about an HSM configuration, which is an object that
-- describes to Amazon Redshift clusters the information they require to
-- connect to an HSM where they can store database encryption keys.
--
-- /See:/ 'hsmConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hcHSMConfigurationIdentifier'
--
-- * 'hcHSMPartitionName'
--
-- * 'hcDescription'
--
-- * 'hcHSMIPAddress'
--
-- * 'hcTags'
data HSMConfiguration = HSMConfiguration'
    { _hcHSMConfigurationIdentifier :: !(Maybe Text)
    , _hcHSMPartitionName           :: !(Maybe Text)
    , _hcDescription                :: !(Maybe Text)
    , _hcHSMIPAddress               :: !(Maybe Text)
    , _hcTags                       :: !(Maybe [Tag])
    } deriving (Eq,Read,Show)

-- | 'HSMConfiguration' smart constructor.
hsmConfiguration :: HSMConfiguration
hsmConfiguration =
    HSMConfiguration'
    { _hcHSMConfigurationIdentifier = Nothing
    , _hcHSMPartitionName = Nothing
    , _hcDescription = Nothing
    , _hcHSMIPAddress = Nothing
    , _hcTags = Nothing
    }

-- | The name of the Amazon Redshift HSM configuration.
hcHSMConfigurationIdentifier :: Lens' HSMConfiguration (Maybe Text)
hcHSMConfigurationIdentifier = lens _hcHSMConfigurationIdentifier (\ s a -> s{_hcHSMConfigurationIdentifier = a});

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
hcHSMPartitionName :: Lens' HSMConfiguration (Maybe Text)
hcHSMPartitionName = lens _hcHSMPartitionName (\ s a -> s{_hcHSMPartitionName = a});

-- | A text description of the HSM configuration.
hcDescription :: Lens' HSMConfiguration (Maybe Text)
hcDescription = lens _hcDescription (\ s a -> s{_hcDescription = a});

-- | The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
hcHSMIPAddress :: Lens' HSMConfiguration (Maybe Text)
hcHSMIPAddress = lens _hcHSMIPAddress (\ s a -> s{_hcHSMIPAddress = a});

-- | The list of tags for the HSM configuration.
hcTags :: Lens' HSMConfiguration [Tag]
hcTags = lens _hcTags (\ s a -> s{_hcTags = a}) . _Default;

instance FromXML HSMConfiguration where
        parseXML x
          = HSMConfiguration' <$>
              (x .@? "HsmConfigurationIdentifier") <*>
                (x .@? "HsmPartitionName")
                <*> (x .@? "Description")
                <*> (x .@? "HsmIpAddress")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

-- |
--
-- /See:/ 'hsmStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hsStatus'
--
-- * 'hsHSMConfigurationIdentifier'
--
-- * 'hsHSMClientCertificateIdentifier'
data HSMStatus = HSMStatus'
    { _hsStatus                         :: !(Maybe Text)
    , _hsHSMConfigurationIdentifier     :: !(Maybe Text)
    , _hsHSMClientCertificateIdentifier :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'HSMStatus' smart constructor.
hsmStatus :: HSMStatus
hsmStatus =
    HSMStatus'
    { _hsStatus = Nothing
    , _hsHSMConfigurationIdentifier = Nothing
    , _hsHSMClientCertificateIdentifier = Nothing
    }

-- | Reports whether the Amazon Redshift cluster has finished applying any
-- HSM settings changes specified in a modify cluster command.
--
-- Values: active, applying
hsStatus :: Lens' HSMStatus (Maybe Text)
hsStatus = lens _hsStatus (\ s a -> s{_hsStatus = a});

-- | Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
hsHSMConfigurationIdentifier :: Lens' HSMStatus (Maybe Text)
hsHSMConfigurationIdentifier = lens _hsHSMConfigurationIdentifier (\ s a -> s{_hsHSMConfigurationIdentifier = a});

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
hsHSMClientCertificateIdentifier :: Lens' HSMStatus (Maybe Text)
hsHSMClientCertificateIdentifier = lens _hsHSMClientCertificateIdentifier (\ s a -> s{_hsHSMClientCertificateIdentifier = a});

instance FromXML HSMStatus where
        parseXML x
          = HSMStatus' <$>
              (x .@? "Status") <*>
                (x .@? "HsmConfigurationIdentifier")
                <*> (x .@? "HsmClientCertificateIdentifier")

-- | Describes an IP range used in a security group.
--
-- /See:/ 'ipRange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irStatus'
--
-- * 'irCIDRIP'
--
-- * 'irTags'
data IPRange = IPRange'
    { _irStatus :: !(Maybe Text)
    , _irCIDRIP :: !(Maybe Text)
    , _irTags   :: !(Maybe [Tag])
    } deriving (Eq,Read,Show)

-- | 'IPRange' smart constructor.
ipRange :: IPRange
ipRange =
    IPRange'
    { _irStatus = Nothing
    , _irCIDRIP = Nothing
    , _irTags = Nothing
    }

-- | The status of the IP range, for example, \"authorized\".
irStatus :: Lens' IPRange (Maybe Text)
irStatus = lens _irStatus (\ s a -> s{_irStatus = a});

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
irCIDRIP :: Lens' IPRange (Maybe Text)
irCIDRIP = lens _irCIDRIP (\ s a -> s{_irCIDRIP = a});

-- | The list of tags for the IP range.
irTags :: Lens' IPRange [Tag]
irTags = lens _irTags (\ s a -> s{_irTags = a}) . _Default;

instance FromXML IPRange where
        parseXML x
          = IPRange' <$>
              (x .@? "Status") <*> (x .@? "CIDRIP") <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

-- | Describes the status of logging for a cluster.
--
-- /See:/ 'loggingStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsLastSuccessfulDeliveryTime'
--
-- * 'lsLastFailureTime'
--
-- * 'lsS3KeyPrefix'
--
-- * 'lsBucketName'
--
-- * 'lsLoggingEnabled'
--
-- * 'lsLastFailureMessage'
data LoggingStatus = LoggingStatus'
    { _lsLastSuccessfulDeliveryTime :: !(Maybe ISO8601)
    , _lsLastFailureTime            :: !(Maybe ISO8601)
    , _lsS3KeyPrefix                :: !(Maybe Text)
    , _lsBucketName                 :: !(Maybe Text)
    , _lsLoggingEnabled             :: !(Maybe Bool)
    , _lsLastFailureMessage         :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'LoggingStatus' smart constructor.
loggingStatus :: LoggingStatus
loggingStatus =
    LoggingStatus'
    { _lsLastSuccessfulDeliveryTime = Nothing
    , _lsLastFailureTime = Nothing
    , _lsS3KeyPrefix = Nothing
    , _lsBucketName = Nothing
    , _lsLoggingEnabled = Nothing
    , _lsLastFailureMessage = Nothing
    }

-- | The last time when logs were delivered.
lsLastSuccessfulDeliveryTime :: Lens' LoggingStatus (Maybe UTCTime)
lsLastSuccessfulDeliveryTime = lens _lsLastSuccessfulDeliveryTime (\ s a -> s{_lsLastSuccessfulDeliveryTime = a}) . mapping _Time;

-- | The last time when logs failed to be delivered.
lsLastFailureTime :: Lens' LoggingStatus (Maybe UTCTime)
lsLastFailureTime = lens _lsLastFailureTime (\ s a -> s{_lsLastFailureTime = a}) . mapping _Time;

-- | The prefix applied to the log file names.
lsS3KeyPrefix :: Lens' LoggingStatus (Maybe Text)
lsS3KeyPrefix = lens _lsS3KeyPrefix (\ s a -> s{_lsS3KeyPrefix = a});

-- | The name of the S3 bucket where the log files are stored.
lsBucketName :: Lens' LoggingStatus (Maybe Text)
lsBucketName = lens _lsBucketName (\ s a -> s{_lsBucketName = a});

-- | @true@ if logging is on, @false@ if logging is off.
lsLoggingEnabled :: Lens' LoggingStatus (Maybe Bool)
lsLoggingEnabled = lens _lsLoggingEnabled (\ s a -> s{_lsLoggingEnabled = a});

-- | The message indicating that logs failed to be delivered.
lsLastFailureMessage :: Lens' LoggingStatus (Maybe Text)
lsLastFailureMessage = lens _lsLastFailureMessage (\ s a -> s{_lsLastFailureMessage = a});

instance FromXML LoggingStatus where
        parseXML x
          = LoggingStatus' <$>
              (x .@? "LastSuccessfulDeliveryTime") <*>
                (x .@? "LastFailureTime")
                <*> (x .@? "S3KeyPrefix")
                <*> (x .@? "BucketName")
                <*> (x .@? "LoggingEnabled")
                <*> (x .@? "LastFailureMessage")

-- | Describes an orderable cluster option.
--
-- /See:/ 'orderableClusterOption' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ocoAvailabilityZones'
--
-- * 'ocoClusterType'
--
-- * 'ocoClusterVersion'
--
-- * 'ocoNodeType'
data OrderableClusterOption = OrderableClusterOption'
    { _ocoAvailabilityZones :: !(Maybe [AvailabilityZone])
    , _ocoClusterType       :: !(Maybe Text)
    , _ocoClusterVersion    :: !(Maybe Text)
    , _ocoNodeType          :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'OrderableClusterOption' smart constructor.
orderableClusterOption :: OrderableClusterOption
orderableClusterOption =
    OrderableClusterOption'
    { _ocoAvailabilityZones = Nothing
    , _ocoClusterType = Nothing
    , _ocoClusterVersion = Nothing
    , _ocoNodeType = Nothing
    }

-- | A list of availability zones for the orderable cluster.
ocoAvailabilityZones :: Lens' OrderableClusterOption [AvailabilityZone]
ocoAvailabilityZones = lens _ocoAvailabilityZones (\ s a -> s{_ocoAvailabilityZones = a}) . _Default;

-- | The cluster type, for example @multi-node@.
ocoClusterType :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterType = lens _ocoClusterType (\ s a -> s{_ocoClusterType = a});

-- | The version of the orderable cluster.
ocoClusterVersion :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterVersion = lens _ocoClusterVersion (\ s a -> s{_ocoClusterVersion = a});

-- | The node type for the orderable cluster.
ocoNodeType :: Lens' OrderableClusterOption (Maybe Text)
ocoNodeType = lens _ocoNodeType (\ s a -> s{_ocoNodeType = a});

instance FromXML OrderableClusterOption where
        parseXML x
          = OrderableClusterOption' <$>
              (x .@? "AvailabilityZones" .!@ mempty >>=
                 may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "ClusterType")
                <*> (x .@? "ClusterVersion")
                <*> (x .@? "NodeType")

-- | Describes a parameter in a cluster parameter group.
--
-- /See:/ 'parameter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'parApplyType'
--
-- * 'parParameterValue'
--
-- * 'parMinimumEngineVersion'
--
-- * 'parSource'
--
-- * 'parIsModifiable'
--
-- * 'parAllowedValues'
--
-- * 'parDataType'
--
-- * 'parParameterName'
--
-- * 'parDescription'
data Parameter = Parameter'
    { _parApplyType            :: !(Maybe ParameterApplyType)
    , _parParameterValue       :: !(Maybe Text)
    , _parMinimumEngineVersion :: !(Maybe Text)
    , _parSource               :: !(Maybe Text)
    , _parIsModifiable         :: !(Maybe Bool)
    , _parAllowedValues        :: !(Maybe Text)
    , _parDataType             :: !(Maybe Text)
    , _parParameterName        :: !(Maybe Text)
    , _parDescription          :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Parameter' smart constructor.
parameter :: Parameter
parameter =
    Parameter'
    { _parApplyType = Nothing
    , _parParameterValue = Nothing
    , _parMinimumEngineVersion = Nothing
    , _parSource = Nothing
    , _parIsModifiable = Nothing
    , _parAllowedValues = Nothing
    , _parDataType = Nothing
    , _parParameterName = Nothing
    , _parDescription = Nothing
    }

-- | Specifies how to apply the parameter. Supported value: @static@.
parApplyType :: Lens' Parameter (Maybe ParameterApplyType)
parApplyType = lens _parApplyType (\ s a -> s{_parApplyType = a});

-- | The value of the parameter.
parParameterValue :: Lens' Parameter (Maybe Text)
parParameterValue = lens _parParameterValue (\ s a -> s{_parParameterValue = a});

-- | The earliest engine version to which the parameter can apply.
parMinimumEngineVersion :: Lens' Parameter (Maybe Text)
parMinimumEngineVersion = lens _parMinimumEngineVersion (\ s a -> s{_parMinimumEngineVersion = a});

-- | The source of the parameter value, such as \"engine-default\" or
-- \"user\".
parSource :: Lens' Parameter (Maybe Text)
parSource = lens _parSource (\ s a -> s{_parSource = a});

-- | If @true@, the parameter can be modified. Some parameters have security
-- or operational implications that prevent them from being changed.
parIsModifiable :: Lens' Parameter (Maybe Bool)
parIsModifiable = lens _parIsModifiable (\ s a -> s{_parIsModifiable = a});

-- | The valid range of values for the parameter.
parAllowedValues :: Lens' Parameter (Maybe Text)
parAllowedValues = lens _parAllowedValues (\ s a -> s{_parAllowedValues = a});

-- | The data type of the parameter.
parDataType :: Lens' Parameter (Maybe Text)
parDataType = lens _parDataType (\ s a -> s{_parDataType = a});

-- | The name of the parameter.
parParameterName :: Lens' Parameter (Maybe Text)
parParameterName = lens _parParameterName (\ s a -> s{_parParameterName = a});

-- | A description of the parameter.
parDescription :: Lens' Parameter (Maybe Text)
parDescription = lens _parDescription (\ s a -> s{_parDescription = a});

instance FromXML Parameter where
        parseXML x
          = Parameter' <$>
              (x .@? "ApplyType") <*> (x .@? "ParameterValue") <*>
                (x .@? "MinimumEngineVersion")
                <*> (x .@? "Source")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "DataType")
                <*> (x .@? "ParameterName")
                <*> (x .@? "Description")

instance ToQuery Parameter where
        toQuery Parameter'{..}
          = mconcat
              ["ApplyType" =: _parApplyType,
               "ParameterValue" =: _parParameterValue,
               "MinimumEngineVersion" =: _parMinimumEngineVersion,
               "Source" =: _parSource,
               "IsModifiable" =: _parIsModifiable,
               "AllowedValues" =: _parAllowedValues,
               "DataType" =: _parDataType,
               "ParameterName" =: _parParameterName,
               "Description" =: _parDescription]

-- | Describes cluster attributes that are in a pending state. A change to
-- one or more the attributes was requested and is in progress or will be
-- applied.
--
-- /See:/ 'pendingModifiedValues' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmvMasterUserPassword'
--
-- * 'pmvAutomatedSnapshotRetentionPeriod'
--
-- * 'pmvClusterIdentifier'
--
-- * 'pmvNumberOfNodes'
--
-- * 'pmvClusterType'
--
-- * 'pmvClusterVersion'
--
-- * 'pmvNodeType'
data PendingModifiedValues = PendingModifiedValues'
    { _pmvMasterUserPassword               :: !(Maybe Text)
    , _pmvAutomatedSnapshotRetentionPeriod :: !(Maybe Int)
    , _pmvClusterIdentifier                :: !(Maybe Text)
    , _pmvNumberOfNodes                    :: !(Maybe Int)
    , _pmvClusterType                      :: !(Maybe Text)
    , _pmvClusterVersion                   :: !(Maybe Text)
    , _pmvNodeType                         :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'PendingModifiedValues' smart constructor.
pendingModifiedValues :: PendingModifiedValues
pendingModifiedValues =
    PendingModifiedValues'
    { _pmvMasterUserPassword = Nothing
    , _pmvAutomatedSnapshotRetentionPeriod = Nothing
    , _pmvClusterIdentifier = Nothing
    , _pmvNumberOfNodes = Nothing
    , _pmvClusterType = Nothing
    , _pmvClusterVersion = Nothing
    , _pmvNodeType = Nothing
    }

-- | The pending or in-progress change of the master user password for the
-- cluster.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword = lens _pmvMasterUserPassword (\ s a -> s{_pmvMasterUserPassword = a});

-- | The pending or in-progress change of the automated snapshot retention
-- period.
pmvAutomatedSnapshotRetentionPeriod :: Lens' PendingModifiedValues (Maybe Int)
pmvAutomatedSnapshotRetentionPeriod = lens _pmvAutomatedSnapshotRetentionPeriod (\ s a -> s{_pmvAutomatedSnapshotRetentionPeriod = a});

-- | The pending or in-progress change of the new identifier for the cluster.
pmvClusterIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterIdentifier = lens _pmvClusterIdentifier (\ s a -> s{_pmvClusterIdentifier = a});

-- | The pending or in-progress change of the number of nodes in the cluster.
pmvNumberOfNodes :: Lens' PendingModifiedValues (Maybe Int)
pmvNumberOfNodes = lens _pmvNumberOfNodes (\ s a -> s{_pmvNumberOfNodes = a});

-- | The pending or in-progress change of the cluster type.
pmvClusterType :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterType = lens _pmvClusterType (\ s a -> s{_pmvClusterType = a});

-- | The pending or in-progress change of the service version.
pmvClusterVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterVersion = lens _pmvClusterVersion (\ s a -> s{_pmvClusterVersion = a});

-- | The pending or in-progress change of the cluster\'s node type.
pmvNodeType :: Lens' PendingModifiedValues (Maybe Text)
pmvNodeType = lens _pmvNodeType (\ s a -> s{_pmvNodeType = a});

instance FromXML PendingModifiedValues where
        parseXML x
          = PendingModifiedValues' <$>
              (x .@? "MasterUserPassword") <*>
                (x .@? "AutomatedSnapshotRetentionPeriod")
                <*> (x .@? "ClusterIdentifier")
                <*> (x .@? "NumberOfNodes")
                <*> (x .@? "ClusterType")
                <*> (x .@? "ClusterVersion")
                <*> (x .@? "NodeType")

-- | Describes a recurring charge.
--
-- /See:/ 'recurringCharge' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcRecurringChargeFrequency'
--
-- * 'rcRecurringChargeAmount'
data RecurringCharge = RecurringCharge'
    { _rcRecurringChargeFrequency :: !(Maybe Text)
    , _rcRecurringChargeAmount    :: !(Maybe Double)
    } deriving (Eq,Read,Show)

-- | 'RecurringCharge' smart constructor.
recurringCharge :: RecurringCharge
recurringCharge =
    RecurringCharge'
    { _rcRecurringChargeFrequency = Nothing
    , _rcRecurringChargeAmount = Nothing
    }

-- | The frequency at which the recurring charge amount is applied.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\ s a -> s{_rcRecurringChargeFrequency = a});

-- | The amount charged per the period of time specified by the recurring
-- charge frequency.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\ s a -> s{_rcRecurringChargeAmount = a});

instance FromXML RecurringCharge where
        parseXML x
          = RecurringCharge' <$>
              (x .@? "RecurringChargeFrequency") <*>
                (x .@? "RecurringChargeAmount")

-- | Describes a reserved node. You can call the
-- DescribeReservedNodeOfferings API to obtain the available reserved node
-- offerings.
--
-- /See:/ 'reservedNode' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnState'
--
-- * 'rnCurrencyCode'
--
-- * 'rnStartTime'
--
-- * 'rnNodeCount'
--
-- * 'rnReservedNodeOfferingId'
--
-- * 'rnReservedNodeId'
--
-- * 'rnOfferingType'
--
-- * 'rnUsagePrice'
--
-- * 'rnNodeType'
--
-- * 'rnRecurringCharges'
--
-- * 'rnFixedPrice'
--
-- * 'rnDuration'
data ReservedNode = ReservedNode'
    { _rnState                  :: !(Maybe Text)
    , _rnCurrencyCode           :: !(Maybe Text)
    , _rnStartTime              :: !(Maybe ISO8601)
    , _rnNodeCount              :: !(Maybe Int)
    , _rnReservedNodeOfferingId :: !(Maybe Text)
    , _rnReservedNodeId         :: !(Maybe Text)
    , _rnOfferingType           :: !(Maybe Text)
    , _rnUsagePrice             :: !(Maybe Double)
    , _rnNodeType               :: !(Maybe Text)
    , _rnRecurringCharges       :: !(Maybe [RecurringCharge])
    , _rnFixedPrice             :: !(Maybe Double)
    , _rnDuration               :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'ReservedNode' smart constructor.
reservedNode :: ReservedNode
reservedNode =
    ReservedNode'
    { _rnState = Nothing
    , _rnCurrencyCode = Nothing
    , _rnStartTime = Nothing
    , _rnNodeCount = Nothing
    , _rnReservedNodeOfferingId = Nothing
    , _rnReservedNodeId = Nothing
    , _rnOfferingType = Nothing
    , _rnUsagePrice = Nothing
    , _rnNodeType = Nothing
    , _rnRecurringCharges = Nothing
    , _rnFixedPrice = Nothing
    , _rnDuration = Nothing
    }

-- | The state of the reserved compute node.
--
-- Possible Values:
--
-- -   pending-payment-This reserved node has recently been purchased, and
--     the sale has been approved, but payment has not yet been confirmed.
-- -   active-This reserved node is owned by the caller and is available
--     for use.
-- -   payment-failed-Payment failed for the purchase attempt.
rnState :: Lens' ReservedNode (Maybe Text)
rnState = lens _rnState (\ s a -> s{_rnState = a});

-- | The currency code for the reserved cluster.
rnCurrencyCode :: Lens' ReservedNode (Maybe Text)
rnCurrencyCode = lens _rnCurrencyCode (\ s a -> s{_rnCurrencyCode = a});

-- | The time the reservation started. You purchase a reserved node offering
-- for a duration. This is the start time of that duration.
rnStartTime :: Lens' ReservedNode (Maybe UTCTime)
rnStartTime = lens _rnStartTime (\ s a -> s{_rnStartTime = a}) . mapping _Time;

-- | The number of reserved compute nodes.
rnNodeCount :: Lens' ReservedNode (Maybe Int)
rnNodeCount = lens _rnNodeCount (\ s a -> s{_rnNodeCount = a});

-- | The identifier for the reserved node offering.
rnReservedNodeOfferingId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeOfferingId = lens _rnReservedNodeOfferingId (\ s a -> s{_rnReservedNodeOfferingId = a});

-- | The unique identifier for the reservation.
rnReservedNodeId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeId = lens _rnReservedNodeId (\ s a -> s{_rnReservedNodeId = a});

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
rnOfferingType :: Lens' ReservedNode (Maybe Text)
rnOfferingType = lens _rnOfferingType (\ s a -> s{_rnOfferingType = a});

-- | The hourly rate Amazon Redshift charges you for this reserved node.
rnUsagePrice :: Lens' ReservedNode (Maybe Double)
rnUsagePrice = lens _rnUsagePrice (\ s a -> s{_rnUsagePrice = a});

-- | The node type of the reserved node.
rnNodeType :: Lens' ReservedNode (Maybe Text)
rnNodeType = lens _rnNodeType (\ s a -> s{_rnNodeType = a});

-- | The recurring charges for the reserved node.
rnRecurringCharges :: Lens' ReservedNode [RecurringCharge]
rnRecurringCharges = lens _rnRecurringCharges (\ s a -> s{_rnRecurringCharges = a}) . _Default;

-- | The fixed cost Amazon Redshift charges you for this reserved node.
rnFixedPrice :: Lens' ReservedNode (Maybe Double)
rnFixedPrice = lens _rnFixedPrice (\ s a -> s{_rnFixedPrice = a});

-- | The duration of the node reservation in seconds.
rnDuration :: Lens' ReservedNode (Maybe Int)
rnDuration = lens _rnDuration (\ s a -> s{_rnDuration = a});

instance FromXML ReservedNode where
        parseXML x
          = ReservedNode' <$>
              (x .@? "State") <*> (x .@? "CurrencyCode") <*>
                (x .@? "StartTime")
                <*> (x .@? "NodeCount")
                <*> (x .@? "ReservedNodeOfferingId")
                <*> (x .@? "ReservedNodeId")
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*> (x .@? "NodeType")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")

-- | Describes a reserved node offering.
--
-- /See:/ 'reservedNodeOffering' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnoCurrencyCode'
--
-- * 'rnoReservedNodeOfferingId'
--
-- * 'rnoOfferingType'
--
-- * 'rnoUsagePrice'
--
-- * 'rnoNodeType'
--
-- * 'rnoRecurringCharges'
--
-- * 'rnoFixedPrice'
--
-- * 'rnoDuration'
data ReservedNodeOffering = ReservedNodeOffering'
    { _rnoCurrencyCode           :: !(Maybe Text)
    , _rnoReservedNodeOfferingId :: !(Maybe Text)
    , _rnoOfferingType           :: !(Maybe Text)
    , _rnoUsagePrice             :: !(Maybe Double)
    , _rnoNodeType               :: !(Maybe Text)
    , _rnoRecurringCharges       :: !(Maybe [RecurringCharge])
    , _rnoFixedPrice             :: !(Maybe Double)
    , _rnoDuration               :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'ReservedNodeOffering' smart constructor.
reservedNodeOffering :: ReservedNodeOffering
reservedNodeOffering =
    ReservedNodeOffering'
    { _rnoCurrencyCode = Nothing
    , _rnoReservedNodeOfferingId = Nothing
    , _rnoOfferingType = Nothing
    , _rnoUsagePrice = Nothing
    , _rnoNodeType = Nothing
    , _rnoRecurringCharges = Nothing
    , _rnoFixedPrice = Nothing
    , _rnoDuration = Nothing
    }

-- | The currency code for the compute nodes offering.
rnoCurrencyCode :: Lens' ReservedNodeOffering (Maybe Text)
rnoCurrencyCode = lens _rnoCurrencyCode (\ s a -> s{_rnoCurrencyCode = a});

-- | The offering identifier.
rnoReservedNodeOfferingId :: Lens' ReservedNodeOffering (Maybe Text)
rnoReservedNodeOfferingId = lens _rnoReservedNodeOfferingId (\ s a -> s{_rnoReservedNodeOfferingId = a});

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
rnoOfferingType :: Lens' ReservedNodeOffering (Maybe Text)
rnoOfferingType = lens _rnoOfferingType (\ s a -> s{_rnoOfferingType = a});

-- | The rate you are charged for each hour the cluster that is using the
-- offering is running.
rnoUsagePrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoUsagePrice = lens _rnoUsagePrice (\ s a -> s{_rnoUsagePrice = a});

-- | The node type offered by the reserved node offering.
rnoNodeType :: Lens' ReservedNodeOffering (Maybe Text)
rnoNodeType = lens _rnoNodeType (\ s a -> s{_rnoNodeType = a});

-- | The charge to your account regardless of whether you are creating any
-- clusters using the node offering. Recurring charges are only in effect
-- for heavy-utilization reserved nodes.
rnoRecurringCharges :: Lens' ReservedNodeOffering [RecurringCharge]
rnoRecurringCharges = lens _rnoRecurringCharges (\ s a -> s{_rnoRecurringCharges = a}) . _Default;

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- node offering.
rnoFixedPrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoFixedPrice = lens _rnoFixedPrice (\ s a -> s{_rnoFixedPrice = a});

-- | The duration, in seconds, for which the offering will reserve the node.
rnoDuration :: Lens' ReservedNodeOffering (Maybe Int)
rnoDuration = lens _rnoDuration (\ s a -> s{_rnoDuration = a});

instance FromXML ReservedNodeOffering where
        parseXML x
          = ReservedNodeOffering' <$>
              (x .@? "CurrencyCode") <*>
                (x .@? "ReservedNodeOfferingId")
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*> (x .@? "NodeType")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
--
-- /See:/ 'restoreStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsEstimatedTimeToCompletionInSeconds'
--
-- * 'rsStatus'
--
-- * 'rsCurrentRestoreRateInMegaBytesPerSecond'
--
-- * 'rsProgressInMegaBytes'
--
-- * 'rsElapsedTimeInSeconds'
--
-- * 'rsSnapshotSizeInMegaBytes'
data RestoreStatus = RestoreStatus'
    { _rsEstimatedTimeToCompletionInSeconds     :: !(Maybe Integer)
    , _rsStatus                                 :: !(Maybe Text)
    , _rsCurrentRestoreRateInMegaBytesPerSecond :: !(Maybe Double)
    , _rsProgressInMegaBytes                    :: !(Maybe Integer)
    , _rsElapsedTimeInSeconds                   :: !(Maybe Integer)
    , _rsSnapshotSizeInMegaBytes                :: !(Maybe Integer)
    } deriving (Eq,Read,Show)

-- | 'RestoreStatus' smart constructor.
restoreStatus :: RestoreStatus
restoreStatus =
    RestoreStatus'
    { _rsEstimatedTimeToCompletionInSeconds = Nothing
    , _rsStatus = Nothing
    , _rsCurrentRestoreRateInMegaBytesPerSecond = Nothing
    , _rsProgressInMegaBytes = Nothing
    , _rsElapsedTimeInSeconds = Nothing
    , _rsSnapshotSizeInMegaBytes = Nothing
    }

-- | The estimate of the time remaining before the restore will complete.
-- Returns 0 for a completed restore.
rsEstimatedTimeToCompletionInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsEstimatedTimeToCompletionInSeconds = lens _rsEstimatedTimeToCompletionInSeconds (\ s a -> s{_rsEstimatedTimeToCompletionInSeconds = a});

-- | The status of the restore action. Returns starting, restoring,
-- completed, or failed.
rsStatus :: Lens' RestoreStatus (Maybe Text)
rsStatus = lens _rsStatus (\ s a -> s{_rsStatus = a});

-- | The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup.
rsCurrentRestoreRateInMegaBytesPerSecond :: Lens' RestoreStatus (Maybe Double)
rsCurrentRestoreRateInMegaBytesPerSecond = lens _rsCurrentRestoreRateInMegaBytesPerSecond (\ s a -> s{_rsCurrentRestoreRateInMegaBytesPerSecond = a});

-- | The number of megabytes that have been transferred from snapshot
-- storage.
rsProgressInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsProgressInMegaBytes = lens _rsProgressInMegaBytes (\ s a -> s{_rsProgressInMegaBytes = a});

-- | The amount of time an in-progress restore has been running, or the
-- amount of time it took a completed restore to finish.
rsElapsedTimeInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsElapsedTimeInSeconds = lens _rsElapsedTimeInSeconds (\ s a -> s{_rsElapsedTimeInSeconds = a});

-- | The size of the set of snapshot data used to restore the cluster.
rsSnapshotSizeInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsSnapshotSizeInMegaBytes = lens _rsSnapshotSizeInMegaBytes (\ s a -> s{_rsSnapshotSizeInMegaBytes = a});

instance FromXML RestoreStatus where
        parseXML x
          = RestoreStatus' <$>
              (x .@? "EstimatedTimeToCompletionInSeconds") <*>
                (x .@? "Status")
                <*> (x .@? "CurrentRestoreRateInMegaBytesPerSecond")
                <*> (x .@? "ProgressInMegaBytes")
                <*> (x .@? "ElapsedTimeInSeconds")
                <*> (x .@? "SnapshotSizeInMegaBytes")

-- | Describes a snapshot.
--
-- /See:/ 'snapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'snaRestorableNodeTypes'
--
-- * 'snaStatus'
--
-- * 'snaAccountsWithRestoreAccess'
--
-- * 'snaSnapshotIdentifier'
--
-- * 'snaEncryptedWithHSM'
--
-- * 'snaMasterUsername'
--
-- * 'snaSourceRegion'
--
-- * 'snaVPCId'
--
-- * 'snaBackupProgressInMegaBytes'
--
-- * 'snaEncrypted'
--
-- * 'snaClusterIdentifier'
--
-- * 'snaNumberOfNodes'
--
-- * 'snaSnapshotType'
--
-- * 'snaAvailabilityZone'
--
-- * 'snaKMSKeyId'
--
-- * 'snaCurrentBackupRateInMegaBytesPerSecond'
--
-- * 'snaSnapshotCreateTime'
--
-- * 'snaClusterVersion'
--
-- * 'snaOwnerAccount'
--
-- * 'snaNodeType'
--
-- * 'snaClusterCreateTime'
--
-- * 'snaElapsedTimeInSeconds'
--
-- * 'snaEstimatedSecondsToCompletion'
--
-- * 'snaTotalBackupSizeInMegaBytes'
--
-- * 'snaDBName'
--
-- * 'snaTags'
--
-- * 'snaActualIncrementalBackupSizeInMegaBytes'
--
-- * 'snaPort'
data Snapshot = Snapshot'
    { _snaRestorableNodeTypes                    :: !(Maybe [Text])
    , _snaStatus                                 :: !(Maybe Text)
    , _snaAccountsWithRestoreAccess              :: !(Maybe [AccountWithRestoreAccess])
    , _snaSnapshotIdentifier                     :: !(Maybe Text)
    , _snaEncryptedWithHSM                       :: !(Maybe Bool)
    , _snaMasterUsername                         :: !(Maybe Text)
    , _snaSourceRegion                           :: !(Maybe Text)
    , _snaVPCId                                  :: !(Maybe Text)
    , _snaBackupProgressInMegaBytes              :: !(Maybe Double)
    , _snaEncrypted                              :: !(Maybe Bool)
    , _snaClusterIdentifier                      :: !(Maybe Text)
    , _snaNumberOfNodes                          :: !(Maybe Int)
    , _snaSnapshotType                           :: !(Maybe Text)
    , _snaAvailabilityZone                       :: !(Maybe Text)
    , _snaKMSKeyId                               :: !(Maybe Text)
    , _snaCurrentBackupRateInMegaBytesPerSecond  :: !(Maybe Double)
    , _snaSnapshotCreateTime                     :: !(Maybe ISO8601)
    , _snaClusterVersion                         :: !(Maybe Text)
    , _snaOwnerAccount                           :: !(Maybe Text)
    , _snaNodeType                               :: !(Maybe Text)
    , _snaClusterCreateTime                      :: !(Maybe ISO8601)
    , _snaElapsedTimeInSeconds                   :: !(Maybe Integer)
    , _snaEstimatedSecondsToCompletion           :: !(Maybe Integer)
    , _snaTotalBackupSizeInMegaBytes             :: !(Maybe Double)
    , _snaDBName                                 :: !(Maybe Text)
    , _snaTags                                   :: !(Maybe [Tag])
    , _snaActualIncrementalBackupSizeInMegaBytes :: !(Maybe Double)
    , _snaPort                                   :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'Snapshot' smart constructor.
snapshot :: Snapshot
snapshot =
    Snapshot'
    { _snaRestorableNodeTypes = Nothing
    , _snaStatus = Nothing
    , _snaAccountsWithRestoreAccess = Nothing
    , _snaSnapshotIdentifier = Nothing
    , _snaEncryptedWithHSM = Nothing
    , _snaMasterUsername = Nothing
    , _snaSourceRegion = Nothing
    , _snaVPCId = Nothing
    , _snaBackupProgressInMegaBytes = Nothing
    , _snaEncrypted = Nothing
    , _snaClusterIdentifier = Nothing
    , _snaNumberOfNodes = Nothing
    , _snaSnapshotType = Nothing
    , _snaAvailabilityZone = Nothing
    , _snaKMSKeyId = Nothing
    , _snaCurrentBackupRateInMegaBytesPerSecond = Nothing
    , _snaSnapshotCreateTime = Nothing
    , _snaClusterVersion = Nothing
    , _snaOwnerAccount = Nothing
    , _snaNodeType = Nothing
    , _snaClusterCreateTime = Nothing
    , _snaElapsedTimeInSeconds = Nothing
    , _snaEstimatedSecondsToCompletion = Nothing
    , _snaTotalBackupSizeInMegaBytes = Nothing
    , _snaDBName = Nothing
    , _snaTags = Nothing
    , _snaActualIncrementalBackupSizeInMegaBytes = Nothing
    , _snaPort = Nothing
    }

-- | The list of node types that this cluster snapshot is able to restore
-- into.
snaRestorableNodeTypes :: Lens' Snapshot [Text]
snaRestorableNodeTypes = lens _snaRestorableNodeTypes (\ s a -> s{_snaRestorableNodeTypes = a}) . _Default;

-- | The snapshot status. The value of the status depends on the API
-- operation used.
--
-- -   CreateClusterSnapshot and CopyClusterSnapshot returns status as
--     \"creating\".
-- -   DescribeClusterSnapshots returns status as \"creating\",
--     \"available\", \"final snapshot\", or \"failed\".
-- -   DeleteClusterSnapshot returns status as \"deleted\".
snaStatus :: Lens' Snapshot (Maybe Text)
snaStatus = lens _snaStatus (\ s a -> s{_snaStatus = a});

-- | A list of the AWS customer accounts authorized to restore the snapshot.
-- Returns @null@ if no accounts are authorized. Visible only to the
-- snapshot owner.
snaAccountsWithRestoreAccess :: Lens' Snapshot [AccountWithRestoreAccess]
snaAccountsWithRestoreAccess = lens _snaAccountsWithRestoreAccess (\ s a -> s{_snaAccountsWithRestoreAccess = a}) . _Default;

-- | The snapshot identifier that is provided in the request.
snaSnapshotIdentifier :: Lens' Snapshot (Maybe Text)
snaSnapshotIdentifier = lens _snaSnapshotIdentifier (\ s a -> s{_snaSnapshotIdentifier = a});

-- | A boolean that indicates whether the snapshot data is encrypted using
-- the HSM keys of the source cluster. @true@ indicates that the data is
-- encrypted using HSM keys.
snaEncryptedWithHSM :: Lens' Snapshot (Maybe Bool)
snaEncryptedWithHSM = lens _snaEncryptedWithHSM (\ s a -> s{_snaEncryptedWithHSM = a});

-- | The master user name for the cluster.
snaMasterUsername :: Lens' Snapshot (Maybe Text)
snaMasterUsername = lens _snaMasterUsername (\ s a -> s{_snaMasterUsername = a});

-- | The source region from which the snapshot was copied.
snaSourceRegion :: Lens' Snapshot (Maybe Text)
snaSourceRegion = lens _snaSourceRegion (\ s a -> s{_snaSourceRegion = a});

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a
-- VPC. Otherwise, this field is not in the output.
snaVPCId :: Lens' Snapshot (Maybe Text)
snaVPCId = lens _snaVPCId (\ s a -> s{_snaVPCId = a});

-- | The number of megabytes that have been transferred to the snapshot
-- backup.
snaBackupProgressInMegaBytes :: Lens' Snapshot (Maybe Double)
snaBackupProgressInMegaBytes = lens _snaBackupProgressInMegaBytes (\ s a -> s{_snaBackupProgressInMegaBytes = a});

-- | If @true@, the data in the snapshot is encrypted at rest.
snaEncrypted :: Lens' Snapshot (Maybe Bool)
snaEncrypted = lens _snaEncrypted (\ s a -> s{_snaEncrypted = a});

-- | The identifier of the cluster for which the snapshot was taken.
snaClusterIdentifier :: Lens' Snapshot (Maybe Text)
snaClusterIdentifier = lens _snaClusterIdentifier (\ s a -> s{_snaClusterIdentifier = a});

-- | The number of nodes in the cluster.
snaNumberOfNodes :: Lens' Snapshot (Maybe Int)
snaNumberOfNodes = lens _snaNumberOfNodes (\ s a -> s{_snaNumberOfNodes = a});

-- | The snapshot type. Snapshots created using CreateClusterSnapshot and
-- CopyClusterSnapshot will be of type \"manual\".
snaSnapshotType :: Lens' Snapshot (Maybe Text)
snaSnapshotType = lens _snaSnapshotType (\ s a -> s{_snaSnapshotType = a});

-- | The Availability Zone in which the cluster was created.
snaAvailabilityZone :: Lens' Snapshot (Maybe Text)
snaAvailabilityZone = lens _snaAvailabilityZone (\ s a -> s{_snaAvailabilityZone = a});

-- | The AWS Key Management Service (KMS) key ID of the encryption key that
-- was used to encrypt data in the cluster from which the snapshot was
-- taken.
snaKMSKeyId :: Lens' Snapshot (Maybe Text)
snaKMSKeyId = lens _snaKMSKeyId (\ s a -> s{_snaKMSKeyId = a});

-- | The number of megabytes per second being transferred to the snapshot
-- backup. Returns @0@ for a completed backup.
snaCurrentBackupRateInMegaBytesPerSecond :: Lens' Snapshot (Maybe Double)
snaCurrentBackupRateInMegaBytesPerSecond = lens _snaCurrentBackupRateInMegaBytesPerSecond (\ s a -> s{_snaCurrentBackupRateInMegaBytesPerSecond = a});

-- | The time (UTC) when Amazon Redshift began the snapshot. A snapshot
-- contains a copy of the cluster data as of this exact time.
snaSnapshotCreateTime :: Lens' Snapshot (Maybe UTCTime)
snaSnapshotCreateTime = lens _snaSnapshotCreateTime (\ s a -> s{_snaSnapshotCreateTime = a}) . mapping _Time;

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
snaClusterVersion :: Lens' Snapshot (Maybe Text)
snaClusterVersion = lens _snaClusterVersion (\ s a -> s{_snaClusterVersion = a});

-- | For manual snapshots, the AWS customer account used to create or copy
-- the snapshot. For automatic snapshots, the owner of the cluster. The
-- owner can perform all snapshot actions, such as sharing a manual
-- snapshot.
snaOwnerAccount :: Lens' Snapshot (Maybe Text)
snaOwnerAccount = lens _snaOwnerAccount (\ s a -> s{_snaOwnerAccount = a});

-- | The node type of the nodes in the cluster.
snaNodeType :: Lens' Snapshot (Maybe Text)
snaNodeType = lens _snaNodeType (\ s a -> s{_snaNodeType = a});

-- | The time (UTC) when the cluster was originally created.
snaClusterCreateTime :: Lens' Snapshot (Maybe UTCTime)
snaClusterCreateTime = lens _snaClusterCreateTime (\ s a -> s{_snaClusterCreateTime = a}) . mapping _Time;

-- | The amount of time an in-progress snapshot backup has been running, or
-- the amount of time it took a completed backup to finish.
snaElapsedTimeInSeconds :: Lens' Snapshot (Maybe Integer)
snaElapsedTimeInSeconds = lens _snaElapsedTimeInSeconds (\ s a -> s{_snaElapsedTimeInSeconds = a});

-- | The estimate of the time remaining before the snapshot backup will
-- complete. Returns @0@ for a completed backup.
snaEstimatedSecondsToCompletion :: Lens' Snapshot (Maybe Integer)
snaEstimatedSecondsToCompletion = lens _snaEstimatedSecondsToCompletion (\ s a -> s{_snaEstimatedSecondsToCompletion = a});

-- | The size of the complete set of backup data that would be used to
-- restore the cluster.
snaTotalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
snaTotalBackupSizeInMegaBytes = lens _snaTotalBackupSizeInMegaBytes (\ s a -> s{_snaTotalBackupSizeInMegaBytes = a});

-- | The name of the database that was created when the cluster was created.
snaDBName :: Lens' Snapshot (Maybe Text)
snaDBName = lens _snaDBName (\ s a -> s{_snaDBName = a});

-- | The list of tags for the cluster snapshot.
snaTags :: Lens' Snapshot [Tag]
snaTags = lens _snaTags (\ s a -> s{_snaTags = a}) . _Default;

-- | The size of the incremental backup.
snaActualIncrementalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
snaActualIncrementalBackupSizeInMegaBytes = lens _snaActualIncrementalBackupSizeInMegaBytes (\ s a -> s{_snaActualIncrementalBackupSizeInMegaBytes = a});

-- | The port that the cluster is listening on.
snaPort :: Lens' Snapshot (Maybe Int)
snaPort = lens _snaPort (\ s a -> s{_snaPort = a});

instance FromXML Snapshot where
        parseXML x
          = Snapshot' <$>
              (x .@? "RestorableNodeTypes" .!@ mempty >>=
                 may (parseXMLList "NodeType"))
                <*> (x .@? "Status")
                <*>
                (x .@? "AccountsWithRestoreAccess" .!@ mempty >>=
                   may (parseXMLList "AccountWithRestoreAccess"))
                <*> (x .@? "SnapshotIdentifier")
                <*> (x .@? "EncryptedWithHSM")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "SourceRegion")
                <*> (x .@? "VpcId")
                <*> (x .@? "BackupProgressInMegaBytes")
                <*> (x .@? "Encrypted")
                <*> (x .@? "ClusterIdentifier")
                <*> (x .@? "NumberOfNodes")
                <*> (x .@? "SnapshotType")
                <*> (x .@? "AvailabilityZone")
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "CurrentBackupRateInMegaBytesPerSecond")
                <*> (x .@? "SnapshotCreateTime")
                <*> (x .@? "ClusterVersion")
                <*> (x .@? "OwnerAccount")
                <*> (x .@? "NodeType")
                <*> (x .@? "ClusterCreateTime")
                <*> (x .@? "ElapsedTimeInSeconds")
                <*> (x .@? "EstimatedSecondsToCompletion")
                <*> (x .@? "TotalBackupSizeInMegaBytes")
                <*> (x .@? "DBName")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))
                <*> (x .@? "ActualIncrementalBackupSizeInMegaBytes")
                <*> (x .@? "Port")

-- | The snapshot copy grant that grants Amazon Redshift permission to
-- encrypt copied snapshots with the specified customer master key (CMK)
-- from AWS KMS in the destination region.
--
-- For more information about managing snapshot copy grants, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- /See:/ 'snapshotCopyGrant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scgKMSKeyId'
--
-- * 'scgSnapshotCopyGrantName'
--
-- * 'scgTags'
data SnapshotCopyGrant = SnapshotCopyGrant'
    { _scgKMSKeyId              :: !(Maybe Text)
    , _scgSnapshotCopyGrantName :: !(Maybe Text)
    , _scgTags                  :: !(Maybe [Tag])
    } deriving (Eq,Read,Show)

-- | 'SnapshotCopyGrant' smart constructor.
snapshotCopyGrant :: SnapshotCopyGrant
snapshotCopyGrant =
    SnapshotCopyGrant'
    { _scgKMSKeyId = Nothing
    , _scgSnapshotCopyGrantName = Nothing
    , _scgTags = Nothing
    }

-- | The unique identifier of the customer master key (CMK) in AWS KMS to
-- which Amazon Redshift is granted permission.
scgKMSKeyId :: Lens' SnapshotCopyGrant (Maybe Text)
scgKMSKeyId = lens _scgKMSKeyId (\ s a -> s{_scgKMSKeyId = a});

-- | The name of the snapshot copy grant.
scgSnapshotCopyGrantName :: Lens' SnapshotCopyGrant (Maybe Text)
scgSnapshotCopyGrantName = lens _scgSnapshotCopyGrantName (\ s a -> s{_scgSnapshotCopyGrantName = a});

-- | A list of tag instances.
scgTags :: Lens' SnapshotCopyGrant [Tag]
scgTags = lens _scgTags (\ s a -> s{_scgTags = a}) . _Default;

instance FromXML SnapshotCopyGrant where
        parseXML x
          = SnapshotCopyGrant' <$>
              (x .@? "KmsKeyId") <*>
                (x .@? "SnapshotCopyGrantName")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

-- | Describes a subnet.
--
-- /See:/ 'subnet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'subSubnetStatus'
--
-- * 'subSubnetIdentifier'
--
-- * 'subSubnetAvailabilityZone'
data Subnet = Subnet'
    { _subSubnetStatus           :: !(Maybe Text)
    , _subSubnetIdentifier       :: !(Maybe Text)
    , _subSubnetAvailabilityZone :: !(Maybe AvailabilityZone)
    } deriving (Eq,Read,Show)

-- | 'Subnet' smart constructor.
subnet :: Subnet
subnet =
    Subnet'
    { _subSubnetStatus = Nothing
    , _subSubnetIdentifier = Nothing
    , _subSubnetAvailabilityZone = Nothing
    }

-- | The status of the subnet.
subSubnetStatus :: Lens' Subnet (Maybe Text)
subSubnetStatus = lens _subSubnetStatus (\ s a -> s{_subSubnetStatus = a});

-- | The identifier of the subnet.
subSubnetIdentifier :: Lens' Subnet (Maybe Text)
subSubnetIdentifier = lens _subSubnetIdentifier (\ s a -> s{_subSubnetIdentifier = a});

-- | FIXME: Undocumented member.
subSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
subSubnetAvailabilityZone = lens _subSubnetAvailabilityZone (\ s a -> s{_subSubnetAvailabilityZone = a});

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (x .@? "SubnetStatus") <*> (x .@? "SubnetIdentifier")
                <*> (x .@? "SubnetAvailabilityZone")

-- | A tag consisting of a name\/value pair for a resource.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Tag' smart constructor.
tag :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | The value for the resource tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key, or name, for the resource tag.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | A tag and its associated resource.
--
-- /See:/ 'taggedResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trResourceType'
--
-- * 'trTag'
--
-- * 'trResourceName'
data TaggedResource = TaggedResource'
    { _trResourceType :: !(Maybe Text)
    , _trTag          :: !(Maybe Tag)
    , _trResourceName :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'TaggedResource' smart constructor.
taggedResource :: TaggedResource
taggedResource =
    TaggedResource'
    { _trResourceType = Nothing
    , _trTag = Nothing
    , _trResourceName = Nothing
    }

-- | The type of resource with which the tag is associated. Valid resource
-- types are:
--
-- -   Cluster
-- -   CIDR\/IP
-- -   EC2 security group
-- -   Snapshot
-- -   Cluster security group
-- -   Subnet group
-- -   HSM connection
-- -   HSM certificate
-- -   Parameter group
--
-- For more information about Amazon Redshift resource types and
-- constructing ARNs, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/constructing-redshift-arn.html Constructing an Amazon Redshift Amazon Resource Name (ARN)>
-- in the Amazon Redshift Cluster Management Guide.
trResourceType :: Lens' TaggedResource (Maybe Text)
trResourceType = lens _trResourceType (\ s a -> s{_trResourceType = a});

-- | The tag for the resource.
trTag :: Lens' TaggedResource (Maybe Tag)
trTag = lens _trTag (\ s a -> s{_trTag = a});

-- | The Amazon Resource Name (ARN) with which the tag is associated. For
-- example, @arn:aws:redshift:us-east-1:123456789:cluster:t1@.
trResourceName :: Lens' TaggedResource (Maybe Text)
trResourceName = lens _trResourceName (\ s a -> s{_trResourceName = a});

instance FromXML TaggedResource where
        parseXML x
          = TaggedResource' <$>
              (x .@? "ResourceType") <*> (x .@? "Tag") <*>
                (x .@? "ResourceName")

-- | Describes the members of a VPC security group.
--
-- /See:/ 'vpcSecurityGroupMembership' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsgmStatus'
--
-- * 'vsgmVPCSecurityGroupId'
data VPCSecurityGroupMembership = VPCSecurityGroupMembership'
    { _vsgmStatus             :: !(Maybe Text)
    , _vsgmVPCSecurityGroupId :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'VPCSecurityGroupMembership' smart constructor.
vpcSecurityGroupMembership :: VPCSecurityGroupMembership
vpcSecurityGroupMembership =
    VPCSecurityGroupMembership'
    { _vsgmStatus = Nothing
    , _vsgmVPCSecurityGroupId = Nothing
    }

-- | FIXME: Undocumented member.
vsgmStatus :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\ s a -> s{_vsgmStatus = a});

-- | FIXME: Undocumented member.
vsgmVPCSecurityGroupId :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmVPCSecurityGroupId = lens _vsgmVPCSecurityGroupId (\ s a -> s{_vsgmVPCSecurityGroupId = a});

instance FromXML VPCSecurityGroupMembership where
        parseXML x
          = VPCSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "VpcSecurityGroupId")
