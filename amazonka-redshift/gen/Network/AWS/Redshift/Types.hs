{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Redshift is a fast, fully managed, petabyte-scale data warehouse
-- service that makes it simple and cost-effective to efficiently analyze all
-- your data using your existing business intelligence tools. You can start
-- small for just $0.25 per hour with no commitments or upfront costs and
-- scale to a petabyte or more for $1,000 per terabyte per year, less than a
-- tenth of most other data warehousing solutions.
module Network.AWS.Redshift.Types
    (
    -- * Service
      Redshift
    -- ** Errors
    , RedshiftError (..)
    , _AccessToSnapshotDeniedFault
    , _AuthorizationAlreadyExistsFault
    , _AuthorizationNotFoundFault
    , _AuthorizationQuotaExceededFault
    , _BucketNotFoundFault
    , _ClusterAlreadyExistsFault
    , _ClusterNotFoundFault
    , _ClusterParameterGroupAlreadyExistsFault
    , _ClusterParameterGroupNotFoundFault
    , _ClusterParameterGroupQuotaExceededFault
    , _ClusterQuotaExceededFault
    , _ClusterSecurityGroupAlreadyExistsFault
    , _ClusterSecurityGroupNotFoundFault
    , _ClusterSecurityGroupQuotaExceededFault
    , _ClusterSnapshotAlreadyExistsFault
    , _ClusterSnapshotNotFoundFault
    , _ClusterSnapshotQuotaExceededFault
    , _ClusterSubnetGroupAlreadyExistsFault
    , _ClusterSubnetGroupNotFoundFault
    , _ClusterSubnetGroupQuotaExceededFault
    , _ClusterSubnetQuotaExceededFault
    , _CopyToRegionDisabledFault
    , _EventSubscriptionQuotaExceededFault
    , _HsmClientCertificateAlreadyExistsFault
    , _HsmClientCertificateNotFoundFault
    , _HsmClientCertificateQuotaExceededFault
    , _HsmConfigurationAlreadyExistsFault
    , _HsmConfigurationNotFoundFault
    , _HsmConfigurationQuotaExceededFault
    , _IncompatibleOrderableOptions
    , _InsufficientClusterCapacityFault
    , _InsufficientS3BucketPolicyFault
    , _InvalidClusterParameterGroupStateFault
    , _InvalidClusterSecurityGroupStateFault
    , _InvalidClusterSnapshotStateFault
    , _InvalidClusterStateFault
    , _InvalidClusterSubnetGroupStateFault
    , _InvalidClusterSubnetStateFault
    , _InvalidElasticIpFault
    , _InvalidHsmClientCertificateStateFault
    , _InvalidHsmConfigurationStateFault
    , _InvalidRestoreFault
    , _InvalidS3BucketNameFault
    , _InvalidS3KeyPrefixFault
    , _InvalidSubnet
    , _InvalidSubscriptionStateFault
    , _InvalidVPCNetworkStateFault
    , _NumberOfNodesPerClusterLimitExceededFault
    , _NumberOfNodesQuotaExceededFault
    , _RedshiftClient
    , _RedshiftSerializer
    , _RedshiftService
    , _ReservedNodeAlreadyExistsFault
    , _ReservedNodeNotFoundFault
    , _ReservedNodeOfferingNotFoundFault
    , _ReservedNodeQuotaExceededFault
    , _ResizeNotFoundFault
    , _SNSInvalidTopicFault
    , _SNSNoAuthorizationFault
    , _SNSTopicArnNotFoundFault
    , _SnapshotCopyAlreadyDisabledFault
    , _SnapshotCopyAlreadyEnabledFault
    , _SnapshotCopyDisabledFault
    , _SourceNotFoundFault
    , _SubnetAlreadyInUse
    , _SubscriptionAlreadyExistFault
    , _SubscriptionCategoryNotFoundFault
    , _SubscriptionEventIdNotFoundFault
    , _SubscriptionNotFoundFault
    , _SubscriptionSeverityNotFoundFault
    , _UnauthorizedOperation
    , _UnknownSnapshotCopyRegionFault
    , _UnsupportedOptionFault
    -- ** XML
    , xmlOptions

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
    , cClusterIdentifier
    , cNodeType
    , cClusterStatus
    , cModifyStatus
    , cMasterUsername
    , cDBName
    , cEndpoint
    , cClusterCreateTime
    , cAutomatedSnapshotRetentionPeriod
    , cClusterSecurityGroups
    , cVpcSecurityGroups
    , cClusterParameterGroups
    , cClusterSubnetGroupName
    , cVpcId
    , cAvailabilityZone
    , cPreferredMaintenanceWindow
    , cPendingModifiedValues
    , cClusterVersion
    , cAllowVersionUpgrade
    , cNumberOfNodes
    , cPubliclyAccessible
    , cEncrypted
    , cRestoreStatus
    , cHsmStatus
    , cClusterSnapshotCopyStatus
    , cClusterPublicKey
    , cClusterNodes
    , cElasticIpStatus
    , cClusterRevisionNumber

    -- * ClusterNode
    , ClusterNode
    , clusterNode
    , cnNodeRole
    , cnPrivateIPAddress
    , cnPublicIPAddress

    -- * ClusterParameterGroup
    , ClusterParameterGroup
    , clusterParameterGroup
    , cpgParameterGroupName
    , cpgParameterGroupFamily
    , cpgDescription

    -- * ClusterParameterGroupStatus
    , ClusterParameterGroupStatus
    , clusterParameterGroupStatus
    , cpgsParameterGroupName
    , cpgsParameterApplyStatus

    -- * ClusterSecurityGroup
    , ClusterSecurityGroup
    , clusterSecurityGroup
    , csgClusterSecurityGroupName
    , csgDescription
    , csgEC2SecurityGroups
    , csgIPRanges

    -- * ClusterSecurityGroupMembership
    , ClusterSecurityGroupMembership
    , clusterSecurityGroupMembership
    , csgmClusterSecurityGroupName
    , csgmStatus

    -- * ClusterSnapshotCopyStatus
    , ClusterSnapshotCopyStatus
    , clusterSnapshotCopyStatus
    , cscsDestinationRegion
    , cscsRetentionPeriod

    -- * ClusterSubnetGroup
    , ClusterSubnetGroup
    , clusterSubnetGroup
    , csgrClusterSubnetGroupName
    , csgrDescription
    , csgrVpcId
    , csgrSubnetGroupStatus
    , csgrSubnets

    -- * ClusterVersion
    , ClusterVersion
    , clusterVersion
    , cvClusterVersion
    , cvClusterParameterGroupFamily
    , cvDescription

    -- * DefaultClusterParameters
    , DefaultClusterParameters
    , defaultClusterParameters
    , dcp1ParameterGroupFamily
    , dcp1Marker
    , dcp1Parameters

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , eC2SecurityGroup
    , ecsgStatus
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId

    -- * ElasticIpStatus
    , ElasticIpStatus
    , elasticIpStatus
    , eisElasticIp
    , eisStatus

    -- * Endpoint'
    , Endpoint'
    , endpoint'
    , eAddress
    , ePort

    -- * Event
    , Event
    , event
    , erSourceIdentifier
    , erSourceType
    , erMessage
    , erEventCategories
    , erSeverity
    , erDate
    , erEventId

    -- * EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmSourceType
    , ecmEvents

    -- * EventInfoMap
    , EventInfoMap
    , eventInfoMap
    , eimEventId
    , eimEventCategories
    , eimEventDescription
    , eimSeverity

    -- * EventSubscription
    , EventSubscription
    , eventSubscription
    , esCustomerAwsId
    , esCustSubscriptionId
    , esSnsTopicArn
    , esStatus
    , esSubscriptionCreationTime
    , esSourceType
    , esSourceIdsList
    , esEventCategoriesList
    , esSeverity
    , esEnabled

    -- * HsmClientCertificate
    , HsmClientCertificate
    , hsmClientCertificate
    , hccHsmClientCertificateIdentifier
    , hccHsmClientCertificatePublicKey

    -- * HsmConfiguration
    , HsmConfiguration
    , hsmConfiguration
    , hcHsmConfigurationIdentifier
    , hcDescription
    , hcHsmIpAddress
    , hcHsmPartitionName

    -- * HsmStatus
    , HsmStatus
    , hsmStatus
    , hsHsmClientCertificateIdentifier
    , hsHsmConfigurationIdentifier
    , hsStatus

    -- * IPRange
    , IPRange
    , iPRange
    , iprStatus
    , iprCIDRIP

    -- * OrderableClusterOption
    , OrderableClusterOption
    , orderableClusterOption
    , ocoClusterVersion
    , ocoClusterType
    , ocoNodeType
    , ocoAvailabilityZones

    -- * Parameter
    , Parameter
    , parameter
    , pParameterName
    , pParameterValue
    , pDescription
    , pSource
    , pDataType
    , pAllowedValues
    , pIsModifiable
    , pMinimumEngineVersion

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvMasterUserPassword
    , pmvNodeType
    , pmvNumberOfNodes
    , pmvClusterType
    , pmvClusterVersion
    , pmvAutomatedSnapshotRetentionPeriod
    , pmvClusterIdentifier

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ReservedNode
    , ReservedNode
    , reservedNode
    , rnReservedNodeId
    , rnReservedNodeOfferingId
    , rnNodeType
    , rnStartTime
    , rnDuration
    , rnFixedPrice
    , rnUsagePrice
    , rnCurrencyCode
    , rnNodeCount
    , rnState
    , rnOfferingType
    , rnRecurringCharges

    -- * ReservedNodeOffering
    , ReservedNodeOffering
    , reservedNodeOffering
    , rnoReservedNodeOfferingId
    , rnoNodeType
    , rnoDuration
    , rnoFixedPrice
    , rnoUsagePrice
    , rnoCurrencyCode
    , rnoOfferingType
    , rnoRecurringCharges

    -- * RestoreStatus
    , RestoreStatus
    , restoreStatus
    , rsStatus
    , rsCurrentRestoreRateInMegaBytesPerSecond
    , rsSnapshotSizeInMegaBytes
    , rsProgressInMegaBytes
    , rsElapsedTimeInSeconds
    , rsEstimatedTimeToCompletionInSeconds

    -- * Snapshot
    , Snapshot
    , snapshot
    , sSnapshotIdentifier
    , sClusterIdentifier
    , sSnapshotCreateTime
    , sStatus
    , sPort
    , sAvailabilityZone
    , sClusterCreateTime
    , sMasterUsername
    , sClusterVersion
    , sSnapshotType
    , sNodeType
    , sNumberOfNodes
    , sDBName
    , sVpcId
    , sEncrypted
    , sEncryptedWithHSM
    , sAccountsWithRestoreAccess
    , sOwnerAccount
    , sTotalBackupSizeInMegaBytes
    , sActualIncrementalBackupSizeInMegaBytes
    , sBackupProgressInMegaBytes
    , sCurrentBackupRateInMegaBytesPerSecond
    , sEstimatedSecondsToCompletion
    , sElapsedTimeInSeconds
    , sSourceRegion

    -- * Subnet
    , Subnet
    , subnet
    , srSubnetIdentifier
    , srSubnetAvailabilityZone
    , srSubnetStatus

    -- * VpcSecurityGroupMembership
    , VpcSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmVpcSecurityGroupId
    , vsgmStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-12-01@) of the
-- @Amazon Redshift@ service.
data Redshift deriving (Typeable)

instance AWSService Redshift where
    type Sg Redshift = V4
    type Er Redshift = RedshiftError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "redshift"
        , _svcVersion  = "2012-12-01"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'Redshift' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data RedshiftError
      -- | The owner of the specified snapshot has not authorized your
      -- account to access the snapshot.
    = AccessToSnapshotDeniedFault
      -- | The specified CIDR block or EC2 security group is already
      -- authorized for the specified cluster security group.
    | AuthorizationAlreadyExistsFault
      -- | The specified CIDR IP range or EC2 security group is not
      -- authorized for the specified cluster security group.
    | AuthorizationNotFoundFault
      -- | The authorization quota for the cluster security group has been
      -- reached.
    | AuthorizationQuotaExceededFault
      -- | Could not find the specified S3 bucket.
    | BucketNotFoundFault
      -- | The account already has a cluster with the given identifier.
    | ClusterAlreadyExistsFault
      -- | The ClusterIdentifier parameter does not refer to an existing
      -- cluster.
    | ClusterNotFoundFault
      -- | A cluster parameter group with the same name already exists.
    | ClusterParameterGroupAlreadyExistsFault
      -- | The parameter group name does not refer to an existing parameter
      -- group.
    | ClusterParameterGroupNotFoundFault
      -- | The request would result in the user exceeding the allowed number
      -- of cluster parameter groups. For information about increasing
      -- your quota, go to Limits in Amazon Redshift in the Amazon
      -- Redshift Management Guide.
    | ClusterParameterGroupQuotaExceededFault
      -- | The request would exceed the allowed number of cluster instances
      -- for this account. For information about increasing your quota, go
      -- to Limits in Amazon Redshift in the Amazon Redshift Management
      -- Guide.
    | ClusterQuotaExceededFault
      -- | A cluster security group with the same name already exists.
    | ClusterSecurityGroupAlreadyExistsFault
      -- | The cluster security group name does not refer to an existing
      -- cluster security group.
    | ClusterSecurityGroupNotFoundFault
      -- | The request would result in the user exceeding the allowed number
      -- of cluster security groups. For information about increasing your
      -- quota, go to Limits in Amazon Redshift in the Amazon Redshift
      -- Management Guide.
    | ClusterSecurityGroupQuotaExceededFault
      -- | The value specified as a snapshot identifier is already used by
      -- an existing snapshot.
    | ClusterSnapshotAlreadyExistsFault
      -- | The snapshot identifier does not refer to an existing cluster
      -- snapshot.
    | ClusterSnapshotNotFoundFault
      -- | The request would result in the user exceeding the allowed number
      -- of cluster snapshots.
    | ClusterSnapshotQuotaExceededFault
      -- | A ClusterSubnetGroupName is already used by an existing cluster
      -- subnet group.
    | ClusterSubnetGroupAlreadyExistsFault
      -- | The cluster subnet group name does not refer to an existing
      -- cluster subnet group.
    | ClusterSubnetGroupNotFoundFault
      -- | The request would result in user exceeding the allowed number of
      -- cluster subnet groups. For information about increasing your
      -- quota, go to Limits in Amazon Redshift in the Amazon Redshift
      -- Management Guide.
    | ClusterSubnetGroupQuotaExceededFault
      -- | The request would result in user exceeding the allowed number of
      -- subnets in a cluster subnet groups. For information about
      -- increasing your quota, go to Limits in Amazon Redshift in the
      -- Amazon Redshift Management Guide.
    | ClusterSubnetQuotaExceededFault
      -- | Cross-region snapshot copy was temporarily disabled. Try your
      -- request again.
    | CopyToRegionDisabledFault
      -- | The request would exceed the allowed number of event
      -- subscriptions for this account. For information about increasing
      -- your quota, go to Limits in Amazon Redshift in the Amazon
      -- Redshift Management Guide.
    | EventSubscriptionQuotaExceededFault
      -- | There is already an existing Amazon Redshift HSM client
      -- certificate with the specified identifier.
    | HsmClientCertificateAlreadyExistsFault
      -- | There is no Amazon Redshift HSM client certificate with the
      -- specified identifier.
    | HsmClientCertificateNotFoundFault
      -- | The quota for HSM client certificates has been reached. For
      -- information about increasing your quota, go to Limits in Amazon
      -- Redshift in the Amazon Redshift Management Guide.
    | HsmClientCertificateQuotaExceededFault
      -- | There is already an existing Amazon Redshift HSM configuration
      -- with the specified identifier.
    | HsmConfigurationAlreadyExistsFault
      -- | There is no Amazon Redshift HSM configuration with the specified
      -- identifier.
    | HsmConfigurationNotFoundFault
      -- | The quota for HSM configurations has been reached. For
      -- information about increasing your quota, go to Limits in Amazon
      -- Redshift in the Amazon Redshift Management Guide.
    | HsmConfigurationQuotaExceededFault
      -- | The specified options are incompatible.
    | IncompatibleOrderableOptions
      -- | The number of nodes specified exceeds the allotted capacity of
      -- the cluster.
    | InsufficientClusterCapacityFault
      -- | The cluster does not have read bucket or put object permissions
      -- on the S3 bucket specified when enabling logging.
    | InsufficientS3BucketPolicyFault
      -- | The cluster parameter group action can not be completed because
      -- another task is in progress that involves the parameter group.
      -- Wait a few moments and try the operation again.
    | InvalidClusterParameterGroupStateFault
      -- | The state of the cluster security group is not available.
    | InvalidClusterSecurityGroupStateFault
      -- | The state of the cluster snapshot is not available, or other
      -- accounts are authorized to access the snapshot.
    | InvalidClusterSnapshotStateFault
      -- | The specified cluster is not in the available state.
    | InvalidClusterStateFault
      -- | The cluster subnet group cannot be deleted because it is in use.
    | InvalidClusterSubnetGroupStateFault
      -- | The state of the subnet is invalid.
    | InvalidClusterSubnetStateFault
      -- | The Elastic IP (EIP) is invalid or cannot be found.
    | InvalidElasticIpFault
      -- | The specified HSM client certificate is not in the available
      -- state, or it is still in use by one or more Amazon Redshift
      -- clusters.
    | InvalidHsmClientCertificateStateFault
      -- | The specified HSM configuration is not in the available state, or
      -- it is still in use by one or more Amazon Redshift clusters.
    | InvalidHsmConfigurationStateFault
      -- | The restore is invalid.
    | InvalidRestoreFault
      -- | The S3 bucket name is invalid. For more information about naming
      -- rules, go to Bucket Restrictions and Limitations in the Amazon
      -- Simple Storage Service (S3) Developer Guide.
    | InvalidS3BucketNameFault
      -- | The string specified for the logging S3 key prefix does not
      -- comply with the documented constraints.
    | InvalidS3KeyPrefixFault
      -- | The requested subnet is not valid, or not all of the subnets are
      -- in the same VPC.
    | InvalidSubnet
      -- | The subscription request is invalid because it is a duplicate
      -- request. This subscription request is already in progress.
    | InvalidSubscriptionStateFault
      -- | The cluster subnet group does not cover all Availability Zones.
    | InvalidVPCNetworkStateFault
      -- | The operation would exceed the number of nodes allowed for a
      -- cluster.
    | NumberOfNodesPerClusterLimitExceededFault
      -- | The operation would exceed the number of nodes allotted to the
      -- account. For information about increasing your quota, go to
      -- Limits in Amazon Redshift in the Amazon Redshift Management
      -- Guide.
    | NumberOfNodesQuotaExceededFault
    | RedshiftClient HttpException
    | RedshiftSerializer String
    | RedshiftService String
      -- | User already has a reservation with the given identifier.
    | ReservedNodeAlreadyExistsFault
      -- | The specified reserved compute node not found.
    | ReservedNodeNotFoundFault
      -- | Specified offering does not exist.
    | ReservedNodeOfferingNotFoundFault
      -- | Request would exceed the user's compute node quota. For
      -- information about increasing your quota, go to Limits in Amazon
      -- Redshift in the Amazon Redshift Management Guide.
    | ReservedNodeQuotaExceededFault
      -- | A resize operation for the specified cluster is not found.
    | ResizeNotFoundFault
      -- | Amazon SNS has responded that there is a problem with the
      -- specified Amazon SNS topic.
    | SNSInvalidTopicFault
      -- | You do not have permission to publish to the specified Amazon SNS
      -- topic.
    | SNSNoAuthorizationFault
      -- | An Amazon SNS topic with the specified Amazon Resource Name (ARN)
      -- does not exist.
    | SNSTopicArnNotFoundFault
      -- | The cluster already has cross-region snapshot copy disabled.
    | SnapshotCopyAlreadyDisabledFault
      -- | The cluster already has cross-region snapshot copy enabled.
    | SnapshotCopyAlreadyEnabledFault
      -- | Cross-region snapshot copy was temporarily disabled. Try your
      -- request again.
    | SnapshotCopyDisabledFault
      -- | The specified Amazon Redshift event source could not be found.
    | SourceNotFoundFault
      -- | A specified subnet is already in use by another cluster.
    | SubnetAlreadyInUse
      -- | There is already an existing event notification subscription with
      -- the specified name.
    | SubscriptionAlreadyExistFault
      -- | The value specified for the event category was not one of the
      -- allowed values, or it specified a category that does not apply to
      -- the specified source type. The allowed values are Configuration,
      -- Management, Monitoring, and Security.
    | SubscriptionCategoryNotFoundFault
      -- | An Amazon Redshift event with the specified event ID does not
      -- exist.
    | SubscriptionEventIdNotFoundFault
      -- | An Amazon Redshift event notification subscription with the
      -- specified name does not exist.
    | SubscriptionNotFoundFault
      -- | The value specified for the event severity was not one of the
      -- allowed values, or it specified a severity that does not apply to
      -- the specified source type. The allowed values are ERROR and INFO.
    | SubscriptionSeverityNotFoundFault
      -- | Your account is not authorized to perform the requested
      -- operation.
    | UnauthorizedOperation
      -- | The specified region is incorrect or does not exist.
    | UnknownSnapshotCopyRegionFault
      -- | A request option was specified that is not supported.
    | UnsupportedOptionFault
      deriving (Show, Typeable, Generic)

instance AWSError RedshiftError where
    awsError = const "RedshiftError"

instance AWSServiceError RedshiftError where
    serviceError    = RedshiftService
    clientError     = RedshiftClient
    serializerError = RedshiftSerializer

instance Exception RedshiftError

-- | The owner of the specified snapshot has not authorized your account to
-- access the snapshot.
--
-- See: 'AccessToSnapshotDeniedFault'
_AccessToSnapshotDeniedFault :: Prism' RedshiftError ()
_AccessToSnapshotDeniedFault = prism
    (const AccessToSnapshotDeniedFault)
    (\case
        AccessToSnapshotDeniedFault -> Right ()
        x -> Left x)

-- | The specified CIDR block or EC2 security group is already authorized for
-- the specified cluster security group.
--
-- See: 'AuthorizationAlreadyExistsFault'
_AuthorizationAlreadyExistsFault :: Prism' RedshiftError ()
_AuthorizationAlreadyExistsFault = prism
    (const AuthorizationAlreadyExistsFault)
    (\case
        AuthorizationAlreadyExistsFault -> Right ()
        x -> Left x)

-- | The specified CIDR IP range or EC2 security group is not authorized for the
-- specified cluster security group.
--
-- See: 'AuthorizationNotFoundFault'
_AuthorizationNotFoundFault :: Prism' RedshiftError ()
_AuthorizationNotFoundFault = prism
    (const AuthorizationNotFoundFault)
    (\case
        AuthorizationNotFoundFault -> Right ()
        x -> Left x)

-- | The authorization quota for the cluster security group has been reached.
--
-- See: 'AuthorizationQuotaExceededFault'
_AuthorizationQuotaExceededFault :: Prism' RedshiftError ()
_AuthorizationQuotaExceededFault = prism
    (const AuthorizationQuotaExceededFault)
    (\case
        AuthorizationQuotaExceededFault -> Right ()
        x -> Left x)

-- | Could not find the specified S3 bucket.
--
-- See: 'BucketNotFoundFault'
_BucketNotFoundFault :: Prism' RedshiftError ()
_BucketNotFoundFault = prism
    (const BucketNotFoundFault)
    (\case
        BucketNotFoundFault -> Right ()
        x -> Left x)

-- | The account already has a cluster with the given identifier.
--
-- See: 'ClusterAlreadyExistsFault'
_ClusterAlreadyExistsFault :: Prism' RedshiftError ()
_ClusterAlreadyExistsFault = prism
    (const ClusterAlreadyExistsFault)
    (\case
        ClusterAlreadyExistsFault -> Right ()
        x -> Left x)

-- | The ClusterIdentifier parameter does not refer to an existing cluster.
--
-- See: 'ClusterNotFoundFault'
_ClusterNotFoundFault :: Prism' RedshiftError ()
_ClusterNotFoundFault = prism
    (const ClusterNotFoundFault)
    (\case
        ClusterNotFoundFault -> Right ()
        x -> Left x)

-- | A cluster parameter group with the same name already exists.
--
-- See: 'ClusterParameterGroupAlreadyExistsFault'
_ClusterParameterGroupAlreadyExistsFault :: Prism' RedshiftError ()
_ClusterParameterGroupAlreadyExistsFault = prism
    (const ClusterParameterGroupAlreadyExistsFault)
    (\case
        ClusterParameterGroupAlreadyExistsFault -> Right ()
        x -> Left x)

-- | The parameter group name does not refer to an existing parameter group.
--
-- See: 'ClusterParameterGroupNotFoundFault'
_ClusterParameterGroupNotFoundFault :: Prism' RedshiftError ()
_ClusterParameterGroupNotFoundFault = prism
    (const ClusterParameterGroupNotFoundFault)
    (\case
        ClusterParameterGroupNotFoundFault -> Right ()
        x -> Left x)

-- | The request would result in the user exceeding the allowed number of
-- cluster parameter groups. For information about increasing your quota, go
-- to Limits in Amazon Redshift in the Amazon Redshift Management Guide.
--
-- See: 'ClusterParameterGroupQuotaExceededFault'
_ClusterParameterGroupQuotaExceededFault :: Prism' RedshiftError ()
_ClusterParameterGroupQuotaExceededFault = prism
    (const ClusterParameterGroupQuotaExceededFault)
    (\case
        ClusterParameterGroupQuotaExceededFault -> Right ()
        x -> Left x)

-- | The request would exceed the allowed number of cluster instances for this
-- account. For information about increasing your quota, go to Limits in
-- Amazon Redshift in the Amazon Redshift Management Guide.
--
-- See: 'ClusterQuotaExceededFault'
_ClusterQuotaExceededFault :: Prism' RedshiftError ()
_ClusterQuotaExceededFault = prism
    (const ClusterQuotaExceededFault)
    (\case
        ClusterQuotaExceededFault -> Right ()
        x -> Left x)

-- | A cluster security group with the same name already exists.
--
-- See: 'ClusterSecurityGroupAlreadyExistsFault'
_ClusterSecurityGroupAlreadyExistsFault :: Prism' RedshiftError ()
_ClusterSecurityGroupAlreadyExistsFault = prism
    (const ClusterSecurityGroupAlreadyExistsFault)
    (\case
        ClusterSecurityGroupAlreadyExistsFault -> Right ()
        x -> Left x)

-- | The cluster security group name does not refer to an existing cluster
-- security group.
--
-- See: 'ClusterSecurityGroupNotFoundFault'
_ClusterSecurityGroupNotFoundFault :: Prism' RedshiftError ()
_ClusterSecurityGroupNotFoundFault = prism
    (const ClusterSecurityGroupNotFoundFault)
    (\case
        ClusterSecurityGroupNotFoundFault -> Right ()
        x -> Left x)

-- | The request would result in the user exceeding the allowed number of
-- cluster security groups. For information about increasing your quota, go to
-- Limits in Amazon Redshift in the Amazon Redshift Management Guide.
--
-- See: 'ClusterSecurityGroupQuotaExceededFault'
_ClusterSecurityGroupQuotaExceededFault :: Prism' RedshiftError ()
_ClusterSecurityGroupQuotaExceededFault = prism
    (const ClusterSecurityGroupQuotaExceededFault)
    (\case
        ClusterSecurityGroupQuotaExceededFault -> Right ()
        x -> Left x)

-- | The value specified as a snapshot identifier is already used by an existing
-- snapshot.
--
-- See: 'ClusterSnapshotAlreadyExistsFault'
_ClusterSnapshotAlreadyExistsFault :: Prism' RedshiftError ()
_ClusterSnapshotAlreadyExistsFault = prism
    (const ClusterSnapshotAlreadyExistsFault)
    (\case
        ClusterSnapshotAlreadyExistsFault -> Right ()
        x -> Left x)

-- | The snapshot identifier does not refer to an existing cluster snapshot.
--
-- See: 'ClusterSnapshotNotFoundFault'
_ClusterSnapshotNotFoundFault :: Prism' RedshiftError ()
_ClusterSnapshotNotFoundFault = prism
    (const ClusterSnapshotNotFoundFault)
    (\case
        ClusterSnapshotNotFoundFault -> Right ()
        x -> Left x)

-- | The request would result in the user exceeding the allowed number of
-- cluster snapshots.
--
-- See: 'ClusterSnapshotQuotaExceededFault'
_ClusterSnapshotQuotaExceededFault :: Prism' RedshiftError ()
_ClusterSnapshotQuotaExceededFault = prism
    (const ClusterSnapshotQuotaExceededFault)
    (\case
        ClusterSnapshotQuotaExceededFault -> Right ()
        x -> Left x)

-- | A ClusterSubnetGroupName is already used by an existing cluster subnet
-- group.
--
-- See: 'ClusterSubnetGroupAlreadyExistsFault'
_ClusterSubnetGroupAlreadyExistsFault :: Prism' RedshiftError ()
_ClusterSubnetGroupAlreadyExistsFault = prism
    (const ClusterSubnetGroupAlreadyExistsFault)
    (\case
        ClusterSubnetGroupAlreadyExistsFault -> Right ()
        x -> Left x)

-- | The cluster subnet group name does not refer to an existing cluster subnet
-- group.
--
-- See: 'ClusterSubnetGroupNotFoundFault'
_ClusterSubnetGroupNotFoundFault :: Prism' RedshiftError ()
_ClusterSubnetGroupNotFoundFault = prism
    (const ClusterSubnetGroupNotFoundFault)
    (\case
        ClusterSubnetGroupNotFoundFault -> Right ()
        x -> Left x)

-- | The request would result in user exceeding the allowed number of cluster
-- subnet groups. For information about increasing your quota, go to Limits in
-- Amazon Redshift in the Amazon Redshift Management Guide.
--
-- See: 'ClusterSubnetGroupQuotaExceededFault'
_ClusterSubnetGroupQuotaExceededFault :: Prism' RedshiftError ()
_ClusterSubnetGroupQuotaExceededFault = prism
    (const ClusterSubnetGroupQuotaExceededFault)
    (\case
        ClusterSubnetGroupQuotaExceededFault -> Right ()
        x -> Left x)

-- | The request would result in user exceeding the allowed number of subnets in
-- a cluster subnet groups. For information about increasing your quota, go to
-- Limits in Amazon Redshift in the Amazon Redshift Management Guide.
--
-- See: 'ClusterSubnetQuotaExceededFault'
_ClusterSubnetQuotaExceededFault :: Prism' RedshiftError ()
_ClusterSubnetQuotaExceededFault = prism
    (const ClusterSubnetQuotaExceededFault)
    (\case
        ClusterSubnetQuotaExceededFault -> Right ()
        x -> Left x)

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
--
-- See: 'CopyToRegionDisabledFault'
_CopyToRegionDisabledFault :: Prism' RedshiftError ()
_CopyToRegionDisabledFault = prism
    (const CopyToRegionDisabledFault)
    (\case
        CopyToRegionDisabledFault -> Right ()
        x -> Left x)

-- | The request would exceed the allowed number of event subscriptions for this
-- account. For information about increasing your quota, go to Limits in
-- Amazon Redshift in the Amazon Redshift Management Guide.
--
-- See: 'EventSubscriptionQuotaExceededFault'
_EventSubscriptionQuotaExceededFault :: Prism' RedshiftError ()
_EventSubscriptionQuotaExceededFault = prism
    (const EventSubscriptionQuotaExceededFault)
    (\case
        EventSubscriptionQuotaExceededFault -> Right ()
        x -> Left x)

-- | There is already an existing Amazon Redshift HSM client certificate with
-- the specified identifier.
--
-- See: 'HsmClientCertificateAlreadyExistsFault'
_HsmClientCertificateAlreadyExistsFault :: Prism' RedshiftError ()
_HsmClientCertificateAlreadyExistsFault = prism
    (const HsmClientCertificateAlreadyExistsFault)
    (\case
        HsmClientCertificateAlreadyExistsFault -> Right ()
        x -> Left x)

-- | There is no Amazon Redshift HSM client certificate with the specified
-- identifier.
--
-- See: 'HsmClientCertificateNotFoundFault'
_HsmClientCertificateNotFoundFault :: Prism' RedshiftError ()
_HsmClientCertificateNotFoundFault = prism
    (const HsmClientCertificateNotFoundFault)
    (\case
        HsmClientCertificateNotFoundFault -> Right ()
        x -> Left x)

-- | The quota for HSM client certificates has been reached. For information
-- about increasing your quota, go to Limits in Amazon Redshift in the Amazon
-- Redshift Management Guide.
--
-- See: 'HsmClientCertificateQuotaExceededFault'
_HsmClientCertificateQuotaExceededFault :: Prism' RedshiftError ()
_HsmClientCertificateQuotaExceededFault = prism
    (const HsmClientCertificateQuotaExceededFault)
    (\case
        HsmClientCertificateQuotaExceededFault -> Right ()
        x -> Left x)

-- | There is already an existing Amazon Redshift HSM configuration with the
-- specified identifier.
--
-- See: 'HsmConfigurationAlreadyExistsFault'
_HsmConfigurationAlreadyExistsFault :: Prism' RedshiftError ()
_HsmConfigurationAlreadyExistsFault = prism
    (const HsmConfigurationAlreadyExistsFault)
    (\case
        HsmConfigurationAlreadyExistsFault -> Right ()
        x -> Left x)

-- | There is no Amazon Redshift HSM configuration with the specified
-- identifier.
--
-- See: 'HsmConfigurationNotFoundFault'
_HsmConfigurationNotFoundFault :: Prism' RedshiftError ()
_HsmConfigurationNotFoundFault = prism
    (const HsmConfigurationNotFoundFault)
    (\case
        HsmConfigurationNotFoundFault -> Right ()
        x -> Left x)

-- | The quota for HSM configurations has been reached. For information about
-- increasing your quota, go to Limits in Amazon Redshift in the Amazon
-- Redshift Management Guide.
--
-- See: 'HsmConfigurationQuotaExceededFault'
_HsmConfigurationQuotaExceededFault :: Prism' RedshiftError ()
_HsmConfigurationQuotaExceededFault = prism
    (const HsmConfigurationQuotaExceededFault)
    (\case
        HsmConfigurationQuotaExceededFault -> Right ()
        x -> Left x)

-- | The specified options are incompatible.
--
-- See: 'IncompatibleOrderableOptions'
_IncompatibleOrderableOptions :: Prism' RedshiftError ()
_IncompatibleOrderableOptions = prism
    (const IncompatibleOrderableOptions)
    (\case
        IncompatibleOrderableOptions -> Right ()
        x -> Left x)

-- | The number of nodes specified exceeds the allotted capacity of the cluster.
--
-- See: 'InsufficientClusterCapacityFault'
_InsufficientClusterCapacityFault :: Prism' RedshiftError ()
_InsufficientClusterCapacityFault = prism
    (const InsufficientClusterCapacityFault)
    (\case
        InsufficientClusterCapacityFault -> Right ()
        x -> Left x)

-- | The cluster does not have read bucket or put object permissions on the S3
-- bucket specified when enabling logging.
--
-- See: 'InsufficientS3BucketPolicyFault'
_InsufficientS3BucketPolicyFault :: Prism' RedshiftError ()
_InsufficientS3BucketPolicyFault = prism
    (const InsufficientS3BucketPolicyFault)
    (\case
        InsufficientS3BucketPolicyFault -> Right ()
        x -> Left x)

-- | The cluster parameter group action can not be completed because another
-- task is in progress that involves the parameter group. Wait a few moments
-- and try the operation again.
--
-- See: 'InvalidClusterParameterGroupStateFault'
_InvalidClusterParameterGroupStateFault :: Prism' RedshiftError ()
_InvalidClusterParameterGroupStateFault = prism
    (const InvalidClusterParameterGroupStateFault)
    (\case
        InvalidClusterParameterGroupStateFault -> Right ()
        x -> Left x)

-- | The state of the cluster security group is not available.
--
-- See: 'InvalidClusterSecurityGroupStateFault'
_InvalidClusterSecurityGroupStateFault :: Prism' RedshiftError ()
_InvalidClusterSecurityGroupStateFault = prism
    (const InvalidClusterSecurityGroupStateFault)
    (\case
        InvalidClusterSecurityGroupStateFault -> Right ()
        x -> Left x)

-- | The state of the cluster snapshot is not available, or other accounts are
-- authorized to access the snapshot.
--
-- See: 'InvalidClusterSnapshotStateFault'
_InvalidClusterSnapshotStateFault :: Prism' RedshiftError ()
_InvalidClusterSnapshotStateFault = prism
    (const InvalidClusterSnapshotStateFault)
    (\case
        InvalidClusterSnapshotStateFault -> Right ()
        x -> Left x)

-- | The specified cluster is not in the available state.
--
-- See: 'InvalidClusterStateFault'
_InvalidClusterStateFault :: Prism' RedshiftError ()
_InvalidClusterStateFault = prism
    (const InvalidClusterStateFault)
    (\case
        InvalidClusterStateFault -> Right ()
        x -> Left x)

-- | The cluster subnet group cannot be deleted because it is in use.
--
-- See: 'InvalidClusterSubnetGroupStateFault'
_InvalidClusterSubnetGroupStateFault :: Prism' RedshiftError ()
_InvalidClusterSubnetGroupStateFault = prism
    (const InvalidClusterSubnetGroupStateFault)
    (\case
        InvalidClusterSubnetGroupStateFault -> Right ()
        x -> Left x)

-- | The state of the subnet is invalid.
--
-- See: 'InvalidClusterSubnetStateFault'
_InvalidClusterSubnetStateFault :: Prism' RedshiftError ()
_InvalidClusterSubnetStateFault = prism
    (const InvalidClusterSubnetStateFault)
    (\case
        InvalidClusterSubnetStateFault -> Right ()
        x -> Left x)

-- | The Elastic IP (EIP) is invalid or cannot be found.
--
-- See: 'InvalidElasticIpFault'
_InvalidElasticIpFault :: Prism' RedshiftError ()
_InvalidElasticIpFault = prism
    (const InvalidElasticIpFault)
    (\case
        InvalidElasticIpFault -> Right ()
        x -> Left x)

-- | The specified HSM client certificate is not in the available state, or it
-- is still in use by one or more Amazon Redshift clusters.
--
-- See: 'InvalidHsmClientCertificateStateFault'
_InvalidHsmClientCertificateStateFault :: Prism' RedshiftError ()
_InvalidHsmClientCertificateStateFault = prism
    (const InvalidHsmClientCertificateStateFault)
    (\case
        InvalidHsmClientCertificateStateFault -> Right ()
        x -> Left x)

-- | The specified HSM configuration is not in the available state, or it is
-- still in use by one or more Amazon Redshift clusters.
--
-- See: 'InvalidHsmConfigurationStateFault'
_InvalidHsmConfigurationStateFault :: Prism' RedshiftError ()
_InvalidHsmConfigurationStateFault = prism
    (const InvalidHsmConfigurationStateFault)
    (\case
        InvalidHsmConfigurationStateFault -> Right ()
        x -> Left x)

-- | The restore is invalid.
--
-- See: 'InvalidRestoreFault'
_InvalidRestoreFault :: Prism' RedshiftError ()
_InvalidRestoreFault = prism
    (const InvalidRestoreFault)
    (\case
        InvalidRestoreFault -> Right ()
        x -> Left x)

-- | The S3 bucket name is invalid. For more information about naming rules, go
-- to Bucket Restrictions and Limitations in the Amazon Simple Storage Service
-- (S3) Developer Guide.
--
-- See: 'InvalidS3BucketNameFault'
_InvalidS3BucketNameFault :: Prism' RedshiftError ()
_InvalidS3BucketNameFault = prism
    (const InvalidS3BucketNameFault)
    (\case
        InvalidS3BucketNameFault -> Right ()
        x -> Left x)

-- | The string specified for the logging S3 key prefix does not comply with the
-- documented constraints.
--
-- See: 'InvalidS3KeyPrefixFault'
_InvalidS3KeyPrefixFault :: Prism' RedshiftError ()
_InvalidS3KeyPrefixFault = prism
    (const InvalidS3KeyPrefixFault)
    (\case
        InvalidS3KeyPrefixFault -> Right ()
        x -> Left x)

-- | The requested subnet is not valid, or not all of the subnets are in the
-- same VPC.
--
-- See: 'InvalidSubnet'
_InvalidSubnet :: Prism' RedshiftError ()
_InvalidSubnet = prism
    (const InvalidSubnet)
    (\case
        InvalidSubnet -> Right ()
        x -> Left x)

-- | The subscription request is invalid because it is a duplicate request. This
-- subscription request is already in progress.
--
-- See: 'InvalidSubscriptionStateFault'
_InvalidSubscriptionStateFault :: Prism' RedshiftError ()
_InvalidSubscriptionStateFault = prism
    (const InvalidSubscriptionStateFault)
    (\case
        InvalidSubscriptionStateFault -> Right ()
        x -> Left x)

-- | The cluster subnet group does not cover all Availability Zones.
--
-- See: 'InvalidVPCNetworkStateFault'
_InvalidVPCNetworkStateFault :: Prism' RedshiftError ()
_InvalidVPCNetworkStateFault = prism
    (const InvalidVPCNetworkStateFault)
    (\case
        InvalidVPCNetworkStateFault -> Right ()
        x -> Left x)

-- | The operation would exceed the number of nodes allowed for a cluster.
--
-- See: 'NumberOfNodesPerClusterLimitExceededFault'
_NumberOfNodesPerClusterLimitExceededFault :: Prism' RedshiftError ()
_NumberOfNodesPerClusterLimitExceededFault = prism
    (const NumberOfNodesPerClusterLimitExceededFault)
    (\case
        NumberOfNodesPerClusterLimitExceededFault -> Right ()
        x -> Left x)

-- | The operation would exceed the number of nodes allotted to the account. For
-- information about increasing your quota, go to Limits in Amazon Redshift in
-- the Amazon Redshift Management Guide.
--
-- See: 'NumberOfNodesQuotaExceededFault'
_NumberOfNodesQuotaExceededFault :: Prism' RedshiftError ()
_NumberOfNodesQuotaExceededFault = prism
    (const NumberOfNodesQuotaExceededFault)
    (\case
        NumberOfNodesQuotaExceededFault -> Right ()
        x -> Left x)

-- | See: 'RedshiftClient'
_RedshiftClient :: Prism' RedshiftError HttpException
_RedshiftClient = prism
    RedshiftClient
    (\case
        RedshiftClient p1 -> Right p1
        x -> Left x)

-- | See: 'RedshiftSerializer'
_RedshiftSerializer :: Prism' RedshiftError String
_RedshiftSerializer = prism
    RedshiftSerializer
    (\case
        RedshiftSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'RedshiftService'
_RedshiftService :: Prism' RedshiftError String
_RedshiftService = prism
    RedshiftService
    (\case
        RedshiftService p1 -> Right p1
        x -> Left x)

-- | User already has a reservation with the given identifier.
--
-- See: 'ReservedNodeAlreadyExistsFault'
_ReservedNodeAlreadyExistsFault :: Prism' RedshiftError ()
_ReservedNodeAlreadyExistsFault = prism
    (const ReservedNodeAlreadyExistsFault)
    (\case
        ReservedNodeAlreadyExistsFault -> Right ()
        x -> Left x)

-- | The specified reserved compute node not found.
--
-- See: 'ReservedNodeNotFoundFault'
_ReservedNodeNotFoundFault :: Prism' RedshiftError ()
_ReservedNodeNotFoundFault = prism
    (const ReservedNodeNotFoundFault)
    (\case
        ReservedNodeNotFoundFault -> Right ()
        x -> Left x)

-- | Specified offering does not exist.
--
-- See: 'ReservedNodeOfferingNotFoundFault'
_ReservedNodeOfferingNotFoundFault :: Prism' RedshiftError ()
_ReservedNodeOfferingNotFoundFault = prism
    (const ReservedNodeOfferingNotFoundFault)
    (\case
        ReservedNodeOfferingNotFoundFault -> Right ()
        x -> Left x)

-- | Request would exceed the user's compute node quota. For information about
-- increasing your quota, go to Limits in Amazon Redshift in the Amazon
-- Redshift Management Guide.
--
-- See: 'ReservedNodeQuotaExceededFault'
_ReservedNodeQuotaExceededFault :: Prism' RedshiftError ()
_ReservedNodeQuotaExceededFault = prism
    (const ReservedNodeQuotaExceededFault)
    (\case
        ReservedNodeQuotaExceededFault -> Right ()
        x -> Left x)

-- | A resize operation for the specified cluster is not found.
--
-- See: 'ResizeNotFoundFault'
_ResizeNotFoundFault :: Prism' RedshiftError ()
_ResizeNotFoundFault = prism
    (const ResizeNotFoundFault)
    (\case
        ResizeNotFoundFault -> Right ()
        x -> Left x)

-- | Amazon SNS has responded that there is a problem with the specified Amazon
-- SNS topic.
--
-- See: 'SNSInvalidTopicFault'
_SNSInvalidTopicFault :: Prism' RedshiftError ()
_SNSInvalidTopicFault = prism
    (const SNSInvalidTopicFault)
    (\case
        SNSInvalidTopicFault -> Right ()
        x -> Left x)

-- | You do not have permission to publish to the specified Amazon SNS topic.
--
-- See: 'SNSNoAuthorizationFault'
_SNSNoAuthorizationFault :: Prism' RedshiftError ()
_SNSNoAuthorizationFault = prism
    (const SNSNoAuthorizationFault)
    (\case
        SNSNoAuthorizationFault -> Right ()
        x -> Left x)

-- | An Amazon SNS topic with the specified Amazon Resource Name (ARN) does not
-- exist.
--
-- See: 'SNSTopicArnNotFoundFault'
_SNSTopicArnNotFoundFault :: Prism' RedshiftError ()
_SNSTopicArnNotFoundFault = prism
    (const SNSTopicArnNotFoundFault)
    (\case
        SNSTopicArnNotFoundFault -> Right ()
        x -> Left x)

-- | The cluster already has cross-region snapshot copy disabled.
--
-- See: 'SnapshotCopyAlreadyDisabledFault'
_SnapshotCopyAlreadyDisabledFault :: Prism' RedshiftError ()
_SnapshotCopyAlreadyDisabledFault = prism
    (const SnapshotCopyAlreadyDisabledFault)
    (\case
        SnapshotCopyAlreadyDisabledFault -> Right ()
        x -> Left x)

-- | The cluster already has cross-region snapshot copy enabled.
--
-- See: 'SnapshotCopyAlreadyEnabledFault'
_SnapshotCopyAlreadyEnabledFault :: Prism' RedshiftError ()
_SnapshotCopyAlreadyEnabledFault = prism
    (const SnapshotCopyAlreadyEnabledFault)
    (\case
        SnapshotCopyAlreadyEnabledFault -> Right ()
        x -> Left x)

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
--
-- See: 'SnapshotCopyDisabledFault'
_SnapshotCopyDisabledFault :: Prism' RedshiftError ()
_SnapshotCopyDisabledFault = prism
    (const SnapshotCopyDisabledFault)
    (\case
        SnapshotCopyDisabledFault -> Right ()
        x -> Left x)

-- | The specified Amazon Redshift event source could not be found.
--
-- See: 'SourceNotFoundFault'
_SourceNotFoundFault :: Prism' RedshiftError ()
_SourceNotFoundFault = prism
    (const SourceNotFoundFault)
    (\case
        SourceNotFoundFault -> Right ()
        x -> Left x)

-- | A specified subnet is already in use by another cluster.
--
-- See: 'SubnetAlreadyInUse'
_SubnetAlreadyInUse :: Prism' RedshiftError ()
_SubnetAlreadyInUse = prism
    (const SubnetAlreadyInUse)
    (\case
        SubnetAlreadyInUse -> Right ()
        x -> Left x)

-- | There is already an existing event notification subscription with the
-- specified name.
--
-- See: 'SubscriptionAlreadyExistFault'
_SubscriptionAlreadyExistFault :: Prism' RedshiftError ()
_SubscriptionAlreadyExistFault = prism
    (const SubscriptionAlreadyExistFault)
    (\case
        SubscriptionAlreadyExistFault -> Right ()
        x -> Left x)

-- | The value specified for the event category was not one of the allowed
-- values, or it specified a category that does not apply to the specified
-- source type. The allowed values are Configuration, Management, Monitoring,
-- and Security.
--
-- See: 'SubscriptionCategoryNotFoundFault'
_SubscriptionCategoryNotFoundFault :: Prism' RedshiftError ()
_SubscriptionCategoryNotFoundFault = prism
    (const SubscriptionCategoryNotFoundFault)
    (\case
        SubscriptionCategoryNotFoundFault -> Right ()
        x -> Left x)

-- | An Amazon Redshift event with the specified event ID does not exist.
--
-- See: 'SubscriptionEventIdNotFoundFault'
_SubscriptionEventIdNotFoundFault :: Prism' RedshiftError ()
_SubscriptionEventIdNotFoundFault = prism
    (const SubscriptionEventIdNotFoundFault)
    (\case
        SubscriptionEventIdNotFoundFault -> Right ()
        x -> Left x)

-- | An Amazon Redshift event notification subscription with the specified name
-- does not exist.
--
-- See: 'SubscriptionNotFoundFault'
_SubscriptionNotFoundFault :: Prism' RedshiftError ()
_SubscriptionNotFoundFault = prism
    (const SubscriptionNotFoundFault)
    (\case
        SubscriptionNotFoundFault -> Right ()
        x -> Left x)

-- | The value specified for the event severity was not one of the allowed
-- values, or it specified a severity that does not apply to the specified
-- source type. The allowed values are ERROR and INFO.
--
-- See: 'SubscriptionSeverityNotFoundFault'
_SubscriptionSeverityNotFoundFault :: Prism' RedshiftError ()
_SubscriptionSeverityNotFoundFault = prism
    (const SubscriptionSeverityNotFoundFault)
    (\case
        SubscriptionSeverityNotFoundFault -> Right ()
        x -> Left x)

-- | Your account is not authorized to perform the requested operation.
--
-- See: 'UnauthorizedOperation'
_UnauthorizedOperation :: Prism' RedshiftError ()
_UnauthorizedOperation = prism
    (const UnauthorizedOperation)
    (\case
        UnauthorizedOperation -> Right ()
        x -> Left x)

-- | The specified region is incorrect or does not exist.
--
-- See: 'UnknownSnapshotCopyRegionFault'
_UnknownSnapshotCopyRegionFault :: Prism' RedshiftError ()
_UnknownSnapshotCopyRegionFault = prism
    (const UnknownSnapshotCopyRegionFault)
    (\case
        UnknownSnapshotCopyRegionFault -> Right ()
        x -> Left x)

-- | A request option was specified that is not supported.
--
-- See: 'UnsupportedOptionFault'
_UnsupportedOptionFault :: Prism' RedshiftError ()
_UnsupportedOptionFault = prism
    (const UnsupportedOptionFault)
    (\case
        UnsupportedOptionFault -> Right ()
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

data SourceType
    = SourceTypeCluster -- ^ cluster
    | SourceTypeClusterParameterGroup -- ^ cluster-parameter-group
    | SourceTypeClusterSecurityGroup -- ^ cluster-security-group
    | SourceTypeClusterSnapshot -- ^ cluster-snapshot
      deriving (Eq, Ord, Show, Generic)

instance Hashable SourceType

instance FromText SourceType where
    parser = match "cluster" SourceTypeCluster
         <|> match "cluster-parameter-group" SourceTypeClusterParameterGroup
         <|> match "cluster-security-group" SourceTypeClusterSecurityGroup
         <|> match "cluster-snapshot" SourceTypeClusterSnapshot

instance ToText SourceType where
    toText SourceTypeCluster = "cluster"
    toText SourceTypeClusterParameterGroup = "cluster-parameter-group"
    toText SourceTypeClusterSecurityGroup = "cluster-security-group"
    toText SourceTypeClusterSnapshot = "cluster-snapshot"

instance ToByteString SourceType

instance FromXML SourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SourceType"

instance ToQuery SourceType where
    toQuery = genericQuery def

-- | Describes an AWS customer account authorized to restore a snapshot.
newtype AccountWithRestoreAccess = AccountWithRestoreAccess
    { _awraAccountId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccountWithRestoreAccess' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AccountId ::@ @Maybe Text@
--
accountWithRestoreAccess :: AccountWithRestoreAccess
accountWithRestoreAccess = AccountWithRestoreAccess
    { _awraAccountId = Nothing
    }

-- | The identifier of an AWS customer account authorized to restore a snapshot.
awraAccountId :: Lens' AccountWithRestoreAccess (Maybe Text)
awraAccountId = lens _awraAccountId (\s a -> s { _awraAccountId = a })

instance FromXML AccountWithRestoreAccess where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccountWithRestoreAccess"

instance ToQuery AccountWithRestoreAccess where
    toQuery = genericQuery def

-- | Describes an availability zone.
newtype AvailabilityZone = AvailabilityZone
    { _azName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityZone' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
availabilityZone :: AvailabilityZone
availabilityZone = AvailabilityZone
    { _azName = Nothing
    }

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\s a -> s { _azName = a })

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone where
    toQuery = genericQuery def

-- | Describes a cluster.
data Cluster = Cluster
    { _cClusterIdentifier :: Maybe Text
    , _cNodeType :: Maybe Text
    , _cClusterStatus :: Maybe Text
    , _cModifyStatus :: Maybe Text
    , _cMasterUsername :: Maybe Text
    , _cDBName :: Maybe Text
    , _cEndpoint :: Maybe Endpoint'
    , _cClusterCreateTime :: Maybe ISO8601
    , _cAutomatedSnapshotRetentionPeriod :: Maybe Integer
    , _cClusterSecurityGroups :: [ClusterSecurityGroupMembership]
    , _cVpcSecurityGroups :: [VpcSecurityGroupMembership]
    , _cClusterParameterGroups :: [ClusterParameterGroupStatus]
    , _cClusterSubnetGroupName :: Maybe Text
    , _cVpcId :: Maybe Text
    , _cAvailabilityZone :: Maybe Text
    , _cPreferredMaintenanceWindow :: Maybe Text
    , _cPendingModifiedValues :: Maybe PendingModifiedValues
    , _cClusterVersion :: Maybe Text
    , _cAllowVersionUpgrade :: Maybe Bool
    , _cNumberOfNodes :: Maybe Integer
    , _cPubliclyAccessible :: Maybe Bool
    , _cEncrypted :: Maybe Bool
    , _cRestoreStatus :: Maybe RestoreStatus
    , _cHsmStatus :: Maybe HsmStatus
    , _cClusterSnapshotCopyStatus :: Maybe ClusterSnapshotCopyStatus
    , _cClusterPublicKey :: Maybe Text
    , _cClusterNodes :: [ClusterNode]
    , _cElasticIpStatus :: Maybe ElasticIpStatus
    , _cClusterRevisionNumber :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Cluster' data type.
--
-- 'Cluster' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterIdentifier ::@ @Maybe Text@
--
-- * @NodeType ::@ @Maybe Text@
--
-- * @ClusterStatus ::@ @Maybe Text@
--
-- * @ModifyStatus ::@ @Maybe Text@
--
-- * @MasterUsername ::@ @Maybe Text@
--
-- * @DBName ::@ @Maybe Text@
--
-- * @Endpoint ::@ @Maybe Endpoint'@
--
-- * @ClusterCreateTime ::@ @Maybe ISO8601@
--
-- * @AutomatedSnapshotRetentionPeriod ::@ @Maybe Integer@
--
-- * @ClusterSecurityGroups ::@ @[ClusterSecurityGroupMembership]@
--
-- * @VpcSecurityGroups ::@ @[VpcSecurityGroupMembership]@
--
-- * @ClusterParameterGroups ::@ @[ClusterParameterGroupStatus]@
--
-- * @ClusterSubnetGroupName ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @PreferredMaintenanceWindow ::@ @Maybe Text@
--
-- * @PendingModifiedValues ::@ @Maybe PendingModifiedValues@
--
-- * @ClusterVersion ::@ @Maybe Text@
--
-- * @AllowVersionUpgrade ::@ @Maybe Bool@
--
-- * @NumberOfNodes ::@ @Maybe Integer@
--
-- * @PubliclyAccessible ::@ @Maybe Bool@
--
-- * @Encrypted ::@ @Maybe Bool@
--
-- * @RestoreStatus ::@ @Maybe RestoreStatus@
--
-- * @HsmStatus ::@ @Maybe HsmStatus@
--
-- * @ClusterSnapshotCopyStatus ::@ @Maybe ClusterSnapshotCopyStatus@
--
-- * @ClusterPublicKey ::@ @Maybe Text@
--
-- * @ClusterNodes ::@ @[ClusterNode]@
--
-- * @ElasticIpStatus ::@ @Maybe ElasticIpStatus@
--
-- * @ClusterRevisionNumber ::@ @Maybe Text@
--
cluster :: Cluster
cluster = Cluster
    { _cClusterIdentifier = Nothing
    , _cNodeType = Nothing
    , _cClusterStatus = Nothing
    , _cModifyStatus = Nothing
    , _cMasterUsername = Nothing
    , _cDBName = Nothing
    , _cEndpoint = Nothing
    , _cClusterCreateTime = Nothing
    , _cAutomatedSnapshotRetentionPeriod = Nothing
    , _cClusterSecurityGroups = mempty
    , _cVpcSecurityGroups = mempty
    , _cClusterParameterGroups = mempty
    , _cClusterSubnetGroupName = Nothing
    , _cVpcId = Nothing
    , _cAvailabilityZone = Nothing
    , _cPreferredMaintenanceWindow = Nothing
    , _cPendingModifiedValues = Nothing
    , _cClusterVersion = Nothing
    , _cAllowVersionUpgrade = Nothing
    , _cNumberOfNodes = Nothing
    , _cPubliclyAccessible = Nothing
    , _cEncrypted = Nothing
    , _cRestoreStatus = Nothing
    , _cHsmStatus = Nothing
    , _cClusterSnapshotCopyStatus = Nothing
    , _cClusterPublicKey = Nothing
    , _cClusterNodes = mempty
    , _cElasticIpStatus = Nothing
    , _cClusterRevisionNumber = Nothing
    }

-- | The unique identifier of the cluster.
cClusterIdentifier :: Lens' Cluster (Maybe Text)
cClusterIdentifier =
    lens _cClusterIdentifier (\s a -> s { _cClusterIdentifier = a })

-- | The node type for the nodes in the cluster.
cNodeType :: Lens' Cluster (Maybe Text)
cNodeType = lens _cNodeType (\s a -> s { _cNodeType = a })

-- | The current state of this cluster. Possible values include available,
-- creating, deleting, rebooting, renaming, and resizing.
cClusterStatus :: Lens' Cluster (Maybe Text)
cClusterStatus = lens _cClusterStatus (\s a -> s { _cClusterStatus = a })

-- | The status of a modify operation, if any, initiated for the cluster.
cModifyStatus :: Lens' Cluster (Maybe Text)
cModifyStatus = lens _cModifyStatus (\s a -> s { _cModifyStatus = a })

-- | The master user name for the cluster. This name is used to connect to the
-- database that is specified in DBName.
cMasterUsername :: Lens' Cluster (Maybe Text)
cMasterUsername = lens _cMasterUsername (\s a -> s { _cMasterUsername = a })

-- | The name of the initial database that was created when the cluster was
-- created. This same name is returned for the life of the cluster. If an
-- initial database was not specified, a database named "dev" was created by
-- default.
cDBName :: Lens' Cluster (Maybe Text)
cDBName = lens _cDBName (\s a -> s { _cDBName = a })

-- | The connection endpoint.
cEndpoint :: Lens' Cluster (Maybe Endpoint')
cEndpoint = lens _cEndpoint (\s a -> s { _cEndpoint = a })

-- | The date and time that the cluster was created.
cClusterCreateTime :: Lens' Cluster (Maybe ISO8601)
cClusterCreateTime =
    lens _cClusterCreateTime (\s a -> s { _cClusterCreateTime = a })

-- | The number of days that automatic cluster snapshots are retained.
cAutomatedSnapshotRetentionPeriod :: Lens' Cluster (Maybe Integer)
cAutomatedSnapshotRetentionPeriod =
    lens _cAutomatedSnapshotRetentionPeriod
         (\s a -> s { _cAutomatedSnapshotRetentionPeriod = a })

-- | A list of cluster security group that are associated with the cluster. Each
-- security group is represented by an element that contains
-- ClusterSecurityGroup.Name and ClusterSecurityGroup.Status subelements.
-- Cluster security groups are used when the cluster is not created in a VPC.
-- Clusters that are created in a VPC use VPC security groups, which are
-- listed by the VpcSecurityGroups parameter.
cClusterSecurityGroups :: Lens' Cluster [ClusterSecurityGroupMembership]
cClusterSecurityGroups =
    lens _cClusterSecurityGroups (\s a -> s { _cClusterSecurityGroups = a })

-- | A list of Virtual Private Cloud (VPC) security groups that are associated
-- with the cluster. This parameter is returned only if the cluster is in a
-- VPC.
cVpcSecurityGroups :: Lens' Cluster [VpcSecurityGroupMembership]
cVpcSecurityGroups =
    lens _cVpcSecurityGroups (\s a -> s { _cVpcSecurityGroups = a })

-- | The list of cluster parameter groups that are associated with this cluster.
cClusterParameterGroups :: Lens' Cluster [ClusterParameterGroupStatus]
cClusterParameterGroups =
    lens _cClusterParameterGroups
         (\s a -> s { _cClusterParameterGroups = a })

-- | The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
cClusterSubnetGroupName :: Lens' Cluster (Maybe Text)
cClusterSubnetGroupName =
    lens _cClusterSubnetGroupName
         (\s a -> s { _cClusterSubnetGroupName = a })

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
cVpcId :: Lens' Cluster (Maybe Text)
cVpcId = lens _cVpcId (\s a -> s { _cVpcId = a })

-- | The name of the Availability Zone in which the cluster is located.
cAvailabilityZone :: Lens' Cluster (Maybe Text)
cAvailabilityZone =
    lens _cAvailabilityZone (\s a -> s { _cAvailabilityZone = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
cPreferredMaintenanceWindow :: Lens' Cluster (Maybe Text)
cPreferredMaintenanceWindow =
    lens _cPreferredMaintenanceWindow
         (\s a -> s { _cPreferredMaintenanceWindow = a })

-- | If present, changes to the cluster are pending. Specific pending changes
-- are identified by subelements.
cPendingModifiedValues :: Lens' Cluster (Maybe PendingModifiedValues)
cPendingModifiedValues =
    lens _cPendingModifiedValues (\s a -> s { _cPendingModifiedValues = a })

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
cClusterVersion :: Lens' Cluster (Maybe Text)
cClusterVersion = lens _cClusterVersion (\s a -> s { _cClusterVersion = a })

-- | If true, version upgrades will be applied automatically to the cluster
-- during the maintenance window.
cAllowVersionUpgrade :: Lens' Cluster (Maybe Bool)
cAllowVersionUpgrade =
    lens _cAllowVersionUpgrade (\s a -> s { _cAllowVersionUpgrade = a })

-- | The number of compute nodes in the cluster.
cNumberOfNodes :: Lens' Cluster (Maybe Integer)
cNumberOfNodes = lens _cNumberOfNodes (\s a -> s { _cNumberOfNodes = a })

-- | If true, the cluster can be accessed from a public network.
cPubliclyAccessible :: Lens' Cluster (Maybe Bool)
cPubliclyAccessible =
    lens _cPubliclyAccessible (\s a -> s { _cPubliclyAccessible = a })

-- | If true, data in the cluster is encrypted at rest.
cEncrypted :: Lens' Cluster (Maybe Bool)
cEncrypted = lens _cEncrypted (\s a -> s { _cEncrypted = a })

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
cRestoreStatus :: Lens' Cluster (Maybe RestoreStatus)
cRestoreStatus = lens _cRestoreStatus (\s a -> s { _cRestoreStatus = a })

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
cHsmStatus :: Lens' Cluster (Maybe HsmStatus)
cHsmStatus = lens _cHsmStatus (\s a -> s { _cHsmStatus = a })

-- | Returns the destination region and retention period that are configured for
-- cross-region snapshot copy.
cClusterSnapshotCopyStatus :: Lens' Cluster (Maybe ClusterSnapshotCopyStatus)
cClusterSnapshotCopyStatus =
    lens _cClusterSnapshotCopyStatus
         (\s a -> s { _cClusterSnapshotCopyStatus = a })

-- | The public key for the cluster.
cClusterPublicKey :: Lens' Cluster (Maybe Text)
cClusterPublicKey =
    lens _cClusterPublicKey (\s a -> s { _cClusterPublicKey = a })

-- | The nodes in a cluster.
cClusterNodes :: Lens' Cluster [ClusterNode]
cClusterNodes = lens _cClusterNodes (\s a -> s { _cClusterNodes = a })

-- | Describes the status of the elastic IP (EIP) address.
cElasticIpStatus :: Lens' Cluster (Maybe ElasticIpStatus)
cElasticIpStatus =
    lens _cElasticIpStatus (\s a -> s { _cElasticIpStatus = a })

-- | The specific revision number of the database in the cluster.
cClusterRevisionNumber :: Lens' Cluster (Maybe Text)
cClusterRevisionNumber =
    lens _cClusterRevisionNumber (\s a -> s { _cClusterRevisionNumber = a })

instance FromXML Cluster where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Cluster"

-- | The identifier of a node in a cluster. --&gt;.
data ClusterNode = ClusterNode
    { _cnNodeRole :: Maybe Text
    , _cnPrivateIPAddress :: Maybe Text
    , _cnPublicIPAddress :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterNode' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NodeRole ::@ @Maybe Text@
--
-- * @PrivateIPAddress ::@ @Maybe Text@
--
-- * @PublicIPAddress ::@ @Maybe Text@
--
clusterNode :: ClusterNode
clusterNode = ClusterNode
    { _cnNodeRole = Nothing
    , _cnPrivateIPAddress = Nothing
    , _cnPublicIPAddress = Nothing
    }

-- | Whether the node is a leader node or a compute node.
cnNodeRole :: Lens' ClusterNode (Maybe Text)
cnNodeRole = lens _cnNodeRole (\s a -> s { _cnNodeRole = a })

-- | The private IP address of a node within a cluster.
cnPrivateIPAddress :: Lens' ClusterNode (Maybe Text)
cnPrivateIPAddress =
    lens _cnPrivateIPAddress (\s a -> s { _cnPrivateIPAddress = a })

-- | The public IP address of a node within a cluster.
cnPublicIPAddress :: Lens' ClusterNode (Maybe Text)
cnPublicIPAddress =
    lens _cnPublicIPAddress (\s a -> s { _cnPublicIPAddress = a })

instance FromXML ClusterNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterNode"

instance ToQuery ClusterNode where
    toQuery = genericQuery def

-- | Describes a parameter group.
data ClusterParameterGroup = ClusterParameterGroup
    { _cpgParameterGroupName :: Maybe Text
    , _cpgParameterGroupFamily :: Maybe Text
    , _cpgDescription :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterParameterGroup' data type.
--
-- 'ClusterParameterGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterGroupName ::@ @Maybe Text@
--
-- * @ParameterGroupFamily ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
clusterParameterGroup :: ClusterParameterGroup
clusterParameterGroup = ClusterParameterGroup
    { _cpgParameterGroupName = Nothing
    , _cpgParameterGroupFamily = Nothing
    , _cpgDescription = Nothing
    }

-- | The name of the cluster parameter group.
cpgParameterGroupName :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupName =
    lens _cpgParameterGroupName (\s a -> s { _cpgParameterGroupName = a })

-- | The name of the cluster parameter group family that this cluster parameter
-- group is compatible with.
cpgParameterGroupFamily :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupFamily =
    lens _cpgParameterGroupFamily
         (\s a -> s { _cpgParameterGroupFamily = a })

-- | The description of the parameter group.
cpgDescription :: Lens' ClusterParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\s a -> s { _cpgDescription = a })

instance FromXML ClusterParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterParameterGroup"

-- | Describes the status of a parameter group.
data ClusterParameterGroupStatus = ClusterParameterGroupStatus
    { _cpgsParameterGroupName :: Maybe Text
    , _cpgsParameterApplyStatus :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterParameterGroupStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterGroupName ::@ @Maybe Text@
--
-- * @ParameterApplyStatus ::@ @Maybe Text@
--
clusterParameterGroupStatus :: ClusterParameterGroupStatus
clusterParameterGroupStatus = ClusterParameterGroupStatus
    { _cpgsParameterGroupName = Nothing
    , _cpgsParameterApplyStatus = Nothing
    }

-- | The name of the cluster parameter group.
cpgsParameterGroupName :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterGroupName =
    lens _cpgsParameterGroupName (\s a -> s { _cpgsParameterGroupName = a })

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus =
    lens _cpgsParameterApplyStatus
         (\s a -> s { _cpgsParameterApplyStatus = a })

instance FromXML ClusterParameterGroupStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterParameterGroup"

instance ToQuery ClusterParameterGroupStatus where
    toQuery = genericQuery def

-- | Describes a security group.
data ClusterSecurityGroup = ClusterSecurityGroup
    { _csgClusterSecurityGroupName :: Maybe Text
    , _csgDescription :: Maybe Text
    , _csgEC2SecurityGroups :: [EC2SecurityGroup]
    , _csgIPRanges :: [IPRange]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterSecurityGroup' data type.
--
-- 'ClusterSecurityGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterSecurityGroupName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @EC2SecurityGroups ::@ @[EC2SecurityGroup]@
--
-- * @IPRanges ::@ @[IPRange]@
--
clusterSecurityGroup :: ClusterSecurityGroup
clusterSecurityGroup = ClusterSecurityGroup
    { _csgClusterSecurityGroupName = Nothing
    , _csgDescription = Nothing
    , _csgEC2SecurityGroups = mempty
    , _csgIPRanges = mempty
    }

-- | The name of the cluster security group to which the operation was applied.
csgClusterSecurityGroupName :: Lens' ClusterSecurityGroup (Maybe Text)
csgClusterSecurityGroupName =
    lens _csgClusterSecurityGroupName
         (\s a -> s { _csgClusterSecurityGroupName = a })

-- | A description of the security group.
csgDescription :: Lens' ClusterSecurityGroup (Maybe Text)
csgDescription = lens _csgDescription (\s a -> s { _csgDescription = a })

-- | A list of EC2 security groups that are permitted to access clusters
-- associated with this cluster security group.
csgEC2SecurityGroups :: Lens' ClusterSecurityGroup [EC2SecurityGroup]
csgEC2SecurityGroups =
    lens _csgEC2SecurityGroups (\s a -> s { _csgEC2SecurityGroups = a })

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters
-- associated with this cluster security group.
csgIPRanges :: Lens' ClusterSecurityGroup [IPRange]
csgIPRanges = lens _csgIPRanges (\s a -> s { _csgIPRanges = a })

instance FromXML ClusterSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSecurityGroup"

-- | Describes a security group.
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership
    { _csgmClusterSecurityGroupName :: Maybe Text
    , _csgmStatus :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterSecurityGroupMembership' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterSecurityGroupName ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
clusterSecurityGroupMembership :: ClusterSecurityGroupMembership
clusterSecurityGroupMembership = ClusterSecurityGroupMembership
    { _csgmClusterSecurityGroupName = Nothing
    , _csgmStatus = Nothing
    }

-- | The name of the cluster security group.
csgmClusterSecurityGroupName :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmClusterSecurityGroupName =
    lens _csgmClusterSecurityGroupName
         (\s a -> s { _csgmClusterSecurityGroupName = a })

-- | The status of the cluster security group.
csgmStatus :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\s a -> s { _csgmStatus = a })

instance FromXML ClusterSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSecurityGroup"

instance ToQuery ClusterSecurityGroupMembership where
    toQuery = genericQuery def

-- | Returns the destination region and retention period that are configured for
-- cross-region snapshot copy.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus
    { _cscsDestinationRegion :: Maybe Text
    , _cscsRetentionPeriod :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterSnapshotCopyStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DestinationRegion ::@ @Maybe Text@
--
-- * @RetentionPeriod ::@ @Maybe Integer@
--
clusterSnapshotCopyStatus :: ClusterSnapshotCopyStatus
clusterSnapshotCopyStatus = ClusterSnapshotCopyStatus
    { _cscsDestinationRegion = Nothing
    , _cscsRetentionPeriod = Nothing
    }

-- | The destination region that snapshots are automatically copied to when
-- cross-region snapshot copy is enabled.
cscsDestinationRegion :: Lens' ClusterSnapshotCopyStatus (Maybe Text)
cscsDestinationRegion =
    lens _cscsDestinationRegion (\s a -> s { _cscsDestinationRegion = a })

-- | The number of days that automated snapshots are retained in the destination
-- region after they are copied from a source region.
cscsRetentionPeriod :: Lens' ClusterSnapshotCopyStatus (Maybe Integer)
cscsRetentionPeriod =
    lens _cscsRetentionPeriod (\s a -> s { _cscsRetentionPeriod = a })

instance FromXML ClusterSnapshotCopyStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSnapshotCopyStatus"

instance ToQuery ClusterSnapshotCopyStatus where
    toQuery = genericQuery def

-- | Describes a subnet group.
data ClusterSubnetGroup = ClusterSubnetGroup
    { _csgrClusterSubnetGroupName :: Maybe Text
    , _csgrDescription :: Maybe Text
    , _csgrVpcId :: Maybe Text
    , _csgrSubnetGroupStatus :: Maybe Text
    , _csgrSubnets :: [Subnet]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterSubnetGroup' data type.
--
-- 'ClusterSubnetGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterSubnetGroupName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @SubnetGroupStatus ::@ @Maybe Text@
--
-- * @Subnets ::@ @[Subnet]@
--
clusterSubnetGroup :: ClusterSubnetGroup
clusterSubnetGroup = ClusterSubnetGroup
    { _csgrClusterSubnetGroupName = Nothing
    , _csgrDescription = Nothing
    , _csgrVpcId = Nothing
    , _csgrSubnetGroupStatus = Nothing
    , _csgrSubnets = mempty
    }

-- | The name of the cluster subnet group.
csgrClusterSubnetGroupName :: Lens' ClusterSubnetGroup (Maybe Text)
csgrClusterSubnetGroupName =
    lens _csgrClusterSubnetGroupName
         (\s a -> s { _csgrClusterSubnetGroupName = a })

-- | The description of the cluster subnet group.
csgrDescription :: Lens' ClusterSubnetGroup (Maybe Text)
csgrDescription = lens _csgrDescription (\s a -> s { _csgrDescription = a })

-- | The VPC ID of the cluster subnet group.
csgrVpcId :: Lens' ClusterSubnetGroup (Maybe Text)
csgrVpcId = lens _csgrVpcId (\s a -> s { _csgrVpcId = a })

-- | The status of the cluster subnet group. Possible values are Complete,
-- Incomplete and Invalid.
csgrSubnetGroupStatus :: Lens' ClusterSubnetGroup (Maybe Text)
csgrSubnetGroupStatus =
    lens _csgrSubnetGroupStatus (\s a -> s { _csgrSubnetGroupStatus = a })

-- | A list of the VPC Subnet elements.
csgrSubnets :: Lens' ClusterSubnetGroup [Subnet]
csgrSubnets = lens _csgrSubnets (\s a -> s { _csgrSubnets = a })

instance FromXML ClusterSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSubnetGroup"

-- | Describes a cluster version, including the parameter group family and
-- description of the version.
data ClusterVersion = ClusterVersion
    { _cvClusterVersion :: Maybe Text
    , _cvClusterParameterGroupFamily :: Maybe Text
    , _cvDescription :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterVersion' data type.
--
-- 'ClusterVersion' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterVersion ::@ @Maybe Text@
--
-- * @ClusterParameterGroupFamily ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
clusterVersion :: ClusterVersion
clusterVersion = ClusterVersion
    { _cvClusterVersion = Nothing
    , _cvClusterParameterGroupFamily = Nothing
    , _cvDescription = Nothing
    }

-- | The version number used by the cluster.
cvClusterVersion :: Lens' ClusterVersion (Maybe Text)
cvClusterVersion =
    lens _cvClusterVersion (\s a -> s { _cvClusterVersion = a })

-- | The name of the cluster parameter group family for the cluster.
cvClusterParameterGroupFamily :: Lens' ClusterVersion (Maybe Text)
cvClusterParameterGroupFamily =
    lens _cvClusterParameterGroupFamily
         (\s a -> s { _cvClusterParameterGroupFamily = a })

-- | The description of the cluster version.
cvDescription :: Lens' ClusterVersion (Maybe Text)
cvDescription = lens _cvDescription (\s a -> s { _cvDescription = a })

instance FromXML ClusterVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterVersion"

-- | Describes the default cluster parameters for a parameter group family.
data DefaultClusterParameters = DefaultClusterParameters
    { _dcp1ParameterGroupFamily :: Maybe Text
    , _dcp1Marker :: Maybe Text
    , _dcp1Parameters :: [Parameter]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DefaultClusterParameters' data type.
--
-- 'DefaultClusterParameters' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterGroupFamily ::@ @Maybe Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @Parameters ::@ @[Parameter]@
--
defaultClusterParameters :: DefaultClusterParameters
defaultClusterParameters = DefaultClusterParameters
    { _dcp1ParameterGroupFamily = Nothing
    , _dcp1Marker = Nothing
    , _dcp1Parameters = mempty
    }

-- | The name of the cluster parameter group family to which the engine default
-- parameters apply.
dcp1ParameterGroupFamily :: Lens' DefaultClusterParameters (Maybe Text)
dcp1ParameterGroupFamily =
    lens _dcp1ParameterGroupFamily
         (\s a -> s { _dcp1ParameterGroupFamily = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dcp1Marker :: Lens' DefaultClusterParameters (Maybe Text)
dcp1Marker = lens _dcp1Marker (\s a -> s { _dcp1Marker = a })

-- | The list of cluster default parameters.
dcp1Parameters :: Lens' DefaultClusterParameters [Parameter]
dcp1Parameters = lens _dcp1Parameters (\s a -> s { _dcp1Parameters = a })

instance FromXML DefaultClusterParameters where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DefaultClusterParameters"

-- | Describes an Amazon EC2 security group.
data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus :: Maybe Text
    , _ecsgEC2SecurityGroupName :: Maybe Text
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EC2SecurityGroup' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @Maybe Text@
--
-- * @EC2SecurityGroupName ::@ @Maybe Text@
--
-- * @EC2SecurityGroupOwnerId ::@ @Maybe Text@
--
eC2SecurityGroup :: EC2SecurityGroup
eC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus = Nothing
    , _ecsgEC2SecurityGroupName = Nothing
    , _ecsgEC2SecurityGroupOwnerId = Nothing
    }

-- | The status of the EC2 security group.
ecsgStatus :: Lens' EC2SecurityGroup (Maybe Text)
ecsgStatus = lens _ecsgStatus (\s a -> s { _ecsgStatus = a })

-- | The name of the EC2 Security Group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName =
    lens _ecsgEC2SecurityGroupName
         (\s a -> s { _ecsgEC2SecurityGroupName = a })

-- | The AWS ID of the owner of the EC2 security group specified in the
-- EC2SecurityGroupName field.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId =
    lens _ecsgEC2SecurityGroupOwnerId
         (\s a -> s { _ecsgEC2SecurityGroupOwnerId = a })

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup where
    toQuery = genericQuery def

-- | Describes the status of the elastic IP (EIP) address.
data ElasticIpStatus = ElasticIpStatus
    { _eisElasticIp :: Maybe Text
    , _eisStatus :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ElasticIpStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ElasticIp ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
elasticIpStatus :: ElasticIpStatus
elasticIpStatus = ElasticIpStatus
    { _eisElasticIp = Nothing
    , _eisStatus = Nothing
    }

-- | The elastic IP (EIP) address for the cluster.
eisElasticIp :: Lens' ElasticIpStatus (Maybe Text)
eisElasticIp = lens _eisElasticIp (\s a -> s { _eisElasticIp = a })

-- | Describes the status of the elastic IP (EIP) address.
eisStatus :: Lens' ElasticIpStatus (Maybe Text)
eisStatus = lens _eisStatus (\s a -> s { _eisStatus = a })

instance FromXML ElasticIpStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ElasticIpStatus"

instance ToQuery ElasticIpStatus where
    toQuery = genericQuery def

-- | The connection endpoint.
data Endpoint' = Endpoint'
    { _eAddress :: Maybe Text
    , _ePort :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Endpoint'' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Address ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
endpoint' :: Endpoint'
endpoint' = Endpoint'
    { _eAddress = Nothing
    , _ePort = Nothing
    }

-- | The DNS address of the Cluster.
eAddress :: Lens' Endpoint' (Maybe Text)
eAddress = lens _eAddress (\s a -> s { _eAddress = a })

-- | The port that the database engine is listening on.
ePort :: Lens' Endpoint' (Maybe Integer)
ePort = lens _ePort (\s a -> s { _ePort = a })

instance FromXML Endpoint' where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint' where
    toQuery = genericQuery def

-- | Describes an event.
data Event = Event
    { _erSourceIdentifier :: Maybe Text
    , _erSourceType :: Maybe SourceType
    , _erMessage :: Maybe Text
    , _erEventCategories :: [Text]
    , _erSeverity :: Maybe Text
    , _erDate :: Maybe ISO8601
    , _erEventId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Event' data type.
--
-- 'Event' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceIdentifier ::@ @Maybe Text@
--
-- * @SourceType ::@ @Maybe SourceType@
--
-- * @Message ::@ @Maybe Text@
--
-- * @EventCategories ::@ @[Text]@
--
-- * @Severity ::@ @Maybe Text@
--
-- * @Date ::@ @Maybe ISO8601@
--
-- * @EventId ::@ @Maybe Text@
--
event :: Event
event = Event
    { _erSourceIdentifier = Nothing
    , _erSourceType = Nothing
    , _erMessage = Nothing
    , _erEventCategories = mempty
    , _erSeverity = Nothing
    , _erDate = Nothing
    , _erEventId = Nothing
    }

-- | The identifier for the source of the event.
erSourceIdentifier :: Lens' Event (Maybe Text)
erSourceIdentifier =
    lens _erSourceIdentifier (\s a -> s { _erSourceIdentifier = a })

-- | The source type for this event.
erSourceType :: Lens' Event (Maybe SourceType)
erSourceType = lens _erSourceType (\s a -> s { _erSourceType = a })

-- | The text of this event.
erMessage :: Lens' Event (Maybe Text)
erMessage = lens _erMessage (\s a -> s { _erMessage = a })

-- | A list of the event categories.
erEventCategories :: Lens' Event [Text]
erEventCategories =
    lens _erEventCategories (\s a -> s { _erEventCategories = a })

-- | The severity of the event. Values: ERROR, INFO.
erSeverity :: Lens' Event (Maybe Text)
erSeverity = lens _erSeverity (\s a -> s { _erSeverity = a })

-- | The date and time of the event.
erDate :: Lens' Event (Maybe ISO8601)
erDate = lens _erDate (\s a -> s { _erDate = a })

-- | The identifier of the event.
erEventId :: Lens' Event (Maybe Text)
erEventId = lens _erEventId (\s a -> s { _erEventId = a })

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

data EventCategoriesMap = EventCategoriesMap
    { _ecmSourceType :: Maybe Text
    , _ecmEvents :: [EventInfoMap]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EventCategoriesMap' data type.
--
-- 'EventCategoriesMap' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceType ::@ @Maybe Text@
--
-- * @Events ::@ @[EventInfoMap]@
--
eventCategoriesMap :: EventCategoriesMap
eventCategoriesMap = EventCategoriesMap
    { _ecmSourceType = Nothing
    , _ecmEvents = mempty
    }

-- | The Amazon Redshift source type, such as cluster or cluster-snapshot, that
-- the returned categories belong to.
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\s a -> s { _ecmSourceType = a })

-- | The events in the event category.
ecmEvents :: Lens' EventCategoriesMap [EventInfoMap]
ecmEvents = lens _ecmEvents (\s a -> s { _ecmEvents = a })

instance FromXML EventCategoriesMap where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventCategoriesMap"

data EventInfoMap = EventInfoMap
    { _eimEventId :: Maybe Text
    , _eimEventCategories :: [Text]
    , _eimEventDescription :: Maybe Text
    , _eimSeverity :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EventInfoMap' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EventId ::@ @Maybe Text@
--
-- * @EventCategories ::@ @[Text]@
--
-- * @EventDescription ::@ @Maybe Text@
--
-- * @Severity ::@ @Maybe Text@
--
eventInfoMap :: EventInfoMap
eventInfoMap = EventInfoMap
    { _eimEventId = Nothing
    , _eimEventCategories = mempty
    , _eimEventDescription = Nothing
    , _eimSeverity = Nothing
    }

-- | The identifier of an Amazon Redshift event.
eimEventId :: Lens' EventInfoMap (Maybe Text)
eimEventId = lens _eimEventId (\s a -> s { _eimEventId = a })

-- | The category of an Amazon Redshift event.
eimEventCategories :: Lens' EventInfoMap [Text]
eimEventCategories =
    lens _eimEventCategories (\s a -> s { _eimEventCategories = a })

-- | The description of an Amazon Redshift event.
eimEventDescription :: Lens' EventInfoMap (Maybe Text)
eimEventDescription =
    lens _eimEventDescription (\s a -> s { _eimEventDescription = a })

-- | The severity of the event. Values: ERROR, INFO.
eimSeverity :: Lens' EventInfoMap (Maybe Text)
eimSeverity = lens _eimSeverity (\s a -> s { _eimSeverity = a })

instance FromXML EventInfoMap where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventInfoMap"

instance ToQuery EventInfoMap where
    toQuery = genericQuery def

data EventSubscription = EventSubscription
    { _esCustomerAwsId :: Maybe Text
    , _esCustSubscriptionId :: Maybe Text
    , _esSnsTopicArn :: Maybe Text
    , _esStatus :: Maybe Text
    , _esSubscriptionCreationTime :: Maybe ISO8601
    , _esSourceType :: Maybe Text
    , _esSourceIdsList :: [Text]
    , _esEventCategoriesList :: [Text]
    , _esSeverity :: Maybe Text
    , _esEnabled :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EventSubscription' data type.
--
-- 'EventSubscription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CustomerAwsId ::@ @Maybe Text@
--
-- * @CustSubscriptionId ::@ @Maybe Text@
--
-- * @SnsTopicArn ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
-- * @SubscriptionCreationTime ::@ @Maybe ISO8601@
--
-- * @SourceType ::@ @Maybe Text@
--
-- * @SourceIdsList ::@ @[Text]@
--
-- * @EventCategoriesList ::@ @[Text]@
--
-- * @Severity ::@ @Maybe Text@
--
-- * @Enabled ::@ @Maybe Bool@
--
eventSubscription :: EventSubscription
eventSubscription = EventSubscription
    { _esCustomerAwsId = Nothing
    , _esCustSubscriptionId = Nothing
    , _esSnsTopicArn = Nothing
    , _esStatus = Nothing
    , _esSubscriptionCreationTime = Nothing
    , _esSourceType = Nothing
    , _esSourceIdsList = mempty
    , _esEventCategoriesList = mempty
    , _esSeverity = Nothing
    , _esEnabled = Nothing
    }

-- | The AWS customer account associated with the Amazon Redshift event
-- notification subscription.
esCustomerAwsId :: Lens' EventSubscription (Maybe Text)
esCustomerAwsId = lens _esCustomerAwsId (\s a -> s { _esCustomerAwsId = a })

-- | The name of the Amazon Redshift event notification subscription.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId =
    lens _esCustSubscriptionId (\s a -> s { _esCustSubscriptionId = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
esSnsTopicArn :: Lens' EventSubscription (Maybe Text)
esSnsTopicArn = lens _esSnsTopicArn (\s a -> s { _esSnsTopicArn = a })

-- | The status of the Amazon Redshift event notification subscription.
-- Constraints: Can be one of the following: active | no-permission |
-- topic-not-exist The status "no-permission" indicates that Amazon Redshift
-- no longer has permission to post to the Amazon SNS topic. The status
-- "topic-not-exist" indicates that the topic was deleted after the
-- subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\s a -> s { _esStatus = a })

-- | The date and time the Amazon Redshift event notification subscription was
-- created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe ISO8601)
esSubscriptionCreationTime =
    lens _esSubscriptionCreationTime
         (\s a -> s { _esSubscriptionCreationTime = a })

-- | The source type of the events returned the Amazon Redshift event
-- notification, such as cluster, or cluster-snapshot.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\s a -> s { _esSourceType = a })

-- | A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
esSourceIdsList :: Lens' EventSubscription [Text]
esSourceIdsList = lens _esSourceIdsList (\s a -> s { _esSourceIdsList = a })

-- | The list of Amazon Redshift event categories specified in the event
-- notification subscription. Values: Configuration, Management, Monitoring,
-- Security.
esEventCategoriesList :: Lens' EventSubscription [Text]
esEventCategoriesList =
    lens _esEventCategoriesList (\s a -> s { _esEventCategoriesList = a })

-- | The event severity specified in the Amazon Redshift event notification
-- subscription. Values: ERROR, INFO.
esSeverity :: Lens' EventSubscription (Maybe Text)
esSeverity = lens _esSeverity (\s a -> s { _esSeverity = a })

-- | A Boolean value indicating whether the subscription is enabled. true
-- indicates the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\s a -> s { _esEnabled = a })

instance FromXML EventSubscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventSubscription"

-- | Returns information about an HSM client certificate. The certificate is
-- stored in a secure Hardware Storage Module (HSM), and used by the Amazon
-- Redshift cluster to encrypt data files.
data HsmClientCertificate = HsmClientCertificate
    { _hccHsmClientCertificateIdentifier :: Maybe Text
    , _hccHsmClientCertificatePublicKey :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HsmClientCertificate' data type.
--
-- 'HsmClientCertificate' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HsmClientCertificateIdentifier ::@ @Maybe Text@
--
-- * @HsmClientCertificatePublicKey ::@ @Maybe Text@
--
hsmClientCertificate :: HsmClientCertificate
hsmClientCertificate = HsmClientCertificate
    { _hccHsmClientCertificateIdentifier = Nothing
    , _hccHsmClientCertificatePublicKey = Nothing
    }

-- | The identifier of the HSM client certificate.
hccHsmClientCertificateIdentifier :: Lens' HsmClientCertificate (Maybe Text)
hccHsmClientCertificateIdentifier =
    lens _hccHsmClientCertificateIdentifier
         (\s a -> s { _hccHsmClientCertificateIdentifier = a })

-- | The public key that the Amazon Redshift cluster will use to connect to the
-- HSM. You must register the public key in the HSM.
hccHsmClientCertificatePublicKey :: Lens' HsmClientCertificate (Maybe Text)
hccHsmClientCertificatePublicKey =
    lens _hccHsmClientCertificatePublicKey
         (\s a -> s { _hccHsmClientCertificatePublicKey = a })

instance FromXML HsmClientCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HsmClientCertificate"

-- | Returns information about an HSM configuration, which is an object that
-- describes to Amazon Redshift clusters the information they require to
-- connect to an HSM where they can store database encryption keys.
data HsmConfiguration = HsmConfiguration
    { _hcHsmConfigurationIdentifier :: Maybe Text
    , _hcDescription :: Maybe Text
    , _hcHsmIpAddress :: Maybe Text
    , _hcHsmPartitionName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HsmConfiguration' data type.
--
-- 'HsmConfiguration' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HsmConfigurationIdentifier ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @HsmIpAddress ::@ @Maybe Text@
--
-- * @HsmPartitionName ::@ @Maybe Text@
--
hsmConfiguration :: HsmConfiguration
hsmConfiguration = HsmConfiguration
    { _hcHsmConfigurationIdentifier = Nothing
    , _hcDescription = Nothing
    , _hcHsmIpAddress = Nothing
    , _hcHsmPartitionName = Nothing
    }

-- | The name of the Amazon Redshift HSM configuration.
hcHsmConfigurationIdentifier :: Lens' HsmConfiguration (Maybe Text)
hcHsmConfigurationIdentifier =
    lens _hcHsmConfigurationIdentifier
         (\s a -> s { _hcHsmConfigurationIdentifier = a })

-- | A text description of the HSM configuration.
hcDescription :: Lens' HsmConfiguration (Maybe Text)
hcDescription = lens _hcDescription (\s a -> s { _hcDescription = a })

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
hcHsmIpAddress :: Lens' HsmConfiguration (Maybe Text)
hcHsmIpAddress = lens _hcHsmIpAddress (\s a -> s { _hcHsmIpAddress = a })

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
hcHsmPartitionName :: Lens' HsmConfiguration (Maybe Text)
hcHsmPartitionName =
    lens _hcHsmPartitionName (\s a -> s { _hcHsmPartitionName = a })

instance FromXML HsmConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HsmConfiguration"

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
data HsmStatus = HsmStatus
    { _hsHsmClientCertificateIdentifier :: Maybe Text
    , _hsHsmConfigurationIdentifier :: Maybe Text
    , _hsStatus :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HsmStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HsmClientCertificateIdentifier ::@ @Maybe Text@
--
-- * @HsmConfigurationIdentifier ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
hsmStatus :: HsmStatus
hsmStatus = HsmStatus
    { _hsHsmClientCertificateIdentifier = Nothing
    , _hsHsmConfigurationIdentifier = Nothing
    , _hsStatus = Nothing
    }

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
hsHsmClientCertificateIdentifier :: Lens' HsmStatus (Maybe Text)
hsHsmClientCertificateIdentifier =
    lens _hsHsmClientCertificateIdentifier
         (\s a -> s { _hsHsmClientCertificateIdentifier = a })

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
hsHsmConfigurationIdentifier :: Lens' HsmStatus (Maybe Text)
hsHsmConfigurationIdentifier =
    lens _hsHsmConfigurationIdentifier
         (\s a -> s { _hsHsmConfigurationIdentifier = a })

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
hsStatus :: Lens' HsmStatus (Maybe Text)
hsStatus = lens _hsStatus (\s a -> s { _hsStatus = a })

instance FromXML HsmStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HsmStatus"

instance ToQuery HsmStatus where
    toQuery = genericQuery def

-- | Describes an IP range used in a security group.
data IPRange = IPRange
    { _iprStatus :: Maybe Text
    , _iprCIDRIP :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IPRange' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @Maybe Text@
--
-- * @CIDRIP ::@ @Maybe Text@
--
iPRange :: IPRange
iPRange = IPRange
    { _iprStatus = Nothing
    , _iprCIDRIP = Nothing
    }

-- | The status of the IP range, for example, "authorized".
iprStatus :: Lens' IPRange (Maybe Text)
iprStatus = lens _iprStatus (\s a -> s { _iprStatus = a })

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
iprCIDRIP :: Lens' IPRange (Maybe Text)
iprCIDRIP = lens _iprCIDRIP (\s a -> s { _iprCIDRIP = a })

instance FromXML IPRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IPRange"

instance ToQuery IPRange where
    toQuery = genericQuery def

-- | Describes an orderable cluster option.
data OrderableClusterOption = OrderableClusterOption
    { _ocoClusterVersion :: Maybe Text
    , _ocoClusterType :: Maybe Text
    , _ocoNodeType :: Maybe Text
    , _ocoAvailabilityZones :: [AvailabilityZone]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OrderableClusterOption' data type.
--
-- 'OrderableClusterOption' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterVersion ::@ @Maybe Text@
--
-- * @ClusterType ::@ @Maybe Text@
--
-- * @NodeType ::@ @Maybe Text@
--
-- * @AvailabilityZones ::@ @[AvailabilityZone]@
--
orderableClusterOption :: OrderableClusterOption
orderableClusterOption = OrderableClusterOption
    { _ocoClusterVersion = Nothing
    , _ocoClusterType = Nothing
    , _ocoNodeType = Nothing
    , _ocoAvailabilityZones = mempty
    }

-- | The version of the orderable cluster.
ocoClusterVersion :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterVersion =
    lens _ocoClusterVersion (\s a -> s { _ocoClusterVersion = a })

-- | The cluster type, for example multi-node.
ocoClusterType :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterType = lens _ocoClusterType (\s a -> s { _ocoClusterType = a })

-- | The node type for the orderable cluster.
ocoNodeType :: Lens' OrderableClusterOption (Maybe Text)
ocoNodeType = lens _ocoNodeType (\s a -> s { _ocoNodeType = a })

-- | A list of availability zones for the orderable cluster.
ocoAvailabilityZones :: Lens' OrderableClusterOption [AvailabilityZone]
ocoAvailabilityZones =
    lens _ocoAvailabilityZones (\s a -> s { _ocoAvailabilityZones = a })

instance FromXML OrderableClusterOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OrderableClusterOption"

-- | Describes a parameter in a cluster parameter group.
data Parameter = Parameter
    { _pParameterName :: Maybe Text
    , _pParameterValue :: Maybe Text
    , _pDescription :: Maybe Text
    , _pSource :: Maybe Text
    , _pDataType :: Maybe Text
    , _pAllowedValues :: Maybe Text
    , _pIsModifiable :: Maybe Bool
    , _pMinimumEngineVersion :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Parameter' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterName ::@ @Maybe Text@
--
-- * @ParameterValue ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Source ::@ @Maybe Text@
--
-- * @DataType ::@ @Maybe Text@
--
-- * @AllowedValues ::@ @Maybe Text@
--
-- * @IsModifiable ::@ @Maybe Bool@
--
-- * @MinimumEngineVersion ::@ @Maybe Text@
--
parameter :: Parameter
parameter = Parameter
    { _pParameterName = Nothing
    , _pParameterValue = Nothing
    , _pDescription = Nothing
    , _pSource = Nothing
    , _pDataType = Nothing
    , _pAllowedValues = Nothing
    , _pIsModifiable = Nothing
    , _pMinimumEngineVersion = Nothing
    }

-- | The name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\s a -> s { _pParameterName = a })

-- | The value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s { _pParameterValue = a })

-- | A description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\s a -> s { _pDescription = a })

-- | The source of the parameter value, such as "engine-default" or "user".
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\s a -> s { _pSource = a })

-- | The data type of the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\s a -> s { _pDataType = a })

-- | The valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\s a -> s { _pAllowedValues = a })

-- | If true, the parameter can be modified. Some parameters have security or
-- operational implications that prevent them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\s a -> s { _pIsModifiable = a })

-- | The earliest engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion =
    lens _pMinimumEngineVersion (\s a -> s { _pMinimumEngineVersion = a })

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

instance ToQuery Parameter where
    toQuery = genericQuery def

-- | If present, changes to the cluster are pending. Specific pending changes
-- are identified by subelements.
data PendingModifiedValues = PendingModifiedValues
    { _pmvMasterUserPassword :: Maybe Text
    , _pmvNodeType :: Maybe Text
    , _pmvNumberOfNodes :: Maybe Integer
    , _pmvClusterType :: Maybe Text
    , _pmvClusterVersion :: Maybe Text
    , _pmvAutomatedSnapshotRetentionPeriod :: Maybe Integer
    , _pmvClusterIdentifier :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PendingModifiedValues' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MasterUserPassword ::@ @Maybe Text@
--
-- * @NodeType ::@ @Maybe Text@
--
-- * @NumberOfNodes ::@ @Maybe Integer@
--
-- * @ClusterType ::@ @Maybe Text@
--
-- * @ClusterVersion ::@ @Maybe Text@
--
-- * @AutomatedSnapshotRetentionPeriod ::@ @Maybe Integer@
--
-- * @ClusterIdentifier ::@ @Maybe Text@
--
pendingModifiedValues :: PendingModifiedValues
pendingModifiedValues = PendingModifiedValues
    { _pmvMasterUserPassword = Nothing
    , _pmvNodeType = Nothing
    , _pmvNumberOfNodes = Nothing
    , _pmvClusterType = Nothing
    , _pmvClusterVersion = Nothing
    , _pmvAutomatedSnapshotRetentionPeriod = Nothing
    , _pmvClusterIdentifier = Nothing
    }

-- | The pending or in-progress change of the master user password for the
-- cluster.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword =
    lens _pmvMasterUserPassword (\s a -> s { _pmvMasterUserPassword = a })

-- | The pending or in-progress change of the cluster's node type.
pmvNodeType :: Lens' PendingModifiedValues (Maybe Text)
pmvNodeType = lens _pmvNodeType (\s a -> s { _pmvNodeType = a })

-- | The pending or in-progress change of the number of nodes in the cluster.
pmvNumberOfNodes :: Lens' PendingModifiedValues (Maybe Integer)
pmvNumberOfNodes =
    lens _pmvNumberOfNodes (\s a -> s { _pmvNumberOfNodes = a })

-- | The pending or in-progress change of the cluster type.
pmvClusterType :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterType = lens _pmvClusterType (\s a -> s { _pmvClusterType = a })

-- | The pending or in-progress change of the service version.
pmvClusterVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterVersion =
    lens _pmvClusterVersion (\s a -> s { _pmvClusterVersion = a })

-- | The pending or in-progress change of the automated snapshot retention
-- period.
pmvAutomatedSnapshotRetentionPeriod :: Lens' PendingModifiedValues (Maybe Integer)
pmvAutomatedSnapshotRetentionPeriod =
    lens _pmvAutomatedSnapshotRetentionPeriod
         (\s a -> s { _pmvAutomatedSnapshotRetentionPeriod = a })

-- | The pending or in-progress change of the new identifier for the cluster.
pmvClusterIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterIdentifier =
    lens _pmvClusterIdentifier (\s a -> s { _pmvClusterIdentifier = a })

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingModifiedValues"

instance ToQuery PendingModifiedValues where
    toQuery = genericQuery def

-- | Describes a recurring charge.
data RecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount :: Maybe Double
    , _rcRecurringChargeFrequency :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecurringCharge' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RecurringChargeAmount ::@ @Maybe Double@
--
-- * @RecurringChargeFrequency ::@ @Maybe Text@
--
recurringCharge :: RecurringCharge
recurringCharge = RecurringCharge
    { _rcRecurringChargeAmount = Nothing
    , _rcRecurringChargeFrequency = Nothing
    }

-- | The amount charged per the period of time specified by the recurring charge
-- frequency.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount =
    lens _rcRecurringChargeAmount
         (\s a -> s { _rcRecurringChargeAmount = a })

-- | The frequency at which the recurring charge amount is applied.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency =
    lens _rcRecurringChargeFrequency
         (\s a -> s { _rcRecurringChargeFrequency = a })

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecurringCharge"

instance ToQuery RecurringCharge where
    toQuery = genericQuery def

-- | Describes a reserved node.
data ReservedNode = ReservedNode
    { _rnReservedNodeId :: Maybe Text
    , _rnReservedNodeOfferingId :: Maybe Text
    , _rnNodeType :: Maybe Text
    , _rnStartTime :: Maybe ISO8601
    , _rnDuration :: Maybe Integer
    , _rnFixedPrice :: Maybe Double
    , _rnUsagePrice :: Maybe Double
    , _rnCurrencyCode :: Maybe Text
    , _rnNodeCount :: Maybe Integer
    , _rnState :: Maybe Text
    , _rnOfferingType :: Maybe Text
    , _rnRecurringCharges :: [RecurringCharge]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedNode' data type.
--
-- 'ReservedNode' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedNodeId ::@ @Maybe Text@
--
-- * @ReservedNodeOfferingId ::@ @Maybe Text@
--
-- * @NodeType ::@ @Maybe Text@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @FixedPrice ::@ @Maybe Double@
--
-- * @UsagePrice ::@ @Maybe Double@
--
-- * @CurrencyCode ::@ @Maybe Text@
--
-- * @NodeCount ::@ @Maybe Integer@
--
-- * @State ::@ @Maybe Text@
--
-- * @OfferingType ::@ @Maybe Text@
--
-- * @RecurringCharges ::@ @[RecurringCharge]@
--
reservedNode :: ReservedNode
reservedNode = ReservedNode
    { _rnReservedNodeId = Nothing
    , _rnReservedNodeOfferingId = Nothing
    , _rnNodeType = Nothing
    , _rnStartTime = Nothing
    , _rnDuration = Nothing
    , _rnFixedPrice = Nothing
    , _rnUsagePrice = Nothing
    , _rnCurrencyCode = Nothing
    , _rnNodeCount = Nothing
    , _rnState = Nothing
    , _rnOfferingType = Nothing
    , _rnRecurringCharges = mempty
    }

-- | The unique identifier for the reservation.
rnReservedNodeId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeId =
    lens _rnReservedNodeId (\s a -> s { _rnReservedNodeId = a })

-- | The identifier for the reserved node offering.
rnReservedNodeOfferingId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeOfferingId =
    lens _rnReservedNodeOfferingId
         (\s a -> s { _rnReservedNodeOfferingId = a })

-- | The node type of the reserved node.
rnNodeType :: Lens' ReservedNode (Maybe Text)
rnNodeType = lens _rnNodeType (\s a -> s { _rnNodeType = a })

-- | The time the reservation started. You purchase a reserved node offering for
-- a duration. This is the start time of that duration.
rnStartTime :: Lens' ReservedNode (Maybe ISO8601)
rnStartTime = lens _rnStartTime (\s a -> s { _rnStartTime = a })

-- | The duration of the node reservation in seconds.
rnDuration :: Lens' ReservedNode (Maybe Integer)
rnDuration = lens _rnDuration (\s a -> s { _rnDuration = a })

-- | The fixed cost Amazon Redshift charged you for this reserved node.
rnFixedPrice :: Lens' ReservedNode (Maybe Double)
rnFixedPrice = lens _rnFixedPrice (\s a -> s { _rnFixedPrice = a })

-- | The hourly rate Amazon Redshift charge you for this reserved node.
rnUsagePrice :: Lens' ReservedNode (Maybe Double)
rnUsagePrice = lens _rnUsagePrice (\s a -> s { _rnUsagePrice = a })

-- | The currency code for the reserved cluster.
rnCurrencyCode :: Lens' ReservedNode (Maybe Text)
rnCurrencyCode = lens _rnCurrencyCode (\s a -> s { _rnCurrencyCode = a })

-- | The number of reserved compute nodes.
rnNodeCount :: Lens' ReservedNode (Maybe Integer)
rnNodeCount = lens _rnNodeCount (\s a -> s { _rnNodeCount = a })

-- | The state of the reserved Compute Node. Possible Values:
-- pending-payment-This reserved node has recently been purchased, and the
-- sale has been approved, but payment has not yet been confirmed. active-This
-- reserved node is owned by the caller and is available for use.
-- payment-failed-Payment failed for the purchase attempt.
rnState :: Lens' ReservedNode (Maybe Text)
rnState = lens _rnState (\s a -> s { _rnState = a })

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
rnOfferingType :: Lens' ReservedNode (Maybe Text)
rnOfferingType = lens _rnOfferingType (\s a -> s { _rnOfferingType = a })

-- | The recurring charges for the reserved node.
rnRecurringCharges :: Lens' ReservedNode [RecurringCharge]
rnRecurringCharges =
    lens _rnRecurringCharges (\s a -> s { _rnRecurringCharges = a })

instance FromXML ReservedNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedNode"

-- | Describes a reserved node offering.
data ReservedNodeOffering = ReservedNodeOffering
    { _rnoReservedNodeOfferingId :: Maybe Text
    , _rnoNodeType :: Maybe Text
    , _rnoDuration :: Maybe Integer
    , _rnoFixedPrice :: Maybe Double
    , _rnoUsagePrice :: Maybe Double
    , _rnoCurrencyCode :: Maybe Text
    , _rnoOfferingType :: Maybe Text
    , _rnoRecurringCharges :: [RecurringCharge]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedNodeOffering' data type.
--
-- 'ReservedNodeOffering' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedNodeOfferingId ::@ @Maybe Text@
--
-- * @NodeType ::@ @Maybe Text@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @FixedPrice ::@ @Maybe Double@
--
-- * @UsagePrice ::@ @Maybe Double@
--
-- * @CurrencyCode ::@ @Maybe Text@
--
-- * @OfferingType ::@ @Maybe Text@
--
-- * @RecurringCharges ::@ @[RecurringCharge]@
--
reservedNodeOffering :: ReservedNodeOffering
reservedNodeOffering = ReservedNodeOffering
    { _rnoReservedNodeOfferingId = Nothing
    , _rnoNodeType = Nothing
    , _rnoDuration = Nothing
    , _rnoFixedPrice = Nothing
    , _rnoUsagePrice = Nothing
    , _rnoCurrencyCode = Nothing
    , _rnoOfferingType = Nothing
    , _rnoRecurringCharges = mempty
    }

-- | The offering identifier.
rnoReservedNodeOfferingId :: Lens' ReservedNodeOffering (Maybe Text)
rnoReservedNodeOfferingId =
    lens _rnoReservedNodeOfferingId
         (\s a -> s { _rnoReservedNodeOfferingId = a })

-- | The node type offered by the reserved node offering.
rnoNodeType :: Lens' ReservedNodeOffering (Maybe Text)
rnoNodeType = lens _rnoNodeType (\s a -> s { _rnoNodeType = a })

-- | The duration, in seconds, for which the offering will reserve the node.
rnoDuration :: Lens' ReservedNodeOffering (Maybe Integer)
rnoDuration = lens _rnoDuration (\s a -> s { _rnoDuration = a })

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- node offering.
rnoFixedPrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoFixedPrice = lens _rnoFixedPrice (\s a -> s { _rnoFixedPrice = a })

-- | The rate you are charged for each hour the cluster that is using the
-- offering is running.
rnoUsagePrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoUsagePrice = lens _rnoUsagePrice (\s a -> s { _rnoUsagePrice = a })

-- | The currency code for the compute nodes offering.
rnoCurrencyCode :: Lens' ReservedNodeOffering (Maybe Text)
rnoCurrencyCode = lens _rnoCurrencyCode (\s a -> s { _rnoCurrencyCode = a })

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
rnoOfferingType :: Lens' ReservedNodeOffering (Maybe Text)
rnoOfferingType = lens _rnoOfferingType (\s a -> s { _rnoOfferingType = a })

-- | The charge to your account regardless of whether you are creating any
-- clusters using the node offering. Recurring charges are only in effect for
-- heavy-utilization reserved nodes.
rnoRecurringCharges :: Lens' ReservedNodeOffering [RecurringCharge]
rnoRecurringCharges =
    lens _rnoRecurringCharges (\s a -> s { _rnoRecurringCharges = a })

instance FromXML ReservedNodeOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedNodeOffering"

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
data RestoreStatus = RestoreStatus
    { _rsStatus :: Maybe Text
    , _rsCurrentRestoreRateInMegaBytesPerSecond :: Maybe Double
    , _rsSnapshotSizeInMegaBytes :: Maybe Integer
    , _rsProgressInMegaBytes :: Maybe Integer
    , _rsElapsedTimeInSeconds :: Maybe Integer
    , _rsEstimatedTimeToCompletionInSeconds :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RestoreStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @Maybe Text@
--
-- * @CurrentRestoreRateInMegaBytesPerSecond ::@ @Maybe Double@
--
-- * @SnapshotSizeInMegaBytes ::@ @Maybe Integer@
--
-- * @ProgressInMegaBytes ::@ @Maybe Integer@
--
-- * @ElapsedTimeInSeconds ::@ @Maybe Integer@
--
-- * @EstimatedTimeToCompletionInSeconds ::@ @Maybe Integer@
--
restoreStatus :: RestoreStatus
restoreStatus = RestoreStatus
    { _rsStatus = Nothing
    , _rsCurrentRestoreRateInMegaBytesPerSecond = Nothing
    , _rsSnapshotSizeInMegaBytes = Nothing
    , _rsProgressInMegaBytes = Nothing
    , _rsElapsedTimeInSeconds = Nothing
    , _rsEstimatedTimeToCompletionInSeconds = Nothing
    }

-- | The status of the restore action. Returns starting, restoring, completed,
-- or failed.
rsStatus :: Lens' RestoreStatus (Maybe Text)
rsStatus = lens _rsStatus (\s a -> s { _rsStatus = a })

-- | The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup.
rsCurrentRestoreRateInMegaBytesPerSecond :: Lens' RestoreStatus (Maybe Double)
rsCurrentRestoreRateInMegaBytesPerSecond =
    lens _rsCurrentRestoreRateInMegaBytesPerSecond
         (\s a -> s { _rsCurrentRestoreRateInMegaBytesPerSecond = a })

-- | The size of the set of snapshot data used to restore the cluster.
rsSnapshotSizeInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsSnapshotSizeInMegaBytes =
    lens _rsSnapshotSizeInMegaBytes
         (\s a -> s { _rsSnapshotSizeInMegaBytes = a })

-- | The number of megabytes that have been transferred from snapshot storage.
rsProgressInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsProgressInMegaBytes =
    lens _rsProgressInMegaBytes (\s a -> s { _rsProgressInMegaBytes = a })

-- | The amount of time an in-progress restore has been running, or the amount
-- of time it took a completed restore to finish.
rsElapsedTimeInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsElapsedTimeInSeconds =
    lens _rsElapsedTimeInSeconds (\s a -> s { _rsElapsedTimeInSeconds = a })

-- | The estimate of the time remaining before the restore will complete.
-- Returns 0 for a completed restore.
rsEstimatedTimeToCompletionInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsEstimatedTimeToCompletionInSeconds =
    lens _rsEstimatedTimeToCompletionInSeconds
         (\s a -> s { _rsEstimatedTimeToCompletionInSeconds = a })

instance FromXML RestoreStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RestoreStatus"

instance ToQuery RestoreStatus where
    toQuery = genericQuery def

-- | Describes a snapshot.
data Snapshot = Snapshot
    { _sSnapshotIdentifier :: Maybe Text
    , _sClusterIdentifier :: Maybe Text
    , _sSnapshotCreateTime :: Maybe ISO8601
    , _sStatus :: Maybe Text
    , _sPort :: Maybe Integer
    , _sAvailabilityZone :: Maybe Text
    , _sClusterCreateTime :: Maybe ISO8601
    , _sMasterUsername :: Maybe Text
    , _sClusterVersion :: Maybe Text
    , _sSnapshotType :: Maybe Text
    , _sNodeType :: Maybe Text
    , _sNumberOfNodes :: Maybe Integer
    , _sDBName :: Maybe Text
    , _sVpcId :: Maybe Text
    , _sEncrypted :: Maybe Bool
    , _sEncryptedWithHSM :: Maybe Bool
    , _sAccountsWithRestoreAccess :: [AccountWithRestoreAccess]
    , _sOwnerAccount :: Maybe Text
    , _sTotalBackupSizeInMegaBytes :: Maybe Double
    , _sActualIncrementalBackupSizeInMegaBytes :: Maybe Double
    , _sBackupProgressInMegaBytes :: Maybe Double
    , _sCurrentBackupRateInMegaBytesPerSecond :: Maybe Double
    , _sEstimatedSecondsToCompletion :: Maybe Integer
    , _sElapsedTimeInSeconds :: Maybe Integer
    , _sSourceRegion :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Snapshot' data type.
--
-- 'Snapshot' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotIdentifier ::@ @Maybe Text@
--
-- * @ClusterIdentifier ::@ @Maybe Text@
--
-- * @SnapshotCreateTime ::@ @Maybe ISO8601@
--
-- * @Status ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @ClusterCreateTime ::@ @Maybe ISO8601@
--
-- * @MasterUsername ::@ @Maybe Text@
--
-- * @ClusterVersion ::@ @Maybe Text@
--
-- * @SnapshotType ::@ @Maybe Text@
--
-- * @NodeType ::@ @Maybe Text@
--
-- * @NumberOfNodes ::@ @Maybe Integer@
--
-- * @DBName ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @Encrypted ::@ @Maybe Bool@
--
-- * @EncryptedWithHSM ::@ @Maybe Bool@
--
-- * @AccountsWithRestoreAccess ::@ @[AccountWithRestoreAccess]@
--
-- * @OwnerAccount ::@ @Maybe Text@
--
-- * @TotalBackupSizeInMegaBytes ::@ @Maybe Double@
--
-- * @ActualIncrementalBackupSizeInMegaBytes ::@ @Maybe Double@
--
-- * @BackupProgressInMegaBytes ::@ @Maybe Double@
--
-- * @CurrentBackupRateInMegaBytesPerSecond ::@ @Maybe Double@
--
-- * @EstimatedSecondsToCompletion ::@ @Maybe Integer@
--
-- * @ElapsedTimeInSeconds ::@ @Maybe Integer@
--
-- * @SourceRegion ::@ @Maybe Text@
--
snapshot :: Snapshot
snapshot = Snapshot
    { _sSnapshotIdentifier = Nothing
    , _sClusterIdentifier = Nothing
    , _sSnapshotCreateTime = Nothing
    , _sStatus = Nothing
    , _sPort = Nothing
    , _sAvailabilityZone = Nothing
    , _sClusterCreateTime = Nothing
    , _sMasterUsername = Nothing
    , _sClusterVersion = Nothing
    , _sSnapshotType = Nothing
    , _sNodeType = Nothing
    , _sNumberOfNodes = Nothing
    , _sDBName = Nothing
    , _sVpcId = Nothing
    , _sEncrypted = Nothing
    , _sEncryptedWithHSM = Nothing
    , _sAccountsWithRestoreAccess = mempty
    , _sOwnerAccount = Nothing
    , _sTotalBackupSizeInMegaBytes = Nothing
    , _sActualIncrementalBackupSizeInMegaBytes = Nothing
    , _sBackupProgressInMegaBytes = Nothing
    , _sCurrentBackupRateInMegaBytesPerSecond = Nothing
    , _sEstimatedSecondsToCompletion = Nothing
    , _sElapsedTimeInSeconds = Nothing
    , _sSourceRegion = Nothing
    }

-- | The snapshot identifier that is provided in the request.
sSnapshotIdentifier :: Lens' Snapshot (Maybe Text)
sSnapshotIdentifier =
    lens _sSnapshotIdentifier (\s a -> s { _sSnapshotIdentifier = a })

-- | The identifier of the cluster for which the snapshot was taken.
sClusterIdentifier :: Lens' Snapshot (Maybe Text)
sClusterIdentifier =
    lens _sClusterIdentifier (\s a -> s { _sClusterIdentifier = a })

-- | The time (UTC) when Amazon Redshift began the snapshot. A snapshot contains
-- a copy of the cluster data as of this exact time.
sSnapshotCreateTime :: Lens' Snapshot (Maybe ISO8601)
sSnapshotCreateTime =
    lens _sSnapshotCreateTime (\s a -> s { _sSnapshotCreateTime = a })

-- | The snapshot status. The value of the status depends on the API operation
-- used. CreateClusterSnapshot and CopyClusterSnapshot returns status as
-- "creating". DescribeClusterSnapshots returns status as "creating",
-- "available", "final snapshot", or "failed". DeleteClusterSnapshot returns
-- status as "deleted".
sStatus :: Lens' Snapshot (Maybe Text)
sStatus = lens _sStatus (\s a -> s { _sStatus = a })

-- | The port that the cluster is listening on.
sPort :: Lens' Snapshot (Maybe Integer)
sPort = lens _sPort (\s a -> s { _sPort = a })

-- | The Availability Zone in which the cluster was created.
sAvailabilityZone :: Lens' Snapshot (Maybe Text)
sAvailabilityZone =
    lens _sAvailabilityZone (\s a -> s { _sAvailabilityZone = a })

-- | The time (UTC) when the cluster was originally created.
sClusterCreateTime :: Lens' Snapshot (Maybe ISO8601)
sClusterCreateTime =
    lens _sClusterCreateTime (\s a -> s { _sClusterCreateTime = a })

-- | The master user name for the cluster.
sMasterUsername :: Lens' Snapshot (Maybe Text)
sMasterUsername = lens _sMasterUsername (\s a -> s { _sMasterUsername = a })

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
sClusterVersion :: Lens' Snapshot (Maybe Text)
sClusterVersion = lens _sClusterVersion (\s a -> s { _sClusterVersion = a })

-- | The snapshot type. Snapshots created using CreateClusterSnapshot and
-- CopyClusterSnapshot will be of type "manual".
sSnapshotType :: Lens' Snapshot (Maybe Text)
sSnapshotType = lens _sSnapshotType (\s a -> s { _sSnapshotType = a })

-- | The node type of the nodes in the cluster.
sNodeType :: Lens' Snapshot (Maybe Text)
sNodeType = lens _sNodeType (\s a -> s { _sNodeType = a })

-- | The number of nodes in the cluster.
sNumberOfNodes :: Lens' Snapshot (Maybe Integer)
sNumberOfNodes = lens _sNumberOfNodes (\s a -> s { _sNumberOfNodes = a })

-- | The name of the database that was created when the cluster was created.
sDBName :: Lens' Snapshot (Maybe Text)
sDBName = lens _sDBName (\s a -> s { _sDBName = a })

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a
-- VPC. Otherwise, this field is not in the output.
sVpcId :: Lens' Snapshot (Maybe Text)
sVpcId = lens _sVpcId (\s a -> s { _sVpcId = a })

-- | If true, the data in the snapshot is encrypted at rest.
sEncrypted :: Lens' Snapshot (Maybe Bool)
sEncrypted = lens _sEncrypted (\s a -> s { _sEncrypted = a })

-- | A boolean that indicates whether the snapshot data is encrypted using the
-- HSM keys of the source cluster. true indicates that the data is encrypted
-- using HSM keys.
sEncryptedWithHSM :: Lens' Snapshot (Maybe Bool)
sEncryptedWithHSM =
    lens _sEncryptedWithHSM (\s a -> s { _sEncryptedWithHSM = a })

-- | A list of the AWS customer accounts authorized to restore the snapshot.
-- Returns null if no accounts are authorized. Visible only to the snapshot
-- owner.
sAccountsWithRestoreAccess :: Lens' Snapshot [AccountWithRestoreAccess]
sAccountsWithRestoreAccess =
    lens _sAccountsWithRestoreAccess
         (\s a -> s { _sAccountsWithRestoreAccess = a })

-- | For manual snapshots, the AWS customer account used to create or copy the
-- snapshot. For automatic snapshots, the owner of the cluster. The owner can
-- perform all snapshot actions, such as sharing a manual snapshot.
sOwnerAccount :: Lens' Snapshot (Maybe Text)
sOwnerAccount = lens _sOwnerAccount (\s a -> s { _sOwnerAccount = a })

-- | The size of the complete set of backup data that would be used to restore
-- the cluster.
sTotalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
sTotalBackupSizeInMegaBytes =
    lens _sTotalBackupSizeInMegaBytes
         (\s a -> s { _sTotalBackupSizeInMegaBytes = a })

-- | The size of the incremental backup.
sActualIncrementalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
sActualIncrementalBackupSizeInMegaBytes =
    lens _sActualIncrementalBackupSizeInMegaBytes
         (\s a -> s { _sActualIncrementalBackupSizeInMegaBytes = a })

-- | The number of megabytes that have been transferred to the snapshot backup.
sBackupProgressInMegaBytes :: Lens' Snapshot (Maybe Double)
sBackupProgressInMegaBytes =
    lens _sBackupProgressInMegaBytes
         (\s a -> s { _sBackupProgressInMegaBytes = a })

-- | The number of megabytes per second being transferred to the snapshot
-- backup. Returns 0 for a completed backup.
sCurrentBackupRateInMegaBytesPerSecond :: Lens' Snapshot (Maybe Double)
sCurrentBackupRateInMegaBytesPerSecond =
    lens _sCurrentBackupRateInMegaBytesPerSecond
         (\s a -> s { _sCurrentBackupRateInMegaBytesPerSecond = a })

-- | The estimate of the time remaining before the snapshot backup will
-- complete. Returns 0 for a completed backup.
sEstimatedSecondsToCompletion :: Lens' Snapshot (Maybe Integer)
sEstimatedSecondsToCompletion =
    lens _sEstimatedSecondsToCompletion
         (\s a -> s { _sEstimatedSecondsToCompletion = a })

-- | The amount of time an in-progress snapshot backup has been running, or the
-- amount of time it took a completed backup to finish.
sElapsedTimeInSeconds :: Lens' Snapshot (Maybe Integer)
sElapsedTimeInSeconds =
    lens _sElapsedTimeInSeconds (\s a -> s { _sElapsedTimeInSeconds = a })

-- | The source region from which the snapshot was copied.
sSourceRegion :: Lens' Snapshot (Maybe Text)
sSourceRegion = lens _sSourceRegion (\s a -> s { _sSourceRegion = a })

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Snapshot"

-- | Describes a subnet.
data Subnet = Subnet
    { _srSubnetIdentifier :: Maybe Text
    , _srSubnetAvailabilityZone :: Maybe AvailabilityZone
    , _srSubnetStatus :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Subnet' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubnetIdentifier ::@ @Maybe Text@
--
-- * @SubnetAvailabilityZone ::@ @Maybe AvailabilityZone@
--
-- * @SubnetStatus ::@ @Maybe Text@
--
subnet :: Subnet
subnet = Subnet
    { _srSubnetIdentifier = Nothing
    , _srSubnetAvailabilityZone = Nothing
    , _srSubnetStatus = Nothing
    }

-- | The identifier of the subnet.
srSubnetIdentifier :: Lens' Subnet (Maybe Text)
srSubnetIdentifier =
    lens _srSubnetIdentifier (\s a -> s { _srSubnetIdentifier = a })

-- | Describes an availability zone.
srSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
srSubnetAvailabilityZone =
    lens _srSubnetAvailabilityZone
         (\s a -> s { _srSubnetAvailabilityZone = a })

-- | The status of the subnet.
srSubnetStatus :: Lens' Subnet (Maybe Text)
srSubnetStatus = lens _srSubnetStatus (\s a -> s { _srSubnetStatus = a })

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericQuery def

-- | Describes the members of a VPC security group.
data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmVpcSecurityGroupId :: Maybe Text
    , _vsgmStatus :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcSecurityGroupMembership' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcSecurityGroupId ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
vpcSecurityGroupMembership :: VpcSecurityGroupMembership
vpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmVpcSecurityGroupId = Nothing
    , _vsgmStatus = Nothing
    }

-- | 
vsgmVpcSecurityGroupId :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmVpcSecurityGroupId =
    lens _vsgmVpcSecurityGroupId (\s a -> s { _vsgmVpcSecurityGroupId = a })

-- | 
vsgmStatus :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\s a -> s { _vsgmStatus = a })

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroup"

instance ToQuery VpcSecurityGroupMembership where
    toQuery = genericQuery def
