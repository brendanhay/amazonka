{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Redshift__
--
-- __Overview__
--
-- This is an interface reference for Amazon Redshift. It contains documentation for one of the programming or command line interfaces you can use to manage Amazon Redshift clusters. Note that Amazon Redshift is asynchronous, which means that some interfaces may require techniques, such as polling or asynchronous callback handlers, to determine when a command has been applied. In this reference, the parameter descriptions indicate whether a change is applied immediately, on the next instance reboot, or during the next maintenance window. For a summary of the Amazon Redshift cluster management interfaces, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/using-aws-sdk.html Using the Amazon Redshift Management Interfaces> .
--
-- Amazon Redshift manages all the work of setting up, operating, and scaling a data warehouse: provisioning capacity, monitoring and backing up the cluster, and applying patches and upgrades to the Amazon Redshift engine. You can focus on using your data to acquire new insights for your business and customers.
--
-- If you are a first-time user of Amazon Redshift, we recommend that you begin by reading the <http://docs.aws.amazon.com/redshift/latest/gsg/getting-started.html Amazon Redshift Getting Started Guide> .
--
-- If you are a database developer, the <http://docs.aws.amazon.com/redshift/latest/dg/welcome.html Amazon Redshift Database Developer Guide> explains how to design, build, query, and maintain the databases that make up your data warehouse.
--
module Network.AWS.Redshift
    (
    -- * Service Configuration
      redshift

    -- * Errors
    -- $errors

    -- ** ClusterSecurityGroupQuotaExceededFault
    , _ClusterSecurityGroupQuotaExceededFault

    -- ** InvalidS3KeyPrefixFault
    , _InvalidS3KeyPrefixFault

    -- ** SourceNotFoundFault
    , _SourceNotFoundFault

    -- ** AuthorizationQuotaExceededFault
    , _AuthorizationQuotaExceededFault

    -- ** CopyToRegionDisabledFault
    , _CopyToRegionDisabledFault

    -- ** LimitExceededFault
    , _LimitExceededFault

    -- ** InvalidClusterSecurityGroupStateFault
    , _InvalidClusterSecurityGroupStateFault

    -- ** ClusterSecurityGroupAlreadyExistsFault
    , _ClusterSecurityGroupAlreadyExistsFault

    -- ** ClusterSnapshotNotFoundFault
    , _ClusterSnapshotNotFoundFault

    -- ** InvalidElasticIPFault
    , _InvalidElasticIPFault

    -- ** TableRestoreNotFoundFault
    , _TableRestoreNotFoundFault

    -- ** HSMConfigurationNotFoundFault
    , _HSMConfigurationNotFoundFault

    -- ** AuthorizationAlreadyExistsFault
    , _AuthorizationAlreadyExistsFault

    -- ** SubscriptionCategoryNotFoundFault
    , _SubscriptionCategoryNotFoundFault

    -- ** HSMConfigurationAlreadyExistsFault
    , _HSMConfigurationAlreadyExistsFault

    -- ** SubscriptionNotFoundFault
    , _SubscriptionNotFoundFault

    -- ** InvalidS3BucketNameFault
    , _InvalidS3BucketNameFault

    -- ** ClusterSnapshotAlreadyExistsFault
    , _ClusterSnapshotAlreadyExistsFault

    -- ** InvalidSubnet
    , _InvalidSubnet

    -- ** InvalidHSMConfigurationStateFault
    , _InvalidHSMConfigurationStateFault

    -- ** SnapshotCopyAlreadyDisabledFault
    , _SnapshotCopyAlreadyDisabledFault

    -- ** ClusterQuotaExceededFault
    , _ClusterQuotaExceededFault

    -- ** HSMClientCertificateQuotaExceededFault
    , _HSMClientCertificateQuotaExceededFault

    -- ** ClusterParameterGroupNotFoundFault
    , _ClusterParameterGroupNotFoundFault

    -- ** SnapshotCopyGrantQuotaExceededFault
    , _SnapshotCopyGrantQuotaExceededFault

    -- ** NumberOfNodesPerClusterLimitExceededFault
    , _NumberOfNodesPerClusterLimitExceededFault

    -- ** SnapshotCopyAlreadyEnabledFault
    , _SnapshotCopyAlreadyEnabledFault

    -- ** ClusterParameterGroupAlreadyExistsFault
    , _ClusterParameterGroupAlreadyExistsFault

    -- ** SnapshotCopyDisabledFault
    , _SnapshotCopyDisabledFault

    -- ** ResizeNotFoundFault
    , _ResizeNotFoundFault

    -- ** HSMClientCertificateNotFoundFault
    , _HSMClientCertificateNotFoundFault

    -- ** SNSTopicARNNotFoundFault
    , _SNSTopicARNNotFoundFault

    -- ** ClusterNotFoundFault
    , _ClusterNotFoundFault

    -- ** ClusterParameterGroupQuotaExceededFault
    , _ClusterParameterGroupQuotaExceededFault

    -- ** SnapshotCopyGrantAlreadyExistsFault
    , _SnapshotCopyGrantAlreadyExistsFault

    -- ** SNSNoAuthorizationFault
    , _SNSNoAuthorizationFault

    -- ** InvalidClusterStateFault
    , _InvalidClusterStateFault

    -- ** InvalidTableRestoreArgumentFault
    , _InvalidTableRestoreArgumentFault

    -- ** SnapshotCopyGrantNotFoundFault
    , _SnapshotCopyGrantNotFoundFault

    -- ** HSMConfigurationQuotaExceededFault
    , _HSMConfigurationQuotaExceededFault

    -- ** ClusterSnapshotQuotaExceededFault
    , _ClusterSnapshotQuotaExceededFault

    -- ** InsufficientClusterCapacityFault
    , _InsufficientClusterCapacityFault

    -- ** SNSInvalidTopicFault
    , _SNSInvalidTopicFault

    -- ** DependentServiceUnavailableFault
    , _DependentServiceUnavailableFault

    -- ** UnsupportedOptionFault
    , _UnsupportedOptionFault

    -- ** SubscriptionAlreadyExistFault
    , _SubscriptionAlreadyExistFault

    -- ** InvalidVPCNetworkStateFault
    , _InvalidVPCNetworkStateFault

    -- ** ClusterSubnetGroupNotFoundFault
    , _ClusterSubnetGroupNotFoundFault

    -- ** BucketNotFoundFault
    , _BucketNotFoundFault

    -- ** InvalidSubscriptionStateFault
    , _InvalidSubscriptionStateFault

    -- ** DependentServiceRequestThrottlingFault
    , _DependentServiceRequestThrottlingFault

    -- ** AuthorizationNotFoundFault
    , _AuthorizationNotFoundFault

    -- ** InvalidClusterSubnetGroupStateFault
    , _InvalidClusterSubnetGroupStateFault

    -- ** UnsupportedOperationFault
    , _UnsupportedOperationFault

    -- ** ClusterSubnetGroupAlreadyExistsFault
    , _ClusterSubnetGroupAlreadyExistsFault

    -- ** InvalidClusterSnapshotStateFault
    , _InvalidClusterSnapshotStateFault

    -- ** ClusterSecurityGroupNotFoundFault
    , _ClusterSecurityGroupNotFoundFault

    -- ** ReservedNodeNotFoundFault
    , _ReservedNodeNotFoundFault

    -- ** ReservedNodeOfferingNotFoundFault
    , _ReservedNodeOfferingNotFoundFault

    -- ** InvalidClusterSubnetStateFault
    , _InvalidClusterSubnetStateFault

    -- ** IncompatibleOrderableOptions
    , _IncompatibleOrderableOptions

    -- ** EventSubscriptionQuotaExceededFault
    , _EventSubscriptionQuotaExceededFault

    -- ** InvalidClusterParameterGroupStateFault
    , _InvalidClusterParameterGroupStateFault

    -- ** ReservedNodeAlreadyExistsFault
    , _ReservedNodeAlreadyExistsFault

    -- ** InProgressTableRestoreQuotaExceededFault
    , _InProgressTableRestoreQuotaExceededFault

    -- ** InvalidRestoreFault
    , _InvalidRestoreFault

    -- ** ResourceNotFoundFault
    , _ResourceNotFoundFault

    -- ** SubscriptionEventIdNotFoundFault
    , _SubscriptionEventIdNotFoundFault

    -- ** InvalidSnapshotCopyGrantStateFault
    , _InvalidSnapshotCopyGrantStateFault

    -- ** UnknownSnapshotCopyRegionFault
    , _UnknownSnapshotCopyRegionFault

    -- ** ReservedNodeQuotaExceededFault
    , _ReservedNodeQuotaExceededFault

    -- ** ClusterSubnetQuotaExceededFault
    , _ClusterSubnetQuotaExceededFault

    -- ** ClusterAlreadyExistsFault
    , _ClusterAlreadyExistsFault

    -- ** AccessToSnapshotDeniedFault
    , _AccessToSnapshotDeniedFault

    -- ** TagLimitExceededFault
    , _TagLimitExceededFault

    -- ** NumberOfNodesQuotaExceededFault
    , _NumberOfNodesQuotaExceededFault

    -- ** HSMClientCertificateAlreadyExistsFault
    , _HSMClientCertificateAlreadyExistsFault

    -- ** InvalidHSMClientCertificateStateFault
    , _InvalidHSMClientCertificateStateFault

    -- ** SubnetAlreadyInUse
    , _SubnetAlreadyInUse

    -- ** SubscriptionSeverityNotFoundFault
    , _SubscriptionSeverityNotFoundFault

    -- ** UnauthorizedOperation
    , _UnauthorizedOperation

    -- ** InvalidTagFault
    , _InvalidTagFault

    -- ** InsufficientS3BucketPolicyFault
    , _InsufficientS3BucketPolicyFault

    -- ** ClusterSubnetGroupQuotaExceededFault
    , _ClusterSubnetGroupQuotaExceededFault

    -- * Waiters
    -- $waiters

    -- ** ClusterRestored
    , clusterRestored

    -- ** ClusterDeleted
    , clusterDeleted

    -- ** SnapshotAvailable
    , snapshotAvailable

    -- ** ClusterAvailable
    , clusterAvailable

    -- * Operations
    -- $operations

    -- ** DescribeClusters (Paginated)
    , module Network.AWS.Redshift.DescribeClusters

    -- ** DescribeTags
    , module Network.AWS.Redshift.DescribeTags

    -- ** DeleteClusterSubnetGroup
    , module Network.AWS.Redshift.DeleteClusterSubnetGroup

    -- ** DisableLogging
    , module Network.AWS.Redshift.DisableLogging

    -- ** ModifyEventSubscription
    , module Network.AWS.Redshift.ModifyEventSubscription

    -- ** DeleteClusterSnapshot
    , module Network.AWS.Redshift.DeleteClusterSnapshot

    -- ** PurchaseReservedNodeOffering
    , module Network.AWS.Redshift.PurchaseReservedNodeOffering

    -- ** DescribeReservedNodeOfferings (Paginated)
    , module Network.AWS.Redshift.DescribeReservedNodeOfferings

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.Redshift.DescribeEvents

    -- ** DescribeReservedNodes (Paginated)
    , module Network.AWS.Redshift.DescribeReservedNodes

    -- ** DescribeClusterParameterGroups (Paginated)
    , module Network.AWS.Redshift.DescribeClusterParameterGroups

    -- ** EnableLogging
    , module Network.AWS.Redshift.EnableLogging

    -- ** CreateClusterSubnetGroup
    , module Network.AWS.Redshift.CreateClusterSubnetGroup

    -- ** DeleteClusterParameterGroup
    , module Network.AWS.Redshift.DeleteClusterParameterGroup

    -- ** DescribeClusterSecurityGroups (Paginated)
    , module Network.AWS.Redshift.DescribeClusterSecurityGroups

    -- ** CreateTags
    , module Network.AWS.Redshift.CreateTags

    -- ** EnableSnapshotCopy
    , module Network.AWS.Redshift.EnableSnapshotCopy

    -- ** DescribeClusterSnapshots (Paginated)
    , module Network.AWS.Redshift.DescribeClusterSnapshots

    -- ** DeleteTags
    , module Network.AWS.Redshift.DeleteTags

    -- ** DescribeClusterSubnetGroups (Paginated)
    , module Network.AWS.Redshift.DescribeClusterSubnetGroups

    -- ** ModifySnapshotCopyRetentionPeriod
    , module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod

    -- ** ModifyClusterIAMRoles
    , module Network.AWS.Redshift.ModifyClusterIAMRoles

    -- ** AuthorizeSnapshotAccess
    , module Network.AWS.Redshift.AuthorizeSnapshotAccess

    -- ** RebootCluster
    , module Network.AWS.Redshift.RebootCluster

    -- ** DeleteCluster
    , module Network.AWS.Redshift.DeleteCluster

    -- ** CreateEventSubscription
    , module Network.AWS.Redshift.CreateEventSubscription

    -- ** DescribeOrderableClusterOptions (Paginated)
    , module Network.AWS.Redshift.DescribeOrderableClusterOptions

    -- ** CreateCluster
    , module Network.AWS.Redshift.CreateCluster

    -- ** CreateHSMClientCertificate
    , module Network.AWS.Redshift.CreateHSMClientCertificate

    -- ** RestoreTableFromClusterSnapshot
    , module Network.AWS.Redshift.RestoreTableFromClusterSnapshot

    -- ** DescribeDefaultClusterParameters (Paginated)
    , module Network.AWS.Redshift.DescribeDefaultClusterParameters

    -- ** DeleteEventSubscription
    , module Network.AWS.Redshift.DeleteEventSubscription

    -- ** ResetClusterParameterGroup
    , module Network.AWS.Redshift.ResetClusterParameterGroup

    -- ** DescribeEventSubscriptions (Paginated)
    , module Network.AWS.Redshift.DescribeEventSubscriptions

    -- ** RevokeClusterSecurityGroupIngress
    , module Network.AWS.Redshift.RevokeClusterSecurityGroupIngress

    -- ** DescribeHSMClientCertificates (Paginated)
    , module Network.AWS.Redshift.DescribeHSMClientCertificates

    -- ** ModifyClusterParameterGroup
    , module Network.AWS.Redshift.ModifyClusterParameterGroup

    -- ** GetClusterCredentials
    , module Network.AWS.Redshift.GetClusterCredentials

    -- ** CreateClusterSecurityGroup
    , module Network.AWS.Redshift.CreateClusterSecurityGroup

    -- ** DescribeEventCategories
    , module Network.AWS.Redshift.DescribeEventCategories

    -- ** DescribeResize
    , module Network.AWS.Redshift.DescribeResize

    -- ** DeleteHSMConfiguration
    , module Network.AWS.Redshift.DeleteHSMConfiguration

    -- ** AuthorizeClusterSecurityGroupIngress
    , module Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress

    -- ** DescribeTableRestoreStatus
    , module Network.AWS.Redshift.DescribeTableRestoreStatus

    -- ** CreateClusterSnapshot
    , module Network.AWS.Redshift.CreateClusterSnapshot

    -- ** CreateHSMConfiguration
    , module Network.AWS.Redshift.CreateHSMConfiguration

    -- ** DescribeLoggingStatus
    , module Network.AWS.Redshift.DescribeLoggingStatus

    -- ** ModifyCluster
    , module Network.AWS.Redshift.ModifyCluster

    -- ** DeleteClusterSecurityGroup
    , module Network.AWS.Redshift.DeleteClusterSecurityGroup

    -- ** DisableSnapshotCopy
    , module Network.AWS.Redshift.DisableSnapshotCopy

    -- ** DescribeClusterParameters (Paginated)
    , module Network.AWS.Redshift.DescribeClusterParameters

    -- ** RestoreFromClusterSnapshot
    , module Network.AWS.Redshift.RestoreFromClusterSnapshot

    -- ** CreateClusterParameterGroup
    , module Network.AWS.Redshift.CreateClusterParameterGroup

    -- ** RevokeSnapshotAccess
    , module Network.AWS.Redshift.RevokeSnapshotAccess

    -- ** DescribeHSMConfigurations (Paginated)
    , module Network.AWS.Redshift.DescribeHSMConfigurations

    -- ** CreateSnapshotCopyGrant
    , module Network.AWS.Redshift.CreateSnapshotCopyGrant

    -- ** CopyClusterSnapshot
    , module Network.AWS.Redshift.CopyClusterSnapshot

    -- ** DeleteHSMClientCertificate
    , module Network.AWS.Redshift.DeleteHSMClientCertificate

    -- ** DeleteSnapshotCopyGrant
    , module Network.AWS.Redshift.DeleteSnapshotCopyGrant

    -- ** DescribeClusterVersions (Paginated)
    , module Network.AWS.Redshift.DescribeClusterVersions

    -- ** ModifyClusterSubnetGroup
    , module Network.AWS.Redshift.ModifyClusterSubnetGroup

    -- ** RotateEncryptionKey
    , module Network.AWS.Redshift.RotateEncryptionKey

    -- ** DescribeSnapshotCopyGrants
    , module Network.AWS.Redshift.DescribeSnapshotCopyGrants

    -- * Types

    -- ** Common
    , module Network.AWS.Redshift.Internal

    -- ** ParameterApplyType
    , ParameterApplyType (..)

    -- ** ReservedNodeOfferingType
    , ReservedNodeOfferingType (..)

    -- ** SourceType
    , SourceType (..)

    -- ** TableRestoreStatusType
    , TableRestoreStatusType (..)

    -- ** AccountWithRestoreAccess
    , AccountWithRestoreAccess
    , accountWithRestoreAccess
    , awraAccountAlias
    , awraAccountId

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName
    , azSupportedPlatforms

    -- ** Cluster
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

    -- ** ClusterIAMRole
    , ClusterIAMRole
    , clusterIAMRole
    , cirIAMRoleARN
    , cirApplyStatus

    -- ** ClusterNode
    , ClusterNode
    , clusterNode
    , cnNodeRole
    , cnPrivateIPAddress
    , cnPublicIPAddress

    -- ** ClusterParameterGroup
    , ClusterParameterGroup
    , clusterParameterGroup
    , cpgParameterGroupFamily
    , cpgDescription
    , cpgTags
    , cpgParameterGroupName

    -- ** ClusterParameterGroupNameMessage
    , ClusterParameterGroupNameMessage
    , clusterParameterGroupNameMessage
    , cpgnmParameterGroupStatus
    , cpgnmParameterGroupName

    -- ** ClusterParameterGroupStatus
    , ClusterParameterGroupStatus
    , clusterParameterGroupStatus
    , cpgsClusterParameterStatusList
    , cpgsParameterApplyStatus
    , cpgsParameterGroupName

    -- ** ClusterParameterStatus
    , ClusterParameterStatus
    , clusterParameterStatus
    , cpsParameterApplyErrorDescription
    , cpsParameterName
    , cpsParameterApplyStatus

    -- ** ClusterSecurityGroup
    , ClusterSecurityGroup
    , clusterSecurityGroup
    , cluClusterSecurityGroupName
    , cluIPRanges
    , cluEC2SecurityGroups
    , cluDescription
    , cluTags

    -- ** ClusterSecurityGroupMembership
    , ClusterSecurityGroupMembership
    , clusterSecurityGroupMembership
    , csgmStatus
    , csgmClusterSecurityGroupName

    -- ** ClusterSnapshotCopyStatus
    , ClusterSnapshotCopyStatus
    , clusterSnapshotCopyStatus
    , cscsRetentionPeriod
    , cscsDestinationRegion
    , cscsSnapshotCopyGrantName

    -- ** ClusterSubnetGroup
    , ClusterSubnetGroup
    , clusterSubnetGroup
    , csgVPCId
    , csgSubnets
    , csgClusterSubnetGroupName
    , csgSubnetGroupStatus
    , csgDescription
    , csgTags

    -- ** ClusterVersion
    , ClusterVersion
    , clusterVersion
    , cvClusterParameterGroupFamily
    , cvClusterVersion
    , cvDescription

    -- ** DefaultClusterParameters
    , DefaultClusterParameters
    , defaultClusterParameters
    , dcpMarker
    , dcpParameters
    , dcpParameterGroupFamily

    -- ** EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , esgStatus
    , esgEC2SecurityGroupOwnerId
    , esgEC2SecurityGroupName
    , esgTags

    -- ** ElasticIPStatus
    , ElasticIPStatus
    , elasticIPStatus
    , eisStatus
    , eisElasticIP

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- ** Event
    , Event
    , event
    , eSourceType
    , eSeverity
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage
    , eEventId

    -- ** EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmSourceType
    , ecmEvents

    -- ** EventInfoMap
    , EventInfoMap
    , eventInfoMap
    , eimEventDescription
    , eimSeverity
    , eimEventCategories
    , eimEventId

    -- ** EventSubscription
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

    -- ** HSMClientCertificate
    , HSMClientCertificate
    , hsmClientCertificate
    , hccHSMClientCertificateIdentifier
    , hccHSMClientCertificatePublicKey
    , hccTags

    -- ** HSMConfiguration
    , HSMConfiguration
    , hsmConfiguration
    , hcHSMConfigurationIdentifier
    , hcHSMPartitionName
    , hcDescription
    , hcTags
    , hcHSMIPAddress

    -- ** HSMStatus
    , HSMStatus
    , hsmStatus
    , hsStatus
    , hsHSMConfigurationIdentifier
    , hsHSMClientCertificateIdentifier

    -- ** IPRange
    , IPRange
    , ipRange
    , irStatus
    , irCIdRIP
    , irTags

    -- ** LoggingStatus
    , LoggingStatus
    , loggingStatus
    , lsLastFailureTime
    , lsLastSuccessfulDeliveryTime
    , lsS3KeyPrefix
    , lsBucketName
    , lsLoggingEnabled
    , lsLastFailureMessage

    -- ** OrderableClusterOption
    , OrderableClusterOption
    , orderableClusterOption
    , ocoAvailabilityZones
    , ocoClusterType
    , ocoClusterVersion
    , ocoNodeType

    -- ** Parameter
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

    -- ** PendingModifiedValues
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

    -- ** RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- ** ReservedNode
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

    -- ** ReservedNodeOffering
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

    -- ** RestoreStatus
    , RestoreStatus
    , restoreStatus
    , rsStatus
    , rsEstimatedTimeToCompletionInSeconds
    , rsCurrentRestoreRateInMegaBytesPerSecond
    , rsProgressInMegaBytes
    , rsElapsedTimeInSeconds
    , rsSnapshotSizeInMegaBytes

    -- ** Snapshot
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

    -- ** SnapshotCopyGrant
    , SnapshotCopyGrant
    , snapshotCopyGrant
    , scgKMSKeyId
    , scgSnapshotCopyGrantName
    , scgTags

    -- ** Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- ** SupportedPlatform
    , SupportedPlatform
    , supportedPlatform
    , spName

    -- ** TableRestoreStatus
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

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TaggedResource
    , TaggedResource
    , taggedResource
    , trTag
    , trResourceType
    , trResourceName

    -- ** VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
import Network.AWS.Redshift.AuthorizeSnapshotAccess
import Network.AWS.Redshift.CopyClusterSnapshot
import Network.AWS.Redshift.CreateCluster
import Network.AWS.Redshift.CreateClusterParameterGroup
import Network.AWS.Redshift.CreateClusterSecurityGroup
import Network.AWS.Redshift.CreateClusterSnapshot
import Network.AWS.Redshift.CreateClusterSubnetGroup
import Network.AWS.Redshift.CreateEventSubscription
import Network.AWS.Redshift.CreateHSMClientCertificate
import Network.AWS.Redshift.CreateHSMConfiguration
import Network.AWS.Redshift.CreateSnapshotCopyGrant
import Network.AWS.Redshift.CreateTags
import Network.AWS.Redshift.DeleteCluster
import Network.AWS.Redshift.DeleteClusterParameterGroup
import Network.AWS.Redshift.DeleteClusterSecurityGroup
import Network.AWS.Redshift.DeleteClusterSnapshot
import Network.AWS.Redshift.DeleteClusterSubnetGroup
import Network.AWS.Redshift.DeleteEventSubscription
import Network.AWS.Redshift.DeleteHSMClientCertificate
import Network.AWS.Redshift.DeleteHSMConfiguration
import Network.AWS.Redshift.DeleteSnapshotCopyGrant
import Network.AWS.Redshift.DeleteTags
import Network.AWS.Redshift.DescribeClusterParameterGroups
import Network.AWS.Redshift.DescribeClusterParameters
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Redshift.DescribeClusterSecurityGroups
import Network.AWS.Redshift.DescribeClusterSnapshots
import Network.AWS.Redshift.DescribeClusterSubnetGroups
import Network.AWS.Redshift.DescribeClusterVersions
import Network.AWS.Redshift.DescribeDefaultClusterParameters
import Network.AWS.Redshift.DescribeEventCategories
import Network.AWS.Redshift.DescribeEvents
import Network.AWS.Redshift.DescribeEventSubscriptions
import Network.AWS.Redshift.DescribeHSMClientCertificates
import Network.AWS.Redshift.DescribeHSMConfigurations
import Network.AWS.Redshift.DescribeLoggingStatus
import Network.AWS.Redshift.DescribeOrderableClusterOptions
import Network.AWS.Redshift.DescribeReservedNodeOfferings
import Network.AWS.Redshift.DescribeReservedNodes
import Network.AWS.Redshift.DescribeResize
import Network.AWS.Redshift.DescribeSnapshotCopyGrants
import Network.AWS.Redshift.DescribeTableRestoreStatus
import Network.AWS.Redshift.DescribeTags
import Network.AWS.Redshift.DisableLogging
import Network.AWS.Redshift.DisableSnapshotCopy
import Network.AWS.Redshift.EnableLogging
import Network.AWS.Redshift.EnableSnapshotCopy
import Network.AWS.Redshift.GetClusterCredentials
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.ModifyCluster
import Network.AWS.Redshift.ModifyClusterIAMRoles
import Network.AWS.Redshift.ModifyClusterParameterGroup
import Network.AWS.Redshift.ModifyClusterSubnetGroup
import Network.AWS.Redshift.ModifyEventSubscription
import Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
import Network.AWS.Redshift.PurchaseReservedNodeOffering
import Network.AWS.Redshift.RebootCluster
import Network.AWS.Redshift.ResetClusterParameterGroup
import Network.AWS.Redshift.RestoreFromClusterSnapshot
import Network.AWS.Redshift.RestoreTableFromClusterSnapshot
import Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
import Network.AWS.Redshift.RevokeSnapshotAccess
import Network.AWS.Redshift.RotateEncryptionKey
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Redshift'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
