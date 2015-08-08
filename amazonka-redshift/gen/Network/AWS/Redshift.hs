{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Amazon Redshift __Overview__
--
-- This is an interface reference for Amazon Redshift. It contains
-- documentation for one of the programming or command line interfaces you
-- can use to manage Amazon Redshift clusters. Note that Amazon Redshift is
-- asynchronous, which means that some interfaces may require techniques,
-- such as polling or asynchronous callback handlers, to determine when a
-- command has been applied. In this reference, the parameter descriptions
-- indicate whether a change is applied immediately, on the next instance
-- reboot, or during the next maintenance window. For a summary of the
-- Amazon Redshift cluster management interfaces, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/using-aws-sdk.html Using the Amazon Redshift Management Interfaces>.
--
-- Amazon Redshift manages all the work of setting up, operating, and
-- scaling a data warehouse: provisioning capacity, monitoring and backing
-- up the cluster, and applying patches and upgrades to the Amazon Redshift
-- engine. You can focus on using your data to acquire new insights for
-- your business and customers.
--
-- If you are a first-time user of Amazon Redshift, we recommend that you
-- begin by reading the The
-- <http://docs.aws.amazon.com/redshift/latest/gsg/getting-started.html Amazon Redshift Getting Started Guide>
--
-- If you are a database developer, the
-- <http://docs.aws.amazon.com/redshift/latest/dg/welcome.html Amazon Redshift Database Developer Guide>
-- explains how to design, build, query, and maintain the databases that
-- make up your data warehouse.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.Redshift
    (
    -- * Service Description
      Redshift

    -- * Error Matchers
    -- $errors
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

    -- * Waiters
    -- $waiters
    , clusterDeleted
    , snapshotAvailable
    , clusterAvailable

    -- * Operations
    -- $operations

    -- ** DescribeClusters (Paginated)
    , module Network.AWS.Redshift.DescribeClusters
    -- $pager

    -- ** DescribeTags
    , module Network.AWS.Redshift.DescribeTags

    -- ** ModifyEventSubscription
    , module Network.AWS.Redshift.ModifyEventSubscription

    -- ** DisableLogging
    , module Network.AWS.Redshift.DisableLogging

    -- ** PurchaseReservedNodeOffering
    , module Network.AWS.Redshift.PurchaseReservedNodeOffering

    -- ** DeleteClusterSubnetGroup
    , module Network.AWS.Redshift.DeleteClusterSubnetGroup

    -- ** DeleteClusterSnapshot
    , module Network.AWS.Redshift.DeleteClusterSnapshot

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.Redshift.DescribeEvents
    -- $pager

    -- ** DescribeReservedNodeOfferings (Paginated)
    , module Network.AWS.Redshift.DescribeReservedNodeOfferings
    -- $pager

    -- ** DescribeClusterParameterGroups (Paginated)
    , module Network.AWS.Redshift.DescribeClusterParameterGroups
    -- $pager

    -- ** CreateClusterSubnetGroup
    , module Network.AWS.Redshift.CreateClusterSubnetGroup

    -- ** DescribeReservedNodes (Paginated)
    , module Network.AWS.Redshift.DescribeReservedNodes
    -- $pager

    -- ** EnableLogging
    , module Network.AWS.Redshift.EnableLogging

    -- ** CreateTags
    , module Network.AWS.Redshift.CreateTags

    -- ** DescribeClusterSecurityGroups (Paginated)
    , module Network.AWS.Redshift.DescribeClusterSecurityGroups
    -- $pager

    -- ** DeleteClusterParameterGroup
    , module Network.AWS.Redshift.DeleteClusterParameterGroup

    -- ** DeleteTags
    , module Network.AWS.Redshift.DeleteTags

    -- ** EnableSnapshotCopy
    , module Network.AWS.Redshift.EnableSnapshotCopy

    -- ** ModifySnapshotCopyRetentionPeriod
    , module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod

    -- ** DescribeClusterSnapshots (Paginated)
    , module Network.AWS.Redshift.DescribeClusterSnapshots
    -- $pager

    -- ** DescribeClusterSubnetGroups (Paginated)
    , module Network.AWS.Redshift.DescribeClusterSubnetGroups
    -- $pager

    -- ** AuthorizeSnapshotAccess
    , module Network.AWS.Redshift.AuthorizeSnapshotAccess

    -- ** CreateEventSubscription
    , module Network.AWS.Redshift.CreateEventSubscription

    -- ** RebootCluster
    , module Network.AWS.Redshift.RebootCluster

    -- ** DescribeOrderableClusterOptions (Paginated)
    , module Network.AWS.Redshift.DescribeOrderableClusterOptions
    -- $pager

    -- ** DeleteCluster
    , module Network.AWS.Redshift.DeleteCluster

    -- ** DeleteEventSubscription
    , module Network.AWS.Redshift.DeleteEventSubscription

    -- ** DescribeDefaultClusterParameters (Paginated)
    , module Network.AWS.Redshift.DescribeDefaultClusterParameters
    -- $pager

    -- ** CreateCluster
    , module Network.AWS.Redshift.CreateCluster

    -- ** CreateHSMClientCertificate
    , module Network.AWS.Redshift.CreateHSMClientCertificate

    -- ** ResetClusterParameterGroup
    , module Network.AWS.Redshift.ResetClusterParameterGroup

    -- ** DescribeEventSubscriptions (Paginated)
    , module Network.AWS.Redshift.DescribeEventSubscriptions
    -- $pager

    -- ** DescribeHSMClientCertificates (Paginated)
    , module Network.AWS.Redshift.DescribeHSMClientCertificates
    -- $pager

    -- ** ModifyClusterParameterGroup
    , module Network.AWS.Redshift.ModifyClusterParameterGroup

    -- ** RevokeClusterSecurityGroupIngress
    , module Network.AWS.Redshift.RevokeClusterSecurityGroupIngress

    -- ** AuthorizeClusterSecurityGroupIngress
    , module Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress

    -- ** CreateClusterSecurityGroup
    , module Network.AWS.Redshift.CreateClusterSecurityGroup

    -- ** DescribeResize
    , module Network.AWS.Redshift.DescribeResize

    -- ** DescribeEventCategories
    , module Network.AWS.Redshift.DescribeEventCategories

    -- ** DeleteHSMConfiguration
    , module Network.AWS.Redshift.DeleteHSMConfiguration

    -- ** DeleteClusterSecurityGroup
    , module Network.AWS.Redshift.DeleteClusterSecurityGroup

    -- ** CreateHSMConfiguration
    , module Network.AWS.Redshift.CreateHSMConfiguration

    -- ** ModifyCluster
    , module Network.AWS.Redshift.ModifyCluster

    -- ** CreateClusterSnapshot
    , module Network.AWS.Redshift.CreateClusterSnapshot

    -- ** DescribeLoggingStatus
    , module Network.AWS.Redshift.DescribeLoggingStatus

    -- ** DescribeClusterParameters (Paginated)
    , module Network.AWS.Redshift.DescribeClusterParameters
    -- $pager

    -- ** DisableSnapshotCopy
    , module Network.AWS.Redshift.DisableSnapshotCopy

    -- ** RestoreFromClusterSnapshot
    , module Network.AWS.Redshift.RestoreFromClusterSnapshot

    -- ** DescribeHSMConfigurations (Paginated)
    , module Network.AWS.Redshift.DescribeHSMConfigurations
    -- $pager

    -- ** CreateClusterParameterGroup
    , module Network.AWS.Redshift.CreateClusterParameterGroup

    -- ** RevokeSnapshotAccess
    , module Network.AWS.Redshift.RevokeSnapshotAccess

    -- ** DeleteHSMClientCertificate
    , module Network.AWS.Redshift.DeleteHSMClientCertificate

    -- ** CreateSnapshotCopyGrant
    , module Network.AWS.Redshift.CreateSnapshotCopyGrant

    -- ** CopyClusterSnapshot
    , module Network.AWS.Redshift.CopyClusterSnapshot

    -- ** DescribeClusterVersions (Paginated)
    , module Network.AWS.Redshift.DescribeClusterVersions
    -- $pager

    -- ** ModifyClusterSubnetGroup
    , module Network.AWS.Redshift.ModifyClusterSubnetGroup

    -- ** DeleteSnapshotCopyGrant
    , module Network.AWS.Redshift.DeleteSnapshotCopyGrant

    -- ** DescribeSnapshotCopyGrants
    , module Network.AWS.Redshift.DescribeSnapshotCopyGrants

    -- ** RotateEncryptionKey
    , module Network.AWS.Redshift.RotateEncryptionKey

    -- * Types

    -- ** ParameterApplyType
    , ParameterApplyType (..)

    -- ** SourceType
    , SourceType (..)

    -- ** AccountWithRestoreAccess
    , AccountWithRestoreAccess
    , accountWithRestoreAccess
    , awraAccountId

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- ** Cluster
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
    , cpgParameterGroupName
    , cpgTags

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
    , dcpParameters
    , dcpMarker
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
    , hcHSMIPAddress
    , hcTags

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
    , lsLastSuccessfulDeliveryTime
    , lsLastFailureTime
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
    , pAllowedValues
    , pDataType
    , pParameterName
    , pDescription

    -- ** PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvMasterUserPassword
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

    -- ** ReservedNodeOffering
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

    -- ** RestoreStatus
    , RestoreStatus
    , restoreStatus
    , rsEstimatedTimeToCompletionInSeconds
    , rsStatus
    , rsCurrentRestoreRateInMegaBytesPerSecond
    , rsProgressInMegaBytes
    , rsElapsedTimeInSeconds
    , rsSnapshotSizeInMegaBytes

    -- ** Snapshot
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

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TaggedResource
    , TaggedResource
    , taggedResource
    , trResourceType
    , trTag
    , trResourceName

    -- ** VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import           Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
import           Network.AWS.Redshift.AuthorizeSnapshotAccess
import           Network.AWS.Redshift.CopyClusterSnapshot
import           Network.AWS.Redshift.CreateCluster
import           Network.AWS.Redshift.CreateClusterParameterGroup
import           Network.AWS.Redshift.CreateClusterSecurityGroup
import           Network.AWS.Redshift.CreateClusterSnapshot
import           Network.AWS.Redshift.CreateClusterSubnetGroup
import           Network.AWS.Redshift.CreateEventSubscription
import           Network.AWS.Redshift.CreateHSMClientCertificate
import           Network.AWS.Redshift.CreateHSMConfiguration
import           Network.AWS.Redshift.CreateSnapshotCopyGrant
import           Network.AWS.Redshift.CreateTags
import           Network.AWS.Redshift.DeleteCluster
import           Network.AWS.Redshift.DeleteClusterParameterGroup
import           Network.AWS.Redshift.DeleteClusterSecurityGroup
import           Network.AWS.Redshift.DeleteClusterSnapshot
import           Network.AWS.Redshift.DeleteClusterSubnetGroup
import           Network.AWS.Redshift.DeleteEventSubscription
import           Network.AWS.Redshift.DeleteHSMClientCertificate
import           Network.AWS.Redshift.DeleteHSMConfiguration
import           Network.AWS.Redshift.DeleteSnapshotCopyGrant
import           Network.AWS.Redshift.DeleteTags
import           Network.AWS.Redshift.DescribeClusterParameterGroups
import           Network.AWS.Redshift.DescribeClusterParameters
import           Network.AWS.Redshift.DescribeClusters
import           Network.AWS.Redshift.DescribeClusterSecurityGroups
import           Network.AWS.Redshift.DescribeClusterSnapshots
import           Network.AWS.Redshift.DescribeClusterSubnetGroups
import           Network.AWS.Redshift.DescribeClusterVersions
import           Network.AWS.Redshift.DescribeDefaultClusterParameters
import           Network.AWS.Redshift.DescribeEventCategories
import           Network.AWS.Redshift.DescribeEvents
import           Network.AWS.Redshift.DescribeEventSubscriptions
import           Network.AWS.Redshift.DescribeHSMClientCertificates
import           Network.AWS.Redshift.DescribeHSMConfigurations
import           Network.AWS.Redshift.DescribeLoggingStatus
import           Network.AWS.Redshift.DescribeOrderableClusterOptions
import           Network.AWS.Redshift.DescribeReservedNodeOfferings
import           Network.AWS.Redshift.DescribeReservedNodes
import           Network.AWS.Redshift.DescribeResize
import           Network.AWS.Redshift.DescribeSnapshotCopyGrants
import           Network.AWS.Redshift.DescribeTags
import           Network.AWS.Redshift.DisableLogging
import           Network.AWS.Redshift.DisableSnapshotCopy
import           Network.AWS.Redshift.EnableLogging
import           Network.AWS.Redshift.EnableSnapshotCopy
import           Network.AWS.Redshift.ModifyCluster
import           Network.AWS.Redshift.ModifyClusterParameterGroup
import           Network.AWS.Redshift.ModifyClusterSubnetGroup
import           Network.AWS.Redshift.ModifyEventSubscription
import           Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
import           Network.AWS.Redshift.PurchaseReservedNodeOffering
import           Network.AWS.Redshift.RebootCluster
import           Network.AWS.Redshift.ResetClusterParameterGroup
import           Network.AWS.Redshift.RestoreFromClusterSnapshot
import           Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
import           Network.AWS.Redshift.RevokeSnapshotAccess
import           Network.AWS.Redshift.RotateEncryptionKey
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Waiters

{- $errors
Error matchers are intended to be used with the <http://hackage.haskell.org/package/lens lens>
library functions provided by the "Control.Exception.Lens" module. This allows
the user to catch (and rethrow) service specific errors returned by 'Redshift'.
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
Waiters poll by repeatedly send a request until some remote success condition
specified by the 'Wait' configuration is fulfilled. The 'Wait' configuration
specifies how many attempts should be made, in addition to delay and retry strategies.
-}

{- $pager
This operation can return paginated results.
-}
