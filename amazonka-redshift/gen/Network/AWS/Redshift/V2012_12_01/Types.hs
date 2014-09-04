{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.Types
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
module Network.AWS.Redshift.V2012_12_01.Types
    (
    -- * Service
      Redshift
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * SourceType
    , SourceType (..)

    -- * AccountWithRestoreAccess
    , AccountWithRestoreAccess (..)
    , awraAccountId

    -- * AvailabilityZone
    , AvailabilityZone (..)
    , azName

    -- * Cluster
    , Cluster (..)
    , crClusterIdentifier
    , crNodeType
    , crClusterStatus
    , crModifyStatus
    , crMasterUsername
    , crDBName
    , crEndpoint
    , crClusterCreateTime
    , crAutomatedSnapshotRetentionPeriod
    , crClusterSecurityGroups
    , crVpcSecurityGroups
    , crClusterParameterGroups
    , crClusterSubnetGroupName
    , crVpcId
    , crAvailabilityZone
    , crPreferredMaintenanceWindow
    , crPendingModifiedValues
    , crClusterVersion
    , crAllowVersionUpgrade
    , crNumberOfNodes
    , crPubliclyAccessible
    , crEncrypted
    , crRestoreStatus
    , crHsmStatus
    , crClusterSnapshotCopyStatus
    , crClusterPublicKey
    , crClusterNodes
    , crElasticIpStatus
    , crClusterRevisionNumber

    -- * ClusterNode
    , ClusterNode (..)
    , cnNodeRole
    , cnPrivateIPAddress
    , cnPublicIPAddress

    -- * ClusterParameterGroup
    , ClusterParameterGroup (..)
    , cpgParameterGroupName
    , cpgParameterGroupFamily
    , cpgDescription

    -- * ClusterParameterGroupStatus
    , ClusterParameterGroupStatus (..)
    , cpgsParameterGroupName
    , cpgsParameterApplyStatus

    -- * ClusterSecurityGroup
    , ClusterSecurityGroup (..)
    , csgClusterSecurityGroupName
    , csgDescription
    , csgEC2SecurityGroups
    , csgIPRanges

    -- * ClusterSecurityGroupMembership
    , ClusterSecurityGroupMembership (..)
    , csgmClusterSecurityGroupName
    , csgmStatus

    -- * ClusterSnapshotCopyStatus
    , ClusterSnapshotCopyStatus (..)
    , cscsDestinationRegion
    , cscsRetentionPeriod

    -- * ClusterSubnetGroup
    , ClusterSubnetGroup (..)
    , csiClusterSubnetGroupName
    , csiDescription
    , csiVpcId
    , csiSubnetGroupStatus
    , csiSubnets

    -- * ClusterVersion
    , ClusterVersion (..)
    , cvClusterVersion
    , cvClusterParameterGroupFamily
    , cvDescription

    -- * DefaultClusterParameters
    , DefaultClusterParameters (..)
    , dcpParameterGroupFamily
    , dcpMarker
    , dcpParameters

    -- * EC2SecurityGroup
    , EC2SecurityGroup (..)
    , ecsgStatus
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId

    -- * ElasticIpStatus
    , ElasticIpStatus (..)
    , eisElasticIp
    , eisStatus

    -- * Endpoint
    , Endpoint (..)
    , etAddress
    , etPort

    -- * Event
    , Event (..)
    , exSourceIdentifier
    , exSourceType
    , exMessage
    , exEventCategories
    , exSeverity
    , exDate
    , exEventId

    -- * EventCategoriesMap
    , EventCategoriesMap (..)
    , ecoSourceType
    , ecoEvents

    -- * EventInfoMap
    , EventInfoMap (..)
    , eimEventId
    , eimEventCategories
    , eimEventDescription
    , eimSeverity

    -- * EventSubscription
    , EventSubscription (..)
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
    , HsmClientCertificate (..)
    , hccHsmClientCertificateIdentifier
    , hccHsmClientCertificatePublicKey

    -- * HsmConfiguration
    , HsmConfiguration (..)
    , hcHsmConfigurationIdentifier
    , hcDescription
    , hcHsmIpAddress
    , hcHsmPartitionName

    -- * HsmStatus
    , HsmStatus (..)
    , hsHsmClientCertificateIdentifier
    , hsHsmConfigurationIdentifier
    , hsStatus

    -- * IPRange
    , IPRange (..)
    , iprStatus
    , iprCIDRIP

    -- * OrderableClusterOption
    , OrderableClusterOption (..)
    , ocoClusterVersion
    , ocoClusterType
    , ocoNodeType
    , ocoAvailabilityZones

    -- * Parameter
    , Parameter (..)
    , prParameterName
    , prParameterValue
    , prDescription
    , prSource
    , prDataType
    , prAllowedValues
    , prIsModifiable
    , prMinimumEngineVersion

    -- * PendingModifiedValues
    , PendingModifiedValues (..)
    , pmvMasterUserPassword
    , pmvNodeType
    , pmvNumberOfNodes
    , pmvClusterType
    , pmvClusterVersion
    , pmvAutomatedSnapshotRetentionPeriod
    , pmvClusterIdentifier

    -- * RecurringCharge
    , RecurringCharge (..)
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ReservedNode
    , ReservedNode (..)
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
    , ReservedNodeOffering (..)
    , rnoReservedNodeOfferingId
    , rnoNodeType
    , rnoDuration
    , rnoFixedPrice
    , rnoUsagePrice
    , rnoCurrencyCode
    , rnoOfferingType
    , rnoRecurringCharges

    -- * RestoreStatus
    , RestoreStatus (..)
    , rsStatus
    , rsCurrentRestoreRateInMegaBytesPerSecond
    , rsSnapshotSizeInMegaBytes
    , rsProgressInMegaBytes
    , rsElapsedTimeInSeconds
    , rsEstimatedTimeToCompletionInSeconds

    -- * Snapshot
    , Snapshot (..)
    , stSnapshotIdentifier
    , stClusterIdentifier
    , stSnapshotCreateTime
    , stStatus
    , stPort
    , stAvailabilityZone
    , stClusterCreateTime
    , stMasterUsername
    , stClusterVersion
    , stSnapshotType
    , stNodeType
    , stNumberOfNodes
    , stDBName
    , stVpcId
    , stEncrypted
    , stEncryptedWithHSM
    , stAccountsWithRestoreAccess
    , stOwnerAccount
    , stTotalBackupSizeInMegaBytes
    , stActualIncrementalBackupSizeInMegaBytes
    , stBackupProgressInMegaBytes
    , stCurrentBackupRateInMegaBytesPerSecond
    , stEstimatedSecondsToCompletion
    , stElapsedTimeInSeconds
    , stSourceRegion

    -- * Subnet
    , Subnet (..)
    , sssssssuSubnetIdentifier
    , sssssssuSubnetAvailabilityZone
    , sssssssuSubnetStatus

    -- * VpcSecurityGroupMembership
    , VpcSecurityGroupMembership (..)
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
    data Er Redshift
        = AccessToSnapshotDeniedFault
        | AuthorizationAlreadyExistsFault
        | AuthorizationNotFoundFault
        | AuthorizationQuotaExceededFault
        | BucketNotFoundFault
        | ClusterAlreadyExistsFault
        | ClusterNotFoundFault
        | ClusterParameterGroupAlreadyExistsFault
        | ClusterParameterGroupNotFoundFault
        | ClusterParameterGroupQuotaExceededFault
        | ClusterQuotaExceededFault
        | ClusterSecurityGroupAlreadyExistsFault
        | ClusterSecurityGroupNotFoundFault
        | ClusterSecurityGroupQuotaExceededFault
        | ClusterSnapshotAlreadyExistsFault
        | ClusterSnapshotNotFoundFault
        | ClusterSnapshotQuotaExceededFault
        | ClusterSubnetGroupAlreadyExistsFault
        | ClusterSubnetGroupNotFoundFault
        | ClusterSubnetGroupQuotaExceededFault
        | ClusterSubnetQuotaExceededFault
        | CopyToRegionDisabledFault
        | EventSubscriptionQuotaExceededFault
        | HsmClientCertificateAlreadyExistsFault
        | HsmClientCertificateNotFoundFault
        | HsmClientCertificateQuotaExceededFault
        | HsmConfigurationAlreadyExistsFault
        | HsmConfigurationNotFoundFault
        | HsmConfigurationQuotaExceededFault
        | IncompatibleOrderableOptions
        | InsufficientClusterCapacityFault
        | InsufficientS3BucketPolicyFault
        | InvalidClusterParameterGroupStateFault
        | InvalidClusterSecurityGroupStateFault
        | InvalidClusterSnapshotStateFault
        | InvalidClusterStateFault
        | InvalidClusterSubnetGroupStateFault
        | InvalidClusterSubnetStateFault
        | InvalidElasticIpFault
        | InvalidHsmClientCertificateStateFault
        | InvalidHsmConfigurationStateFault
        | InvalidRestoreFault
        | InvalidS3BucketNameFault
        | InvalidS3KeyPrefixFault
        | InvalidSubnet
        | InvalidSubscriptionStateFault
        | InvalidVPCNetworkStateFault
        | NumberOfNodesPerClusterLimitExceededFault
        | NumberOfNodesQuotaExceededFault
        | RedshiftClient HttpException
        | RedshiftSerializer String
        | RedshiftService String
        | ReservedNodeAlreadyExistsFault
        | ReservedNodeNotFoundFault
        | ReservedNodeOfferingNotFoundFault
        | ReservedNodeQuotaExceededFault
        | ResizeNotFoundFault
        | SNSInvalidTopicFault
        | SNSNoAuthorizationFault
        | SNSTopicArnNotFoundFault
        | SnapshotCopyAlreadyDisabledFault
        | SnapshotCopyAlreadyEnabledFault
        | SnapshotCopyDisabledFault
        | SourceNotFoundFault
        | SubnetAlreadyInUse
        | SubscriptionAlreadyExistFault
        | SubscriptionCategoryNotFoundFault
        | SubscriptionEventIdNotFoundFault
        | SubscriptionNotFoundFault
        | SubscriptionSeverityNotFoundFault
        | UnauthorizedOperation
        | UnknownSnapshotCopyRegionFault
        | UnsupportedOptionFault

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "redshift"
        , _svcVersion  = "2012-12-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er Redshift)
deriving instance Generic (Er Redshift)

instance AWSError (Er Redshift) where
    awsError = const "RedshiftError"

instance AWSServiceError (Er Redshift) where
    serviceError    = RedshiftService
    clientError     = RedshiftClient
    serializerError = RedshiftSerializer

instance Exception (Er Redshift)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://redshift.amazonaws.com/doc/2012-12-01/"
    }

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Constraints: If SourceType is supplied,
-- SourceIdentifier must also be provided. Specify cluster when
-- SourceIdentifier is a cluster identifier. Specify cluster-security-group
-- when SourceIdentifier is a cluster security group name. Specify
-- cluster-parameter-group when SourceIdentifier is a cluster parameter group
-- name. Specify cluster-snapshot when SourceIdentifier is a cluster snapshot
-- identifier.
data SourceType
    = SourceTypeCluster -- ^ cluster
    | SourceTypeClusterParameterGroup -- ^ cluster-parameter-group
    | SourceTypeClusterSecurityGroup -- ^ cluster-security-group
    | SourceTypeClusterSnapshot -- ^ cluster-snapshot
      deriving (Eq, Show, Generic)

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
      -- ^ The identifier of an AWS customer account authorized to restore a
      -- snapshot.
    } deriving (Show, Generic)

-- | The identifier of an AWS customer account authorized to restore a snapshot.
awraAccountId :: Lens' AccountWithRestoreAccess (Maybe Text)
awraAccountId f x =
    f (_awraAccountId x)
        <&> \y -> x { _awraAccountId = y }
{-# INLINE awraAccountId #-}

instance FromXML AccountWithRestoreAccess where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccountWithRestoreAccess"

instance ToQuery AccountWithRestoreAccess where
    toQuery = genericQuery def

-- | Describes an availability zone.
newtype AvailabilityZone = AvailabilityZone
    { _azName :: Maybe Text
      -- ^ The name of the availability zone.
    } deriving (Show, Generic)

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName f x =
    f (_azName x)
        <&> \y -> x { _azName = y }
{-# INLINE azName #-}

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone where
    toQuery = genericQuery def

-- | Describes a cluster.
data Cluster = Cluster
    { _crClusterIdentifier :: Maybe Text
      -- ^ The unique identifier of the cluster.
    , _crNodeType :: Maybe Text
      -- ^ The node type for the nodes in the cluster.
    , _crClusterStatus :: Maybe Text
      -- ^ The current state of this cluster. Possible values include
      -- available, creating, deleting, rebooting, renaming, and resizing.
    , _crModifyStatus :: Maybe Text
      -- ^ The status of a modify operation, if any, initiated for the
      -- cluster.
    , _crMasterUsername :: Maybe Text
      -- ^ The master user name for the cluster. This name is used to
      -- connect to the database that is specified in DBName.
    , _crDBName :: Maybe Text
      -- ^ The name of the initial database that was created when the
      -- cluster was created. This same name is returned for the life of
      -- the cluster. If an initial database was not specified, a database
      -- named "dev" was created by default.
    , _crEndpoint :: Maybe Endpoint
      -- ^ The connection endpoint.
    , _crClusterCreateTime :: Maybe ISO8601
      -- ^ The date and time that the cluster was created.
    , _crAutomatedSnapshotRetentionPeriod :: Maybe Integer
      -- ^ The number of days that automatic cluster snapshots are retained.
    , _crClusterSecurityGroups :: [ClusterSecurityGroupMembership]
      -- ^ A list of cluster security group that are associated with the
      -- cluster. Each security group is represented by an element that
      -- contains ClusterSecurityGroup.Name and
      -- ClusterSecurityGroup.Status subelements. Cluster security groups
      -- are used when the cluster is not created in a VPC. Clusters that
      -- are created in a VPC use VPC security groups, which are listed by
      -- the VpcSecurityGroups parameter.
    , _crVpcSecurityGroups :: [VpcSecurityGroupMembership]
      -- ^ A list of Virtual Private Cloud (VPC) security groups that are
      -- associated with the cluster. This parameter is returned only if
      -- the cluster is in a VPC.
    , _crClusterParameterGroups :: [ClusterParameterGroupStatus]
      -- ^ The list of cluster parameter groups that are associated with
      -- this cluster.
    , _crClusterSubnetGroupName :: Maybe Text
      -- ^ The name of the subnet group that is associated with the cluster.
      -- This parameter is valid only when the cluster is in a VPC.
    , _crVpcId :: Maybe Text
      -- ^ The identifier of the VPC the cluster is in, if the cluster is in
      -- a VPC.
    , _crAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the cluster is
      -- located.
    , _crPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance
      -- can occur.
    , _crPendingModifiedValues :: Maybe PendingModifiedValues
      -- ^ If present, changes to the cluster are pending. Specific pending
      -- changes are identified by subelements.
    , _crClusterVersion :: Maybe Text
      -- ^ The version ID of the Amazon Redshift engine that is running on
      -- the cluster.
    , _crAllowVersionUpgrade :: Maybe Bool
      -- ^ If true, version upgrades will be applied automatically to the
      -- cluster during the maintenance window.
    , _crNumberOfNodes :: Maybe Integer
      -- ^ The number of compute nodes in the cluster.
    , _crPubliclyAccessible :: Maybe Bool
      -- ^ If true, the cluster can be accessed from a public network.
    , _crEncrypted :: Maybe Bool
      -- ^ If true, data in the cluster is encrypted at rest.
    , _crRestoreStatus :: Maybe RestoreStatus
      -- ^ Describes the status of a cluster restore action. Returns null if
      -- the cluster was not created by restoring a snapshot.
    , _crHsmStatus :: Maybe HsmStatus
      -- ^ Reports whether the Amazon Redshift cluster has finished applying
      -- any HSM settings changes specified in a modify cluster command.
      -- Values: active, applying.
    , _crClusterSnapshotCopyStatus :: Maybe ClusterSnapshotCopyStatus
      -- ^ Returns the destination region and retention period that are
      -- configured for cross-region snapshot copy.
    , _crClusterPublicKey :: Maybe Text
      -- ^ The public key for the cluster.
    , _crClusterNodes :: [ClusterNode]
      -- ^ The nodes in a cluster.
    , _crElasticIpStatus :: Maybe ElasticIpStatus
      -- ^ Describes the status of the elastic IP (EIP) address.
    , _crClusterRevisionNumber :: Maybe Text
      -- ^ The specific revision number of the database in the cluster.
    } deriving (Show, Generic)

-- | The unique identifier of the cluster.
crClusterIdentifier :: Lens' Cluster (Maybe Text)
crClusterIdentifier f x =
    f (_crClusterIdentifier x)
        <&> \y -> x { _crClusterIdentifier = y }
{-# INLINE crClusterIdentifier #-}

-- | The node type for the nodes in the cluster.
crNodeType :: Lens' Cluster (Maybe Text)
crNodeType f x =
    f (_crNodeType x)
        <&> \y -> x { _crNodeType = y }
{-# INLINE crNodeType #-}

-- | The current state of this cluster. Possible values include available,
-- creating, deleting, rebooting, renaming, and resizing.
crClusterStatus :: Lens' Cluster (Maybe Text)
crClusterStatus f x =
    f (_crClusterStatus x)
        <&> \y -> x { _crClusterStatus = y }
{-# INLINE crClusterStatus #-}

-- | The status of a modify operation, if any, initiated for the cluster.
crModifyStatus :: Lens' Cluster (Maybe Text)
crModifyStatus f x =
    f (_crModifyStatus x)
        <&> \y -> x { _crModifyStatus = y }
{-# INLINE crModifyStatus #-}

-- | The master user name for the cluster. This name is used to connect to the
-- database that is specified in DBName.
crMasterUsername :: Lens' Cluster (Maybe Text)
crMasterUsername f x =
    f (_crMasterUsername x)
        <&> \y -> x { _crMasterUsername = y }
{-# INLINE crMasterUsername #-}

-- | The name of the initial database that was created when the cluster was
-- created. This same name is returned for the life of the cluster. If an
-- initial database was not specified, a database named "dev" was created by
-- default.
crDBName :: Lens' Cluster (Maybe Text)
crDBName f x =
    f (_crDBName x)
        <&> \y -> x { _crDBName = y }
{-# INLINE crDBName #-}

-- | The connection endpoint.
crEndpoint :: Lens' Cluster (Maybe Endpoint)
crEndpoint f x =
    f (_crEndpoint x)
        <&> \y -> x { _crEndpoint = y }
{-# INLINE crEndpoint #-}

-- | The date and time that the cluster was created.
crClusterCreateTime :: Lens' Cluster (Maybe ISO8601)
crClusterCreateTime f x =
    f (_crClusterCreateTime x)
        <&> \y -> x { _crClusterCreateTime = y }
{-# INLINE crClusterCreateTime #-}

-- | The number of days that automatic cluster snapshots are retained.
crAutomatedSnapshotRetentionPeriod :: Lens' Cluster (Maybe Integer)
crAutomatedSnapshotRetentionPeriod f x =
    f (_crAutomatedSnapshotRetentionPeriod x)
        <&> \y -> x { _crAutomatedSnapshotRetentionPeriod = y }
{-# INLINE crAutomatedSnapshotRetentionPeriod #-}

-- | A list of cluster security group that are associated with the cluster. Each
-- security group is represented by an element that contains
-- ClusterSecurityGroup.Name and ClusterSecurityGroup.Status subelements.
-- Cluster security groups are used when the cluster is not created in a VPC.
-- Clusters that are created in a VPC use VPC security groups, which are
-- listed by the VpcSecurityGroups parameter.
crClusterSecurityGroups :: Lens' Cluster ([ClusterSecurityGroupMembership])
crClusterSecurityGroups f x =
    f (_crClusterSecurityGroups x)
        <&> \y -> x { _crClusterSecurityGroups = y }
{-# INLINE crClusterSecurityGroups #-}

-- | A list of Virtual Private Cloud (VPC) security groups that are associated
-- with the cluster. This parameter is returned only if the cluster is in a
-- VPC.
crVpcSecurityGroups :: Lens' Cluster ([VpcSecurityGroupMembership])
crVpcSecurityGroups f x =
    f (_crVpcSecurityGroups x)
        <&> \y -> x { _crVpcSecurityGroups = y }
{-# INLINE crVpcSecurityGroups #-}

-- | The list of cluster parameter groups that are associated with this cluster.
crClusterParameterGroups :: Lens' Cluster ([ClusterParameterGroupStatus])
crClusterParameterGroups f x =
    f (_crClusterParameterGroups x)
        <&> \y -> x { _crClusterParameterGroups = y }
{-# INLINE crClusterParameterGroups #-}

-- | The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
crClusterSubnetGroupName :: Lens' Cluster (Maybe Text)
crClusterSubnetGroupName f x =
    f (_crClusterSubnetGroupName x)
        <&> \y -> x { _crClusterSubnetGroupName = y }
{-# INLINE crClusterSubnetGroupName #-}

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
crVpcId :: Lens' Cluster (Maybe Text)
crVpcId f x =
    f (_crVpcId x)
        <&> \y -> x { _crVpcId = y }
{-# INLINE crVpcId #-}

-- | The name of the Availability Zone in which the cluster is located.
crAvailabilityZone :: Lens' Cluster (Maybe Text)
crAvailabilityZone f x =
    f (_crAvailabilityZone x)
        <&> \y -> x { _crAvailabilityZone = y }
{-# INLINE crAvailabilityZone #-}

-- | The weekly time range (in UTC) during which system maintenance can occur.
crPreferredMaintenanceWindow :: Lens' Cluster (Maybe Text)
crPreferredMaintenanceWindow f x =
    f (_crPreferredMaintenanceWindow x)
        <&> \y -> x { _crPreferredMaintenanceWindow = y }
{-# INLINE crPreferredMaintenanceWindow #-}

-- | If present, changes to the cluster are pending. Specific pending changes
-- are identified by subelements.
crPendingModifiedValues :: Lens' Cluster (Maybe PendingModifiedValues)
crPendingModifiedValues f x =
    f (_crPendingModifiedValues x)
        <&> \y -> x { _crPendingModifiedValues = y }
{-# INLINE crPendingModifiedValues #-}

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
crClusterVersion :: Lens' Cluster (Maybe Text)
crClusterVersion f x =
    f (_crClusterVersion x)
        <&> \y -> x { _crClusterVersion = y }
{-# INLINE crClusterVersion #-}

-- | If true, version upgrades will be applied automatically to the cluster
-- during the maintenance window.
crAllowVersionUpgrade :: Lens' Cluster (Maybe Bool)
crAllowVersionUpgrade f x =
    f (_crAllowVersionUpgrade x)
        <&> \y -> x { _crAllowVersionUpgrade = y }
{-# INLINE crAllowVersionUpgrade #-}

-- | The number of compute nodes in the cluster.
crNumberOfNodes :: Lens' Cluster (Maybe Integer)
crNumberOfNodes f x =
    f (_crNumberOfNodes x)
        <&> \y -> x { _crNumberOfNodes = y }
{-# INLINE crNumberOfNodes #-}

-- | If true, the cluster can be accessed from a public network.
crPubliclyAccessible :: Lens' Cluster (Maybe Bool)
crPubliclyAccessible f x =
    f (_crPubliclyAccessible x)
        <&> \y -> x { _crPubliclyAccessible = y }
{-# INLINE crPubliclyAccessible #-}

-- | If true, data in the cluster is encrypted at rest.
crEncrypted :: Lens' Cluster (Maybe Bool)
crEncrypted f x =
    f (_crEncrypted x)
        <&> \y -> x { _crEncrypted = y }
{-# INLINE crEncrypted #-}

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
crRestoreStatus :: Lens' Cluster (Maybe RestoreStatus)
crRestoreStatus f x =
    f (_crRestoreStatus x)
        <&> \y -> x { _crRestoreStatus = y }
{-# INLINE crRestoreStatus #-}

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
crHsmStatus :: Lens' Cluster (Maybe HsmStatus)
crHsmStatus f x =
    f (_crHsmStatus x)
        <&> \y -> x { _crHsmStatus = y }
{-# INLINE crHsmStatus #-}

-- | Returns the destination region and retention period that are configured for
-- cross-region snapshot copy.
crClusterSnapshotCopyStatus :: Lens' Cluster (Maybe ClusterSnapshotCopyStatus)
crClusterSnapshotCopyStatus f x =
    f (_crClusterSnapshotCopyStatus x)
        <&> \y -> x { _crClusterSnapshotCopyStatus = y }
{-# INLINE crClusterSnapshotCopyStatus #-}

-- | The public key for the cluster.
crClusterPublicKey :: Lens' Cluster (Maybe Text)
crClusterPublicKey f x =
    f (_crClusterPublicKey x)
        <&> \y -> x { _crClusterPublicKey = y }
{-# INLINE crClusterPublicKey #-}

-- | The nodes in a cluster.
crClusterNodes :: Lens' Cluster ([ClusterNode])
crClusterNodes f x =
    f (_crClusterNodes x)
        <&> \y -> x { _crClusterNodes = y }
{-# INLINE crClusterNodes #-}

-- | Describes the status of the elastic IP (EIP) address.
crElasticIpStatus :: Lens' Cluster (Maybe ElasticIpStatus)
crElasticIpStatus f x =
    f (_crElasticIpStatus x)
        <&> \y -> x { _crElasticIpStatus = y }
{-# INLINE crElasticIpStatus #-}

-- | The specific revision number of the database in the cluster.
crClusterRevisionNumber :: Lens' Cluster (Maybe Text)
crClusterRevisionNumber f x =
    f (_crClusterRevisionNumber x)
        <&> \y -> x { _crClusterRevisionNumber = y }
{-# INLINE crClusterRevisionNumber #-}

instance FromXML Cluster where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Cluster"

-- | The identifier of a node in a cluster. --&gt;.
data ClusterNode = ClusterNode
    { _cnNodeRole :: Maybe Text
      -- ^ Whether the node is a leader node or a compute node.
    , _cnPrivateIPAddress :: Maybe Text
      -- ^ The private IP address of a node within a cluster.
    , _cnPublicIPAddress :: Maybe Text
      -- ^ The public IP address of a node within a cluster.
    } deriving (Show, Generic)

-- | Whether the node is a leader node or a compute node.
cnNodeRole :: Lens' ClusterNode (Maybe Text)
cnNodeRole f x =
    f (_cnNodeRole x)
        <&> \y -> x { _cnNodeRole = y }
{-# INLINE cnNodeRole #-}

-- | The private IP address of a node within a cluster.
cnPrivateIPAddress :: Lens' ClusterNode (Maybe Text)
cnPrivateIPAddress f x =
    f (_cnPrivateIPAddress x)
        <&> \y -> x { _cnPrivateIPAddress = y }
{-# INLINE cnPrivateIPAddress #-}

-- | The public IP address of a node within a cluster.
cnPublicIPAddress :: Lens' ClusterNode (Maybe Text)
cnPublicIPAddress f x =
    f (_cnPublicIPAddress x)
        <&> \y -> x { _cnPublicIPAddress = y }
{-# INLINE cnPublicIPAddress #-}

instance FromXML ClusterNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterNode"

instance ToQuery ClusterNode where
    toQuery = genericQuery def

-- | Describes a parameter group.
data ClusterParameterGroup = ClusterParameterGroup
    { _cpgParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group.
    , _cpgParameterGroupFamily :: Maybe Text
      -- ^ The name of the cluster parameter group family that this cluster
      -- parameter group is compatible with.
    , _cpgDescription :: Maybe Text
      -- ^ The description of the parameter group.
    } deriving (Show, Generic)

-- | The name of the cluster parameter group.
cpgParameterGroupName :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupName f x =
    f (_cpgParameterGroupName x)
        <&> \y -> x { _cpgParameterGroupName = y }
{-# INLINE cpgParameterGroupName #-}

-- | The name of the cluster parameter group family that this cluster parameter
-- group is compatible with.
cpgParameterGroupFamily :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupFamily f x =
    f (_cpgParameterGroupFamily x)
        <&> \y -> x { _cpgParameterGroupFamily = y }
{-# INLINE cpgParameterGroupFamily #-}

-- | The description of the parameter group.
cpgDescription :: Lens' ClusterParameterGroup (Maybe Text)
cpgDescription f x =
    f (_cpgDescription x)
        <&> \y -> x { _cpgDescription = y }
{-# INLINE cpgDescription #-}

instance FromXML ClusterParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterParameterGroup"

-- | Describes the status of a parameter group.
data ClusterParameterGroupStatus = ClusterParameterGroupStatus
    { _cpgsParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group.
    , _cpgsParameterApplyStatus :: Maybe Text
      -- ^ The status of parameter updates.
    } deriving (Show, Generic)

-- | The name of the cluster parameter group.
cpgsParameterGroupName :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterGroupName f x =
    f (_cpgsParameterGroupName x)
        <&> \y -> x { _cpgsParameterGroupName = y }
{-# INLINE cpgsParameterGroupName #-}

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus f x =
    f (_cpgsParameterApplyStatus x)
        <&> \y -> x { _cpgsParameterApplyStatus = y }
{-# INLINE cpgsParameterApplyStatus #-}

instance FromXML ClusterParameterGroupStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterParameterGroup"

instance ToQuery ClusterParameterGroupStatus where
    toQuery = genericQuery def

-- | Describes a security group.
data ClusterSecurityGroup = ClusterSecurityGroup
    { _csgClusterSecurityGroupName :: Maybe Text
      -- ^ The name of the cluster security group to which the operation was
      -- applied.
    , _csgDescription :: Maybe Text
      -- ^ A description of the security group.
    , _csgEC2SecurityGroups :: [EC2SecurityGroup]
      -- ^ A list of EC2 security groups that are permitted to access
      -- clusters associated with this cluster security group.
    , _csgIPRanges :: [IPRange]
      -- ^ A list of IP ranges (CIDR blocks) that are permitted to access
      -- clusters associated with this cluster security group.
    } deriving (Show, Generic)

-- | The name of the cluster security group to which the operation was applied.
csgClusterSecurityGroupName :: Lens' ClusterSecurityGroup (Maybe Text)
csgClusterSecurityGroupName f x =
    f (_csgClusterSecurityGroupName x)
        <&> \y -> x { _csgClusterSecurityGroupName = y }
{-# INLINE csgClusterSecurityGroupName #-}

-- | A description of the security group.
csgDescription :: Lens' ClusterSecurityGroup (Maybe Text)
csgDescription f x =
    f (_csgDescription x)
        <&> \y -> x { _csgDescription = y }
{-# INLINE csgDescription #-}

-- | A list of EC2 security groups that are permitted to access clusters
-- associated with this cluster security group.
csgEC2SecurityGroups :: Lens' ClusterSecurityGroup ([EC2SecurityGroup])
csgEC2SecurityGroups f x =
    f (_csgEC2SecurityGroups x)
        <&> \y -> x { _csgEC2SecurityGroups = y }
{-# INLINE csgEC2SecurityGroups #-}

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters
-- associated with this cluster security group.
csgIPRanges :: Lens' ClusterSecurityGroup ([IPRange])
csgIPRanges f x =
    f (_csgIPRanges x)
        <&> \y -> x { _csgIPRanges = y }
{-# INLINE csgIPRanges #-}

instance FromXML ClusterSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSecurityGroup"

-- | Describes a security group.
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership
    { _csgmClusterSecurityGroupName :: Maybe Text
      -- ^ The name of the cluster security group.
    , _csgmStatus :: Maybe Text
      -- ^ The status of the cluster security group.
    } deriving (Show, Generic)

-- | The name of the cluster security group.
csgmClusterSecurityGroupName :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmClusterSecurityGroupName f x =
    f (_csgmClusterSecurityGroupName x)
        <&> \y -> x { _csgmClusterSecurityGroupName = y }
{-# INLINE csgmClusterSecurityGroupName #-}

-- | The status of the cluster security group.
csgmStatus :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmStatus f x =
    f (_csgmStatus x)
        <&> \y -> x { _csgmStatus = y }
{-# INLINE csgmStatus #-}

instance FromXML ClusterSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSecurityGroup"

instance ToQuery ClusterSecurityGroupMembership where
    toQuery = genericQuery def

-- | Returns the destination region and retention period that are configured for
-- cross-region snapshot copy.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus
    { _cscsDestinationRegion :: Maybe Text
      -- ^ The destination region that snapshots are automatically copied to
      -- when cross-region snapshot copy is enabled.
    , _cscsRetentionPeriod :: Maybe Integer
      -- ^ The number of days that automated snapshots are retained in the
      -- destination region after they are copied from a source region.
    } deriving (Show, Generic)

-- | The destination region that snapshots are automatically copied to when
-- cross-region snapshot copy is enabled.
cscsDestinationRegion :: Lens' ClusterSnapshotCopyStatus (Maybe Text)
cscsDestinationRegion f x =
    f (_cscsDestinationRegion x)
        <&> \y -> x { _cscsDestinationRegion = y }
{-# INLINE cscsDestinationRegion #-}

-- | The number of days that automated snapshots are retained in the destination
-- region after they are copied from a source region.
cscsRetentionPeriod :: Lens' ClusterSnapshotCopyStatus (Maybe Integer)
cscsRetentionPeriod f x =
    f (_cscsRetentionPeriod x)
        <&> \y -> x { _cscsRetentionPeriod = y }
{-# INLINE cscsRetentionPeriod #-}

instance FromXML ClusterSnapshotCopyStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSnapshotCopyStatus"

instance ToQuery ClusterSnapshotCopyStatus where
    toQuery = genericQuery def

-- | Describes a subnet group.
data ClusterSubnetGroup = ClusterSubnetGroup
    { _csiClusterSubnetGroupName :: Maybe Text
      -- ^ The name of the cluster subnet group.
    , _csiDescription :: Maybe Text
      -- ^ The description of the cluster subnet group.
    , _csiVpcId :: Maybe Text
      -- ^ The VPC ID of the cluster subnet group.
    , _csiSubnetGroupStatus :: Maybe Text
      -- ^ The status of the cluster subnet group. Possible values are
      -- Complete, Incomplete and Invalid.
    , _csiSubnets :: [Subnet]
      -- ^ A list of the VPC Subnet elements.
    } deriving (Show, Generic)

-- | The name of the cluster subnet group.
csiClusterSubnetGroupName :: Lens' ClusterSubnetGroup (Maybe Text)
csiClusterSubnetGroupName f x =
    f (_csiClusterSubnetGroupName x)
        <&> \y -> x { _csiClusterSubnetGroupName = y }
{-# INLINE csiClusterSubnetGroupName #-}

-- | The description of the cluster subnet group.
csiDescription :: Lens' ClusterSubnetGroup (Maybe Text)
csiDescription f x =
    f (_csiDescription x)
        <&> \y -> x { _csiDescription = y }
{-# INLINE csiDescription #-}

-- | The VPC ID of the cluster subnet group.
csiVpcId :: Lens' ClusterSubnetGroup (Maybe Text)
csiVpcId f x =
    f (_csiVpcId x)
        <&> \y -> x { _csiVpcId = y }
{-# INLINE csiVpcId #-}

-- | The status of the cluster subnet group. Possible values are Complete,
-- Incomplete and Invalid.
csiSubnetGroupStatus :: Lens' ClusterSubnetGroup (Maybe Text)
csiSubnetGroupStatus f x =
    f (_csiSubnetGroupStatus x)
        <&> \y -> x { _csiSubnetGroupStatus = y }
{-# INLINE csiSubnetGroupStatus #-}

-- | A list of the VPC Subnet elements.
csiSubnets :: Lens' ClusterSubnetGroup ([Subnet])
csiSubnets f x =
    f (_csiSubnets x)
        <&> \y -> x { _csiSubnets = y }
{-# INLINE csiSubnets #-}

instance FromXML ClusterSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSubnetGroup"

-- | Describes a cluster version, including the parameter group family and
-- description of the version.
data ClusterVersion = ClusterVersion
    { _cvClusterVersion :: Maybe Text
      -- ^ The version number used by the cluster.
    , _cvClusterParameterGroupFamily :: Maybe Text
      -- ^ The name of the cluster parameter group family for the cluster.
    , _cvDescription :: Maybe Text
      -- ^ The description of the cluster version.
    } deriving (Show, Generic)

-- | The version number used by the cluster.
cvClusterVersion :: Lens' ClusterVersion (Maybe Text)
cvClusterVersion f x =
    f (_cvClusterVersion x)
        <&> \y -> x { _cvClusterVersion = y }
{-# INLINE cvClusterVersion #-}

-- | The name of the cluster parameter group family for the cluster.
cvClusterParameterGroupFamily :: Lens' ClusterVersion (Maybe Text)
cvClusterParameterGroupFamily f x =
    f (_cvClusterParameterGroupFamily x)
        <&> \y -> x { _cvClusterParameterGroupFamily = y }
{-# INLINE cvClusterParameterGroupFamily #-}

-- | The description of the cluster version.
cvDescription :: Lens' ClusterVersion (Maybe Text)
cvDescription f x =
    f (_cvDescription x)
        <&> \y -> x { _cvDescription = y }
{-# INLINE cvDescription #-}

instance FromXML ClusterVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterVersion"

-- | Describes the default cluster parameters for a parameter group family.
data DefaultClusterParameters = DefaultClusterParameters
    { _dcpParameterGroupFamily :: Maybe Text
      -- ^ The name of the cluster parameter group family to which the
      -- engine default parameters apply.
    , _dcpMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    , _dcpParameters :: [Parameter]
      -- ^ The list of cluster default parameters.
    } deriving (Show, Generic)

-- | The name of the cluster parameter group family to which the engine default
-- parameters apply.
dcpParameterGroupFamily :: Lens' DefaultClusterParameters (Maybe Text)
dcpParameterGroupFamily f x =
    f (_dcpParameterGroupFamily x)
        <&> \y -> x { _dcpParameterGroupFamily = y }
{-# INLINE dcpParameterGroupFamily #-}

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dcpMarker :: Lens' DefaultClusterParameters (Maybe Text)
dcpMarker f x =
    f (_dcpMarker x)
        <&> \y -> x { _dcpMarker = y }
{-# INLINE dcpMarker #-}

-- | The list of cluster default parameters.
dcpParameters :: Lens' DefaultClusterParameters ([Parameter])
dcpParameters f x =
    f (_dcpParameters x)
        <&> \y -> x { _dcpParameters = y }
{-# INLINE dcpParameters #-}

instance FromXML DefaultClusterParameters where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DefaultClusterParameters"

-- | Describes an Amazon EC2 security group.
data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus :: Maybe Text
      -- ^ The status of the EC2 security group.
    , _ecsgEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the EC2 Security Group.
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS ID of the owner of the EC2 security group specified in
      -- the EC2SecurityGroupName field.
    } deriving (Show, Generic)

-- | The status of the EC2 security group.
ecsgStatus :: Lens' EC2SecurityGroup (Maybe Text)
ecsgStatus f x =
    f (_ecsgStatus x)
        <&> \y -> x { _ecsgStatus = y }
{-# INLINE ecsgStatus #-}

-- | The name of the EC2 Security Group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName f x =
    f (_ecsgEC2SecurityGroupName x)
        <&> \y -> x { _ecsgEC2SecurityGroupName = y }
{-# INLINE ecsgEC2SecurityGroupName #-}

-- | The AWS ID of the owner of the EC2 security group specified in the
-- EC2SecurityGroupName field.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId f x =
    f (_ecsgEC2SecurityGroupOwnerId x)
        <&> \y -> x { _ecsgEC2SecurityGroupOwnerId = y }
{-# INLINE ecsgEC2SecurityGroupOwnerId #-}

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup where
    toQuery = genericQuery def

-- | Describes the status of the elastic IP (EIP) address.
data ElasticIpStatus = ElasticIpStatus
    { _eisElasticIp :: Maybe Text
      -- ^ The elastic IP (EIP) address for the cluster.
    , _eisStatus :: Maybe Text
      -- ^ Describes the status of the elastic IP (EIP) address.
    } deriving (Show, Generic)

-- | The elastic IP (EIP) address for the cluster.
eisElasticIp :: Lens' ElasticIpStatus (Maybe Text)
eisElasticIp f x =
    f (_eisElasticIp x)
        <&> \y -> x { _eisElasticIp = y }
{-# INLINE eisElasticIp #-}

-- | Describes the status of the elastic IP (EIP) address.
eisStatus :: Lens' ElasticIpStatus (Maybe Text)
eisStatus f x =
    f (_eisStatus x)
        <&> \y -> x { _eisStatus = y }
{-# INLINE eisStatus #-}

instance FromXML ElasticIpStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ElasticIpStatus"

instance ToQuery ElasticIpStatus where
    toQuery = genericQuery def

-- | The connection endpoint.
data Endpoint = Endpoint
    { _etAddress :: Maybe Text
      -- ^ The DNS address of the Cluster.
    , _etPort :: Maybe Integer
      -- ^ The port that the database engine is listening on.
    } deriving (Show, Generic)

-- | The DNS address of the Cluster.
etAddress :: Lens' Endpoint (Maybe Text)
etAddress f x =
    f (_etAddress x)
        <&> \y -> x { _etAddress = y }
{-# INLINE etAddress #-}

-- | The port that the database engine is listening on.
etPort :: Lens' Endpoint (Maybe Integer)
etPort f x =
    f (_etPort x)
        <&> \y -> x { _etPort = y }
{-# INLINE etPort #-}

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint where
    toQuery = genericQuery def

-- | Describes an event.
data Event = Event
    { _exSourceIdentifier :: Maybe Text
      -- ^ The identifier for the source of the event.
    , _exSourceType :: Maybe SourceType
      -- ^ The source type for this event.
    , _exMessage :: Maybe Text
      -- ^ The text of this event.
    , _exEventCategories :: [Text]
      -- ^ A list of the event categories.
    , _exSeverity :: Maybe Text
      -- ^ The severity of the event. Values: ERROR, INFO.
    , _exDate :: Maybe ISO8601
      -- ^ The date and time of the event.
    , _exEventId :: Maybe Text
      -- ^ The identifier of the event.
    } deriving (Show, Generic)

-- | The identifier for the source of the event.
exSourceIdentifier :: Lens' Event (Maybe Text)
exSourceIdentifier f x =
    f (_exSourceIdentifier x)
        <&> \y -> x { _exSourceIdentifier = y }
{-# INLINE exSourceIdentifier #-}

-- | The source type for this event.
exSourceType :: Lens' Event (Maybe SourceType)
exSourceType f x =
    f (_exSourceType x)
        <&> \y -> x { _exSourceType = y }
{-# INLINE exSourceType #-}

-- | The text of this event.
exMessage :: Lens' Event (Maybe Text)
exMessage f x =
    f (_exMessage x)
        <&> \y -> x { _exMessage = y }
{-# INLINE exMessage #-}

-- | A list of the event categories.
exEventCategories :: Lens' Event ([Text])
exEventCategories f x =
    f (_exEventCategories x)
        <&> \y -> x { _exEventCategories = y }
{-# INLINE exEventCategories #-}

-- | The severity of the event. Values: ERROR, INFO.
exSeverity :: Lens' Event (Maybe Text)
exSeverity f x =
    f (_exSeverity x)
        <&> \y -> x { _exSeverity = y }
{-# INLINE exSeverity #-}

-- | The date and time of the event.
exDate :: Lens' Event (Maybe ISO8601)
exDate f x =
    f (_exDate x)
        <&> \y -> x { _exDate = y }
{-# INLINE exDate #-}

-- | The identifier of the event.
exEventId :: Lens' Event (Maybe Text)
exEventId f x =
    f (_exEventId x)
        <&> \y -> x { _exEventId = y }
{-# INLINE exEventId #-}

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

-- | 
data EventCategoriesMap = EventCategoriesMap
    { _ecoSourceType :: Maybe Text
      -- ^ The Amazon Redshift source type, such as cluster or
      -- cluster-snapshot, that the returned categories belong to.
    , _ecoEvents :: [EventInfoMap]
      -- ^ The events in the event category.
    } deriving (Show, Generic)

-- | The Amazon Redshift source type, such as cluster or cluster-snapshot, that
-- the returned categories belong to.
ecoSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecoSourceType f x =
    f (_ecoSourceType x)
        <&> \y -> x { _ecoSourceType = y }
{-# INLINE ecoSourceType #-}

-- | The events in the event category.
ecoEvents :: Lens' EventCategoriesMap ([EventInfoMap])
ecoEvents f x =
    f (_ecoEvents x)
        <&> \y -> x { _ecoEvents = y }
{-# INLINE ecoEvents #-}

instance FromXML EventCategoriesMap where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventCategoriesMap"

-- | 
data EventInfoMap = EventInfoMap
    { _eimEventId :: Maybe Text
      -- ^ The identifier of an Amazon Redshift event.
    , _eimEventCategories :: [Text]
      -- ^ The category of an Amazon Redshift event.
    , _eimEventDescription :: Maybe Text
      -- ^ The description of an Amazon Redshift event.
    , _eimSeverity :: Maybe Text
      -- ^ The severity of the event. Values: ERROR, INFO.
    } deriving (Show, Generic)

-- | The identifier of an Amazon Redshift event.
eimEventId :: Lens' EventInfoMap (Maybe Text)
eimEventId f x =
    f (_eimEventId x)
        <&> \y -> x { _eimEventId = y }
{-# INLINE eimEventId #-}

-- | The category of an Amazon Redshift event.
eimEventCategories :: Lens' EventInfoMap ([Text])
eimEventCategories f x =
    f (_eimEventCategories x)
        <&> \y -> x { _eimEventCategories = y }
{-# INLINE eimEventCategories #-}

-- | The description of an Amazon Redshift event.
eimEventDescription :: Lens' EventInfoMap (Maybe Text)
eimEventDescription f x =
    f (_eimEventDescription x)
        <&> \y -> x { _eimEventDescription = y }
{-# INLINE eimEventDescription #-}

-- | The severity of the event. Values: ERROR, INFO.
eimSeverity :: Lens' EventInfoMap (Maybe Text)
eimSeverity f x =
    f (_eimSeverity x)
        <&> \y -> x { _eimSeverity = y }
{-# INLINE eimSeverity #-}

instance FromXML EventInfoMap where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventInfoMap"

instance ToQuery EventInfoMap where
    toQuery = genericQuery def

-- | 
data EventSubscription = EventSubscription
    { _esCustomerAwsId :: Maybe Text
      -- ^ The AWS customer account associated with the Amazon Redshift
      -- event notification subscription.
    , _esCustSubscriptionId :: Maybe Text
      -- ^ The name of the Amazon Redshift event notification subscription.
    , _esSnsTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic used by
      -- the event notification subscription.
    , _esStatus :: Maybe Text
      -- ^ The status of the Amazon Redshift event notification
      -- subscription. Constraints: Can be one of the following: active |
      -- no-permission | topic-not-exist The status "no-permission"
      -- indicates that Amazon Redshift no longer has permission to post
      -- to the Amazon SNS topic. The status "topic-not-exist" indicates
      -- that the topic was deleted after the subscription was created.
    , _esSubscriptionCreationTime :: Maybe ISO8601
      -- ^ The date and time the Amazon Redshift event notification
      -- subscription was created.
    , _esSourceType :: Maybe Text
      -- ^ The source type of the events returned the Amazon Redshift event
      -- notification, such as cluster, or cluster-snapshot.
    , _esSourceIdsList :: [Text]
      -- ^ A list of the sources that publish events to the Amazon Redshift
      -- event notification subscription.
    , _esEventCategoriesList :: [Text]
      -- ^ The list of Amazon Redshift event categories specified in the
      -- event notification subscription. Values: Configuration,
      -- Management, Monitoring, Security.
    , _esSeverity :: Maybe Text
      -- ^ The event severity specified in the Amazon Redshift event
      -- notification subscription. Values: ERROR, INFO.
    , _esEnabled :: Maybe Bool
      -- ^ A Boolean value indicating whether the subscription is enabled.
      -- true indicates the subscription is enabled.
    } deriving (Show, Generic)

-- | The AWS customer account associated with the Amazon Redshift event
-- notification subscription.
esCustomerAwsId :: Lens' EventSubscription (Maybe Text)
esCustomerAwsId f x =
    f (_esCustomerAwsId x)
        <&> \y -> x { _esCustomerAwsId = y }
{-# INLINE esCustomerAwsId #-}

-- | The name of the Amazon Redshift event notification subscription.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId f x =
    f (_esCustSubscriptionId x)
        <&> \y -> x { _esCustSubscriptionId = y }
{-# INLINE esCustSubscriptionId #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
esSnsTopicArn :: Lens' EventSubscription (Maybe Text)
esSnsTopicArn f x =
    f (_esSnsTopicArn x)
        <&> \y -> x { _esSnsTopicArn = y }
{-# INLINE esSnsTopicArn #-}

-- | The status of the Amazon Redshift event notification subscription.
-- Constraints: Can be one of the following: active | no-permission |
-- topic-not-exist The status "no-permission" indicates that Amazon Redshift
-- no longer has permission to post to the Amazon SNS topic. The status
-- "topic-not-exist" indicates that the topic was deleted after the
-- subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus f x =
    f (_esStatus x)
        <&> \y -> x { _esStatus = y }
{-# INLINE esStatus #-}

-- | The date and time the Amazon Redshift event notification subscription was
-- created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe ISO8601)
esSubscriptionCreationTime f x =
    f (_esSubscriptionCreationTime x)
        <&> \y -> x { _esSubscriptionCreationTime = y }
{-# INLINE esSubscriptionCreationTime #-}

-- | The source type of the events returned the Amazon Redshift event
-- notification, such as cluster, or cluster-snapshot.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType f x =
    f (_esSourceType x)
        <&> \y -> x { _esSourceType = y }
{-# INLINE esSourceType #-}

-- | A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
esSourceIdsList :: Lens' EventSubscription ([Text])
esSourceIdsList f x =
    f (_esSourceIdsList x)
        <&> \y -> x { _esSourceIdsList = y }
{-# INLINE esSourceIdsList #-}

-- | The list of Amazon Redshift event categories specified in the event
-- notification subscription. Values: Configuration, Management, Monitoring,
-- Security.
esEventCategoriesList :: Lens' EventSubscription ([Text])
esEventCategoriesList f x =
    f (_esEventCategoriesList x)
        <&> \y -> x { _esEventCategoriesList = y }
{-# INLINE esEventCategoriesList #-}

-- | The event severity specified in the Amazon Redshift event notification
-- subscription. Values: ERROR, INFO.
esSeverity :: Lens' EventSubscription (Maybe Text)
esSeverity f x =
    f (_esSeverity x)
        <&> \y -> x { _esSeverity = y }
{-# INLINE esSeverity #-}

-- | A Boolean value indicating whether the subscription is enabled. true
-- indicates the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled f x =
    f (_esEnabled x)
        <&> \y -> x { _esEnabled = y }
{-# INLINE esEnabled #-}

instance FromXML EventSubscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventSubscription"

-- | Returns information about an HSM client certificate. The certificate is
-- stored in a secure Hardware Storage Module (HSM), and used by the Amazon
-- Redshift cluster to encrypt data files.
data HsmClientCertificate = HsmClientCertificate
    { _hccHsmClientCertificateIdentifier :: Maybe Text
      -- ^ The identifier of the HSM client certificate.
    , _hccHsmClientCertificatePublicKey :: Maybe Text
      -- ^ The public key that the Amazon Redshift cluster will use to
      -- connect to the HSM. You must register the public key in the HSM.
    } deriving (Show, Generic)

-- | The identifier of the HSM client certificate.
hccHsmClientCertificateIdentifier :: Lens' HsmClientCertificate (Maybe Text)
hccHsmClientCertificateIdentifier f x =
    f (_hccHsmClientCertificateIdentifier x)
        <&> \y -> x { _hccHsmClientCertificateIdentifier = y }
{-# INLINE hccHsmClientCertificateIdentifier #-}

-- | The public key that the Amazon Redshift cluster will use to connect to the
-- HSM. You must register the public key in the HSM.
hccHsmClientCertificatePublicKey :: Lens' HsmClientCertificate (Maybe Text)
hccHsmClientCertificatePublicKey f x =
    f (_hccHsmClientCertificatePublicKey x)
        <&> \y -> x { _hccHsmClientCertificatePublicKey = y }
{-# INLINE hccHsmClientCertificatePublicKey #-}

instance FromXML HsmClientCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HsmClientCertificate"

-- | Returns information about an HSM configuration, which is an object that
-- describes to Amazon Redshift clusters the information they require to
-- connect to an HSM where they can store database encryption keys.
data HsmConfiguration = HsmConfiguration
    { _hcHsmConfigurationIdentifier :: Maybe Text
      -- ^ The name of the Amazon Redshift HSM configuration.
    , _hcDescription :: Maybe Text
      -- ^ A text description of the HSM configuration.
    , _hcHsmIpAddress :: Maybe Text
      -- ^ The IP address that the Amazon Redshift cluster must use to
      -- access the HSM.
    , _hcHsmPartitionName :: Maybe Text
      -- ^ The name of the partition in the HSM where the Amazon Redshift
      -- clusters will store their database encryption keys.
    } deriving (Show, Generic)

-- | The name of the Amazon Redshift HSM configuration.
hcHsmConfigurationIdentifier :: Lens' HsmConfiguration (Maybe Text)
hcHsmConfigurationIdentifier f x =
    f (_hcHsmConfigurationIdentifier x)
        <&> \y -> x { _hcHsmConfigurationIdentifier = y }
{-# INLINE hcHsmConfigurationIdentifier #-}

-- | A text description of the HSM configuration.
hcDescription :: Lens' HsmConfiguration (Maybe Text)
hcDescription f x =
    f (_hcDescription x)
        <&> \y -> x { _hcDescription = y }
{-# INLINE hcDescription #-}

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
hcHsmIpAddress :: Lens' HsmConfiguration (Maybe Text)
hcHsmIpAddress f x =
    f (_hcHsmIpAddress x)
        <&> \y -> x { _hcHsmIpAddress = y }
{-# INLINE hcHsmIpAddress #-}

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
hcHsmPartitionName :: Lens' HsmConfiguration (Maybe Text)
hcHsmPartitionName f x =
    f (_hcHsmPartitionName x)
        <&> \y -> x { _hcHsmPartitionName = y }
{-# INLINE hcHsmPartitionName #-}

instance FromXML HsmConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HsmConfiguration"

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
data HsmStatus = HsmStatus
    { _hsHsmClientCertificateIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM client certificate the Amazon
      -- Redshift cluster uses to retrieve the data encryption keys stored
      -- in an HSM.
    , _hsHsmConfigurationIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM configuration that contains the
      -- information the Amazon Redshift cluster can use to retrieve and
      -- store keys in an HSM.
    , _hsStatus :: Maybe Text
      -- ^ Reports whether the Amazon Redshift cluster has finished applying
      -- any HSM settings changes specified in a modify cluster command.
      -- Values: active, applying.
    } deriving (Show, Generic)

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
hsHsmClientCertificateIdentifier :: Lens' HsmStatus (Maybe Text)
hsHsmClientCertificateIdentifier f x =
    f (_hsHsmClientCertificateIdentifier x)
        <&> \y -> x { _hsHsmClientCertificateIdentifier = y }
{-# INLINE hsHsmClientCertificateIdentifier #-}

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
hsHsmConfigurationIdentifier :: Lens' HsmStatus (Maybe Text)
hsHsmConfigurationIdentifier f x =
    f (_hsHsmConfigurationIdentifier x)
        <&> \y -> x { _hsHsmConfigurationIdentifier = y }
{-# INLINE hsHsmConfigurationIdentifier #-}

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
hsStatus :: Lens' HsmStatus (Maybe Text)
hsStatus f x =
    f (_hsStatus x)
        <&> \y -> x { _hsStatus = y }
{-# INLINE hsStatus #-}

instance FromXML HsmStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HsmStatus"

instance ToQuery HsmStatus where
    toQuery = genericQuery def

-- | Describes an IP range used in a security group.
data IPRange = IPRange
    { _iprStatus :: Maybe Text
      -- ^ The status of the IP range, for example, "authorized".
    , _iprCIDRIP :: Maybe Text
      -- ^ The IP range in Classless Inter-Domain Routing (CIDR) notation.
    } deriving (Show, Generic)

-- | The status of the IP range, for example, "authorized".
iprStatus :: Lens' IPRange (Maybe Text)
iprStatus f x =
    f (_iprStatus x)
        <&> \y -> x { _iprStatus = y }
{-# INLINE iprStatus #-}

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
iprCIDRIP :: Lens' IPRange (Maybe Text)
iprCIDRIP f x =
    f (_iprCIDRIP x)
        <&> \y -> x { _iprCIDRIP = y }
{-# INLINE iprCIDRIP #-}

instance FromXML IPRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IPRange"

instance ToQuery IPRange where
    toQuery = genericQuery def

-- | Describes an orderable cluster option.
data OrderableClusterOption = OrderableClusterOption
    { _ocoClusterVersion :: Maybe Text
      -- ^ The version of the orderable cluster.
    , _ocoClusterType :: Maybe Text
      -- ^ The cluster type, for example multi-node.
    , _ocoNodeType :: Maybe Text
      -- ^ The node type for the orderable cluster.
    , _ocoAvailabilityZones :: [AvailabilityZone]
      -- ^ A list of availability zones for the orderable cluster.
    } deriving (Show, Generic)

-- | The version of the orderable cluster.
ocoClusterVersion :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterVersion f x =
    f (_ocoClusterVersion x)
        <&> \y -> x { _ocoClusterVersion = y }
{-# INLINE ocoClusterVersion #-}

-- | The cluster type, for example multi-node.
ocoClusterType :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterType f x =
    f (_ocoClusterType x)
        <&> \y -> x { _ocoClusterType = y }
{-# INLINE ocoClusterType #-}

-- | The node type for the orderable cluster.
ocoNodeType :: Lens' OrderableClusterOption (Maybe Text)
ocoNodeType f x =
    f (_ocoNodeType x)
        <&> \y -> x { _ocoNodeType = y }
{-# INLINE ocoNodeType #-}

-- | A list of availability zones for the orderable cluster.
ocoAvailabilityZones :: Lens' OrderableClusterOption ([AvailabilityZone])
ocoAvailabilityZones f x =
    f (_ocoAvailabilityZones x)
        <&> \y -> x { _ocoAvailabilityZones = y }
{-# INLINE ocoAvailabilityZones #-}

instance FromXML OrderableClusterOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OrderableClusterOption"

-- | Describes a parameter in a cluster parameter group.
data Parameter = Parameter
    { _prParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , _prParameterValue :: Maybe Text
      -- ^ The value of the parameter.
    , _prDescription :: Maybe Text
      -- ^ A description of the parameter.
    , _prSource :: Maybe Text
      -- ^ The source of the parameter value, such as "engine-default" or
      -- "user".
    , _prDataType :: Maybe Text
      -- ^ The data type of the parameter.
    , _prAllowedValues :: Maybe Text
      -- ^ The valid range of values for the parameter.
    , _prIsModifiable :: Maybe Bool
      -- ^ If true, the parameter can be modified. Some parameters have
      -- security or operational implications that prevent them from being
      -- changed.
    , _prMinimumEngineVersion :: Maybe Text
      -- ^ The earliest engine version to which the parameter can apply.
    } deriving (Show, Generic)

-- | The name of the parameter.
prParameterName :: Lens' Parameter (Maybe Text)
prParameterName f x =
    f (_prParameterName x)
        <&> \y -> x { _prParameterName = y }
{-# INLINE prParameterName #-}

-- | The value of the parameter.
prParameterValue :: Lens' Parameter (Maybe Text)
prParameterValue f x =
    f (_prParameterValue x)
        <&> \y -> x { _prParameterValue = y }
{-# INLINE prParameterValue #-}

-- | A description of the parameter.
prDescription :: Lens' Parameter (Maybe Text)
prDescription f x =
    f (_prDescription x)
        <&> \y -> x { _prDescription = y }
{-# INLINE prDescription #-}

-- | The source of the parameter value, such as "engine-default" or "user".
prSource :: Lens' Parameter (Maybe Text)
prSource f x =
    f (_prSource x)
        <&> \y -> x { _prSource = y }
{-# INLINE prSource #-}

-- | The data type of the parameter.
prDataType :: Lens' Parameter (Maybe Text)
prDataType f x =
    f (_prDataType x)
        <&> \y -> x { _prDataType = y }
{-# INLINE prDataType #-}

-- | The valid range of values for the parameter.
prAllowedValues :: Lens' Parameter (Maybe Text)
prAllowedValues f x =
    f (_prAllowedValues x)
        <&> \y -> x { _prAllowedValues = y }
{-# INLINE prAllowedValues #-}

-- | If true, the parameter can be modified. Some parameters have security or
-- operational implications that prevent them from being changed.
prIsModifiable :: Lens' Parameter (Maybe Bool)
prIsModifiable f x =
    f (_prIsModifiable x)
        <&> \y -> x { _prIsModifiable = y }
{-# INLINE prIsModifiable #-}

-- | The earliest engine version to which the parameter can apply.
prMinimumEngineVersion :: Lens' Parameter (Maybe Text)
prMinimumEngineVersion f x =
    f (_prMinimumEngineVersion x)
        <&> \y -> x { _prMinimumEngineVersion = y }
{-# INLINE prMinimumEngineVersion #-}

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

instance ToQuery Parameter where
    toQuery = genericQuery def

-- | If present, changes to the cluster are pending. Specific pending changes
-- are identified by subelements.
data PendingModifiedValues = PendingModifiedValues
    { _pmvMasterUserPassword :: Maybe Text
      -- ^ The pending or in-progress change of the master user password for
      -- the cluster.
    , _pmvNodeType :: Maybe Text
      -- ^ The pending or in-progress change of the cluster's node type.
    , _pmvNumberOfNodes :: Maybe Integer
      -- ^ The pending or in-progress change of the number of nodes in the
      -- cluster.
    , _pmvClusterType :: Maybe Text
      -- ^ The pending or in-progress change of the cluster type.
    , _pmvClusterVersion :: Maybe Text
      -- ^ The pending or in-progress change of the service version.
    , _pmvAutomatedSnapshotRetentionPeriod :: Maybe Integer
      -- ^ The pending or in-progress change of the automated snapshot
      -- retention period.
    , _pmvClusterIdentifier :: Maybe Text
      -- ^ The pending or in-progress change of the new identifier for the
      -- cluster.
    } deriving (Show, Generic)

-- | The pending or in-progress change of the master user password for the
-- cluster.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword f x =
    f (_pmvMasterUserPassword x)
        <&> \y -> x { _pmvMasterUserPassword = y }
{-# INLINE pmvMasterUserPassword #-}

-- | The pending or in-progress change of the cluster's node type.
pmvNodeType :: Lens' PendingModifiedValues (Maybe Text)
pmvNodeType f x =
    f (_pmvNodeType x)
        <&> \y -> x { _pmvNodeType = y }
{-# INLINE pmvNodeType #-}

-- | The pending or in-progress change of the number of nodes in the cluster.
pmvNumberOfNodes :: Lens' PendingModifiedValues (Maybe Integer)
pmvNumberOfNodes f x =
    f (_pmvNumberOfNodes x)
        <&> \y -> x { _pmvNumberOfNodes = y }
{-# INLINE pmvNumberOfNodes #-}

-- | The pending or in-progress change of the cluster type.
pmvClusterType :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterType f x =
    f (_pmvClusterType x)
        <&> \y -> x { _pmvClusterType = y }
{-# INLINE pmvClusterType #-}

-- | The pending or in-progress change of the service version.
pmvClusterVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterVersion f x =
    f (_pmvClusterVersion x)
        <&> \y -> x { _pmvClusterVersion = y }
{-# INLINE pmvClusterVersion #-}

-- | The pending or in-progress change of the automated snapshot retention
-- period.
pmvAutomatedSnapshotRetentionPeriod :: Lens' PendingModifiedValues (Maybe Integer)
pmvAutomatedSnapshotRetentionPeriod f x =
    f (_pmvAutomatedSnapshotRetentionPeriod x)
        <&> \y -> x { _pmvAutomatedSnapshotRetentionPeriod = y }
{-# INLINE pmvAutomatedSnapshotRetentionPeriod #-}

-- | The pending or in-progress change of the new identifier for the cluster.
pmvClusterIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterIdentifier f x =
    f (_pmvClusterIdentifier x)
        <&> \y -> x { _pmvClusterIdentifier = y }
{-# INLINE pmvClusterIdentifier #-}

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingModifiedValues"

instance ToQuery PendingModifiedValues where
    toQuery = genericQuery def

-- | Describes a recurring charge.
data RecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount :: Maybe Double
      -- ^ The amount charged per the period of time specified by the
      -- recurring charge frequency.
    , _rcRecurringChargeFrequency :: Maybe Text
      -- ^ The frequency at which the recurring charge amount is applied.
    } deriving (Show, Generic)

-- | The amount charged per the period of time specified by the recurring charge
-- frequency.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount f x =
    f (_rcRecurringChargeAmount x)
        <&> \y -> x { _rcRecurringChargeAmount = y }
{-# INLINE rcRecurringChargeAmount #-}

-- | The frequency at which the recurring charge amount is applied.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency f x =
    f (_rcRecurringChargeFrequency x)
        <&> \y -> x { _rcRecurringChargeFrequency = y }
{-# INLINE rcRecurringChargeFrequency #-}

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecurringCharge"

instance ToQuery RecurringCharge where
    toQuery = genericQuery def

-- | Describes a reserved node.
data ReservedNode = ReservedNode
    { _rnReservedNodeId :: Maybe Text
      -- ^ The unique identifier for the reservation.
    , _rnReservedNodeOfferingId :: Maybe Text
      -- ^ The identifier for the reserved node offering.
    , _rnNodeType :: Maybe Text
      -- ^ The node type of the reserved node.
    , _rnStartTime :: Maybe ISO8601
      -- ^ The time the reservation started. You purchase a reserved node
      -- offering for a duration. This is the start time of that duration.
    , _rnDuration :: Maybe Integer
      -- ^ The duration of the node reservation in seconds.
    , _rnFixedPrice :: Maybe Double
      -- ^ The fixed cost Amazon Redshift charged you for this reserved
      -- node.
    , _rnUsagePrice :: Maybe Double
      -- ^ The hourly rate Amazon Redshift charge you for this reserved
      -- node.
    , _rnCurrencyCode :: Maybe Text
      -- ^ The currency code for the reserved cluster.
    , _rnNodeCount :: Maybe Integer
      -- ^ The number of reserved compute nodes.
    , _rnState :: Maybe Text
      -- ^ The state of the reserved Compute Node. Possible Values:
      -- pending-payment-This reserved node has recently been purchased,
      -- and the sale has been approved, but payment has not yet been
      -- confirmed. active-This reserved node is owned by the caller and
      -- is available for use. payment-failed-Payment failed for the
      -- purchase attempt.
    , _rnOfferingType :: Maybe Text
      -- ^ The anticipated utilization of the reserved node, as defined in
      -- the reserved node offering.
    , _rnRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charges for the reserved node.
    } deriving (Show, Generic)

-- | The unique identifier for the reservation.
rnReservedNodeId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeId f x =
    f (_rnReservedNodeId x)
        <&> \y -> x { _rnReservedNodeId = y }
{-# INLINE rnReservedNodeId #-}

-- | The identifier for the reserved node offering.
rnReservedNodeOfferingId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeOfferingId f x =
    f (_rnReservedNodeOfferingId x)
        <&> \y -> x { _rnReservedNodeOfferingId = y }
{-# INLINE rnReservedNodeOfferingId #-}

-- | The node type of the reserved node.
rnNodeType :: Lens' ReservedNode (Maybe Text)
rnNodeType f x =
    f (_rnNodeType x)
        <&> \y -> x { _rnNodeType = y }
{-# INLINE rnNodeType #-}

-- | The time the reservation started. You purchase a reserved node offering for
-- a duration. This is the start time of that duration.
rnStartTime :: Lens' ReservedNode (Maybe ISO8601)
rnStartTime f x =
    f (_rnStartTime x)
        <&> \y -> x { _rnStartTime = y }
{-# INLINE rnStartTime #-}

-- | The duration of the node reservation in seconds.
rnDuration :: Lens' ReservedNode (Maybe Integer)
rnDuration f x =
    f (_rnDuration x)
        <&> \y -> x { _rnDuration = y }
{-# INLINE rnDuration #-}

-- | The fixed cost Amazon Redshift charged you for this reserved node.
rnFixedPrice :: Lens' ReservedNode (Maybe Double)
rnFixedPrice f x =
    f (_rnFixedPrice x)
        <&> \y -> x { _rnFixedPrice = y }
{-# INLINE rnFixedPrice #-}

-- | The hourly rate Amazon Redshift charge you for this reserved node.
rnUsagePrice :: Lens' ReservedNode (Maybe Double)
rnUsagePrice f x =
    f (_rnUsagePrice x)
        <&> \y -> x { _rnUsagePrice = y }
{-# INLINE rnUsagePrice #-}

-- | The currency code for the reserved cluster.
rnCurrencyCode :: Lens' ReservedNode (Maybe Text)
rnCurrencyCode f x =
    f (_rnCurrencyCode x)
        <&> \y -> x { _rnCurrencyCode = y }
{-# INLINE rnCurrencyCode #-}

-- | The number of reserved compute nodes.
rnNodeCount :: Lens' ReservedNode (Maybe Integer)
rnNodeCount f x =
    f (_rnNodeCount x)
        <&> \y -> x { _rnNodeCount = y }
{-# INLINE rnNodeCount #-}

-- | The state of the reserved Compute Node. Possible Values:
-- pending-payment-This reserved node has recently been purchased, and the
-- sale has been approved, but payment has not yet been confirmed. active-This
-- reserved node is owned by the caller and is available for use.
-- payment-failed-Payment failed for the purchase attempt.
rnState :: Lens' ReservedNode (Maybe Text)
rnState f x =
    f (_rnState x)
        <&> \y -> x { _rnState = y }
{-# INLINE rnState #-}

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
rnOfferingType :: Lens' ReservedNode (Maybe Text)
rnOfferingType f x =
    f (_rnOfferingType x)
        <&> \y -> x { _rnOfferingType = y }
{-# INLINE rnOfferingType #-}

-- | The recurring charges for the reserved node.
rnRecurringCharges :: Lens' ReservedNode ([RecurringCharge])
rnRecurringCharges f x =
    f (_rnRecurringCharges x)
        <&> \y -> x { _rnRecurringCharges = y }
{-# INLINE rnRecurringCharges #-}

instance FromXML ReservedNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedNode"

-- | Describes a reserved node offering.
data ReservedNodeOffering = ReservedNodeOffering
    { _rnoReservedNodeOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , _rnoNodeType :: Maybe Text
      -- ^ The node type offered by the reserved node offering.
    , _rnoDuration :: Maybe Integer
      -- ^ The duration, in seconds, for which the offering will reserve the
      -- node.
    , _rnoFixedPrice :: Maybe Double
      -- ^ The upfront fixed charge you will pay to purchase the specific
      -- reserved node offering.
    , _rnoUsagePrice :: Maybe Double
      -- ^ The rate you are charged for each hour the cluster that is using
      -- the offering is running.
    , _rnoCurrencyCode :: Maybe Text
      -- ^ The currency code for the compute nodes offering.
    , _rnoOfferingType :: Maybe Text
      -- ^ The anticipated utilization of the reserved node, as defined in
      -- the reserved node offering.
    , _rnoRecurringCharges :: [RecurringCharge]
      -- ^ The charge to your account regardless of whether you are creating
      -- any clusters using the node offering. Recurring charges are only
      -- in effect for heavy-utilization reserved nodes.
    } deriving (Show, Generic)

-- | The offering identifier.
rnoReservedNodeOfferingId :: Lens' ReservedNodeOffering (Maybe Text)
rnoReservedNodeOfferingId f x =
    f (_rnoReservedNodeOfferingId x)
        <&> \y -> x { _rnoReservedNodeOfferingId = y }
{-# INLINE rnoReservedNodeOfferingId #-}

-- | The node type offered by the reserved node offering.
rnoNodeType :: Lens' ReservedNodeOffering (Maybe Text)
rnoNodeType f x =
    f (_rnoNodeType x)
        <&> \y -> x { _rnoNodeType = y }
{-# INLINE rnoNodeType #-}

-- | The duration, in seconds, for which the offering will reserve the node.
rnoDuration :: Lens' ReservedNodeOffering (Maybe Integer)
rnoDuration f x =
    f (_rnoDuration x)
        <&> \y -> x { _rnoDuration = y }
{-# INLINE rnoDuration #-}

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- node offering.
rnoFixedPrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoFixedPrice f x =
    f (_rnoFixedPrice x)
        <&> \y -> x { _rnoFixedPrice = y }
{-# INLINE rnoFixedPrice #-}

-- | The rate you are charged for each hour the cluster that is using the
-- offering is running.
rnoUsagePrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoUsagePrice f x =
    f (_rnoUsagePrice x)
        <&> \y -> x { _rnoUsagePrice = y }
{-# INLINE rnoUsagePrice #-}

-- | The currency code for the compute nodes offering.
rnoCurrencyCode :: Lens' ReservedNodeOffering (Maybe Text)
rnoCurrencyCode f x =
    f (_rnoCurrencyCode x)
        <&> \y -> x { _rnoCurrencyCode = y }
{-# INLINE rnoCurrencyCode #-}

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
rnoOfferingType :: Lens' ReservedNodeOffering (Maybe Text)
rnoOfferingType f x =
    f (_rnoOfferingType x)
        <&> \y -> x { _rnoOfferingType = y }
{-# INLINE rnoOfferingType #-}

-- | The charge to your account regardless of whether you are creating any
-- clusters using the node offering. Recurring charges are only in effect for
-- heavy-utilization reserved nodes.
rnoRecurringCharges :: Lens' ReservedNodeOffering ([RecurringCharge])
rnoRecurringCharges f x =
    f (_rnoRecurringCharges x)
        <&> \y -> x { _rnoRecurringCharges = y }
{-# INLINE rnoRecurringCharges #-}

instance FromXML ReservedNodeOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedNodeOffering"

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
data RestoreStatus = RestoreStatus
    { _rsStatus :: Maybe Text
      -- ^ The status of the restore action. Returns starting, restoring,
      -- completed, or failed.
    , _rsCurrentRestoreRateInMegaBytesPerSecond :: Maybe Double
      -- ^ The number of megabytes per second being transferred from the
      -- backup storage. Returns the average rate for a completed backup.
    , _rsSnapshotSizeInMegaBytes :: Maybe Integer
      -- ^ The size of the set of snapshot data used to restore the cluster.
    , _rsProgressInMegaBytes :: Maybe Integer
      -- ^ The number of megabytes that have been transferred from snapshot
      -- storage.
    , _rsElapsedTimeInSeconds :: Maybe Integer
      -- ^ The amount of time an in-progress restore has been running, or
      -- the amount of time it took a completed restore to finish.
    , _rsEstimatedTimeToCompletionInSeconds :: Maybe Integer
      -- ^ The estimate of the time remaining before the restore will
      -- complete. Returns 0 for a completed restore.
    } deriving (Show, Generic)

-- | The status of the restore action. Returns starting, restoring, completed,
-- or failed.
rsStatus :: Lens' RestoreStatus (Maybe Text)
rsStatus f x =
    f (_rsStatus x)
        <&> \y -> x { _rsStatus = y }
{-# INLINE rsStatus #-}

-- | The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup.
rsCurrentRestoreRateInMegaBytesPerSecond :: Lens' RestoreStatus (Maybe Double)
rsCurrentRestoreRateInMegaBytesPerSecond f x =
    f (_rsCurrentRestoreRateInMegaBytesPerSecond x)
        <&> \y -> x { _rsCurrentRestoreRateInMegaBytesPerSecond = y }
{-# INLINE rsCurrentRestoreRateInMegaBytesPerSecond #-}

-- | The size of the set of snapshot data used to restore the cluster.
rsSnapshotSizeInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsSnapshotSizeInMegaBytes f x =
    f (_rsSnapshotSizeInMegaBytes x)
        <&> \y -> x { _rsSnapshotSizeInMegaBytes = y }
{-# INLINE rsSnapshotSizeInMegaBytes #-}

-- | The number of megabytes that have been transferred from snapshot storage.
rsProgressInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsProgressInMegaBytes f x =
    f (_rsProgressInMegaBytes x)
        <&> \y -> x { _rsProgressInMegaBytes = y }
{-# INLINE rsProgressInMegaBytes #-}

-- | The amount of time an in-progress restore has been running, or the amount
-- of time it took a completed restore to finish.
rsElapsedTimeInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsElapsedTimeInSeconds f x =
    f (_rsElapsedTimeInSeconds x)
        <&> \y -> x { _rsElapsedTimeInSeconds = y }
{-# INLINE rsElapsedTimeInSeconds #-}

-- | The estimate of the time remaining before the restore will complete.
-- Returns 0 for a completed restore.
rsEstimatedTimeToCompletionInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsEstimatedTimeToCompletionInSeconds f x =
    f (_rsEstimatedTimeToCompletionInSeconds x)
        <&> \y -> x { _rsEstimatedTimeToCompletionInSeconds = y }
{-# INLINE rsEstimatedTimeToCompletionInSeconds #-}

instance FromXML RestoreStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RestoreStatus"

instance ToQuery RestoreStatus where
    toQuery = genericQuery def

-- | Describes a snapshot.
data Snapshot = Snapshot
    { _stSnapshotIdentifier :: Maybe Text
      -- ^ The snapshot identifier that is provided in the request.
    , _stClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster for which the snapshot was taken.
    , _stSnapshotCreateTime :: Maybe ISO8601
      -- ^ The time (UTC) when Amazon Redshift began the snapshot. A
      -- snapshot contains a copy of the cluster data as of this exact
      -- time.
    , _stStatus :: Maybe Text
      -- ^ The snapshot status. The value of the status depends on the API
      -- operation used. CreateClusterSnapshot and CopyClusterSnapshot
      -- returns status as "creating". DescribeClusterSnapshots returns
      -- status as "creating", "available", "final snapshot", or "failed".
      -- DeleteClusterSnapshot returns status as "deleted".
    , _stPort :: Maybe Integer
      -- ^ The port that the cluster is listening on.
    , _stAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the cluster was created.
    , _stClusterCreateTime :: Maybe ISO8601
      -- ^ The time (UTC) when the cluster was originally created.
    , _stMasterUsername :: Maybe Text
      -- ^ The master user name for the cluster.
    , _stClusterVersion :: Maybe Text
      -- ^ The version ID of the Amazon Redshift engine that is running on
      -- the cluster.
    , _stSnapshotType :: Maybe Text
      -- ^ The snapshot type. Snapshots created using CreateClusterSnapshot
      -- and CopyClusterSnapshot will be of type "manual".
    , _stNodeType :: Maybe Text
      -- ^ The node type of the nodes in the cluster.
    , _stNumberOfNodes :: Maybe Integer
      -- ^ The number of nodes in the cluster.
    , _stDBName :: Maybe Text
      -- ^ The name of the database that was created when the cluster was
      -- created.
    , _stVpcId :: Maybe Text
      -- ^ The VPC identifier of the cluster if the snapshot is from a
      -- cluster in a VPC. Otherwise, this field is not in the output.
    , _stEncrypted :: Maybe Bool
      -- ^ If true, the data in the snapshot is encrypted at rest.
    , _stEncryptedWithHSM :: Maybe Bool
      -- ^ A boolean that indicates whether the snapshot data is encrypted
      -- using the HSM keys of the source cluster. true indicates that the
      -- data is encrypted using HSM keys.
    , _stAccountsWithRestoreAccess :: [AccountWithRestoreAccess]
      -- ^ A list of the AWS customer accounts authorized to restore the
      -- snapshot. Returns null if no accounts are authorized. Visible
      -- only to the snapshot owner.
    , _stOwnerAccount :: Maybe Text
      -- ^ For manual snapshots, the AWS customer account used to create or
      -- copy the snapshot. For automatic snapshots, the owner of the
      -- cluster. The owner can perform all snapshot actions, such as
      -- sharing a manual snapshot.
    , _stTotalBackupSizeInMegaBytes :: Maybe Double
      -- ^ The size of the complete set of backup data that would be used to
      -- restore the cluster.
    , _stActualIncrementalBackupSizeInMegaBytes :: Maybe Double
      -- ^ The size of the incremental backup.
    , _stBackupProgressInMegaBytes :: Maybe Double
      -- ^ The number of megabytes that have been transferred to the
      -- snapshot backup.
    , _stCurrentBackupRateInMegaBytesPerSecond :: Maybe Double
      -- ^ The number of megabytes per second being transferred to the
      -- snapshot backup. Returns 0 for a completed backup.
    , _stEstimatedSecondsToCompletion :: Maybe Integer
      -- ^ The estimate of the time remaining before the snapshot backup
      -- will complete. Returns 0 for a completed backup.
    , _stElapsedTimeInSeconds :: Maybe Integer
      -- ^ The amount of time an in-progress snapshot backup has been
      -- running, or the amount of time it took a completed backup to
      -- finish.
    , _stSourceRegion :: Maybe Text
      -- ^ The source region from which the snapshot was copied.
    } deriving (Show, Generic)

-- | The snapshot identifier that is provided in the request.
stSnapshotIdentifier :: Lens' Snapshot (Maybe Text)
stSnapshotIdentifier f x =
    f (_stSnapshotIdentifier x)
        <&> \y -> x { _stSnapshotIdentifier = y }
{-# INLINE stSnapshotIdentifier #-}

-- | The identifier of the cluster for which the snapshot was taken.
stClusterIdentifier :: Lens' Snapshot (Maybe Text)
stClusterIdentifier f x =
    f (_stClusterIdentifier x)
        <&> \y -> x { _stClusterIdentifier = y }
{-# INLINE stClusterIdentifier #-}

-- | The time (UTC) when Amazon Redshift began the snapshot. A snapshot contains
-- a copy of the cluster data as of this exact time.
stSnapshotCreateTime :: Lens' Snapshot (Maybe ISO8601)
stSnapshotCreateTime f x =
    f (_stSnapshotCreateTime x)
        <&> \y -> x { _stSnapshotCreateTime = y }
{-# INLINE stSnapshotCreateTime #-}

-- | The snapshot status. The value of the status depends on the API operation
-- used. CreateClusterSnapshot and CopyClusterSnapshot returns status as
-- "creating". DescribeClusterSnapshots returns status as "creating",
-- "available", "final snapshot", or "failed". DeleteClusterSnapshot returns
-- status as "deleted".
stStatus :: Lens' Snapshot (Maybe Text)
stStatus f x =
    f (_stStatus x)
        <&> \y -> x { _stStatus = y }
{-# INLINE stStatus #-}

-- | The port that the cluster is listening on.
stPort :: Lens' Snapshot (Maybe Integer)
stPort f x =
    f (_stPort x)
        <&> \y -> x { _stPort = y }
{-# INLINE stPort #-}

-- | The Availability Zone in which the cluster was created.
stAvailabilityZone :: Lens' Snapshot (Maybe Text)
stAvailabilityZone f x =
    f (_stAvailabilityZone x)
        <&> \y -> x { _stAvailabilityZone = y }
{-# INLINE stAvailabilityZone #-}

-- | The time (UTC) when the cluster was originally created.
stClusterCreateTime :: Lens' Snapshot (Maybe ISO8601)
stClusterCreateTime f x =
    f (_stClusterCreateTime x)
        <&> \y -> x { _stClusterCreateTime = y }
{-# INLINE stClusterCreateTime #-}

-- | The master user name for the cluster.
stMasterUsername :: Lens' Snapshot (Maybe Text)
stMasterUsername f x =
    f (_stMasterUsername x)
        <&> \y -> x { _stMasterUsername = y }
{-# INLINE stMasterUsername #-}

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
stClusterVersion :: Lens' Snapshot (Maybe Text)
stClusterVersion f x =
    f (_stClusterVersion x)
        <&> \y -> x { _stClusterVersion = y }
{-# INLINE stClusterVersion #-}

-- | The snapshot type. Snapshots created using CreateClusterSnapshot and
-- CopyClusterSnapshot will be of type "manual".
stSnapshotType :: Lens' Snapshot (Maybe Text)
stSnapshotType f x =
    f (_stSnapshotType x)
        <&> \y -> x { _stSnapshotType = y }
{-# INLINE stSnapshotType #-}

-- | The node type of the nodes in the cluster.
stNodeType :: Lens' Snapshot (Maybe Text)
stNodeType f x =
    f (_stNodeType x)
        <&> \y -> x { _stNodeType = y }
{-# INLINE stNodeType #-}

-- | The number of nodes in the cluster.
stNumberOfNodes :: Lens' Snapshot (Maybe Integer)
stNumberOfNodes f x =
    f (_stNumberOfNodes x)
        <&> \y -> x { _stNumberOfNodes = y }
{-# INLINE stNumberOfNodes #-}

-- | The name of the database that was created when the cluster was created.
stDBName :: Lens' Snapshot (Maybe Text)
stDBName f x =
    f (_stDBName x)
        <&> \y -> x { _stDBName = y }
{-# INLINE stDBName #-}

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a
-- VPC. Otherwise, this field is not in the output.
stVpcId :: Lens' Snapshot (Maybe Text)
stVpcId f x =
    f (_stVpcId x)
        <&> \y -> x { _stVpcId = y }
{-# INLINE stVpcId #-}

-- | If true, the data in the snapshot is encrypted at rest.
stEncrypted :: Lens' Snapshot (Maybe Bool)
stEncrypted f x =
    f (_stEncrypted x)
        <&> \y -> x { _stEncrypted = y }
{-# INLINE stEncrypted #-}

-- | A boolean that indicates whether the snapshot data is encrypted using the
-- HSM keys of the source cluster. true indicates that the data is encrypted
-- using HSM keys.
stEncryptedWithHSM :: Lens' Snapshot (Maybe Bool)
stEncryptedWithHSM f x =
    f (_stEncryptedWithHSM x)
        <&> \y -> x { _stEncryptedWithHSM = y }
{-# INLINE stEncryptedWithHSM #-}

-- | A list of the AWS customer accounts authorized to restore the snapshot.
-- Returns null if no accounts are authorized. Visible only to the snapshot
-- owner.
stAccountsWithRestoreAccess :: Lens' Snapshot ([AccountWithRestoreAccess])
stAccountsWithRestoreAccess f x =
    f (_stAccountsWithRestoreAccess x)
        <&> \y -> x { _stAccountsWithRestoreAccess = y }
{-# INLINE stAccountsWithRestoreAccess #-}

-- | For manual snapshots, the AWS customer account used to create or copy the
-- snapshot. For automatic snapshots, the owner of the cluster. The owner can
-- perform all snapshot actions, such as sharing a manual snapshot.
stOwnerAccount :: Lens' Snapshot (Maybe Text)
stOwnerAccount f x =
    f (_stOwnerAccount x)
        <&> \y -> x { _stOwnerAccount = y }
{-# INLINE stOwnerAccount #-}

-- | The size of the complete set of backup data that would be used to restore
-- the cluster.
stTotalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
stTotalBackupSizeInMegaBytes f x =
    f (_stTotalBackupSizeInMegaBytes x)
        <&> \y -> x { _stTotalBackupSizeInMegaBytes = y }
{-# INLINE stTotalBackupSizeInMegaBytes #-}

-- | The size of the incremental backup.
stActualIncrementalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
stActualIncrementalBackupSizeInMegaBytes f x =
    f (_stActualIncrementalBackupSizeInMegaBytes x)
        <&> \y -> x { _stActualIncrementalBackupSizeInMegaBytes = y }
{-# INLINE stActualIncrementalBackupSizeInMegaBytes #-}

-- | The number of megabytes that have been transferred to the snapshot backup.
stBackupProgressInMegaBytes :: Lens' Snapshot (Maybe Double)
stBackupProgressInMegaBytes f x =
    f (_stBackupProgressInMegaBytes x)
        <&> \y -> x { _stBackupProgressInMegaBytes = y }
{-# INLINE stBackupProgressInMegaBytes #-}

-- | The number of megabytes per second being transferred to the snapshot
-- backup. Returns 0 for a completed backup.
stCurrentBackupRateInMegaBytesPerSecond :: Lens' Snapshot (Maybe Double)
stCurrentBackupRateInMegaBytesPerSecond f x =
    f (_stCurrentBackupRateInMegaBytesPerSecond x)
        <&> \y -> x { _stCurrentBackupRateInMegaBytesPerSecond = y }
{-# INLINE stCurrentBackupRateInMegaBytesPerSecond #-}

-- | The estimate of the time remaining before the snapshot backup will
-- complete. Returns 0 for a completed backup.
stEstimatedSecondsToCompletion :: Lens' Snapshot (Maybe Integer)
stEstimatedSecondsToCompletion f x =
    f (_stEstimatedSecondsToCompletion x)
        <&> \y -> x { _stEstimatedSecondsToCompletion = y }
{-# INLINE stEstimatedSecondsToCompletion #-}

-- | The amount of time an in-progress snapshot backup has been running, or the
-- amount of time it took a completed backup to finish.
stElapsedTimeInSeconds :: Lens' Snapshot (Maybe Integer)
stElapsedTimeInSeconds f x =
    f (_stElapsedTimeInSeconds x)
        <&> \y -> x { _stElapsedTimeInSeconds = y }
{-# INLINE stElapsedTimeInSeconds #-}

-- | The source region from which the snapshot was copied.
stSourceRegion :: Lens' Snapshot (Maybe Text)
stSourceRegion f x =
    f (_stSourceRegion x)
        <&> \y -> x { _stSourceRegion = y }
{-# INLINE stSourceRegion #-}

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Snapshot"

-- | Describes a subnet.
data Subnet = Subnet
    { _sssssssuSubnetIdentifier :: Maybe Text
      -- ^ The identifier of the subnet.
    , _sssssssuSubnetAvailabilityZone :: Maybe AvailabilityZone
      -- ^ Describes an availability zone.
    , _sssssssuSubnetStatus :: Maybe Text
      -- ^ The status of the subnet.
    } deriving (Show, Generic)

-- | The identifier of the subnet.
sssssssuSubnetIdentifier :: Lens' Subnet (Maybe Text)
sssssssuSubnetIdentifier f x =
    f (_sssssssuSubnetIdentifier x)
        <&> \y -> x { _sssssssuSubnetIdentifier = y }
{-# INLINE sssssssuSubnetIdentifier #-}

-- | Describes an availability zone.
sssssssuSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sssssssuSubnetAvailabilityZone f x =
    f (_sssssssuSubnetAvailabilityZone x)
        <&> \y -> x { _sssssssuSubnetAvailabilityZone = y }
{-# INLINE sssssssuSubnetAvailabilityZone #-}

-- | The status of the subnet.
sssssssuSubnetStatus :: Lens' Subnet (Maybe Text)
sssssssuSubnetStatus f x =
    f (_sssssssuSubnetStatus x)
        <&> \y -> x { _sssssssuSubnetStatus = y }
{-# INLINE sssssssuSubnetStatus #-}

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericQuery def

-- | Describes the members of a VPC security group.
data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmVpcSecurityGroupId :: Maybe Text
      -- ^ 
    , _vsgmStatus :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

-- | 
vsgmVpcSecurityGroupId :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmVpcSecurityGroupId f x =
    f (_vsgmVpcSecurityGroupId x)
        <&> \y -> x { _vsgmVpcSecurityGroupId = y }
{-# INLINE vsgmVpcSecurityGroupId #-}

-- | 
vsgmStatus :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmStatus f x =
    f (_vsgmStatus x)
        <&> \y -> x { _vsgmStatus = y }
{-# INLINE vsgmStatus #-}

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroup"

instance ToQuery VpcSecurityGroupMembership where
    toQuery = genericQuery def
