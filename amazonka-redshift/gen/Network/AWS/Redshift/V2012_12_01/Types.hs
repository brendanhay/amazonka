{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    , AccountWithRestoreAccess
    , mkAccountWithRestoreAccess
    , awraAccountId

    -- * AvailabilityZone
    , AvailabilityZone
    , mkAvailabilityZone
    , azName

    -- * Cluster
    , Cluster
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
    , ClusterNode
    , mkClusterNode
    , cnNodeRole
    , cnPrivateIPAddress
    , cnPublicIPAddress

    -- * ClusterParameterGroup
    , ClusterParameterGroup
    , cpgParameterGroupName
    , cpgParameterGroupFamily
    , cpgDescription

    -- * ClusterParameterGroupStatus
    , ClusterParameterGroupStatus
    , mkClusterParameterGroupStatus
    , cpgsParameterGroupName
    , cpgsParameterApplyStatus

    -- * ClusterSecurityGroup
    , ClusterSecurityGroup
    , csgClusterSecurityGroupName
    , csgDescription
    , csgEC2SecurityGroups
    , csgIPRanges

    -- * ClusterSecurityGroupMembership
    , ClusterSecurityGroupMembership
    , mkClusterSecurityGroupMembership
    , csgmClusterSecurityGroupName
    , csgmStatus

    -- * ClusterSnapshotCopyStatus
    , ClusterSnapshotCopyStatus
    , mkClusterSnapshotCopyStatus
    , cscsDestinationRegion
    , cscsRetentionPeriod

    -- * ClusterSubnetGroup
    , ClusterSubnetGroup
    , csiClusterSubnetGroupName
    , csiDescription
    , csiVpcId
    , csiSubnetGroupStatus
    , csiSubnets

    -- * ClusterVersion
    , ClusterVersion
    , cvClusterVersion
    , cvClusterParameterGroupFamily
    , cvDescription

    -- * DefaultClusterParameters
    , DefaultClusterParameters
    , dcpParameterGroupFamily
    , dcpMarker
    , dcpParameters

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , mkEC2SecurityGroup
    , ecsgStatus
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId

    -- * ElasticIpStatus
    , ElasticIpStatus
    , mkElasticIpStatus
    , eisElasticIp
    , eisStatus

    -- * Endpoint
    , Endpoint
    , mkEndpoint
    , etAddress
    , etPort

    -- * Event
    , Event
    , exSourceIdentifier
    , exSourceType
    , exMessage
    , exEventCategories
    , exSeverity
    , exDate
    , exEventId

    -- * EventCategoriesMap
    , EventCategoriesMap
    , ecoSourceType
    , ecoEvents

    -- * EventInfoMap
    , EventInfoMap
    , mkEventInfoMap
    , eimEventId
    , eimEventCategories
    , eimEventDescription
    , eimSeverity

    -- * EventSubscription
    , EventSubscription
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
    , hccHsmClientCertificateIdentifier
    , hccHsmClientCertificatePublicKey

    -- * HsmConfiguration
    , HsmConfiguration
    , hcHsmConfigurationIdentifier
    , hcDescription
    , hcHsmIpAddress
    , hcHsmPartitionName

    -- * HsmStatus
    , HsmStatus
    , mkHsmStatus
    , hsHsmClientCertificateIdentifier
    , hsHsmConfigurationIdentifier
    , hsStatus

    -- * IPRange
    , IPRange
    , mkIPRange
    , iprStatus
    , iprCIDRIP

    -- * OrderableClusterOption
    , OrderableClusterOption
    , ocoClusterVersion
    , ocoClusterType
    , ocoNodeType
    , ocoAvailabilityZones

    -- * Parameter
    , Parameter
    , mkParameter
    , prParameterName
    , prParameterValue
    , prDescription
    , prSource
    , prDataType
    , prAllowedValues
    , prIsModifiable
    , prMinimumEngineVersion

    -- * PendingModifiedValues
    , PendingModifiedValues
    , mkPendingModifiedValues
    , pmvMasterUserPassword
    , pmvNodeType
    , pmvNumberOfNodes
    , pmvClusterType
    , pmvClusterVersion
    , pmvAutomatedSnapshotRetentionPeriod
    , pmvClusterIdentifier

    -- * RecurringCharge
    , RecurringCharge
    , mkRecurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ReservedNode
    , ReservedNode
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
    , mkRestoreStatus
    , rsStatus
    , rsCurrentRestoreRateInMegaBytesPerSecond
    , rsSnapshotSizeInMegaBytes
    , rsProgressInMegaBytes
    , rsElapsedTimeInSeconds
    , rsEstimatedTimeToCompletionInSeconds

    -- * Snapshot
    , Snapshot
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
    , Subnet
    , mkSubnet
    , sssssssuSubnetIdentifier
    , sssssssuSubnetAvailabilityZone
    , sssssssuSubnetStatus

    -- * VpcSecurityGroupMembership
    , VpcSecurityGroupMembership
    , mkVpcSecurityGroupMembership
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
awraAccountId = lens _awraAccountId (\s a -> s { _awraAccountId = a })
{-# INLINE awraAccountId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccountWithRestoreAccess' data type to populate a request.
mkAccountWithRestoreAccess :: AccountWithRestoreAccess
mkAccountWithRestoreAccess = AccountWithRestoreAccess
    { _awraAccountId = Nothing
    }
{-# INLINE mkAccountWithRestoreAccess #-}

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
azName = lens _azName (\s a -> s { _azName = a })
{-# INLINE azName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityZone' data type to populate a request.
mkAvailabilityZone :: AvailabilityZone
mkAvailabilityZone = AvailabilityZone
    { _azName = Nothing
    }
{-# INLINE mkAvailabilityZone #-}

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
crClusterIdentifier = lens _crClusterIdentifier (\s a -> s { _crClusterIdentifier = a })
{-# INLINE crClusterIdentifier #-}

-- | The node type for the nodes in the cluster.
crNodeType :: Lens' Cluster (Maybe Text)
crNodeType = lens _crNodeType (\s a -> s { _crNodeType = a })
{-# INLINE crNodeType #-}

-- | The current state of this cluster. Possible values include available,
-- creating, deleting, rebooting, renaming, and resizing.
crClusterStatus :: Lens' Cluster (Maybe Text)
crClusterStatus = lens _crClusterStatus (\s a -> s { _crClusterStatus = a })
{-# INLINE crClusterStatus #-}

-- | The status of a modify operation, if any, initiated for the cluster.
crModifyStatus :: Lens' Cluster (Maybe Text)
crModifyStatus = lens _crModifyStatus (\s a -> s { _crModifyStatus = a })
{-# INLINE crModifyStatus #-}

-- | The master user name for the cluster. This name is used to connect to the
-- database that is specified in DBName.
crMasterUsername :: Lens' Cluster (Maybe Text)
crMasterUsername = lens _crMasterUsername (\s a -> s { _crMasterUsername = a })
{-# INLINE crMasterUsername #-}

-- | The name of the initial database that was created when the cluster was
-- created. This same name is returned for the life of the cluster. If an
-- initial database was not specified, a database named "dev" was created by
-- default.
crDBName :: Lens' Cluster (Maybe Text)
crDBName = lens _crDBName (\s a -> s { _crDBName = a })
{-# INLINE crDBName #-}

-- | The connection endpoint.
crEndpoint :: Lens' Cluster (Maybe Endpoint)
crEndpoint = lens _crEndpoint (\s a -> s { _crEndpoint = a })
{-# INLINE crEndpoint #-}

-- | The date and time that the cluster was created.
crClusterCreateTime :: Lens' Cluster (Maybe ISO8601)
crClusterCreateTime = lens _crClusterCreateTime (\s a -> s { _crClusterCreateTime = a })
{-# INLINE crClusterCreateTime #-}

-- | The number of days that automatic cluster snapshots are retained.
crAutomatedSnapshotRetentionPeriod :: Lens' Cluster (Maybe Integer)
crAutomatedSnapshotRetentionPeriod = lens _crAutomatedSnapshotRetentionPeriod (\s a -> s { _crAutomatedSnapshotRetentionPeriod = a })
{-# INLINE crAutomatedSnapshotRetentionPeriod #-}

-- | A list of cluster security group that are associated with the cluster. Each
-- security group is represented by an element that contains
-- ClusterSecurityGroup.Name and ClusterSecurityGroup.Status subelements.
-- Cluster security groups are used when the cluster is not created in a VPC.
-- Clusters that are created in a VPC use VPC security groups, which are
-- listed by the VpcSecurityGroups parameter.
crClusterSecurityGroups :: Lens' Cluster ([ClusterSecurityGroupMembership])
crClusterSecurityGroups = lens _crClusterSecurityGroups (\s a -> s { _crClusterSecurityGroups = a })
{-# INLINE crClusterSecurityGroups #-}

-- | A list of Virtual Private Cloud (VPC) security groups that are associated
-- with the cluster. This parameter is returned only if the cluster is in a
-- VPC.
crVpcSecurityGroups :: Lens' Cluster ([VpcSecurityGroupMembership])
crVpcSecurityGroups = lens _crVpcSecurityGroups (\s a -> s { _crVpcSecurityGroups = a })
{-# INLINE crVpcSecurityGroups #-}

-- | The list of cluster parameter groups that are associated with this cluster.
crClusterParameterGroups :: Lens' Cluster ([ClusterParameterGroupStatus])
crClusterParameterGroups = lens _crClusterParameterGroups (\s a -> s { _crClusterParameterGroups = a })
{-# INLINE crClusterParameterGroups #-}

-- | The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
crClusterSubnetGroupName :: Lens' Cluster (Maybe Text)
crClusterSubnetGroupName = lens _crClusterSubnetGroupName (\s a -> s { _crClusterSubnetGroupName = a })
{-# INLINE crClusterSubnetGroupName #-}

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
crVpcId :: Lens' Cluster (Maybe Text)
crVpcId = lens _crVpcId (\s a -> s { _crVpcId = a })
{-# INLINE crVpcId #-}

-- | The name of the Availability Zone in which the cluster is located.
crAvailabilityZone :: Lens' Cluster (Maybe Text)
crAvailabilityZone = lens _crAvailabilityZone (\s a -> s { _crAvailabilityZone = a })
{-# INLINE crAvailabilityZone #-}

-- | The weekly time range (in UTC) during which system maintenance can occur.
crPreferredMaintenanceWindow :: Lens' Cluster (Maybe Text)
crPreferredMaintenanceWindow = lens _crPreferredMaintenanceWindow (\s a -> s { _crPreferredMaintenanceWindow = a })
{-# INLINE crPreferredMaintenanceWindow #-}

-- | If present, changes to the cluster are pending. Specific pending changes
-- are identified by subelements.
crPendingModifiedValues :: Lens' Cluster (Maybe PendingModifiedValues)
crPendingModifiedValues = lens _crPendingModifiedValues (\s a -> s { _crPendingModifiedValues = a })
{-# INLINE crPendingModifiedValues #-}

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
crClusterVersion :: Lens' Cluster (Maybe Text)
crClusterVersion = lens _crClusterVersion (\s a -> s { _crClusterVersion = a })
{-# INLINE crClusterVersion #-}

-- | If true, version upgrades will be applied automatically to the cluster
-- during the maintenance window.
crAllowVersionUpgrade :: Lens' Cluster (Maybe Bool)
crAllowVersionUpgrade = lens _crAllowVersionUpgrade (\s a -> s { _crAllowVersionUpgrade = a })
{-# INLINE crAllowVersionUpgrade #-}

-- | The number of compute nodes in the cluster.
crNumberOfNodes :: Lens' Cluster (Maybe Integer)
crNumberOfNodes = lens _crNumberOfNodes (\s a -> s { _crNumberOfNodes = a })
{-# INLINE crNumberOfNodes #-}

-- | If true, the cluster can be accessed from a public network.
crPubliclyAccessible :: Lens' Cluster (Maybe Bool)
crPubliclyAccessible = lens _crPubliclyAccessible (\s a -> s { _crPubliclyAccessible = a })
{-# INLINE crPubliclyAccessible #-}

-- | If true, data in the cluster is encrypted at rest.
crEncrypted :: Lens' Cluster (Maybe Bool)
crEncrypted = lens _crEncrypted (\s a -> s { _crEncrypted = a })
{-# INLINE crEncrypted #-}

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
crRestoreStatus :: Lens' Cluster (Maybe RestoreStatus)
crRestoreStatus = lens _crRestoreStatus (\s a -> s { _crRestoreStatus = a })
{-# INLINE crRestoreStatus #-}

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
crHsmStatus :: Lens' Cluster (Maybe HsmStatus)
crHsmStatus = lens _crHsmStatus (\s a -> s { _crHsmStatus = a })
{-# INLINE crHsmStatus #-}

-- | Returns the destination region and retention period that are configured for
-- cross-region snapshot copy.
crClusterSnapshotCopyStatus :: Lens' Cluster (Maybe ClusterSnapshotCopyStatus)
crClusterSnapshotCopyStatus = lens _crClusterSnapshotCopyStatus (\s a -> s { _crClusterSnapshotCopyStatus = a })
{-# INLINE crClusterSnapshotCopyStatus #-}

-- | The public key for the cluster.
crClusterPublicKey :: Lens' Cluster (Maybe Text)
crClusterPublicKey = lens _crClusterPublicKey (\s a -> s { _crClusterPublicKey = a })
{-# INLINE crClusterPublicKey #-}

-- | The nodes in a cluster.
crClusterNodes :: Lens' Cluster ([ClusterNode])
crClusterNodes = lens _crClusterNodes (\s a -> s { _crClusterNodes = a })
{-# INLINE crClusterNodes #-}

-- | Describes the status of the elastic IP (EIP) address.
crElasticIpStatus :: Lens' Cluster (Maybe ElasticIpStatus)
crElasticIpStatus = lens _crElasticIpStatus (\s a -> s { _crElasticIpStatus = a })
{-# INLINE crElasticIpStatus #-}

-- | The specific revision number of the database in the cluster.
crClusterRevisionNumber :: Lens' Cluster (Maybe Text)
crClusterRevisionNumber = lens _crClusterRevisionNumber (\s a -> s { _crClusterRevisionNumber = a })
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
cnNodeRole = lens _cnNodeRole (\s a -> s { _cnNodeRole = a })
{-# INLINE cnNodeRole #-}

-- | The private IP address of a node within a cluster.
cnPrivateIPAddress :: Lens' ClusterNode (Maybe Text)
cnPrivateIPAddress = lens _cnPrivateIPAddress (\s a -> s { _cnPrivateIPAddress = a })
{-# INLINE cnPrivateIPAddress #-}

-- | The public IP address of a node within a cluster.
cnPublicIPAddress :: Lens' ClusterNode (Maybe Text)
cnPublicIPAddress = lens _cnPublicIPAddress (\s a -> s { _cnPublicIPAddress = a })
{-# INLINE cnPublicIPAddress #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterNode' data type to populate a request.
mkClusterNode :: ClusterNode
mkClusterNode = ClusterNode
    { _cnNodeRole = Nothing
    , _cnPrivateIPAddress = Nothing
    , _cnPublicIPAddress = Nothing
    }
{-# INLINE mkClusterNode #-}

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
cpgParameterGroupName = lens _cpgParameterGroupName (\s a -> s { _cpgParameterGroupName = a })
{-# INLINE cpgParameterGroupName #-}

-- | The name of the cluster parameter group family that this cluster parameter
-- group is compatible with.
cpgParameterGroupFamily :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupFamily = lens _cpgParameterGroupFamily (\s a -> s { _cpgParameterGroupFamily = a })
{-# INLINE cpgParameterGroupFamily #-}

-- | The description of the parameter group.
cpgDescription :: Lens' ClusterParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\s a -> s { _cpgDescription = a })
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
cpgsParameterGroupName = lens _cpgsParameterGroupName (\s a -> s { _cpgsParameterGroupName = a })
{-# INLINE cpgsParameterGroupName #-}

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus = lens _cpgsParameterApplyStatus (\s a -> s { _cpgsParameterApplyStatus = a })
{-# INLINE cpgsParameterApplyStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterParameterGroupStatus' data type to populate a request.
mkClusterParameterGroupStatus :: ClusterParameterGroupStatus
mkClusterParameterGroupStatus = ClusterParameterGroupStatus
    { _cpgsParameterGroupName = Nothing
    , _cpgsParameterApplyStatus = Nothing
    }
{-# INLINE mkClusterParameterGroupStatus #-}

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
csgClusterSecurityGroupName = lens _csgClusterSecurityGroupName (\s a -> s { _csgClusterSecurityGroupName = a })
{-# INLINE csgClusterSecurityGroupName #-}

-- | A description of the security group.
csgDescription :: Lens' ClusterSecurityGroup (Maybe Text)
csgDescription = lens _csgDescription (\s a -> s { _csgDescription = a })
{-# INLINE csgDescription #-}

-- | A list of EC2 security groups that are permitted to access clusters
-- associated with this cluster security group.
csgEC2SecurityGroups :: Lens' ClusterSecurityGroup ([EC2SecurityGroup])
csgEC2SecurityGroups = lens _csgEC2SecurityGroups (\s a -> s { _csgEC2SecurityGroups = a })
{-# INLINE csgEC2SecurityGroups #-}

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters
-- associated with this cluster security group.
csgIPRanges :: Lens' ClusterSecurityGroup ([IPRange])
csgIPRanges = lens _csgIPRanges (\s a -> s { _csgIPRanges = a })
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
csgmClusterSecurityGroupName = lens _csgmClusterSecurityGroupName (\s a -> s { _csgmClusterSecurityGroupName = a })
{-# INLINE csgmClusterSecurityGroupName #-}

-- | The status of the cluster security group.
csgmStatus :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\s a -> s { _csgmStatus = a })
{-# INLINE csgmStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterSecurityGroupMembership' data type to populate a request.
mkClusterSecurityGroupMembership :: ClusterSecurityGroupMembership
mkClusterSecurityGroupMembership = ClusterSecurityGroupMembership
    { _csgmClusterSecurityGroupName = Nothing
    , _csgmStatus = Nothing
    }
{-# INLINE mkClusterSecurityGroupMembership #-}

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
cscsDestinationRegion = lens _cscsDestinationRegion (\s a -> s { _cscsDestinationRegion = a })
{-# INLINE cscsDestinationRegion #-}

-- | The number of days that automated snapshots are retained in the destination
-- region after they are copied from a source region.
cscsRetentionPeriod :: Lens' ClusterSnapshotCopyStatus (Maybe Integer)
cscsRetentionPeriod = lens _cscsRetentionPeriod (\s a -> s { _cscsRetentionPeriod = a })
{-# INLINE cscsRetentionPeriod #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterSnapshotCopyStatus' data type to populate a request.
mkClusterSnapshotCopyStatus :: ClusterSnapshotCopyStatus
mkClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus
    { _cscsDestinationRegion = Nothing
    , _cscsRetentionPeriod = Nothing
    }
{-# INLINE mkClusterSnapshotCopyStatus #-}

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
csiClusterSubnetGroupName = lens _csiClusterSubnetGroupName (\s a -> s { _csiClusterSubnetGroupName = a })
{-# INLINE csiClusterSubnetGroupName #-}

-- | The description of the cluster subnet group.
csiDescription :: Lens' ClusterSubnetGroup (Maybe Text)
csiDescription = lens _csiDescription (\s a -> s { _csiDescription = a })
{-# INLINE csiDescription #-}

-- | The VPC ID of the cluster subnet group.
csiVpcId :: Lens' ClusterSubnetGroup (Maybe Text)
csiVpcId = lens _csiVpcId (\s a -> s { _csiVpcId = a })
{-# INLINE csiVpcId #-}

-- | The status of the cluster subnet group. Possible values are Complete,
-- Incomplete and Invalid.
csiSubnetGroupStatus :: Lens' ClusterSubnetGroup (Maybe Text)
csiSubnetGroupStatus = lens _csiSubnetGroupStatus (\s a -> s { _csiSubnetGroupStatus = a })
{-# INLINE csiSubnetGroupStatus #-}

-- | A list of the VPC Subnet elements.
csiSubnets :: Lens' ClusterSubnetGroup ([Subnet])
csiSubnets = lens _csiSubnets (\s a -> s { _csiSubnets = a })
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
cvClusterVersion = lens _cvClusterVersion (\s a -> s { _cvClusterVersion = a })
{-# INLINE cvClusterVersion #-}

-- | The name of the cluster parameter group family for the cluster.
cvClusterParameterGroupFamily :: Lens' ClusterVersion (Maybe Text)
cvClusterParameterGroupFamily = lens _cvClusterParameterGroupFamily (\s a -> s { _cvClusterParameterGroupFamily = a })
{-# INLINE cvClusterParameterGroupFamily #-}

-- | The description of the cluster version.
cvDescription :: Lens' ClusterVersion (Maybe Text)
cvDescription = lens _cvDescription (\s a -> s { _cvDescription = a })
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
dcpParameterGroupFamily = lens _dcpParameterGroupFamily (\s a -> s { _dcpParameterGroupFamily = a })
{-# INLINE dcpParameterGroupFamily #-}

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dcpMarker :: Lens' DefaultClusterParameters (Maybe Text)
dcpMarker = lens _dcpMarker (\s a -> s { _dcpMarker = a })
{-# INLINE dcpMarker #-}

-- | The list of cluster default parameters.
dcpParameters :: Lens' DefaultClusterParameters ([Parameter])
dcpParameters = lens _dcpParameters (\s a -> s { _dcpParameters = a })
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
ecsgStatus = lens _ecsgStatus (\s a -> s { _ecsgStatus = a })
{-# INLINE ecsgStatus #-}

-- | The name of the EC2 Security Group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName = lens _ecsgEC2SecurityGroupName (\s a -> s { _ecsgEC2SecurityGroupName = a })
{-# INLINE ecsgEC2SecurityGroupName #-}

-- | The AWS ID of the owner of the EC2 security group specified in the
-- EC2SecurityGroupName field.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId = lens _ecsgEC2SecurityGroupOwnerId (\s a -> s { _ecsgEC2SecurityGroupOwnerId = a })
{-# INLINE ecsgEC2SecurityGroupOwnerId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EC2SecurityGroup' data type to populate a request.
mkEC2SecurityGroup :: EC2SecurityGroup
mkEC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus = Nothing
    , _ecsgEC2SecurityGroupName = Nothing
    , _ecsgEC2SecurityGroupOwnerId = Nothing
    }
{-# INLINE mkEC2SecurityGroup #-}

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
eisElasticIp = lens _eisElasticIp (\s a -> s { _eisElasticIp = a })
{-# INLINE eisElasticIp #-}

-- | Describes the status of the elastic IP (EIP) address.
eisStatus :: Lens' ElasticIpStatus (Maybe Text)
eisStatus = lens _eisStatus (\s a -> s { _eisStatus = a })
{-# INLINE eisStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ElasticIpStatus' data type to populate a request.
mkElasticIpStatus :: ElasticIpStatus
mkElasticIpStatus = ElasticIpStatus
    { _eisElasticIp = Nothing
    , _eisStatus = Nothing
    }
{-# INLINE mkElasticIpStatus #-}

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
etAddress = lens _etAddress (\s a -> s { _etAddress = a })
{-# INLINE etAddress #-}

-- | The port that the database engine is listening on.
etPort :: Lens' Endpoint (Maybe Integer)
etPort = lens _etPort (\s a -> s { _etPort = a })
{-# INLINE etPort #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Endpoint' data type to populate a request.
mkEndpoint :: Endpoint
mkEndpoint = Endpoint
    { _etAddress = Nothing
    , _etPort = Nothing
    }
{-# INLINE mkEndpoint #-}

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
exSourceIdentifier = lens _exSourceIdentifier (\s a -> s { _exSourceIdentifier = a })
{-# INLINE exSourceIdentifier #-}

-- | The source type for this event.
exSourceType :: Lens' Event (Maybe SourceType)
exSourceType = lens _exSourceType (\s a -> s { _exSourceType = a })
{-# INLINE exSourceType #-}

-- | The text of this event.
exMessage :: Lens' Event (Maybe Text)
exMessage = lens _exMessage (\s a -> s { _exMessage = a })
{-# INLINE exMessage #-}

-- | A list of the event categories.
exEventCategories :: Lens' Event ([Text])
exEventCategories = lens _exEventCategories (\s a -> s { _exEventCategories = a })
{-# INLINE exEventCategories #-}

-- | The severity of the event. Values: ERROR, INFO.
exSeverity :: Lens' Event (Maybe Text)
exSeverity = lens _exSeverity (\s a -> s { _exSeverity = a })
{-# INLINE exSeverity #-}

-- | The date and time of the event.
exDate :: Lens' Event (Maybe ISO8601)
exDate = lens _exDate (\s a -> s { _exDate = a })
{-# INLINE exDate #-}

-- | The identifier of the event.
exEventId :: Lens' Event (Maybe Text)
exEventId = lens _exEventId (\s a -> s { _exEventId = a })
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
ecoSourceType = lens _ecoSourceType (\s a -> s { _ecoSourceType = a })
{-# INLINE ecoSourceType #-}

-- | The events in the event category.
ecoEvents :: Lens' EventCategoriesMap ([EventInfoMap])
ecoEvents = lens _ecoEvents (\s a -> s { _ecoEvents = a })
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
eimEventId = lens _eimEventId (\s a -> s { _eimEventId = a })
{-# INLINE eimEventId #-}

-- | The category of an Amazon Redshift event.
eimEventCategories :: Lens' EventInfoMap ([Text])
eimEventCategories = lens _eimEventCategories (\s a -> s { _eimEventCategories = a })
{-# INLINE eimEventCategories #-}

-- | The description of an Amazon Redshift event.
eimEventDescription :: Lens' EventInfoMap (Maybe Text)
eimEventDescription = lens _eimEventDescription (\s a -> s { _eimEventDescription = a })
{-# INLINE eimEventDescription #-}

-- | The severity of the event. Values: ERROR, INFO.
eimSeverity :: Lens' EventInfoMap (Maybe Text)
eimSeverity = lens _eimSeverity (\s a -> s { _eimSeverity = a })
{-# INLINE eimSeverity #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EventInfoMap' data type to populate a request.
mkEventInfoMap :: EventInfoMap
mkEventInfoMap = EventInfoMap
    { _eimEventId = Nothing
    , _eimEventCategories = mempty
    , _eimEventDescription = Nothing
    , _eimSeverity = Nothing
    }
{-# INLINE mkEventInfoMap #-}

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
esCustomerAwsId = lens _esCustomerAwsId (\s a -> s { _esCustomerAwsId = a })
{-# INLINE esCustomerAwsId #-}

-- | The name of the Amazon Redshift event notification subscription.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId = lens _esCustSubscriptionId (\s a -> s { _esCustSubscriptionId = a })
{-# INLINE esCustSubscriptionId #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
esSnsTopicArn :: Lens' EventSubscription (Maybe Text)
esSnsTopicArn = lens _esSnsTopicArn (\s a -> s { _esSnsTopicArn = a })
{-# INLINE esSnsTopicArn #-}

-- | The status of the Amazon Redshift event notification subscription.
-- Constraints: Can be one of the following: active | no-permission |
-- topic-not-exist The status "no-permission" indicates that Amazon Redshift
-- no longer has permission to post to the Amazon SNS topic. The status
-- "topic-not-exist" indicates that the topic was deleted after the
-- subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\s a -> s { _esStatus = a })
{-# INLINE esStatus #-}

-- | The date and time the Amazon Redshift event notification subscription was
-- created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe ISO8601)
esSubscriptionCreationTime = lens _esSubscriptionCreationTime (\s a -> s { _esSubscriptionCreationTime = a })
{-# INLINE esSubscriptionCreationTime #-}

-- | The source type of the events returned the Amazon Redshift event
-- notification, such as cluster, or cluster-snapshot.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\s a -> s { _esSourceType = a })
{-# INLINE esSourceType #-}

-- | A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
esSourceIdsList :: Lens' EventSubscription ([Text])
esSourceIdsList = lens _esSourceIdsList (\s a -> s { _esSourceIdsList = a })
{-# INLINE esSourceIdsList #-}

-- | The list of Amazon Redshift event categories specified in the event
-- notification subscription. Values: Configuration, Management, Monitoring,
-- Security.
esEventCategoriesList :: Lens' EventSubscription ([Text])
esEventCategoriesList = lens _esEventCategoriesList (\s a -> s { _esEventCategoriesList = a })
{-# INLINE esEventCategoriesList #-}

-- | The event severity specified in the Amazon Redshift event notification
-- subscription. Values: ERROR, INFO.
esSeverity :: Lens' EventSubscription (Maybe Text)
esSeverity = lens _esSeverity (\s a -> s { _esSeverity = a })
{-# INLINE esSeverity #-}

-- | A Boolean value indicating whether the subscription is enabled. true
-- indicates the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\s a -> s { _esEnabled = a })
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
hccHsmClientCertificateIdentifier = lens _hccHsmClientCertificateIdentifier (\s a -> s { _hccHsmClientCertificateIdentifier = a })
{-# INLINE hccHsmClientCertificateIdentifier #-}

-- | The public key that the Amazon Redshift cluster will use to connect to the
-- HSM. You must register the public key in the HSM.
hccHsmClientCertificatePublicKey :: Lens' HsmClientCertificate (Maybe Text)
hccHsmClientCertificatePublicKey = lens _hccHsmClientCertificatePublicKey (\s a -> s { _hccHsmClientCertificatePublicKey = a })
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
hcHsmConfigurationIdentifier = lens _hcHsmConfigurationIdentifier (\s a -> s { _hcHsmConfigurationIdentifier = a })
{-# INLINE hcHsmConfigurationIdentifier #-}

-- | A text description of the HSM configuration.
hcDescription :: Lens' HsmConfiguration (Maybe Text)
hcDescription = lens _hcDescription (\s a -> s { _hcDescription = a })
{-# INLINE hcDescription #-}

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
hcHsmIpAddress :: Lens' HsmConfiguration (Maybe Text)
hcHsmIpAddress = lens _hcHsmIpAddress (\s a -> s { _hcHsmIpAddress = a })
{-# INLINE hcHsmIpAddress #-}

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
hcHsmPartitionName :: Lens' HsmConfiguration (Maybe Text)
hcHsmPartitionName = lens _hcHsmPartitionName (\s a -> s { _hcHsmPartitionName = a })
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
hsHsmClientCertificateIdentifier = lens _hsHsmClientCertificateIdentifier (\s a -> s { _hsHsmClientCertificateIdentifier = a })
{-# INLINE hsHsmClientCertificateIdentifier #-}

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
hsHsmConfigurationIdentifier :: Lens' HsmStatus (Maybe Text)
hsHsmConfigurationIdentifier = lens _hsHsmConfigurationIdentifier (\s a -> s { _hsHsmConfigurationIdentifier = a })
{-# INLINE hsHsmConfigurationIdentifier #-}

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
hsStatus :: Lens' HsmStatus (Maybe Text)
hsStatus = lens _hsStatus (\s a -> s { _hsStatus = a })
{-# INLINE hsStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HsmStatus' data type to populate a request.
mkHsmStatus :: HsmStatus
mkHsmStatus = HsmStatus
    { _hsHsmClientCertificateIdentifier = Nothing
    , _hsHsmConfigurationIdentifier = Nothing
    , _hsStatus = Nothing
    }
{-# INLINE mkHsmStatus #-}

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
iprStatus = lens _iprStatus (\s a -> s { _iprStatus = a })
{-# INLINE iprStatus #-}

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
iprCIDRIP :: Lens' IPRange (Maybe Text)
iprCIDRIP = lens _iprCIDRIP (\s a -> s { _iprCIDRIP = a })
{-# INLINE iprCIDRIP #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IPRange' data type to populate a request.
mkIPRange :: IPRange
mkIPRange = IPRange
    { _iprStatus = Nothing
    , _iprCIDRIP = Nothing
    }
{-# INLINE mkIPRange #-}

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
ocoClusterVersion = lens _ocoClusterVersion (\s a -> s { _ocoClusterVersion = a })
{-# INLINE ocoClusterVersion #-}

-- | The cluster type, for example multi-node.
ocoClusterType :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterType = lens _ocoClusterType (\s a -> s { _ocoClusterType = a })
{-# INLINE ocoClusterType #-}

-- | The node type for the orderable cluster.
ocoNodeType :: Lens' OrderableClusterOption (Maybe Text)
ocoNodeType = lens _ocoNodeType (\s a -> s { _ocoNodeType = a })
{-# INLINE ocoNodeType #-}

-- | A list of availability zones for the orderable cluster.
ocoAvailabilityZones :: Lens' OrderableClusterOption ([AvailabilityZone])
ocoAvailabilityZones = lens _ocoAvailabilityZones (\s a -> s { _ocoAvailabilityZones = a })
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
prParameterName = lens _prParameterName (\s a -> s { _prParameterName = a })
{-# INLINE prParameterName #-}

-- | The value of the parameter.
prParameterValue :: Lens' Parameter (Maybe Text)
prParameterValue = lens _prParameterValue (\s a -> s { _prParameterValue = a })
{-# INLINE prParameterValue #-}

-- | A description of the parameter.
prDescription :: Lens' Parameter (Maybe Text)
prDescription = lens _prDescription (\s a -> s { _prDescription = a })
{-# INLINE prDescription #-}

-- | The source of the parameter value, such as "engine-default" or "user".
prSource :: Lens' Parameter (Maybe Text)
prSource = lens _prSource (\s a -> s { _prSource = a })
{-# INLINE prSource #-}

-- | The data type of the parameter.
prDataType :: Lens' Parameter (Maybe Text)
prDataType = lens _prDataType (\s a -> s { _prDataType = a })
{-# INLINE prDataType #-}

-- | The valid range of values for the parameter.
prAllowedValues :: Lens' Parameter (Maybe Text)
prAllowedValues = lens _prAllowedValues (\s a -> s { _prAllowedValues = a })
{-# INLINE prAllowedValues #-}

-- | If true, the parameter can be modified. Some parameters have security or
-- operational implications that prevent them from being changed.
prIsModifiable :: Lens' Parameter (Maybe Bool)
prIsModifiable = lens _prIsModifiable (\s a -> s { _prIsModifiable = a })
{-# INLINE prIsModifiable #-}

-- | The earliest engine version to which the parameter can apply.
prMinimumEngineVersion :: Lens' Parameter (Maybe Text)
prMinimumEngineVersion = lens _prMinimumEngineVersion (\s a -> s { _prMinimumEngineVersion = a })
{-# INLINE prMinimumEngineVersion #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Parameter' data type to populate a request.
mkParameter :: Parameter
mkParameter = Parameter
    { _prParameterName = Nothing
    , _prParameterValue = Nothing
    , _prDescription = Nothing
    , _prSource = Nothing
    , _prDataType = Nothing
    , _prAllowedValues = Nothing
    , _prIsModifiable = Nothing
    , _prMinimumEngineVersion = Nothing
    }
{-# INLINE mkParameter #-}

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
pmvMasterUserPassword = lens _pmvMasterUserPassword (\s a -> s { _pmvMasterUserPassword = a })
{-# INLINE pmvMasterUserPassword #-}

-- | The pending or in-progress change of the cluster's node type.
pmvNodeType :: Lens' PendingModifiedValues (Maybe Text)
pmvNodeType = lens _pmvNodeType (\s a -> s { _pmvNodeType = a })
{-# INLINE pmvNodeType #-}

-- | The pending or in-progress change of the number of nodes in the cluster.
pmvNumberOfNodes :: Lens' PendingModifiedValues (Maybe Integer)
pmvNumberOfNodes = lens _pmvNumberOfNodes (\s a -> s { _pmvNumberOfNodes = a })
{-# INLINE pmvNumberOfNodes #-}

-- | The pending or in-progress change of the cluster type.
pmvClusterType :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterType = lens _pmvClusterType (\s a -> s { _pmvClusterType = a })
{-# INLINE pmvClusterType #-}

-- | The pending or in-progress change of the service version.
pmvClusterVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterVersion = lens _pmvClusterVersion (\s a -> s { _pmvClusterVersion = a })
{-# INLINE pmvClusterVersion #-}

-- | The pending or in-progress change of the automated snapshot retention
-- period.
pmvAutomatedSnapshotRetentionPeriod :: Lens' PendingModifiedValues (Maybe Integer)
pmvAutomatedSnapshotRetentionPeriod = lens _pmvAutomatedSnapshotRetentionPeriod (\s a -> s { _pmvAutomatedSnapshotRetentionPeriod = a })
{-# INLINE pmvAutomatedSnapshotRetentionPeriod #-}

-- | The pending or in-progress change of the new identifier for the cluster.
pmvClusterIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterIdentifier = lens _pmvClusterIdentifier (\s a -> s { _pmvClusterIdentifier = a })
{-# INLINE pmvClusterIdentifier #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PendingModifiedValues' data type to populate a request.
mkPendingModifiedValues :: PendingModifiedValues
mkPendingModifiedValues = PendingModifiedValues
    { _pmvMasterUserPassword = Nothing
    , _pmvNodeType = Nothing
    , _pmvNumberOfNodes = Nothing
    , _pmvClusterType = Nothing
    , _pmvClusterVersion = Nothing
    , _pmvAutomatedSnapshotRetentionPeriod = Nothing
    , _pmvClusterIdentifier = Nothing
    }
{-# INLINE mkPendingModifiedValues #-}

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
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\s a -> s { _rcRecurringChargeAmount = a })
{-# INLINE rcRecurringChargeAmount #-}

-- | The frequency at which the recurring charge amount is applied.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\s a -> s { _rcRecurringChargeFrequency = a })
{-# INLINE rcRecurringChargeFrequency #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecurringCharge' data type to populate a request.
mkRecurringCharge :: RecurringCharge
mkRecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount = Nothing
    , _rcRecurringChargeFrequency = Nothing
    }
{-# INLINE mkRecurringCharge #-}

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
rnReservedNodeId = lens _rnReservedNodeId (\s a -> s { _rnReservedNodeId = a })
{-# INLINE rnReservedNodeId #-}

-- | The identifier for the reserved node offering.
rnReservedNodeOfferingId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeOfferingId = lens _rnReservedNodeOfferingId (\s a -> s { _rnReservedNodeOfferingId = a })
{-# INLINE rnReservedNodeOfferingId #-}

-- | The node type of the reserved node.
rnNodeType :: Lens' ReservedNode (Maybe Text)
rnNodeType = lens _rnNodeType (\s a -> s { _rnNodeType = a })
{-# INLINE rnNodeType #-}

-- | The time the reservation started. You purchase a reserved node offering for
-- a duration. This is the start time of that duration.
rnStartTime :: Lens' ReservedNode (Maybe ISO8601)
rnStartTime = lens _rnStartTime (\s a -> s { _rnStartTime = a })
{-# INLINE rnStartTime #-}

-- | The duration of the node reservation in seconds.
rnDuration :: Lens' ReservedNode (Maybe Integer)
rnDuration = lens _rnDuration (\s a -> s { _rnDuration = a })
{-# INLINE rnDuration #-}

-- | The fixed cost Amazon Redshift charged you for this reserved node.
rnFixedPrice :: Lens' ReservedNode (Maybe Double)
rnFixedPrice = lens _rnFixedPrice (\s a -> s { _rnFixedPrice = a })
{-# INLINE rnFixedPrice #-}

-- | The hourly rate Amazon Redshift charge you for this reserved node.
rnUsagePrice :: Lens' ReservedNode (Maybe Double)
rnUsagePrice = lens _rnUsagePrice (\s a -> s { _rnUsagePrice = a })
{-# INLINE rnUsagePrice #-}

-- | The currency code for the reserved cluster.
rnCurrencyCode :: Lens' ReservedNode (Maybe Text)
rnCurrencyCode = lens _rnCurrencyCode (\s a -> s { _rnCurrencyCode = a })
{-# INLINE rnCurrencyCode #-}

-- | The number of reserved compute nodes.
rnNodeCount :: Lens' ReservedNode (Maybe Integer)
rnNodeCount = lens _rnNodeCount (\s a -> s { _rnNodeCount = a })
{-# INLINE rnNodeCount #-}

-- | The state of the reserved Compute Node. Possible Values:
-- pending-payment-This reserved node has recently been purchased, and the
-- sale has been approved, but payment has not yet been confirmed. active-This
-- reserved node is owned by the caller and is available for use.
-- payment-failed-Payment failed for the purchase attempt.
rnState :: Lens' ReservedNode (Maybe Text)
rnState = lens _rnState (\s a -> s { _rnState = a })
{-# INLINE rnState #-}

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
rnOfferingType :: Lens' ReservedNode (Maybe Text)
rnOfferingType = lens _rnOfferingType (\s a -> s { _rnOfferingType = a })
{-# INLINE rnOfferingType #-}

-- | The recurring charges for the reserved node.
rnRecurringCharges :: Lens' ReservedNode ([RecurringCharge])
rnRecurringCharges = lens _rnRecurringCharges (\s a -> s { _rnRecurringCharges = a })
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
rnoReservedNodeOfferingId = lens _rnoReservedNodeOfferingId (\s a -> s { _rnoReservedNodeOfferingId = a })
{-# INLINE rnoReservedNodeOfferingId #-}

-- | The node type offered by the reserved node offering.
rnoNodeType :: Lens' ReservedNodeOffering (Maybe Text)
rnoNodeType = lens _rnoNodeType (\s a -> s { _rnoNodeType = a })
{-# INLINE rnoNodeType #-}

-- | The duration, in seconds, for which the offering will reserve the node.
rnoDuration :: Lens' ReservedNodeOffering (Maybe Integer)
rnoDuration = lens _rnoDuration (\s a -> s { _rnoDuration = a })
{-# INLINE rnoDuration #-}

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- node offering.
rnoFixedPrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoFixedPrice = lens _rnoFixedPrice (\s a -> s { _rnoFixedPrice = a })
{-# INLINE rnoFixedPrice #-}

-- | The rate you are charged for each hour the cluster that is using the
-- offering is running.
rnoUsagePrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoUsagePrice = lens _rnoUsagePrice (\s a -> s { _rnoUsagePrice = a })
{-# INLINE rnoUsagePrice #-}

-- | The currency code for the compute nodes offering.
rnoCurrencyCode :: Lens' ReservedNodeOffering (Maybe Text)
rnoCurrencyCode = lens _rnoCurrencyCode (\s a -> s { _rnoCurrencyCode = a })
{-# INLINE rnoCurrencyCode #-}

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
rnoOfferingType :: Lens' ReservedNodeOffering (Maybe Text)
rnoOfferingType = lens _rnoOfferingType (\s a -> s { _rnoOfferingType = a })
{-# INLINE rnoOfferingType #-}

-- | The charge to your account regardless of whether you are creating any
-- clusters using the node offering. Recurring charges are only in effect for
-- heavy-utilization reserved nodes.
rnoRecurringCharges :: Lens' ReservedNodeOffering ([RecurringCharge])
rnoRecurringCharges = lens _rnoRecurringCharges (\s a -> s { _rnoRecurringCharges = a })
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
rsStatus = lens _rsStatus (\s a -> s { _rsStatus = a })
{-# INLINE rsStatus #-}

-- | The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup.
rsCurrentRestoreRateInMegaBytesPerSecond :: Lens' RestoreStatus (Maybe Double)
rsCurrentRestoreRateInMegaBytesPerSecond = lens _rsCurrentRestoreRateInMegaBytesPerSecond (\s a -> s { _rsCurrentRestoreRateInMegaBytesPerSecond = a })
{-# INLINE rsCurrentRestoreRateInMegaBytesPerSecond #-}

-- | The size of the set of snapshot data used to restore the cluster.
rsSnapshotSizeInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsSnapshotSizeInMegaBytes = lens _rsSnapshotSizeInMegaBytes (\s a -> s { _rsSnapshotSizeInMegaBytes = a })
{-# INLINE rsSnapshotSizeInMegaBytes #-}

-- | The number of megabytes that have been transferred from snapshot storage.
rsProgressInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsProgressInMegaBytes = lens _rsProgressInMegaBytes (\s a -> s { _rsProgressInMegaBytes = a })
{-# INLINE rsProgressInMegaBytes #-}

-- | The amount of time an in-progress restore has been running, or the amount
-- of time it took a completed restore to finish.
rsElapsedTimeInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsElapsedTimeInSeconds = lens _rsElapsedTimeInSeconds (\s a -> s { _rsElapsedTimeInSeconds = a })
{-# INLINE rsElapsedTimeInSeconds #-}

-- | The estimate of the time remaining before the restore will complete.
-- Returns 0 for a completed restore.
rsEstimatedTimeToCompletionInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsEstimatedTimeToCompletionInSeconds = lens _rsEstimatedTimeToCompletionInSeconds (\s a -> s { _rsEstimatedTimeToCompletionInSeconds = a })
{-# INLINE rsEstimatedTimeToCompletionInSeconds #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RestoreStatus' data type to populate a request.
mkRestoreStatus :: RestoreStatus
mkRestoreStatus = RestoreStatus
    { _rsStatus = Nothing
    , _rsCurrentRestoreRateInMegaBytesPerSecond = Nothing
    , _rsSnapshotSizeInMegaBytes = Nothing
    , _rsProgressInMegaBytes = Nothing
    , _rsElapsedTimeInSeconds = Nothing
    , _rsEstimatedTimeToCompletionInSeconds = Nothing
    }
{-# INLINE mkRestoreStatus #-}

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
stSnapshotIdentifier = lens _stSnapshotIdentifier (\s a -> s { _stSnapshotIdentifier = a })
{-# INLINE stSnapshotIdentifier #-}

-- | The identifier of the cluster for which the snapshot was taken.
stClusterIdentifier :: Lens' Snapshot (Maybe Text)
stClusterIdentifier = lens _stClusterIdentifier (\s a -> s { _stClusterIdentifier = a })
{-# INLINE stClusterIdentifier #-}

-- | The time (UTC) when Amazon Redshift began the snapshot. A snapshot contains
-- a copy of the cluster data as of this exact time.
stSnapshotCreateTime :: Lens' Snapshot (Maybe ISO8601)
stSnapshotCreateTime = lens _stSnapshotCreateTime (\s a -> s { _stSnapshotCreateTime = a })
{-# INLINE stSnapshotCreateTime #-}

-- | The snapshot status. The value of the status depends on the API operation
-- used. CreateClusterSnapshot and CopyClusterSnapshot returns status as
-- "creating". DescribeClusterSnapshots returns status as "creating",
-- "available", "final snapshot", or "failed". DeleteClusterSnapshot returns
-- status as "deleted".
stStatus :: Lens' Snapshot (Maybe Text)
stStatus = lens _stStatus (\s a -> s { _stStatus = a })
{-# INLINE stStatus #-}

-- | The port that the cluster is listening on.
stPort :: Lens' Snapshot (Maybe Integer)
stPort = lens _stPort (\s a -> s { _stPort = a })
{-# INLINE stPort #-}

-- | The Availability Zone in which the cluster was created.
stAvailabilityZone :: Lens' Snapshot (Maybe Text)
stAvailabilityZone = lens _stAvailabilityZone (\s a -> s { _stAvailabilityZone = a })
{-# INLINE stAvailabilityZone #-}

-- | The time (UTC) when the cluster was originally created.
stClusterCreateTime :: Lens' Snapshot (Maybe ISO8601)
stClusterCreateTime = lens _stClusterCreateTime (\s a -> s { _stClusterCreateTime = a })
{-# INLINE stClusterCreateTime #-}

-- | The master user name for the cluster.
stMasterUsername :: Lens' Snapshot (Maybe Text)
stMasterUsername = lens _stMasterUsername (\s a -> s { _stMasterUsername = a })
{-# INLINE stMasterUsername #-}

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
stClusterVersion :: Lens' Snapshot (Maybe Text)
stClusterVersion = lens _stClusterVersion (\s a -> s { _stClusterVersion = a })
{-# INLINE stClusterVersion #-}

-- | The snapshot type. Snapshots created using CreateClusterSnapshot and
-- CopyClusterSnapshot will be of type "manual".
stSnapshotType :: Lens' Snapshot (Maybe Text)
stSnapshotType = lens _stSnapshotType (\s a -> s { _stSnapshotType = a })
{-# INLINE stSnapshotType #-}

-- | The node type of the nodes in the cluster.
stNodeType :: Lens' Snapshot (Maybe Text)
stNodeType = lens _stNodeType (\s a -> s { _stNodeType = a })
{-# INLINE stNodeType #-}

-- | The number of nodes in the cluster.
stNumberOfNodes :: Lens' Snapshot (Maybe Integer)
stNumberOfNodes = lens _stNumberOfNodes (\s a -> s { _stNumberOfNodes = a })
{-# INLINE stNumberOfNodes #-}

-- | The name of the database that was created when the cluster was created.
stDBName :: Lens' Snapshot (Maybe Text)
stDBName = lens _stDBName (\s a -> s { _stDBName = a })
{-# INLINE stDBName #-}

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a
-- VPC. Otherwise, this field is not in the output.
stVpcId :: Lens' Snapshot (Maybe Text)
stVpcId = lens _stVpcId (\s a -> s { _stVpcId = a })
{-# INLINE stVpcId #-}

-- | If true, the data in the snapshot is encrypted at rest.
stEncrypted :: Lens' Snapshot (Maybe Bool)
stEncrypted = lens _stEncrypted (\s a -> s { _stEncrypted = a })
{-# INLINE stEncrypted #-}

-- | A boolean that indicates whether the snapshot data is encrypted using the
-- HSM keys of the source cluster. true indicates that the data is encrypted
-- using HSM keys.
stEncryptedWithHSM :: Lens' Snapshot (Maybe Bool)
stEncryptedWithHSM = lens _stEncryptedWithHSM (\s a -> s { _stEncryptedWithHSM = a })
{-# INLINE stEncryptedWithHSM #-}

-- | A list of the AWS customer accounts authorized to restore the snapshot.
-- Returns null if no accounts are authorized. Visible only to the snapshot
-- owner.
stAccountsWithRestoreAccess :: Lens' Snapshot ([AccountWithRestoreAccess])
stAccountsWithRestoreAccess = lens _stAccountsWithRestoreAccess (\s a -> s { _stAccountsWithRestoreAccess = a })
{-# INLINE stAccountsWithRestoreAccess #-}

-- | For manual snapshots, the AWS customer account used to create or copy the
-- snapshot. For automatic snapshots, the owner of the cluster. The owner can
-- perform all snapshot actions, such as sharing a manual snapshot.
stOwnerAccount :: Lens' Snapshot (Maybe Text)
stOwnerAccount = lens _stOwnerAccount (\s a -> s { _stOwnerAccount = a })
{-# INLINE stOwnerAccount #-}

-- | The size of the complete set of backup data that would be used to restore
-- the cluster.
stTotalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
stTotalBackupSizeInMegaBytes = lens _stTotalBackupSizeInMegaBytes (\s a -> s { _stTotalBackupSizeInMegaBytes = a })
{-# INLINE stTotalBackupSizeInMegaBytes #-}

-- | The size of the incremental backup.
stActualIncrementalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
stActualIncrementalBackupSizeInMegaBytes = lens _stActualIncrementalBackupSizeInMegaBytes (\s a -> s { _stActualIncrementalBackupSizeInMegaBytes = a })
{-# INLINE stActualIncrementalBackupSizeInMegaBytes #-}

-- | The number of megabytes that have been transferred to the snapshot backup.
stBackupProgressInMegaBytes :: Lens' Snapshot (Maybe Double)
stBackupProgressInMegaBytes = lens _stBackupProgressInMegaBytes (\s a -> s { _stBackupProgressInMegaBytes = a })
{-# INLINE stBackupProgressInMegaBytes #-}

-- | The number of megabytes per second being transferred to the snapshot
-- backup. Returns 0 for a completed backup.
stCurrentBackupRateInMegaBytesPerSecond :: Lens' Snapshot (Maybe Double)
stCurrentBackupRateInMegaBytesPerSecond = lens _stCurrentBackupRateInMegaBytesPerSecond (\s a -> s { _stCurrentBackupRateInMegaBytesPerSecond = a })
{-# INLINE stCurrentBackupRateInMegaBytesPerSecond #-}

-- | The estimate of the time remaining before the snapshot backup will
-- complete. Returns 0 for a completed backup.
stEstimatedSecondsToCompletion :: Lens' Snapshot (Maybe Integer)
stEstimatedSecondsToCompletion = lens _stEstimatedSecondsToCompletion (\s a -> s { _stEstimatedSecondsToCompletion = a })
{-# INLINE stEstimatedSecondsToCompletion #-}

-- | The amount of time an in-progress snapshot backup has been running, or the
-- amount of time it took a completed backup to finish.
stElapsedTimeInSeconds :: Lens' Snapshot (Maybe Integer)
stElapsedTimeInSeconds = lens _stElapsedTimeInSeconds (\s a -> s { _stElapsedTimeInSeconds = a })
{-# INLINE stElapsedTimeInSeconds #-}

-- | The source region from which the snapshot was copied.
stSourceRegion :: Lens' Snapshot (Maybe Text)
stSourceRegion = lens _stSourceRegion (\s a -> s { _stSourceRegion = a })
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
sssssssuSubnetIdentifier = lens _sssssssuSubnetIdentifier (\s a -> s { _sssssssuSubnetIdentifier = a })
{-# INLINE sssssssuSubnetIdentifier #-}

-- | Describes an availability zone.
sssssssuSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sssssssuSubnetAvailabilityZone = lens _sssssssuSubnetAvailabilityZone (\s a -> s { _sssssssuSubnetAvailabilityZone = a })
{-# INLINE sssssssuSubnetAvailabilityZone #-}

-- | The status of the subnet.
sssssssuSubnetStatus :: Lens' Subnet (Maybe Text)
sssssssuSubnetStatus = lens _sssssssuSubnetStatus (\s a -> s { _sssssssuSubnetStatus = a })
{-# INLINE sssssssuSubnetStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Subnet' data type to populate a request.
mkSubnet :: Subnet
mkSubnet = Subnet
    { _sssssssuSubnetIdentifier = Nothing
    , _sssssssuSubnetAvailabilityZone = Nothing
    , _sssssssuSubnetStatus = Nothing
    }
{-# INLINE mkSubnet #-}

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
vsgmVpcSecurityGroupId = lens _vsgmVpcSecurityGroupId (\s a -> s { _vsgmVpcSecurityGroupId = a })
{-# INLINE vsgmVpcSecurityGroupId #-}

-- | 
vsgmStatus :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\s a -> s { _vsgmStatus = a })
{-# INLINE vsgmStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcSecurityGroupMembership' data type to populate a request.
mkVpcSecurityGroupMembership :: VpcSecurityGroupMembership
mkVpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmVpcSecurityGroupId = Nothing
    , _vsgmStatus = Nothing
    }
{-# INLINE mkVpcSecurityGroupMembership #-}

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroup"

instance ToQuery VpcSecurityGroupMembership where
    toQuery = genericQuery def
