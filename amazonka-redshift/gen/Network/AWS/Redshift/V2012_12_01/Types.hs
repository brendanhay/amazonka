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
awraAccountId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AccountWithRestoreAccess
    -> f AccountWithRestoreAccess
awraAccountId f x =
    (\y -> x { _awraAccountId = y })
       <$> f (_awraAccountId x)
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
azName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AvailabilityZone
    -> f AvailabilityZone
azName f x =
    (\y -> x { _azName = y })
       <$> f (_azName x)
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
crClusterIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crClusterIdentifier f x =
    (\y -> x { _crClusterIdentifier = y })
       <$> f (_crClusterIdentifier x)
{-# INLINE crClusterIdentifier #-}

-- | The node type for the nodes in the cluster.
crNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crNodeType f x =
    (\y -> x { _crNodeType = y })
       <$> f (_crNodeType x)
{-# INLINE crNodeType #-}

-- | The current state of this cluster. Possible values include available,
-- creating, deleting, rebooting, renaming, and resizing.
crClusterStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crClusterStatus f x =
    (\y -> x { _crClusterStatus = y })
       <$> f (_crClusterStatus x)
{-# INLINE crClusterStatus #-}

-- | The status of a modify operation, if any, initiated for the cluster.
crModifyStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crModifyStatus f x =
    (\y -> x { _crModifyStatus = y })
       <$> f (_crModifyStatus x)
{-# INLINE crModifyStatus #-}

-- | The master user name for the cluster. This name is used to connect to the
-- database that is specified in DBName.
crMasterUsername
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crMasterUsername f x =
    (\y -> x { _crMasterUsername = y })
       <$> f (_crMasterUsername x)
{-# INLINE crMasterUsername #-}

-- | The name of the initial database that was created when the cluster was
-- created. This same name is returned for the life of the cluster. If an
-- initial database was not specified, a database named "dev" was created by
-- default.
crDBName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crDBName f x =
    (\y -> x { _crDBName = y })
       <$> f (_crDBName x)
{-# INLINE crDBName #-}

-- | The connection endpoint.
crEndpoint
    :: Functor f
    => (Maybe Endpoint
    -> f (Maybe Endpoint))
    -> Cluster
    -> f Cluster
crEndpoint f x =
    (\y -> x { _crEndpoint = y })
       <$> f (_crEndpoint x)
{-# INLINE crEndpoint #-}

-- | The date and time that the cluster was created.
crClusterCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Cluster
    -> f Cluster
crClusterCreateTime f x =
    (\y -> x { _crClusterCreateTime = y })
       <$> f (_crClusterCreateTime x)
{-# INLINE crClusterCreateTime #-}

-- | The number of days that automatic cluster snapshots are retained.
crAutomatedSnapshotRetentionPeriod
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Cluster
    -> f Cluster
crAutomatedSnapshotRetentionPeriod f x =
    (\y -> x { _crAutomatedSnapshotRetentionPeriod = y })
       <$> f (_crAutomatedSnapshotRetentionPeriod x)
{-# INLINE crAutomatedSnapshotRetentionPeriod #-}

-- | A list of cluster security group that are associated with the cluster. Each
-- security group is represented by an element that contains
-- ClusterSecurityGroup.Name and ClusterSecurityGroup.Status subelements.
-- Cluster security groups are used when the cluster is not created in a VPC.
-- Clusters that are created in a VPC use VPC security groups, which are
-- listed by the VpcSecurityGroups parameter.
crClusterSecurityGroups
    :: Functor f
    => ([ClusterSecurityGroupMembership]
    -> f ([ClusterSecurityGroupMembership]))
    -> Cluster
    -> f Cluster
crClusterSecurityGroups f x =
    (\y -> x { _crClusterSecurityGroups = y })
       <$> f (_crClusterSecurityGroups x)
{-# INLINE crClusterSecurityGroups #-}

-- | A list of Virtual Private Cloud (VPC) security groups that are associated
-- with the cluster. This parameter is returned only if the cluster is in a
-- VPC.
crVpcSecurityGroups
    :: Functor f
    => ([VpcSecurityGroupMembership]
    -> f ([VpcSecurityGroupMembership]))
    -> Cluster
    -> f Cluster
crVpcSecurityGroups f x =
    (\y -> x { _crVpcSecurityGroups = y })
       <$> f (_crVpcSecurityGroups x)
{-# INLINE crVpcSecurityGroups #-}

-- | The list of cluster parameter groups that are associated with this cluster.
crClusterParameterGroups
    :: Functor f
    => ([ClusterParameterGroupStatus]
    -> f ([ClusterParameterGroupStatus]))
    -> Cluster
    -> f Cluster
crClusterParameterGroups f x =
    (\y -> x { _crClusterParameterGroups = y })
       <$> f (_crClusterParameterGroups x)
{-# INLINE crClusterParameterGroups #-}

-- | The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
crClusterSubnetGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crClusterSubnetGroupName f x =
    (\y -> x { _crClusterSubnetGroupName = y })
       <$> f (_crClusterSubnetGroupName x)
{-# INLINE crClusterSubnetGroupName #-}

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
crVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crVpcId f x =
    (\y -> x { _crVpcId = y })
       <$> f (_crVpcId x)
{-# INLINE crVpcId #-}

-- | The name of the Availability Zone in which the cluster is located.
crAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crAvailabilityZone f x =
    (\y -> x { _crAvailabilityZone = y })
       <$> f (_crAvailabilityZone x)
{-# INLINE crAvailabilityZone #-}

-- | The weekly time range (in UTC) during which system maintenance can occur.
crPreferredMaintenanceWindow
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crPreferredMaintenanceWindow f x =
    (\y -> x { _crPreferredMaintenanceWindow = y })
       <$> f (_crPreferredMaintenanceWindow x)
{-# INLINE crPreferredMaintenanceWindow #-}

-- | If present, changes to the cluster are pending. Specific pending changes
-- are identified by subelements.
crPendingModifiedValues
    :: Functor f
    => (Maybe PendingModifiedValues
    -> f (Maybe PendingModifiedValues))
    -> Cluster
    -> f Cluster
crPendingModifiedValues f x =
    (\y -> x { _crPendingModifiedValues = y })
       <$> f (_crPendingModifiedValues x)
{-# INLINE crPendingModifiedValues #-}

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
crClusterVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crClusterVersion f x =
    (\y -> x { _crClusterVersion = y })
       <$> f (_crClusterVersion x)
{-# INLINE crClusterVersion #-}

-- | If true, version upgrades will be applied automatically to the cluster
-- during the maintenance window.
crAllowVersionUpgrade
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Cluster
    -> f Cluster
crAllowVersionUpgrade f x =
    (\y -> x { _crAllowVersionUpgrade = y })
       <$> f (_crAllowVersionUpgrade x)
{-# INLINE crAllowVersionUpgrade #-}

-- | The number of compute nodes in the cluster.
crNumberOfNodes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Cluster
    -> f Cluster
crNumberOfNodes f x =
    (\y -> x { _crNumberOfNodes = y })
       <$> f (_crNumberOfNodes x)
{-# INLINE crNumberOfNodes #-}

-- | If true, the cluster can be accessed from a public network.
crPubliclyAccessible
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Cluster
    -> f Cluster
crPubliclyAccessible f x =
    (\y -> x { _crPubliclyAccessible = y })
       <$> f (_crPubliclyAccessible x)
{-# INLINE crPubliclyAccessible #-}

-- | If true, data in the cluster is encrypted at rest.
crEncrypted
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Cluster
    -> f Cluster
crEncrypted f x =
    (\y -> x { _crEncrypted = y })
       <$> f (_crEncrypted x)
{-# INLINE crEncrypted #-}

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
crRestoreStatus
    :: Functor f
    => (Maybe RestoreStatus
    -> f (Maybe RestoreStatus))
    -> Cluster
    -> f Cluster
crRestoreStatus f x =
    (\y -> x { _crRestoreStatus = y })
       <$> f (_crRestoreStatus x)
{-# INLINE crRestoreStatus #-}

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
crHsmStatus
    :: Functor f
    => (Maybe HsmStatus
    -> f (Maybe HsmStatus))
    -> Cluster
    -> f Cluster
crHsmStatus f x =
    (\y -> x { _crHsmStatus = y })
       <$> f (_crHsmStatus x)
{-# INLINE crHsmStatus #-}

-- | Returns the destination region and retention period that are configured for
-- cross-region snapshot copy.
crClusterSnapshotCopyStatus
    :: Functor f
    => (Maybe ClusterSnapshotCopyStatus
    -> f (Maybe ClusterSnapshotCopyStatus))
    -> Cluster
    -> f Cluster
crClusterSnapshotCopyStatus f x =
    (\y -> x { _crClusterSnapshotCopyStatus = y })
       <$> f (_crClusterSnapshotCopyStatus x)
{-# INLINE crClusterSnapshotCopyStatus #-}

-- | The public key for the cluster.
crClusterPublicKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crClusterPublicKey f x =
    (\y -> x { _crClusterPublicKey = y })
       <$> f (_crClusterPublicKey x)
{-# INLINE crClusterPublicKey #-}

-- | The nodes in a cluster.
crClusterNodes
    :: Functor f
    => ([ClusterNode]
    -> f ([ClusterNode]))
    -> Cluster
    -> f Cluster
crClusterNodes f x =
    (\y -> x { _crClusterNodes = y })
       <$> f (_crClusterNodes x)
{-# INLINE crClusterNodes #-}

-- | Describes the status of the elastic IP (EIP) address.
crElasticIpStatus
    :: Functor f
    => (Maybe ElasticIpStatus
    -> f (Maybe ElasticIpStatus))
    -> Cluster
    -> f Cluster
crElasticIpStatus f x =
    (\y -> x { _crElasticIpStatus = y })
       <$> f (_crElasticIpStatus x)
{-# INLINE crElasticIpStatus #-}

-- | The specific revision number of the database in the cluster.
crClusterRevisionNumber
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
crClusterRevisionNumber f x =
    (\y -> x { _crClusterRevisionNumber = y })
       <$> f (_crClusterRevisionNumber x)
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
cnNodeRole
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterNode
    -> f ClusterNode
cnNodeRole f x =
    (\y -> x { _cnNodeRole = y })
       <$> f (_cnNodeRole x)
{-# INLINE cnNodeRole #-}

-- | The private IP address of a node within a cluster.
cnPrivateIPAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterNode
    -> f ClusterNode
cnPrivateIPAddress f x =
    (\y -> x { _cnPrivateIPAddress = y })
       <$> f (_cnPrivateIPAddress x)
{-# INLINE cnPrivateIPAddress #-}

-- | The public IP address of a node within a cluster.
cnPublicIPAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterNode
    -> f ClusterNode
cnPublicIPAddress f x =
    (\y -> x { _cnPublicIPAddress = y })
       <$> f (_cnPublicIPAddress x)
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
cpgParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterParameterGroup
    -> f ClusterParameterGroup
cpgParameterGroupName f x =
    (\y -> x { _cpgParameterGroupName = y })
       <$> f (_cpgParameterGroupName x)
{-# INLINE cpgParameterGroupName #-}

-- | The name of the cluster parameter group family that this cluster parameter
-- group is compatible with.
cpgParameterGroupFamily
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterParameterGroup
    -> f ClusterParameterGroup
cpgParameterGroupFamily f x =
    (\y -> x { _cpgParameterGroupFamily = y })
       <$> f (_cpgParameterGroupFamily x)
{-# INLINE cpgParameterGroupFamily #-}

-- | The description of the parameter group.
cpgDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterParameterGroup
    -> f ClusterParameterGroup
cpgDescription f x =
    (\y -> x { _cpgDescription = y })
       <$> f (_cpgDescription x)
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
cpgsParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterParameterGroupStatus
    -> f ClusterParameterGroupStatus
cpgsParameterGroupName f x =
    (\y -> x { _cpgsParameterGroupName = y })
       <$> f (_cpgsParameterGroupName x)
{-# INLINE cpgsParameterGroupName #-}

-- | The status of parameter updates.
cpgsParameterApplyStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterParameterGroupStatus
    -> f ClusterParameterGroupStatus
cpgsParameterApplyStatus f x =
    (\y -> x { _cpgsParameterApplyStatus = y })
       <$> f (_cpgsParameterApplyStatus x)
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
csgClusterSecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSecurityGroup
    -> f ClusterSecurityGroup
csgClusterSecurityGroupName f x =
    (\y -> x { _csgClusterSecurityGroupName = y })
       <$> f (_csgClusterSecurityGroupName x)
{-# INLINE csgClusterSecurityGroupName #-}

-- | A description of the security group.
csgDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSecurityGroup
    -> f ClusterSecurityGroup
csgDescription f x =
    (\y -> x { _csgDescription = y })
       <$> f (_csgDescription x)
{-# INLINE csgDescription #-}

-- | A list of EC2 security groups that are permitted to access clusters
-- associated with this cluster security group.
csgEC2SecurityGroups
    :: Functor f
    => ([EC2SecurityGroup]
    -> f ([EC2SecurityGroup]))
    -> ClusterSecurityGroup
    -> f ClusterSecurityGroup
csgEC2SecurityGroups f x =
    (\y -> x { _csgEC2SecurityGroups = y })
       <$> f (_csgEC2SecurityGroups x)
{-# INLINE csgEC2SecurityGroups #-}

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters
-- associated with this cluster security group.
csgIPRanges
    :: Functor f
    => ([IPRange]
    -> f ([IPRange]))
    -> ClusterSecurityGroup
    -> f ClusterSecurityGroup
csgIPRanges f x =
    (\y -> x { _csgIPRanges = y })
       <$> f (_csgIPRanges x)
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
csgmClusterSecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSecurityGroupMembership
    -> f ClusterSecurityGroupMembership
csgmClusterSecurityGroupName f x =
    (\y -> x { _csgmClusterSecurityGroupName = y })
       <$> f (_csgmClusterSecurityGroupName x)
{-# INLINE csgmClusterSecurityGroupName #-}

-- | The status of the cluster security group.
csgmStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSecurityGroupMembership
    -> f ClusterSecurityGroupMembership
csgmStatus f x =
    (\y -> x { _csgmStatus = y })
       <$> f (_csgmStatus x)
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
cscsDestinationRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSnapshotCopyStatus
    -> f ClusterSnapshotCopyStatus
cscsDestinationRegion f x =
    (\y -> x { _cscsDestinationRegion = y })
       <$> f (_cscsDestinationRegion x)
{-# INLINE cscsDestinationRegion #-}

-- | The number of days that automated snapshots are retained in the destination
-- region after they are copied from a source region.
cscsRetentionPeriod
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ClusterSnapshotCopyStatus
    -> f ClusterSnapshotCopyStatus
cscsRetentionPeriod f x =
    (\y -> x { _cscsRetentionPeriod = y })
       <$> f (_cscsRetentionPeriod x)
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
csiClusterSubnetGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSubnetGroup
    -> f ClusterSubnetGroup
csiClusterSubnetGroupName f x =
    (\y -> x { _csiClusterSubnetGroupName = y })
       <$> f (_csiClusterSubnetGroupName x)
{-# INLINE csiClusterSubnetGroupName #-}

-- | The description of the cluster subnet group.
csiDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSubnetGroup
    -> f ClusterSubnetGroup
csiDescription f x =
    (\y -> x { _csiDescription = y })
       <$> f (_csiDescription x)
{-# INLINE csiDescription #-}

-- | The VPC ID of the cluster subnet group.
csiVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSubnetGroup
    -> f ClusterSubnetGroup
csiVpcId f x =
    (\y -> x { _csiVpcId = y })
       <$> f (_csiVpcId x)
{-# INLINE csiVpcId #-}

-- | The status of the cluster subnet group. Possible values are Complete,
-- Incomplete and Invalid.
csiSubnetGroupStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSubnetGroup
    -> f ClusterSubnetGroup
csiSubnetGroupStatus f x =
    (\y -> x { _csiSubnetGroupStatus = y })
       <$> f (_csiSubnetGroupStatus x)
{-# INLINE csiSubnetGroupStatus #-}

-- | A list of the VPC Subnet elements.
csiSubnets
    :: Functor f
    => ([Subnet]
    -> f ([Subnet]))
    -> ClusterSubnetGroup
    -> f ClusterSubnetGroup
csiSubnets f x =
    (\y -> x { _csiSubnets = y })
       <$> f (_csiSubnets x)
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
cvClusterVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterVersion
    -> f ClusterVersion
cvClusterVersion f x =
    (\y -> x { _cvClusterVersion = y })
       <$> f (_cvClusterVersion x)
{-# INLINE cvClusterVersion #-}

-- | The name of the cluster parameter group family for the cluster.
cvClusterParameterGroupFamily
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterVersion
    -> f ClusterVersion
cvClusterParameterGroupFamily f x =
    (\y -> x { _cvClusterParameterGroupFamily = y })
       <$> f (_cvClusterParameterGroupFamily x)
{-# INLINE cvClusterParameterGroupFamily #-}

-- | The description of the cluster version.
cvDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterVersion
    -> f ClusterVersion
cvDescription f x =
    (\y -> x { _cvDescription = y })
       <$> f (_cvDescription x)
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
dcpParameterGroupFamily
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DefaultClusterParameters
    -> f DefaultClusterParameters
dcpParameterGroupFamily f x =
    (\y -> x { _dcpParameterGroupFamily = y })
       <$> f (_dcpParameterGroupFamily x)
{-# INLINE dcpParameterGroupFamily #-}

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dcpMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DefaultClusterParameters
    -> f DefaultClusterParameters
dcpMarker f x =
    (\y -> x { _dcpMarker = y })
       <$> f (_dcpMarker x)
{-# INLINE dcpMarker #-}

-- | The list of cluster default parameters.
dcpParameters
    :: Functor f
    => ([Parameter]
    -> f ([Parameter]))
    -> DefaultClusterParameters
    -> f DefaultClusterParameters
dcpParameters f x =
    (\y -> x { _dcpParameters = y })
       <$> f (_dcpParameters x)
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
ecsgStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EC2SecurityGroup
    -> f EC2SecurityGroup
ecsgStatus f x =
    (\y -> x { _ecsgStatus = y })
       <$> f (_ecsgStatus x)
{-# INLINE ecsgStatus #-}

-- | The name of the EC2 Security Group.
ecsgEC2SecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EC2SecurityGroup
    -> f EC2SecurityGroup
ecsgEC2SecurityGroupName f x =
    (\y -> x { _ecsgEC2SecurityGroupName = y })
       <$> f (_ecsgEC2SecurityGroupName x)
{-# INLINE ecsgEC2SecurityGroupName #-}

-- | The AWS ID of the owner of the EC2 security group specified in the
-- EC2SecurityGroupName field.
ecsgEC2SecurityGroupOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EC2SecurityGroup
    -> f EC2SecurityGroup
ecsgEC2SecurityGroupOwnerId f x =
    (\y -> x { _ecsgEC2SecurityGroupOwnerId = y })
       <$> f (_ecsgEC2SecurityGroupOwnerId x)
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
eisElasticIp
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticIpStatus
    -> f ElasticIpStatus
eisElasticIp f x =
    (\y -> x { _eisElasticIp = y })
       <$> f (_eisElasticIp x)
{-# INLINE eisElasticIp #-}

-- | Describes the status of the elastic IP (EIP) address.
eisStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticIpStatus
    -> f ElasticIpStatus
eisStatus f x =
    (\y -> x { _eisStatus = y })
       <$> f (_eisStatus x)
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
etAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Endpoint
    -> f Endpoint
etAddress f x =
    (\y -> x { _etAddress = y })
       <$> f (_etAddress x)
{-# INLINE etAddress #-}

-- | The port that the database engine is listening on.
etPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Endpoint
    -> f Endpoint
etPort f x =
    (\y -> x { _etPort = y })
       <$> f (_etPort x)
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
exSourceIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Event
    -> f Event
exSourceIdentifier f x =
    (\y -> x { _exSourceIdentifier = y })
       <$> f (_exSourceIdentifier x)
{-# INLINE exSourceIdentifier #-}

-- | The source type for this event.
exSourceType
    :: Functor f
    => (Maybe SourceType
    -> f (Maybe SourceType))
    -> Event
    -> f Event
exSourceType f x =
    (\y -> x { _exSourceType = y })
       <$> f (_exSourceType x)
{-# INLINE exSourceType #-}

-- | The text of this event.
exMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Event
    -> f Event
exMessage f x =
    (\y -> x { _exMessage = y })
       <$> f (_exMessage x)
{-# INLINE exMessage #-}

-- | A list of the event categories.
exEventCategories
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Event
    -> f Event
exEventCategories f x =
    (\y -> x { _exEventCategories = y })
       <$> f (_exEventCategories x)
{-# INLINE exEventCategories #-}

-- | The severity of the event. Values: ERROR, INFO.
exSeverity
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Event
    -> f Event
exSeverity f x =
    (\y -> x { _exSeverity = y })
       <$> f (_exSeverity x)
{-# INLINE exSeverity #-}

-- | The date and time of the event.
exDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Event
    -> f Event
exDate f x =
    (\y -> x { _exDate = y })
       <$> f (_exDate x)
{-# INLINE exDate #-}

-- | The identifier of the event.
exEventId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Event
    -> f Event
exEventId f x =
    (\y -> x { _exEventId = y })
       <$> f (_exEventId x)
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
ecoSourceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventCategoriesMap
    -> f EventCategoriesMap
ecoSourceType f x =
    (\y -> x { _ecoSourceType = y })
       <$> f (_ecoSourceType x)
{-# INLINE ecoSourceType #-}

-- | The events in the event category.
ecoEvents
    :: Functor f
    => ([EventInfoMap]
    -> f ([EventInfoMap]))
    -> EventCategoriesMap
    -> f EventCategoriesMap
ecoEvents f x =
    (\y -> x { _ecoEvents = y })
       <$> f (_ecoEvents x)
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
eimEventId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventInfoMap
    -> f EventInfoMap
eimEventId f x =
    (\y -> x { _eimEventId = y })
       <$> f (_eimEventId x)
{-# INLINE eimEventId #-}

-- | The category of an Amazon Redshift event.
eimEventCategories
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> EventInfoMap
    -> f EventInfoMap
eimEventCategories f x =
    (\y -> x { _eimEventCategories = y })
       <$> f (_eimEventCategories x)
{-# INLINE eimEventCategories #-}

-- | The description of an Amazon Redshift event.
eimEventDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventInfoMap
    -> f EventInfoMap
eimEventDescription f x =
    (\y -> x { _eimEventDescription = y })
       <$> f (_eimEventDescription x)
{-# INLINE eimEventDescription #-}

-- | The severity of the event. Values: ERROR, INFO.
eimSeverity
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventInfoMap
    -> f EventInfoMap
eimSeverity f x =
    (\y -> x { _eimSeverity = y })
       <$> f (_eimSeverity x)
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
esCustomerAwsId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esCustomerAwsId f x =
    (\y -> x { _esCustomerAwsId = y })
       <$> f (_esCustomerAwsId x)
{-# INLINE esCustomerAwsId #-}

-- | The name of the Amazon Redshift event notification subscription.
esCustSubscriptionId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esCustSubscriptionId f x =
    (\y -> x { _esCustSubscriptionId = y })
       <$> f (_esCustSubscriptionId x)
{-# INLINE esCustSubscriptionId #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
esSnsTopicArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esSnsTopicArn f x =
    (\y -> x { _esSnsTopicArn = y })
       <$> f (_esSnsTopicArn x)
{-# INLINE esSnsTopicArn #-}

-- | The status of the Amazon Redshift event notification subscription.
-- Constraints: Can be one of the following: active | no-permission |
-- topic-not-exist The status "no-permission" indicates that Amazon Redshift
-- no longer has permission to post to the Amazon SNS topic. The status
-- "topic-not-exist" indicates that the topic was deleted after the
-- subscription was created.
esStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esStatus f x =
    (\y -> x { _esStatus = y })
       <$> f (_esStatus x)
{-# INLINE esStatus #-}

-- | The date and time the Amazon Redshift event notification subscription was
-- created.
esSubscriptionCreationTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> EventSubscription
    -> f EventSubscription
esSubscriptionCreationTime f x =
    (\y -> x { _esSubscriptionCreationTime = y })
       <$> f (_esSubscriptionCreationTime x)
{-# INLINE esSubscriptionCreationTime #-}

-- | The source type of the events returned the Amazon Redshift event
-- notification, such as cluster, or cluster-snapshot.
esSourceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esSourceType f x =
    (\y -> x { _esSourceType = y })
       <$> f (_esSourceType x)
{-# INLINE esSourceType #-}

-- | A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
esSourceIdsList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> EventSubscription
    -> f EventSubscription
esSourceIdsList f x =
    (\y -> x { _esSourceIdsList = y })
       <$> f (_esSourceIdsList x)
{-# INLINE esSourceIdsList #-}

-- | The list of Amazon Redshift event categories specified in the event
-- notification subscription. Values: Configuration, Management, Monitoring,
-- Security.
esEventCategoriesList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> EventSubscription
    -> f EventSubscription
esEventCategoriesList f x =
    (\y -> x { _esEventCategoriesList = y })
       <$> f (_esEventCategoriesList x)
{-# INLINE esEventCategoriesList #-}

-- | The event severity specified in the Amazon Redshift event notification
-- subscription. Values: ERROR, INFO.
esSeverity
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esSeverity f x =
    (\y -> x { _esSeverity = y })
       <$> f (_esSeverity x)
{-# INLINE esSeverity #-}

-- | A Boolean value indicating whether the subscription is enabled. true
-- indicates the subscription is enabled.
esEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> EventSubscription
    -> f EventSubscription
esEnabled f x =
    (\y -> x { _esEnabled = y })
       <$> f (_esEnabled x)
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
hccHsmClientCertificateIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HsmClientCertificate
    -> f HsmClientCertificate
hccHsmClientCertificateIdentifier f x =
    (\y -> x { _hccHsmClientCertificateIdentifier = y })
       <$> f (_hccHsmClientCertificateIdentifier x)
{-# INLINE hccHsmClientCertificateIdentifier #-}

-- | The public key that the Amazon Redshift cluster will use to connect to the
-- HSM. You must register the public key in the HSM.
hccHsmClientCertificatePublicKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HsmClientCertificate
    -> f HsmClientCertificate
hccHsmClientCertificatePublicKey f x =
    (\y -> x { _hccHsmClientCertificatePublicKey = y })
       <$> f (_hccHsmClientCertificatePublicKey x)
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
hcHsmConfigurationIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HsmConfiguration
    -> f HsmConfiguration
hcHsmConfigurationIdentifier f x =
    (\y -> x { _hcHsmConfigurationIdentifier = y })
       <$> f (_hcHsmConfigurationIdentifier x)
{-# INLINE hcHsmConfigurationIdentifier #-}

-- | A text description of the HSM configuration.
hcDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HsmConfiguration
    -> f HsmConfiguration
hcDescription f x =
    (\y -> x { _hcDescription = y })
       <$> f (_hcDescription x)
{-# INLINE hcDescription #-}

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
hcHsmIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HsmConfiguration
    -> f HsmConfiguration
hcHsmIpAddress f x =
    (\y -> x { _hcHsmIpAddress = y })
       <$> f (_hcHsmIpAddress x)
{-# INLINE hcHsmIpAddress #-}

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
hcHsmPartitionName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HsmConfiguration
    -> f HsmConfiguration
hcHsmPartitionName f x =
    (\y -> x { _hcHsmPartitionName = y })
       <$> f (_hcHsmPartitionName x)
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
hsHsmClientCertificateIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HsmStatus
    -> f HsmStatus
hsHsmClientCertificateIdentifier f x =
    (\y -> x { _hsHsmClientCertificateIdentifier = y })
       <$> f (_hsHsmClientCertificateIdentifier x)
{-# INLINE hsHsmClientCertificateIdentifier #-}

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
hsHsmConfigurationIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HsmStatus
    -> f HsmStatus
hsHsmConfigurationIdentifier f x =
    (\y -> x { _hsHsmConfigurationIdentifier = y })
       <$> f (_hsHsmConfigurationIdentifier x)
{-# INLINE hsHsmConfigurationIdentifier #-}

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
hsStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HsmStatus
    -> f HsmStatus
hsStatus f x =
    (\y -> x { _hsStatus = y })
       <$> f (_hsStatus x)
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
iprStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> IPRange
    -> f IPRange
iprStatus f x =
    (\y -> x { _iprStatus = y })
       <$> f (_iprStatus x)
{-# INLINE iprStatus #-}

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
iprCIDRIP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> IPRange
    -> f IPRange
iprCIDRIP f x =
    (\y -> x { _iprCIDRIP = y })
       <$> f (_iprCIDRIP x)
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
ocoClusterVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OrderableClusterOption
    -> f OrderableClusterOption
ocoClusterVersion f x =
    (\y -> x { _ocoClusterVersion = y })
       <$> f (_ocoClusterVersion x)
{-# INLINE ocoClusterVersion #-}

-- | The cluster type, for example multi-node.
ocoClusterType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OrderableClusterOption
    -> f OrderableClusterOption
ocoClusterType f x =
    (\y -> x { _ocoClusterType = y })
       <$> f (_ocoClusterType x)
{-# INLINE ocoClusterType #-}

-- | The node type for the orderable cluster.
ocoNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OrderableClusterOption
    -> f OrderableClusterOption
ocoNodeType f x =
    (\y -> x { _ocoNodeType = y })
       <$> f (_ocoNodeType x)
{-# INLINE ocoNodeType #-}

-- | A list of availability zones for the orderable cluster.
ocoAvailabilityZones
    :: Functor f
    => ([AvailabilityZone]
    -> f ([AvailabilityZone]))
    -> OrderableClusterOption
    -> f OrderableClusterOption
ocoAvailabilityZones f x =
    (\y -> x { _ocoAvailabilityZones = y })
       <$> f (_ocoAvailabilityZones x)
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
prParameterName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prParameterName f x =
    (\y -> x { _prParameterName = y })
       <$> f (_prParameterName x)
{-# INLINE prParameterName #-}

-- | The value of the parameter.
prParameterValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prParameterValue f x =
    (\y -> x { _prParameterValue = y })
       <$> f (_prParameterValue x)
{-# INLINE prParameterValue #-}

-- | A description of the parameter.
prDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prDescription f x =
    (\y -> x { _prDescription = y })
       <$> f (_prDescription x)
{-# INLINE prDescription #-}

-- | The source of the parameter value, such as "engine-default" or "user".
prSource
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prSource f x =
    (\y -> x { _prSource = y })
       <$> f (_prSource x)
{-# INLINE prSource #-}

-- | The data type of the parameter.
prDataType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prDataType f x =
    (\y -> x { _prDataType = y })
       <$> f (_prDataType x)
{-# INLINE prDataType #-}

-- | The valid range of values for the parameter.
prAllowedValues
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prAllowedValues f x =
    (\y -> x { _prAllowedValues = y })
       <$> f (_prAllowedValues x)
{-# INLINE prAllowedValues #-}

-- | If true, the parameter can be modified. Some parameters have security or
-- operational implications that prevent them from being changed.
prIsModifiable
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Parameter
    -> f Parameter
prIsModifiable f x =
    (\y -> x { _prIsModifiable = y })
       <$> f (_prIsModifiable x)
{-# INLINE prIsModifiable #-}

-- | The earliest engine version to which the parameter can apply.
prMinimumEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prMinimumEngineVersion f x =
    (\y -> x { _prMinimumEngineVersion = y })
       <$> f (_prMinimumEngineVersion x)
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
pmvMasterUserPassword
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvMasterUserPassword f x =
    (\y -> x { _pmvMasterUserPassword = y })
       <$> f (_pmvMasterUserPassword x)
{-# INLINE pmvMasterUserPassword #-}

-- | The pending or in-progress change of the cluster's node type.
pmvNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvNodeType f x =
    (\y -> x { _pmvNodeType = y })
       <$> f (_pmvNodeType x)
{-# INLINE pmvNodeType #-}

-- | The pending or in-progress change of the number of nodes in the cluster.
pmvNumberOfNodes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvNumberOfNodes f x =
    (\y -> x { _pmvNumberOfNodes = y })
       <$> f (_pmvNumberOfNodes x)
{-# INLINE pmvNumberOfNodes #-}

-- | The pending or in-progress change of the cluster type.
pmvClusterType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvClusterType f x =
    (\y -> x { _pmvClusterType = y })
       <$> f (_pmvClusterType x)
{-# INLINE pmvClusterType #-}

-- | The pending or in-progress change of the service version.
pmvClusterVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvClusterVersion f x =
    (\y -> x { _pmvClusterVersion = y })
       <$> f (_pmvClusterVersion x)
{-# INLINE pmvClusterVersion #-}

-- | The pending or in-progress change of the automated snapshot retention
-- period.
pmvAutomatedSnapshotRetentionPeriod
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvAutomatedSnapshotRetentionPeriod f x =
    (\y -> x { _pmvAutomatedSnapshotRetentionPeriod = y })
       <$> f (_pmvAutomatedSnapshotRetentionPeriod x)
{-# INLINE pmvAutomatedSnapshotRetentionPeriod #-}

-- | The pending or in-progress change of the new identifier for the cluster.
pmvClusterIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvClusterIdentifier f x =
    (\y -> x { _pmvClusterIdentifier = y })
       <$> f (_pmvClusterIdentifier x)
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
rcRecurringChargeAmount
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> RecurringCharge
    -> f RecurringCharge
rcRecurringChargeAmount f x =
    (\y -> x { _rcRecurringChargeAmount = y })
       <$> f (_rcRecurringChargeAmount x)
{-# INLINE rcRecurringChargeAmount #-}

-- | The frequency at which the recurring charge amount is applied.
rcRecurringChargeFrequency
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RecurringCharge
    -> f RecurringCharge
rcRecurringChargeFrequency f x =
    (\y -> x { _rcRecurringChargeFrequency = y })
       <$> f (_rcRecurringChargeFrequency x)
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
rnReservedNodeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedNode
    -> f ReservedNode
rnReservedNodeId f x =
    (\y -> x { _rnReservedNodeId = y })
       <$> f (_rnReservedNodeId x)
{-# INLINE rnReservedNodeId #-}

-- | The identifier for the reserved node offering.
rnReservedNodeOfferingId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedNode
    -> f ReservedNode
rnReservedNodeOfferingId f x =
    (\y -> x { _rnReservedNodeOfferingId = y })
       <$> f (_rnReservedNodeOfferingId x)
{-# INLINE rnReservedNodeOfferingId #-}

-- | The node type of the reserved node.
rnNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedNode
    -> f ReservedNode
rnNodeType f x =
    (\y -> x { _rnNodeType = y })
       <$> f (_rnNodeType x)
{-# INLINE rnNodeType #-}

-- | The time the reservation started. You purchase a reserved node offering for
-- a duration. This is the start time of that duration.
rnStartTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ReservedNode
    -> f ReservedNode
rnStartTime f x =
    (\y -> x { _rnStartTime = y })
       <$> f (_rnStartTime x)
{-# INLINE rnStartTime #-}

-- | The duration of the node reservation in seconds.
rnDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedNode
    -> f ReservedNode
rnDuration f x =
    (\y -> x { _rnDuration = y })
       <$> f (_rnDuration x)
{-# INLINE rnDuration #-}

-- | The fixed cost Amazon Redshift charged you for this reserved node.
rnFixedPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedNode
    -> f ReservedNode
rnFixedPrice f x =
    (\y -> x { _rnFixedPrice = y })
       <$> f (_rnFixedPrice x)
{-# INLINE rnFixedPrice #-}

-- | The hourly rate Amazon Redshift charge you for this reserved node.
rnUsagePrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedNode
    -> f ReservedNode
rnUsagePrice f x =
    (\y -> x { _rnUsagePrice = y })
       <$> f (_rnUsagePrice x)
{-# INLINE rnUsagePrice #-}

-- | The currency code for the reserved cluster.
rnCurrencyCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedNode
    -> f ReservedNode
rnCurrencyCode f x =
    (\y -> x { _rnCurrencyCode = y })
       <$> f (_rnCurrencyCode x)
{-# INLINE rnCurrencyCode #-}

-- | The number of reserved compute nodes.
rnNodeCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedNode
    -> f ReservedNode
rnNodeCount f x =
    (\y -> x { _rnNodeCount = y })
       <$> f (_rnNodeCount x)
{-# INLINE rnNodeCount #-}

-- | The state of the reserved Compute Node. Possible Values:
-- pending-payment-This reserved node has recently been purchased, and the
-- sale has been approved, but payment has not yet been confirmed. active-This
-- reserved node is owned by the caller and is available for use.
-- payment-failed-Payment failed for the purchase attempt.
rnState
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedNode
    -> f ReservedNode
rnState f x =
    (\y -> x { _rnState = y })
       <$> f (_rnState x)
{-# INLINE rnState #-}

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
rnOfferingType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedNode
    -> f ReservedNode
rnOfferingType f x =
    (\y -> x { _rnOfferingType = y })
       <$> f (_rnOfferingType x)
{-# INLINE rnOfferingType #-}

-- | The recurring charges for the reserved node.
rnRecurringCharges
    :: Functor f
    => ([RecurringCharge]
    -> f ([RecurringCharge]))
    -> ReservedNode
    -> f ReservedNode
rnRecurringCharges f x =
    (\y -> x { _rnRecurringCharges = y })
       <$> f (_rnRecurringCharges x)
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
rnoReservedNodeOfferingId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedNodeOffering
    -> f ReservedNodeOffering
rnoReservedNodeOfferingId f x =
    (\y -> x { _rnoReservedNodeOfferingId = y })
       <$> f (_rnoReservedNodeOfferingId x)
{-# INLINE rnoReservedNodeOfferingId #-}

-- | The node type offered by the reserved node offering.
rnoNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedNodeOffering
    -> f ReservedNodeOffering
rnoNodeType f x =
    (\y -> x { _rnoNodeType = y })
       <$> f (_rnoNodeType x)
{-# INLINE rnoNodeType #-}

-- | The duration, in seconds, for which the offering will reserve the node.
rnoDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedNodeOffering
    -> f ReservedNodeOffering
rnoDuration f x =
    (\y -> x { _rnoDuration = y })
       <$> f (_rnoDuration x)
{-# INLINE rnoDuration #-}

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- node offering.
rnoFixedPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedNodeOffering
    -> f ReservedNodeOffering
rnoFixedPrice f x =
    (\y -> x { _rnoFixedPrice = y })
       <$> f (_rnoFixedPrice x)
{-# INLINE rnoFixedPrice #-}

-- | The rate you are charged for each hour the cluster that is using the
-- offering is running.
rnoUsagePrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedNodeOffering
    -> f ReservedNodeOffering
rnoUsagePrice f x =
    (\y -> x { _rnoUsagePrice = y })
       <$> f (_rnoUsagePrice x)
{-# INLINE rnoUsagePrice #-}

-- | The currency code for the compute nodes offering.
rnoCurrencyCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedNodeOffering
    -> f ReservedNodeOffering
rnoCurrencyCode f x =
    (\y -> x { _rnoCurrencyCode = y })
       <$> f (_rnoCurrencyCode x)
{-# INLINE rnoCurrencyCode #-}

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
rnoOfferingType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedNodeOffering
    -> f ReservedNodeOffering
rnoOfferingType f x =
    (\y -> x { _rnoOfferingType = y })
       <$> f (_rnoOfferingType x)
{-# INLINE rnoOfferingType #-}

-- | The charge to your account regardless of whether you are creating any
-- clusters using the node offering. Recurring charges are only in effect for
-- heavy-utilization reserved nodes.
rnoRecurringCharges
    :: Functor f
    => ([RecurringCharge]
    -> f ([RecurringCharge]))
    -> ReservedNodeOffering
    -> f ReservedNodeOffering
rnoRecurringCharges f x =
    (\y -> x { _rnoRecurringCharges = y })
       <$> f (_rnoRecurringCharges x)
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
rsStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RestoreStatus
    -> f RestoreStatus
rsStatus f x =
    (\y -> x { _rsStatus = y })
       <$> f (_rsStatus x)
{-# INLINE rsStatus #-}

-- | The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup.
rsCurrentRestoreRateInMegaBytesPerSecond
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> RestoreStatus
    -> f RestoreStatus
rsCurrentRestoreRateInMegaBytesPerSecond f x =
    (\y -> x { _rsCurrentRestoreRateInMegaBytesPerSecond = y })
       <$> f (_rsCurrentRestoreRateInMegaBytesPerSecond x)
{-# INLINE rsCurrentRestoreRateInMegaBytesPerSecond #-}

-- | The size of the set of snapshot data used to restore the cluster.
rsSnapshotSizeInMegaBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> RestoreStatus
    -> f RestoreStatus
rsSnapshotSizeInMegaBytes f x =
    (\y -> x { _rsSnapshotSizeInMegaBytes = y })
       <$> f (_rsSnapshotSizeInMegaBytes x)
{-# INLINE rsSnapshotSizeInMegaBytes #-}

-- | The number of megabytes that have been transferred from snapshot storage.
rsProgressInMegaBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> RestoreStatus
    -> f RestoreStatus
rsProgressInMegaBytes f x =
    (\y -> x { _rsProgressInMegaBytes = y })
       <$> f (_rsProgressInMegaBytes x)
{-# INLINE rsProgressInMegaBytes #-}

-- | The amount of time an in-progress restore has been running, or the amount
-- of time it took a completed restore to finish.
rsElapsedTimeInSeconds
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> RestoreStatus
    -> f RestoreStatus
rsElapsedTimeInSeconds f x =
    (\y -> x { _rsElapsedTimeInSeconds = y })
       <$> f (_rsElapsedTimeInSeconds x)
{-# INLINE rsElapsedTimeInSeconds #-}

-- | The estimate of the time remaining before the restore will complete.
-- Returns 0 for a completed restore.
rsEstimatedTimeToCompletionInSeconds
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> RestoreStatus
    -> f RestoreStatus
rsEstimatedTimeToCompletionInSeconds f x =
    (\y -> x { _rsEstimatedTimeToCompletionInSeconds = y })
       <$> f (_rsEstimatedTimeToCompletionInSeconds x)
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
stSnapshotIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stSnapshotIdentifier f x =
    (\y -> x { _stSnapshotIdentifier = y })
       <$> f (_stSnapshotIdentifier x)
{-# INLINE stSnapshotIdentifier #-}

-- | The identifier of the cluster for which the snapshot was taken.
stClusterIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stClusterIdentifier f x =
    (\y -> x { _stClusterIdentifier = y })
       <$> f (_stClusterIdentifier x)
{-# INLINE stClusterIdentifier #-}

-- | The time (UTC) when Amazon Redshift began the snapshot. A snapshot contains
-- a copy of the cluster data as of this exact time.
stSnapshotCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Snapshot
    -> f Snapshot
stSnapshotCreateTime f x =
    (\y -> x { _stSnapshotCreateTime = y })
       <$> f (_stSnapshotCreateTime x)
{-# INLINE stSnapshotCreateTime #-}

-- | The snapshot status. The value of the status depends on the API operation
-- used. CreateClusterSnapshot and CopyClusterSnapshot returns status as
-- "creating". DescribeClusterSnapshots returns status as "creating",
-- "available", "final snapshot", or "failed". DeleteClusterSnapshot returns
-- status as "deleted".
stStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stStatus f x =
    (\y -> x { _stStatus = y })
       <$> f (_stStatus x)
{-# INLINE stStatus #-}

-- | The port that the cluster is listening on.
stPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Snapshot
    -> f Snapshot
stPort f x =
    (\y -> x { _stPort = y })
       <$> f (_stPort x)
{-# INLINE stPort #-}

-- | The Availability Zone in which the cluster was created.
stAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stAvailabilityZone f x =
    (\y -> x { _stAvailabilityZone = y })
       <$> f (_stAvailabilityZone x)
{-# INLINE stAvailabilityZone #-}

-- | The time (UTC) when the cluster was originally created.
stClusterCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Snapshot
    -> f Snapshot
stClusterCreateTime f x =
    (\y -> x { _stClusterCreateTime = y })
       <$> f (_stClusterCreateTime x)
{-# INLINE stClusterCreateTime #-}

-- | The master user name for the cluster.
stMasterUsername
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stMasterUsername f x =
    (\y -> x { _stMasterUsername = y })
       <$> f (_stMasterUsername x)
{-# INLINE stMasterUsername #-}

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
stClusterVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stClusterVersion f x =
    (\y -> x { _stClusterVersion = y })
       <$> f (_stClusterVersion x)
{-# INLINE stClusterVersion #-}

-- | The snapshot type. Snapshots created using CreateClusterSnapshot and
-- CopyClusterSnapshot will be of type "manual".
stSnapshotType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stSnapshotType f x =
    (\y -> x { _stSnapshotType = y })
       <$> f (_stSnapshotType x)
{-# INLINE stSnapshotType #-}

-- | The node type of the nodes in the cluster.
stNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stNodeType f x =
    (\y -> x { _stNodeType = y })
       <$> f (_stNodeType x)
{-# INLINE stNodeType #-}

-- | The number of nodes in the cluster.
stNumberOfNodes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Snapshot
    -> f Snapshot
stNumberOfNodes f x =
    (\y -> x { _stNumberOfNodes = y })
       <$> f (_stNumberOfNodes x)
{-# INLINE stNumberOfNodes #-}

-- | The name of the database that was created when the cluster was created.
stDBName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stDBName f x =
    (\y -> x { _stDBName = y })
       <$> f (_stDBName x)
{-# INLINE stDBName #-}

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a
-- VPC. Otherwise, this field is not in the output.
stVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stVpcId f x =
    (\y -> x { _stVpcId = y })
       <$> f (_stVpcId x)
{-# INLINE stVpcId #-}

-- | If true, the data in the snapshot is encrypted at rest.
stEncrypted
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Snapshot
    -> f Snapshot
stEncrypted f x =
    (\y -> x { _stEncrypted = y })
       <$> f (_stEncrypted x)
{-# INLINE stEncrypted #-}

-- | A boolean that indicates whether the snapshot data is encrypted using the
-- HSM keys of the source cluster. true indicates that the data is encrypted
-- using HSM keys.
stEncryptedWithHSM
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Snapshot
    -> f Snapshot
stEncryptedWithHSM f x =
    (\y -> x { _stEncryptedWithHSM = y })
       <$> f (_stEncryptedWithHSM x)
{-# INLINE stEncryptedWithHSM #-}

-- | A list of the AWS customer accounts authorized to restore the snapshot.
-- Returns null if no accounts are authorized. Visible only to the snapshot
-- owner.
stAccountsWithRestoreAccess
    :: Functor f
    => ([AccountWithRestoreAccess]
    -> f ([AccountWithRestoreAccess]))
    -> Snapshot
    -> f Snapshot
stAccountsWithRestoreAccess f x =
    (\y -> x { _stAccountsWithRestoreAccess = y })
       <$> f (_stAccountsWithRestoreAccess x)
{-# INLINE stAccountsWithRestoreAccess #-}

-- | For manual snapshots, the AWS customer account used to create or copy the
-- snapshot. For automatic snapshots, the owner of the cluster. The owner can
-- perform all snapshot actions, such as sharing a manual snapshot.
stOwnerAccount
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stOwnerAccount f x =
    (\y -> x { _stOwnerAccount = y })
       <$> f (_stOwnerAccount x)
{-# INLINE stOwnerAccount #-}

-- | The size of the complete set of backup data that would be used to restore
-- the cluster.
stTotalBackupSizeInMegaBytes
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> Snapshot
    -> f Snapshot
stTotalBackupSizeInMegaBytes f x =
    (\y -> x { _stTotalBackupSizeInMegaBytes = y })
       <$> f (_stTotalBackupSizeInMegaBytes x)
{-# INLINE stTotalBackupSizeInMegaBytes #-}

-- | The size of the incremental backup.
stActualIncrementalBackupSizeInMegaBytes
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> Snapshot
    -> f Snapshot
stActualIncrementalBackupSizeInMegaBytes f x =
    (\y -> x { _stActualIncrementalBackupSizeInMegaBytes = y })
       <$> f (_stActualIncrementalBackupSizeInMegaBytes x)
{-# INLINE stActualIncrementalBackupSizeInMegaBytes #-}

-- | The number of megabytes that have been transferred to the snapshot backup.
stBackupProgressInMegaBytes
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> Snapshot
    -> f Snapshot
stBackupProgressInMegaBytes f x =
    (\y -> x { _stBackupProgressInMegaBytes = y })
       <$> f (_stBackupProgressInMegaBytes x)
{-# INLINE stBackupProgressInMegaBytes #-}

-- | The number of megabytes per second being transferred to the snapshot
-- backup. Returns 0 for a completed backup.
stCurrentBackupRateInMegaBytesPerSecond
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> Snapshot
    -> f Snapshot
stCurrentBackupRateInMegaBytesPerSecond f x =
    (\y -> x { _stCurrentBackupRateInMegaBytesPerSecond = y })
       <$> f (_stCurrentBackupRateInMegaBytesPerSecond x)
{-# INLINE stCurrentBackupRateInMegaBytesPerSecond #-}

-- | The estimate of the time remaining before the snapshot backup will
-- complete. Returns 0 for a completed backup.
stEstimatedSecondsToCompletion
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Snapshot
    -> f Snapshot
stEstimatedSecondsToCompletion f x =
    (\y -> x { _stEstimatedSecondsToCompletion = y })
       <$> f (_stEstimatedSecondsToCompletion x)
{-# INLINE stEstimatedSecondsToCompletion #-}

-- | The amount of time an in-progress snapshot backup has been running, or the
-- amount of time it took a completed backup to finish.
stElapsedTimeInSeconds
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Snapshot
    -> f Snapshot
stElapsedTimeInSeconds f x =
    (\y -> x { _stElapsedTimeInSeconds = y })
       <$> f (_stElapsedTimeInSeconds x)
{-# INLINE stElapsedTimeInSeconds #-}

-- | The source region from which the snapshot was copied.
stSourceRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stSourceRegion f x =
    (\y -> x { _stSourceRegion = y })
       <$> f (_stSourceRegion x)
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
sssssssuSubnetIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Subnet
    -> f Subnet
sssssssuSubnetIdentifier f x =
    (\y -> x { _sssssssuSubnetIdentifier = y })
       <$> f (_sssssssuSubnetIdentifier x)
{-# INLINE sssssssuSubnetIdentifier #-}

-- | Describes an availability zone.
sssssssuSubnetAvailabilityZone
    :: Functor f
    => (Maybe AvailabilityZone
    -> f (Maybe AvailabilityZone))
    -> Subnet
    -> f Subnet
sssssssuSubnetAvailabilityZone f x =
    (\y -> x { _sssssssuSubnetAvailabilityZone = y })
       <$> f (_sssssssuSubnetAvailabilityZone x)
{-# INLINE sssssssuSubnetAvailabilityZone #-}

-- | The status of the subnet.
sssssssuSubnetStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Subnet
    -> f Subnet
sssssssuSubnetStatus f x =
    (\y -> x { _sssssssuSubnetStatus = y })
       <$> f (_sssssssuSubnetStatus x)
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
vsgmVpcSecurityGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcSecurityGroupMembership
    -> f VpcSecurityGroupMembership
vsgmVpcSecurityGroupId f x =
    (\y -> x { _vsgmVpcSecurityGroupId = y })
       <$> f (_vsgmVpcSecurityGroupId x)
{-# INLINE vsgmVpcSecurityGroupId #-}

-- | 
vsgmStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcSecurityGroupMembership
    -> f VpcSecurityGroupMembership
vsgmStatus f x =
    (\y -> x { _vsgmStatus = y })
       <$> f (_vsgmStatus x)
{-# INLINE vsgmStatus #-}

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroup"

instance ToQuery VpcSecurityGroupMembership where
    toQuery = genericQuery def
