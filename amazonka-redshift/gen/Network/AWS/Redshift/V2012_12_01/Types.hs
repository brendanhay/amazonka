{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
    ( module Network.AWS.Redshift.V2012_12_01.Types
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

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone where
    toQuery = genericQuery def

-- | Describes a cluster.
data Cluster = Cluster
    { _cRestoreStatus :: Maybe RestoreStatus
      -- ^ Describes the status of a cluster restore action. Returns null if
      -- the cluster was not created by restoring a snapshot.
    , _cClusterSnapshotCopyStatus :: Maybe ClusterSnapshotCopyStatus
      -- ^ Returns the destination region and retention period that are
      -- configured for cross-region snapshot copy.
    , _cClusterRevisionNumber :: Maybe Text
      -- ^ The specific revision number of the database in the cluster.
    , _cPubliclyAccessible :: Maybe Bool
      -- ^ If true, the cluster can be accessed from a public network.
    , _cMasterUsername :: Maybe Text
      -- ^ The master user name for the cluster. This name is used to
      -- connect to the database that is specified in DBName.
    , _cVpcId :: Maybe Text
      -- ^ The identifier of the VPC the cluster is in, if the cluster is in
      -- a VPC.
    , _cClusterSecurityGroups :: [ClusterSecurityGroupMembership]
      -- ^ A list of cluster security group that are associated with the
      -- cluster. Each security group is represented by an element that
      -- contains ClusterSecurityGroup.Name and
      -- ClusterSecurityGroup.Status subelements. Cluster security groups
      -- are used when the cluster is not created in a VPC. Clusters that
      -- are created in a VPC use VPC security groups, which are listed by
      -- the VpcSecurityGroups parameter.
    , _cAutomatedSnapshotRetentionPeriod :: Maybe Integer
      -- ^ The number of days that automatic cluster snapshots are retained.
    , _cEncrypted :: Maybe Bool
      -- ^ If true, data in the cluster is encrypted at rest.
    , _cClusterSubnetGroupName :: Maybe Text
      -- ^ The name of the subnet group that is associated with the cluster.
      -- This parameter is valid only when the cluster is in a VPC.
    , _cClusterIdentifier :: Maybe Text
      -- ^ The unique identifier of the cluster.
    , _cNumberOfNodes :: Maybe Integer
      -- ^ The number of compute nodes in the cluster.
    , _cClusterPublicKey :: Maybe Text
      -- ^ The public key for the cluster.
    , _cPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance
      -- can occur.
    , _cModifyStatus :: Maybe Text
      -- ^ The status of a modify operation, if any, initiated for the
      -- cluster.
    , _cClusterParameterGroups :: [ClusterParameterGroupStatus]
      -- ^ The list of cluster parameter groups that are associated with
      -- this cluster.
    , _cAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the cluster is
      -- located.
    , _cVpcSecurityGroups :: [VpcSecurityGroupMembership]
      -- ^ A list of Virtual Private Cloud (VPC) security groups that are
      -- associated with the cluster. This parameter is returned only if
      -- the cluster is in a VPC.
    , _cHsmStatus :: Maybe HsmStatus
      -- ^ Reports whether the Amazon Redshift cluster has finished applying
      -- any HSM settings changes specified in a modify cluster command.
      -- Values: active, applying.
    , _cElasticIpStatus :: Maybe ElasticIpStatus
      -- ^ Describes the status of the elastic IP (EIP) address.
    , _cClusterVersion :: Maybe Text
      -- ^ The version ID of the Amazon Redshift engine that is running on
      -- the cluster.
    , _cNodeType :: Maybe Text
      -- ^ The node type for the nodes in the cluster.
    , _cClusterCreateTime :: Maybe ISO8601
      -- ^ The date and time that the cluster was created.
    , _cEndpoint :: Maybe Endpoint
      -- ^ The connection endpoint.
    , _cAllowVersionUpgrade :: Maybe Bool
      -- ^ If true, version upgrades will be applied automatically to the
      -- cluster during the maintenance window.
    , _cClusterStatus :: Maybe Text
      -- ^ The current state of this cluster. Possible values include
      -- available, creating, deleting, rebooting, renaming, and resizing.
    , _cPendingModifiedValues :: Maybe PendingModifiedValues
      -- ^ If present, changes to the cluster are pending. Specific pending
      -- changes are identified by subelements.
    , _cClusterNodes :: [ClusterNode]
      -- ^ The nodes in a cluster.
    , _cDBName :: Maybe Text
      -- ^ The name of the initial database that was created when the
      -- cluster was created. This same name is returned for the life of
      -- the cluster. If an initial database was not specified, a database
      -- named "dev" was created by default.
    } deriving (Show, Generic)

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

instance FromXML ClusterNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterNode"

instance ToQuery ClusterNode where
    toQuery = genericQuery def

-- | Describes a parameter group.
data ClusterParameterGroup = ClusterParameterGroup
    { _cpgParameterGroupFamily :: Maybe Text
      -- ^ The name of the cluster parameter group family that this cluster
      -- parameter group is compatible with.
    , _cpgDescription :: Maybe Text
      -- ^ The description of the parameter group.
    , _cpgParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group.
    } deriving (Show, Generic)

instance FromXML ClusterParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterParameterGroup"

-- | Describes the status of a parameter group.
data ClusterParameterGroupStatus = ClusterParameterGroupStatus
    { _cpgsParameterApplyStatus :: Maybe Text
      -- ^ The status of parameter updates.
    , _cpgsParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group.
    } deriving (Show, Generic)

instance FromXML ClusterParameterGroupStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterParameterGroup"

instance ToQuery ClusterParameterGroupStatus where
    toQuery = genericQuery def

-- | Describes a security group.
data ClusterSecurityGroup = ClusterSecurityGroup
    { _csiClusterSecurityGroupName :: Maybe Text
      -- ^ The name of the cluster security group to which the operation was
      -- applied.
    , _csiIPRanges :: [IPRange]
      -- ^ A list of IP ranges (CIDR blocks) that are permitted to access
      -- clusters associated with this cluster security group.
    , _csiEC2SecurityGroups :: [EC2SecurityGroup]
      -- ^ A list of EC2 security groups that are permitted to access
      -- clusters associated with this cluster security group.
    , _csiDescription :: Maybe Text
      -- ^ A description of the security group.
    } deriving (Show, Generic)

instance FromXML ClusterSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSecurityGroup"

-- | Describes a security group.
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership
    { _csgmStatus :: Maybe Text
      -- ^ The status of the cluster security group.
    , _csgmClusterSecurityGroupName :: Maybe Text
      -- ^ The name of the cluster security group.
    } deriving (Show, Generic)

instance FromXML ClusterSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSecurityGroup"

instance ToQuery ClusterSecurityGroupMembership where
    toQuery = genericQuery def

-- | Returns the destination region and retention period that are configured for
-- cross-region snapshot copy.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus
    { _cscsRetentionPeriod :: Maybe Integer
      -- ^ The number of days that automated snapshots are retained in the
      -- destination region after they are copied from a source region.
    , _cscsDestinationRegion :: Maybe Text
      -- ^ The destination region that snapshots are automatically copied to
      -- when cross-region snapshot copy is enabled.
    } deriving (Show, Generic)

instance FromXML ClusterSnapshotCopyStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSnapshotCopyStatus"

instance ToQuery ClusterSnapshotCopyStatus where
    toQuery = genericQuery def

-- | Describes a subnet group.
data ClusterSubnetGroup = ClusterSubnetGroup
    { _csgVpcId :: Maybe Text
      -- ^ The VPC ID of the cluster subnet group.
    , _csgSubnets :: [Subnet]
      -- ^ A list of the VPC Subnet elements.
    , _csgClusterSubnetGroupName :: Maybe Text
      -- ^ The name of the cluster subnet group.
    , _csgSubnetGroupStatus :: Maybe Text
      -- ^ The status of the cluster subnet group. Possible values are
      -- Complete, Incomplete and Invalid.
    , _csgDescription :: Maybe Text
      -- ^ The description of the cluster subnet group.
    } deriving (Show, Generic)

instance FromXML ClusterSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSubnetGroup"

-- | Describes a cluster version, including the parameter group family and
-- description of the version.
data ClusterVersion = ClusterVersion
    { _ccnClusterParameterGroupFamily :: Maybe Text
      -- ^ The name of the cluster parameter group family for the cluster.
    , _ccnClusterVersion :: Maybe Text
      -- ^ The version number used by the cluster.
    , _ccnDescription :: Maybe Text
      -- ^ The description of the cluster version.
    } deriving (Show, Generic)

instance FromXML ClusterVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterVersion"

-- | Describes the default cluster parameters for a parameter group family.
data DefaultClusterParameters = DefaultClusterParameters
    { _dcpMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    , _dcpParameters :: [Parameter]
      -- ^ The list of cluster default parameters.
    , _dcpParameterGroupFamily :: Maybe Text
      -- ^ The name of the cluster parameter group family to which the
      -- engine default parameters apply.
    } deriving (Show, Generic)

instance FromXML DefaultClusterParameters where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DefaultClusterParameters"

-- | Describes an Amazon EC2 security group.
data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus :: Maybe Text
      -- ^ The status of the EC2 security group.
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS ID of the owner of the EC2 security group specified in
      -- the EC2SecurityGroupName field.
    , _ecsgEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the EC2 Security Group.
    } deriving (Show, Generic)

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup where
    toQuery = genericQuery def

-- | Describes the status of the elastic IP (EIP) address.
data ElasticIpStatus = ElasticIpStatus
    { _eisStatus :: Maybe Text
      -- ^ Describes the status of the elastic IP (EIP) address.
    , _eisElasticIp :: Maybe Text
      -- ^ The elastic IP (EIP) address for the cluster.
    } deriving (Show, Generic)

instance FromXML ElasticIpStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ElasticIpStatus"

instance ToQuery ElasticIpStatus where
    toQuery = genericQuery def

-- | The connection endpoint.
data Endpoint = Endpoint
    { _fAddress :: Maybe Text
      -- ^ The DNS address of the Cluster.
    , _fPort :: Maybe Integer
      -- ^ The port that the database engine is listening on.
    } deriving (Show, Generic)

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint where
    toQuery = genericQuery def

-- | Describes an event.
data Event = Event
    { _etSourceType :: Maybe SourceType
      -- ^ The source type for this event.
    , _etSeverity :: Maybe Text
      -- ^ The severity of the event. Values: ERROR, INFO.
    , _etSourceIdentifier :: Maybe Text
      -- ^ The identifier for the source of the event.
    , _etDate :: Maybe ISO8601
      -- ^ The date and time of the event.
    , _etEventCategories :: [Text]
      -- ^ A list of the event categories.
    , _etMessage :: Maybe Text
      -- ^ The text of this event.
    , _etEventId :: Maybe Text
      -- ^ The identifier of the event.
    } deriving (Show, Generic)

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

-- | 
data EventCategoriesMap = EventCategoriesMap
    { _ecsSourceType :: Maybe Text
      -- ^ The Amazon Redshift source type, such as cluster or
      -- cluster-snapshot, that the returned categories belong to.
    , _ecsEvents :: [EventInfoMap]
      -- ^ The events in the event category.
    } deriving (Show, Generic)

instance FromXML EventCategoriesMap where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventCategoriesMap"

-- | 
data EventInfoMap = EventInfoMap
    { _eimEventDescription :: Maybe Text
      -- ^ The description of an Amazon Redshift event.
    , _eimSeverity :: Maybe Text
      -- ^ The severity of the event. Values: ERROR, INFO.
    , _eimEventCategories :: [Text]
      -- ^ The category of an Amazon Redshift event.
    , _eimEventId :: Maybe Text
      -- ^ The identifier of an Amazon Redshift event.
    } deriving (Show, Generic)

instance FromXML EventInfoMap where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventInfoMap"

instance ToQuery EventInfoMap where
    toQuery = genericQuery def

-- | 
data EventSubscription = EventSubscription
    { _esStatus :: Maybe Text
      -- ^ The status of the Amazon Redshift event notification
      -- subscription. Constraints: Can be one of the following: active |
      -- no-permission | topic-not-exist The status "no-permission"
      -- indicates that Amazon Redshift no longer has permission to post
      -- to the Amazon SNS topic. The status "topic-not-exist" indicates
      -- that the topic was deleted after the subscription was created.
    , _esCustomerAwsId :: Maybe Text
      -- ^ The AWS customer account associated with the Amazon Redshift
      -- event notification subscription.
    , _esCustSubscriptionId :: Maybe Text
      -- ^ The name of the Amazon Redshift event notification subscription.
    , _esSnsTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic used by
      -- the event notification subscription.
    , _esEnabled :: Maybe Bool
      -- ^ A Boolean value indicating whether the subscription is enabled.
      -- true indicates the subscription is enabled.
    , _esSourceType :: Maybe Text
      -- ^ The source type of the events returned the Amazon Redshift event
      -- notification, such as cluster, or cluster-snapshot.
    , _esSeverity :: Maybe Text
      -- ^ The event severity specified in the Amazon Redshift event
      -- notification subscription. Values: ERROR, INFO.
    , _esSubscriptionCreationTime :: Maybe ISO8601
      -- ^ The date and time the Amazon Redshift event notification
      -- subscription was created.
    , _esEventCategoriesList :: [Text]
      -- ^ The list of Amazon Redshift event categories specified in the
      -- event notification subscription. Values: Configuration,
      -- Management, Monitoring, Security.
    , _esSourceIdsList :: [Text]
      -- ^ A list of the sources that publish events to the Amazon Redshift
      -- event notification subscription.
    } deriving (Show, Generic)

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

instance FromXML HsmClientCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HsmClientCertificate"

-- | Returns information about an HSM configuration, which is an object that
-- describes to Amazon Redshift clusters the information they require to
-- connect to an HSM where they can store database encryption keys.
data HsmConfiguration = HsmConfiguration
    { _hcHsmConfigurationIdentifier :: Maybe Text
      -- ^ The name of the Amazon Redshift HSM configuration.
    , _hcHsmPartitionName :: Maybe Text
      -- ^ The name of the partition in the HSM where the Amazon Redshift
      -- clusters will store their database encryption keys.
    , _hcDescription :: Maybe Text
      -- ^ A text description of the HSM configuration.
    , _hcHsmIpAddress :: Maybe Text
      -- ^ The IP address that the Amazon Redshift cluster must use to
      -- access the HSM.
    } deriving (Show, Generic)

instance FromXML HsmConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HsmConfiguration"

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
data HsmStatus = HsmStatus
    { _hsStatus :: Maybe Text
      -- ^ Reports whether the Amazon Redshift cluster has finished applying
      -- any HSM settings changes specified in a modify cluster command.
      -- Values: active, applying.
    , _hsHsmConfigurationIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM configuration that contains the
      -- information the Amazon Redshift cluster can use to retrieve and
      -- store keys in an HSM.
    , _hsHsmClientCertificateIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM client certificate the Amazon
      -- Redshift cluster uses to retrieve the data encryption keys stored
      -- in an HSM.
    } deriving (Show, Generic)

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

instance FromXML IPRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IPRange"

instance ToQuery IPRange where
    toQuery = genericQuery def

-- | Describes an orderable cluster option.
data OrderableClusterOption = OrderableClusterOption
    { _ocoAvailabilityZones :: [AvailabilityZone]
      -- ^ A list of availability zones for the orderable cluster.
    , _ocoClusterType :: Maybe Text
      -- ^ The cluster type, for example multi-node.
    , _ocoClusterVersion :: Maybe Text
      -- ^ The version of the orderable cluster.
    , _ocoNodeType :: Maybe Text
      -- ^ The node type for the orderable cluster.
    } deriving (Show, Generic)

instance FromXML OrderableClusterOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OrderableClusterOption"

-- | Describes a parameter in a cluster parameter group.
data Parameter = Parameter
    { _prParameterValue :: Maybe Text
      -- ^ The value of the parameter.
    , _prMinimumEngineVersion :: Maybe Text
      -- ^ The earliest engine version to which the parameter can apply.
    , _prSource :: Maybe Text
      -- ^ The source of the parameter value, such as "engine-default" or
      -- "user".
    , _prIsModifiable :: Maybe Bool
      -- ^ If true, the parameter can be modified. Some parameters have
      -- security or operational implications that prevent them from being
      -- changed.
    , _prDataType :: Maybe Text
      -- ^ The data type of the parameter.
    , _prAllowedValues :: Maybe Text
      -- ^ The valid range of values for the parameter.
    , _prParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , _prDescription :: Maybe Text
      -- ^ A description of the parameter.
    } deriving (Show, Generic)

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
    , _pmvAutomatedSnapshotRetentionPeriod :: Maybe Integer
      -- ^ The pending or in-progress change of the automated snapshot
      -- retention period.
    , _pmvClusterIdentifier :: Maybe Text
      -- ^ The pending or in-progress change of the new identifier for the
      -- cluster.
    , _pmvNumberOfNodes :: Maybe Integer
      -- ^ The pending or in-progress change of the number of nodes in the
      -- cluster.
    , _pmvClusterType :: Maybe Text
      -- ^ The pending or in-progress change of the cluster type.
    , _pmvClusterVersion :: Maybe Text
      -- ^ The pending or in-progress change of the service version.
    , _pmvNodeType :: Maybe Text
      -- ^ The pending or in-progress change of the cluster's node type.
    } deriving (Show, Generic)

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingModifiedValues"

instance ToQuery PendingModifiedValues where
    toQuery = genericQuery def

-- | Describes a recurring charge.
data RecurringCharge = RecurringCharge
    { _rcRecurringChargeFrequency :: Maybe Text
      -- ^ The frequency at which the recurring charge amount is applied.
    , _rcRecurringChargeAmount :: Maybe Double
      -- ^ The amount charged per the period of time specified by the
      -- recurring charge frequency.
    } deriving (Show, Generic)

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecurringCharge"

instance ToQuery RecurringCharge where
    toQuery = genericQuery def

-- | Describes a reserved node.
data ReservedNode = ReservedNode
    { _rnState :: Maybe Text
      -- ^ The state of the reserved Compute Node. Possible Values:
      -- pending-payment-This reserved node has recently been purchased,
      -- and the sale has been approved, but payment has not yet been
      -- confirmed. active-This reserved node is owned by the caller and
      -- is available for use. payment-failed-Payment failed for the
      -- purchase attempt.
    , _rnCurrencyCode :: Maybe Text
      -- ^ The currency code for the reserved cluster.
    , _rnStartTime :: Maybe ISO8601
      -- ^ The time the reservation started. You purchase a reserved node
      -- offering for a duration. This is the start time of that duration.
    , _rnNodeCount :: Maybe Integer
      -- ^ The number of reserved compute nodes.
    , _rnReservedNodeId :: Maybe Text
      -- ^ The unique identifier for the reservation.
    , _rnReservedNodeOfferingId :: Maybe Text
      -- ^ The identifier for the reserved node offering.
    , _rnRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charges for the reserved node.
    , _rnOfferingType :: Maybe Text
      -- ^ The anticipated utilization of the reserved node, as defined in
      -- the reserved node offering.
    , _rnUsagePrice :: Maybe Double
      -- ^ The hourly rate Amazon Redshift charge you for this reserved
      -- node.
    , _rnNodeType :: Maybe Text
      -- ^ The node type of the reserved node.
    , _rnFixedPrice :: Maybe Double
      -- ^ The fixed cost Amazon Redshift charged you for this reserved
      -- node.
    , _rnDuration :: Maybe Integer
      -- ^ The duration of the node reservation in seconds.
    } deriving (Show, Generic)

instance FromXML ReservedNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedNode"

-- | Describes a reserved node offering.
data ReservedNodeOffering = ReservedNodeOffering
    { _rnoCurrencyCode :: Maybe Text
      -- ^ The currency code for the compute nodes offering.
    , _rnoReservedNodeOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , _rnoRecurringCharges :: [RecurringCharge]
      -- ^ The charge to your account regardless of whether you are creating
      -- any clusters using the node offering. Recurring charges are only
      -- in effect for heavy-utilization reserved nodes.
    , _rnoOfferingType :: Maybe Text
      -- ^ The anticipated utilization of the reserved node, as defined in
      -- the reserved node offering.
    , _rnoUsagePrice :: Maybe Double
      -- ^ The rate you are charged for each hour the cluster that is using
      -- the offering is running.
    , _rnoNodeType :: Maybe Text
      -- ^ The node type offered by the reserved node offering.
    , _rnoFixedPrice :: Maybe Double
      -- ^ The upfront fixed charge you will pay to purchase the specific
      -- reserved node offering.
    , _rnoDuration :: Maybe Integer
      -- ^ The duration, in seconds, for which the offering will reserve the
      -- node.
    } deriving (Show, Generic)

instance FromXML ReservedNodeOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedNodeOffering"

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
data RestoreStatus = RestoreStatus
    { _rsStatus :: Maybe Text
      -- ^ The status of the restore action. Returns starting, restoring,
      -- completed, or failed.
    , _rsEstimatedTimeToCompletionInSeconds :: Maybe Integer
      -- ^ The estimate of the time remaining before the restore will
      -- complete. Returns 0 for a completed restore.
    , _rsCurrentRestoreRateInMegaBytesPerSecond :: Maybe Double
      -- ^ The number of megabytes per second being transferred from the
      -- backup storage. Returns the average rate for a completed backup.
    , _rsProgressInMegaBytes :: Maybe Integer
      -- ^ The number of megabytes that have been transferred from snapshot
      -- storage.
    , _rsElapsedTimeInSeconds :: Maybe Integer
      -- ^ The amount of time an in-progress restore has been running, or
      -- the amount of time it took a completed restore to finish.
    , _rsSnapshotSizeInMegaBytes :: Maybe Integer
      -- ^ The size of the set of snapshot data used to restore the cluster.
    } deriving (Show, Generic)

instance FromXML RestoreStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RestoreStatus"

instance ToQuery RestoreStatus where
    toQuery = genericQuery def

-- | Describes a snapshot.
data Snapshot = Snapshot
    { _ssstStatus :: Maybe Text
      -- ^ The snapshot status. The value of the status depends on the API
      -- operation used. CreateClusterSnapshot and CopyClusterSnapshot
      -- returns status as "creating". DescribeClusterSnapshots returns
      -- status as "creating", "available", "final snapshot", or "failed".
      -- DeleteClusterSnapshot returns status as "deleted".
    , _ssstAccountsWithRestoreAccess :: [AccountWithRestoreAccess]
      -- ^ A list of the AWS customer accounts authorized to restore the
      -- snapshot. Returns null if no accounts are authorized. Visible
      -- only to the snapshot owner.
    , _ssstSnapshotIdentifier :: Maybe Text
      -- ^ The snapshot identifier that is provided in the request.
    , _ssstEncryptedWithHSM :: Maybe Bool
      -- ^ A boolean that indicates whether the snapshot data is encrypted
      -- using the HSM keys of the source cluster. true indicates that the
      -- data is encrypted using HSM keys.
    , _ssstMasterUsername :: Maybe Text
      -- ^ The master user name for the cluster.
    , _ssstSourceRegion :: Maybe Text
      -- ^ The source region from which the snapshot was copied.
    , _ssstVpcId :: Maybe Text
      -- ^ The VPC identifier of the cluster if the snapshot is from a
      -- cluster in a VPC. Otherwise, this field is not in the output.
    , _ssstBackupProgressInMegaBytes :: Maybe Double
      -- ^ The number of megabytes that have been transferred to the
      -- snapshot backup.
    , _ssstEncrypted :: Maybe Bool
      -- ^ If true, the data in the snapshot is encrypted at rest.
    , _ssstClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster for which the snapshot was taken.
    , _ssstNumberOfNodes :: Maybe Integer
      -- ^ The number of nodes in the cluster.
    , _ssstSnapshotType :: Maybe Text
      -- ^ The snapshot type. Snapshots created using CreateClusterSnapshot
      -- and CopyClusterSnapshot will be of type "manual".
    , _ssstAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the cluster was created.
    , _ssstCurrentBackupRateInMegaBytesPerSecond :: Maybe Double
      -- ^ The number of megabytes per second being transferred to the
      -- snapshot backup. Returns 0 for a completed backup.
    , _ssstSnapshotCreateTime :: Maybe ISO8601
      -- ^ The time (UTC) when Amazon Redshift began the snapshot. A
      -- snapshot contains a copy of the cluster data as of this exact
      -- time.
    , _ssstClusterVersion :: Maybe Text
      -- ^ The version ID of the Amazon Redshift engine that is running on
      -- the cluster.
    , _ssstOwnerAccount :: Maybe Text
      -- ^ For manual snapshots, the AWS customer account used to create or
      -- copy the snapshot. For automatic snapshots, the owner of the
      -- cluster. The owner can perform all snapshot actions, such as
      -- sharing a manual snapshot.
    , _ssstNodeType :: Maybe Text
      -- ^ The node type of the nodes in the cluster.
    , _ssstElapsedTimeInSeconds :: Maybe Integer
      -- ^ The amount of time an in-progress snapshot backup has been
      -- running, or the amount of time it took a completed backup to
      -- finish.
    , _ssstClusterCreateTime :: Maybe ISO8601
      -- ^ The time (UTC) when the cluster was originally created.
    , _ssstEstimatedSecondsToCompletion :: Maybe Integer
      -- ^ The estimate of the time remaining before the snapshot backup
      -- will complete. Returns 0 for a completed backup.
    , _ssstActualIncrementalBackupSizeInMegaBytes :: Maybe Double
      -- ^ The size of the incremental backup.
    , _ssstPort :: Maybe Integer
      -- ^ The port that the cluster is listening on.
    , _ssstTotalBackupSizeInMegaBytes :: Maybe Double
      -- ^ The size of the complete set of backup data that would be used to
      -- restore the cluster.
    , _ssstDBName :: Maybe Text
      -- ^ The name of the database that was created when the cluster was
      -- created.
    } deriving (Show, Generic)

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Snapshot"

-- | Describes a subnet.
data Subnet = Subnet
    { _ssssssxSubnetStatus :: Maybe Text
      -- ^ The status of the subnet.
    , _ssssssxSubnetIdentifier :: Maybe Text
      -- ^ The identifier of the subnet.
    , _ssssssxSubnetAvailabilityZone :: Maybe AvailabilityZone
      -- ^ Describes an availability zone.
    } deriving (Show, Generic)

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericQuery def

-- | Describes the members of a VPC security group.
data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmStatus :: Maybe Text
      -- ^ 
    , _vsgmVpcSecurityGroupId :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroup"

instance ToQuery VpcSecurityGroupMembership where
    toQuery = genericQuery def

makeLenses ''AccountWithRestoreAccess
makeLenses ''AvailabilityZone
makeLenses ''Cluster
makeLenses ''ClusterNode
makeLenses ''ClusterParameterGroup
makeLenses ''ClusterParameterGroupStatus
makeLenses ''ClusterSecurityGroup
makeLenses ''ClusterSecurityGroupMembership
makeLenses ''ClusterSnapshotCopyStatus
makeLenses ''ClusterSubnetGroup
makeLenses ''ClusterVersion
makeLenses ''DefaultClusterParameters
makeLenses ''EC2SecurityGroup
makeLenses ''ElasticIpStatus
makeLenses ''Endpoint
makeLenses ''Event
makeLenses ''EventCategoriesMap
makeLenses ''EventInfoMap
makeLenses ''EventSubscription
makeLenses ''HsmClientCertificate
makeLenses ''HsmConfiguration
makeLenses ''HsmStatus
makeLenses ''IPRange
makeLenses ''OrderableClusterOption
makeLenses ''Parameter
makeLenses ''PendingModifiedValues
makeLenses ''RecurringCharge
makeLenses ''ReservedNode
makeLenses ''ReservedNodeOffering
makeLenses ''RestoreStatus
makeLenses ''Snapshot
makeLenses ''Subnet
makeLenses ''VpcSecurityGroupMembership
