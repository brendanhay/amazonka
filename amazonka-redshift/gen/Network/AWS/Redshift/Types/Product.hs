{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Sum

-- | Describes an AWS customer account authorized to restore a snapshot.
--
--
--
-- /See:/ 'accountWithRestoreAccess' smart constructor.
data AccountWithRestoreAccess = AccountWithRestoreAccess'
  { _awraAccountAlias :: !(Maybe Text)
  , _awraAccountId    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountWithRestoreAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'awraAccountAlias' - The identifier of an AWS support account authorized to restore a snapshot. For AWS support, the identifier is @amazon-redshift-support@ .
--
-- * 'awraAccountId' - The identifier of an AWS customer account authorized to restore a snapshot.
accountWithRestoreAccess
    :: AccountWithRestoreAccess
accountWithRestoreAccess =
  AccountWithRestoreAccess'
    {_awraAccountAlias = Nothing, _awraAccountId = Nothing}


-- | The identifier of an AWS support account authorized to restore a snapshot. For AWS support, the identifier is @amazon-redshift-support@ .
awraAccountAlias :: Lens' AccountWithRestoreAccess (Maybe Text)
awraAccountAlias = lens _awraAccountAlias (\ s a -> s{_awraAccountAlias = a})

-- | The identifier of an AWS customer account authorized to restore a snapshot.
awraAccountId :: Lens' AccountWithRestoreAccess (Maybe Text)
awraAccountId = lens _awraAccountId (\ s a -> s{_awraAccountId = a})

instance FromXML AccountWithRestoreAccess where
        parseXML x
          = AccountWithRestoreAccess' <$>
              (x .@? "AccountAlias") <*> (x .@? "AccountId")

instance Hashable AccountWithRestoreAccess where

instance NFData AccountWithRestoreAccess where

-- | Describes an availability zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { _azName               :: !(Maybe Text)
  , _azSupportedPlatforms :: !(Maybe [SupportedPlatform])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName' - The name of the availability zone.
--
-- * 'azSupportedPlatforms' - Undocumented member.
availabilityZone
    :: AvailabilityZone
availabilityZone =
  AvailabilityZone' {_azName = Nothing, _azSupportedPlatforms = Nothing}


-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\ s a -> s{_azName = a})

-- | Undocumented member.
azSupportedPlatforms :: Lens' AvailabilityZone [SupportedPlatform]
azSupportedPlatforms = lens _azSupportedPlatforms (\ s a -> s{_azSupportedPlatforms = a}) . _Default . _Coerce

instance FromXML AvailabilityZone where
        parseXML x
          = AvailabilityZone' <$>
              (x .@? "Name") <*>
                (x .@? "SupportedPlatforms" .!@ mempty >>=
                   may (parseXMLList "SupportedPlatform"))

instance Hashable AvailabilityZone where

instance NFData AvailabilityZone where

-- | Describes a cluster.
--
--
--
-- /See:/ 'cluster' smart constructor.
data Cluster = Cluster'
  { _cRestoreStatus :: !(Maybe RestoreStatus)
  , _cEnhancedVPCRouting :: !(Maybe Bool)
  , _cClusterSnapshotCopyStatus :: !(Maybe ClusterSnapshotCopyStatus)
  , _cClusterRevisionNumber :: !(Maybe Text)
  , _cPubliclyAccessible :: !(Maybe Bool)
  , _cMasterUsername :: !(Maybe Text)
  , _cVPCId :: !(Maybe Text)
  , _cClusterSecurityGroups :: !(Maybe [ClusterSecurityGroupMembership])
  , _cAutomatedSnapshotRetentionPeriod :: !(Maybe Int)
  , _cEncrypted :: !(Maybe Bool)
  , _cClusterSubnetGroupName :: !(Maybe Text)
  , _cClusterIdentifier :: !(Maybe Text)
  , _cNumberOfNodes :: !(Maybe Int)
  , _cClusterPublicKey :: !(Maybe Text)
  , _cPreferredMaintenanceWindow :: !(Maybe Text)
  , _cModifyStatus :: !(Maybe Text)
  , _cKMSKeyId :: !(Maybe Text)
  , _cClusterParameterGroups :: !(Maybe [ClusterParameterGroupStatus])
  , _cAvailabilityZone :: !(Maybe Text)
  , _cVPCSecurityGroups :: !(Maybe [VPCSecurityGroupMembership])
  , _cHSMStatus :: !(Maybe HSMStatus)
  , _cIAMRoles :: !(Maybe [ClusterIAMRole])
  , _cElasticIPStatus :: !(Maybe ElasticIPStatus)
  , _cClusterVersion :: !(Maybe Text)
  , _cNodeType :: !(Maybe Text)
  , _cClusterCreateTime :: !(Maybe ISO8601)
  , _cEndpoint :: !(Maybe Endpoint)
  , _cAllowVersionUpgrade :: !(Maybe Bool)
  , _cClusterStatus :: !(Maybe Text)
  , _cPendingModifiedValues :: !(Maybe PendingModifiedValues)
  , _cTags :: !(Maybe [Tag])
  , _cClusterNodes :: !(Maybe [ClusterNode])
  , _cDBName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cRestoreStatus' - A value that describes the status of a cluster restore action. This parameter returns null if the cluster was not created by restoring a snapshot.
--
-- * 'cEnhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
--
-- * 'cClusterSnapshotCopyStatus' - A value that returns the destination region and retention period that are configured for cross-region snapshot copy.
--
-- * 'cClusterRevisionNumber' - The specific revision number of the database in the cluster.
--
-- * 'cPubliclyAccessible' - A Boolean value that, if @true@ , indicates that the cluster can be accessed from a public network.
--
-- * 'cMasterUsername' - The master user name for the cluster. This name is used to connect to the database that is specified in the __DBName__ parameter.
--
-- * 'cVPCId' - The identifier of the VPC the cluster is in, if the cluster is in a VPC.
--
-- * 'cClusterSecurityGroups' - A list of cluster security group that are associated with the cluster. Each security group is represented by an element that contains @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@ subelements.  Cluster security groups are used when the cluster is not created in an Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC use VPC security groups, which are listed by the __VpcSecurityGroups__ parameter.
--
-- * 'cAutomatedSnapshotRetentionPeriod' - The number of days that automatic cluster snapshots are retained.
--
-- * 'cEncrypted' - A Boolean value that, if @true@ , indicates that data in the cluster is encrypted at rest.
--
-- * 'cClusterSubnetGroupName' - The name of the subnet group that is associated with the cluster. This parameter is valid only when the cluster is in a VPC.
--
-- * 'cClusterIdentifier' - The unique identifier of the cluster.
--
-- * 'cNumberOfNodes' - The number of compute nodes in the cluster.
--
-- * 'cClusterPublicKey' - The public key for the cluster.
--
-- * 'cPreferredMaintenanceWindow' - The weekly time range, in Universal Coordinated Time (UTC), during which system maintenance can occur.
--
-- * 'cModifyStatus' - The status of a modify operation, if any, initiated for the cluster.
--
-- * 'cKMSKeyId' - The AWS Key Management Service (AWS KMS) key ID of the encryption key used to encrypt data in the cluster.
--
-- * 'cClusterParameterGroups' - The list of cluster parameter groups that are associated with this cluster. Each parameter group in the list is returned with its status.
--
-- * 'cAvailabilityZone' - The name of the Availability Zone in which the cluster is located.
--
-- * 'cVPCSecurityGroups' - A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that are associated with the cluster. This parameter is returned only if the cluster is in a VPC.
--
-- * 'cHSMStatus' - A value that reports whether the Amazon Redshift cluster has finished applying any hardware security module (HSM) settings changes specified in a modify cluster command. Values: active, applying
--
-- * 'cIAMRoles' - A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
--
-- * 'cElasticIPStatus' - The status of the elastic IP (EIP) address.
--
-- * 'cClusterVersion' - The version ID of the Amazon Redshift engine that is running on the cluster.
--
-- * 'cNodeType' - The node type for the nodes in the cluster.
--
-- * 'cClusterCreateTime' - The date and time that the cluster was created.
--
-- * 'cEndpoint' - The connection endpoint.
--
-- * 'cAllowVersionUpgrade' - A Boolean value that, if @true@ , indicates that major version upgrades will be applied automatically to the cluster during the maintenance window.
--
-- * 'cClusterStatus' - The current state of the cluster. Possible values are the following:     * @available@      * @creating@      * @deleting@      * @final-snapshot@      * @hardware-failure@      * @incompatible-hsm@      * @incompatible-network@      * @incompatible-parameters@      * @incompatible-restore@      * @modifying@      * @rebooting@      * @renaming@      * @resizing@      * @rotating-keys@      * @storage-full@      * @updating-hsm@
--
-- * 'cPendingModifiedValues' - A value that, if present, indicates that changes to the cluster are pending. Specific pending changes are identified by subelements.
--
-- * 'cTags' - The list of tags for the cluster.
--
-- * 'cClusterNodes' - The nodes in the cluster.
--
-- * 'cDBName' - The name of the initial database that was created when the cluster was created. This same name is returned for the life of the cluster. If an initial database was not specified, a database named @dev@ dev was created by default.
cluster
    :: Cluster
cluster =
  Cluster'
    { _cRestoreStatus = Nothing
    , _cEnhancedVPCRouting = Nothing
    , _cClusterSnapshotCopyStatus = Nothing
    , _cClusterRevisionNumber = Nothing
    , _cPubliclyAccessible = Nothing
    , _cMasterUsername = Nothing
    , _cVPCId = Nothing
    , _cClusterSecurityGroups = Nothing
    , _cAutomatedSnapshotRetentionPeriod = Nothing
    , _cEncrypted = Nothing
    , _cClusterSubnetGroupName = Nothing
    , _cClusterIdentifier = Nothing
    , _cNumberOfNodes = Nothing
    , _cClusterPublicKey = Nothing
    , _cPreferredMaintenanceWindow = Nothing
    , _cModifyStatus = Nothing
    , _cKMSKeyId = Nothing
    , _cClusterParameterGroups = Nothing
    , _cAvailabilityZone = Nothing
    , _cVPCSecurityGroups = Nothing
    , _cHSMStatus = Nothing
    , _cIAMRoles = Nothing
    , _cElasticIPStatus = Nothing
    , _cClusterVersion = Nothing
    , _cNodeType = Nothing
    , _cClusterCreateTime = Nothing
    , _cEndpoint = Nothing
    , _cAllowVersionUpgrade = Nothing
    , _cClusterStatus = Nothing
    , _cPendingModifiedValues = Nothing
    , _cTags = Nothing
    , _cClusterNodes = Nothing
    , _cDBName = Nothing
    }


-- | A value that describes the status of a cluster restore action. This parameter returns null if the cluster was not created by restoring a snapshot.
cRestoreStatus :: Lens' Cluster (Maybe RestoreStatus)
cRestoreStatus = lens _cRestoreStatus (\ s a -> s{_cRestoreStatus = a})

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
cEnhancedVPCRouting :: Lens' Cluster (Maybe Bool)
cEnhancedVPCRouting = lens _cEnhancedVPCRouting (\ s a -> s{_cEnhancedVPCRouting = a})

-- | A value that returns the destination region and retention period that are configured for cross-region snapshot copy.
cClusterSnapshotCopyStatus :: Lens' Cluster (Maybe ClusterSnapshotCopyStatus)
cClusterSnapshotCopyStatus = lens _cClusterSnapshotCopyStatus (\ s a -> s{_cClusterSnapshotCopyStatus = a})

-- | The specific revision number of the database in the cluster.
cClusterRevisionNumber :: Lens' Cluster (Maybe Text)
cClusterRevisionNumber = lens _cClusterRevisionNumber (\ s a -> s{_cClusterRevisionNumber = a})

-- | A Boolean value that, if @true@ , indicates that the cluster can be accessed from a public network.
cPubliclyAccessible :: Lens' Cluster (Maybe Bool)
cPubliclyAccessible = lens _cPubliclyAccessible (\ s a -> s{_cPubliclyAccessible = a})

-- | The master user name for the cluster. This name is used to connect to the database that is specified in the __DBName__ parameter.
cMasterUsername :: Lens' Cluster (Maybe Text)
cMasterUsername = lens _cMasterUsername (\ s a -> s{_cMasterUsername = a})

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
cVPCId :: Lens' Cluster (Maybe Text)
cVPCId = lens _cVPCId (\ s a -> s{_cVPCId = a})

-- | A list of cluster security group that are associated with the cluster. Each security group is represented by an element that contains @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@ subelements.  Cluster security groups are used when the cluster is not created in an Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC use VPC security groups, which are listed by the __VpcSecurityGroups__ parameter.
cClusterSecurityGroups :: Lens' Cluster [ClusterSecurityGroupMembership]
cClusterSecurityGroups = lens _cClusterSecurityGroups (\ s a -> s{_cClusterSecurityGroups = a}) . _Default . _Coerce

-- | The number of days that automatic cluster snapshots are retained.
cAutomatedSnapshotRetentionPeriod :: Lens' Cluster (Maybe Int)
cAutomatedSnapshotRetentionPeriod = lens _cAutomatedSnapshotRetentionPeriod (\ s a -> s{_cAutomatedSnapshotRetentionPeriod = a})

-- | A Boolean value that, if @true@ , indicates that data in the cluster is encrypted at rest.
cEncrypted :: Lens' Cluster (Maybe Bool)
cEncrypted = lens _cEncrypted (\ s a -> s{_cEncrypted = a})

-- | The name of the subnet group that is associated with the cluster. This parameter is valid only when the cluster is in a VPC.
cClusterSubnetGroupName :: Lens' Cluster (Maybe Text)
cClusterSubnetGroupName = lens _cClusterSubnetGroupName (\ s a -> s{_cClusterSubnetGroupName = a})

-- | The unique identifier of the cluster.
cClusterIdentifier :: Lens' Cluster (Maybe Text)
cClusterIdentifier = lens _cClusterIdentifier (\ s a -> s{_cClusterIdentifier = a})

-- | The number of compute nodes in the cluster.
cNumberOfNodes :: Lens' Cluster (Maybe Int)
cNumberOfNodes = lens _cNumberOfNodes (\ s a -> s{_cNumberOfNodes = a})

-- | The public key for the cluster.
cClusterPublicKey :: Lens' Cluster (Maybe Text)
cClusterPublicKey = lens _cClusterPublicKey (\ s a -> s{_cClusterPublicKey = a})

-- | The weekly time range, in Universal Coordinated Time (UTC), during which system maintenance can occur.
cPreferredMaintenanceWindow :: Lens' Cluster (Maybe Text)
cPreferredMaintenanceWindow = lens _cPreferredMaintenanceWindow (\ s a -> s{_cPreferredMaintenanceWindow = a})

-- | The status of a modify operation, if any, initiated for the cluster.
cModifyStatus :: Lens' Cluster (Maybe Text)
cModifyStatus = lens _cModifyStatus (\ s a -> s{_cModifyStatus = a})

-- | The AWS Key Management Service (AWS KMS) key ID of the encryption key used to encrypt data in the cluster.
cKMSKeyId :: Lens' Cluster (Maybe Text)
cKMSKeyId = lens _cKMSKeyId (\ s a -> s{_cKMSKeyId = a})

-- | The list of cluster parameter groups that are associated with this cluster. Each parameter group in the list is returned with its status.
cClusterParameterGroups :: Lens' Cluster [ClusterParameterGroupStatus]
cClusterParameterGroups = lens _cClusterParameterGroups (\ s a -> s{_cClusterParameterGroups = a}) . _Default . _Coerce

-- | The name of the Availability Zone in which the cluster is located.
cAvailabilityZone :: Lens' Cluster (Maybe Text)
cAvailabilityZone = lens _cAvailabilityZone (\ s a -> s{_cAvailabilityZone = a})

-- | A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that are associated with the cluster. This parameter is returned only if the cluster is in a VPC.
cVPCSecurityGroups :: Lens' Cluster [VPCSecurityGroupMembership]
cVPCSecurityGroups = lens _cVPCSecurityGroups (\ s a -> s{_cVPCSecurityGroups = a}) . _Default . _Coerce

-- | A value that reports whether the Amazon Redshift cluster has finished applying any hardware security module (HSM) settings changes specified in a modify cluster command. Values: active, applying
cHSMStatus :: Lens' Cluster (Maybe HSMStatus)
cHSMStatus = lens _cHSMStatus (\ s a -> s{_cHSMStatus = a})

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
cIAMRoles :: Lens' Cluster [ClusterIAMRole]
cIAMRoles = lens _cIAMRoles (\ s a -> s{_cIAMRoles = a}) . _Default . _Coerce

-- | The status of the elastic IP (EIP) address.
cElasticIPStatus :: Lens' Cluster (Maybe ElasticIPStatus)
cElasticIPStatus = lens _cElasticIPStatus (\ s a -> s{_cElasticIPStatus = a})

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
cClusterVersion :: Lens' Cluster (Maybe Text)
cClusterVersion = lens _cClusterVersion (\ s a -> s{_cClusterVersion = a})

-- | The node type for the nodes in the cluster.
cNodeType :: Lens' Cluster (Maybe Text)
cNodeType = lens _cNodeType (\ s a -> s{_cNodeType = a})

-- | The date and time that the cluster was created.
cClusterCreateTime :: Lens' Cluster (Maybe UTCTime)
cClusterCreateTime = lens _cClusterCreateTime (\ s a -> s{_cClusterCreateTime = a}) . mapping _Time

-- | The connection endpoint.
cEndpoint :: Lens' Cluster (Maybe Endpoint)
cEndpoint = lens _cEndpoint (\ s a -> s{_cEndpoint = a})

-- | A Boolean value that, if @true@ , indicates that major version upgrades will be applied automatically to the cluster during the maintenance window.
cAllowVersionUpgrade :: Lens' Cluster (Maybe Bool)
cAllowVersionUpgrade = lens _cAllowVersionUpgrade (\ s a -> s{_cAllowVersionUpgrade = a})

-- | The current state of the cluster. Possible values are the following:     * @available@      * @creating@      * @deleting@      * @final-snapshot@      * @hardware-failure@      * @incompatible-hsm@      * @incompatible-network@      * @incompatible-parameters@      * @incompatible-restore@      * @modifying@      * @rebooting@      * @renaming@      * @resizing@      * @rotating-keys@      * @storage-full@      * @updating-hsm@
cClusterStatus :: Lens' Cluster (Maybe Text)
cClusterStatus = lens _cClusterStatus (\ s a -> s{_cClusterStatus = a})

-- | A value that, if present, indicates that changes to the cluster are pending. Specific pending changes are identified by subelements.
cPendingModifiedValues :: Lens' Cluster (Maybe PendingModifiedValues)
cPendingModifiedValues = lens _cPendingModifiedValues (\ s a -> s{_cPendingModifiedValues = a})

-- | The list of tags for the cluster.
cTags :: Lens' Cluster [Tag]
cTags = lens _cTags (\ s a -> s{_cTags = a}) . _Default . _Coerce

-- | The nodes in the cluster.
cClusterNodes :: Lens' Cluster [ClusterNode]
cClusterNodes = lens _cClusterNodes (\ s a -> s{_cClusterNodes = a}) . _Default . _Coerce

-- | The name of the initial database that was created when the cluster was created. This same name is returned for the life of the cluster. If an initial database was not specified, a database named @dev@ dev was created by default.
cDBName :: Lens' Cluster (Maybe Text)
cDBName = lens _cDBName (\ s a -> s{_cDBName = a})

instance FromXML Cluster where
        parseXML x
          = Cluster' <$>
              (x .@? "RestoreStatus") <*>
                (x .@? "EnhancedVpcRouting")
                <*> (x .@? "ClusterSnapshotCopyStatus")
                <*> (x .@? "ClusterRevisionNumber")
                <*> (x .@? "PubliclyAccessible")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "VpcId")
                <*>
                (x .@? "ClusterSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "ClusterSecurityGroup"))
                <*> (x .@? "AutomatedSnapshotRetentionPeriod")
                <*> (x .@? "Encrypted")
                <*> (x .@? "ClusterSubnetGroupName")
                <*> (x .@? "ClusterIdentifier")
                <*> (x .@? "NumberOfNodes")
                <*> (x .@? "ClusterPublicKey")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*> (x .@? "ModifyStatus")
                <*> (x .@? "KmsKeyId")
                <*>
                (x .@? "ClusterParameterGroups" .!@ mempty >>=
                   may (parseXMLList "ClusterParameterGroup"))
                <*> (x .@? "AvailabilityZone")
                <*>
                (x .@? "VpcSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "VpcSecurityGroup"))
                <*> (x .@? "HsmStatus")
                <*>
                (x .@? "IamRoles" .!@ mempty >>=
                   may (parseXMLList "ClusterIamRole"))
                <*> (x .@? "ElasticIpStatus")
                <*> (x .@? "ClusterVersion")
                <*> (x .@? "NodeType")
                <*> (x .@? "ClusterCreateTime")
                <*> (x .@? "Endpoint")
                <*> (x .@? "AllowVersionUpgrade")
                <*> (x .@? "ClusterStatus")
                <*> (x .@? "PendingModifiedValues")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))
                <*>
                (x .@? "ClusterNodes" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "DBName")

instance Hashable Cluster where

instance NFData Cluster where

-- | An AWS Identity and Access Management (IAM) role that can be used by the associated Amazon Redshift cluster to access other AWS services.
--
--
--
-- /See:/ 'clusterIAMRole' smart constructor.
data ClusterIAMRole = ClusterIAMRole'
  { _cirIAMRoleARN  :: !(Maybe Text)
  , _cirApplyStatus :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterIAMRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirIAMRoleARN' - The Amazon Resource Name (ARN) of the IAM role, for example, @arn:aws:iam::123456789012:role/RedshiftCopyUnload@ .
--
-- * 'cirApplyStatus' - A value that describes the status of the IAM role's association with an Amazon Redshift cluster. The following are possible statuses and descriptions.     * @in-sync@ : The role is available for use by the cluster.     * @adding@ : The role is in the process of being associated with the cluster.     * @removing@ : The role is in the process of being disassociated with the cluster.
clusterIAMRole
    :: ClusterIAMRole
clusterIAMRole =
  ClusterIAMRole' {_cirIAMRoleARN = Nothing, _cirApplyStatus = Nothing}


-- | The Amazon Resource Name (ARN) of the IAM role, for example, @arn:aws:iam::123456789012:role/RedshiftCopyUnload@ .
cirIAMRoleARN :: Lens' ClusterIAMRole (Maybe Text)
cirIAMRoleARN = lens _cirIAMRoleARN (\ s a -> s{_cirIAMRoleARN = a})

-- | A value that describes the status of the IAM role's association with an Amazon Redshift cluster. The following are possible statuses and descriptions.     * @in-sync@ : The role is available for use by the cluster.     * @adding@ : The role is in the process of being associated with the cluster.     * @removing@ : The role is in the process of being disassociated with the cluster.
cirApplyStatus :: Lens' ClusterIAMRole (Maybe Text)
cirApplyStatus = lens _cirApplyStatus (\ s a -> s{_cirApplyStatus = a})

instance FromXML ClusterIAMRole where
        parseXML x
          = ClusterIAMRole' <$>
              (x .@? "IamRoleArn") <*> (x .@? "ApplyStatus")

instance Hashable ClusterIAMRole where

instance NFData ClusterIAMRole where

-- | The identifier of a node in a cluster.
--
--
--
-- /See:/ 'clusterNode' smart constructor.
data ClusterNode = ClusterNode'
  { _cnNodeRole         :: !(Maybe Text)
  , _cnPrivateIPAddress :: !(Maybe Text)
  , _cnPublicIPAddress  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnNodeRole' - Whether the node is a leader node or a compute node.
--
-- * 'cnPrivateIPAddress' - The private IP address of a node within a cluster.
--
-- * 'cnPublicIPAddress' - The public IP address of a node within a cluster.
clusterNode
    :: ClusterNode
clusterNode =
  ClusterNode'
    { _cnNodeRole = Nothing
    , _cnPrivateIPAddress = Nothing
    , _cnPublicIPAddress = Nothing
    }


-- | Whether the node is a leader node or a compute node.
cnNodeRole :: Lens' ClusterNode (Maybe Text)
cnNodeRole = lens _cnNodeRole (\ s a -> s{_cnNodeRole = a})

-- | The private IP address of a node within a cluster.
cnPrivateIPAddress :: Lens' ClusterNode (Maybe Text)
cnPrivateIPAddress = lens _cnPrivateIPAddress (\ s a -> s{_cnPrivateIPAddress = a})

-- | The public IP address of a node within a cluster.
cnPublicIPAddress :: Lens' ClusterNode (Maybe Text)
cnPublicIPAddress = lens _cnPublicIPAddress (\ s a -> s{_cnPublicIPAddress = a})

instance FromXML ClusterNode where
        parseXML x
          = ClusterNode' <$>
              (x .@? "NodeRole") <*> (x .@? "PrivateIPAddress") <*>
                (x .@? "PublicIPAddress")

instance Hashable ClusterNode where

instance NFData ClusterNode where

-- | Describes a parameter group.
--
--
--
-- /See:/ 'clusterParameterGroup' smart constructor.
data ClusterParameterGroup = ClusterParameterGroup'
  { _cpgParameterGroupFamily :: !(Maybe Text)
  , _cpgDescription          :: !(Maybe Text)
  , _cpgTags                 :: !(Maybe [Tag])
  , _cpgParameterGroupName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgParameterGroupFamily' - The name of the cluster parameter group family that this cluster parameter group is compatible with.
--
-- * 'cpgDescription' - The description of the parameter group.
--
-- * 'cpgTags' - The list of tags for the cluster parameter group.
--
-- * 'cpgParameterGroupName' - The name of the cluster parameter group.
clusterParameterGroup
    :: ClusterParameterGroup
clusterParameterGroup =
  ClusterParameterGroup'
    { _cpgParameterGroupFamily = Nothing
    , _cpgDescription = Nothing
    , _cpgTags = Nothing
    , _cpgParameterGroupName = Nothing
    }


-- | The name of the cluster parameter group family that this cluster parameter group is compatible with.
cpgParameterGroupFamily :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupFamily = lens _cpgParameterGroupFamily (\ s a -> s{_cpgParameterGroupFamily = a})

-- | The description of the parameter group.
cpgDescription :: Lens' ClusterParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\ s a -> s{_cpgDescription = a})

-- | The list of tags for the cluster parameter group.
cpgTags :: Lens' ClusterParameterGroup [Tag]
cpgTags = lens _cpgTags (\ s a -> s{_cpgTags = a}) . _Default . _Coerce

-- | The name of the cluster parameter group.
cpgParameterGroupName :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupName = lens _cpgParameterGroupName (\ s a -> s{_cpgParameterGroupName = a})

instance FromXML ClusterParameterGroup where
        parseXML x
          = ClusterParameterGroup' <$>
              (x .@? "ParameterGroupFamily") <*>
                (x .@? "Description")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))
                <*> (x .@? "ParameterGroupName")

instance Hashable ClusterParameterGroup where

instance NFData ClusterParameterGroup where

-- |
--
--
--
-- /See:/ 'clusterParameterGroupNameMessage' smart constructor.
data ClusterParameterGroupNameMessage = ClusterParameterGroupNameMessage'
  { _cpgnmParameterGroupStatus :: !(Maybe Text)
  , _cpgnmParameterGroupName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgnmParameterGroupStatus' - The status of the parameter group. For example, if you made a change to a parameter group name-value pair, then the change could be pending a reboot of an associated cluster.
--
-- * 'cpgnmParameterGroupName' - The name of the cluster parameter group.
clusterParameterGroupNameMessage
    :: ClusterParameterGroupNameMessage
clusterParameterGroupNameMessage =
  ClusterParameterGroupNameMessage'
    {_cpgnmParameterGroupStatus = Nothing, _cpgnmParameterGroupName = Nothing}


-- | The status of the parameter group. For example, if you made a change to a parameter group name-value pair, then the change could be pending a reboot of an associated cluster.
cpgnmParameterGroupStatus :: Lens' ClusterParameterGroupNameMessage (Maybe Text)
cpgnmParameterGroupStatus = lens _cpgnmParameterGroupStatus (\ s a -> s{_cpgnmParameterGroupStatus = a})

-- | The name of the cluster parameter group.
cpgnmParameterGroupName :: Lens' ClusterParameterGroupNameMessage (Maybe Text)
cpgnmParameterGroupName = lens _cpgnmParameterGroupName (\ s a -> s{_cpgnmParameterGroupName = a})

instance FromXML ClusterParameterGroupNameMessage
         where
        parseXML x
          = ClusterParameterGroupNameMessage' <$>
              (x .@? "ParameterGroupStatus") <*>
                (x .@? "ParameterGroupName")

instance Hashable ClusterParameterGroupNameMessage
         where

instance NFData ClusterParameterGroupNameMessage
         where

-- | Describes the status of a parameter group.
--
--
--
-- /See:/ 'clusterParameterGroupStatus' smart constructor.
data ClusterParameterGroupStatus = ClusterParameterGroupStatus'
  { _cpgsClusterParameterStatusList :: !(Maybe [ClusterParameterStatus])
  , _cpgsParameterApplyStatus       :: !(Maybe Text)
  , _cpgsParameterGroupName         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterParameterGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgsClusterParameterStatusList' - The list of parameter statuses. For more information about parameters and parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- * 'cpgsParameterApplyStatus' - The status of parameter updates.
--
-- * 'cpgsParameterGroupName' - The name of the cluster parameter group.
clusterParameterGroupStatus
    :: ClusterParameterGroupStatus
clusterParameterGroupStatus =
  ClusterParameterGroupStatus'
    { _cpgsClusterParameterStatusList = Nothing
    , _cpgsParameterApplyStatus = Nothing
    , _cpgsParameterGroupName = Nothing
    }


-- | The list of parameter statuses. For more information about parameters and parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
cpgsClusterParameterStatusList :: Lens' ClusterParameterGroupStatus [ClusterParameterStatus]
cpgsClusterParameterStatusList = lens _cpgsClusterParameterStatusList (\ s a -> s{_cpgsClusterParameterStatusList = a}) . _Default . _Coerce

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus = lens _cpgsParameterApplyStatus (\ s a -> s{_cpgsParameterApplyStatus = a})

-- | The name of the cluster parameter group.
cpgsParameterGroupName :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterGroupName = lens _cpgsParameterGroupName (\ s a -> s{_cpgsParameterGroupName = a})

instance FromXML ClusterParameterGroupStatus where
        parseXML x
          = ClusterParameterGroupStatus' <$>
              (x .@? "ClusterParameterStatusList" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "ParameterApplyStatus")
                <*> (x .@? "ParameterGroupName")

instance Hashable ClusterParameterGroupStatus where

instance NFData ClusterParameterGroupStatus where

-- | Describes the status of a parameter group.
--
--
--
-- /See:/ 'clusterParameterStatus' smart constructor.
data ClusterParameterStatus = ClusterParameterStatus'
  { _cpsParameterApplyErrorDescription :: !(Maybe Text)
  , _cpsParameterName                  :: !(Maybe Text)
  , _cpsParameterApplyStatus           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterParameterStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsParameterApplyErrorDescription' - The error that prevented the parameter from being applied to the database.
--
-- * 'cpsParameterName' - The name of the parameter.
--
-- * 'cpsParameterApplyStatus' - The status of the parameter that indicates whether the parameter is in sync with the database, waiting for a cluster reboot, or encountered an error when being applied. The following are possible statuses and descriptions.     * @in-sync@ : The parameter value is in sync with the database.     * @pending-reboot@ : The parameter value will be applied after the cluster reboots.     * @applying@ : The parameter value is being applied to the database.     * @invalid-parameter@ : Cannot apply the parameter value because it has an invalid value or syntax.     * @apply-deferred@ : The parameter contains static property changes. The changes are deferred until the cluster reboots.     * @apply-error@ : Cannot connect to the cluster. The parameter change will be applied after the cluster reboots.     * @unknown-error@ : Cannot apply the parameter change right now. The change will be applied after the cluster reboots.
clusterParameterStatus
    :: ClusterParameterStatus
clusterParameterStatus =
  ClusterParameterStatus'
    { _cpsParameterApplyErrorDescription = Nothing
    , _cpsParameterName = Nothing
    , _cpsParameterApplyStatus = Nothing
    }


-- | The error that prevented the parameter from being applied to the database.
cpsParameterApplyErrorDescription :: Lens' ClusterParameterStatus (Maybe Text)
cpsParameterApplyErrorDescription = lens _cpsParameterApplyErrorDescription (\ s a -> s{_cpsParameterApplyErrorDescription = a})

-- | The name of the parameter.
cpsParameterName :: Lens' ClusterParameterStatus (Maybe Text)
cpsParameterName = lens _cpsParameterName (\ s a -> s{_cpsParameterName = a})

-- | The status of the parameter that indicates whether the parameter is in sync with the database, waiting for a cluster reboot, or encountered an error when being applied. The following are possible statuses and descriptions.     * @in-sync@ : The parameter value is in sync with the database.     * @pending-reboot@ : The parameter value will be applied after the cluster reboots.     * @applying@ : The parameter value is being applied to the database.     * @invalid-parameter@ : Cannot apply the parameter value because it has an invalid value or syntax.     * @apply-deferred@ : The parameter contains static property changes. The changes are deferred until the cluster reboots.     * @apply-error@ : Cannot connect to the cluster. The parameter change will be applied after the cluster reboots.     * @unknown-error@ : Cannot apply the parameter change right now. The change will be applied after the cluster reboots.
cpsParameterApplyStatus :: Lens' ClusterParameterStatus (Maybe Text)
cpsParameterApplyStatus = lens _cpsParameterApplyStatus (\ s a -> s{_cpsParameterApplyStatus = a})

instance FromXML ClusterParameterStatus where
        parseXML x
          = ClusterParameterStatus' <$>
              (x .@? "ParameterApplyErrorDescription") <*>
                (x .@? "ParameterName")
                <*> (x .@? "ParameterApplyStatus")

instance Hashable ClusterParameterStatus where

instance NFData ClusterParameterStatus where

-- | Describes a security group.
--
--
--
-- /See:/ 'clusterSecurityGroup' smart constructor.
data ClusterSecurityGroup = ClusterSecurityGroup'
  { _cluClusterSecurityGroupName :: !(Maybe Text)
  , _cluIPRanges                 :: !(Maybe [IPRange])
  , _cluEC2SecurityGroups        :: !(Maybe [EC2SecurityGroup])
  , _cluDescription              :: !(Maybe Text)
  , _cluTags                     :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cluClusterSecurityGroupName' - The name of the cluster security group to which the operation was applied.
--
-- * 'cluIPRanges' - A list of IP ranges (CIDR blocks) that are permitted to access clusters associated with this cluster security group.
--
-- * 'cluEC2SecurityGroups' - A list of EC2 security groups that are permitted to access clusters associated with this cluster security group.
--
-- * 'cluDescription' - A description of the security group.
--
-- * 'cluTags' - The list of tags for the cluster security group.
clusterSecurityGroup
    :: ClusterSecurityGroup
clusterSecurityGroup =
  ClusterSecurityGroup'
    { _cluClusterSecurityGroupName = Nothing
    , _cluIPRanges = Nothing
    , _cluEC2SecurityGroups = Nothing
    , _cluDescription = Nothing
    , _cluTags = Nothing
    }


-- | The name of the cluster security group to which the operation was applied.
cluClusterSecurityGroupName :: Lens' ClusterSecurityGroup (Maybe Text)
cluClusterSecurityGroupName = lens _cluClusterSecurityGroupName (\ s a -> s{_cluClusterSecurityGroupName = a})

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters associated with this cluster security group.
cluIPRanges :: Lens' ClusterSecurityGroup [IPRange]
cluIPRanges = lens _cluIPRanges (\ s a -> s{_cluIPRanges = a}) . _Default . _Coerce

-- | A list of EC2 security groups that are permitted to access clusters associated with this cluster security group.
cluEC2SecurityGroups :: Lens' ClusterSecurityGroup [EC2SecurityGroup]
cluEC2SecurityGroups = lens _cluEC2SecurityGroups (\ s a -> s{_cluEC2SecurityGroups = a}) . _Default . _Coerce

-- | A description of the security group.
cluDescription :: Lens' ClusterSecurityGroup (Maybe Text)
cluDescription = lens _cluDescription (\ s a -> s{_cluDescription = a})

-- | The list of tags for the cluster security group.
cluTags :: Lens' ClusterSecurityGroup [Tag]
cluTags = lens _cluTags (\ s a -> s{_cluTags = a}) . _Default . _Coerce

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

instance Hashable ClusterSecurityGroup where

instance NFData ClusterSecurityGroup where

-- | Describes a cluster security group.
--
--
--
-- /See:/ 'clusterSecurityGroupMembership' smart constructor.
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership'
  { _csgmStatus                   :: !(Maybe Text)
  , _csgmClusterSecurityGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgmStatus' - The status of the cluster security group.
--
-- * 'csgmClusterSecurityGroupName' - The name of the cluster security group.
clusterSecurityGroupMembership
    :: ClusterSecurityGroupMembership
clusterSecurityGroupMembership =
  ClusterSecurityGroupMembership'
    {_csgmStatus = Nothing, _csgmClusterSecurityGroupName = Nothing}


-- | The status of the cluster security group.
csgmStatus :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\ s a -> s{_csgmStatus = a})

-- | The name of the cluster security group.
csgmClusterSecurityGroupName :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmClusterSecurityGroupName = lens _csgmClusterSecurityGroupName (\ s a -> s{_csgmClusterSecurityGroupName = a})

instance FromXML ClusterSecurityGroupMembership where
        parseXML x
          = ClusterSecurityGroupMembership' <$>
              (x .@? "Status") <*>
                (x .@? "ClusterSecurityGroupName")

instance Hashable ClusterSecurityGroupMembership
         where

instance NFData ClusterSecurityGroupMembership where

-- | Returns the destination region and retention period that are configured for cross-region snapshot copy.
--
--
--
-- /See:/ 'clusterSnapshotCopyStatus' smart constructor.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus'
  { _cscsRetentionPeriod       :: !(Maybe Integer)
  , _cscsDestinationRegion     :: !(Maybe Text)
  , _cscsSnapshotCopyGrantName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterSnapshotCopyStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscsRetentionPeriod' - The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
--
-- * 'cscsDestinationRegion' - The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
--
-- * 'cscsSnapshotCopyGrantName' - The name of the snapshot copy grant.
clusterSnapshotCopyStatus
    :: ClusterSnapshotCopyStatus
clusterSnapshotCopyStatus =
  ClusterSnapshotCopyStatus'
    { _cscsRetentionPeriod = Nothing
    , _cscsDestinationRegion = Nothing
    , _cscsSnapshotCopyGrantName = Nothing
    }


-- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
cscsRetentionPeriod :: Lens' ClusterSnapshotCopyStatus (Maybe Integer)
cscsRetentionPeriod = lens _cscsRetentionPeriod (\ s a -> s{_cscsRetentionPeriod = a})

-- | The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
cscsDestinationRegion :: Lens' ClusterSnapshotCopyStatus (Maybe Text)
cscsDestinationRegion = lens _cscsDestinationRegion (\ s a -> s{_cscsDestinationRegion = a})

-- | The name of the snapshot copy grant.
cscsSnapshotCopyGrantName :: Lens' ClusterSnapshotCopyStatus (Maybe Text)
cscsSnapshotCopyGrantName = lens _cscsSnapshotCopyGrantName (\ s a -> s{_cscsSnapshotCopyGrantName = a})

instance FromXML ClusterSnapshotCopyStatus where
        parseXML x
          = ClusterSnapshotCopyStatus' <$>
              (x .@? "RetentionPeriod") <*>
                (x .@? "DestinationRegion")
                <*> (x .@? "SnapshotCopyGrantName")

instance Hashable ClusterSnapshotCopyStatus where

instance NFData ClusterSnapshotCopyStatus where

-- | Describes a subnet group.
--
--
--
-- /See:/ 'clusterSubnetGroup' smart constructor.
data ClusterSubnetGroup = ClusterSubnetGroup'
  { _csgVPCId                  :: !(Maybe Text)
  , _csgSubnets                :: !(Maybe [Subnet])
  , _csgClusterSubnetGroupName :: !(Maybe Text)
  , _csgSubnetGroupStatus      :: !(Maybe Text)
  , _csgDescription            :: !(Maybe Text)
  , _csgTags                   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgVPCId' - The VPC ID of the cluster subnet group.
--
-- * 'csgSubnets' - A list of the VPC 'Subnet' elements.
--
-- * 'csgClusterSubnetGroupName' - The name of the cluster subnet group.
--
-- * 'csgSubnetGroupStatus' - The status of the cluster subnet group. Possible values are @Complete@ , @Incomplete@ and @Invalid@ .
--
-- * 'csgDescription' - The description of the cluster subnet group.
--
-- * 'csgTags' - The list of tags for the cluster subnet group.
clusterSubnetGroup
    :: ClusterSubnetGroup
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
csgVPCId = lens _csgVPCId (\ s a -> s{_csgVPCId = a})

-- | A list of the VPC 'Subnet' elements.
csgSubnets :: Lens' ClusterSubnetGroup [Subnet]
csgSubnets = lens _csgSubnets (\ s a -> s{_csgSubnets = a}) . _Default . _Coerce

-- | The name of the cluster subnet group.
csgClusterSubnetGroupName :: Lens' ClusterSubnetGroup (Maybe Text)
csgClusterSubnetGroupName = lens _csgClusterSubnetGroupName (\ s a -> s{_csgClusterSubnetGroupName = a})

-- | The status of the cluster subnet group. Possible values are @Complete@ , @Incomplete@ and @Invalid@ .
csgSubnetGroupStatus :: Lens' ClusterSubnetGroup (Maybe Text)
csgSubnetGroupStatus = lens _csgSubnetGroupStatus (\ s a -> s{_csgSubnetGroupStatus = a})

-- | The description of the cluster subnet group.
csgDescription :: Lens' ClusterSubnetGroup (Maybe Text)
csgDescription = lens _csgDescription (\ s a -> s{_csgDescription = a})

-- | The list of tags for the cluster subnet group.
csgTags :: Lens' ClusterSubnetGroup [Tag]
csgTags = lens _csgTags (\ s a -> s{_csgTags = a}) . _Default . _Coerce

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

instance Hashable ClusterSubnetGroup where

instance NFData ClusterSubnetGroup where

-- | Describes a cluster version, including the parameter group family and description of the version.
--
--
--
-- /See:/ 'clusterVersion' smart constructor.
data ClusterVersion = ClusterVersion'
  { _cvClusterParameterGroupFamily :: !(Maybe Text)
  , _cvClusterVersion              :: !(Maybe Text)
  , _cvDescription                 :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvClusterParameterGroupFamily' - The name of the cluster parameter group family for the cluster.
--
-- * 'cvClusterVersion' - The version number used by the cluster.
--
-- * 'cvDescription' - The description of the cluster version.
clusterVersion
    :: ClusterVersion
clusterVersion =
  ClusterVersion'
    { _cvClusterParameterGroupFamily = Nothing
    , _cvClusterVersion = Nothing
    , _cvDescription = Nothing
    }


-- | The name of the cluster parameter group family for the cluster.
cvClusterParameterGroupFamily :: Lens' ClusterVersion (Maybe Text)
cvClusterParameterGroupFamily = lens _cvClusterParameterGroupFamily (\ s a -> s{_cvClusterParameterGroupFamily = a})

-- | The version number used by the cluster.
cvClusterVersion :: Lens' ClusterVersion (Maybe Text)
cvClusterVersion = lens _cvClusterVersion (\ s a -> s{_cvClusterVersion = a})

-- | The description of the cluster version.
cvDescription :: Lens' ClusterVersion (Maybe Text)
cvDescription = lens _cvDescription (\ s a -> s{_cvDescription = a})

instance FromXML ClusterVersion where
        parseXML x
          = ClusterVersion' <$>
              (x .@? "ClusterParameterGroupFamily") <*>
                (x .@? "ClusterVersion")
                <*> (x .@? "Description")

instance Hashable ClusterVersion where

instance NFData ClusterVersion where

-- | Describes the default cluster parameters for a parameter group family.
--
--
--
-- /See:/ 'defaultClusterParameters' smart constructor.
data DefaultClusterParameters = DefaultClusterParameters'
  { _dcpMarker               :: !(Maybe Text)
  , _dcpParameters           :: !(Maybe [Parameter])
  , _dcpParameterGroupFamily :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefaultClusterParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpMarker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- * 'dcpParameters' - The list of cluster default parameters.
--
-- * 'dcpParameterGroupFamily' - The name of the cluster parameter group family to which the engine default parameters apply.
defaultClusterParameters
    :: DefaultClusterParameters
defaultClusterParameters =
  DefaultClusterParameters'
    { _dcpMarker = Nothing
    , _dcpParameters = Nothing
    , _dcpParameterGroupFamily = Nothing
    }


-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
dcpMarker :: Lens' DefaultClusterParameters (Maybe Text)
dcpMarker = lens _dcpMarker (\ s a -> s{_dcpMarker = a})

-- | The list of cluster default parameters.
dcpParameters :: Lens' DefaultClusterParameters [Parameter]
dcpParameters = lens _dcpParameters (\ s a -> s{_dcpParameters = a}) . _Default . _Coerce

-- | The name of the cluster parameter group family to which the engine default parameters apply.
dcpParameterGroupFamily :: Lens' DefaultClusterParameters (Maybe Text)
dcpParameterGroupFamily = lens _dcpParameterGroupFamily (\ s a -> s{_dcpParameterGroupFamily = a})

instance FromXML DefaultClusterParameters where
        parseXML x
          = DefaultClusterParameters' <$>
              (x .@? "Marker") <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "Parameter"))
                <*> (x .@? "ParameterGroupFamily")

instance Hashable DefaultClusterParameters where

instance NFData DefaultClusterParameters where

-- | Describes an Amazon EC2 security group.
--
--
--
-- /See:/ 'ec2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
  { _esgStatus                  :: !(Maybe Text)
  , _esgEC2SecurityGroupOwnerId :: !(Maybe Text)
  , _esgEC2SecurityGroupName    :: !(Maybe Text)
  , _esgTags                    :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EC2SecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esgStatus' - The status of the EC2 security group.
--
-- * 'esgEC2SecurityGroupOwnerId' - The AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field.
--
-- * 'esgEC2SecurityGroupName' - The name of the EC2 Security Group.
--
-- * 'esgTags' - The list of tags for the EC2 security group.
ec2SecurityGroup
    :: EC2SecurityGroup
ec2SecurityGroup =
  EC2SecurityGroup'
    { _esgStatus = Nothing
    , _esgEC2SecurityGroupOwnerId = Nothing
    , _esgEC2SecurityGroupName = Nothing
    , _esgTags = Nothing
    }


-- | The status of the EC2 security group.
esgStatus :: Lens' EC2SecurityGroup (Maybe Text)
esgStatus = lens _esgStatus (\ s a -> s{_esgStatus = a})

-- | The AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field.
esgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupOwnerId = lens _esgEC2SecurityGroupOwnerId (\ s a -> s{_esgEC2SecurityGroupOwnerId = a})

-- | The name of the EC2 Security Group.
esgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupName = lens _esgEC2SecurityGroupName (\ s a -> s{_esgEC2SecurityGroupName = a})

-- | The list of tags for the EC2 security group.
esgTags :: Lens' EC2SecurityGroup [Tag]
esgTags = lens _esgTags (\ s a -> s{_esgTags = a}) . _Default . _Coerce

instance FromXML EC2SecurityGroup where
        parseXML x
          = EC2SecurityGroup' <$>
              (x .@? "Status") <*>
                (x .@? "EC2SecurityGroupOwnerId")
                <*> (x .@? "EC2SecurityGroupName")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

instance Hashable EC2SecurityGroup where

instance NFData EC2SecurityGroup where

-- | Describes the status of the elastic IP (EIP) address.
--
--
--
-- /See:/ 'elasticIPStatus' smart constructor.
data ElasticIPStatus = ElasticIPStatus'
  { _eisStatus    :: !(Maybe Text)
  , _eisElasticIP :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticIPStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eisStatus' - The status of the elastic IP (EIP) address.
--
-- * 'eisElasticIP' - The elastic IP (EIP) address for the cluster.
elasticIPStatus
    :: ElasticIPStatus
elasticIPStatus =
  ElasticIPStatus' {_eisStatus = Nothing, _eisElasticIP = Nothing}


-- | The status of the elastic IP (EIP) address.
eisStatus :: Lens' ElasticIPStatus (Maybe Text)
eisStatus = lens _eisStatus (\ s a -> s{_eisStatus = a})

-- | The elastic IP (EIP) address for the cluster.
eisElasticIP :: Lens' ElasticIPStatus (Maybe Text)
eisElasticIP = lens _eisElasticIP (\ s a -> s{_eisElasticIP = a})

instance FromXML ElasticIPStatus where
        parseXML x
          = ElasticIPStatus' <$>
              (x .@? "Status") <*> (x .@? "ElasticIp")

instance Hashable ElasticIPStatus where

instance NFData ElasticIPStatus where

-- | Describes a connection endpoint.
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eAddress :: !(Maybe Text)
  , _ePort    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAddress' - The DNS address of the Cluster.
--
-- * 'ePort' - The port that the database engine is listening on.
endpoint
    :: Endpoint
endpoint = Endpoint' {_eAddress = Nothing, _ePort = Nothing}


-- | The DNS address of the Cluster.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\ s a -> s{_eAddress = a})

-- | The port that the database engine is listening on.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\ s a -> s{_ePort = a})

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$> (x .@? "Address") <*> (x .@? "Port")

instance Hashable Endpoint where

instance NFData Endpoint where

-- | Describes an event.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eSourceType       :: !(Maybe SourceType)
  , _eSeverity         :: !(Maybe Text)
  , _eSourceIdentifier :: !(Maybe Text)
  , _eDate             :: !(Maybe ISO8601)
  , _eEventCategories  :: !(Maybe [Text])
  , _eMessage          :: !(Maybe Text)
  , _eEventId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceType' - The source type for this event.
--
-- * 'eSeverity' - The severity of the event. Values: ERROR, INFO
--
-- * 'eSourceIdentifier' - The identifier for the source of the event.
--
-- * 'eDate' - The date and time of the event.
--
-- * 'eEventCategories' - A list of the event categories. Values: Configuration, Management, Monitoring, Security
--
-- * 'eMessage' - The text of this event.
--
-- * 'eEventId' - The identifier of the event.
event
    :: Event
event =
  Event'
    { _eSourceType = Nothing
    , _eSeverity = Nothing
    , _eSourceIdentifier = Nothing
    , _eDate = Nothing
    , _eEventCategories = Nothing
    , _eMessage = Nothing
    , _eEventId = Nothing
    }


-- | The source type for this event.
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\ s a -> s{_eSourceType = a})

-- | The severity of the event. Values: ERROR, INFO
eSeverity :: Lens' Event (Maybe Text)
eSeverity = lens _eSeverity (\ s a -> s{_eSeverity = a})

-- | The identifier for the source of the event.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier = lens _eSourceIdentifier (\ s a -> s{_eSourceIdentifier = a})

-- | The date and time of the event.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\ s a -> s{_eDate = a}) . mapping _Time

-- | A list of the event categories. Values: Configuration, Management, Monitoring, Security
eEventCategories :: Lens' Event [Text]
eEventCategories = lens _eEventCategories (\ s a -> s{_eEventCategories = a}) . _Default . _Coerce

-- | The text of this event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a})

-- | The identifier of the event.
eEventId :: Lens' Event (Maybe Text)
eEventId = lens _eEventId (\ s a -> s{_eEventId = a})

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

instance Hashable Event where

instance NFData Event where

-- | Describes event categories.
--
--
--
-- /See:/ 'eventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { _ecmSourceType :: !(Maybe Text)
  , _ecmEvents     :: !(Maybe [EventInfoMap])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventCategoriesMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecmSourceType' - The source type, such as cluster or cluster-snapshot, that the returned categories belong to.
--
-- * 'ecmEvents' - The events in the event category.
eventCategoriesMap
    :: EventCategoriesMap
eventCategoriesMap =
  EventCategoriesMap' {_ecmSourceType = Nothing, _ecmEvents = Nothing}


-- | The source type, such as cluster or cluster-snapshot, that the returned categories belong to.
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\ s a -> s{_ecmSourceType = a})

-- | The events in the event category.
ecmEvents :: Lens' EventCategoriesMap [EventInfoMap]
ecmEvents = lens _ecmEvents (\ s a -> s{_ecmEvents = a}) . _Default . _Coerce

instance FromXML EventCategoriesMap where
        parseXML x
          = EventCategoriesMap' <$>
              (x .@? "SourceType") <*>
                (x .@? "Events" .!@ mempty >>=
                   may (parseXMLList "EventInfoMap"))

instance Hashable EventCategoriesMap where

instance NFData EventCategoriesMap where

-- | Describes event information.
--
--
--
-- /See:/ 'eventInfoMap' smart constructor.
data EventInfoMap = EventInfoMap'
  { _eimEventDescription :: !(Maybe Text)
  , _eimSeverity         :: !(Maybe Text)
  , _eimEventCategories  :: !(Maybe [Text])
  , _eimEventId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventInfoMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eimEventDescription' - The description of an Amazon Redshift event.
--
-- * 'eimSeverity' - The severity of the event. Values: ERROR, INFO
--
-- * 'eimEventCategories' - The category of an Amazon Redshift event.
--
-- * 'eimEventId' - The identifier of an Amazon Redshift event.
eventInfoMap
    :: EventInfoMap
eventInfoMap =
  EventInfoMap'
    { _eimEventDescription = Nothing
    , _eimSeverity = Nothing
    , _eimEventCategories = Nothing
    , _eimEventId = Nothing
    }


-- | The description of an Amazon Redshift event.
eimEventDescription :: Lens' EventInfoMap (Maybe Text)
eimEventDescription = lens _eimEventDescription (\ s a -> s{_eimEventDescription = a})

-- | The severity of the event. Values: ERROR, INFO
eimSeverity :: Lens' EventInfoMap (Maybe Text)
eimSeverity = lens _eimSeverity (\ s a -> s{_eimSeverity = a})

-- | The category of an Amazon Redshift event.
eimEventCategories :: Lens' EventInfoMap [Text]
eimEventCategories = lens _eimEventCategories (\ s a -> s{_eimEventCategories = a}) . _Default . _Coerce

-- | The identifier of an Amazon Redshift event.
eimEventId :: Lens' EventInfoMap (Maybe Text)
eimEventId = lens _eimEventId (\ s a -> s{_eimEventId = a})

instance FromXML EventInfoMap where
        parseXML x
          = EventInfoMap' <$>
              (x .@? "EventDescription") <*> (x .@? "Severity") <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*> (x .@? "EventId")

instance Hashable EventInfoMap where

instance NFData EventInfoMap where

-- | Describes event subscriptions.
--
--
--
-- /See:/ 'eventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { _esStatus                   :: !(Maybe Text)
  , _esCustomerAWSId            :: !(Maybe Text)
  , _esCustSubscriptionId       :: !(Maybe Text)
  , _esSNSTopicARN              :: !(Maybe Text)
  , _esEnabled                  :: !(Maybe Bool)
  , _esSourceType               :: !(Maybe Text)
  , _esSeverity                 :: !(Maybe Text)
  , _esSubscriptionCreationTime :: !(Maybe ISO8601)
  , _esEventCategoriesList      :: !(Maybe [Text])
  , _esTags                     :: !(Maybe [Tag])
  , _esSourceIdsList            :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esStatus' - The status of the Amazon Redshift event notification subscription. Constraints:     * Can be one of the following: active | no-permission | topic-not-exist     * The status "no-permission" indicates that Amazon Redshift no longer has permission to post to the Amazon SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
--
-- * 'esCustomerAWSId' - The AWS customer account associated with the Amazon Redshift event notification subscription.
--
-- * 'esCustSubscriptionId' - The name of the Amazon Redshift event notification subscription.
--
-- * 'esSNSTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event notification subscription.
--
-- * 'esEnabled' - A Boolean value indicating whether the subscription is enabled. @true@ indicates the subscription is enabled.
--
-- * 'esSourceType' - The source type of the events returned the Amazon Redshift event notification, such as cluster, or cluster-snapshot.
--
-- * 'esSeverity' - The event severity specified in the Amazon Redshift event notification subscription. Values: ERROR, INFO
--
-- * 'esSubscriptionCreationTime' - The date and time the Amazon Redshift event notification subscription was created.
--
-- * 'esEventCategoriesList' - The list of Amazon Redshift event categories specified in the event notification subscription. Values: Configuration, Management, Monitoring, Security
--
-- * 'esTags' - The list of tags for the event subscription.
--
-- * 'esSourceIdsList' - A list of the sources that publish events to the Amazon Redshift event notification subscription.
eventSubscription
    :: EventSubscription
eventSubscription =
  EventSubscription'
    { _esStatus = Nothing
    , _esCustomerAWSId = Nothing
    , _esCustSubscriptionId = Nothing
    , _esSNSTopicARN = Nothing
    , _esEnabled = Nothing
    , _esSourceType = Nothing
    , _esSeverity = Nothing
    , _esSubscriptionCreationTime = Nothing
    , _esEventCategoriesList = Nothing
    , _esTags = Nothing
    , _esSourceIdsList = Nothing
    }


-- | The status of the Amazon Redshift event notification subscription. Constraints:     * Can be one of the following: active | no-permission | topic-not-exist     * The status "no-permission" indicates that Amazon Redshift no longer has permission to post to the Amazon SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\ s a -> s{_esStatus = a})

-- | The AWS customer account associated with the Amazon Redshift event notification subscription.
esCustomerAWSId :: Lens' EventSubscription (Maybe Text)
esCustomerAWSId = lens _esCustomerAWSId (\ s a -> s{_esCustomerAWSId = a})

-- | The name of the Amazon Redshift event notification subscription.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId = lens _esCustSubscriptionId (\ s a -> s{_esCustSubscriptionId = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event notification subscription.
esSNSTopicARN :: Lens' EventSubscription (Maybe Text)
esSNSTopicARN = lens _esSNSTopicARN (\ s a -> s{_esSNSTopicARN = a})

-- | A Boolean value indicating whether the subscription is enabled. @true@ indicates the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\ s a -> s{_esEnabled = a})

-- | The source type of the events returned the Amazon Redshift event notification, such as cluster, or cluster-snapshot.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\ s a -> s{_esSourceType = a})

-- | The event severity specified in the Amazon Redshift event notification subscription. Values: ERROR, INFO
esSeverity :: Lens' EventSubscription (Maybe Text)
esSeverity = lens _esSeverity (\ s a -> s{_esSeverity = a})

-- | The date and time the Amazon Redshift event notification subscription was created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe UTCTime)
esSubscriptionCreationTime = lens _esSubscriptionCreationTime (\ s a -> s{_esSubscriptionCreationTime = a}) . mapping _Time

-- | The list of Amazon Redshift event categories specified in the event notification subscription. Values: Configuration, Management, Monitoring, Security
esEventCategoriesList :: Lens' EventSubscription [Text]
esEventCategoriesList = lens _esEventCategoriesList (\ s a -> s{_esEventCategoriesList = a}) . _Default . _Coerce

-- | The list of tags for the event subscription.
esTags :: Lens' EventSubscription [Tag]
esTags = lens _esTags (\ s a -> s{_esTags = a}) . _Default . _Coerce

-- | A list of the sources that publish events to the Amazon Redshift event notification subscription.
esSourceIdsList :: Lens' EventSubscription [Text]
esSourceIdsList = lens _esSourceIdsList (\ s a -> s{_esSourceIdsList = a}) . _Default . _Coerce

instance FromXML EventSubscription where
        parseXML x
          = EventSubscription' <$>
              (x .@? "Status") <*> (x .@? "CustomerAwsId") <*>
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
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))
                <*>
                (x .@? "SourceIdsList" .!@ mempty >>=
                   may (parseXMLList "SourceId"))

instance Hashable EventSubscription where

instance NFData EventSubscription where

-- | Returns information about an HSM client certificate. The certificate is stored in a secure Hardware Storage Module (HSM), and used by the Amazon Redshift cluster to encrypt data files.
--
--
--
-- /See:/ 'hsmClientCertificate' smart constructor.
data HSMClientCertificate = HSMClientCertificate'
  { _hccHSMClientCertificateIdentifier :: !(Maybe Text)
  , _hccHSMClientCertificatePublicKey  :: !(Maybe Text)
  , _hccTags                           :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HSMClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hccHSMClientCertificateIdentifier' - The identifier of the HSM client certificate.
--
-- * 'hccHSMClientCertificatePublicKey' - The public key that the Amazon Redshift cluster will use to connect to the HSM. You must register the public key in the HSM.
--
-- * 'hccTags' - The list of tags for the HSM client certificate.
hsmClientCertificate
    :: HSMClientCertificate
hsmClientCertificate =
  HSMClientCertificate'
    { _hccHSMClientCertificateIdentifier = Nothing
    , _hccHSMClientCertificatePublicKey = Nothing
    , _hccTags = Nothing
    }


-- | The identifier of the HSM client certificate.
hccHSMClientCertificateIdentifier :: Lens' HSMClientCertificate (Maybe Text)
hccHSMClientCertificateIdentifier = lens _hccHSMClientCertificateIdentifier (\ s a -> s{_hccHSMClientCertificateIdentifier = a})

-- | The public key that the Amazon Redshift cluster will use to connect to the HSM. You must register the public key in the HSM.
hccHSMClientCertificatePublicKey :: Lens' HSMClientCertificate (Maybe Text)
hccHSMClientCertificatePublicKey = lens _hccHSMClientCertificatePublicKey (\ s a -> s{_hccHSMClientCertificatePublicKey = a})

-- | The list of tags for the HSM client certificate.
hccTags :: Lens' HSMClientCertificate [Tag]
hccTags = lens _hccTags (\ s a -> s{_hccTags = a}) . _Default . _Coerce

instance FromXML HSMClientCertificate where
        parseXML x
          = HSMClientCertificate' <$>
              (x .@? "HsmClientCertificateIdentifier") <*>
                (x .@? "HsmClientCertificatePublicKey")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

instance Hashable HSMClientCertificate where

instance NFData HSMClientCertificate where

-- | Returns information about an HSM configuration, which is an object that describes to Amazon Redshift clusters the information they require to connect to an HSM where they can store database encryption keys.
--
--
--
-- /See:/ 'hsmConfiguration' smart constructor.
data HSMConfiguration = HSMConfiguration'
  { _hcHSMConfigurationIdentifier :: !(Maybe Text)
  , _hcHSMPartitionName           :: !(Maybe Text)
  , _hcDescription                :: !(Maybe Text)
  , _hcTags                       :: !(Maybe [Tag])
  , _hcHSMIPAddress               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HSMConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcHSMConfigurationIdentifier' - The name of the Amazon Redshift HSM configuration.
--
-- * 'hcHSMPartitionName' - The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
--
-- * 'hcDescription' - A text description of the HSM configuration.
--
-- * 'hcTags' - The list of tags for the HSM configuration.
--
-- * 'hcHSMIPAddress' - The IP address that the Amazon Redshift cluster must use to access the HSM.
hsmConfiguration
    :: HSMConfiguration
hsmConfiguration =
  HSMConfiguration'
    { _hcHSMConfigurationIdentifier = Nothing
    , _hcHSMPartitionName = Nothing
    , _hcDescription = Nothing
    , _hcTags = Nothing
    , _hcHSMIPAddress = Nothing
    }


-- | The name of the Amazon Redshift HSM configuration.
hcHSMConfigurationIdentifier :: Lens' HSMConfiguration (Maybe Text)
hcHSMConfigurationIdentifier = lens _hcHSMConfigurationIdentifier (\ s a -> s{_hcHSMConfigurationIdentifier = a})

-- | The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
hcHSMPartitionName :: Lens' HSMConfiguration (Maybe Text)
hcHSMPartitionName = lens _hcHSMPartitionName (\ s a -> s{_hcHSMPartitionName = a})

-- | A text description of the HSM configuration.
hcDescription :: Lens' HSMConfiguration (Maybe Text)
hcDescription = lens _hcDescription (\ s a -> s{_hcDescription = a})

-- | The list of tags for the HSM configuration.
hcTags :: Lens' HSMConfiguration [Tag]
hcTags = lens _hcTags (\ s a -> s{_hcTags = a}) . _Default . _Coerce

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
hcHSMIPAddress :: Lens' HSMConfiguration (Maybe Text)
hcHSMIPAddress = lens _hcHSMIPAddress (\ s a -> s{_hcHSMIPAddress = a})

instance FromXML HSMConfiguration where
        parseXML x
          = HSMConfiguration' <$>
              (x .@? "HsmConfigurationIdentifier") <*>
                (x .@? "HsmPartitionName")
                <*> (x .@? "Description")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))
                <*> (x .@? "HsmIpAddress")

instance Hashable HSMConfiguration where

instance NFData HSMConfiguration where

-- | Describes the status of changes to HSM settings.
--
--
--
-- /See:/ 'hsmStatus' smart constructor.
data HSMStatus = HSMStatus'
  { _hsStatus                         :: !(Maybe Text)
  , _hsHSMConfigurationIdentifier     :: !(Maybe Text)
  , _hsHSMClientCertificateIdentifier :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HSMStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsStatus' - Reports whether the Amazon Redshift cluster has finished applying any HSM settings changes specified in a modify cluster command. Values: active, applying
--
-- * 'hsHSMConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- * 'hsHSMClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
hsmStatus
    :: HSMStatus
hsmStatus =
  HSMStatus'
    { _hsStatus = Nothing
    , _hsHSMConfigurationIdentifier = Nothing
    , _hsHSMClientCertificateIdentifier = Nothing
    }


-- | Reports whether the Amazon Redshift cluster has finished applying any HSM settings changes specified in a modify cluster command. Values: active, applying
hsStatus :: Lens' HSMStatus (Maybe Text)
hsStatus = lens _hsStatus (\ s a -> s{_hsStatus = a})

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
hsHSMConfigurationIdentifier :: Lens' HSMStatus (Maybe Text)
hsHSMConfigurationIdentifier = lens _hsHSMConfigurationIdentifier (\ s a -> s{_hsHSMConfigurationIdentifier = a})

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
hsHSMClientCertificateIdentifier :: Lens' HSMStatus (Maybe Text)
hsHSMClientCertificateIdentifier = lens _hsHSMClientCertificateIdentifier (\ s a -> s{_hsHSMClientCertificateIdentifier = a})

instance FromXML HSMStatus where
        parseXML x
          = HSMStatus' <$>
              (x .@? "Status") <*>
                (x .@? "HsmConfigurationIdentifier")
                <*> (x .@? "HsmClientCertificateIdentifier")

instance Hashable HSMStatus where

instance NFData HSMStatus where

-- | Describes an IP range used in a security group.
--
--
--
-- /See:/ 'ipRange' smart constructor.
data IPRange = IPRange'
  { _irStatus :: !(Maybe Text)
  , _irCIdRIP :: !(Maybe Text)
  , _irTags   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IPRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irStatus' - The status of the IP range, for example, "authorized".
--
-- * 'irCIdRIP' - The IP range in Classless Inter-Domain Routing (CIDR) notation.
--
-- * 'irTags' - The list of tags for the IP range.
ipRange
    :: IPRange
ipRange = IPRange' {_irStatus = Nothing, _irCIdRIP = Nothing, _irTags = Nothing}


-- | The status of the IP range, for example, "authorized".
irStatus :: Lens' IPRange (Maybe Text)
irStatus = lens _irStatus (\ s a -> s{_irStatus = a})

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
irCIdRIP :: Lens' IPRange (Maybe Text)
irCIdRIP = lens _irCIdRIP (\ s a -> s{_irCIdRIP = a})

-- | The list of tags for the IP range.
irTags :: Lens' IPRange [Tag]
irTags = lens _irTags (\ s a -> s{_irTags = a}) . _Default . _Coerce

instance FromXML IPRange where
        parseXML x
          = IPRange' <$>
              (x .@? "Status") <*> (x .@? "CIDRIP") <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

instance Hashable IPRange where

instance NFData IPRange where

-- | Describes the status of logging for a cluster.
--
--
--
-- /See:/ 'loggingStatus' smart constructor.
data LoggingStatus = LoggingStatus'
  { _lsLastFailureTime            :: !(Maybe ISO8601)
  , _lsLastSuccessfulDeliveryTime :: !(Maybe ISO8601)
  , _lsS3KeyPrefix                :: !(Maybe Text)
  , _lsBucketName                 :: !(Maybe Text)
  , _lsLoggingEnabled             :: !(Maybe Bool)
  , _lsLastFailureMessage         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoggingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsLastFailureTime' - The last time when logs failed to be delivered.
--
-- * 'lsLastSuccessfulDeliveryTime' - The last time that logs were delivered.
--
-- * 'lsS3KeyPrefix' - The prefix applied to the log file names.
--
-- * 'lsBucketName' - The name of the S3 bucket where the log files are stored.
--
-- * 'lsLoggingEnabled' - @true@ if logging is on, @false@ if logging is off.
--
-- * 'lsLastFailureMessage' - The message indicating that logs failed to be delivered.
loggingStatus
    :: LoggingStatus
loggingStatus =
  LoggingStatus'
    { _lsLastFailureTime = Nothing
    , _lsLastSuccessfulDeliveryTime = Nothing
    , _lsS3KeyPrefix = Nothing
    , _lsBucketName = Nothing
    , _lsLoggingEnabled = Nothing
    , _lsLastFailureMessage = Nothing
    }


-- | The last time when logs failed to be delivered.
lsLastFailureTime :: Lens' LoggingStatus (Maybe UTCTime)
lsLastFailureTime = lens _lsLastFailureTime (\ s a -> s{_lsLastFailureTime = a}) . mapping _Time

-- | The last time that logs were delivered.
lsLastSuccessfulDeliveryTime :: Lens' LoggingStatus (Maybe UTCTime)
lsLastSuccessfulDeliveryTime = lens _lsLastSuccessfulDeliveryTime (\ s a -> s{_lsLastSuccessfulDeliveryTime = a}) . mapping _Time

-- | The prefix applied to the log file names.
lsS3KeyPrefix :: Lens' LoggingStatus (Maybe Text)
lsS3KeyPrefix = lens _lsS3KeyPrefix (\ s a -> s{_lsS3KeyPrefix = a})

-- | The name of the S3 bucket where the log files are stored.
lsBucketName :: Lens' LoggingStatus (Maybe Text)
lsBucketName = lens _lsBucketName (\ s a -> s{_lsBucketName = a})

-- | @true@ if logging is on, @false@ if logging is off.
lsLoggingEnabled :: Lens' LoggingStatus (Maybe Bool)
lsLoggingEnabled = lens _lsLoggingEnabled (\ s a -> s{_lsLoggingEnabled = a})

-- | The message indicating that logs failed to be delivered.
lsLastFailureMessage :: Lens' LoggingStatus (Maybe Text)
lsLastFailureMessage = lens _lsLastFailureMessage (\ s a -> s{_lsLastFailureMessage = a})

instance FromXML LoggingStatus where
        parseXML x
          = LoggingStatus' <$>
              (x .@? "LastFailureTime") <*>
                (x .@? "LastSuccessfulDeliveryTime")
                <*> (x .@? "S3KeyPrefix")
                <*> (x .@? "BucketName")
                <*> (x .@? "LoggingEnabled")
                <*> (x .@? "LastFailureMessage")

instance Hashable LoggingStatus where

instance NFData LoggingStatus where

-- | Describes an orderable cluster option.
--
--
--
-- /See:/ 'orderableClusterOption' smart constructor.
data OrderableClusterOption = OrderableClusterOption'
  { _ocoAvailabilityZones :: !(Maybe [AvailabilityZone])
  , _ocoClusterType       :: !(Maybe Text)
  , _ocoClusterVersion    :: !(Maybe Text)
  , _ocoNodeType          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OrderableClusterOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocoAvailabilityZones' - A list of availability zones for the orderable cluster.
--
-- * 'ocoClusterType' - The cluster type, for example @multi-node@ .
--
-- * 'ocoClusterVersion' - The version of the orderable cluster.
--
-- * 'ocoNodeType' - The node type for the orderable cluster.
orderableClusterOption
    :: OrderableClusterOption
orderableClusterOption =
  OrderableClusterOption'
    { _ocoAvailabilityZones = Nothing
    , _ocoClusterType = Nothing
    , _ocoClusterVersion = Nothing
    , _ocoNodeType = Nothing
    }


-- | A list of availability zones for the orderable cluster.
ocoAvailabilityZones :: Lens' OrderableClusterOption [AvailabilityZone]
ocoAvailabilityZones = lens _ocoAvailabilityZones (\ s a -> s{_ocoAvailabilityZones = a}) . _Default . _Coerce

-- | The cluster type, for example @multi-node@ .
ocoClusterType :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterType = lens _ocoClusterType (\ s a -> s{_ocoClusterType = a})

-- | The version of the orderable cluster.
ocoClusterVersion :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterVersion = lens _ocoClusterVersion (\ s a -> s{_ocoClusterVersion = a})

-- | The node type for the orderable cluster.
ocoNodeType :: Lens' OrderableClusterOption (Maybe Text)
ocoNodeType = lens _ocoNodeType (\ s a -> s{_ocoNodeType = a})

instance FromXML OrderableClusterOption where
        parseXML x
          = OrderableClusterOption' <$>
              (x .@? "AvailabilityZones" .!@ mempty >>=
                 may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "ClusterType")
                <*> (x .@? "ClusterVersion")
                <*> (x .@? "NodeType")

instance Hashable OrderableClusterOption where

instance NFData OrderableClusterOption where

-- | Describes a parameter in a cluster parameter group.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pApplyType            :: !(Maybe ParameterApplyType)
  , _pParameterValue       :: !(Maybe Text)
  , _pMinimumEngineVersion :: !(Maybe Text)
  , _pSource               :: !(Maybe Text)
  , _pIsModifiable         :: !(Maybe Bool)
  , _pDataType             :: !(Maybe Text)
  , _pAllowedValues        :: !(Maybe Text)
  , _pParameterName        :: !(Maybe Text)
  , _pDescription          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pApplyType' - Specifies how to apply the WLM configuration parameter. Some properties can be applied dynamically, while other properties require that any associated clusters be rebooted for the configuration changes to be applied. For more information about parameters and parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- * 'pParameterValue' - The value of the parameter.
--
-- * 'pMinimumEngineVersion' - The earliest engine version to which the parameter can apply.
--
-- * 'pSource' - The source of the parameter value, such as "engine-default" or "user".
--
-- * 'pIsModifiable' - If @true@ , the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- * 'pDataType' - The data type of the parameter.
--
-- * 'pAllowedValues' - The valid range of values for the parameter.
--
-- * 'pParameterName' - The name of the parameter.
--
-- * 'pDescription' - A description of the parameter.
parameter
    :: Parameter
parameter =
  Parameter'
    { _pApplyType = Nothing
    , _pParameterValue = Nothing
    , _pMinimumEngineVersion = Nothing
    , _pSource = Nothing
    , _pIsModifiable = Nothing
    , _pDataType = Nothing
    , _pAllowedValues = Nothing
    , _pParameterName = Nothing
    , _pDescription = Nothing
    }


-- | Specifies how to apply the WLM configuration parameter. Some properties can be applied dynamically, while other properties require that any associated clusters be rebooted for the configuration changes to be applied. For more information about parameters and parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
pApplyType :: Lens' Parameter (Maybe ParameterApplyType)
pApplyType = lens _pApplyType (\ s a -> s{_pApplyType = a})

-- | The value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\ s a -> s{_pParameterValue = a})

-- | The earliest engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion = lens _pMinimumEngineVersion (\ s a -> s{_pMinimumEngineVersion = a})

-- | The source of the parameter value, such as "engine-default" or "user".
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\ s a -> s{_pSource = a})

-- | If @true@ , the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\ s a -> s{_pIsModifiable = a})

-- | The data type of the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\ s a -> s{_pDataType = a})

-- | The valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\ s a -> s{_pAllowedValues = a})

-- | The name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\ s a -> s{_pParameterName = a})

-- | A description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

instance FromXML Parameter where
        parseXML x
          = Parameter' <$>
              (x .@? "ApplyType") <*> (x .@? "ParameterValue") <*>
                (x .@? "MinimumEngineVersion")
                <*> (x .@? "Source")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "DataType")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "ParameterName")
                <*> (x .@? "Description")

instance Hashable Parameter where

instance NFData Parameter where

instance ToQuery Parameter where
        toQuery Parameter'{..}
          = mconcat
              ["ApplyType" =: _pApplyType,
               "ParameterValue" =: _pParameterValue,
               "MinimumEngineVersion" =: _pMinimumEngineVersion,
               "Source" =: _pSource,
               "IsModifiable" =: _pIsModifiable,
               "DataType" =: _pDataType,
               "AllowedValues" =: _pAllowedValues,
               "ParameterName" =: _pParameterName,
               "Description" =: _pDescription]

-- | Describes cluster attributes that are in a pending state. A change to one or more the attributes was requested and is in progress or will be applied.
--
--
--
-- /See:/ 'pendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { _pmvEnhancedVPCRouting               :: !(Maybe Bool)
  , _pmvMasterUserPassword               :: !(Maybe Text)
  , _pmvPubliclyAccessible               :: !(Maybe Bool)
  , _pmvAutomatedSnapshotRetentionPeriod :: !(Maybe Int)
  , _pmvClusterIdentifier                :: !(Maybe Text)
  , _pmvNumberOfNodes                    :: !(Maybe Int)
  , _pmvClusterType                      :: !(Maybe Text)
  , _pmvClusterVersion                   :: !(Maybe Text)
  , _pmvNodeType                         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmvEnhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
--
-- * 'pmvMasterUserPassword' - The pending or in-progress change of the master user password for the cluster.
--
-- * 'pmvPubliclyAccessible' - The pending or in-progress change of the ability to connect to the cluster from the public network.
--
-- * 'pmvAutomatedSnapshotRetentionPeriod' - The pending or in-progress change of the automated snapshot retention period.
--
-- * 'pmvClusterIdentifier' - The pending or in-progress change of the new identifier for the cluster.
--
-- * 'pmvNumberOfNodes' - The pending or in-progress change of the number of nodes in the cluster.
--
-- * 'pmvClusterType' - The pending or in-progress change of the cluster type.
--
-- * 'pmvClusterVersion' - The pending or in-progress change of the service version.
--
-- * 'pmvNodeType' - The pending or in-progress change of the cluster's node type.
pendingModifiedValues
    :: PendingModifiedValues
pendingModifiedValues =
  PendingModifiedValues'
    { _pmvEnhancedVPCRouting = Nothing
    , _pmvMasterUserPassword = Nothing
    , _pmvPubliclyAccessible = Nothing
    , _pmvAutomatedSnapshotRetentionPeriod = Nothing
    , _pmvClusterIdentifier = Nothing
    , _pmvNumberOfNodes = Nothing
    , _pmvClusterType = Nothing
    , _pmvClusterVersion = Nothing
    , _pmvNodeType = Nothing
    }


-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
pmvEnhancedVPCRouting :: Lens' PendingModifiedValues (Maybe Bool)
pmvEnhancedVPCRouting = lens _pmvEnhancedVPCRouting (\ s a -> s{_pmvEnhancedVPCRouting = a})

-- | The pending or in-progress change of the master user password for the cluster.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword = lens _pmvMasterUserPassword (\ s a -> s{_pmvMasterUserPassword = a})

-- | The pending or in-progress change of the ability to connect to the cluster from the public network.
pmvPubliclyAccessible :: Lens' PendingModifiedValues (Maybe Bool)
pmvPubliclyAccessible = lens _pmvPubliclyAccessible (\ s a -> s{_pmvPubliclyAccessible = a})

-- | The pending or in-progress change of the automated snapshot retention period.
pmvAutomatedSnapshotRetentionPeriod :: Lens' PendingModifiedValues (Maybe Int)
pmvAutomatedSnapshotRetentionPeriod = lens _pmvAutomatedSnapshotRetentionPeriod (\ s a -> s{_pmvAutomatedSnapshotRetentionPeriod = a})

-- | The pending or in-progress change of the new identifier for the cluster.
pmvClusterIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterIdentifier = lens _pmvClusterIdentifier (\ s a -> s{_pmvClusterIdentifier = a})

-- | The pending or in-progress change of the number of nodes in the cluster.
pmvNumberOfNodes :: Lens' PendingModifiedValues (Maybe Int)
pmvNumberOfNodes = lens _pmvNumberOfNodes (\ s a -> s{_pmvNumberOfNodes = a})

-- | The pending or in-progress change of the cluster type.
pmvClusterType :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterType = lens _pmvClusterType (\ s a -> s{_pmvClusterType = a})

-- | The pending or in-progress change of the service version.
pmvClusterVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterVersion = lens _pmvClusterVersion (\ s a -> s{_pmvClusterVersion = a})

-- | The pending or in-progress change of the cluster's node type.
pmvNodeType :: Lens' PendingModifiedValues (Maybe Text)
pmvNodeType = lens _pmvNodeType (\ s a -> s{_pmvNodeType = a})

instance FromXML PendingModifiedValues where
        parseXML x
          = PendingModifiedValues' <$>
              (x .@? "EnhancedVpcRouting") <*>
                (x .@? "MasterUserPassword")
                <*> (x .@? "PubliclyAccessible")
                <*> (x .@? "AutomatedSnapshotRetentionPeriod")
                <*> (x .@? "ClusterIdentifier")
                <*> (x .@? "NumberOfNodes")
                <*> (x .@? "ClusterType")
                <*> (x .@? "ClusterVersion")
                <*> (x .@? "NodeType")

instance Hashable PendingModifiedValues where

instance NFData PendingModifiedValues where

-- | Describes a recurring charge.
--
--
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { _rcRecurringChargeFrequency :: !(Maybe Text)
  , _rcRecurringChargeAmount    :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRecurringChargeFrequency' - The frequency at which the recurring charge amount is applied.
--
-- * 'rcRecurringChargeAmount' - The amount charged per the period of time specified by the recurring charge frequency.
recurringCharge
    :: RecurringCharge
recurringCharge =
  RecurringCharge'
    {_rcRecurringChargeFrequency = Nothing, _rcRecurringChargeAmount = Nothing}


-- | The frequency at which the recurring charge amount is applied.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\ s a -> s{_rcRecurringChargeFrequency = a})

-- | The amount charged per the period of time specified by the recurring charge frequency.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\ s a -> s{_rcRecurringChargeAmount = a})

instance FromXML RecurringCharge where
        parseXML x
          = RecurringCharge' <$>
              (x .@? "RecurringChargeFrequency") <*>
                (x .@? "RecurringChargeAmount")

instance Hashable RecurringCharge where

instance NFData RecurringCharge where

-- | Describes a reserved node. You can call the 'DescribeReservedNodeOfferings' API to obtain the available reserved node offerings.
--
--
--
-- /See:/ 'reservedNode' smart constructor.
data ReservedNode = ReservedNode'
  { _rnReservedNodeOfferingType :: !(Maybe ReservedNodeOfferingType)
  , _rnState                    :: !(Maybe Text)
  , _rnCurrencyCode             :: !(Maybe Text)
  , _rnStartTime                :: !(Maybe ISO8601)
  , _rnNodeCount                :: !(Maybe Int)
  , _rnReservedNodeId           :: !(Maybe Text)
  , _rnReservedNodeOfferingId   :: !(Maybe Text)
  , _rnRecurringCharges         :: !(Maybe [RecurringCharge])
  , _rnOfferingType             :: !(Maybe Text)
  , _rnUsagePrice               :: !(Maybe Double)
  , _rnNodeType                 :: !(Maybe Text)
  , _rnFixedPrice               :: !(Maybe Double)
  , _rnDuration                 :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnReservedNodeOfferingType' - Undocumented member.
--
-- * 'rnState' - The state of the reserved compute node. Possible Values:     * pending-payment-This reserved node has recently been purchased, and the sale has been approved, but payment has not yet been confirmed.     * active-This reserved node is owned by the caller and is available for use.     * payment-failed-Payment failed for the purchase attempt.
--
-- * 'rnCurrencyCode' - The currency code for the reserved cluster.
--
-- * 'rnStartTime' - The time the reservation started. You purchase a reserved node offering for a duration. This is the start time of that duration.
--
-- * 'rnNodeCount' - The number of reserved compute nodes.
--
-- * 'rnReservedNodeId' - The unique identifier for the reservation.
--
-- * 'rnReservedNodeOfferingId' - The identifier for the reserved node offering.
--
-- * 'rnRecurringCharges' - The recurring charges for the reserved node.
--
-- * 'rnOfferingType' - The anticipated utilization of the reserved node, as defined in the reserved node offering.
--
-- * 'rnUsagePrice' - The hourly rate Amazon Redshift charges you for this reserved node.
--
-- * 'rnNodeType' - The node type of the reserved node.
--
-- * 'rnFixedPrice' - The fixed cost Amazon Redshift charges you for this reserved node.
--
-- * 'rnDuration' - The duration of the node reservation in seconds.
reservedNode
    :: ReservedNode
reservedNode =
  ReservedNode'
    { _rnReservedNodeOfferingType = Nothing
    , _rnState = Nothing
    , _rnCurrencyCode = Nothing
    , _rnStartTime = Nothing
    , _rnNodeCount = Nothing
    , _rnReservedNodeId = Nothing
    , _rnReservedNodeOfferingId = Nothing
    , _rnRecurringCharges = Nothing
    , _rnOfferingType = Nothing
    , _rnUsagePrice = Nothing
    , _rnNodeType = Nothing
    , _rnFixedPrice = Nothing
    , _rnDuration = Nothing
    }


-- | Undocumented member.
rnReservedNodeOfferingType :: Lens' ReservedNode (Maybe ReservedNodeOfferingType)
rnReservedNodeOfferingType = lens _rnReservedNodeOfferingType (\ s a -> s{_rnReservedNodeOfferingType = a})

-- | The state of the reserved compute node. Possible Values:     * pending-payment-This reserved node has recently been purchased, and the sale has been approved, but payment has not yet been confirmed.     * active-This reserved node is owned by the caller and is available for use.     * payment-failed-Payment failed for the purchase attempt.
rnState :: Lens' ReservedNode (Maybe Text)
rnState = lens _rnState (\ s a -> s{_rnState = a})

-- | The currency code for the reserved cluster.
rnCurrencyCode :: Lens' ReservedNode (Maybe Text)
rnCurrencyCode = lens _rnCurrencyCode (\ s a -> s{_rnCurrencyCode = a})

-- | The time the reservation started. You purchase a reserved node offering for a duration. This is the start time of that duration.
rnStartTime :: Lens' ReservedNode (Maybe UTCTime)
rnStartTime = lens _rnStartTime (\ s a -> s{_rnStartTime = a}) . mapping _Time

-- | The number of reserved compute nodes.
rnNodeCount :: Lens' ReservedNode (Maybe Int)
rnNodeCount = lens _rnNodeCount (\ s a -> s{_rnNodeCount = a})

-- | The unique identifier for the reservation.
rnReservedNodeId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeId = lens _rnReservedNodeId (\ s a -> s{_rnReservedNodeId = a})

-- | The identifier for the reserved node offering.
rnReservedNodeOfferingId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeOfferingId = lens _rnReservedNodeOfferingId (\ s a -> s{_rnReservedNodeOfferingId = a})

-- | The recurring charges for the reserved node.
rnRecurringCharges :: Lens' ReservedNode [RecurringCharge]
rnRecurringCharges = lens _rnRecurringCharges (\ s a -> s{_rnRecurringCharges = a}) . _Default . _Coerce

-- | The anticipated utilization of the reserved node, as defined in the reserved node offering.
rnOfferingType :: Lens' ReservedNode (Maybe Text)
rnOfferingType = lens _rnOfferingType (\ s a -> s{_rnOfferingType = a})

-- | The hourly rate Amazon Redshift charges you for this reserved node.
rnUsagePrice :: Lens' ReservedNode (Maybe Double)
rnUsagePrice = lens _rnUsagePrice (\ s a -> s{_rnUsagePrice = a})

-- | The node type of the reserved node.
rnNodeType :: Lens' ReservedNode (Maybe Text)
rnNodeType = lens _rnNodeType (\ s a -> s{_rnNodeType = a})

-- | The fixed cost Amazon Redshift charges you for this reserved node.
rnFixedPrice :: Lens' ReservedNode (Maybe Double)
rnFixedPrice = lens _rnFixedPrice (\ s a -> s{_rnFixedPrice = a})

-- | The duration of the node reservation in seconds.
rnDuration :: Lens' ReservedNode (Maybe Int)
rnDuration = lens _rnDuration (\ s a -> s{_rnDuration = a})

instance FromXML ReservedNode where
        parseXML x
          = ReservedNode' <$>
              (x .@? "ReservedNodeOfferingType") <*>
                (x .@? "State")
                <*> (x .@? "CurrencyCode")
                <*> (x .@? "StartTime")
                <*> (x .@? "NodeCount")
                <*> (x .@? "ReservedNodeId")
                <*> (x .@? "ReservedNodeOfferingId")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*> (x .@? "NodeType")
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")

instance Hashable ReservedNode where

instance NFData ReservedNode where

-- | Describes a reserved node offering.
--
--
--
-- /See:/ 'reservedNodeOffering' smart constructor.
data ReservedNodeOffering = ReservedNodeOffering'
  { _rnoReservedNodeOfferingType :: !(Maybe ReservedNodeOfferingType)
  , _rnoCurrencyCode             :: !(Maybe Text)
  , _rnoReservedNodeOfferingId   :: !(Maybe Text)
  , _rnoRecurringCharges         :: !(Maybe [RecurringCharge])
  , _rnoOfferingType             :: !(Maybe Text)
  , _rnoUsagePrice               :: !(Maybe Double)
  , _rnoNodeType                 :: !(Maybe Text)
  , _rnoFixedPrice               :: !(Maybe Double)
  , _rnoDuration                 :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedNodeOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnoReservedNodeOfferingType' - Undocumented member.
--
-- * 'rnoCurrencyCode' - The currency code for the compute nodes offering.
--
-- * 'rnoReservedNodeOfferingId' - The offering identifier.
--
-- * 'rnoRecurringCharges' - The charge to your account regardless of whether you are creating any clusters using the node offering. Recurring charges are only in effect for heavy-utilization reserved nodes.
--
-- * 'rnoOfferingType' - The anticipated utilization of the reserved node, as defined in the reserved node offering.
--
-- * 'rnoUsagePrice' - The rate you are charged for each hour the cluster that is using the offering is running.
--
-- * 'rnoNodeType' - The node type offered by the reserved node offering.
--
-- * 'rnoFixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved node offering.
--
-- * 'rnoDuration' - The duration, in seconds, for which the offering will reserve the node.
reservedNodeOffering
    :: ReservedNodeOffering
reservedNodeOffering =
  ReservedNodeOffering'
    { _rnoReservedNodeOfferingType = Nothing
    , _rnoCurrencyCode = Nothing
    , _rnoReservedNodeOfferingId = Nothing
    , _rnoRecurringCharges = Nothing
    , _rnoOfferingType = Nothing
    , _rnoUsagePrice = Nothing
    , _rnoNodeType = Nothing
    , _rnoFixedPrice = Nothing
    , _rnoDuration = Nothing
    }


-- | Undocumented member.
rnoReservedNodeOfferingType :: Lens' ReservedNodeOffering (Maybe ReservedNodeOfferingType)
rnoReservedNodeOfferingType = lens _rnoReservedNodeOfferingType (\ s a -> s{_rnoReservedNodeOfferingType = a})

-- | The currency code for the compute nodes offering.
rnoCurrencyCode :: Lens' ReservedNodeOffering (Maybe Text)
rnoCurrencyCode = lens _rnoCurrencyCode (\ s a -> s{_rnoCurrencyCode = a})

-- | The offering identifier.
rnoReservedNodeOfferingId :: Lens' ReservedNodeOffering (Maybe Text)
rnoReservedNodeOfferingId = lens _rnoReservedNodeOfferingId (\ s a -> s{_rnoReservedNodeOfferingId = a})

-- | The charge to your account regardless of whether you are creating any clusters using the node offering. Recurring charges are only in effect for heavy-utilization reserved nodes.
rnoRecurringCharges :: Lens' ReservedNodeOffering [RecurringCharge]
rnoRecurringCharges = lens _rnoRecurringCharges (\ s a -> s{_rnoRecurringCharges = a}) . _Default . _Coerce

-- | The anticipated utilization of the reserved node, as defined in the reserved node offering.
rnoOfferingType :: Lens' ReservedNodeOffering (Maybe Text)
rnoOfferingType = lens _rnoOfferingType (\ s a -> s{_rnoOfferingType = a})

-- | The rate you are charged for each hour the cluster that is using the offering is running.
rnoUsagePrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoUsagePrice = lens _rnoUsagePrice (\ s a -> s{_rnoUsagePrice = a})

-- | The node type offered by the reserved node offering.
rnoNodeType :: Lens' ReservedNodeOffering (Maybe Text)
rnoNodeType = lens _rnoNodeType (\ s a -> s{_rnoNodeType = a})

-- | The upfront fixed charge you will pay to purchase the specific reserved node offering.
rnoFixedPrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoFixedPrice = lens _rnoFixedPrice (\ s a -> s{_rnoFixedPrice = a})

-- | The duration, in seconds, for which the offering will reserve the node.
rnoDuration :: Lens' ReservedNodeOffering (Maybe Int)
rnoDuration = lens _rnoDuration (\ s a -> s{_rnoDuration = a})

instance FromXML ReservedNodeOffering where
        parseXML x
          = ReservedNodeOffering' <$>
              (x .@? "ReservedNodeOfferingType") <*>
                (x .@? "CurrencyCode")
                <*> (x .@? "ReservedNodeOfferingId")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*> (x .@? "NodeType")
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")

instance Hashable ReservedNodeOffering where

instance NFData ReservedNodeOffering where

-- | Describes the status of a cluster restore action. Returns null if the cluster was not created by restoring a snapshot.
--
--
--
-- /See:/ 'restoreStatus' smart constructor.
data RestoreStatus = RestoreStatus'
  { _rsStatus                                 :: !(Maybe Text)
  , _rsEstimatedTimeToCompletionInSeconds     :: !(Maybe Integer)
  , _rsCurrentRestoreRateInMegaBytesPerSecond :: !(Maybe Double)
  , _rsProgressInMegaBytes                    :: !(Maybe Integer)
  , _rsElapsedTimeInSeconds                   :: !(Maybe Integer)
  , _rsSnapshotSizeInMegaBytes                :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsStatus' - The status of the restore action. Returns starting, restoring, completed, or failed.
--
-- * 'rsEstimatedTimeToCompletionInSeconds' - The estimate of the time remaining before the restore will complete. Returns 0 for a completed restore.
--
-- * 'rsCurrentRestoreRateInMegaBytesPerSecond' - The number of megabytes per second being transferred from the backup storage. Returns the average rate for a completed backup.
--
-- * 'rsProgressInMegaBytes' - The number of megabytes that have been transferred from snapshot storage.
--
-- * 'rsElapsedTimeInSeconds' - The amount of time an in-progress restore has been running, or the amount of time it took a completed restore to finish.
--
-- * 'rsSnapshotSizeInMegaBytes' - The size of the set of snapshot data used to restore the cluster.
restoreStatus
    :: RestoreStatus
restoreStatus =
  RestoreStatus'
    { _rsStatus = Nothing
    , _rsEstimatedTimeToCompletionInSeconds = Nothing
    , _rsCurrentRestoreRateInMegaBytesPerSecond = Nothing
    , _rsProgressInMegaBytes = Nothing
    , _rsElapsedTimeInSeconds = Nothing
    , _rsSnapshotSizeInMegaBytes = Nothing
    }


-- | The status of the restore action. Returns starting, restoring, completed, or failed.
rsStatus :: Lens' RestoreStatus (Maybe Text)
rsStatus = lens _rsStatus (\ s a -> s{_rsStatus = a})

-- | The estimate of the time remaining before the restore will complete. Returns 0 for a completed restore.
rsEstimatedTimeToCompletionInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsEstimatedTimeToCompletionInSeconds = lens _rsEstimatedTimeToCompletionInSeconds (\ s a -> s{_rsEstimatedTimeToCompletionInSeconds = a})

-- | The number of megabytes per second being transferred from the backup storage. Returns the average rate for a completed backup.
rsCurrentRestoreRateInMegaBytesPerSecond :: Lens' RestoreStatus (Maybe Double)
rsCurrentRestoreRateInMegaBytesPerSecond = lens _rsCurrentRestoreRateInMegaBytesPerSecond (\ s a -> s{_rsCurrentRestoreRateInMegaBytesPerSecond = a})

-- | The number of megabytes that have been transferred from snapshot storage.
rsProgressInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsProgressInMegaBytes = lens _rsProgressInMegaBytes (\ s a -> s{_rsProgressInMegaBytes = a})

-- | The amount of time an in-progress restore has been running, or the amount of time it took a completed restore to finish.
rsElapsedTimeInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsElapsedTimeInSeconds = lens _rsElapsedTimeInSeconds (\ s a -> s{_rsElapsedTimeInSeconds = a})

-- | The size of the set of snapshot data used to restore the cluster.
rsSnapshotSizeInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsSnapshotSizeInMegaBytes = lens _rsSnapshotSizeInMegaBytes (\ s a -> s{_rsSnapshotSizeInMegaBytes = a})

instance FromXML RestoreStatus where
        parseXML x
          = RestoreStatus' <$>
              (x .@? "Status") <*>
                (x .@? "EstimatedTimeToCompletionInSeconds")
                <*> (x .@? "CurrentRestoreRateInMegaBytesPerSecond")
                <*> (x .@? "ProgressInMegaBytes")
                <*> (x .@? "ElapsedTimeInSeconds")
                <*> (x .@? "SnapshotSizeInMegaBytes")

instance Hashable RestoreStatus where

instance NFData RestoreStatus where

-- | Describes a snapshot.
--
--
--
-- /See:/ 'snapshot' smart constructor.
data Snapshot = Snapshot'
  { _sStatus :: !(Maybe Text)
  , _sRestorableNodeTypes :: !(Maybe [Text])
  , _sAccountsWithRestoreAccess :: !(Maybe [AccountWithRestoreAccess])
  , _sEnhancedVPCRouting :: !(Maybe Bool)
  , _sSnapshotIdentifier :: !(Maybe Text)
  , _sEncryptedWithHSM :: !(Maybe Bool)
  , _sMasterUsername :: !(Maybe Text)
  , _sSourceRegion :: !(Maybe Text)
  , _sVPCId :: !(Maybe Text)
  , _sBackupProgressInMegaBytes :: !(Maybe Double)
  , _sEncrypted :: !(Maybe Bool)
  , _sClusterIdentifier :: !(Maybe Text)
  , _sNumberOfNodes :: !(Maybe Int)
  , _sSnapshotType :: !(Maybe Text)
  , _sKMSKeyId :: !(Maybe Text)
  , _sAvailabilityZone :: !(Maybe Text)
  , _sCurrentBackupRateInMegaBytesPerSecond :: !(Maybe Double)
  , _sSnapshotCreateTime :: !(Maybe ISO8601)
  , _sClusterVersion :: !(Maybe Text)
  , _sOwnerAccount :: !(Maybe Text)
  , _sNodeType :: !(Maybe Text)
  , _sElapsedTimeInSeconds :: !(Maybe Integer)
  , _sClusterCreateTime :: !(Maybe ISO8601)
  , _sEstimatedSecondsToCompletion :: !(Maybe Integer)
  , _sActualIncrementalBackupSizeInMegaBytes :: !(Maybe Double)
  , _sTags :: !(Maybe [Tag])
  , _sPort :: !(Maybe Int)
  , _sTotalBackupSizeInMegaBytes :: !(Maybe Double)
  , _sDBName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus' - The snapshot status. The value of the status depends on the API operation used.      * 'CreateClusterSnapshot' and 'CopyClusterSnapshot' returns status as "creating".      * 'DescribeClusterSnapshots' returns status as "creating", "available", "final snapshot", or "failed".     * 'DeleteClusterSnapshot' returns status as "deleted".
--
-- * 'sRestorableNodeTypes' - The list of node types that this cluster snapshot is able to restore into.
--
-- * 'sAccountsWithRestoreAccess' - A list of the AWS customer accounts authorized to restore the snapshot. Returns @null@ if no accounts are authorized. Visible only to the snapshot owner.
--
-- * 'sEnhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
--
-- * 'sSnapshotIdentifier' - The snapshot identifier that is provided in the request.
--
-- * 'sEncryptedWithHSM' - A boolean that indicates whether the snapshot data is encrypted using the HSM keys of the source cluster. @true@ indicates that the data is encrypted using HSM keys.
--
-- * 'sMasterUsername' - The master user name for the cluster.
--
-- * 'sSourceRegion' - The source region from which the snapshot was copied.
--
-- * 'sVPCId' - The VPC identifier of the cluster if the snapshot is from a cluster in a VPC. Otherwise, this field is not in the output.
--
-- * 'sBackupProgressInMegaBytes' - The number of megabytes that have been transferred to the snapshot backup.
--
-- * 'sEncrypted' - If @true@ , the data in the snapshot is encrypted at rest.
--
-- * 'sClusterIdentifier' - The identifier of the cluster for which the snapshot was taken.
--
-- * 'sNumberOfNodes' - The number of nodes in the cluster.
--
-- * 'sSnapshotType' - The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' will be of type "manual".
--
-- * 'sKMSKeyId' - The AWS Key Management Service (KMS) key ID of the encryption key that was used to encrypt data in the cluster from which the snapshot was taken.
--
-- * 'sAvailabilityZone' - The Availability Zone in which the cluster was created.
--
-- * 'sCurrentBackupRateInMegaBytesPerSecond' - The number of megabytes per second being transferred to the snapshot backup. Returns @0@ for a completed backup.
--
-- * 'sSnapshotCreateTime' - The time (UTC) when Amazon Redshift began the snapshot. A snapshot contains a copy of the cluster data as of this exact time.
--
-- * 'sClusterVersion' - The version ID of the Amazon Redshift engine that is running on the cluster.
--
-- * 'sOwnerAccount' - For manual snapshots, the AWS customer account used to create or copy the snapshot. For automatic snapshots, the owner of the cluster. The owner can perform all snapshot actions, such as sharing a manual snapshot.
--
-- * 'sNodeType' - The node type of the nodes in the cluster.
--
-- * 'sElapsedTimeInSeconds' - The amount of time an in-progress snapshot backup has been running, or the amount of time it took a completed backup to finish.
--
-- * 'sClusterCreateTime' - The time (UTC) when the cluster was originally created.
--
-- * 'sEstimatedSecondsToCompletion' - The estimate of the time remaining before the snapshot backup will complete. Returns @0@ for a completed backup.
--
-- * 'sActualIncrementalBackupSizeInMegaBytes' - The size of the incremental backup.
--
-- * 'sTags' - The list of tags for the cluster snapshot.
--
-- * 'sPort' - The port that the cluster is listening on.
--
-- * 'sTotalBackupSizeInMegaBytes' - The size of the complete set of backup data that would be used to restore the cluster.
--
-- * 'sDBName' - The name of the database that was created when the cluster was created.
snapshot
    :: Snapshot
snapshot =
  Snapshot'
    { _sStatus = Nothing
    , _sRestorableNodeTypes = Nothing
    , _sAccountsWithRestoreAccess = Nothing
    , _sEnhancedVPCRouting = Nothing
    , _sSnapshotIdentifier = Nothing
    , _sEncryptedWithHSM = Nothing
    , _sMasterUsername = Nothing
    , _sSourceRegion = Nothing
    , _sVPCId = Nothing
    , _sBackupProgressInMegaBytes = Nothing
    , _sEncrypted = Nothing
    , _sClusterIdentifier = Nothing
    , _sNumberOfNodes = Nothing
    , _sSnapshotType = Nothing
    , _sKMSKeyId = Nothing
    , _sAvailabilityZone = Nothing
    , _sCurrentBackupRateInMegaBytesPerSecond = Nothing
    , _sSnapshotCreateTime = Nothing
    , _sClusterVersion = Nothing
    , _sOwnerAccount = Nothing
    , _sNodeType = Nothing
    , _sElapsedTimeInSeconds = Nothing
    , _sClusterCreateTime = Nothing
    , _sEstimatedSecondsToCompletion = Nothing
    , _sActualIncrementalBackupSizeInMegaBytes = Nothing
    , _sTags = Nothing
    , _sPort = Nothing
    , _sTotalBackupSizeInMegaBytes = Nothing
    , _sDBName = Nothing
    }


-- | The snapshot status. The value of the status depends on the API operation used.      * 'CreateClusterSnapshot' and 'CopyClusterSnapshot' returns status as "creating".      * 'DescribeClusterSnapshots' returns status as "creating", "available", "final snapshot", or "failed".     * 'DeleteClusterSnapshot' returns status as "deleted".
sStatus :: Lens' Snapshot (Maybe Text)
sStatus = lens _sStatus (\ s a -> s{_sStatus = a})

-- | The list of node types that this cluster snapshot is able to restore into.
sRestorableNodeTypes :: Lens' Snapshot [Text]
sRestorableNodeTypes = lens _sRestorableNodeTypes (\ s a -> s{_sRestorableNodeTypes = a}) . _Default . _Coerce

-- | A list of the AWS customer accounts authorized to restore the snapshot. Returns @null@ if no accounts are authorized. Visible only to the snapshot owner.
sAccountsWithRestoreAccess :: Lens' Snapshot [AccountWithRestoreAccess]
sAccountsWithRestoreAccess = lens _sAccountsWithRestoreAccess (\ s a -> s{_sAccountsWithRestoreAccess = a}) . _Default . _Coerce

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
sEnhancedVPCRouting :: Lens' Snapshot (Maybe Bool)
sEnhancedVPCRouting = lens _sEnhancedVPCRouting (\ s a -> s{_sEnhancedVPCRouting = a})

-- | The snapshot identifier that is provided in the request.
sSnapshotIdentifier :: Lens' Snapshot (Maybe Text)
sSnapshotIdentifier = lens _sSnapshotIdentifier (\ s a -> s{_sSnapshotIdentifier = a})

-- | A boolean that indicates whether the snapshot data is encrypted using the HSM keys of the source cluster. @true@ indicates that the data is encrypted using HSM keys.
sEncryptedWithHSM :: Lens' Snapshot (Maybe Bool)
sEncryptedWithHSM = lens _sEncryptedWithHSM (\ s a -> s{_sEncryptedWithHSM = a})

-- | The master user name for the cluster.
sMasterUsername :: Lens' Snapshot (Maybe Text)
sMasterUsername = lens _sMasterUsername (\ s a -> s{_sMasterUsername = a})

-- | The source region from which the snapshot was copied.
sSourceRegion :: Lens' Snapshot (Maybe Text)
sSourceRegion = lens _sSourceRegion (\ s a -> s{_sSourceRegion = a})

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a VPC. Otherwise, this field is not in the output.
sVPCId :: Lens' Snapshot (Maybe Text)
sVPCId = lens _sVPCId (\ s a -> s{_sVPCId = a})

-- | The number of megabytes that have been transferred to the snapshot backup.
sBackupProgressInMegaBytes :: Lens' Snapshot (Maybe Double)
sBackupProgressInMegaBytes = lens _sBackupProgressInMegaBytes (\ s a -> s{_sBackupProgressInMegaBytes = a})

-- | If @true@ , the data in the snapshot is encrypted at rest.
sEncrypted :: Lens' Snapshot (Maybe Bool)
sEncrypted = lens _sEncrypted (\ s a -> s{_sEncrypted = a})

-- | The identifier of the cluster for which the snapshot was taken.
sClusterIdentifier :: Lens' Snapshot (Maybe Text)
sClusterIdentifier = lens _sClusterIdentifier (\ s a -> s{_sClusterIdentifier = a})

-- | The number of nodes in the cluster.
sNumberOfNodes :: Lens' Snapshot (Maybe Int)
sNumberOfNodes = lens _sNumberOfNodes (\ s a -> s{_sNumberOfNodes = a})

-- | The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' will be of type "manual".
sSnapshotType :: Lens' Snapshot (Maybe Text)
sSnapshotType = lens _sSnapshotType (\ s a -> s{_sSnapshotType = a})

-- | The AWS Key Management Service (KMS) key ID of the encryption key that was used to encrypt data in the cluster from which the snapshot was taken.
sKMSKeyId :: Lens' Snapshot (Maybe Text)
sKMSKeyId = lens _sKMSKeyId (\ s a -> s{_sKMSKeyId = a})

-- | The Availability Zone in which the cluster was created.
sAvailabilityZone :: Lens' Snapshot (Maybe Text)
sAvailabilityZone = lens _sAvailabilityZone (\ s a -> s{_sAvailabilityZone = a})

-- | The number of megabytes per second being transferred to the snapshot backup. Returns @0@ for a completed backup.
sCurrentBackupRateInMegaBytesPerSecond :: Lens' Snapshot (Maybe Double)
sCurrentBackupRateInMegaBytesPerSecond = lens _sCurrentBackupRateInMegaBytesPerSecond (\ s a -> s{_sCurrentBackupRateInMegaBytesPerSecond = a})

-- | The time (UTC) when Amazon Redshift began the snapshot. A snapshot contains a copy of the cluster data as of this exact time.
sSnapshotCreateTime :: Lens' Snapshot (Maybe UTCTime)
sSnapshotCreateTime = lens _sSnapshotCreateTime (\ s a -> s{_sSnapshotCreateTime = a}) . mapping _Time

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
sClusterVersion :: Lens' Snapshot (Maybe Text)
sClusterVersion = lens _sClusterVersion (\ s a -> s{_sClusterVersion = a})

-- | For manual snapshots, the AWS customer account used to create or copy the snapshot. For automatic snapshots, the owner of the cluster. The owner can perform all snapshot actions, such as sharing a manual snapshot.
sOwnerAccount :: Lens' Snapshot (Maybe Text)
sOwnerAccount = lens _sOwnerAccount (\ s a -> s{_sOwnerAccount = a})

-- | The node type of the nodes in the cluster.
sNodeType :: Lens' Snapshot (Maybe Text)
sNodeType = lens _sNodeType (\ s a -> s{_sNodeType = a})

-- | The amount of time an in-progress snapshot backup has been running, or the amount of time it took a completed backup to finish.
sElapsedTimeInSeconds :: Lens' Snapshot (Maybe Integer)
sElapsedTimeInSeconds = lens _sElapsedTimeInSeconds (\ s a -> s{_sElapsedTimeInSeconds = a})

-- | The time (UTC) when the cluster was originally created.
sClusterCreateTime :: Lens' Snapshot (Maybe UTCTime)
sClusterCreateTime = lens _sClusterCreateTime (\ s a -> s{_sClusterCreateTime = a}) . mapping _Time

-- | The estimate of the time remaining before the snapshot backup will complete. Returns @0@ for a completed backup.
sEstimatedSecondsToCompletion :: Lens' Snapshot (Maybe Integer)
sEstimatedSecondsToCompletion = lens _sEstimatedSecondsToCompletion (\ s a -> s{_sEstimatedSecondsToCompletion = a})

-- | The size of the incremental backup.
sActualIncrementalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
sActualIncrementalBackupSizeInMegaBytes = lens _sActualIncrementalBackupSizeInMegaBytes (\ s a -> s{_sActualIncrementalBackupSizeInMegaBytes = a})

-- | The list of tags for the cluster snapshot.
sTags :: Lens' Snapshot [Tag]
sTags = lens _sTags (\ s a -> s{_sTags = a}) . _Default . _Coerce

-- | The port that the cluster is listening on.
sPort :: Lens' Snapshot (Maybe Int)
sPort = lens _sPort (\ s a -> s{_sPort = a})

-- | The size of the complete set of backup data that would be used to restore the cluster.
sTotalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
sTotalBackupSizeInMegaBytes = lens _sTotalBackupSizeInMegaBytes (\ s a -> s{_sTotalBackupSizeInMegaBytes = a})

-- | The name of the database that was created when the cluster was created.
sDBName :: Lens' Snapshot (Maybe Text)
sDBName = lens _sDBName (\ s a -> s{_sDBName = a})

instance FromXML Snapshot where
        parseXML x
          = Snapshot' <$>
              (x .@? "Status") <*>
                (x .@? "RestorableNodeTypes" .!@ mempty >>=
                   may (parseXMLList "NodeType"))
                <*>
                (x .@? "AccountsWithRestoreAccess" .!@ mempty >>=
                   may (parseXMLList "AccountWithRestoreAccess"))
                <*> (x .@? "EnhancedVpcRouting")
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
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "AvailabilityZone")
                <*> (x .@? "CurrentBackupRateInMegaBytesPerSecond")
                <*> (x .@? "SnapshotCreateTime")
                <*> (x .@? "ClusterVersion")
                <*> (x .@? "OwnerAccount")
                <*> (x .@? "NodeType")
                <*> (x .@? "ElapsedTimeInSeconds")
                <*> (x .@? "ClusterCreateTime")
                <*> (x .@? "EstimatedSecondsToCompletion")
                <*> (x .@? "ActualIncrementalBackupSizeInMegaBytes")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))
                <*> (x .@? "Port")
                <*> (x .@? "TotalBackupSizeInMegaBytes")
                <*> (x .@? "DBName")

instance Hashable Snapshot where

instance NFData Snapshot where

-- | The snapshot copy grant that grants Amazon Redshift permission to encrypt copied snapshots with the specified customer master key (CMK) from AWS KMS in the destination region.
--
--
-- For more information about managing snapshot copy grants, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption> in the /Amazon Redshift Cluster Management Guide/ .
--
--
-- /See:/ 'snapshotCopyGrant' smart constructor.
data SnapshotCopyGrant = SnapshotCopyGrant'
  { _scgKMSKeyId              :: !(Maybe Text)
  , _scgSnapshotCopyGrantName :: !(Maybe Text)
  , _scgTags                  :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SnapshotCopyGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scgKMSKeyId' - The unique identifier of the customer master key (CMK) in AWS KMS to which Amazon Redshift is granted permission.
--
-- * 'scgSnapshotCopyGrantName' - The name of the snapshot copy grant.
--
-- * 'scgTags' - A list of tag instances.
snapshotCopyGrant
    :: SnapshotCopyGrant
snapshotCopyGrant =
  SnapshotCopyGrant'
    { _scgKMSKeyId = Nothing
    , _scgSnapshotCopyGrantName = Nothing
    , _scgTags = Nothing
    }


-- | The unique identifier of the customer master key (CMK) in AWS KMS to which Amazon Redshift is granted permission.
scgKMSKeyId :: Lens' SnapshotCopyGrant (Maybe Text)
scgKMSKeyId = lens _scgKMSKeyId (\ s a -> s{_scgKMSKeyId = a})

-- | The name of the snapshot copy grant.
scgSnapshotCopyGrantName :: Lens' SnapshotCopyGrant (Maybe Text)
scgSnapshotCopyGrantName = lens _scgSnapshotCopyGrantName (\ s a -> s{_scgSnapshotCopyGrantName = a})

-- | A list of tag instances.
scgTags :: Lens' SnapshotCopyGrant [Tag]
scgTags = lens _scgTags (\ s a -> s{_scgTags = a}) . _Default . _Coerce

instance FromXML SnapshotCopyGrant where
        parseXML x
          = SnapshotCopyGrant' <$>
              (x .@? "KmsKeyId") <*>
                (x .@? "SnapshotCopyGrantName")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "Tag"))

instance Hashable SnapshotCopyGrant where

instance NFData SnapshotCopyGrant where

-- | Describes a subnet.
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _sSubnetStatus           :: !(Maybe Text)
  , _sSubnetIdentifier       :: !(Maybe Text)
  , _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetStatus' - The status of the subnet.
--
-- * 'sSubnetIdentifier' - The identifier of the subnet.
--
-- * 'sSubnetAvailabilityZone' - Undocumented member.
subnet
    :: Subnet
subnet =
  Subnet'
    { _sSubnetStatus = Nothing
    , _sSubnetIdentifier = Nothing
    , _sSubnetAvailabilityZone = Nothing
    }


-- | The status of the subnet.
sSubnetStatus :: Lens' Subnet (Maybe Text)
sSubnetStatus = lens _sSubnetStatus (\ s a -> s{_sSubnetStatus = a})

-- | The identifier of the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\ s a -> s{_sSubnetIdentifier = a})

-- | Undocumented member.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\ s a -> s{_sSubnetAvailabilityZone = a})

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (x .@? "SubnetStatus") <*> (x .@? "SubnetIdentifier")
                <*> (x .@? "SubnetAvailabilityZone")

instance Hashable Subnet where

instance NFData Subnet where

-- | A list of supported platforms for orderable clusters.
--
--
--
-- /See:/ 'supportedPlatform' smart constructor.
newtype SupportedPlatform = SupportedPlatform'
  { _spName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SupportedPlatform' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spName' - Undocumented member.
supportedPlatform
    :: SupportedPlatform
supportedPlatform = SupportedPlatform' {_spName = Nothing}


-- | Undocumented member.
spName :: Lens' SupportedPlatform (Maybe Text)
spName = lens _spName (\ s a -> s{_spName = a})

instance FromXML SupportedPlatform where
        parseXML x = SupportedPlatform' <$> (x .@? "Name")

instance Hashable SupportedPlatform where

instance NFData SupportedPlatform where

-- | Describes the status of a 'RestoreTableFromClusterSnapshot' operation.
--
--
--
-- /See:/ 'tableRestoreStatus' smart constructor.
data TableRestoreStatus = TableRestoreStatus'
  { _trsStatus                :: !(Maybe TableRestoreStatusType)
  , _trsTargetSchemaName      :: !(Maybe Text)
  , _trsSnapshotIdentifier    :: !(Maybe Text)
  , _trsSourceDatabaseName    :: !(Maybe Text)
  , _trsTableRestoreRequestId :: !(Maybe Text)
  , _trsNewTableName          :: !(Maybe Text)
  , _trsTargetDatabaseName    :: !(Maybe Text)
  , _trsSourceSchemaName      :: !(Maybe Text)
  , _trsClusterIdentifier     :: !(Maybe Text)
  , _trsRequestTime           :: !(Maybe ISO8601)
  , _trsSourceTableName       :: !(Maybe Text)
  , _trsTotalDataInMegaBytes  :: !(Maybe Integer)
  , _trsProgressInMegaBytes   :: !(Maybe Integer)
  , _trsMessage               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TableRestoreStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trsStatus' - A value that describes the current state of the table restore request. Valid Values: @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@
--
-- * 'trsTargetSchemaName' - The name of the schema to restore the table to.
--
-- * 'trsSnapshotIdentifier' - The identifier of the snapshot that the table is being restored from.
--
-- * 'trsSourceDatabaseName' - The name of the source database that contains the table being restored.
--
-- * 'trsTableRestoreRequestId' - The unique identifier for the table restore request.
--
-- * 'trsNewTableName' - The name of the table to create as a result of the table restore request.
--
-- * 'trsTargetDatabaseName' - The name of the database to restore the table to.
--
-- * 'trsSourceSchemaName' - The name of the source schema that contains the table being restored.
--
-- * 'trsClusterIdentifier' - The identifier of the Amazon Redshift cluster that the table is being restored to.
--
-- * 'trsRequestTime' - The time that the table restore request was made, in Universal Coordinated Time (UTC).
--
-- * 'trsSourceTableName' - The name of the source table being restored.
--
-- * 'trsTotalDataInMegaBytes' - The total amount of data to restore to the new table, in megabytes (MB).
--
-- * 'trsProgressInMegaBytes' - The amount of data restored to the new table so far, in megabytes (MB).
--
-- * 'trsMessage' - A description of the status of the table restore request. Status values include @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@ .
tableRestoreStatus
    :: TableRestoreStatus
tableRestoreStatus =
  TableRestoreStatus'
    { _trsStatus = Nothing
    , _trsTargetSchemaName = Nothing
    , _trsSnapshotIdentifier = Nothing
    , _trsSourceDatabaseName = Nothing
    , _trsTableRestoreRequestId = Nothing
    , _trsNewTableName = Nothing
    , _trsTargetDatabaseName = Nothing
    , _trsSourceSchemaName = Nothing
    , _trsClusterIdentifier = Nothing
    , _trsRequestTime = Nothing
    , _trsSourceTableName = Nothing
    , _trsTotalDataInMegaBytes = Nothing
    , _trsProgressInMegaBytes = Nothing
    , _trsMessage = Nothing
    }


-- | A value that describes the current state of the table restore request. Valid Values: @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@
trsStatus :: Lens' TableRestoreStatus (Maybe TableRestoreStatusType)
trsStatus = lens _trsStatus (\ s a -> s{_trsStatus = a})

-- | The name of the schema to restore the table to.
trsTargetSchemaName :: Lens' TableRestoreStatus (Maybe Text)
trsTargetSchemaName = lens _trsTargetSchemaName (\ s a -> s{_trsTargetSchemaName = a})

-- | The identifier of the snapshot that the table is being restored from.
trsSnapshotIdentifier :: Lens' TableRestoreStatus (Maybe Text)
trsSnapshotIdentifier = lens _trsSnapshotIdentifier (\ s a -> s{_trsSnapshotIdentifier = a})

-- | The name of the source database that contains the table being restored.
trsSourceDatabaseName :: Lens' TableRestoreStatus (Maybe Text)
trsSourceDatabaseName = lens _trsSourceDatabaseName (\ s a -> s{_trsSourceDatabaseName = a})

-- | The unique identifier for the table restore request.
trsTableRestoreRequestId :: Lens' TableRestoreStatus (Maybe Text)
trsTableRestoreRequestId = lens _trsTableRestoreRequestId (\ s a -> s{_trsTableRestoreRequestId = a})

-- | The name of the table to create as a result of the table restore request.
trsNewTableName :: Lens' TableRestoreStatus (Maybe Text)
trsNewTableName = lens _trsNewTableName (\ s a -> s{_trsNewTableName = a})

-- | The name of the database to restore the table to.
trsTargetDatabaseName :: Lens' TableRestoreStatus (Maybe Text)
trsTargetDatabaseName = lens _trsTargetDatabaseName (\ s a -> s{_trsTargetDatabaseName = a})

-- | The name of the source schema that contains the table being restored.
trsSourceSchemaName :: Lens' TableRestoreStatus (Maybe Text)
trsSourceSchemaName = lens _trsSourceSchemaName (\ s a -> s{_trsSourceSchemaName = a})

-- | The identifier of the Amazon Redshift cluster that the table is being restored to.
trsClusterIdentifier :: Lens' TableRestoreStatus (Maybe Text)
trsClusterIdentifier = lens _trsClusterIdentifier (\ s a -> s{_trsClusterIdentifier = a})

-- | The time that the table restore request was made, in Universal Coordinated Time (UTC).
trsRequestTime :: Lens' TableRestoreStatus (Maybe UTCTime)
trsRequestTime = lens _trsRequestTime (\ s a -> s{_trsRequestTime = a}) . mapping _Time

-- | The name of the source table being restored.
trsSourceTableName :: Lens' TableRestoreStatus (Maybe Text)
trsSourceTableName = lens _trsSourceTableName (\ s a -> s{_trsSourceTableName = a})

-- | The total amount of data to restore to the new table, in megabytes (MB).
trsTotalDataInMegaBytes :: Lens' TableRestoreStatus (Maybe Integer)
trsTotalDataInMegaBytes = lens _trsTotalDataInMegaBytes (\ s a -> s{_trsTotalDataInMegaBytes = a})

-- | The amount of data restored to the new table so far, in megabytes (MB).
trsProgressInMegaBytes :: Lens' TableRestoreStatus (Maybe Integer)
trsProgressInMegaBytes = lens _trsProgressInMegaBytes (\ s a -> s{_trsProgressInMegaBytes = a})

-- | A description of the status of the table restore request. Status values include @SUCCEEDED@ , @FAILED@ , @CANCELED@ , @PENDING@ , @IN_PROGRESS@ .
trsMessage :: Lens' TableRestoreStatus (Maybe Text)
trsMessage = lens _trsMessage (\ s a -> s{_trsMessage = a})

instance FromXML TableRestoreStatus where
        parseXML x
          = TableRestoreStatus' <$>
              (x .@? "Status") <*> (x .@? "TargetSchemaName") <*>
                (x .@? "SnapshotIdentifier")
                <*> (x .@? "SourceDatabaseName")
                <*> (x .@? "TableRestoreRequestId")
                <*> (x .@? "NewTableName")
                <*> (x .@? "TargetDatabaseName")
                <*> (x .@? "SourceSchemaName")
                <*> (x .@? "ClusterIdentifier")
                <*> (x .@? "RequestTime")
                <*> (x .@? "SourceTableName")
                <*> (x .@? "TotalDataInMegaBytes")
                <*> (x .@? "ProgressInMegaBytes")
                <*> (x .@? "Message")

instance Hashable TableRestoreStatus where

instance NFData TableRestoreStatus where

-- | A tag consisting of a name/value pair for a resource.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value for the resource tag.
--
-- * 'tagKey' - The key, or name, for the resource tag.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The value for the resource tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key, or name, for the resource tag.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance Hashable Tag where

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | A tag and its associated resource.
--
--
--
-- /See:/ 'taggedResource' smart constructor.
data TaggedResource = TaggedResource'
  { _trTag          :: !(Maybe Tag)
  , _trResourceType :: !(Maybe Text)
  , _trResourceName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TaggedResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trTag' - The tag for the resource.
--
-- * 'trResourceType' - The type of resource with which the tag is associated. Valid resource types are:      * Cluster     * CIDR/IP     * EC2 security group     * Snapshot     * Cluster security group     * Subnet group     * HSM connection     * HSM certificate     * Parameter group For more information about Amazon Redshift resource types and constructing ARNs, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Constructing an Amazon Redshift Amazon Resource Name (ARN)> in the Amazon Redshift Cluster Management Guide.
--
-- * 'trResourceName' - The Amazon Resource Name (ARN) with which the tag is associated. For example, @arn:aws:redshift:us-east-1:123456789:cluster:t1@ .
taggedResource
    :: TaggedResource
taggedResource =
  TaggedResource'
    {_trTag = Nothing, _trResourceType = Nothing, _trResourceName = Nothing}


-- | The tag for the resource.
trTag :: Lens' TaggedResource (Maybe Tag)
trTag = lens _trTag (\ s a -> s{_trTag = a})

-- | The type of resource with which the tag is associated. Valid resource types are:      * Cluster     * CIDR/IP     * EC2 security group     * Snapshot     * Cluster security group     * Subnet group     * HSM connection     * HSM certificate     * Parameter group For more information about Amazon Redshift resource types and constructing ARNs, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Constructing an Amazon Redshift Amazon Resource Name (ARN)> in the Amazon Redshift Cluster Management Guide.
trResourceType :: Lens' TaggedResource (Maybe Text)
trResourceType = lens _trResourceType (\ s a -> s{_trResourceType = a})

-- | The Amazon Resource Name (ARN) with which the tag is associated. For example, @arn:aws:redshift:us-east-1:123456789:cluster:t1@ .
trResourceName :: Lens' TaggedResource (Maybe Text)
trResourceName = lens _trResourceName (\ s a -> s{_trResourceName = a})

instance FromXML TaggedResource where
        parseXML x
          = TaggedResource' <$>
              (x .@? "Tag") <*> (x .@? "ResourceType") <*>
                (x .@? "ResourceName")

instance Hashable TaggedResource where

instance NFData TaggedResource where

-- | Describes the members of a VPC security group.
--
--
--
-- /See:/ 'vpcSecurityGroupMembership' smart constructor.
data VPCSecurityGroupMembership = VPCSecurityGroupMembership'
  { _vsgmStatus             :: !(Maybe Text)
  , _vsgmVPCSecurityGroupId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsgmStatus' - The status of the VPC security group.
--
-- * 'vsgmVPCSecurityGroupId' - The identifier of the VPC security group.
vpcSecurityGroupMembership
    :: VPCSecurityGroupMembership
vpcSecurityGroupMembership =
  VPCSecurityGroupMembership'
    {_vsgmStatus = Nothing, _vsgmVPCSecurityGroupId = Nothing}


-- | The status of the VPC security group.
vsgmStatus :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\ s a -> s{_vsgmStatus = a})

-- | The identifier of the VPC security group.
vsgmVPCSecurityGroupId :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmVPCSecurityGroupId = lens _vsgmVPCSecurityGroupId (\ s a -> s{_vsgmVPCSecurityGroupId = a})

instance FromXML VPCSecurityGroupMembership where
        parseXML x
          = VPCSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "VpcSecurityGroupId")

instance Hashable VPCSecurityGroupMembership where

instance NFData VPCSecurityGroupMembership where
