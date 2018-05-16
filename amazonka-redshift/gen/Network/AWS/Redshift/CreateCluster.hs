{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cluster.
--
--
-- To create the cluster in Virtual Private Cloud (VPC), you must provide a cluster subnet group name. The cluster subnet group identifies the subnets of your VPC that Amazon Redshift uses when creating the cluster. For more information about managing clusters, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.CreateCluster
    (
    -- * Creating a Request
      createCluster
    , CreateCluster
    -- * Request Lenses
    , ccEnhancedVPCRouting
    , ccAdditionalInfo
    , ccPubliclyAccessible
    , ccHSMConfigurationIdentifier
    , ccClusterSecurityGroups
    , ccAutomatedSnapshotRetentionPeriod
    , ccEncrypted
    , ccClusterSubnetGroupName
    , ccHSMClientCertificateIdentifier
    , ccNumberOfNodes
    , ccElasticIP
    , ccPreferredMaintenanceWindow
    , ccKMSKeyId
    , ccAvailabilityZone
    , ccVPCSecurityGroupIds
    , ccIAMRoles
    , ccClusterType
    , ccClusterVersion
    , ccAllowVersionUpgrade
    , ccClusterParameterGroupName
    , ccTags
    , ccPort
    , ccDBName
    , ccClusterIdentifier
    , ccNodeType
    , ccMasterUsername
    , ccMasterUserPassword

    -- * Destructuring the Response
    , createClusterResponse
    , CreateClusterResponse
    -- * Response Lenses
    , ccrsCluster
    , ccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createCluster' smart constructor.
data CreateCluster = CreateCluster'
  { _ccEnhancedVPCRouting               :: !(Maybe Bool)
  , _ccAdditionalInfo                   :: !(Maybe Text)
  , _ccPubliclyAccessible               :: !(Maybe Bool)
  , _ccHSMConfigurationIdentifier       :: !(Maybe Text)
  , _ccClusterSecurityGroups            :: !(Maybe [Text])
  , _ccAutomatedSnapshotRetentionPeriod :: !(Maybe Int)
  , _ccEncrypted                        :: !(Maybe Bool)
  , _ccClusterSubnetGroupName           :: !(Maybe Text)
  , _ccHSMClientCertificateIdentifier   :: !(Maybe Text)
  , _ccNumberOfNodes                    :: !(Maybe Int)
  , _ccElasticIP                        :: !(Maybe Text)
  , _ccPreferredMaintenanceWindow       :: !(Maybe Text)
  , _ccKMSKeyId                         :: !(Maybe Text)
  , _ccAvailabilityZone                 :: !(Maybe Text)
  , _ccVPCSecurityGroupIds              :: !(Maybe [Text])
  , _ccIAMRoles                         :: !(Maybe [Text])
  , _ccClusterType                      :: !(Maybe Text)
  , _ccClusterVersion                   :: !(Maybe Text)
  , _ccAllowVersionUpgrade              :: !(Maybe Bool)
  , _ccClusterParameterGroupName        :: !(Maybe Text)
  , _ccTags                             :: !(Maybe [Tag])
  , _ccPort                             :: !(Maybe Int)
  , _ccDBName                           :: !(Maybe Text)
  , _ccClusterIdentifier                :: !Text
  , _ccNodeType                         :: !Text
  , _ccMasterUsername                   :: !Text
  , _ccMasterUserPassword               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccEnhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
--
-- * 'ccAdditionalInfo' - Reserved.
--
-- * 'ccPubliclyAccessible' - If @true@ , the cluster can be accessed from a public network.
--
-- * 'ccHSMConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- * 'ccClusterSecurityGroups' - A list of security groups to be associated with this cluster. Default: The default cluster security group for Amazon Redshift.
--
-- * 'ccAutomatedSnapshotRetentionPeriod' - The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .  Default: @1@  Constraints: Must be a value from 0 to 35.
--
-- * 'ccEncrypted' - If @true@ , the data in the cluster is encrypted at rest.  Default: false
--
-- * 'ccClusterSubnetGroupName' - The name of a cluster subnet group to be associated with this cluster. If this parameter is not provided the resulting cluster will be deployed outside virtual private cloud (VPC).
--
-- * 'ccHSMClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- * 'ccNumberOfNodes' - The number of compute nodes in the cluster. This parameter is required when the __ClusterType__ parameter is specified as @multi-node@ .  For information about determining how many nodes you need, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .  If you don't specify this parameter, you get a single-node cluster. When requesting a multi-node cluster, you must specify the number of nodes that you want in the cluster. Default: @1@  Constraints: Value must be at least 1 and no more than 100.
--
-- * 'ccElasticIP' - The Elastic IP (EIP) address for the cluster. Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
--
-- * 'ccPreferredMaintenanceWindow' - The weekly time range (in UTC) during which automated cluster maintenance can occur. Format: @ddd:hh24:mi-ddd:hh24:mi@  Default: A 30-minute window selected at random from an 8-hour block of time per region, occurring on a random day of the week. For more information about the time blocks for each region, see <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide. Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints: Minimum 30-minute window.
--
-- * 'ccKMSKeyId' - The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
--
-- * 'ccAvailabilityZone' - The EC2 Availability Zone (AZ) in which you want Amazon Redshift to provision the cluster. For example, if you have several EC2 instances running in a specific Availability Zone, then you might want the cluster to be provisioned in the same zone in order to decrease network latency. Default: A random, system-chosen Availability Zone in the region that is specified by the endpoint. Example: @us-east-1d@  Constraint: The specified Availability Zone must be in the same region as the current endpoint.
--
-- * 'ccVPCSecurityGroupIds' - A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster. Default: The default VPC security group is associated with the cluster.
--
-- * 'ccIAMRoles' - A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request. A cluster can have up to 10 IAM roles associated with it at any time.
--
-- * 'ccClusterType' - The type of the cluster. When cluster type is specified as     * @single-node@ , the __NumberOfNodes__ parameter is not required.     * @multi-node@ , the __NumberOfNodes__ parameter is required. Valid Values: @multi-node@ | @single-node@  Default: @multi-node@
--
-- * 'ccClusterVersion' - The version of the Amazon Redshift engine software that you want to deploy on the cluster. The version selected runs on all the nodes in the cluster. Constraints: Only version 1.0 is currently available. Example: @1.0@
--
-- * 'ccAllowVersionUpgrade' - If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster. When a new major version of the Amazon Redshift engine is released, you can request that the service automatically apply upgrades during the maintenance window to the Amazon Redshift engine that is running on your cluster. Default: @true@
--
-- * 'ccClusterParameterGroupName' - The name of the parameter group to be associated with this cluster. Default: The default Amazon Redshift cluster parameter group. For information about the default parameter group, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>  Constraints:     * Must be 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'ccTags' - A list of tag instances.
--
-- * 'ccPort' - The port number on which the cluster accepts incoming connections. The cluster is accessible only via the JDBC and ODBC connection strings. Part of the connection string requires the port on which the cluster will listen for incoming connections. Default: @5439@  Valid Values: @1150-65535@
--
-- * 'ccDBName' - The name of the first database to be created when the cluster is created. To create additional databases after the cluster is created, connect to the cluster with a SQL client and use SQL commands to create a database. For more information, go to <http://docs.aws.amazon.com/redshift/latest/dg/t_creating_database.html Create a Database> in the Amazon Redshift Database Developer Guide.  Default: @dev@  Constraints:     * Must contain 1 to 64 alphanumeric characters.     * Must contain only lowercase letters.     * Cannot be a word that is reserved by the service. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
-- * 'ccClusterIdentifier' - A unique identifier for the cluster. You use this identifier to refer to the cluster for any subsequent cluster operations such as deleting or modifying. The identifier also appears in the Amazon Redshift console. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * Alphabetic characters must be lowercase.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.     * Must be unique for all clusters within an AWS account. Example: @myexamplecluster@
--
-- * 'ccNodeType' - The node type to be provisioned for the cluster. For information about node types, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .  Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@
--
-- * 'ccMasterUsername' - The user name associated with the master user account for the cluster that is being created. Constraints:     * Must be 1 - 128 alphanumeric characters. The user name can't be @PUBLIC@ .     * First character must be a letter.     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
-- * 'ccMasterUserPassword' - The password associated with the master user account for the cluster that is being created. Constraints:     * Must be between 8 and 64 characters in length.     * Must contain at least one uppercase letter.     * Must contain at least one lowercase letter.     * Must contain one number.     * Can be any printable ASCII character (ASCII code 33 to 126) except ' (single quote), " (double quote), \, /, @, or space.
createCluster
    :: Text -- ^ 'ccClusterIdentifier'
    -> Text -- ^ 'ccNodeType'
    -> Text -- ^ 'ccMasterUsername'
    -> Text -- ^ 'ccMasterUserPassword'
    -> CreateCluster
createCluster pClusterIdentifier_ pNodeType_ pMasterUsername_ pMasterUserPassword_ =
  CreateCluster'
    { _ccEnhancedVPCRouting = Nothing
    , _ccAdditionalInfo = Nothing
    , _ccPubliclyAccessible = Nothing
    , _ccHSMConfigurationIdentifier = Nothing
    , _ccClusterSecurityGroups = Nothing
    , _ccAutomatedSnapshotRetentionPeriod = Nothing
    , _ccEncrypted = Nothing
    , _ccClusterSubnetGroupName = Nothing
    , _ccHSMClientCertificateIdentifier = Nothing
    , _ccNumberOfNodes = Nothing
    , _ccElasticIP = Nothing
    , _ccPreferredMaintenanceWindow = Nothing
    , _ccKMSKeyId = Nothing
    , _ccAvailabilityZone = Nothing
    , _ccVPCSecurityGroupIds = Nothing
    , _ccIAMRoles = Nothing
    , _ccClusterType = Nothing
    , _ccClusterVersion = Nothing
    , _ccAllowVersionUpgrade = Nothing
    , _ccClusterParameterGroupName = Nothing
    , _ccTags = Nothing
    , _ccPort = Nothing
    , _ccDBName = Nothing
    , _ccClusterIdentifier = pClusterIdentifier_
    , _ccNodeType = pNodeType_
    , _ccMasterUsername = pMasterUsername_
    , _ccMasterUserPassword = pMasterUserPassword_
    }


-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
ccEnhancedVPCRouting :: Lens' CreateCluster (Maybe Bool)
ccEnhancedVPCRouting = lens _ccEnhancedVPCRouting (\ s a -> s{_ccEnhancedVPCRouting = a})

-- | Reserved.
ccAdditionalInfo :: Lens' CreateCluster (Maybe Text)
ccAdditionalInfo = lens _ccAdditionalInfo (\ s a -> s{_ccAdditionalInfo = a})

-- | If @true@ , the cluster can be accessed from a public network.
ccPubliclyAccessible :: Lens' CreateCluster (Maybe Bool)
ccPubliclyAccessible = lens _ccPubliclyAccessible (\ s a -> s{_ccPubliclyAccessible = a})

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
ccHSMConfigurationIdentifier :: Lens' CreateCluster (Maybe Text)
ccHSMConfigurationIdentifier = lens _ccHSMConfigurationIdentifier (\ s a -> s{_ccHSMConfigurationIdentifier = a})

-- | A list of security groups to be associated with this cluster. Default: The default cluster security group for Amazon Redshift.
ccClusterSecurityGroups :: Lens' CreateCluster [Text]
ccClusterSecurityGroups = lens _ccClusterSecurityGroups (\ s a -> s{_ccClusterSecurityGroups = a}) . _Default . _Coerce

-- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .  Default: @1@  Constraints: Must be a value from 0 to 35.
ccAutomatedSnapshotRetentionPeriod :: Lens' CreateCluster (Maybe Int)
ccAutomatedSnapshotRetentionPeriod = lens _ccAutomatedSnapshotRetentionPeriod (\ s a -> s{_ccAutomatedSnapshotRetentionPeriod = a})

-- | If @true@ , the data in the cluster is encrypted at rest.  Default: false
ccEncrypted :: Lens' CreateCluster (Maybe Bool)
ccEncrypted = lens _ccEncrypted (\ s a -> s{_ccEncrypted = a})

-- | The name of a cluster subnet group to be associated with this cluster. If this parameter is not provided the resulting cluster will be deployed outside virtual private cloud (VPC).
ccClusterSubnetGroupName :: Lens' CreateCluster (Maybe Text)
ccClusterSubnetGroupName = lens _ccClusterSubnetGroupName (\ s a -> s{_ccClusterSubnetGroupName = a})

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
ccHSMClientCertificateIdentifier :: Lens' CreateCluster (Maybe Text)
ccHSMClientCertificateIdentifier = lens _ccHSMClientCertificateIdentifier (\ s a -> s{_ccHSMClientCertificateIdentifier = a})

-- | The number of compute nodes in the cluster. This parameter is required when the __ClusterType__ parameter is specified as @multi-node@ .  For information about determining how many nodes you need, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .  If you don't specify this parameter, you get a single-node cluster. When requesting a multi-node cluster, you must specify the number of nodes that you want in the cluster. Default: @1@  Constraints: Value must be at least 1 and no more than 100.
ccNumberOfNodes :: Lens' CreateCluster (Maybe Int)
ccNumberOfNodes = lens _ccNumberOfNodes (\ s a -> s{_ccNumberOfNodes = a})

-- | The Elastic IP (EIP) address for the cluster. Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
ccElasticIP :: Lens' CreateCluster (Maybe Text)
ccElasticIP = lens _ccElasticIP (\ s a -> s{_ccElasticIP = a})

-- | The weekly time range (in UTC) during which automated cluster maintenance can occur. Format: @ddd:hh24:mi-ddd:hh24:mi@  Default: A 30-minute window selected at random from an 8-hour block of time per region, occurring on a random day of the week. For more information about the time blocks for each region, see <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide. Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints: Minimum 30-minute window.
ccPreferredMaintenanceWindow :: Lens' CreateCluster (Maybe Text)
ccPreferredMaintenanceWindow = lens _ccPreferredMaintenanceWindow (\ s a -> s{_ccPreferredMaintenanceWindow = a})

-- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
ccKMSKeyId :: Lens' CreateCluster (Maybe Text)
ccKMSKeyId = lens _ccKMSKeyId (\ s a -> s{_ccKMSKeyId = a})

-- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to provision the cluster. For example, if you have several EC2 instances running in a specific Availability Zone, then you might want the cluster to be provisioned in the same zone in order to decrease network latency. Default: A random, system-chosen Availability Zone in the region that is specified by the endpoint. Example: @us-east-1d@  Constraint: The specified Availability Zone must be in the same region as the current endpoint.
ccAvailabilityZone :: Lens' CreateCluster (Maybe Text)
ccAvailabilityZone = lens _ccAvailabilityZone (\ s a -> s{_ccAvailabilityZone = a})

-- | A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster. Default: The default VPC security group is associated with the cluster.
ccVPCSecurityGroupIds :: Lens' CreateCluster [Text]
ccVPCSecurityGroupIds = lens _ccVPCSecurityGroupIds (\ s a -> s{_ccVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request. A cluster can have up to 10 IAM roles associated with it at any time.
ccIAMRoles :: Lens' CreateCluster [Text]
ccIAMRoles = lens _ccIAMRoles (\ s a -> s{_ccIAMRoles = a}) . _Default . _Coerce

-- | The type of the cluster. When cluster type is specified as     * @single-node@ , the __NumberOfNodes__ parameter is not required.     * @multi-node@ , the __NumberOfNodes__ parameter is required. Valid Values: @multi-node@ | @single-node@  Default: @multi-node@
ccClusterType :: Lens' CreateCluster (Maybe Text)
ccClusterType = lens _ccClusterType (\ s a -> s{_ccClusterType = a})

-- | The version of the Amazon Redshift engine software that you want to deploy on the cluster. The version selected runs on all the nodes in the cluster. Constraints: Only version 1.0 is currently available. Example: @1.0@
ccClusterVersion :: Lens' CreateCluster (Maybe Text)
ccClusterVersion = lens _ccClusterVersion (\ s a -> s{_ccClusterVersion = a})

-- | If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster. When a new major version of the Amazon Redshift engine is released, you can request that the service automatically apply upgrades during the maintenance window to the Amazon Redshift engine that is running on your cluster. Default: @true@
ccAllowVersionUpgrade :: Lens' CreateCluster (Maybe Bool)
ccAllowVersionUpgrade = lens _ccAllowVersionUpgrade (\ s a -> s{_ccAllowVersionUpgrade = a})

-- | The name of the parameter group to be associated with this cluster. Default: The default Amazon Redshift cluster parameter group. For information about the default parameter group, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>  Constraints:     * Must be 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
ccClusterParameterGroupName :: Lens' CreateCluster (Maybe Text)
ccClusterParameterGroupName = lens _ccClusterParameterGroupName (\ s a -> s{_ccClusterParameterGroupName = a})

-- | A list of tag instances.
ccTags :: Lens' CreateCluster [Tag]
ccTags = lens _ccTags (\ s a -> s{_ccTags = a}) . _Default . _Coerce

-- | The port number on which the cluster accepts incoming connections. The cluster is accessible only via the JDBC and ODBC connection strings. Part of the connection string requires the port on which the cluster will listen for incoming connections. Default: @5439@  Valid Values: @1150-65535@
ccPort :: Lens' CreateCluster (Maybe Int)
ccPort = lens _ccPort (\ s a -> s{_ccPort = a})

-- | The name of the first database to be created when the cluster is created. To create additional databases after the cluster is created, connect to the cluster with a SQL client and use SQL commands to create a database. For more information, go to <http://docs.aws.amazon.com/redshift/latest/dg/t_creating_database.html Create a Database> in the Amazon Redshift Database Developer Guide.  Default: @dev@  Constraints:     * Must contain 1 to 64 alphanumeric characters.     * Must contain only lowercase letters.     * Cannot be a word that is reserved by the service. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
ccDBName :: Lens' CreateCluster (Maybe Text)
ccDBName = lens _ccDBName (\ s a -> s{_ccDBName = a})

-- | A unique identifier for the cluster. You use this identifier to refer to the cluster for any subsequent cluster operations such as deleting or modifying. The identifier also appears in the Amazon Redshift console. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * Alphabetic characters must be lowercase.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.     * Must be unique for all clusters within an AWS account. Example: @myexamplecluster@
ccClusterIdentifier :: Lens' CreateCluster Text
ccClusterIdentifier = lens _ccClusterIdentifier (\ s a -> s{_ccClusterIdentifier = a})

-- | The node type to be provisioned for the cluster. For information about node types, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .  Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@
ccNodeType :: Lens' CreateCluster Text
ccNodeType = lens _ccNodeType (\ s a -> s{_ccNodeType = a})

-- | The user name associated with the master user account for the cluster that is being created. Constraints:     * Must be 1 - 128 alphanumeric characters. The user name can't be @PUBLIC@ .     * First character must be a letter.     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
ccMasterUsername :: Lens' CreateCluster Text
ccMasterUsername = lens _ccMasterUsername (\ s a -> s{_ccMasterUsername = a})

-- | The password associated with the master user account for the cluster that is being created. Constraints:     * Must be between 8 and 64 characters in length.     * Must contain at least one uppercase letter.     * Must contain at least one lowercase letter.     * Must contain one number.     * Can be any printable ASCII character (ASCII code 33 to 126) except ' (single quote), " (double quote), \, /, @, or space.
ccMasterUserPassword :: Lens' CreateCluster Text
ccMasterUserPassword = lens _ccMasterUserPassword (\ s a -> s{_ccMasterUserPassword = a})

instance AWSRequest CreateCluster where
        type Rs CreateCluster = CreateClusterResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "CreateClusterResult"
              (\ s h x ->
                 CreateClusterResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance Hashable CreateCluster where

instance NFData CreateCluster where

instance ToHeaders CreateCluster where
        toHeaders = const mempty

instance ToPath CreateCluster where
        toPath = const "/"

instance ToQuery CreateCluster where
        toQuery CreateCluster'{..}
          = mconcat
              ["Action" =: ("CreateCluster" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "EnhancedVpcRouting" =: _ccEnhancedVPCRouting,
               "AdditionalInfo" =: _ccAdditionalInfo,
               "PubliclyAccessible" =: _ccPubliclyAccessible,
               "HsmConfigurationIdentifier" =:
                 _ccHSMConfigurationIdentifier,
               "ClusterSecurityGroups" =:
                 toQuery
                   (toQueryList "ClusterSecurityGroupName" <$>
                      _ccClusterSecurityGroups),
               "AutomatedSnapshotRetentionPeriod" =:
                 _ccAutomatedSnapshotRetentionPeriod,
               "Encrypted" =: _ccEncrypted,
               "ClusterSubnetGroupName" =:
                 _ccClusterSubnetGroupName,
               "HsmClientCertificateIdentifier" =:
                 _ccHSMClientCertificateIdentifier,
               "NumberOfNodes" =: _ccNumberOfNodes,
               "ElasticIp" =: _ccElasticIP,
               "PreferredMaintenanceWindow" =:
                 _ccPreferredMaintenanceWindow,
               "KmsKeyId" =: _ccKMSKeyId,
               "AvailabilityZone" =: _ccAvailabilityZone,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _ccVPCSecurityGroupIds),
               "IamRoles" =:
                 toQuery (toQueryList "IamRoleArn" <$> _ccIAMRoles),
               "ClusterType" =: _ccClusterType,
               "ClusterVersion" =: _ccClusterVersion,
               "AllowVersionUpgrade" =: _ccAllowVersionUpgrade,
               "ClusterParameterGroupName" =:
                 _ccClusterParameterGroupName,
               "Tags" =: toQuery (toQueryList "Tag" <$> _ccTags),
               "Port" =: _ccPort, "DBName" =: _ccDBName,
               "ClusterIdentifier" =: _ccClusterIdentifier,
               "NodeType" =: _ccNodeType,
               "MasterUsername" =: _ccMasterUsername,
               "MasterUserPassword" =: _ccMasterUserPassword]

-- | /See:/ 'createClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { _ccrsCluster        :: !(Maybe Cluster)
  , _ccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsCluster' - Undocumented member.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createClusterResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateClusterResponse
createClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    {_ccrsCluster = Nothing, _ccrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ccrsCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrsCluster = lens _ccrsCluster (\ s a -> s{_ccrsCluster = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateClusterResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateClusterResponse where
