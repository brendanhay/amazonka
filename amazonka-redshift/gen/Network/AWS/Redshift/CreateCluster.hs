{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cluster. To create the cluster in virtual private cloud
-- (VPC), you must provide cluster subnet group name. If you don\'t provide
-- a cluster subnet group name or the cluster security group parameter,
-- Amazon Redshift creates a non-VPC cluster, it associates the default
-- cluster security group with the cluster. For more information about
-- managing clusters, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/ .
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateCluster.html>
module Network.AWS.Redshift.CreateCluster
    (
    -- * Request
      CreateCluster
    -- ** Request constructor
    , createCluster
    -- ** Request lenses
    , ccrqPubliclyAccessible
    , ccrqHSMConfigurationIdentifier
    , ccrqClusterSecurityGroups
    , ccrqAutomatedSnapshotRetentionPeriod
    , ccrqEncrypted
    , ccrqNumberOfNodes
    , ccrqHSMClientCertificateIdentifier
    , ccrqClusterSubnetGroupName
    , ccrqElasticIP
    , ccrqPreferredMaintenanceWindow
    , ccrqAvailabilityZone
    , ccrqKMSKeyId
    , ccrqVPCSecurityGroupIds
    , ccrqClusterType
    , ccrqClusterVersion
    , ccrqAllowVersionUpgrade
    , ccrqClusterParameterGroupName
    , ccrqDBName
    , ccrqTags
    , ccrqPort
    , ccrqClusterIdentifier
    , ccrqNodeType
    , ccrqMasterUsername
    , ccrqMasterUserPassword

    -- * Response
    , CreateClusterResponse
    -- ** Response constructor
    , createClusterResponse
    -- ** Response lenses
    , ccrsCluster
    , ccrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrqPubliclyAccessible'
--
-- * 'ccrqHSMConfigurationIdentifier'
--
-- * 'ccrqClusterSecurityGroups'
--
-- * 'ccrqAutomatedSnapshotRetentionPeriod'
--
-- * 'ccrqEncrypted'
--
-- * 'ccrqNumberOfNodes'
--
-- * 'ccrqHSMClientCertificateIdentifier'
--
-- * 'ccrqClusterSubnetGroupName'
--
-- * 'ccrqElasticIP'
--
-- * 'ccrqPreferredMaintenanceWindow'
--
-- * 'ccrqAvailabilityZone'
--
-- * 'ccrqKMSKeyId'
--
-- * 'ccrqVPCSecurityGroupIds'
--
-- * 'ccrqClusterType'
--
-- * 'ccrqClusterVersion'
--
-- * 'ccrqAllowVersionUpgrade'
--
-- * 'ccrqClusterParameterGroupName'
--
-- * 'ccrqDBName'
--
-- * 'ccrqTags'
--
-- * 'ccrqPort'
--
-- * 'ccrqClusterIdentifier'
--
-- * 'ccrqNodeType'
--
-- * 'ccrqMasterUsername'
--
-- * 'ccrqMasterUserPassword'
data CreateCluster = CreateCluster'
    { _ccrqPubliclyAccessible               :: !(Maybe Bool)
    , _ccrqHSMConfigurationIdentifier       :: !(Maybe Text)
    , _ccrqClusterSecurityGroups            :: !(Maybe [Text])
    , _ccrqAutomatedSnapshotRetentionPeriod :: !(Maybe Int)
    , _ccrqEncrypted                        :: !(Maybe Bool)
    , _ccrqNumberOfNodes                    :: !(Maybe Int)
    , _ccrqHSMClientCertificateIdentifier   :: !(Maybe Text)
    , _ccrqClusterSubnetGroupName           :: !(Maybe Text)
    , _ccrqElasticIP                        :: !(Maybe Text)
    , _ccrqPreferredMaintenanceWindow       :: !(Maybe Text)
    , _ccrqAvailabilityZone                 :: !(Maybe Text)
    , _ccrqKMSKeyId                         :: !(Maybe Text)
    , _ccrqVPCSecurityGroupIds              :: !(Maybe [Text])
    , _ccrqClusterType                      :: !(Maybe Text)
    , _ccrqClusterVersion                   :: !(Maybe Text)
    , _ccrqAllowVersionUpgrade              :: !(Maybe Bool)
    , _ccrqClusterParameterGroupName        :: !(Maybe Text)
    , _ccrqDBName                           :: !(Maybe Text)
    , _ccrqTags                             :: !(Maybe [Tag])
    , _ccrqPort                             :: !(Maybe Int)
    , _ccrqClusterIdentifier                :: !Text
    , _ccrqNodeType                         :: !Text
    , _ccrqMasterUsername                   :: !Text
    , _ccrqMasterUserPassword               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCluster' smart constructor.
createCluster :: Text -> Text -> Text -> Text -> CreateCluster
createCluster pClusterIdentifier pNodeType pMasterUsername pMasterUserPassword =
    CreateCluster'
    { _ccrqPubliclyAccessible = Nothing
    , _ccrqHSMConfigurationIdentifier = Nothing
    , _ccrqClusterSecurityGroups = Nothing
    , _ccrqAutomatedSnapshotRetentionPeriod = Nothing
    , _ccrqEncrypted = Nothing
    , _ccrqNumberOfNodes = Nothing
    , _ccrqHSMClientCertificateIdentifier = Nothing
    , _ccrqClusterSubnetGroupName = Nothing
    , _ccrqElasticIP = Nothing
    , _ccrqPreferredMaintenanceWindow = Nothing
    , _ccrqAvailabilityZone = Nothing
    , _ccrqKMSKeyId = Nothing
    , _ccrqVPCSecurityGroupIds = Nothing
    , _ccrqClusterType = Nothing
    , _ccrqClusterVersion = Nothing
    , _ccrqAllowVersionUpgrade = Nothing
    , _ccrqClusterParameterGroupName = Nothing
    , _ccrqDBName = Nothing
    , _ccrqTags = Nothing
    , _ccrqPort = Nothing
    , _ccrqClusterIdentifier = pClusterIdentifier
    , _ccrqNodeType = pNodeType
    , _ccrqMasterUsername = pMasterUsername
    , _ccrqMasterUserPassword = pMasterUserPassword
    }

-- | If @true@, the cluster can be accessed from a public network.
ccrqPubliclyAccessible :: Lens' CreateCluster (Maybe Bool)
ccrqPubliclyAccessible = lens _ccrqPubliclyAccessible (\ s a -> s{_ccrqPubliclyAccessible = a});

-- | Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
ccrqHSMConfigurationIdentifier :: Lens' CreateCluster (Maybe Text)
ccrqHSMConfigurationIdentifier = lens _ccrqHSMConfigurationIdentifier (\ s a -> s{_ccrqHSMConfigurationIdentifier = a});

-- | A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
ccrqClusterSecurityGroups :: Lens' CreateCluster [Text]
ccrqClusterSecurityGroups = lens _ccrqClusterSecurityGroups (\ s a -> s{_ccrqClusterSecurityGroups = a}) . _Default;

-- | The number of days that automated snapshots are retained. If the value
-- is 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot.
--
-- Default: @1@
--
-- Constraints: Must be a value from 0 to 35.
ccrqAutomatedSnapshotRetentionPeriod :: Lens' CreateCluster (Maybe Int)
ccrqAutomatedSnapshotRetentionPeriod = lens _ccrqAutomatedSnapshotRetentionPeriod (\ s a -> s{_ccrqAutomatedSnapshotRetentionPeriod = a});

-- | If @true@, the data in the cluster is encrypted at rest.
--
-- Default: false
ccrqEncrypted :: Lens' CreateCluster (Maybe Bool)
ccrqEncrypted = lens _ccrqEncrypted (\ s a -> s{_ccrqEncrypted = a});

-- | The number of compute nodes in the cluster. This parameter is required
-- when the __ClusterType__ parameter is specified as @multi-node@.
--
-- For information about determining how many nodes you need, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- If you don\'t specify this parameter, you get a single-node cluster.
-- When requesting a multi-node cluster, you must specify the number of
-- nodes that you want in the cluster.
--
-- Default: @1@
--
-- Constraints: Value must be at least 1 and no more than 100.
ccrqNumberOfNodes :: Lens' CreateCluster (Maybe Int)
ccrqNumberOfNodes = lens _ccrqNumberOfNodes (\ s a -> s{_ccrqNumberOfNodes = a});

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
ccrqHSMClientCertificateIdentifier :: Lens' CreateCluster (Maybe Text)
ccrqHSMClientCertificateIdentifier = lens _ccrqHSMClientCertificateIdentifier (\ s a -> s{_ccrqHSMClientCertificateIdentifier = a});

-- | The name of a cluster subnet group to be associated with this cluster.
--
-- If this parameter is not provided the resulting cluster will be deployed
-- outside virtual private cloud (VPC).
ccrqClusterSubnetGroupName :: Lens' CreateCluster (Maybe Text)
ccrqClusterSubnetGroupName = lens _ccrqClusterSubnetGroupName (\ s a -> s{_ccrqClusterSubnetGroupName = a});

-- | The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and
-- publicly-accessible through an Internet gateway. For more information
-- about provisioning clusters in EC2-VPC, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster>
-- in the Amazon Redshift Cluster Management Guide.
ccrqElasticIP :: Lens' CreateCluster (Maybe Text)
ccrqElasticIP = lens _ccrqElasticIP (\ s a -> s{_ccrqElasticIP = a});

-- | The weekly time range (in UTC) during which automated cluster
-- maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region, occurring on a random day of the week. For more
-- information about the time blocks for each region, see
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows>
-- in Amazon Redshift Cluster Management Guide.
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Minimum 30-minute window.
ccrqPreferredMaintenanceWindow :: Lens' CreateCluster (Maybe Text)
ccrqPreferredMaintenanceWindow = lens _ccrqPreferredMaintenanceWindow (\ s a -> s{_ccrqPreferredMaintenanceWindow = a});

-- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to
-- provision the cluster. For example, if you have several EC2 instances
-- running in a specific Availability Zone, then you might want the cluster
-- to be provisioned in the same zone in order to decrease network latency.
--
-- Default: A random, system-chosen Availability Zone in the region that is
-- specified by the endpoint.
--
-- Example: @us-east-1d@
--
-- Constraint: The specified Availability Zone must be in the same region
-- as the current endpoint.
ccrqAvailabilityZone :: Lens' CreateCluster (Maybe Text)
ccrqAvailabilityZone = lens _ccrqAvailabilityZone (\ s a -> s{_ccrqAvailabilityZone = a});

-- | The AWS Key Management Service (KMS) key ID of the encryption key that
-- you want to use to encrypt data in the cluster.
ccrqKMSKeyId :: Lens' CreateCluster (Maybe Text)
ccrqKMSKeyId = lens _ccrqKMSKeyId (\ s a -> s{_ccrqKMSKeyId = a});

-- | A list of Virtual Private Cloud (VPC) security groups to be associated
-- with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
ccrqVPCSecurityGroupIds :: Lens' CreateCluster [Text]
ccrqVPCSecurityGroupIds = lens _ccrqVPCSecurityGroupIds (\ s a -> s{_ccrqVPCSecurityGroupIds = a}) . _Default;

-- | The type of the cluster. When cluster type is specified as
--
-- -   @single-node@, the __NumberOfNodes__ parameter is not required.
-- -   @multi-node@, the __NumberOfNodes__ parameter is required.
--
-- Valid Values: @multi-node@ | @single-node@
--
-- Default: @multi-node@
ccrqClusterType :: Lens' CreateCluster (Maybe Text)
ccrqClusterType = lens _ccrqClusterType (\ s a -> s{_ccrqClusterType = a});

-- | The version of the Amazon Redshift engine software that you want to
-- deploy on the cluster.
--
-- The version selected runs on all the nodes in the cluster.
--
-- Constraints: Only version 1.0 is currently available.
--
-- Example: @1.0@
ccrqClusterVersion :: Lens' CreateCluster (Maybe Text)
ccrqClusterVersion = lens _ccrqClusterVersion (\ s a -> s{_ccrqClusterVersion = a});

-- | If @true@, major version upgrades can be applied during the maintenance
-- window to the Amazon Redshift engine that is running on the cluster.
--
-- When a new major version of the Amazon Redshift engine is released, you
-- can request that the service automatically apply upgrades during the
-- maintenance window to the Amazon Redshift engine that is running on your
-- cluster.
--
-- Default: @true@
ccrqAllowVersionUpgrade :: Lens' CreateCluster (Maybe Bool)
ccrqAllowVersionUpgrade = lens _ccrqAllowVersionUpgrade (\ s a -> s{_ccrqAllowVersionUpgrade = a});

-- | The name of the parameter group to be associated with this cluster.
--
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
ccrqClusterParameterGroupName :: Lens' CreateCluster (Maybe Text)
ccrqClusterParameterGroupName = lens _ccrqClusterParameterGroupName (\ s a -> s{_ccrqClusterParameterGroupName = a});

-- | The name of the first database to be created when the cluster is
-- created.
--
-- To create additional databases after the cluster is created, connect to
-- the cluster with a SQL client and use SQL commands to create a database.
-- For more information, go to
-- <http://docs.aws.amazon.com/redshift/latest/dg/t_creating_database.html Create a Database>
-- in the Amazon Redshift Database Developer Guide.
--
-- Default: @dev@
--
-- Constraints:
--
-- -   Must contain 1 to 64 alphanumeric characters.
-- -   Must contain only lowercase letters.
-- -   Cannot be a word that is reserved by the service. A list of reserved
--     words can be found in
--     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
ccrqDBName :: Lens' CreateCluster (Maybe Text)
ccrqDBName = lens _ccrqDBName (\ s a -> s{_ccrqDBName = a});

-- | A list of tag instances.
ccrqTags :: Lens' CreateCluster [Tag]
ccrqTags = lens _ccrqTags (\ s a -> s{_ccrqTags = a}) . _Default;

-- | The port number on which the cluster accepts incoming connections.
--
-- The cluster is accessible only via the JDBC and ODBC connection strings.
-- Part of the connection string requires the port on which the cluster
-- will listen for incoming connections.
--
-- Default: @5439@
--
-- Valid Values: @1150-65535@
ccrqPort :: Lens' CreateCluster (Maybe Int)
ccrqPort = lens _ccrqPort (\ s a -> s{_ccrqPort = a});

-- | A unique identifier for the cluster. You use this identifier to refer to
-- the cluster for any subsequent cluster operations such as deleting or
-- modifying. The identifier also appears in the Amazon Redshift console.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   Alphabetic characters must be lowercase.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
-- -   Must be unique for all clusters within an AWS account.
--
-- Example: @myexamplecluster@
ccrqClusterIdentifier :: Lens' CreateCluster Text
ccrqClusterIdentifier = lens _ccrqClusterIdentifier (\ s a -> s{_ccrqClusterIdentifier = a});

-- | The node type to be provisioned for the cluster. For information about
-- node types, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- Valid Values: @ds1.xlarge@ | @ds1.8xlarge@ | @ds2.xlarge@ |
-- @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@.
ccrqNodeType :: Lens' CreateCluster Text
ccrqNodeType = lens _ccrqNodeType (\ s a -> s{_ccrqNodeType = a});

-- | The user name associated with the master user account for the cluster
-- that is being created.
--
-- Constraints:
--
-- -   Must be 1 - 128 alphanumeric characters.
-- -   First character must be a letter.
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
ccrqMasterUsername :: Lens' CreateCluster Text
ccrqMasterUsername = lens _ccrqMasterUsername (\ s a -> s{_ccrqMasterUsername = a});

-- | The password associated with the master user account for the cluster
-- that is being created.
--
-- Constraints:
--
-- -   Must be between 8 and 64 characters in length.
-- -   Must contain at least one uppercase letter.
-- -   Must contain at least one lowercase letter.
-- -   Must contain one number.
-- -   Can be any printable ASCII character (ASCII code 33 to 126) except
--     \' (single quote), \" (double quote), \\, \/, \@, or space.
ccrqMasterUserPassword :: Lens' CreateCluster Text
ccrqMasterUserPassword = lens _ccrqMasterUserPassword (\ s a -> s{_ccrqMasterUserPassword = a});

instance AWSRequest CreateCluster where
        type Sv CreateCluster = Redshift
        type Rs CreateCluster = CreateClusterResponse
        request = post
        response
          = receiveXMLWrapper "CreateClusterResult"
              (\ s h x ->
                 CreateClusterResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance ToHeaders CreateCluster where
        toHeaders = const mempty

instance ToPath CreateCluster where
        toPath = const "/"

instance ToQuery CreateCluster where
        toQuery CreateCluster'{..}
          = mconcat
              ["Action" =: ("CreateCluster" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "PubliclyAccessible" =: _ccrqPubliclyAccessible,
               "HsmConfigurationIdentifier" =:
                 _ccrqHSMConfigurationIdentifier,
               "ClusterSecurityGroups" =:
                 toQuery
                   (toQueryList "ClusterSecurityGroupName" <$>
                      _ccrqClusterSecurityGroups),
               "AutomatedSnapshotRetentionPeriod" =:
                 _ccrqAutomatedSnapshotRetentionPeriod,
               "Encrypted" =: _ccrqEncrypted,
               "NumberOfNodes" =: _ccrqNumberOfNodes,
               "HsmClientCertificateIdentifier" =:
                 _ccrqHSMClientCertificateIdentifier,
               "ClusterSubnetGroupName" =:
                 _ccrqClusterSubnetGroupName,
               "ElasticIp" =: _ccrqElasticIP,
               "PreferredMaintenanceWindow" =:
                 _ccrqPreferredMaintenanceWindow,
               "AvailabilityZone" =: _ccrqAvailabilityZone,
               "KmsKeyId" =: _ccrqKMSKeyId,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _ccrqVPCSecurityGroupIds),
               "ClusterType" =: _ccrqClusterType,
               "ClusterVersion" =: _ccrqClusterVersion,
               "AllowVersionUpgrade" =: _ccrqAllowVersionUpgrade,
               "ClusterParameterGroupName" =:
                 _ccrqClusterParameterGroupName,
               "DBName" =: _ccrqDBName,
               "Tags" =: toQuery (toQueryList "Tag" <$> _ccrqTags),
               "Port" =: _ccrqPort,
               "ClusterIdentifier" =: _ccrqClusterIdentifier,
               "NodeType" =: _ccrqNodeType,
               "MasterUsername" =: _ccrqMasterUsername,
               "MasterUserPassword" =: _ccrqMasterUserPassword]

-- | /See:/ 'createClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrsCluster'
--
-- * 'ccrsStatus'
data CreateClusterResponse = CreateClusterResponse'
    { _ccrsCluster :: !(Maybe Cluster)
    , _ccrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterResponse' smart constructor.
createClusterResponse :: Int -> CreateClusterResponse
createClusterResponse pStatus =
    CreateClusterResponse'
    { _ccrsCluster = Nothing
    , _ccrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
ccrsCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrsCluster = lens _ccrsCluster (\ s a -> s{_ccrsCluster = a});

-- | FIXME: Undocumented member.
ccrsStatus :: Lens' CreateClusterResponse Int
ccrsStatus = lens _ccrsStatus (\ s a -> s{_ccrsStatus = a});
