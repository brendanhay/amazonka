{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new cluster. To create the cluster in virtual private cloud
-- (VPC), you must provide cluster subnet group name. If you don't provide a
-- cluster subnet group name or the cluster security group parameter, Amazon
-- Redshift creates a non-VPC cluster, it associates the default cluster
-- security group with the cluster. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Management
-- Guide . Create a non-VPC cluster. https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateCluster &ClusterIdentifier=examplecluster
-- &MasterUsername=masteruser &MasterUserPassword=12345678Aa &NumberOfNodes=2
-- &NodeType=dw1.xlarge &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T000028Z
-- &x-amz-signedheaders=content-type;host;x-amz-date **** 1.0 creating 2 1
-- true false dev sun:10:30-sun:11:00 in-sync default.redshift-1.0 active
-- default dw1.xlarge examplecluster true masteruser
-- e69b1294-64ef-11e2-b07c-f7fbdd006c67 Create cluster in virtual private
-- cloud (VPC). This example request specifies a ClusterSubnetGroup in the
-- request. https://redshift.us-east-1.amazonaws.com/ ?Action=CreateCluster
-- &ClusterIdentifier=exampleclusterinvpc &MasterUsername=master
-- &MasterUserPassword=1234abcdA &NodeType=dw1.xlarge &NumberOfNodes=2
-- &ClusterSubnetGroupName=mysubnetgroup1 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T000028Z
-- &x-amz-signedheaders=content-type;host;x-amz-date **** mysubnetgroup1 1.0
-- creating 2 1 false false dev sat:08:30-sat:09:00 in-sync
-- default.redshift-1.0 vpc-796a5913 dw1.xlarge exampleclusterinvpc true
-- master fa337bb4-6a4d-11e2-a12a-cb8076a904bd.
module Network.AWS.Redshift.CreateCluster
    (
    -- * Request
      CreateCluster
    -- ** Request constructor
    , createCluster
    -- ** Request lenses
    , ccDBName
    , ccClusterIdentifier
    , ccClusterType
    , ccNodeType
    , ccMasterUsername
    , ccMasterUserPassword
    , ccClusterSecurityGroupName
    , ccVpcSecurityGroupId
    , ccClusterSubnetGroupName
    , ccAvailabilityZone
    , ccPreferredMaintenanceWindow
    , ccClusterParameterGroupName
    , ccAutomatedSnapshotRetentionPeriod
    , ccPort
    , ccClusterVersion
    , ccAllowVersionUpgrade
    , ccNumberOfNodes
    , ccPubliclyAccessible
    , ccEncrypted
    , ccHsmClientCertificateIdentifier
    , ccHsmConfigurationIdentifier
    , ccElasticIp

    -- * Response
    , CreateClusterResponse
    -- ** Response constructor
    , createClusterResponse
    -- ** Response lenses
    , ccrCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data CreateCluster = CreateCluster
    { _ccDBName :: Maybe Text
    , _ccClusterIdentifier :: Text
    , _ccClusterType :: Maybe Text
    , _ccNodeType :: Text
    , _ccMasterUsername :: Text
    , _ccMasterUserPassword :: Text
    , _ccClusterSecurityGroupName :: [Text]
    , _ccVpcSecurityGroupId :: [Text]
    , _ccClusterSubnetGroupName :: Maybe Text
    , _ccAvailabilityZone :: Maybe Text
    , _ccPreferredMaintenanceWindow :: Maybe Text
    , _ccClusterParameterGroupName :: Maybe Text
    , _ccAutomatedSnapshotRetentionPeriod :: Maybe Integer
    , _ccPort :: Maybe Integer
    , _ccClusterVersion :: Maybe Text
    , _ccAllowVersionUpgrade :: Maybe Bool
    , _ccNumberOfNodes :: Maybe Integer
    , _ccPubliclyAccessible :: Maybe Bool
    , _ccEncrypted :: Maybe Bool
    , _ccHsmClientCertificateIdentifier :: Maybe Text
    , _ccHsmConfigurationIdentifier :: Maybe Text
    , _ccElasticIp :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCluster' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBName ::@ @Maybe Text@
--
-- * @ClusterIdentifier ::@ @Text@
--
-- * @ClusterType ::@ @Maybe Text@
--
-- * @NodeType ::@ @Text@
--
-- * @MasterUsername ::@ @Text@
--
-- * @MasterUserPassword ::@ @Text@
--
-- * @ClusterSecurityGroupName ::@ @[Text]@
--
-- * @VpcSecurityGroupId ::@ @[Text]@
--
-- * @ClusterSubnetGroupName ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @PreferredMaintenanceWindow ::@ @Maybe Text@
--
-- * @ClusterParameterGroupName ::@ @Maybe Text@
--
-- * @AutomatedSnapshotRetentionPeriod ::@ @Maybe Integer@
--
-- * @Port ::@ @Maybe Integer@
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
-- * @HsmClientCertificateIdentifier ::@ @Maybe Text@
--
-- * @HsmConfigurationIdentifier ::@ @Maybe Text@
--
-- * @ElasticIp ::@ @Maybe Text@
--
createCluster :: Text -- ^ 'ccClusterIdentifier'
              -> Text -- ^ 'ccNodeType'
              -> Text -- ^ 'ccMasterUsername'
              -> Text -- ^ 'ccMasterUserPassword'
              -> CreateCluster
createCluster p2 p4 p5 p6 = CreateCluster
    { _ccDBName = Nothing
    , _ccClusterIdentifier = p2
    , _ccClusterType = Nothing
    , _ccNodeType = p4
    , _ccMasterUsername = p5
    , _ccMasterUserPassword = p6
    , _ccClusterSecurityGroupName = mempty
    , _ccVpcSecurityGroupId = mempty
    , _ccClusterSubnetGroupName = Nothing
    , _ccAvailabilityZone = Nothing
    , _ccPreferredMaintenanceWindow = Nothing
    , _ccClusterParameterGroupName = Nothing
    , _ccAutomatedSnapshotRetentionPeriod = Nothing
    , _ccPort = Nothing
    , _ccClusterVersion = Nothing
    , _ccAllowVersionUpgrade = Nothing
    , _ccNumberOfNodes = Nothing
    , _ccPubliclyAccessible = Nothing
    , _ccEncrypted = Nothing
    , _ccHsmClientCertificateIdentifier = Nothing
    , _ccHsmConfigurationIdentifier = Nothing
    , _ccElasticIp = Nothing
    }

-- | The name of the first database to be created when the cluster is created.
-- To create additional databases after the cluster is created, connect to the
-- cluster with a SQL client and use SQL commands to create a database. For
-- more information, go to Create a Database in the Amazon Redshift Database
-- Developer Guide. Default: dev Constraints: Must contain 1 to 64
-- alphanumeric characters. Must contain only lowercase letters. Cannot be a
-- word that is reserved by the service. A list of reserved words can be found
-- in Reserved Words in the Amazon Redshift Database Developer Guide.
ccDBName :: Lens' CreateCluster (Maybe Text)
ccDBName = lens _ccDBName (\s a -> s { _ccDBName = a })

-- | A unique identifier for the cluster. You use this identifier to refer to
-- the cluster for any subsequent cluster operations such as deleting or
-- modifying. The identifier also appears in the Amazon Redshift console.
-- Constraints: Must contain from 1 to 63 alphanumeric characters or hyphens.
-- Alphabetic characters must be lowercase. First character must be a letter.
-- Cannot end with a hyphen or contain two consecutive hyphens. Must be unique
-- for all clusters within an AWS account. Example: myexamplecluster.
ccClusterIdentifier :: Lens' CreateCluster Text
ccClusterIdentifier =
    lens _ccClusterIdentifier (\s a -> s { _ccClusterIdentifier = a })

-- | The type of the cluster. When cluster type is specified as single-node, the
-- NumberOfNodes parameter is not required. multi-node, the NumberOfNodes
-- parameter is required. Valid Values: multi-node | single-node Default:
-- multi-node.
ccClusterType :: Lens' CreateCluster (Maybe Text)
ccClusterType = lens _ccClusterType (\s a -> s { _ccClusterType = a })

-- | The node type to be provisioned for the cluster. For information about node
-- types, go to Working with Clusters in the Amazon Redshift Management Guide.
-- Valid Values: dw1.xlarge | dw1.8xlarge | dw2.large | dw2.8xlarge.
ccNodeType :: Lens' CreateCluster Text
ccNodeType = lens _ccNodeType (\s a -> s { _ccNodeType = a })

-- | The user name associated with the master user account for the cluster that
-- is being created. Constraints: Must be 1 - 128 alphanumeric characters.
-- First character must be a letter. Cannot be a reserved word. A list of
-- reserved words can be found in Reserved Words in the Amazon Redshift
-- Database Developer Guide.
ccMasterUsername :: Lens' CreateCluster Text
ccMasterUsername =
    lens _ccMasterUsername (\s a -> s { _ccMasterUsername = a })

-- | The password associated with the master user account for the cluster that
-- is being created. Constraints: Must be between 8 and 64 characters in
-- length. Must contain at least one uppercase letter. Must contain at least
-- one lowercase letter. Must contain one number. Can be any printable ASCII
-- character (ASCII code 33 to 126) except ' (single quote), " (double quote),
-- \, /, @, or space.
ccMasterUserPassword :: Lens' CreateCluster Text
ccMasterUserPassword =
    lens _ccMasterUserPassword (\s a -> s { _ccMasterUserPassword = a })

-- | A list of security groups to be associated with this cluster. Default: The
-- default cluster security group for Amazon Redshift.
ccClusterSecurityGroupName :: Lens' CreateCluster [Text]
ccClusterSecurityGroupName =
    lens _ccClusterSecurityGroupName
         (\s a -> s { _ccClusterSecurityGroupName = a })

-- | A list of Virtual Private Cloud (VPC) security groups to be associated with
-- the cluster. Default: The default VPC security group is associated with the
-- cluster.
ccVpcSecurityGroupId :: Lens' CreateCluster [Text]
ccVpcSecurityGroupId =
    lens _ccVpcSecurityGroupId (\s a -> s { _ccVpcSecurityGroupId = a })

-- | The name of a cluster subnet group to be associated with this cluster. If
-- this parameter is not provided the resulting cluster will be deployed
-- outside virtual private cloud (VPC).
ccClusterSubnetGroupName :: Lens' CreateCluster (Maybe Text)
ccClusterSubnetGroupName =
    lens _ccClusterSubnetGroupName
         (\s a -> s { _ccClusterSubnetGroupName = a })

-- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to
-- provision the cluster. For example, if you have several EC2 instances
-- running in a specific Availability Zone, then you might want the cluster to
-- be provisioned in the same zone in order to decrease network latency.
-- Default: A random, system-chosen Availability Zone in the region that is
-- specified by the endpoint. Example: us-east-1d Constraint: The specified
-- Availability Zone must be in the same region as the current endpoint.
ccAvailabilityZone :: Lens' CreateCluster (Maybe Text)
ccAvailabilityZone =
    lens _ccAvailabilityZone (\s a -> s { _ccAvailabilityZone = a })

-- | The weekly time range (in UTC) during which automated cluster maintenance
-- can occur. Format: ddd:hh24:mi-ddd:hh24:mi Default: A 30-minute window
-- selected at random from an 8-hour block of time per region, occurring on a
-- random day of the week. The following list shows the time blocks for each
-- region from which the default maintenance windows are assigned. US-East
-- (Northern Virginia) Region: 03:00-11:00 UTC US-West (Oregon) Region
-- 06:00-14:00 UTC EU (Ireland) Region 22:00-06:00 UTC Asia Pacific
-- (Singapore) Region 14:00-22:00 UTC Asia Pacific (Sydney) Region 12:00-20:00
-- UTC Asia Pacific (Tokyo) Region 17:00-03:00 UTC Valid Days: Mon | Tue | Wed
-- | Thu | Fri | Sat | Sun Constraints: Minimum 30-minute window.
ccPreferredMaintenanceWindow :: Lens' CreateCluster (Maybe Text)
ccPreferredMaintenanceWindow =
    lens _ccPreferredMaintenanceWindow
         (\s a -> s { _ccPreferredMaintenanceWindow = a })

-- | The name of the parameter group to be associated with this cluster.
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to Working with Amazon
-- Redshift Parameter Groups Constraints: Must be 1 to 255 alphanumeric
-- characters or hyphens. First character must be a letter. Cannot end with a
-- hyphen or contain two consecutive hyphens.
ccClusterParameterGroupName :: Lens' CreateCluster (Maybe Text)
ccClusterParameterGroupName =
    lens _ccClusterParameterGroupName
         (\s a -> s { _ccClusterParameterGroupName = a })

-- | The number of days that automated snapshots are retained. If the value is
-- 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot. Default: 1 Constraints: Must be a value from 0 to
-- 35.
ccAutomatedSnapshotRetentionPeriod :: Lens' CreateCluster (Maybe Integer)
ccAutomatedSnapshotRetentionPeriod =
    lens _ccAutomatedSnapshotRetentionPeriod
         (\s a -> s { _ccAutomatedSnapshotRetentionPeriod = a })

-- | The port number on which the cluster accepts incoming connections. The
-- cluster is accessible only via the JDBC and ODBC connection strings. Part
-- of the connection string requires the port on which the cluster will listen
-- for incoming connections. Default: 5439 Valid Values: 1150-65535.
ccPort :: Lens' CreateCluster (Maybe Integer)
ccPort = lens _ccPort (\s a -> s { _ccPort = a })

-- | The version of the Amazon Redshift engine software that you want to deploy
-- on the cluster. The version selected runs on all the nodes in the cluster.
-- Constraints: Only version 1.0 is currently available. Example: 1.0.
ccClusterVersion :: Lens' CreateCluster (Maybe Text)
ccClusterVersion =
    lens _ccClusterVersion (\s a -> s { _ccClusterVersion = a })

-- | If true, upgrades can be applied during the maintenance window to the
-- Amazon Redshift engine that is running on the cluster. When a new version
-- of the Amazon Redshift engine is released, you can request that the service
-- automatically apply upgrades during the maintenance window to the Amazon
-- Redshift engine that is running on your cluster. Default: true.
ccAllowVersionUpgrade :: Lens' CreateCluster (Maybe Bool)
ccAllowVersionUpgrade =
    lens _ccAllowVersionUpgrade (\s a -> s { _ccAllowVersionUpgrade = a })

-- | The number of compute nodes in the cluster. This parameter is required when
-- the ClusterType parameter is specified as multi-node. For information about
-- determining how many nodes you need, go to Working with Clusters in the
-- Amazon Redshift Management Guide. If you don't specify this parameter, you
-- get a single-node cluster. When requesting a multi-node cluster, you must
-- specify the number of nodes that you want in the cluster. Default: 1
-- Constraints: Value must be at least 1 and no more than 100.
ccNumberOfNodes :: Lens' CreateCluster (Maybe Integer)
ccNumberOfNodes = lens _ccNumberOfNodes (\s a -> s { _ccNumberOfNodes = a })

-- | If true, the cluster can be accessed from a public network.
ccPubliclyAccessible :: Lens' CreateCluster (Maybe Bool)
ccPubliclyAccessible =
    lens _ccPubliclyAccessible (\s a -> s { _ccPubliclyAccessible = a })

-- | If true, the data in the cluster is encrypted at rest. Default: false.
ccEncrypted :: Lens' CreateCluster (Maybe Bool)
ccEncrypted = lens _ccEncrypted (\s a -> s { _ccEncrypted = a })

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
ccHsmClientCertificateIdentifier :: Lens' CreateCluster (Maybe Text)
ccHsmClientCertificateIdentifier =
    lens _ccHsmClientCertificateIdentifier
         (\s a -> s { _ccHsmClientCertificateIdentifier = a })

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
ccHsmConfigurationIdentifier :: Lens' CreateCluster (Maybe Text)
ccHsmConfigurationIdentifier =
    lens _ccHsmConfigurationIdentifier
         (\s a -> s { _ccHsmConfigurationIdentifier = a })

-- | The Elastic IP (EIP) address for the cluster. Constraints: The cluster must
-- be provisioned in EC2-VPC and publicly-accessible through an Internet
-- gateway. For more information about provisioning clusters in EC2-VPC, go to
-- Supported Platforms to Launch Your Cluster in the Amazon Redshift
-- Management Guide.
ccElasticIp :: Lens' CreateCluster (Maybe Text)
ccElasticIp = lens _ccElasticIp (\s a -> s { _ccElasticIp = a })

instance ToQuery CreateCluster where
    toQuery = genericQuery def

newtype CreateClusterResponse = CreateClusterResponse
    { _ccrCluster :: Maybe Cluster
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateClusterResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Cluster ::@ @Maybe Cluster@
--
createClusterResponse :: CreateClusterResponse
createClusterResponse = CreateClusterResponse
    { _ccrCluster = Nothing
    }

-- | Describes a cluster.
ccrCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrCluster = lens _ccrCluster (\s a -> s { _ccrCluster = a })

instance FromXML CreateClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCluster where
    type Sv CreateCluster = Redshift
    type Rs CreateCluster = CreateClusterResponse

    request = post "CreateCluster"
    response _ = xmlResponse
