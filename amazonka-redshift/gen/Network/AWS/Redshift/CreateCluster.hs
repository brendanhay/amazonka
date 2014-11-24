{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
-- clusters, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html
-- Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/
-- .
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateCluster.html>
module Network.AWS.Redshift.CreateCluster
    (
    -- * Request
      CreateCluster
    -- ** Request constructor
    , createCluster
    -- ** Request lenses
    , ccAllowVersionUpgrade
    , ccAutomatedSnapshotRetentionPeriod
    , ccAvailabilityZone
    , ccClusterIdentifier
    , ccClusterParameterGroupName
    , ccClusterSecurityGroups
    , ccClusterSubnetGroupName
    , ccClusterType
    , ccClusterVersion
    , ccDBName
    , ccElasticIp
    , ccEncrypted
    , ccHsmClientCertificateIdentifier
    , ccHsmConfigurationIdentifier
    , ccKmsKeyId
    , ccMasterUserPassword
    , ccMasterUsername
    , ccNodeType
    , ccNumberOfNodes
    , ccPort
    , ccPreferredMaintenanceWindow
    , ccPubliclyAccessible
    , ccTags
    , ccVpcSecurityGroupIds

    -- * Response
    , CreateClusterResponse
    -- ** Response constructor
    , createClusterResponse
    -- ** Response lenses
    , ccrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data CreateCluster = CreateCluster
    { _ccAllowVersionUpgrade              :: Maybe Bool
    , _ccAutomatedSnapshotRetentionPeriod :: Maybe Int
    , _ccAvailabilityZone                 :: Maybe Text
    , _ccClusterIdentifier                :: Text
    , _ccClusterParameterGroupName        :: Maybe Text
    , _ccClusterSecurityGroups            :: List "ClusterSecurityGroupName" Text
    , _ccClusterSubnetGroupName           :: Maybe Text
    , _ccClusterType                      :: Maybe Text
    , _ccClusterVersion                   :: Maybe Text
    , _ccDBName                           :: Maybe Text
    , _ccElasticIp                        :: Maybe Text
    , _ccEncrypted                        :: Maybe Bool
    , _ccHsmClientCertificateIdentifier   :: Maybe Text
    , _ccHsmConfigurationIdentifier       :: Maybe Text
    , _ccKmsKeyId                         :: Maybe Text
    , _ccMasterUserPassword               :: Text
    , _ccMasterUsername                   :: Text
    , _ccNodeType                         :: Text
    , _ccNumberOfNodes                    :: Maybe Int
    , _ccPort                             :: Maybe Int
    , _ccPreferredMaintenanceWindow       :: Maybe Text
    , _ccPubliclyAccessible               :: Maybe Bool
    , _ccTags                             :: List "Tag" Tag
    , _ccVpcSecurityGroupIds              :: List "VpcSecurityGroupId" Text
    } deriving (Eq, Show)

-- | 'CreateCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccAllowVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'ccAutomatedSnapshotRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'ccAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'ccClusterIdentifier' @::@ 'Text'
--
-- * 'ccClusterParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ccClusterSecurityGroups' @::@ ['Text']
--
-- * 'ccClusterSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ccClusterType' @::@ 'Maybe' 'Text'
--
-- * 'ccClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'ccDBName' @::@ 'Maybe' 'Text'
--
-- * 'ccElasticIp' @::@ 'Maybe' 'Text'
--
-- * 'ccEncrypted' @::@ 'Maybe' 'Bool'
--
-- * 'ccHsmClientCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ccHsmConfigurationIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ccKmsKeyId' @::@ 'Maybe' 'Text'
--
-- * 'ccMasterUserPassword' @::@ 'Text'
--
-- * 'ccMasterUsername' @::@ 'Text'
--
-- * 'ccNodeType' @::@ 'Text'
--
-- * 'ccNumberOfNodes' @::@ 'Maybe' 'Int'
--
-- * 'ccPort' @::@ 'Maybe' 'Int'
--
-- * 'ccPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'ccPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'ccTags' @::@ ['Tag']
--
-- * 'ccVpcSecurityGroupIds' @::@ ['Text']
--
createCluster :: Text -- ^ 'ccClusterIdentifier'
              -> Text -- ^ 'ccNodeType'
              -> Text -- ^ 'ccMasterUsername'
              -> Text -- ^ 'ccMasterUserPassword'
              -> CreateCluster
createCluster p1 p2 p3 p4 = CreateCluster
    { _ccClusterIdentifier                = p1
    , _ccNodeType                         = p2
    , _ccMasterUsername                   = p3
    , _ccMasterUserPassword               = p4
    , _ccDBName                           = Nothing
    , _ccClusterType                      = Nothing
    , _ccClusterSecurityGroups            = mempty
    , _ccVpcSecurityGroupIds              = mempty
    , _ccClusterSubnetGroupName           = Nothing
    , _ccAvailabilityZone                 = Nothing
    , _ccPreferredMaintenanceWindow       = Nothing
    , _ccClusterParameterGroupName        = Nothing
    , _ccAutomatedSnapshotRetentionPeriod = Nothing
    , _ccPort                             = Nothing
    , _ccClusterVersion                   = Nothing
    , _ccAllowVersionUpgrade              = Nothing
    , _ccNumberOfNodes                    = Nothing
    , _ccPubliclyAccessible               = Nothing
    , _ccEncrypted                        = Nothing
    , _ccHsmClientCertificateIdentifier   = Nothing
    , _ccHsmConfigurationIdentifier       = Nothing
    , _ccElasticIp                        = Nothing
    , _ccTags                             = mempty
    , _ccKmsKeyId                         = Nothing
    }

-- | If true, major version upgrades can be applied during the maintenance
-- window to the Amazon Redshift engine that is running on the cluster. When
-- a new major version of the Amazon Redshift engine is released, you can
-- request that the service automatically apply upgrades during the
-- maintenance window to the Amazon Redshift engine that is running on your
-- cluster. Default: true.
ccAllowVersionUpgrade :: Lens' CreateCluster (Maybe Bool)
ccAllowVersionUpgrade =
    lens _ccAllowVersionUpgrade (\s a -> s { _ccAllowVersionUpgrade = a })

-- | The number of days that automated snapshots are retained. If the value is
-- 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot>. Default: 1 Constraints: Must be a value from 0 to
-- 35.
ccAutomatedSnapshotRetentionPeriod :: Lens' CreateCluster (Maybe Int)
ccAutomatedSnapshotRetentionPeriod =
    lens _ccAutomatedSnapshotRetentionPeriod
        (\s a -> s { _ccAutomatedSnapshotRetentionPeriod = a })

-- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to
-- provision the cluster. For example, if you have several EC2 instances
-- running in a specific Availability Zone, then you might want the cluster
-- to be provisioned in the same zone in order to decrease network latency.
-- Default: A random, system-chosen Availability Zone in the region that is
-- specified by the endpoint. Example: us-east-1d Constraint: The specified
-- Availability Zone must be in the same region as the current endpoint.
ccAvailabilityZone :: Lens' CreateCluster (Maybe Text)
ccAvailabilityZone =
    lens _ccAvailabilityZone (\s a -> s { _ccAvailabilityZone = a })

-- | A unique identifier for the cluster. You use this identifier to refer to
-- the cluster for any subsequent cluster operations such as deleting or
-- modifying. The identifier also appears in the Amazon Redshift console.
-- Constraints: Must contain from 1 to 63 alphanumeric characters or
-- hyphens. Alphabetic characters must be lowercase. First character must be
-- a letter. Cannot end with a hyphen or contain two consecutive hyphens.
-- Must be unique for all clusters within an AWS account. Example:
-- myexamplecluster.
ccClusterIdentifier :: Lens' CreateCluster Text
ccClusterIdentifier =
    lens _ccClusterIdentifier (\s a -> s { _ccClusterIdentifier = a })

-- | The name of the parameter group to be associated with this cluster.
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html
-- Working with Amazon Redshift Parameter Groups> Constraints: Must be 1 to
-- 255 alphanumeric characters or hyphens. First character must be a letter.
-- Cannot end with a hyphen or contain two consecutive hyphens.
ccClusterParameterGroupName :: Lens' CreateCluster (Maybe Text)
ccClusterParameterGroupName =
    lens _ccClusterParameterGroupName
        (\s a -> s { _ccClusterParameterGroupName = a })

-- | A list of security groups to be associated with this cluster. Default:
-- The default cluster security group for Amazon Redshift.
ccClusterSecurityGroups :: Lens' CreateCluster [Text]
ccClusterSecurityGroups =
    lens _ccClusterSecurityGroups (\s a -> s { _ccClusterSecurityGroups = a })
        . _List

-- | The name of a cluster subnet group to be associated with this cluster. If
-- this parameter is not provided the resulting cluster will be deployed
-- outside virtual private cloud (VPC).
ccClusterSubnetGroupName :: Lens' CreateCluster (Maybe Text)
ccClusterSubnetGroupName =
    lens _ccClusterSubnetGroupName
        (\s a -> s { _ccClusterSubnetGroupName = a })

-- | The type of the cluster. When cluster type is specified as single-node,
-- the NumberOfNodes parameter is not required. multi-node, the
-- NumberOfNodes parameter is required. Valid Values: multi-node |
-- single-node Default: multi-node.
ccClusterType :: Lens' CreateCluster (Maybe Text)
ccClusterType = lens _ccClusterType (\s a -> s { _ccClusterType = a })

-- | The version of the Amazon Redshift engine software that you want to
-- deploy on the cluster. The version selected runs on all the nodes in the
-- cluster. Constraints: Only version 1.0 is currently available. Example:
-- 1.0.
ccClusterVersion :: Lens' CreateCluster (Maybe Text)
ccClusterVersion = lens _ccClusterVersion (\s a -> s { _ccClusterVersion = a })

-- | The name of the first database to be created when the cluster is created.
-- To create additional databases after the cluster is created, connect to
-- the cluster with a SQL client and use SQL commands to create a database.
-- For more information, go to
-- <http://docs.aws.amazon.com/redshift/latest/dg/t_creating_database.html
-- Create a Database> in the Amazon Redshift Database Developer Guide.
-- Default: dev Constraints: Must contain 1 to 64 alphanumeric characters.
-- Must contain only lowercase letters. Cannot be a word that is reserved by
-- the service. A list of reserved words can be found in
-- <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html
-- Reserved Words> in the Amazon Redshift Database Developer Guide.
ccDBName :: Lens' CreateCluster (Maybe Text)
ccDBName = lens _ccDBName (\s a -> s { _ccDBName = a })

-- | The Elastic IP (EIP) address for the cluster. Constraints: The cluster
-- must be provisioned in EC2-VPC and publicly-accessible through an
-- Internet gateway. For more information about provisioning clusters in
-- EC2-VPC, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms
-- Supported Platforms to Launch Your Cluster> in the Amazon Redshift
-- Cluster Management Guide.
ccElasticIp :: Lens' CreateCluster (Maybe Text)
ccElasticIp = lens _ccElasticIp (\s a -> s { _ccElasticIp = a })

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

-- | The AWS Key Management Service (KMS) key ID of the encryption key that
-- you want to use to encrypt data in the cluster.
ccKmsKeyId :: Lens' CreateCluster (Maybe Text)
ccKmsKeyId = lens _ccKmsKeyId (\s a -> s { _ccKmsKeyId = a })

-- | The password associated with the master user account for the cluster that
-- is being created. Constraints: Must be between 8 and 64 characters in
-- length. Must contain at least one uppercase letter. Must contain at least
-- one lowercase letter. Must contain one number. Can be any printable ASCII
-- character (ASCII code 33 to 126) except ' (single quote), " (double
-- quote), \, /, @, or space.
ccMasterUserPassword :: Lens' CreateCluster Text
ccMasterUserPassword =
    lens _ccMasterUserPassword (\s a -> s { _ccMasterUserPassword = a })

-- | The user name associated with the master user account for the cluster
-- that is being created. Constraints: Must be 1 - 128 alphanumeric
-- characters. First character must be a letter. Cannot be a reserved word.
-- A list of reserved words can be found in
-- <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html
-- Reserved Words> in the Amazon Redshift Database Developer Guide.
ccMasterUsername :: Lens' CreateCluster Text
ccMasterUsername = lens _ccMasterUsername (\s a -> s { _ccMasterUsername = a })

-- | The node type to be provisioned for the cluster. For information about
-- node types, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes
-- Working with Clusters> in the /Amazon Redshift Cluster Management Guide/.
-- Valid Values: dw1.xlarge | dw1.8xlarge | dw2.large | dw2.8xlarge.
ccNodeType :: Lens' CreateCluster Text
ccNodeType = lens _ccNodeType (\s a -> s { _ccNodeType = a })

-- | The number of compute nodes in the cluster. This parameter is required
-- when the ClusterType parameter is specified as multi-node. For
-- information about determining how many nodes you need, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes
-- Working with Clusters> in the /Amazon Redshift Cluster Management Guide/.
-- If you don't specify this parameter, you get a single-node cluster. When
-- requesting a multi-node cluster, you must specify the number of nodes
-- that you want in the cluster. Default: 1 Constraints: Value must be at
-- least 1 and no more than 100.
ccNumberOfNodes :: Lens' CreateCluster (Maybe Int)
ccNumberOfNodes = lens _ccNumberOfNodes (\s a -> s { _ccNumberOfNodes = a })

-- | The port number on which the cluster accepts incoming connections. The
-- cluster is accessible only via the JDBC and ODBC connection strings. Part
-- of the connection string requires the port on which the cluster will
-- listen for incoming connections. Default: 5439 Valid Values: 1150-65535.
ccPort :: Lens' CreateCluster (Maybe Int)
ccPort = lens _ccPort (\s a -> s { _ccPort = a })

-- | The weekly time range (in UTC) during which automated cluster maintenance
-- can occur. Format: ddd:hh24:mi-ddd:hh24:mi Default: A 30-minute window
-- selected at random from an 8-hour block of time per region, occurring on
-- a random day of the week. For more information about the time blocks for
-- each region, see
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows
-- Maintenance Windows> in Amazon Redshift Cluster Management Guide. Valid
-- Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints: Minimum
-- 30-minute window.
ccPreferredMaintenanceWindow :: Lens' CreateCluster (Maybe Text)
ccPreferredMaintenanceWindow =
    lens _ccPreferredMaintenanceWindow
        (\s a -> s { _ccPreferredMaintenanceWindow = a })

-- | If true, the cluster can be accessed from a public network.
ccPubliclyAccessible :: Lens' CreateCluster (Maybe Bool)
ccPubliclyAccessible =
    lens _ccPubliclyAccessible (\s a -> s { _ccPubliclyAccessible = a })

-- | A list of tag instances.
ccTags :: Lens' CreateCluster [Tag]
ccTags = lens _ccTags (\s a -> s { _ccTags = a }) . _List

-- | A list of Virtual Private Cloud (VPC) security groups to be associated
-- with the cluster. Default: The default VPC security group is associated
-- with the cluster.
ccVpcSecurityGroupIds :: Lens' CreateCluster [Text]
ccVpcSecurityGroupIds =
    lens _ccVpcSecurityGroupIds (\s a -> s { _ccVpcSecurityGroupIds = a })
        . _List

newtype CreateClusterResponse = CreateClusterResponse
    { _ccrCluster :: Maybe Cluster
    } deriving (Eq, Show)

-- | 'CreateClusterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrCluster' @::@ 'Maybe' 'Cluster'
--
createClusterResponse :: CreateClusterResponse
createClusterResponse = CreateClusterResponse
    { _ccrCluster = Nothing
    }

ccrCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrCluster = lens _ccrCluster (\s a -> s { _ccrCluster = a })

instance ToPath CreateCluster where
    toPath = const "/"

instance ToQuery CreateCluster where
    toQuery CreateCluster{..} = mconcat
        [ "AllowVersionUpgrade"              =? _ccAllowVersionUpgrade
        , "AutomatedSnapshotRetentionPeriod" =? _ccAutomatedSnapshotRetentionPeriod
        , "AvailabilityZone"                 =? _ccAvailabilityZone
        , "ClusterIdentifier"                =? _ccClusterIdentifier
        , "ClusterParameterGroupName"        =? _ccClusterParameterGroupName
        , "ClusterSecurityGroups"            =? _ccClusterSecurityGroups
        , "ClusterSubnetGroupName"           =? _ccClusterSubnetGroupName
        , "ClusterType"                      =? _ccClusterType
        , "ClusterVersion"                   =? _ccClusterVersion
        , "DBName"                           =? _ccDBName
        , "ElasticIp"                        =? _ccElasticIp
        , "Encrypted"                        =? _ccEncrypted
        , "HsmClientCertificateIdentifier"   =? _ccHsmClientCertificateIdentifier
        , "HsmConfigurationIdentifier"       =? _ccHsmConfigurationIdentifier
        , "KmsKeyId"                         =? _ccKmsKeyId
        , "MasterUserPassword"               =? _ccMasterUserPassword
        , "MasterUsername"                   =? _ccMasterUsername
        , "NodeType"                         =? _ccNodeType
        , "NumberOfNodes"                    =? _ccNumberOfNodes
        , "Port"                             =? _ccPort
        , "PreferredMaintenanceWindow"       =? _ccPreferredMaintenanceWindow
        , "PubliclyAccessible"               =? _ccPubliclyAccessible
        , "Tags"                             =? _ccTags
        , "VpcSecurityGroupIds"              =? _ccVpcSecurityGroupIds
        ]

instance ToHeaders CreateCluster

instance AWSRequest CreateCluster where
    type Sv CreateCluster = Redshift
    type Rs CreateCluster = CreateClusterResponse

    request  = post "CreateCluster"
    response = xmlResponse

instance FromXML CreateClusterResponse where
    parseXML = withElement "CreateClusterResult" $ \x -> CreateClusterResponse
        <$> x .@? "Cluster"
