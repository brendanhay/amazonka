{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- Guide .
module Network.AWS.Redshift.CreateCluster
    (
    -- * Request
      CreateClusterMessage
    -- ** Request constructor
    , createClusterMessage
    -- ** Request lenses
    , ccmAllowVersionUpgrade
    , ccmAutomatedSnapshotRetentionPeriod
    , ccmAvailabilityZone
    , ccmClusterIdentifier
    , ccmClusterParameterGroupName
    , ccmClusterSecurityGroups
    , ccmClusterSubnetGroupName
    , ccmClusterType
    , ccmClusterVersion
    , ccmDBName
    , ccmElasticIp
    , ccmEncrypted
    , ccmHsmClientCertificateIdentifier
    , ccmHsmConfigurationIdentifier
    , ccmMasterUserPassword
    , ccmMasterUsername
    , ccmNodeType
    , ccmNumberOfNodes
    , ccmPort
    , ccmPreferredMaintenanceWindow
    , ccmPubliclyAccessible
    , ccmVpcSecurityGroupIds

    -- * Response
    , CreateClusterResult
    -- ** Response constructor
    , createClusterResult
    -- ** Response lenses
    , ccrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data CreateClusterMessage = CreateClusterMessage
    { _ccmAllowVersionUpgrade              :: Maybe Bool
    , _ccmAutomatedSnapshotRetentionPeriod :: Maybe Int
    , _ccmAvailabilityZone                 :: Maybe Text
    , _ccmClusterIdentifier                :: Text
    , _ccmClusterParameterGroupName        :: Maybe Text
    , _ccmClusterSecurityGroups            :: [Text]
    , _ccmClusterSubnetGroupName           :: Maybe Text
    , _ccmClusterType                      :: Maybe Text
    , _ccmClusterVersion                   :: Maybe Text
    , _ccmDBName                           :: Maybe Text
    , _ccmElasticIp                        :: Maybe Text
    , _ccmEncrypted                        :: Maybe Bool
    , _ccmHsmClientCertificateIdentifier   :: Maybe Text
    , _ccmHsmConfigurationIdentifier       :: Maybe Text
    , _ccmMasterUserPassword               :: Text
    , _ccmMasterUsername                   :: Text
    , _ccmNodeType                         :: Text
    , _ccmNumberOfNodes                    :: Maybe Int
    , _ccmPort                             :: Maybe Int
    , _ccmPreferredMaintenanceWindow       :: Maybe Text
    , _ccmPubliclyAccessible               :: Maybe Bool
    , _ccmVpcSecurityGroupIds              :: [Text]
    } (Eq, Ord, Show, Generic)

-- | 'CreateClusterMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccmAllowVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'ccmAutomatedSnapshotRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'ccmAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'ccmClusterIdentifier' @::@ 'Text'
--
-- * 'ccmClusterParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ccmClusterSecurityGroups' @::@ ['Text']
--
-- * 'ccmClusterSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ccmClusterType' @::@ 'Maybe' 'Text'
--
-- * 'ccmClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'ccmDBName' @::@ 'Maybe' 'Text'
--
-- * 'ccmElasticIp' @::@ 'Maybe' 'Text'
--
-- * 'ccmEncrypted' @::@ 'Maybe' 'Bool'
--
-- * 'ccmHsmClientCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ccmHsmConfigurationIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ccmMasterUserPassword' @::@ 'Text'
--
-- * 'ccmMasterUsername' @::@ 'Text'
--
-- * 'ccmNodeType' @::@ 'Text'
--
-- * 'ccmNumberOfNodes' @::@ 'Maybe' 'Int'
--
-- * 'ccmPort' @::@ 'Maybe' 'Int'
--
-- * 'ccmPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'ccmPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'ccmVpcSecurityGroupIds' @::@ ['Text']
--
createClusterMessage :: Text -- ^ 'ccmClusterIdentifier'
                     -> Text -- ^ 'ccmNodeType'
                     -> Text -- ^ 'ccmMasterUsername'
                     -> Text -- ^ 'ccmMasterUserPassword'
                     -> CreateClusterMessage
createClusterMessage p1 p2 p3 p4 = CreateClusterMessage
    { _ccmClusterIdentifier                = p1
    , _ccmNodeType                         = p2
    , _ccmMasterUsername                   = p3
    , _ccmMasterUserPassword               = p4
    , _ccmDBName                           = Nothing
    , _ccmClusterType                      = Nothing
    , _ccmClusterSecurityGroups            = mempty
    , _ccmVpcSecurityGroupIds              = mempty
    , _ccmClusterSubnetGroupName           = Nothing
    , _ccmAvailabilityZone                 = Nothing
    , _ccmPreferredMaintenanceWindow       = Nothing
    , _ccmClusterParameterGroupName        = Nothing
    , _ccmAutomatedSnapshotRetentionPeriod = Nothing
    , _ccmPort                             = Nothing
    , _ccmClusterVersion                   = Nothing
    , _ccmAllowVersionUpgrade              = Nothing
    , _ccmNumberOfNodes                    = Nothing
    , _ccmPubliclyAccessible               = Nothing
    , _ccmEncrypted                        = Nothing
    , _ccmHsmClientCertificateIdentifier   = Nothing
    , _ccmHsmConfigurationIdentifier       = Nothing
    , _ccmElasticIp                        = Nothing
    }

-- | If true, major version upgrades can be applied during the maintenance
-- window to the Amazon Redshift engine that is running on the cluster. When
-- a new major version of the Amazon Redshift engine is released, you can
-- request that the service automatically apply upgrades during the
-- maintenance window to the Amazon Redshift engine that is running on your
-- cluster. Default: true.
ccmAllowVersionUpgrade :: Lens' CreateClusterMessage (Maybe Bool)
ccmAllowVersionUpgrade =
    lens _ccmAllowVersionUpgrade (\s a -> s { _ccmAllowVersionUpgrade = a })

-- | The number of days that automated snapshots are retained. If the value is
-- 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot. Default: 1 Constraints: Must be a value from 0 to
-- 35.
ccmAutomatedSnapshotRetentionPeriod :: Lens' CreateClusterMessage (Maybe Int)
ccmAutomatedSnapshotRetentionPeriod =
    lens _ccmAutomatedSnapshotRetentionPeriod
        (\s a -> s { _ccmAutomatedSnapshotRetentionPeriod = a })

-- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to
-- provision the cluster. For example, if you have several EC2 instances
-- running in a specific Availability Zone, then you might want the cluster
-- to be provisioned in the same zone in order to decrease network latency.
-- Default: A random, system-chosen Availability Zone in the region that is
-- specified by the endpoint. Example: us-east-1d Constraint: The specified
-- Availability Zone must be in the same region as the current endpoint.
ccmAvailabilityZone :: Lens' CreateClusterMessage (Maybe Text)
ccmAvailabilityZone =
    lens _ccmAvailabilityZone (\s a -> s { _ccmAvailabilityZone = a })

-- | A unique identifier for the cluster. You use this identifier to refer to
-- the cluster for any subsequent cluster operations such as deleting or
-- modifying. The identifier also appears in the Amazon Redshift console.
-- Constraints: Must contain from 1 to 63 alphanumeric characters or
-- hyphens. Alphabetic characters must be lowercase. First character must be
-- a letter. Cannot end with a hyphen or contain two consecutive hyphens.
-- Must be unique for all clusters within an AWS account. Example:
-- myexamplecluster.
ccmClusterIdentifier :: Lens' CreateClusterMessage Text
ccmClusterIdentifier =
    lens _ccmClusterIdentifier (\s a -> s { _ccmClusterIdentifier = a })

-- | The name of the parameter group to be associated with this cluster.
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to Working with Amazon
-- Redshift Parameter Groups Constraints: Must be 1 to 255 alphanumeric
-- characters or hyphens. First character must be a letter. Cannot end with
-- a hyphen or contain two consecutive hyphens.
ccmClusterParameterGroupName :: Lens' CreateClusterMessage (Maybe Text)
ccmClusterParameterGroupName =
    lens _ccmClusterParameterGroupName
        (\s a -> s { _ccmClusterParameterGroupName = a })

-- | A list of security groups to be associated with this cluster. Default:
-- The default cluster security group for Amazon Redshift.
ccmClusterSecurityGroups :: Lens' CreateClusterMessage [Text]
ccmClusterSecurityGroups =
    lens _ccmClusterSecurityGroups
        (\s a -> s { _ccmClusterSecurityGroups = a })

-- | The name of a cluster subnet group to be associated with this cluster. If
-- this parameter is not provided the resulting cluster will be deployed
-- outside virtual private cloud (VPC).
ccmClusterSubnetGroupName :: Lens' CreateClusterMessage (Maybe Text)
ccmClusterSubnetGroupName =
    lens _ccmClusterSubnetGroupName
        (\s a -> s { _ccmClusterSubnetGroupName = a })

-- | The type of the cluster. When cluster type is specified as single-node,
-- the NumberOfNodes parameter is not required. multi-node, the
-- NumberOfNodes parameter is required. Valid Values: multi-node |
-- single-node Default: multi-node.
ccmClusterType :: Lens' CreateClusterMessage (Maybe Text)
ccmClusterType = lens _ccmClusterType (\s a -> s { _ccmClusterType = a })

-- | The version of the Amazon Redshift engine software that you want to
-- deploy on the cluster. The version selected runs on all the nodes in the
-- cluster. Constraints: Only version 1.0 is currently available. Example:
-- 1.0.
ccmClusterVersion :: Lens' CreateClusterMessage (Maybe Text)
ccmClusterVersion =
    lens _ccmClusterVersion (\s a -> s { _ccmClusterVersion = a })

-- | The name of the first database to be created when the cluster is created.
-- To create additional databases after the cluster is created, connect to
-- the cluster with a SQL client and use SQL commands to create a database.
-- For more information, go to Create a Database in the Amazon Redshift
-- Database Developer Guide. Default: dev Constraints: Must contain 1 to 64
-- alphanumeric characters. Must contain only lowercase letters. Cannot be a
-- word that is reserved by the service. A list of reserved words can be
-- found in Reserved Words in the Amazon Redshift Database Developer Guide.
ccmDBName :: Lens' CreateClusterMessage (Maybe Text)
ccmDBName = lens _ccmDBName (\s a -> s { _ccmDBName = a })

-- | The Elastic IP (EIP) address for the cluster. Constraints: The cluster
-- must be provisioned in EC2-VPC and publicly-accessible through an
-- Internet gateway. For more information about provisioning clusters in
-- EC2-VPC, go to Supported Platforms to Launch Your Cluster in the Amazon
-- Redshift Management Guide.
ccmElasticIp :: Lens' CreateClusterMessage (Maybe Text)
ccmElasticIp = lens _ccmElasticIp (\s a -> s { _ccmElasticIp = a })

-- | If true, the data in the cluster is encrypted at rest. Default: false.
ccmEncrypted :: Lens' CreateClusterMessage (Maybe Bool)
ccmEncrypted = lens _ccmEncrypted (\s a -> s { _ccmEncrypted = a })

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
ccmHsmClientCertificateIdentifier :: Lens' CreateClusterMessage (Maybe Text)
ccmHsmClientCertificateIdentifier =
    lens _ccmHsmClientCertificateIdentifier
        (\s a -> s { _ccmHsmClientCertificateIdentifier = a })

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
ccmHsmConfigurationIdentifier :: Lens' CreateClusterMessage (Maybe Text)
ccmHsmConfigurationIdentifier =
    lens _ccmHsmConfigurationIdentifier
        (\s a -> s { _ccmHsmConfigurationIdentifier = a })

-- | The password associated with the master user account for the cluster that
-- is being created. Constraints: Must be between 8 and 64 characters in
-- length. Must contain at least one uppercase letter. Must contain at least
-- one lowercase letter. Must contain one number. Can be any printable ASCII
-- character (ASCII code 33 to 126) except ' (single quote), " (double
-- quote), \, /, @, or space.
ccmMasterUserPassword :: Lens' CreateClusterMessage Text
ccmMasterUserPassword =
    lens _ccmMasterUserPassword (\s a -> s { _ccmMasterUserPassword = a })

-- | The user name associated with the master user account for the cluster
-- that is being created. Constraints: Must be 1 - 128 alphanumeric
-- characters. First character must be a letter. Cannot be a reserved word.
-- A list of reserved words can be found in Reserved Words in the Amazon
-- Redshift Database Developer Guide.
ccmMasterUsername :: Lens' CreateClusterMessage Text
ccmMasterUsername =
    lens _ccmMasterUsername (\s a -> s { _ccmMasterUsername = a })

-- | The node type to be provisioned for the cluster. For information about
-- node types, go to Working with Clusters in the Amazon Redshift Management
-- Guide. Valid Values: dw1.xlarge | dw1.8xlarge | dw2.large | dw2.8xlarge.
ccmNodeType :: Lens' CreateClusterMessage Text
ccmNodeType = lens _ccmNodeType (\s a -> s { _ccmNodeType = a })

-- | The number of compute nodes in the cluster. This parameter is required
-- when the ClusterType parameter is specified as multi-node. For
-- information about determining how many nodes you need, go to Working with
-- Clusters in the Amazon Redshift Management Guide. If you don't specify
-- this parameter, you get a single-node cluster. When requesting a
-- multi-node cluster, you must specify the number of nodes that you want in
-- the cluster. Default: 1 Constraints: Value must be at least 1 and no more
-- than 100.
ccmNumberOfNodes :: Lens' CreateClusterMessage (Maybe Int)
ccmNumberOfNodes = lens _ccmNumberOfNodes (\s a -> s { _ccmNumberOfNodes = a })

-- | The port number on which the cluster accepts incoming connections. The
-- cluster is accessible only via the JDBC and ODBC connection strings. Part
-- of the connection string requires the port on which the cluster will
-- listen for incoming connections. Default: 5439 Valid Values: 1150-65535.
ccmPort :: Lens' CreateClusterMessage (Maybe Int)
ccmPort = lens _ccmPort (\s a -> s { _ccmPort = a })

-- | The weekly time range (in UTC) during which automated cluster maintenance
-- can occur. Format: ddd:hh24:mi-ddd:hh24:mi Default: The value selected
-- for the cluster from which the snapshot was taken. For more information
-- about the time blocks for each region, see Maintenance Windows in Amazon
-- Redshift Management Guide. Valid Days: Mon | Tue | Wed | Thu | Fri | Sat
-- | Sun Constraints: Minimum 30-minute window.
ccmPreferredMaintenanceWindow :: Lens' CreateClusterMessage (Maybe Text)
ccmPreferredMaintenanceWindow =
    lens _ccmPreferredMaintenanceWindow
        (\s a -> s { _ccmPreferredMaintenanceWindow = a })

-- | If true, the cluster can be accessed from a public network.
ccmPubliclyAccessible :: Lens' CreateClusterMessage (Maybe Bool)
ccmPubliclyAccessible =
    lens _ccmPubliclyAccessible (\s a -> s { _ccmPubliclyAccessible = a })

-- | A list of Virtual Private Cloud (VPC) security groups to be associated
-- with the cluster. Default: The default VPC security group is associated
-- with the cluster.
ccmVpcSecurityGroupIds :: Lens' CreateClusterMessage [Text]
ccmVpcSecurityGroupIds =
    lens _ccmVpcSecurityGroupIds (\s a -> s { _ccmVpcSecurityGroupIds = a })
instance ToQuery CreateClusterMessage

instance ToPath CreateClusterMessage where
    toPath = const "/"

newtype CreateClusterResult = CreateClusterResult
    { _ccrCluster :: Maybe Cluster
    } (Eq, Show, Generic)

-- | 'CreateClusterResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrCluster' @::@ 'Maybe' 'Cluster'
--
createClusterResult :: CreateClusterResult
createClusterResult = CreateClusterResult
    { _ccrCluster = Nothing
    }

ccrCluster :: Lens' CreateClusterResult (Maybe Cluster)
ccrCluster = lens _ccrCluster (\s a -> s { _ccrCluster = a })

instance FromXML CreateClusterResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateClusterResult"

instance AWSRequest CreateClusterMessage where
    type Sv CreateClusterMessage = Redshift
    type Rs CreateClusterMessage = CreateClusterResult

    request  = post "CreateCluster"
    response = xmlResponse $ \h x -> CreateClusterResult
        <$> x %| "Cluster"
