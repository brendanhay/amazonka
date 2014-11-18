{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.RestoreFromClusterSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new cluster from a snapshot. Amazon Redshift creates the
-- resulting cluster with the same configuration as the original cluster from
-- which the snapshot was created, except that the new cluster is created with
-- the default cluster security and parameter group. After Amazon Redshift
-- creates the cluster you can use the ModifyCluster API to associate a
-- different security group and different parameter group with the restored
-- cluster. If you restore a cluster into a VPC, you must provide a cluster
-- subnet group where you want the cluster restored. For more information
-- about working with snapshots, go to Amazon Redshift Snapshots in the Amazon
-- Redshift Management Guide.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_RestoreFromClusterSnapshot.html>
module Network.AWS.Redshift.RestoreFromClusterSnapshot
    (
    -- * Request
      RestoreFromClusterSnapshot
    -- ** Request constructor
    , restoreFromClusterSnapshot
    -- ** Request lenses
    , rfcsAllowVersionUpgrade
    , rfcsAutomatedSnapshotRetentionPeriod
    , rfcsAvailabilityZone
    , rfcsClusterIdentifier
    , rfcsClusterParameterGroupName
    , rfcsClusterSecurityGroups
    , rfcsClusterSubnetGroupName
    , rfcsElasticIp
    , rfcsHsmClientCertificateIdentifier
    , rfcsHsmConfigurationIdentifier
    , rfcsOwnerAccount
    , rfcsPort
    , rfcsPreferredMaintenanceWindow
    , rfcsPubliclyAccessible
    , rfcsSnapshotClusterIdentifier
    , rfcsSnapshotIdentifier
    , rfcsVpcSecurityGroupIds

    -- * Response
    , RestoreFromClusterSnapshotResponse
    -- ** Response constructor
    , restoreFromClusterSnapshotResponse
    -- ** Response lenses
    , rfcsrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data RestoreFromClusterSnapshot = RestoreFromClusterSnapshot
    { _rfcsAllowVersionUpgrade              :: Maybe Bool
    , _rfcsAutomatedSnapshotRetentionPeriod :: Maybe Int
    , _rfcsAvailabilityZone                 :: Maybe Text
    , _rfcsClusterIdentifier                :: Text
    , _rfcsClusterParameterGroupName        :: Maybe Text
    , _rfcsClusterSecurityGroups            :: [Text]
    , _rfcsClusterSubnetGroupName           :: Maybe Text
    , _rfcsElasticIp                        :: Maybe Text
    , _rfcsHsmClientCertificateIdentifier   :: Maybe Text
    , _rfcsHsmConfigurationIdentifier       :: Maybe Text
    , _rfcsOwnerAccount                     :: Maybe Text
    , _rfcsPort                             :: Maybe Int
    , _rfcsPreferredMaintenanceWindow       :: Maybe Text
    , _rfcsPubliclyAccessible               :: Maybe Bool
    , _rfcsSnapshotClusterIdentifier        :: Maybe Text
    , _rfcsSnapshotIdentifier               :: Text
    , _rfcsVpcSecurityGroupIds              :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'RestoreFromClusterSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfcsAllowVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'rfcsAutomatedSnapshotRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'rfcsAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'rfcsClusterIdentifier' @::@ 'Text'
--
-- * 'rfcsClusterParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rfcsClusterSecurityGroups' @::@ ['Text']
--
-- * 'rfcsClusterSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rfcsElasticIp' @::@ 'Maybe' 'Text'
--
-- * 'rfcsHsmClientCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'rfcsHsmConfigurationIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'rfcsOwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'rfcsPort' @::@ 'Maybe' 'Int'
--
-- * 'rfcsPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'rfcsPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'rfcsSnapshotClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'rfcsSnapshotIdentifier' @::@ 'Text'
--
-- * 'rfcsVpcSecurityGroupIds' @::@ ['Text']
--
restoreFromClusterSnapshot :: Text -- ^ 'rfcsClusterIdentifier'
                           -> Text -- ^ 'rfcsSnapshotIdentifier'
                           -> RestoreFromClusterSnapshot
restoreFromClusterSnapshot p1 p2 = RestoreFromClusterSnapshot
    { _rfcsClusterIdentifier                = p1
    , _rfcsSnapshotIdentifier               = p2
    , _rfcsSnapshotClusterIdentifier        = Nothing
    , _rfcsPort                             = Nothing
    , _rfcsAvailabilityZone                 = Nothing
    , _rfcsAllowVersionUpgrade              = Nothing
    , _rfcsClusterSubnetGroupName           = Nothing
    , _rfcsPubliclyAccessible               = Nothing
    , _rfcsOwnerAccount                     = Nothing
    , _rfcsHsmClientCertificateIdentifier   = Nothing
    , _rfcsHsmConfigurationIdentifier       = Nothing
    , _rfcsElasticIp                        = Nothing
    , _rfcsClusterParameterGroupName        = Nothing
    , _rfcsClusterSecurityGroups            = mempty
    , _rfcsVpcSecurityGroupIds              = mempty
    , _rfcsPreferredMaintenanceWindow       = Nothing
    , _rfcsAutomatedSnapshotRetentionPeriod = Nothing
    }

-- | If true, major version upgrades can be applied during the maintenance
-- window to the Amazon Redshift engine that is running on the cluster.
-- Default: true.
rfcsAllowVersionUpgrade :: Lens' RestoreFromClusterSnapshot (Maybe Bool)
rfcsAllowVersionUpgrade =
    lens _rfcsAllowVersionUpgrade (\s a -> s { _rfcsAllowVersionUpgrade = a })

-- | The number of days that automated snapshots are retained. If the value is
-- 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot. Default: The value selected for the cluster from
-- which the snapshot was taken. Constraints: Must be a value from 0 to 35.
rfcsAutomatedSnapshotRetentionPeriod :: Lens' RestoreFromClusterSnapshot (Maybe Int)
rfcsAutomatedSnapshotRetentionPeriod =
    lens _rfcsAutomatedSnapshotRetentionPeriod
        (\s a -> s { _rfcsAutomatedSnapshotRetentionPeriod = a })

-- | The Amazon EC2 Availability Zone in which to restore the cluster.
-- Default: A random, system-chosen Availability Zone. Example: us-east-1a.
rfcsAvailabilityZone :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsAvailabilityZone =
    lens _rfcsAvailabilityZone (\s a -> s { _rfcsAvailabilityZone = a })

-- | The identifier of the cluster that will be created from restoring the
-- snapshot. Constraints: Must contain from 1 to 63 alphanumeric characters
-- or hyphens. Alphabetic characters must be lowercase. First character must
-- be a letter. Cannot end with a hyphen or contain two consecutive hyphens.
-- Must be unique for all clusters within an AWS account.
rfcsClusterIdentifier :: Lens' RestoreFromClusterSnapshot Text
rfcsClusterIdentifier =
    lens _rfcsClusterIdentifier (\s a -> s { _rfcsClusterIdentifier = a })

-- | The name of the parameter group to be associated with this cluster.
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to Working with Amazon
-- Redshift Parameter Groups. Constraints: Must be 1 to 255 alphanumeric
-- characters or hyphens. First character must be a letter. Cannot end with
-- a hyphen or contain two consecutive hyphens.
rfcsClusterParameterGroupName :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsClusterParameterGroupName =
    lens _rfcsClusterParameterGroupName
        (\s a -> s { _rfcsClusterParameterGroupName = a })

-- | A list of security groups to be associated with this cluster. Default:
-- The default cluster security group for Amazon Redshift. Cluster security
-- groups only apply to clusters outside of VPCs.
rfcsClusterSecurityGroups :: Lens' RestoreFromClusterSnapshot [Text]
rfcsClusterSecurityGroups =
    lens _rfcsClusterSecurityGroups
        (\s a -> s { _rfcsClusterSecurityGroups = a })

-- | The name of the subnet group where you want to cluster restored. A
-- snapshot of cluster in VPC can be restored only in VPC. Therefore, you
-- must provide subnet group name where you want the cluster restored.
rfcsClusterSubnetGroupName :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsClusterSubnetGroupName =
    lens _rfcsClusterSubnetGroupName
        (\s a -> s { _rfcsClusterSubnetGroupName = a })

-- | The elastic IP (EIP) address for the cluster.
rfcsElasticIp :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsElasticIp = lens _rfcsElasticIp (\s a -> s { _rfcsElasticIp = a })

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
rfcsHsmClientCertificateIdentifier :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsHsmClientCertificateIdentifier =
    lens _rfcsHsmClientCertificateIdentifier
        (\s a -> s { _rfcsHsmClientCertificateIdentifier = a })

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
rfcsHsmConfigurationIdentifier :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsHsmConfigurationIdentifier =
    lens _rfcsHsmConfigurationIdentifier
        (\s a -> s { _rfcsHsmConfigurationIdentifier = a })

-- | The AWS customer account used to create or copy the snapshot. Required if
-- you are restoring a snapshot you do not own, optional if you own the
-- snapshot.
rfcsOwnerAccount :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsOwnerAccount = lens _rfcsOwnerAccount (\s a -> s { _rfcsOwnerAccount = a })

-- | The port number on which the cluster accepts connections. Default: The
-- same port as the original cluster. Constraints: Must be between 1115 and
-- 65535.
rfcsPort :: Lens' RestoreFromClusterSnapshot (Maybe Int)
rfcsPort = lens _rfcsPort (\s a -> s { _rfcsPort = a })

-- | The weekly time range (in UTC) during which automated cluster maintenance
-- can occur. Format: ddd:hh24:mi-ddd:hh24:mi Default: The value selected
-- for the cluster from which the snapshot was taken. For more information
-- about the time blocks for each region, see Maintenance Windows in Amazon
-- Redshift Management Guide. Valid Days: Mon | Tue | Wed | Thu | Fri | Sat
-- | Sun Constraints: Minimum 30-minute window.
rfcsPreferredMaintenanceWindow :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsPreferredMaintenanceWindow =
    lens _rfcsPreferredMaintenanceWindow
        (\s a -> s { _rfcsPreferredMaintenanceWindow = a })

-- | If true, the cluster can be accessed from a public network.
rfcsPubliclyAccessible :: Lens' RestoreFromClusterSnapshot (Maybe Bool)
rfcsPubliclyAccessible =
    lens _rfcsPubliclyAccessible (\s a -> s { _rfcsPubliclyAccessible = a })

-- | The name of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a snapshot
-- resource element that specifies anything other than * for the cluster
-- name.
rfcsSnapshotClusterIdentifier :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsSnapshotClusterIdentifier =
    lens _rfcsSnapshotClusterIdentifier
        (\s a -> s { _rfcsSnapshotClusterIdentifier = a })

-- | The name of the snapshot from which to create the new cluster. This
-- parameter isn't case sensitive. Example: my-snapshot-id.
rfcsSnapshotIdentifier :: Lens' RestoreFromClusterSnapshot Text
rfcsSnapshotIdentifier =
    lens _rfcsSnapshotIdentifier (\s a -> s { _rfcsSnapshotIdentifier = a })

-- | A list of Virtual Private Cloud (VPC) security groups to be associated
-- with the cluster. Default: The default VPC security group is associated
-- with the cluster. VPC security groups only apply to clusters in VPCs.
rfcsVpcSecurityGroupIds :: Lens' RestoreFromClusterSnapshot [Text]
rfcsVpcSecurityGroupIds =
    lens _rfcsVpcSecurityGroupIds (\s a -> s { _rfcsVpcSecurityGroupIds = a })

newtype RestoreFromClusterSnapshotResponse = RestoreFromClusterSnapshotResponse
    { _rfcsrCluster :: Maybe Cluster
    } deriving (Eq, Show, Generic)

-- | 'RestoreFromClusterSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfcsrCluster' @::@ 'Maybe' 'Cluster'
--
restoreFromClusterSnapshotResponse :: RestoreFromClusterSnapshotResponse
restoreFromClusterSnapshotResponse = RestoreFromClusterSnapshotResponse
    { _rfcsrCluster = Nothing
    }

rfcsrCluster :: Lens' RestoreFromClusterSnapshotResponse (Maybe Cluster)
rfcsrCluster = lens _rfcsrCluster (\s a -> s { _rfcsrCluster = a })

instance ToPath RestoreFromClusterSnapshot where
    toPath = const "/"

instance ToQuery RestoreFromClusterSnapshot

instance ToHeaders RestoreFromClusterSnapshot

instance AWSRequest RestoreFromClusterSnapshot where
    type Sv RestoreFromClusterSnapshot = Redshift
    type Rs RestoreFromClusterSnapshot = RestoreFromClusterSnapshotResponse

    request  = post "RestoreFromClusterSnapshot"
    response = xmlResponse

instance FromXML RestoreFromClusterSnapshotResponse where
    parseXML c = RestoreFromClusterSnapshotResponse
        <$> c .:? "Cluster"
