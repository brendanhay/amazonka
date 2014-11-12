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
module Network.AWS.Redshift.RestoreFromClusterSnapshot
    (
    -- * Request
      RestoreFromClusterSnapshotMessage
    -- ** Request constructor
    , restoreFromClusterSnapshotMessage
    -- ** Request lenses
    , rfcsmAllowVersionUpgrade
    , rfcsmAutomatedSnapshotRetentionPeriod
    , rfcsmAvailabilityZone
    , rfcsmClusterIdentifier
    , rfcsmClusterParameterGroupName
    , rfcsmClusterSecurityGroups
    , rfcsmClusterSubnetGroupName
    , rfcsmElasticIp
    , rfcsmHsmClientCertificateIdentifier
    , rfcsmHsmConfigurationIdentifier
    , rfcsmOwnerAccount
    , rfcsmPort
    , rfcsmPreferredMaintenanceWindow
    , rfcsmPubliclyAccessible
    , rfcsmSnapshotClusterIdentifier
    , rfcsmSnapshotIdentifier
    , rfcsmVpcSecurityGroupIds

    -- * Response
    , RestoreFromClusterSnapshotResult
    -- ** Response constructor
    , restoreFromClusterSnapshotResult
    -- ** Response lenses
    , rfcsrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data RestoreFromClusterSnapshotMessage = RestoreFromClusterSnapshotMessage
    { _rfcsmAllowVersionUpgrade              :: Maybe Bool
    , _rfcsmAutomatedSnapshotRetentionPeriod :: Maybe Int
    , _rfcsmAvailabilityZone                 :: Maybe Text
    , _rfcsmClusterIdentifier                :: Text
    , _rfcsmClusterParameterGroupName        :: Maybe Text
    , _rfcsmClusterSecurityGroups            :: [Text]
    , _rfcsmClusterSubnetGroupName           :: Maybe Text
    , _rfcsmElasticIp                        :: Maybe Text
    , _rfcsmHsmClientCertificateIdentifier   :: Maybe Text
    , _rfcsmHsmConfigurationIdentifier       :: Maybe Text
    , _rfcsmOwnerAccount                     :: Maybe Text
    , _rfcsmPort                             :: Maybe Int
    , _rfcsmPreferredMaintenanceWindow       :: Maybe Text
    , _rfcsmPubliclyAccessible               :: Maybe Bool
    , _rfcsmSnapshotClusterIdentifier        :: Maybe Text
    , _rfcsmSnapshotIdentifier               :: Text
    , _rfcsmVpcSecurityGroupIds              :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'RestoreFromClusterSnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfcsmAllowVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'rfcsmAutomatedSnapshotRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'rfcsmAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'rfcsmClusterIdentifier' @::@ 'Text'
--
-- * 'rfcsmClusterParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rfcsmClusterSecurityGroups' @::@ ['Text']
--
-- * 'rfcsmClusterSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rfcsmElasticIp' @::@ 'Maybe' 'Text'
--
-- * 'rfcsmHsmClientCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'rfcsmHsmConfigurationIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'rfcsmOwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'rfcsmPort' @::@ 'Maybe' 'Int'
--
-- * 'rfcsmPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'rfcsmPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'rfcsmSnapshotClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'rfcsmSnapshotIdentifier' @::@ 'Text'
--
-- * 'rfcsmVpcSecurityGroupIds' @::@ ['Text']
--
restoreFromClusterSnapshotMessage :: Text -- ^ 'rfcsmClusterIdentifier'
                                  -> Text -- ^ 'rfcsmSnapshotIdentifier'
                                  -> RestoreFromClusterSnapshotMessage
restoreFromClusterSnapshotMessage p1 p2 = RestoreFromClusterSnapshotMessage
    { _rfcsmClusterIdentifier                = p1
    , _rfcsmSnapshotIdentifier               = p2
    , _rfcsmSnapshotClusterIdentifier        = Nothing
    , _rfcsmPort                             = Nothing
    , _rfcsmAvailabilityZone                 = Nothing
    , _rfcsmAllowVersionUpgrade              = Nothing
    , _rfcsmClusterSubnetGroupName           = Nothing
    , _rfcsmPubliclyAccessible               = Nothing
    , _rfcsmOwnerAccount                     = Nothing
    , _rfcsmHsmClientCertificateIdentifier   = Nothing
    , _rfcsmHsmConfigurationIdentifier       = Nothing
    , _rfcsmElasticIp                        = Nothing
    , _rfcsmClusterParameterGroupName        = Nothing
    , _rfcsmClusterSecurityGroups            = mempty
    , _rfcsmVpcSecurityGroupIds              = mempty
    , _rfcsmPreferredMaintenanceWindow       = Nothing
    , _rfcsmAutomatedSnapshotRetentionPeriod = Nothing
    }

-- | If true, major version upgrades can be applied during the maintenance
-- window to the Amazon Redshift engine that is running on the cluster.
-- Default: true.
rfcsmAllowVersionUpgrade :: Lens' RestoreFromClusterSnapshotMessage (Maybe Bool)
rfcsmAllowVersionUpgrade =
    lens _rfcsmAllowVersionUpgrade
        (\s a -> s { _rfcsmAllowVersionUpgrade = a })

-- | The number of days that automated snapshots are retained. If the value is
-- 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot. Default: The value selected for the cluster from
-- which the snapshot was taken. Constraints: Must be a value from 0 to 35.
rfcsmAutomatedSnapshotRetentionPeriod :: Lens' RestoreFromClusterSnapshotMessage (Maybe Int)
rfcsmAutomatedSnapshotRetentionPeriod =
    lens _rfcsmAutomatedSnapshotRetentionPeriod
        (\s a -> s { _rfcsmAutomatedSnapshotRetentionPeriod = a })

-- | The Amazon EC2 Availability Zone in which to restore the cluster.
-- Default: A random, system-chosen Availability Zone. Example: us-east-1a.
rfcsmAvailabilityZone :: Lens' RestoreFromClusterSnapshotMessage (Maybe Text)
rfcsmAvailabilityZone =
    lens _rfcsmAvailabilityZone (\s a -> s { _rfcsmAvailabilityZone = a })

-- | The identifier of the cluster that will be created from restoring the
-- snapshot. Constraints: Must contain from 1 to 63 alphanumeric characters
-- or hyphens. Alphabetic characters must be lowercase. First character must
-- be a letter. Cannot end with a hyphen or contain two consecutive hyphens.
-- Must be unique for all clusters within an AWS account.
rfcsmClusterIdentifier :: Lens' RestoreFromClusterSnapshotMessage Text
rfcsmClusterIdentifier =
    lens _rfcsmClusterIdentifier (\s a -> s { _rfcsmClusterIdentifier = a })

-- | The name of the parameter group to be associated with this cluster.
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to Working with Amazon
-- Redshift Parameter Groups. Constraints: Must be 1 to 255 alphanumeric
-- characters or hyphens. First character must be a letter. Cannot end with
-- a hyphen or contain two consecutive hyphens.
rfcsmClusterParameterGroupName :: Lens' RestoreFromClusterSnapshotMessage (Maybe Text)
rfcsmClusterParameterGroupName =
    lens _rfcsmClusterParameterGroupName
        (\s a -> s { _rfcsmClusterParameterGroupName = a })

-- | A list of security groups to be associated with this cluster. Default:
-- The default cluster security group for Amazon Redshift. Cluster security
-- groups only apply to clusters outside of VPCs.
rfcsmClusterSecurityGroups :: Lens' RestoreFromClusterSnapshotMessage [Text]
rfcsmClusterSecurityGroups =
    lens _rfcsmClusterSecurityGroups
        (\s a -> s { _rfcsmClusterSecurityGroups = a })

-- | The name of the subnet group where you want to cluster restored. A
-- snapshot of cluster in VPC can be restored only in VPC. Therefore, you
-- must provide subnet group name where you want the cluster restored.
rfcsmClusterSubnetGroupName :: Lens' RestoreFromClusterSnapshotMessage (Maybe Text)
rfcsmClusterSubnetGroupName =
    lens _rfcsmClusterSubnetGroupName
        (\s a -> s { _rfcsmClusterSubnetGroupName = a })

-- | The elastic IP (EIP) address for the cluster.
rfcsmElasticIp :: Lens' RestoreFromClusterSnapshotMessage (Maybe Text)
rfcsmElasticIp = lens _rfcsmElasticIp (\s a -> s { _rfcsmElasticIp = a })

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
rfcsmHsmClientCertificateIdentifier :: Lens' RestoreFromClusterSnapshotMessage (Maybe Text)
rfcsmHsmClientCertificateIdentifier =
    lens _rfcsmHsmClientCertificateIdentifier
        (\s a -> s { _rfcsmHsmClientCertificateIdentifier = a })

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
rfcsmHsmConfigurationIdentifier :: Lens' RestoreFromClusterSnapshotMessage (Maybe Text)
rfcsmHsmConfigurationIdentifier =
    lens _rfcsmHsmConfigurationIdentifier
        (\s a -> s { _rfcsmHsmConfigurationIdentifier = a })

-- | The AWS customer account used to create or copy the snapshot. Required if
-- you are restoring a snapshot you do not own, optional if you own the
-- snapshot.
rfcsmOwnerAccount :: Lens' RestoreFromClusterSnapshotMessage (Maybe Text)
rfcsmOwnerAccount =
    lens _rfcsmOwnerAccount (\s a -> s { _rfcsmOwnerAccount = a })

-- | The port number on which the cluster accepts connections. Default: The
-- same port as the original cluster. Constraints: Must be between 1115 and
-- 65535.
rfcsmPort :: Lens' RestoreFromClusterSnapshotMessage (Maybe Int)
rfcsmPort = lens _rfcsmPort (\s a -> s { _rfcsmPort = a })

-- | The weekly time range (in UTC) during which automated cluster maintenance
-- can occur. Format: ddd:hh24:mi-ddd:hh24:mi Default: The value selected
-- for the cluster from which the snapshot was taken. For more information
-- about the time blocks for each region, see Maintenance Windows in Amazon
-- Redshift Management Guide. Valid Days: Mon | Tue | Wed | Thu | Fri | Sat
-- | Sun Constraints: Minimum 30-minute window.
rfcsmPreferredMaintenanceWindow :: Lens' RestoreFromClusterSnapshotMessage (Maybe Text)
rfcsmPreferredMaintenanceWindow =
    lens _rfcsmPreferredMaintenanceWindow
        (\s a -> s { _rfcsmPreferredMaintenanceWindow = a })

-- | If true, the cluster can be accessed from a public network.
rfcsmPubliclyAccessible :: Lens' RestoreFromClusterSnapshotMessage (Maybe Bool)
rfcsmPubliclyAccessible =
    lens _rfcsmPubliclyAccessible (\s a -> s { _rfcsmPubliclyAccessible = a })

-- | The name of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a snapshot
-- resource element that specifies anything other than * for the cluster
-- name.
rfcsmSnapshotClusterIdentifier :: Lens' RestoreFromClusterSnapshotMessage (Maybe Text)
rfcsmSnapshotClusterIdentifier =
    lens _rfcsmSnapshotClusterIdentifier
        (\s a -> s { _rfcsmSnapshotClusterIdentifier = a })

-- | The name of the snapshot from which to create the new cluster. This
-- parameter isn't case sensitive. Example: my-snapshot-id.
rfcsmSnapshotIdentifier :: Lens' RestoreFromClusterSnapshotMessage Text
rfcsmSnapshotIdentifier =
    lens _rfcsmSnapshotIdentifier (\s a -> s { _rfcsmSnapshotIdentifier = a })

-- | A list of Virtual Private Cloud (VPC) security groups to be associated
-- with the cluster. Default: The default VPC security group is associated
-- with the cluster. VPC security groups only apply to clusters in VPCs.
rfcsmVpcSecurityGroupIds :: Lens' RestoreFromClusterSnapshotMessage [Text]
rfcsmVpcSecurityGroupIds =
    lens _rfcsmVpcSecurityGroupIds
        (\s a -> s { _rfcsmVpcSecurityGroupIds = a })

instance ToQuery RestoreFromClusterSnapshotMessage

instance ToPath RestoreFromClusterSnapshotMessage where
    toPath = const "/"

newtype RestoreFromClusterSnapshotResult = RestoreFromClusterSnapshotResult
    { _rfcsrCluster :: Maybe Cluster
    } deriving (Eq, Show, Generic)

-- | 'RestoreFromClusterSnapshotResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfcsrCluster' @::@ 'Maybe' 'Cluster'
--
restoreFromClusterSnapshotResult :: RestoreFromClusterSnapshotResult
restoreFromClusterSnapshotResult = RestoreFromClusterSnapshotResult
    { _rfcsrCluster = Nothing
    }

rfcsrCluster :: Lens' RestoreFromClusterSnapshotResult (Maybe Cluster)
rfcsrCluster = lens _rfcsrCluster (\s a -> s { _rfcsrCluster = a })

instance FromXML RestoreFromClusterSnapshotResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RestoreFromClusterSnapshotResult"

instance AWSRequest RestoreFromClusterSnapshotMessage where
    type Sv RestoreFromClusterSnapshotMessage = Redshift
    type Rs RestoreFromClusterSnapshotMessage = RestoreFromClusterSnapshotResult

    request  = post "RestoreFromClusterSnapshot"
    response = xmlResponse $ \h x -> RestoreFromClusterSnapshotResult
        <$> x %| "Cluster"
