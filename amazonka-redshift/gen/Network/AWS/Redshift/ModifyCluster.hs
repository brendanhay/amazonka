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

-- Module      : Network.AWS.Redshift.ModifyCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the settings for a cluster. For example, you can add another
-- security or parameter group, update the preferred maintenance window, or
-- change the master user password. Resetting a cluster password or modifying
-- the security groups associated with a cluster do not need a reboot.
-- However, modifying a parameter group requires a reboot for parameters to
-- take effect. For more information about managing clusters, go to Amazon
-- Redshift Clusters in the Amazon Redshift Management Guide . You can also
-- change node type and the number of nodes to scale up or down the cluster.
-- When resizing a cluster, you must specify both the number of nodes and the
-- node type even if one of the parameters does not change.
module Network.AWS.Redshift.ModifyCluster
    (
    -- * Request
      ModifyClusterMessage
    -- ** Request constructor
    , modifyClusterMessage
    -- ** Request lenses
    , mcmAllowVersionUpgrade
    , mcmAutomatedSnapshotRetentionPeriod
    , mcmClusterIdentifier
    , mcmClusterParameterGroupName
    , mcmClusterSecurityGroups
    , mcmClusterType
    , mcmClusterVersion
    , mcmHsmClientCertificateIdentifier
    , mcmHsmConfigurationIdentifier
    , mcmMasterUserPassword
    , mcmNewClusterIdentifier
    , mcmNodeType
    , mcmNumberOfNodes
    , mcmPreferredMaintenanceWindow
    , mcmVpcSecurityGroupIds

    -- * Response
    , ModifyClusterResult
    -- ** Response constructor
    , modifyClusterResult
    -- ** Response lenses
    , mcrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data ModifyClusterMessage = ModifyClusterMessage
    { _mcmAllowVersionUpgrade              :: Maybe Bool
    , _mcmAutomatedSnapshotRetentionPeriod :: Maybe Int
    , _mcmClusterIdentifier                :: Text
    , _mcmClusterParameterGroupName        :: Maybe Text
    , _mcmClusterSecurityGroups            :: [Text]
    , _mcmClusterType                      :: Maybe Text
    , _mcmClusterVersion                   :: Maybe Text
    , _mcmHsmClientCertificateIdentifier   :: Maybe Text
    , _mcmHsmConfigurationIdentifier       :: Maybe Text
    , _mcmMasterUserPassword               :: Maybe Text
    , _mcmNewClusterIdentifier             :: Maybe Text
    , _mcmNodeType                         :: Maybe Text
    , _mcmNumberOfNodes                    :: Maybe Int
    , _mcmPreferredMaintenanceWindow       :: Maybe Text
    , _mcmVpcSecurityGroupIds              :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ModifyClusterMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcmAllowVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'mcmAutomatedSnapshotRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'mcmClusterIdentifier' @::@ 'Text'
--
-- * 'mcmClusterParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mcmClusterSecurityGroups' @::@ ['Text']
--
-- * 'mcmClusterType' @::@ 'Maybe' 'Text'
--
-- * 'mcmClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'mcmHsmClientCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'mcmHsmConfigurationIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'mcmMasterUserPassword' @::@ 'Maybe' 'Text'
--
-- * 'mcmNewClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'mcmNodeType' @::@ 'Maybe' 'Text'
--
-- * 'mcmNumberOfNodes' @::@ 'Maybe' 'Int'
--
-- * 'mcmPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'mcmVpcSecurityGroupIds' @::@ ['Text']
--
modifyClusterMessage :: Text -- ^ 'mcmClusterIdentifier'
                     -> ModifyClusterMessage
modifyClusterMessage p1 = ModifyClusterMessage
    { _mcmClusterIdentifier                = p1
    , _mcmClusterType                      = Nothing
    , _mcmNodeType                         = Nothing
    , _mcmNumberOfNodes                    = Nothing
    , _mcmClusterSecurityGroups            = mempty
    , _mcmVpcSecurityGroupIds              = mempty
    , _mcmMasterUserPassword               = Nothing
    , _mcmClusterParameterGroupName        = Nothing
    , _mcmAutomatedSnapshotRetentionPeriod = Nothing
    , _mcmPreferredMaintenanceWindow       = Nothing
    , _mcmClusterVersion                   = Nothing
    , _mcmAllowVersionUpgrade              = Nothing
    , _mcmHsmClientCertificateIdentifier   = Nothing
    , _mcmHsmConfigurationIdentifier       = Nothing
    , _mcmNewClusterIdentifier             = Nothing
    }

-- | If true, major version upgrades will be applied automatically to the
-- cluster during the maintenance window. Default: false.
mcmAllowVersionUpgrade :: Lens' ModifyClusterMessage (Maybe Bool)
mcmAllowVersionUpgrade =
    lens _mcmAllowVersionUpgrade (\s a -> s { _mcmAllowVersionUpgrade = a })

-- | The number of days that automated snapshots are retained. If the value is
-- 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot. If you decrease the automated snapshot retention
-- period from its current value, existing automated snapshots that fall
-- outside of the new retention period will be immediately deleted. Default:
-- Uses existing setting. Constraints: Must be a value from 0 to 35.
mcmAutomatedSnapshotRetentionPeriod :: Lens' ModifyClusterMessage (Maybe Int)
mcmAutomatedSnapshotRetentionPeriod =
    lens _mcmAutomatedSnapshotRetentionPeriod
        (\s a -> s { _mcmAutomatedSnapshotRetentionPeriod = a })

-- | The unique identifier of the cluster to be modified. Example:
-- examplecluster.
mcmClusterIdentifier :: Lens' ModifyClusterMessage Text
mcmClusterIdentifier =
    lens _mcmClusterIdentifier (\s a -> s { _mcmClusterIdentifier = a })

-- | The name of the cluster parameter group to apply to this cluster. This
-- change is applied only after the cluster is rebooted. To reboot a cluster
-- use RebootCluster. Default: Uses existing setting. Constraints: The
-- cluster parameter group must be in the same parameter group family that
-- matches the cluster version.
mcmClusterParameterGroupName :: Lens' ModifyClusterMessage (Maybe Text)
mcmClusterParameterGroupName =
    lens _mcmClusterParameterGroupName
        (\s a -> s { _mcmClusterParameterGroupName = a })

-- | A list of cluster security groups to be authorized on this cluster. This
-- change is asynchronously applied as soon as possible. Security groups
-- currently associated with the cluster, and not in the list of groups to
-- apply, will be revoked from the cluster. Constraints: Must be 1 to 255
-- alphanumeric characters or hyphens First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens.
mcmClusterSecurityGroups :: Lens' ModifyClusterMessage [Text]
mcmClusterSecurityGroups =
    lens _mcmClusterSecurityGroups
        (\s a -> s { _mcmClusterSecurityGroups = a })

-- | The new cluster type. When you submit your cluster resize request, your
-- existing cluster goes into a read-only mode. After Amazon Redshift
-- provisions a new cluster based on your resize requirements, there will be
-- outage for a period while the old cluster is deleted and your connection
-- is switched to the new cluster. You can use DescribeResize to track the
-- progress of the resize request. Valid Values: multi-node | single-node.
mcmClusterType :: Lens' ModifyClusterMessage (Maybe Text)
mcmClusterType = lens _mcmClusterType (\s a -> s { _mcmClusterType = a })

-- | The new version number of the Amazon Redshift engine to upgrade to. For
-- major version upgrades, if a non-default cluster parameter group is
-- currently in use, a new cluster parameter group in the cluster parameter
-- group family for the new version must be specified. The new cluster
-- parameter group can be the default for that cluster parameter group
-- family. For more information about managing parameter groups, go to
-- Amazon Redshift Parameter Groups in the Amazon Redshift Management Guide.
-- Example: 1.0.
mcmClusterVersion :: Lens' ModifyClusterMessage (Maybe Text)
mcmClusterVersion =
    lens _mcmClusterVersion (\s a -> s { _mcmClusterVersion = a })

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
mcmHsmClientCertificateIdentifier :: Lens' ModifyClusterMessage (Maybe Text)
mcmHsmClientCertificateIdentifier =
    lens _mcmHsmClientCertificateIdentifier
        (\s a -> s { _mcmHsmClientCertificateIdentifier = a })

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
mcmHsmConfigurationIdentifier :: Lens' ModifyClusterMessage (Maybe Text)
mcmHsmConfigurationIdentifier =
    lens _mcmHsmConfigurationIdentifier
        (\s a -> s { _mcmHsmConfigurationIdentifier = a })

-- | The new password for the cluster master user. This change is
-- asynchronously applied as soon as possible. Between the time of the
-- request and the completion of the request, the MasterUserPassword element
-- exists in the PendingModifiedValues element of the operation response.
-- Default: Uses existing setting. Constraints: Must be between 8 and 64
-- characters in length. Must contain at least one uppercase letter. Must
-- contain at least one lowercase letter. Must contain one number. Can be
-- any printable ASCII character (ASCII code 33 to 126) except ' (single
-- quote), " (double quote), \, /, @, or space.
mcmMasterUserPassword :: Lens' ModifyClusterMessage (Maybe Text)
mcmMasterUserPassword =
    lens _mcmMasterUserPassword (\s a -> s { _mcmMasterUserPassword = a })

-- | The new identifier for the cluster. Constraints: Must contain from 1 to
-- 63 alphanumeric characters or hyphens. Alphabetic characters must be
-- lowercase. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens. Must be unique for all clusters within
-- an AWS account. Example: examplecluster.
mcmNewClusterIdentifier :: Lens' ModifyClusterMessage (Maybe Text)
mcmNewClusterIdentifier =
    lens _mcmNewClusterIdentifier (\s a -> s { _mcmNewClusterIdentifier = a })

-- | The new node type of the cluster. If you specify a new node type, you
-- must also specify the number of nodes parameter. When you submit your
-- request to resize a cluster, Amazon Redshift sets access permissions for
-- the cluster to read-only. After Amazon Redshift provisions a new cluster
-- according to your resize requirements, there will be a temporary outage
-- while the old cluster is deleted and your connection is switched to the
-- new cluster. When the new connection is complete, the original access
-- permissions for the cluster are restored. You can use DescribeResize to
-- track the progress of the resize request. Valid Values: dw1.xlarge |
-- dw1.8xlarge | dw2.large | dw2.8xlarge.
mcmNodeType :: Lens' ModifyClusterMessage (Maybe Text)
mcmNodeType = lens _mcmNodeType (\s a -> s { _mcmNodeType = a })

-- | The new number of nodes of the cluster. If you specify a new number of
-- nodes, you must also specify the node type parameter. When you submit
-- your request to resize a cluster, Amazon Redshift sets access permissions
-- for the cluster to read-only. After Amazon Redshift provisions a new
-- cluster according to your resize requirements, there will be a temporary
-- outage while the old cluster is deleted and your connection is switched
-- to the new cluster. When the new connection is complete, the original
-- access permissions for the cluster are restored. You can use
-- DescribeResize to track the progress of the resize request. Valid Values:
-- Integer greater than 0.
mcmNumberOfNodes :: Lens' ModifyClusterMessage (Maybe Int)
mcmNumberOfNodes = lens _mcmNumberOfNodes (\s a -> s { _mcmNumberOfNodes = a })

-- | The weekly time range (in UTC) during which system maintenance can occur,
-- if necessary. If system maintenance is necessary during the window, it
-- may result in an outage. This maintenance window change is made
-- immediately. If the new maintenance window indicates the current time,
-- there must be at least 120 minutes between the current time and end of
-- the window in order to ensure that pending changes are applied. Default:
-- Uses existing setting. Format: ddd:hh24:mi-ddd:hh24:mi, for example
-- wed:07:30-wed:08:00. Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes.
mcmPreferredMaintenanceWindow :: Lens' ModifyClusterMessage (Maybe Text)
mcmPreferredMaintenanceWindow =
    lens _mcmPreferredMaintenanceWindow
        (\s a -> s { _mcmPreferredMaintenanceWindow = a })

-- | A list of virtual private cloud (VPC) security groups to be associated
-- with the cluster.
mcmVpcSecurityGroupIds :: Lens' ModifyClusterMessage [Text]
mcmVpcSecurityGroupIds =
    lens _mcmVpcSecurityGroupIds (\s a -> s { _mcmVpcSecurityGroupIds = a })
instance ToQuery ModifyClusterMessage

instance ToPath ModifyClusterMessage where
    toPath = const "/"

newtype ModifyClusterResult = ModifyClusterResult
    { _mcrCluster :: Maybe Cluster
    } deriving (Eq, Show, Generic)

-- | 'ModifyClusterResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcrCluster' @::@ 'Maybe' 'Cluster'
--
modifyClusterResult :: ModifyClusterResult
modifyClusterResult = ModifyClusterResult
    { _mcrCluster = Nothing
    }

mcrCluster :: Lens' ModifyClusterResult (Maybe Cluster)
mcrCluster = lens _mcrCluster (\s a -> s { _mcrCluster = a })
instance FromXML ModifyClusterResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyClusterResult"

instance AWSRequest ModifyClusterMessage where
    type Sv ModifyClusterMessage = Redshift
    type Rs ModifyClusterMessage = ModifyClusterResult

    request  = post "ModifyCluster"
    response = xmlResponse $ \h x -> ModifyClusterResult
        <$> x %| "Cluster"
