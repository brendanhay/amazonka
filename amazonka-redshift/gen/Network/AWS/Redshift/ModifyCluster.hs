{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_ModifyCluster.html>
module Network.AWS.Redshift.ModifyCluster
    (
    -- * Request
      ModifyCluster
    -- ** Request constructor
    , modifyCluster
    -- ** Request lenses
    , mcAllowVersionUpgrade
    , mcAutomatedSnapshotRetentionPeriod
    , mcClusterIdentifier
    , mcClusterParameterGroupName
    , mcClusterSecurityGroups
    , mcClusterType
    , mcClusterVersion
    , mcHsmClientCertificateIdentifier
    , mcHsmConfigurationIdentifier
    , mcMasterUserPassword
    , mcNewClusterIdentifier
    , mcNodeType
    , mcNumberOfNodes
    , mcPreferredMaintenanceWindow
    , mcVpcSecurityGroupIds

    -- * Response
    , ModifyClusterResponse
    -- ** Response constructor
    , modifyClusterResponse
    -- ** Response lenses
    , mcrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data ModifyCluster = ModifyCluster
    { _mcAllowVersionUpgrade              :: Maybe Bool
    , _mcAutomatedSnapshotRetentionPeriod :: Maybe Int
    , _mcClusterIdentifier                :: Text
    , _mcClusterParameterGroupName        :: Maybe Text
    , _mcClusterSecurityGroups            :: [Text]
    , _mcClusterType                      :: Maybe Text
    , _mcClusterVersion                   :: Maybe Text
    , _mcHsmClientCertificateIdentifier   :: Maybe Text
    , _mcHsmConfigurationIdentifier       :: Maybe Text
    , _mcMasterUserPassword               :: Maybe Text
    , _mcNewClusterIdentifier             :: Maybe Text
    , _mcNodeType                         :: Maybe Text
    , _mcNumberOfNodes                    :: Maybe Int
    , _mcPreferredMaintenanceWindow       :: Maybe Text
    , _mcVpcSecurityGroupIds              :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ModifyCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcAllowVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'mcAutomatedSnapshotRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'mcClusterIdentifier' @::@ 'Text'
--
-- * 'mcClusterParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mcClusterSecurityGroups' @::@ ['Text']
--
-- * 'mcClusterType' @::@ 'Maybe' 'Text'
--
-- * 'mcClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'mcHsmClientCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'mcHsmConfigurationIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'mcMasterUserPassword' @::@ 'Maybe' 'Text'
--
-- * 'mcNewClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'mcNodeType' @::@ 'Maybe' 'Text'
--
-- * 'mcNumberOfNodes' @::@ 'Maybe' 'Int'
--
-- * 'mcPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'mcVpcSecurityGroupIds' @::@ ['Text']
--
modifyCluster :: Text -- ^ 'mcClusterIdentifier'
              -> ModifyCluster
modifyCluster p1 = ModifyCluster
    { _mcClusterIdentifier                = p1
    , _mcClusterType                      = Nothing
    , _mcNodeType                         = Nothing
    , _mcNumberOfNodes                    = Nothing
    , _mcClusterSecurityGroups            = mempty
    , _mcVpcSecurityGroupIds              = mempty
    , _mcMasterUserPassword               = Nothing
    , _mcClusterParameterGroupName        = Nothing
    , _mcAutomatedSnapshotRetentionPeriod = Nothing
    , _mcPreferredMaintenanceWindow       = Nothing
    , _mcClusterVersion                   = Nothing
    , _mcAllowVersionUpgrade              = Nothing
    , _mcHsmClientCertificateIdentifier   = Nothing
    , _mcHsmConfigurationIdentifier       = Nothing
    , _mcNewClusterIdentifier             = Nothing
    }

-- | If true, major version upgrades will be applied automatically to the
-- cluster during the maintenance window. Default: false.
mcAllowVersionUpgrade :: Lens' ModifyCluster (Maybe Bool)
mcAllowVersionUpgrade =
    lens _mcAllowVersionUpgrade (\s a -> s { _mcAllowVersionUpgrade = a })

-- | The number of days that automated snapshots are retained. If the value is
-- 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot. If you decrease the automated snapshot retention
-- period from its current value, existing automated snapshots that fall
-- outside of the new retention period will be immediately deleted. Default:
-- Uses existing setting. Constraints: Must be a value from 0 to 35.
mcAutomatedSnapshotRetentionPeriod :: Lens' ModifyCluster (Maybe Int)
mcAutomatedSnapshotRetentionPeriod =
    lens _mcAutomatedSnapshotRetentionPeriod
        (\s a -> s { _mcAutomatedSnapshotRetentionPeriod = a })

-- | The unique identifier of the cluster to be modified. Example:
-- examplecluster.
mcClusterIdentifier :: Lens' ModifyCluster Text
mcClusterIdentifier =
    lens _mcClusterIdentifier (\s a -> s { _mcClusterIdentifier = a })

-- | The name of the cluster parameter group to apply to this cluster. This
-- change is applied only after the cluster is rebooted. To reboot a cluster
-- use RebootCluster. Default: Uses existing setting. Constraints: The
-- cluster parameter group must be in the same parameter group family that
-- matches the cluster version.
mcClusterParameterGroupName :: Lens' ModifyCluster (Maybe Text)
mcClusterParameterGroupName =
    lens _mcClusterParameterGroupName
        (\s a -> s { _mcClusterParameterGroupName = a })

-- | A list of cluster security groups to be authorized on this cluster. This
-- change is asynchronously applied as soon as possible. Security groups
-- currently associated with the cluster, and not in the list of groups to
-- apply, will be revoked from the cluster. Constraints: Must be 1 to 255
-- alphanumeric characters or hyphens First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens.
mcClusterSecurityGroups :: Lens' ModifyCluster [Text]
mcClusterSecurityGroups =
    lens _mcClusterSecurityGroups (\s a -> s { _mcClusterSecurityGroups = a })

-- | The new cluster type. When you submit your cluster resize request, your
-- existing cluster goes into a read-only mode. After Amazon Redshift
-- provisions a new cluster based on your resize requirements, there will be
-- outage for a period while the old cluster is deleted and your connection
-- is switched to the new cluster. You can use DescribeResize to track the
-- progress of the resize request. Valid Values: multi-node | single-node.
mcClusterType :: Lens' ModifyCluster (Maybe Text)
mcClusterType = lens _mcClusterType (\s a -> s { _mcClusterType = a })

-- | The new version number of the Amazon Redshift engine to upgrade to. For
-- major version upgrades, if a non-default cluster parameter group is
-- currently in use, a new cluster parameter group in the cluster parameter
-- group family for the new version must be specified. The new cluster
-- parameter group can be the default for that cluster parameter group
-- family. For more information about managing parameter groups, go to
-- Amazon Redshift Parameter Groups in the Amazon Redshift Management Guide.
-- Example: 1.0.
mcClusterVersion :: Lens' ModifyCluster (Maybe Text)
mcClusterVersion = lens _mcClusterVersion (\s a -> s { _mcClusterVersion = a })

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
mcHsmClientCertificateIdentifier :: Lens' ModifyCluster (Maybe Text)
mcHsmClientCertificateIdentifier =
    lens _mcHsmClientCertificateIdentifier
        (\s a -> s { _mcHsmClientCertificateIdentifier = a })

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
mcHsmConfigurationIdentifier :: Lens' ModifyCluster (Maybe Text)
mcHsmConfigurationIdentifier =
    lens _mcHsmConfigurationIdentifier
        (\s a -> s { _mcHsmConfigurationIdentifier = a })

-- | The new password for the cluster master user. This change is
-- asynchronously applied as soon as possible. Between the time of the
-- request and the completion of the request, the MasterUserPassword element
-- exists in the PendingModifiedValues element of the operation response.
-- Default: Uses existing setting. Constraints: Must be between 8 and 64
-- characters in length. Must contain at least one uppercase letter. Must
-- contain at least one lowercase letter. Must contain one number. Can be
-- any printable ASCII character (ASCII code 33 to 126) except ' (single
-- quote), " (double quote), \, /, @, or space.
mcMasterUserPassword :: Lens' ModifyCluster (Maybe Text)
mcMasterUserPassword =
    lens _mcMasterUserPassword (\s a -> s { _mcMasterUserPassword = a })

-- | The new identifier for the cluster. Constraints: Must contain from 1 to
-- 63 alphanumeric characters or hyphens. Alphabetic characters must be
-- lowercase. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens. Must be unique for all clusters within
-- an AWS account. Example: examplecluster.
mcNewClusterIdentifier :: Lens' ModifyCluster (Maybe Text)
mcNewClusterIdentifier =
    lens _mcNewClusterIdentifier (\s a -> s { _mcNewClusterIdentifier = a })

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
mcNodeType :: Lens' ModifyCluster (Maybe Text)
mcNodeType = lens _mcNodeType (\s a -> s { _mcNodeType = a })

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
mcNumberOfNodes :: Lens' ModifyCluster (Maybe Int)
mcNumberOfNodes = lens _mcNumberOfNodes (\s a -> s { _mcNumberOfNodes = a })

-- | The weekly time range (in UTC) during which system maintenance can occur,
-- if necessary. If system maintenance is necessary during the window, it
-- may result in an outage. This maintenance window change is made
-- immediately. If the new maintenance window indicates the current time,
-- there must be at least 120 minutes between the current time and end of
-- the window in order to ensure that pending changes are applied. Default:
-- Uses existing setting. Format: ddd:hh24:mi-ddd:hh24:mi, for example
-- wed:07:30-wed:08:00. Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes.
mcPreferredMaintenanceWindow :: Lens' ModifyCluster (Maybe Text)
mcPreferredMaintenanceWindow =
    lens _mcPreferredMaintenanceWindow
        (\s a -> s { _mcPreferredMaintenanceWindow = a })

-- | A list of virtual private cloud (VPC) security groups to be associated
-- with the cluster.
mcVpcSecurityGroupIds :: Lens' ModifyCluster [Text]
mcVpcSecurityGroupIds =
    lens _mcVpcSecurityGroupIds (\s a -> s { _mcVpcSecurityGroupIds = a })

newtype ModifyClusterResponse = ModifyClusterResponse
    { _mcrCluster :: Maybe Cluster
    } deriving (Eq, Show, Generic)

-- | 'ModifyClusterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcrCluster' @::@ 'Maybe' 'Cluster'
--
modifyClusterResponse :: ModifyClusterResponse
modifyClusterResponse = ModifyClusterResponse
    { _mcrCluster = Nothing
    }

mcrCluster :: Lens' ModifyClusterResponse (Maybe Cluster)
mcrCluster = lens _mcrCluster (\s a -> s { _mcrCluster = a })

instance AWSRequest ModifyCluster where
    type Sv ModifyCluster = Redshift
    type Rs ModifyCluster = ModifyClusterResponse

    request  = post "ModifyCluster"
    response = xmlResponse

instance FromXML ModifyClusterResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyClusterResponse"

instance ToPath ModifyCluster where
    toPath = const "/"

instance ToHeaders ModifyCluster

instance ToQuery ModifyCluster
