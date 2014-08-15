{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.ModifyCluster
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
-- Redshift Clusters in the Amazon Redshift Management Guide You can also
-- change node type and the number of nodes to scale up or down the cluster.
-- When resizing a cluster, you must specify both the number of nodes and the
-- node type even if one of the parameters does not change. If you specify the
-- same number of nodes and node type that are already configured for the
-- cluster, an error is returned. https://redshift.us-east-1.amazonaws.com/
-- ?Action=ModifyCluster &AllowVersionUpgrade=true
-- &AutomatedSnapshotRetentionPeriod=2 &ClusterIdentifier=examplecluster
-- &ClusterParameterGroupName=parametergroup1
-- &PreferredMaintenanceWindow=wed:07:30-wed:08:00 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T022911Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 5439
-- examplecluster.coqoarplqhsn.us-east-1.redshift.amazonaws.com available 2 2
-- true false dev wed:07:30-wed:08:00 applying parametergroup1
-- 2013-01-22T19:23:59.368Z active default us-east-1c dw1.xlarge
-- examplecluster true adminuser acbc43d5-6504-11e2-bea9-49e0ce183f07.
module Network.AWS.Redshift.V2012_12_01.ModifyCluster where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyCluster' request.
modifyCluster :: Text -- ^ '_mcmClusterIdentifier'
              -> ModifyCluster
modifyCluster p1 = ModifyCluster
    { _mcmClusterIdentifier = p1
    , _mcmAllowVersionUpgrade = Nothing
    , _mcmClusterSecurityGroups = mempty
    , _mcmAutomatedSnapshotRetentionPeriod = Nothing
    , _mcmNumberOfNodes = Nothing
    , _mcmMasterUserPassword = Nothing
    , _mcmHsmConfigurationIdentifier = Nothing
    , _mcmHsmClientCertificateIdentifier = Nothing
    , _mcmPreferredMaintenanceWindow = Nothing
    , _mcmClusterType = Nothing
    , _mcmNewClusterIdentifier = Nothing
    , _mcmClusterVersion = Nothing
    , _mcmNodeType = Nothing
    , _mcmClusterParameterGroupName = Nothing
    , _mcmVpcSecurityGroupIds = mempty
    }

data ModifyCluster = ModifyCluster
    { _mcmClusterIdentifier :: Text
      -- ^ The unique identifier of the cluster to be modified. Example:
      -- examplecluster.
    , _mcmAllowVersionUpgrade :: Maybe Bool
      -- ^ If true, upgrades will be applied automatically to the cluster
      -- during the maintenance window. Default: false.
    , _mcmClusterSecurityGroups :: [Text]
      -- ^ A list of cluster security groups to be authorized on this
      -- cluster. This change is asynchronously applied as soon as
      -- possible. Security groups currently associated with the cluster,
      -- and not in the list of groups to apply, will be revoked from the
      -- cluster. Constraints: Must be 1 to 255 alphanumeric characters or
      -- hyphens First character must be a letter Cannot end with a hyphen
      -- or contain two consecutive hyphens.
    , _mcmAutomatedSnapshotRetentionPeriod :: Maybe Integer
      -- ^ The number of days that automated snapshots are retained. If the
      -- value is 0, automated snapshots are disabled. Even if automated
      -- snapshots are disabled, you can still create manual snapshots
      -- when you want with CreateClusterSnapshot. If you decrease the
      -- automated snapshot retention period from its current value,
      -- existing automated snapshots that fall outside of the new
      -- retention period will be immediately deleted. Default: Uses
      -- existing setting. Constraints: Must be a value from 0 to 35.
    , _mcmNumberOfNodes :: Maybe Integer
      -- ^ The new number of nodes of the cluster. If you specify a new
      -- number of nodes, you must also specify the node type parameter
      -- also. When you submit your request to resize a cluster, Amazon
      -- Redshift sets access permissions for the cluster to read-only.
      -- After Amazon Redshift provisions a new cluster according to your
      -- resize requirements, there will be a temporary outage while the
      -- old cluster is deleted and your connection is switched to the new
      -- cluster. When the new connection is complete, the original access
      -- permissions for the cluster are restored. You can use
      -- DescribeResize to track the progress of the resize request. Valid
      -- Values: Integer greater than 0.
    , _mcmMasterUserPassword :: Maybe Text
      -- ^ The new password for the cluster master user. This change is
      -- asynchronously applied as soon as possible. Between the time of
      -- the request and the completion of the request, the
      -- MasterUserPassword element exists in the PendingModifiedValues
      -- element of the operation response. Operations never return the
      -- password, so this operation provides a way to regain access to
      -- the master user account for a cluster if the password is lost.
      -- Default: Uses existing setting. Constraints: Must be between 8
      -- and 64 characters in length. Must contain at least one uppercase
      -- letter. Must contain at least one lowercase letter. Must contain
      -- one number. Can be any printable ASCII character (ASCII code 33
      -- to 126) except ' (single quote), " (double quote), \, /, @, or
      -- space.
    , _mcmHsmConfigurationIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM configuration that contains the
      -- information the Amazon Redshift cluster can use to retrieve and
      -- store keys in an HSM.
    , _mcmHsmClientCertificateIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM client certificate the Amazon
      -- Redshift cluster uses to retrieve the data encryption keys stored
      -- in an HSM.
    , _mcmPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance
      -- can occur, if necessary. If system maintenance is necessary
      -- during the window, it may result in an outage. This maintenance
      -- window change is made immediately. If the new maintenance window
      -- indicates the current time, there must be at least 120 minutes
      -- between the current time and end of the window in order to ensure
      -- that pending changes are applied. Default: Uses existing setting.
      -- Format: ddd:hh24:mi-ddd:hh24:mi, for example wed:07:30-wed:08:00.
      -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints:
      -- Must be at least 30 minutes.
    , _mcmClusterType :: Maybe Text
      -- ^ The new cluster type. When you submit your cluster resize
      -- request, your existing cluster goes into a read-only mode. After
      -- Amazon Redshift provisions a new cluster based on your resize
      -- requirements, there will be outage for a period while the old
      -- cluster is deleted and your connection is switched to the new
      -- cluster. You can use DescribeResize to track the progress of the
      -- resize request. Valid Values: multi-node | single-node.
    , _mcmNewClusterIdentifier :: Maybe Text
      -- ^ The new identifier for the cluster. Constraints: Must contain
      -- from 1 to 63 alphanumeric characters or hyphens. Alphabetic
      -- characters must be lowercase. First character must be a letter.
      -- Cannot end with a hyphen or contain two consecutive hyphens. Must
      -- be unique for all clusters within an AWS account. Example:
      -- examplecluster.
    , _mcmClusterVersion :: Maybe Text
      -- ^ The new version number of the Amazon Redshift engine to upgrade
      -- to. For major version upgrades, if a non-default cluster
      -- parameter group is currently in use, a new cluster parameter
      -- group in the cluster parameter group family for the new version
      -- must be specified. The new cluster parameter group can be the
      -- default for that cluster parameter group family. For more
      -- information about managing parameter groups, go to Amazon
      -- Redshift Parameter Groups in the Amazon Redshift Management
      -- Guide. Example: 1.0.
    , _mcmNodeType :: Maybe Text
      -- ^ The new node type of the cluster. If you specify a new node type,
      -- you must also specify the number of nodes parameter also. When
      -- you submit your request to resize a cluster, Amazon Redshift sets
      -- access permissions for the cluster to read-only. After Amazon
      -- Redshift provisions a new cluster according to your resize
      -- requirements, there will be a temporary outage while the old
      -- cluster is deleted and your connection is switched to the new
      -- cluster. When the new connection is complete, the original access
      -- permissions for the cluster are restored. You can use the
      -- DescribeResize to track the progress of the resize request. Valid
      -- Values: dw1.xlarge | dw1.8xlarge | dw2.large | dw2.8xlarge.
    , _mcmClusterParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group to apply to this cluster.
      -- This change is applied only after the cluster is rebooted. To
      -- reboot a cluster use RebootCluster. Default: Uses existing
      -- setting. Constraints: The cluster parameter group must be in the
      -- same parameter group family that matches the cluster version.
    , _mcmVpcSecurityGroupIds :: [Text]
      -- ^ A list of virtual private cloud (VPC) security groups to be
      -- associated with the cluster.
    } deriving (Show, Generic)

makeLenses ''ModifyCluster

instance ToQuery ModifyCluster where
    toQuery = genericQuery def

data ModifyClusterResponse = ModifyClusterResponse
    { _cctCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Show, Generic)

makeLenses ''ModifyClusterResponse

instance FromXML ModifyClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCluster where
    type Sv ModifyCluster = Redshift
    type Rs ModifyCluster = ModifyClusterResponse

    request = post "ModifyCluster"
    response _ = xmlResponse
