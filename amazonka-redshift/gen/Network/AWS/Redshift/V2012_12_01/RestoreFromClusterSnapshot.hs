{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.RestoreFromClusterSnapshot
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
-- Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=RestoreFromClusterSnapshot
-- &ClusterIdentifier=examplecluster-restored
-- &SnapshotIdentifier=cm:examplecluster-2013-01-22-19-27-58
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T023350Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 creating 2 1 true
-- false dev sun:06:30-sun:07:00 in-sync default.redshift-1.0 active default
-- dw1.xlarge examplecluster-restored true adminuser
-- 52a9aee8-6505-11e2-bec0-17624ad140dd.
module Network.AWS.Redshift.V2012_12_01.RestoreFromClusterSnapshot where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RestoreFromClusterSnapshot' request.
restoreFromClusterSnapshot :: Text -- ^ '_rfcsmClusterIdentifier'
                           -> Text -- ^ '_rfcsmSnapshotIdentifier'
                           -> RestoreFromClusterSnapshot
restoreFromClusterSnapshot p1 p2 = RestoreFromClusterSnapshot
    { _rfcsmClusterIdentifier = p1
    , _rfcsmSnapshotIdentifier = p2
    , _rfcsmAllowVersionUpgrade = Nothing
    , _rfcsmPubliclyAccessible = Nothing
    , _rfcsmClusterSecurityGroups = mempty
    , _rfcsmAutomatedSnapshotRetentionPeriod = Nothing
    , _rfcsmPort = Nothing
    , _rfcsmAvailabilityZone = Nothing
    , _rfcsmClusterParameterGroupName = Nothing
    , _rfcsmClusterSubnetGroupName = Nothing
    , _rfcsmElasticIp = Nothing
    , _rfcsmHsmClientCertificateIdentifier = Nothing
    , _rfcsmHsmConfigurationIdentifier = Nothing
    , _rfcsmOwnerAccount = Nothing
    , _rfcsmPreferredMaintenanceWindow = Nothing
    , _rfcsmSnapshotClusterIdentifier = Nothing
    , _rfcsmVpcSecurityGroupIds = mempty
    }

data RestoreFromClusterSnapshot = RestoreFromClusterSnapshot
    { _rfcsmClusterIdentifier :: Text
      -- ^ The identifier of the cluster that will be created from restoring
      -- the snapshot. Constraints: Must contain from 1 to 63 alphanumeric
      -- characters or hyphens. Alphabetic characters must be lowercase.
      -- First character must be a letter. Cannot end with a hyphen or
      -- contain two consecutive hyphens. Must be unique for all clusters
      -- within an AWS account.
    , _rfcsmSnapshotIdentifier :: Text
      -- ^ The name of the snapshot from which to create the new cluster.
      -- This parameter isn't case sensitive. Example: my-snapshot-id.
    , _rfcsmAllowVersionUpgrade :: Maybe Bool
      -- ^ If true, upgrades can be applied during the maintenance window to
      -- the Amazon Redshift engine that is running on the cluster.
      -- Default: true.
    , _rfcsmPubliclyAccessible :: Maybe Bool
      -- ^ If true, the cluster can be accessed from a public network.
    , _rfcsmClusterSecurityGroups :: [Text]
      -- ^ A list of security groups to be associated with this cluster.
      -- Default: The default cluster security group for Amazon Redshift.
      -- Cluster security groups only apply to clusters outside of VPCs.
    , _rfcsmAutomatedSnapshotRetentionPeriod :: Maybe Integer
      -- ^ The number of days that automated snapshots are retained. If the
      -- value is 0, automated snapshots are disabled. Even if automated
      -- snapshots are disabled, you can still create manual snapshots
      -- when you want with CreateClusterSnapshot. Default: The value
      -- selected for the cluster from which the snapshot was taken.
      -- Constraints: Must be a value from 0 to 35.
    , _rfcsmPort :: Maybe Integer
      -- ^ The port number on which the cluster accepts connections.
      -- Default: The same port as the original cluster. Constraints: Must
      -- be between 1115 and 65535.
    , _rfcsmAvailabilityZone :: Maybe Text
      -- ^ The Amazon EC2 Availability Zone in which to restore the cluster.
      -- Default: A random, system-chosen Availability Zone. Example:
      -- us-east-1a.
    , _rfcsmClusterParameterGroupName :: Maybe Text
      -- ^ The name of the parameter group to be associated with this
      -- cluster. Default: The default Amazon Redshift cluster parameter
      -- group. For information about the default parameter group, go to
      -- Working with Amazon Redshift Parameter Groups. Constraints: Must
      -- be 1 to 255 alphanumeric characters or hyphens. First character
      -- must be a letter. Cannot end with a hyphen or contain two
      -- consecutive hyphens.
    , _rfcsmClusterSubnetGroupName :: Maybe Text
      -- ^ The name of the subnet group where you want to cluster restored.
      -- A snapshot of cluster in VPC can be restored only in VPC.
      -- Therefore, you must provide subnet group name where you want the
      -- cluster restored.
    , _rfcsmElasticIp :: Maybe Text
      -- ^ The elastic IP (EIP) address for the cluster.
    , _rfcsmHsmClientCertificateIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM client certificate the Amazon
      -- Redshift cluster uses to retrieve the data encryption keys stored
      -- in an HSM.
    , _rfcsmHsmConfigurationIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM configuration that contains the
      -- information the Amazon Redshift cluster can use to retrieve and
      -- store keys in an HSM.
    , _rfcsmOwnerAccount :: Maybe Text
      -- ^ The AWS customer account used to create or copy the snapshot.
      -- Required if you are restoring a snapshot you do not own, optional
      -- if you own the snapshot.
    , _rfcsmPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which automated cluster
      -- maintenance can occur. Format: ddd:hh24:mi-ddd:hh24:mi Default:
      -- The value selected for the cluster from which the snapshot was
      -- taken. The following list shows the time blocks for each region
      -- from which the default maintenance windows are assigned. US-East
      -- (Northern Virginia) Region: 03:00-11:00 UTC US-West (Oregon)
      -- Region 06:00-14:00 UTC EU (Ireland) Region 22:00-06:00 UTC Asia
      -- Pacific (Singapore) Region 14:00-22:00 UTC Asia Pacific (Sydney)
      -- Region 12:00-20:00 UTC Asia Pacific (Tokyo) Region 17:00-03:00
      -- UTC Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
      -- Constraints: Minimum 30-minute window.
    , _rfcsmSnapshotClusterIdentifier :: Maybe Text
      -- ^ The name of the cluster the source snapshot was created from.
      -- This parameter is required if your IAM user has a policy
      -- containing a snapshot resource element that specifies anything
      -- other than * for the cluster name.
    , _rfcsmVpcSecurityGroupIds :: [Text]
      -- ^ A list of Virtual Private Cloud (VPC) security groups to be
      -- associated with the cluster. Default: The default VPC security
      -- group is associated with the cluster. VPC security groups only
      -- apply to clusters in VPCs.
    } deriving (Show, Generic)

makeLenses ''RestoreFromClusterSnapshot

instance ToQuery RestoreFromClusterSnapshot where
    toQuery = genericQuery def

data RestoreFromClusterSnapshotResponse = RestoreFromClusterSnapshotResponse
    { _ccxCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Show, Generic)

makeLenses ''RestoreFromClusterSnapshotResponse

instance FromXML RestoreFromClusterSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RestoreFromClusterSnapshot where
    type Sv RestoreFromClusterSnapshot = Redshift
    type Rs RestoreFromClusterSnapshot = RestoreFromClusterSnapshotResponse

    request = post "RestoreFromClusterSnapshot"
    response _ = xmlResponse
