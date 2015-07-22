{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a cluster. For example, you can add another
-- security or parameter group, update the preferred maintenance window, or
-- change the master user password. Resetting a cluster password or
-- modifying the security groups associated with a cluster do not need a
-- reboot. However, modifying a parameter group requires a reboot for
-- parameters to take effect. For more information about managing clusters,
-- go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/ .
--
-- You can also change node type and the number of nodes to scale up or
-- down the cluster. When resizing a cluster, you must specify both the
-- number of nodes and the node type even if one of the parameters does not
-- change.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_ModifyCluster.html>
module Network.AWS.Redshift.ModifyCluster
    (
    -- * Request
      ModifyCluster
    -- ** Request constructor
    , modifyCluster
    -- ** Request lenses
    , mcrqMasterUserPassword
    , mcrqHSMConfigurationIdentifier
    , mcrqClusterSecurityGroups
    , mcrqAutomatedSnapshotRetentionPeriod
    , mcrqNumberOfNodes
    , mcrqHSMClientCertificateIdentifier
    , mcrqPreferredMaintenanceWindow
    , mcrqVPCSecurityGroupIds
    , mcrqClusterType
    , mcrqNewClusterIdentifier
    , mcrqClusterVersion
    , mcrqNodeType
    , mcrqAllowVersionUpgrade
    , mcrqClusterParameterGroupName
    , mcrqClusterIdentifier

    -- * Response
    , ModifyClusterResponse
    -- ** Response constructor
    , modifyClusterResponse
    -- ** Response lenses
    , mcrsCluster
    , mcrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'modifyCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcrqMasterUserPassword'
--
-- * 'mcrqHSMConfigurationIdentifier'
--
-- * 'mcrqClusterSecurityGroups'
--
-- * 'mcrqAutomatedSnapshotRetentionPeriod'
--
-- * 'mcrqNumberOfNodes'
--
-- * 'mcrqHSMClientCertificateIdentifier'
--
-- * 'mcrqPreferredMaintenanceWindow'
--
-- * 'mcrqVPCSecurityGroupIds'
--
-- * 'mcrqClusterType'
--
-- * 'mcrqNewClusterIdentifier'
--
-- * 'mcrqClusterVersion'
--
-- * 'mcrqNodeType'
--
-- * 'mcrqAllowVersionUpgrade'
--
-- * 'mcrqClusterParameterGroupName'
--
-- * 'mcrqClusterIdentifier'
data ModifyCluster = ModifyCluster'
    { _mcrqMasterUserPassword               :: !(Maybe Text)
    , _mcrqHSMConfigurationIdentifier       :: !(Maybe Text)
    , _mcrqClusterSecurityGroups            :: !(Maybe [Text])
    , _mcrqAutomatedSnapshotRetentionPeriod :: !(Maybe Int)
    , _mcrqNumberOfNodes                    :: !(Maybe Int)
    , _mcrqHSMClientCertificateIdentifier   :: !(Maybe Text)
    , _mcrqPreferredMaintenanceWindow       :: !(Maybe Text)
    , _mcrqVPCSecurityGroupIds              :: !(Maybe [Text])
    , _mcrqClusterType                      :: !(Maybe Text)
    , _mcrqNewClusterIdentifier             :: !(Maybe Text)
    , _mcrqClusterVersion                   :: !(Maybe Text)
    , _mcrqNodeType                         :: !(Maybe Text)
    , _mcrqAllowVersionUpgrade              :: !(Maybe Bool)
    , _mcrqClusterParameterGroupName        :: !(Maybe Text)
    , _mcrqClusterIdentifier                :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyCluster' smart constructor.
modifyCluster :: Text -> ModifyCluster
modifyCluster pClusterIdentifier =
    ModifyCluster'
    { _mcrqMasterUserPassword = Nothing
    , _mcrqHSMConfigurationIdentifier = Nothing
    , _mcrqClusterSecurityGroups = Nothing
    , _mcrqAutomatedSnapshotRetentionPeriod = Nothing
    , _mcrqNumberOfNodes = Nothing
    , _mcrqHSMClientCertificateIdentifier = Nothing
    , _mcrqPreferredMaintenanceWindow = Nothing
    , _mcrqVPCSecurityGroupIds = Nothing
    , _mcrqClusterType = Nothing
    , _mcrqNewClusterIdentifier = Nothing
    , _mcrqClusterVersion = Nothing
    , _mcrqNodeType = Nothing
    , _mcrqAllowVersionUpgrade = Nothing
    , _mcrqClusterParameterGroupName = Nothing
    , _mcrqClusterIdentifier = pClusterIdentifier
    }

-- | The new password for the cluster master user. This change is
-- asynchronously applied as soon as possible. Between the time of the
-- request and the completion of the request, the @MasterUserPassword@
-- element exists in the @PendingModifiedValues@ element of the operation
-- response.
--
-- Operations never return the password, so this operation provides a way
-- to regain access to the master user account for a cluster if the
-- password is lost.
--
-- Default: Uses existing setting.
--
-- Constraints:
--
-- -   Must be between 8 and 64 characters in length.
-- -   Must contain at least one uppercase letter.
-- -   Must contain at least one lowercase letter.
-- -   Must contain one number.
-- -   Can be any printable ASCII character (ASCII code 33 to 126) except
--     \' (single quote), \" (double quote), \\, \/, \@, or space.
mcrqMasterUserPassword :: Lens' ModifyCluster (Maybe Text)
mcrqMasterUserPassword = lens _mcrqMasterUserPassword (\ s a -> s{_mcrqMasterUserPassword = a});

-- | Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
mcrqHSMConfigurationIdentifier :: Lens' ModifyCluster (Maybe Text)
mcrqHSMConfigurationIdentifier = lens _mcrqHSMConfigurationIdentifier (\ s a -> s{_mcrqHSMConfigurationIdentifier = a});

-- | A list of cluster security groups to be authorized on this cluster. This
-- change is asynchronously applied as soon as possible.
--
-- Security groups currently associated with the cluster, and not in the
-- list of groups to apply, will be revoked from the cluster.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
mcrqClusterSecurityGroups :: Lens' ModifyCluster [Text]
mcrqClusterSecurityGroups = lens _mcrqClusterSecurityGroups (\ s a -> s{_mcrqClusterSecurityGroups = a}) . _Default;

-- | The number of days that automated snapshots are retained. If the value
-- is 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot.
--
-- If you decrease the automated snapshot retention period from its current
-- value, existing automated snapshots that fall outside of the new
-- retention period will be immediately deleted.
--
-- Default: Uses existing setting.
--
-- Constraints: Must be a value from 0 to 35.
mcrqAutomatedSnapshotRetentionPeriod :: Lens' ModifyCluster (Maybe Int)
mcrqAutomatedSnapshotRetentionPeriod = lens _mcrqAutomatedSnapshotRetentionPeriod (\ s a -> s{_mcrqAutomatedSnapshotRetentionPeriod = a});

-- | The new number of nodes of the cluster. If you specify a new number of
-- nodes, you must also specify the node type parameter.
--
-- When you submit your request to resize a cluster, Amazon Redshift sets
-- access permissions for the cluster to read-only. After Amazon Redshift
-- provisions a new cluster according to your resize requirements, there
-- will be a temporary outage while the old cluster is deleted and your
-- connection is switched to the new cluster. When the new connection is
-- complete, the original access permissions for the cluster are restored.
-- You can use DescribeResize to track the progress of the resize request.
--
-- Valid Values: Integer greater than @0@.
mcrqNumberOfNodes :: Lens' ModifyCluster (Maybe Int)
mcrqNumberOfNodes = lens _mcrqNumberOfNodes (\ s a -> s{_mcrqNumberOfNodes = a});

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
mcrqHSMClientCertificateIdentifier :: Lens' ModifyCluster (Maybe Text)
mcrqHSMClientCertificateIdentifier = lens _mcrqHSMClientCertificateIdentifier (\ s a -> s{_mcrqHSMClientCertificateIdentifier = a});

-- | The weekly time range (in UTC) during which system maintenance can
-- occur, if necessary. If system maintenance is necessary during the
-- window, it may result in an outage.
--
-- This maintenance window change is made immediately. If the new
-- maintenance window indicates the current time, there must be at least
-- 120 minutes between the current time and end of the window in order to
-- ensure that pending changes are applied.
--
-- Default: Uses existing setting.
--
-- Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@.
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Must be at least 30 minutes.
mcrqPreferredMaintenanceWindow :: Lens' ModifyCluster (Maybe Text)
mcrqPreferredMaintenanceWindow = lens _mcrqPreferredMaintenanceWindow (\ s a -> s{_mcrqPreferredMaintenanceWindow = a});

-- | A list of virtual private cloud (VPC) security groups to be associated
-- with the cluster.
mcrqVPCSecurityGroupIds :: Lens' ModifyCluster [Text]
mcrqVPCSecurityGroupIds = lens _mcrqVPCSecurityGroupIds (\ s a -> s{_mcrqVPCSecurityGroupIds = a}) . _Default;

-- | The new cluster type.
--
-- When you submit your cluster resize request, your existing cluster goes
-- into a read-only mode. After Amazon Redshift provisions a new cluster
-- based on your resize requirements, there will be outage for a period
-- while the old cluster is deleted and your connection is switched to the
-- new cluster. You can use DescribeResize to track the progress of the
-- resize request.
--
-- Valid Values: @ multi-node | single-node @
mcrqClusterType :: Lens' ModifyCluster (Maybe Text)
mcrqClusterType = lens _mcrqClusterType (\ s a -> s{_mcrqClusterType = a});

-- | The new identifier for the cluster.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   Alphabetic characters must be lowercase.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
-- -   Must be unique for all clusters within an AWS account.
--
-- Example: @examplecluster@
mcrqNewClusterIdentifier :: Lens' ModifyCluster (Maybe Text)
mcrqNewClusterIdentifier = lens _mcrqNewClusterIdentifier (\ s a -> s{_mcrqNewClusterIdentifier = a});

-- | The new version number of the Amazon Redshift engine to upgrade to.
--
-- For major version upgrades, if a non-default cluster parameter group is
-- currently in use, a new cluster parameter group in the cluster parameter
-- group family for the new version must be specified. The new cluster
-- parameter group can be the default for that cluster parameter group
-- family. For more information about parameters and parameter groups, go
-- to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- Example: @1.0@
mcrqClusterVersion :: Lens' ModifyCluster (Maybe Text)
mcrqClusterVersion = lens _mcrqClusterVersion (\ s a -> s{_mcrqClusterVersion = a});

-- | The new node type of the cluster. If you specify a new node type, you
-- must also specify the number of nodes parameter.
--
-- When you submit your request to resize a cluster, Amazon Redshift sets
-- access permissions for the cluster to read-only. After Amazon Redshift
-- provisions a new cluster according to your resize requirements, there
-- will be a temporary outage while the old cluster is deleted and your
-- connection is switched to the new cluster. When the new connection is
-- complete, the original access permissions for the cluster are restored.
-- You can use DescribeResize to track the progress of the resize request.
--
-- Valid Values: @ ds1.xlarge@ | @ds1.8xlarge@ | @ ds2.xlarge@ |
-- @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@.
mcrqNodeType :: Lens' ModifyCluster (Maybe Text)
mcrqNodeType = lens _mcrqNodeType (\ s a -> s{_mcrqNodeType = a});

-- | If @true@, major version upgrades will be applied automatically to the
-- cluster during the maintenance window.
--
-- Default: @false@
mcrqAllowVersionUpgrade :: Lens' ModifyCluster (Maybe Bool)
mcrqAllowVersionUpgrade = lens _mcrqAllowVersionUpgrade (\ s a -> s{_mcrqAllowVersionUpgrade = a});

-- | The name of the cluster parameter group to apply to this cluster. This
-- change is applied only after the cluster is rebooted. To reboot a
-- cluster use RebootCluster.
--
-- Default: Uses existing setting.
--
-- Constraints: The cluster parameter group must be in the same parameter
-- group family that matches the cluster version.
mcrqClusterParameterGroupName :: Lens' ModifyCluster (Maybe Text)
mcrqClusterParameterGroupName = lens _mcrqClusterParameterGroupName (\ s a -> s{_mcrqClusterParameterGroupName = a});

-- | The unique identifier of the cluster to be modified.
--
-- Example: @examplecluster@
mcrqClusterIdentifier :: Lens' ModifyCluster Text
mcrqClusterIdentifier = lens _mcrqClusterIdentifier (\ s a -> s{_mcrqClusterIdentifier = a});

instance AWSRequest ModifyCluster where
        type Sv ModifyCluster = Redshift
        type Rs ModifyCluster = ModifyClusterResponse
        request = post
        response
          = receiveXMLWrapper "ModifyClusterResult"
              (\ s h x ->
                 ModifyClusterResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance ToHeaders ModifyCluster where
        toHeaders = const mempty

instance ToPath ModifyCluster where
        toPath = const "/"

instance ToQuery ModifyCluster where
        toQuery ModifyCluster'{..}
          = mconcat
              ["Action" =: ("ModifyCluster" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "MasterUserPassword" =: _mcrqMasterUserPassword,
               "HsmConfigurationIdentifier" =:
                 _mcrqHSMConfigurationIdentifier,
               "ClusterSecurityGroups" =:
                 toQuery
                   (toQueryList "ClusterSecurityGroupName" <$>
                      _mcrqClusterSecurityGroups),
               "AutomatedSnapshotRetentionPeriod" =:
                 _mcrqAutomatedSnapshotRetentionPeriod,
               "NumberOfNodes" =: _mcrqNumberOfNodes,
               "HsmClientCertificateIdentifier" =:
                 _mcrqHSMClientCertificateIdentifier,
               "PreferredMaintenanceWindow" =:
                 _mcrqPreferredMaintenanceWindow,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _mcrqVPCSecurityGroupIds),
               "ClusterType" =: _mcrqClusterType,
               "NewClusterIdentifier" =: _mcrqNewClusterIdentifier,
               "ClusterVersion" =: _mcrqClusterVersion,
               "NodeType" =: _mcrqNodeType,
               "AllowVersionUpgrade" =: _mcrqAllowVersionUpgrade,
               "ClusterParameterGroupName" =:
                 _mcrqClusterParameterGroupName,
               "ClusterIdentifier" =: _mcrqClusterIdentifier]

-- | /See:/ 'modifyClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcrsCluster'
--
-- * 'mcrsStatus'
data ModifyClusterResponse = ModifyClusterResponse'
    { _mcrsCluster :: !(Maybe Cluster)
    , _mcrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyClusterResponse' smart constructor.
modifyClusterResponse :: Int -> ModifyClusterResponse
modifyClusterResponse pStatus =
    ModifyClusterResponse'
    { _mcrsCluster = Nothing
    , _mcrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
mcrsCluster :: Lens' ModifyClusterResponse (Maybe Cluster)
mcrsCluster = lens _mcrsCluster (\ s a -> s{_mcrsCluster = a});

-- | FIXME: Undocumented member.
mcrsStatus :: Lens' ModifyClusterResponse Int
mcrsStatus = lens _mcrsStatus (\ s a -> s{_mcrsStatus = a});
