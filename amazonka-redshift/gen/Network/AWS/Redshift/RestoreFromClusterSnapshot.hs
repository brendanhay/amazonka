{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RestoreFromClusterSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cluster from a snapshot. By default, Amazon Redshift
-- creates the resulting cluster with the same configuration as the
-- original cluster from which the snapshot was created, except that the
-- new cluster is created with the default cluster security and parameter
-- groups. After Amazon Redshift creates the cluster, you can use the
-- ModifyCluster API to associate a different security group and different
-- parameter group with the restored cluster. If you are using a DS node
-- type, you can also choose to change to another DS node type of the same
-- size during restore.
--
-- If you restore a cluster into a VPC, you must provide a cluster subnet
-- group where you want the cluster restored.
--
-- For more information about working with snapshots, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_RestoreFromClusterSnapshot.html>
module Network.AWS.Redshift.RestoreFromClusterSnapshot
    (
    -- * Request
      RestoreFromClusterSnapshot
    -- ** Request constructor
    , restoreFromClusterSnapshot
    -- ** Request lenses
    , rfcsrqPubliclyAccessible
    , rfcsrqHSMConfigurationIdentifier
    , rfcsrqSnapshotClusterIdentifier
    , rfcsrqClusterSecurityGroups
    , rfcsrqAutomatedSnapshotRetentionPeriod
    , rfcsrqHSMClientCertificateIdentifier
    , rfcsrqClusterSubnetGroupName
    , rfcsrqElasticIP
    , rfcsrqPreferredMaintenanceWindow
    , rfcsrqAvailabilityZone
    , rfcsrqKMSKeyId
    , rfcsrqVPCSecurityGroupIds
    , rfcsrqOwnerAccount
    , rfcsrqNodeType
    , rfcsrqAllowVersionUpgrade
    , rfcsrqClusterParameterGroupName
    , rfcsrqPort
    , rfcsrqClusterIdentifier
    , rfcsrqSnapshotIdentifier

    -- * Response
    , RestoreFromClusterSnapshotResponse
    -- ** Response constructor
    , restoreFromClusterSnapshotResponse
    -- ** Response lenses
    , rfcsrsCluster
    , rfcsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'restoreFromClusterSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfcsrqPubliclyAccessible'
--
-- * 'rfcsrqHSMConfigurationIdentifier'
--
-- * 'rfcsrqSnapshotClusterIdentifier'
--
-- * 'rfcsrqClusterSecurityGroups'
--
-- * 'rfcsrqAutomatedSnapshotRetentionPeriod'
--
-- * 'rfcsrqHSMClientCertificateIdentifier'
--
-- * 'rfcsrqClusterSubnetGroupName'
--
-- * 'rfcsrqElasticIP'
--
-- * 'rfcsrqPreferredMaintenanceWindow'
--
-- * 'rfcsrqAvailabilityZone'
--
-- * 'rfcsrqKMSKeyId'
--
-- * 'rfcsrqVPCSecurityGroupIds'
--
-- * 'rfcsrqOwnerAccount'
--
-- * 'rfcsrqNodeType'
--
-- * 'rfcsrqAllowVersionUpgrade'
--
-- * 'rfcsrqClusterParameterGroupName'
--
-- * 'rfcsrqPort'
--
-- * 'rfcsrqClusterIdentifier'
--
-- * 'rfcsrqSnapshotIdentifier'
data RestoreFromClusterSnapshot = RestoreFromClusterSnapshot'
    { _rfcsrqPubliclyAccessible               :: !(Maybe Bool)
    , _rfcsrqHSMConfigurationIdentifier       :: !(Maybe Text)
    , _rfcsrqSnapshotClusterIdentifier        :: !(Maybe Text)
    , _rfcsrqClusterSecurityGroups            :: !(Maybe [Text])
    , _rfcsrqAutomatedSnapshotRetentionPeriod :: !(Maybe Int)
    , _rfcsrqHSMClientCertificateIdentifier   :: !(Maybe Text)
    , _rfcsrqClusterSubnetGroupName           :: !(Maybe Text)
    , _rfcsrqElasticIP                        :: !(Maybe Text)
    , _rfcsrqPreferredMaintenanceWindow       :: !(Maybe Text)
    , _rfcsrqAvailabilityZone                 :: !(Maybe Text)
    , _rfcsrqKMSKeyId                         :: !(Maybe Text)
    , _rfcsrqVPCSecurityGroupIds              :: !(Maybe [Text])
    , _rfcsrqOwnerAccount                     :: !(Maybe Text)
    , _rfcsrqNodeType                         :: !(Maybe Text)
    , _rfcsrqAllowVersionUpgrade              :: !(Maybe Bool)
    , _rfcsrqClusterParameterGroupName        :: !(Maybe Text)
    , _rfcsrqPort                             :: !(Maybe Int)
    , _rfcsrqClusterIdentifier                :: !Text
    , _rfcsrqSnapshotIdentifier               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreFromClusterSnapshot' smart constructor.
restoreFromClusterSnapshot :: Text -> Text -> RestoreFromClusterSnapshot
restoreFromClusterSnapshot pClusterIdentifier_ pSnapshotIdentifier_ =
    RestoreFromClusterSnapshot'
    { _rfcsrqPubliclyAccessible = Nothing
    , _rfcsrqHSMConfigurationIdentifier = Nothing
    , _rfcsrqSnapshotClusterIdentifier = Nothing
    , _rfcsrqClusterSecurityGroups = Nothing
    , _rfcsrqAutomatedSnapshotRetentionPeriod = Nothing
    , _rfcsrqHSMClientCertificateIdentifier = Nothing
    , _rfcsrqClusterSubnetGroupName = Nothing
    , _rfcsrqElasticIP = Nothing
    , _rfcsrqPreferredMaintenanceWindow = Nothing
    , _rfcsrqAvailabilityZone = Nothing
    , _rfcsrqKMSKeyId = Nothing
    , _rfcsrqVPCSecurityGroupIds = Nothing
    , _rfcsrqOwnerAccount = Nothing
    , _rfcsrqNodeType = Nothing
    , _rfcsrqAllowVersionUpgrade = Nothing
    , _rfcsrqClusterParameterGroupName = Nothing
    , _rfcsrqPort = Nothing
    , _rfcsrqClusterIdentifier = pClusterIdentifier_
    , _rfcsrqSnapshotIdentifier = pSnapshotIdentifier_
    }

-- | If @true@, the cluster can be accessed from a public network.
rfcsrqPubliclyAccessible :: Lens' RestoreFromClusterSnapshot (Maybe Bool)
rfcsrqPubliclyAccessible = lens _rfcsrqPubliclyAccessible (\ s a -> s{_rfcsrqPubliclyAccessible = a});

-- | Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
rfcsrqHSMConfigurationIdentifier :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqHSMConfigurationIdentifier = lens _rfcsrqHSMConfigurationIdentifier (\ s a -> s{_rfcsrqHSMConfigurationIdentifier = a});

-- | The name of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
rfcsrqSnapshotClusterIdentifier :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqSnapshotClusterIdentifier = lens _rfcsrqSnapshotClusterIdentifier (\ s a -> s{_rfcsrqSnapshotClusterIdentifier = a});

-- | A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
--
-- Cluster security groups only apply to clusters outside of VPCs.
rfcsrqClusterSecurityGroups :: Lens' RestoreFromClusterSnapshot [Text]
rfcsrqClusterSecurityGroups = lens _rfcsrqClusterSecurityGroups (\ s a -> s{_rfcsrqClusterSecurityGroups = a}) . _Default;

-- | The number of days that automated snapshots are retained. If the value
-- is 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot.
--
-- Default: The value selected for the cluster from which the snapshot was
-- taken.
--
-- Constraints: Must be a value from 0 to 35.
rfcsrqAutomatedSnapshotRetentionPeriod :: Lens' RestoreFromClusterSnapshot (Maybe Int)
rfcsrqAutomatedSnapshotRetentionPeriod = lens _rfcsrqAutomatedSnapshotRetentionPeriod (\ s a -> s{_rfcsrqAutomatedSnapshotRetentionPeriod = a});

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
rfcsrqHSMClientCertificateIdentifier :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqHSMClientCertificateIdentifier = lens _rfcsrqHSMClientCertificateIdentifier (\ s a -> s{_rfcsrqHSMClientCertificateIdentifier = a});

-- | The name of the subnet group where you want to cluster restored.
--
-- A snapshot of cluster in VPC can be restored only in VPC. Therefore, you
-- must provide subnet group name where you want the cluster restored.
rfcsrqClusterSubnetGroupName :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqClusterSubnetGroupName = lens _rfcsrqClusterSubnetGroupName (\ s a -> s{_rfcsrqClusterSubnetGroupName = a});

-- | The elastic IP (EIP) address for the cluster.
rfcsrqElasticIP :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqElasticIP = lens _rfcsrqElasticIP (\ s a -> s{_rfcsrqElasticIP = a});

-- | The weekly time range (in UTC) during which automated cluster
-- maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: The value selected for the cluster from which the snapshot was
-- taken. For more information about the time blocks for each region, see
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows>
-- in Amazon Redshift Cluster Management Guide.
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Minimum 30-minute window.
rfcsrqPreferredMaintenanceWindow :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqPreferredMaintenanceWindow = lens _rfcsrqPreferredMaintenanceWindow (\ s a -> s{_rfcsrqPreferredMaintenanceWindow = a});

-- | The Amazon EC2 Availability Zone in which to restore the cluster.
--
-- Default: A random, system-chosen Availability Zone.
--
-- Example: @us-east-1a@
rfcsrqAvailabilityZone :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqAvailabilityZone = lens _rfcsrqAvailabilityZone (\ s a -> s{_rfcsrqAvailabilityZone = a});

-- | The AWS Key Management Service (KMS) key ID of the encryption key that
-- you want to use to encrypt data in the cluster that you restore from a
-- shared snapshot.
rfcsrqKMSKeyId :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqKMSKeyId = lens _rfcsrqKMSKeyId (\ s a -> s{_rfcsrqKMSKeyId = a});

-- | A list of Virtual Private Cloud (VPC) security groups to be associated
-- with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
--
-- VPC security groups only apply to clusters in VPCs.
rfcsrqVPCSecurityGroupIds :: Lens' RestoreFromClusterSnapshot [Text]
rfcsrqVPCSecurityGroupIds = lens _rfcsrqVPCSecurityGroupIds (\ s a -> s{_rfcsrqVPCSecurityGroupIds = a}) . _Default;

-- | The AWS customer account used to create or copy the snapshot. Required
-- if you are restoring a snapshot you do not own, optional if you own the
-- snapshot.
rfcsrqOwnerAccount :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqOwnerAccount = lens _rfcsrqOwnerAccount (\ s a -> s{_rfcsrqOwnerAccount = a});

-- | The node type that the restored cluster will be provisioned with.
--
-- Default: The node type of the cluster from which the snapshot was taken.
-- You can modify this if you are using any DS node type. In that case, you
-- can choose to restore into another DS node type of the same size. For
-- example, you can restore ds1.8xlarge into ds2.8xlarge, or ds2.xlarge
-- into ds1.xlarge. If you have a DC instance type, you must restore into
-- that same instance type and size. In other words, you can only restore a
-- dc1.large instance type into another dc1.large instance type. For more
-- information about node types, see
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-about-clusters-and-nodes About Clusters and Nodes>
-- in the /Amazon Redshift Cluster Management Guide/
rfcsrqNodeType :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqNodeType = lens _rfcsrqNodeType (\ s a -> s{_rfcsrqNodeType = a});

-- | If @true@, major version upgrades can be applied during the maintenance
-- window to the Amazon Redshift engine that is running on the cluster.
--
-- Default: @true@
rfcsrqAllowVersionUpgrade :: Lens' RestoreFromClusterSnapshot (Maybe Bool)
rfcsrqAllowVersionUpgrade = lens _rfcsrqAllowVersionUpgrade (\ s a -> s{_rfcsrqAllowVersionUpgrade = a});

-- | The name of the parameter group to be associated with this cluster.
--
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
rfcsrqClusterParameterGroupName :: Lens' RestoreFromClusterSnapshot (Maybe Text)
rfcsrqClusterParameterGroupName = lens _rfcsrqClusterParameterGroupName (\ s a -> s{_rfcsrqClusterParameterGroupName = a});

-- | The port number on which the cluster accepts connections.
--
-- Default: The same port as the original cluster.
--
-- Constraints: Must be between @1115@ and @65535@.
rfcsrqPort :: Lens' RestoreFromClusterSnapshot (Maybe Int)
rfcsrqPort = lens _rfcsrqPort (\ s a -> s{_rfcsrqPort = a});

-- | The identifier of the cluster that will be created from restoring the
-- snapshot.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   Alphabetic characters must be lowercase.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
-- -   Must be unique for all clusters within an AWS account.
rfcsrqClusterIdentifier :: Lens' RestoreFromClusterSnapshot Text
rfcsrqClusterIdentifier = lens _rfcsrqClusterIdentifier (\ s a -> s{_rfcsrqClusterIdentifier = a});

-- | The name of the snapshot from which to create the new cluster. This
-- parameter isn\'t case sensitive.
--
-- Example: @my-snapshot-id@
rfcsrqSnapshotIdentifier :: Lens' RestoreFromClusterSnapshot Text
rfcsrqSnapshotIdentifier = lens _rfcsrqSnapshotIdentifier (\ s a -> s{_rfcsrqSnapshotIdentifier = a});

instance AWSRequest RestoreFromClusterSnapshot where
        type Sv RestoreFromClusterSnapshot = Redshift
        type Rs RestoreFromClusterSnapshot =
             RestoreFromClusterSnapshotResponse
        request = post
        response
          = receiveXMLWrapper
              "RestoreFromClusterSnapshotResult"
              (\ s h x ->
                 RestoreFromClusterSnapshotResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance ToHeaders RestoreFromClusterSnapshot where
        toHeaders = const mempty

instance ToPath RestoreFromClusterSnapshot where
        toPath = const "/"

instance ToQuery RestoreFromClusterSnapshot where
        toQuery RestoreFromClusterSnapshot'{..}
          = mconcat
              ["Action" =:
                 ("RestoreFromClusterSnapshot" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "PubliclyAccessible" =: _rfcsrqPubliclyAccessible,
               "HsmConfigurationIdentifier" =:
                 _rfcsrqHSMConfigurationIdentifier,
               "SnapshotClusterIdentifier" =:
                 _rfcsrqSnapshotClusterIdentifier,
               "ClusterSecurityGroups" =:
                 toQuery
                   (toQueryList "ClusterSecurityGroupName" <$>
                      _rfcsrqClusterSecurityGroups),
               "AutomatedSnapshotRetentionPeriod" =:
                 _rfcsrqAutomatedSnapshotRetentionPeriod,
               "HsmClientCertificateIdentifier" =:
                 _rfcsrqHSMClientCertificateIdentifier,
               "ClusterSubnetGroupName" =:
                 _rfcsrqClusterSubnetGroupName,
               "ElasticIp" =: _rfcsrqElasticIP,
               "PreferredMaintenanceWindow" =:
                 _rfcsrqPreferredMaintenanceWindow,
               "AvailabilityZone" =: _rfcsrqAvailabilityZone,
               "KmsKeyId" =: _rfcsrqKMSKeyId,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _rfcsrqVPCSecurityGroupIds),
               "OwnerAccount" =: _rfcsrqOwnerAccount,
               "NodeType" =: _rfcsrqNodeType,
               "AllowVersionUpgrade" =: _rfcsrqAllowVersionUpgrade,
               "ClusterParameterGroupName" =:
                 _rfcsrqClusterParameterGroupName,
               "Port" =: _rfcsrqPort,
               "ClusterIdentifier" =: _rfcsrqClusterIdentifier,
               "SnapshotIdentifier" =: _rfcsrqSnapshotIdentifier]

-- | /See:/ 'restoreFromClusterSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfcsrsCluster'
--
-- * 'rfcsrsStatus'
data RestoreFromClusterSnapshotResponse = RestoreFromClusterSnapshotResponse'
    { _rfcsrsCluster :: !(Maybe Cluster)
    , _rfcsrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreFromClusterSnapshotResponse' smart constructor.
restoreFromClusterSnapshotResponse :: Int -> RestoreFromClusterSnapshotResponse
restoreFromClusterSnapshotResponse pStatus_ =
    RestoreFromClusterSnapshotResponse'
    { _rfcsrsCluster = Nothing
    , _rfcsrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
rfcsrsCluster :: Lens' RestoreFromClusterSnapshotResponse (Maybe Cluster)
rfcsrsCluster = lens _rfcsrsCluster (\ s a -> s{_rfcsrsCluster = a});

-- | FIXME: Undocumented member.
rfcsrsStatus :: Lens' RestoreFromClusterSnapshotResponse Int
rfcsrsStatus = lens _rfcsrsStatus (\ s a -> s{_rfcsrsStatus = a});
