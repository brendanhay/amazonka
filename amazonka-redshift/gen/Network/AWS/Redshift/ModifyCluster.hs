{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a cluster. For example, you can add another security or parameter group, update the preferred maintenance window, or change the master user password. Resetting a cluster password or modifying the security groups associated with a cluster do not need a reboot. However, modifying a parameter group requires a reboot for parameters to take effect. For more information about managing clusters, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
--
-- You can also change node type and the number of nodes to scale up or down the cluster. When resizing a cluster, you must specify both the number of nodes and the node type even if one of the parameters does not change.
--
module Network.AWS.Redshift.ModifyCluster
    (
    -- * Creating a Request
      modifyCluster
    , ModifyCluster
    -- * Request Lenses
    , mcEnhancedVPCRouting
    , mcMasterUserPassword
    , mcPubliclyAccessible
    , mcHSMConfigurationIdentifier
    , mcClusterSecurityGroups
    , mcAutomatedSnapshotRetentionPeriod
    , mcHSMClientCertificateIdentifier
    , mcNumberOfNodes
    , mcElasticIP
    , mcPreferredMaintenanceWindow
    , mcVPCSecurityGroupIds
    , mcClusterType
    , mcNewClusterIdentifier
    , mcClusterVersion
    , mcNodeType
    , mcAllowVersionUpgrade
    , mcClusterParameterGroupName
    , mcClusterIdentifier

    -- * Destructuring the Response
    , modifyClusterResponse
    , ModifyClusterResponse
    -- * Response Lenses
    , mcrsCluster
    , mcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'modifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { _mcEnhancedVPCRouting               :: !(Maybe Bool)
  , _mcMasterUserPassword               :: !(Maybe Text)
  , _mcPubliclyAccessible               :: !(Maybe Bool)
  , _mcHSMConfigurationIdentifier       :: !(Maybe Text)
  , _mcClusterSecurityGroups            :: !(Maybe [Text])
  , _mcAutomatedSnapshotRetentionPeriod :: !(Maybe Int)
  , _mcHSMClientCertificateIdentifier   :: !(Maybe Text)
  , _mcNumberOfNodes                    :: !(Maybe Int)
  , _mcElasticIP                        :: !(Maybe Text)
  , _mcPreferredMaintenanceWindow       :: !(Maybe Text)
  , _mcVPCSecurityGroupIds              :: !(Maybe [Text])
  , _mcClusterType                      :: !(Maybe Text)
  , _mcNewClusterIdentifier             :: !(Maybe Text)
  , _mcClusterVersion                   :: !(Maybe Text)
  , _mcNodeType                         :: !(Maybe Text)
  , _mcAllowVersionUpgrade              :: !(Maybe Bool)
  , _mcClusterParameterGroupName        :: !(Maybe Text)
  , _mcClusterIdentifier                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcEnhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
--
-- * 'mcMasterUserPassword' - The new password for the cluster master user. This change is asynchronously applied as soon as possible. Between the time of the request and the completion of the request, the @MasterUserPassword@ element exists in the @PendingModifiedValues@ element of the operation response.  Default: Uses existing setting. Constraints:     * Must be between 8 and 64 characters in length.     * Must contain at least one uppercase letter.     * Must contain at least one lowercase letter.     * Must contain one number.     * Can be any printable ASCII character (ASCII code 33 to 126) except ' (single quote), " (double quote), \, /, @, or space.
--
-- * 'mcPubliclyAccessible' - If @true@ , the cluster can be accessed from a public network. Only clusters in VPCs can be set to be publicly available.
--
-- * 'mcHSMConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- * 'mcClusterSecurityGroups' - A list of cluster security groups to be authorized on this cluster. This change is asynchronously applied as soon as possible. Security groups currently associated with the cluster, and not in the list of groups to apply, will be revoked from the cluster. Constraints:     * Must be 1 to 255 alphanumeric characters or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
--
-- * 'mcAutomatedSnapshotRetentionPeriod' - The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .  If you decrease the automated snapshot retention period from its current value, existing automated snapshots that fall outside of the new retention period will be immediately deleted. Default: Uses existing setting. Constraints: Must be a value from 0 to 35.
--
-- * 'mcHSMClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- * 'mcNumberOfNodes' - The new number of nodes of the cluster. If you specify a new number of nodes, you must also specify the node type parameter. When you submit your request to resize a cluster, Amazon Redshift sets access permissions for the cluster to read-only. After Amazon Redshift provisions a new cluster according to your resize requirements, there will be a temporary outage while the old cluster is deleted and your connection is switched to the new cluster. When the new connection is complete, the original access permissions for the cluster are restored. You can use 'DescribeResize' to track the progress of the resize request.  Valid Values: Integer greater than @0@ .
--
-- * 'mcElasticIP' - The Elastic IP (EIP) address for the cluster. Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
--
-- * 'mcPreferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can occur, if necessary. If system maintenance is necessary during the window, it may result in an outage. This maintenance window change is made immediately. If the new maintenance window indicates the current time, there must be at least 120 minutes between the current time and end of the window in order to ensure that pending changes are applied. Default: Uses existing setting. Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@ . Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints: Must be at least 30 minutes.
--
-- * 'mcVPCSecurityGroupIds' - A list of virtual private cloud (VPC) security groups to be associated with the cluster.
--
-- * 'mcClusterType' - The new cluster type. When you submit your cluster resize request, your existing cluster goes into a read-only mode. After Amazon Redshift provisions a new cluster based on your resize requirements, there will be outage for a period while the old cluster is deleted and your connection is switched to the new cluster. You can use 'DescribeResize' to track the progress of the resize request.  Valid Values: @multi-node | single-node @
--
-- * 'mcNewClusterIdentifier' - The new identifier for the cluster. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * Alphabetic characters must be lowercase.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.     * Must be unique for all clusters within an AWS account. Example: @examplecluster@
--
-- * 'mcClusterVersion' - The new version number of the Amazon Redshift engine to upgrade to. For major version upgrades, if a non-default cluster parameter group is currently in use, a new cluster parameter group in the cluster parameter group family for the new version must be specified. The new cluster parameter group can be the default for that cluster parameter group family. For more information about parameters and parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ . Example: @1.0@
--
-- * 'mcNodeType' - The new node type of the cluster. If you specify a new node type, you must also specify the number of nodes parameter. When you submit your request to resize a cluster, Amazon Redshift sets access permissions for the cluster to read-only. After Amazon Redshift provisions a new cluster according to your resize requirements, there will be a temporary outage while the old cluster is deleted and your connection is switched to the new cluster. When the new connection is complete, the original access permissions for the cluster are restored. You can use 'DescribeResize' to track the progress of the resize request.  Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@
--
-- * 'mcAllowVersionUpgrade' - If @true@ , major version upgrades will be applied automatically to the cluster during the maintenance window.  Default: @false@
--
-- * 'mcClusterParameterGroupName' - The name of the cluster parameter group to apply to this cluster. This change is applied only after the cluster is rebooted. To reboot a cluster use 'RebootCluster' .  Default: Uses existing setting. Constraints: The cluster parameter group must be in the same parameter group family that matches the cluster version.
--
-- * 'mcClusterIdentifier' - The unique identifier of the cluster to be modified. Example: @examplecluster@
modifyCluster
    :: Text -- ^ 'mcClusterIdentifier'
    -> ModifyCluster
modifyCluster pClusterIdentifier_ =
  ModifyCluster'
    { _mcEnhancedVPCRouting = Nothing
    , _mcMasterUserPassword = Nothing
    , _mcPubliclyAccessible = Nothing
    , _mcHSMConfigurationIdentifier = Nothing
    , _mcClusterSecurityGroups = Nothing
    , _mcAutomatedSnapshotRetentionPeriod = Nothing
    , _mcHSMClientCertificateIdentifier = Nothing
    , _mcNumberOfNodes = Nothing
    , _mcElasticIP = Nothing
    , _mcPreferredMaintenanceWindow = Nothing
    , _mcVPCSecurityGroupIds = Nothing
    , _mcClusterType = Nothing
    , _mcNewClusterIdentifier = Nothing
    , _mcClusterVersion = Nothing
    , _mcNodeType = Nothing
    , _mcAllowVersionUpgrade = Nothing
    , _mcClusterParameterGroupName = Nothing
    , _mcClusterIdentifier = pClusterIdentifier_
    }


-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
mcEnhancedVPCRouting :: Lens' ModifyCluster (Maybe Bool)
mcEnhancedVPCRouting = lens _mcEnhancedVPCRouting (\ s a -> s{_mcEnhancedVPCRouting = a})

-- | The new password for the cluster master user. This change is asynchronously applied as soon as possible. Between the time of the request and the completion of the request, the @MasterUserPassword@ element exists in the @PendingModifiedValues@ element of the operation response.  Default: Uses existing setting. Constraints:     * Must be between 8 and 64 characters in length.     * Must contain at least one uppercase letter.     * Must contain at least one lowercase letter.     * Must contain one number.     * Can be any printable ASCII character (ASCII code 33 to 126) except ' (single quote), " (double quote), \, /, @, or space.
mcMasterUserPassword :: Lens' ModifyCluster (Maybe Text)
mcMasterUserPassword = lens _mcMasterUserPassword (\ s a -> s{_mcMasterUserPassword = a})

-- | If @true@ , the cluster can be accessed from a public network. Only clusters in VPCs can be set to be publicly available.
mcPubliclyAccessible :: Lens' ModifyCluster (Maybe Bool)
mcPubliclyAccessible = lens _mcPubliclyAccessible (\ s a -> s{_mcPubliclyAccessible = a})

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
mcHSMConfigurationIdentifier :: Lens' ModifyCluster (Maybe Text)
mcHSMConfigurationIdentifier = lens _mcHSMConfigurationIdentifier (\ s a -> s{_mcHSMConfigurationIdentifier = a})

-- | A list of cluster security groups to be authorized on this cluster. This change is asynchronously applied as soon as possible. Security groups currently associated with the cluster, and not in the list of groups to apply, will be revoked from the cluster. Constraints:     * Must be 1 to 255 alphanumeric characters or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
mcClusterSecurityGroups :: Lens' ModifyCluster [Text]
mcClusterSecurityGroups = lens _mcClusterSecurityGroups (\ s a -> s{_mcClusterSecurityGroups = a}) . _Default . _Coerce

-- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .  If you decrease the automated snapshot retention period from its current value, existing automated snapshots that fall outside of the new retention period will be immediately deleted. Default: Uses existing setting. Constraints: Must be a value from 0 to 35.
mcAutomatedSnapshotRetentionPeriod :: Lens' ModifyCluster (Maybe Int)
mcAutomatedSnapshotRetentionPeriod = lens _mcAutomatedSnapshotRetentionPeriod (\ s a -> s{_mcAutomatedSnapshotRetentionPeriod = a})

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
mcHSMClientCertificateIdentifier :: Lens' ModifyCluster (Maybe Text)
mcHSMClientCertificateIdentifier = lens _mcHSMClientCertificateIdentifier (\ s a -> s{_mcHSMClientCertificateIdentifier = a})

-- | The new number of nodes of the cluster. If you specify a new number of nodes, you must also specify the node type parameter. When you submit your request to resize a cluster, Amazon Redshift sets access permissions for the cluster to read-only. After Amazon Redshift provisions a new cluster according to your resize requirements, there will be a temporary outage while the old cluster is deleted and your connection is switched to the new cluster. When the new connection is complete, the original access permissions for the cluster are restored. You can use 'DescribeResize' to track the progress of the resize request.  Valid Values: Integer greater than @0@ .
mcNumberOfNodes :: Lens' ModifyCluster (Maybe Int)
mcNumberOfNodes = lens _mcNumberOfNodes (\ s a -> s{_mcNumberOfNodes = a})

-- | The Elastic IP (EIP) address for the cluster. Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
mcElasticIP :: Lens' ModifyCluster (Maybe Text)
mcElasticIP = lens _mcElasticIP (\ s a -> s{_mcElasticIP = a})

-- | The weekly time range (in UTC) during which system maintenance can occur, if necessary. If system maintenance is necessary during the window, it may result in an outage. This maintenance window change is made immediately. If the new maintenance window indicates the current time, there must be at least 120 minutes between the current time and end of the window in order to ensure that pending changes are applied. Default: Uses existing setting. Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@ . Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints: Must be at least 30 minutes.
mcPreferredMaintenanceWindow :: Lens' ModifyCluster (Maybe Text)
mcPreferredMaintenanceWindow = lens _mcPreferredMaintenanceWindow (\ s a -> s{_mcPreferredMaintenanceWindow = a})

-- | A list of virtual private cloud (VPC) security groups to be associated with the cluster.
mcVPCSecurityGroupIds :: Lens' ModifyCluster [Text]
mcVPCSecurityGroupIds = lens _mcVPCSecurityGroupIds (\ s a -> s{_mcVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | The new cluster type. When you submit your cluster resize request, your existing cluster goes into a read-only mode. After Amazon Redshift provisions a new cluster based on your resize requirements, there will be outage for a period while the old cluster is deleted and your connection is switched to the new cluster. You can use 'DescribeResize' to track the progress of the resize request.  Valid Values: @multi-node | single-node @
mcClusterType :: Lens' ModifyCluster (Maybe Text)
mcClusterType = lens _mcClusterType (\ s a -> s{_mcClusterType = a})

-- | The new identifier for the cluster. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * Alphabetic characters must be lowercase.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.     * Must be unique for all clusters within an AWS account. Example: @examplecluster@
mcNewClusterIdentifier :: Lens' ModifyCluster (Maybe Text)
mcNewClusterIdentifier = lens _mcNewClusterIdentifier (\ s a -> s{_mcNewClusterIdentifier = a})

-- | The new version number of the Amazon Redshift engine to upgrade to. For major version upgrades, if a non-default cluster parameter group is currently in use, a new cluster parameter group in the cluster parameter group family for the new version must be specified. The new cluster parameter group can be the default for that cluster parameter group family. For more information about parameters and parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ . Example: @1.0@
mcClusterVersion :: Lens' ModifyCluster (Maybe Text)
mcClusterVersion = lens _mcClusterVersion (\ s a -> s{_mcClusterVersion = a})

-- | The new node type of the cluster. If you specify a new node type, you must also specify the number of nodes parameter. When you submit your request to resize a cluster, Amazon Redshift sets access permissions for the cluster to read-only. After Amazon Redshift provisions a new cluster according to your resize requirements, there will be a temporary outage while the old cluster is deleted and your connection is switched to the new cluster. When the new connection is complete, the original access permissions for the cluster are restored. You can use 'DescribeResize' to track the progress of the resize request.  Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@
mcNodeType :: Lens' ModifyCluster (Maybe Text)
mcNodeType = lens _mcNodeType (\ s a -> s{_mcNodeType = a})

-- | If @true@ , major version upgrades will be applied automatically to the cluster during the maintenance window.  Default: @false@
mcAllowVersionUpgrade :: Lens' ModifyCluster (Maybe Bool)
mcAllowVersionUpgrade = lens _mcAllowVersionUpgrade (\ s a -> s{_mcAllowVersionUpgrade = a})

-- | The name of the cluster parameter group to apply to this cluster. This change is applied only after the cluster is rebooted. To reboot a cluster use 'RebootCluster' .  Default: Uses existing setting. Constraints: The cluster parameter group must be in the same parameter group family that matches the cluster version.
mcClusterParameterGroupName :: Lens' ModifyCluster (Maybe Text)
mcClusterParameterGroupName = lens _mcClusterParameterGroupName (\ s a -> s{_mcClusterParameterGroupName = a})

-- | The unique identifier of the cluster to be modified. Example: @examplecluster@
mcClusterIdentifier :: Lens' ModifyCluster Text
mcClusterIdentifier = lens _mcClusterIdentifier (\ s a -> s{_mcClusterIdentifier = a})

instance AWSRequest ModifyCluster where
        type Rs ModifyCluster = ModifyClusterResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "ModifyClusterResult"
              (\ s h x ->
                 ModifyClusterResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance Hashable ModifyCluster where

instance NFData ModifyCluster where

instance ToHeaders ModifyCluster where
        toHeaders = const mempty

instance ToPath ModifyCluster where
        toPath = const "/"

instance ToQuery ModifyCluster where
        toQuery ModifyCluster'{..}
          = mconcat
              ["Action" =: ("ModifyCluster" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "EnhancedVpcRouting" =: _mcEnhancedVPCRouting,
               "MasterUserPassword" =: _mcMasterUserPassword,
               "PubliclyAccessible" =: _mcPubliclyAccessible,
               "HsmConfigurationIdentifier" =:
                 _mcHSMConfigurationIdentifier,
               "ClusterSecurityGroups" =:
                 toQuery
                   (toQueryList "ClusterSecurityGroupName" <$>
                      _mcClusterSecurityGroups),
               "AutomatedSnapshotRetentionPeriod" =:
                 _mcAutomatedSnapshotRetentionPeriod,
               "HsmClientCertificateIdentifier" =:
                 _mcHSMClientCertificateIdentifier,
               "NumberOfNodes" =: _mcNumberOfNodes,
               "ElasticIp" =: _mcElasticIP,
               "PreferredMaintenanceWindow" =:
                 _mcPreferredMaintenanceWindow,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _mcVPCSecurityGroupIds),
               "ClusterType" =: _mcClusterType,
               "NewClusterIdentifier" =: _mcNewClusterIdentifier,
               "ClusterVersion" =: _mcClusterVersion,
               "NodeType" =: _mcNodeType,
               "AllowVersionUpgrade" =: _mcAllowVersionUpgrade,
               "ClusterParameterGroupName" =:
                 _mcClusterParameterGroupName,
               "ClusterIdentifier" =: _mcClusterIdentifier]

-- | /See:/ 'modifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { _mcrsCluster        :: !(Maybe Cluster)
  , _mcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcrsCluster' - Undocumented member.
--
-- * 'mcrsResponseStatus' - -- | The response status code.
modifyClusterResponse
    :: Int -- ^ 'mcrsResponseStatus'
    -> ModifyClusterResponse
modifyClusterResponse pResponseStatus_ =
  ModifyClusterResponse'
    {_mcrsCluster = Nothing, _mcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
mcrsCluster :: Lens' ModifyClusterResponse (Maybe Cluster)
mcrsCluster = lens _mcrsCluster (\ s a -> s{_mcrsCluster = a})

-- | -- | The response status code.
mcrsResponseStatus :: Lens' ModifyClusterResponse Int
mcrsResponseStatus = lens _mcrsResponseStatus (\ s a -> s{_mcrsResponseStatus = a})

instance NFData ModifyClusterResponse where
