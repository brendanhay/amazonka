{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Cluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Cluster where

import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.ClusterSetting
import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.
--
--
--
-- /See:/ 'cluster' smart constructor.
data Cluster = Cluster'
  { _cStatus :: !(Maybe Text),
    _cClusterARN :: !(Maybe Text),
    _cAttachments :: !(Maybe [Attachment]),
    _cRunningTasksCount :: !(Maybe Int),
    _cDefaultCapacityProviderStrategy ::
      !(Maybe [CapacityProviderStrategyItem]),
    _cSettings :: !(Maybe [ClusterSetting]),
    _cRegisteredContainerInstancesCount :: !(Maybe Int),
    _cPendingTasksCount :: !(Maybe Int),
    _cClusterName :: !(Maybe Text),
    _cStatistics :: !(Maybe [KeyValuePair]),
    _cAttachmentsStatus :: !(Maybe Text),
    _cCapacityProviders :: !(Maybe [Text]),
    _cActiveServicesCount :: !(Maybe Int),
    _cTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The status of the cluster. The following are the possible states that will be returned.     * ACTIVE    * The cluster is ready to accept tasks and if applicable you can register container instances with the cluster.     * PROVISIONING    * The cluster has capacity providers associated with it and the resources needed for the capacity provider are being created.     * DEPROVISIONING    * The cluster has capacity providers associated with it and the resources needed for the capacity provider are being deleted.     * FAILED    * The cluster has capacity providers associated with it and the resources needed for the capacity provider have failed to create.     * INACTIVE    * The cluster has been deleted. Clusters with an @INACTIVE@ status may remain discoverable in your account for a period of time. However, this behavior is subject to change in the future, so you should not rely on @INACTIVE@ clusters persisting.
--
-- * 'cClusterARN' - The Amazon Resource Name (ARN) that identifies the cluster. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the cluster, the AWS account ID of the cluster owner, the @cluster@ namespace, and then the cluster name. For example, @arn:aws:ecs:region:012345678910:cluster/test@ .
--
-- * 'cAttachments' - The resources attached to a cluster. When using a capacity provider with a cluster, the Auto Scaling plan that is created will be returned as a cluster attachment.
--
-- * 'cRunningTasksCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- * 'cDefaultCapacityProviderStrategy' - The default capacity provider strategy for the cluster. When services or tasks are run in the cluster with no launch type or capacity provider strategy specified, the default capacity provider strategy is used.
--
-- * 'cSettings' - The settings for the cluster. This parameter indicates whether CloudWatch Container Insights is enabled or disabled for a cluster.
--
-- * 'cRegisteredContainerInstancesCount' - The number of container instances registered into the cluster. This includes container instances in both @ACTIVE@ and @DRAINING@ status.
--
-- * 'cPendingTasksCount' - The number of tasks in the cluster that are in the @PENDING@ state.
--
-- * 'cClusterName' - A user-generated string that you use to identify your cluster.
--
-- * 'cStatistics' - Additional information about your clusters that are separated by launch type, including:     * runningEC2TasksCount     * RunningFargateTasksCount     * pendingEC2TasksCount     * pendingFargateTasksCount     * activeEC2ServiceCount     * activeFargateServiceCount     * drainingEC2ServiceCount     * drainingFargateServiceCount
--
-- * 'cAttachmentsStatus' - The status of the capacity providers associated with the cluster. The following are the states that will be returned:     * UPDATE_IN_PROGRESS    * The available capacity providers for the cluster are updating. This occurs when the Auto Scaling plan is provisioning or deprovisioning.     * UPDATE_COMPLETE    * The capacity providers have successfully updated.     * UPDATE_FAILED    * The capacity provider updates failed.
--
-- * 'cCapacityProviders' - The capacity providers associated with the cluster.
--
-- * 'cActiveServicesCount' - The number of services that are running on the cluster in an @ACTIVE@ state. You can view these services with 'ListServices' .
--
-- * 'cTags' - The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
cluster ::
  Cluster
cluster =
  Cluster'
    { _cStatus = Nothing,
      _cClusterARN = Nothing,
      _cAttachments = Nothing,
      _cRunningTasksCount = Nothing,
      _cDefaultCapacityProviderStrategy = Nothing,
      _cSettings = Nothing,
      _cRegisteredContainerInstancesCount = Nothing,
      _cPendingTasksCount = Nothing,
      _cClusterName = Nothing,
      _cStatistics = Nothing,
      _cAttachmentsStatus = Nothing,
      _cCapacityProviders = Nothing,
      _cActiveServicesCount = Nothing,
      _cTags = Nothing
    }

-- | The status of the cluster. The following are the possible states that will be returned.     * ACTIVE    * The cluster is ready to accept tasks and if applicable you can register container instances with the cluster.     * PROVISIONING    * The cluster has capacity providers associated with it and the resources needed for the capacity provider are being created.     * DEPROVISIONING    * The cluster has capacity providers associated with it and the resources needed for the capacity provider are being deleted.     * FAILED    * The cluster has capacity providers associated with it and the resources needed for the capacity provider have failed to create.     * INACTIVE    * The cluster has been deleted. Clusters with an @INACTIVE@ status may remain discoverable in your account for a period of time. However, this behavior is subject to change in the future, so you should not rely on @INACTIVE@ clusters persisting.
cStatus :: Lens' Cluster (Maybe Text)
cStatus = lens _cStatus (\s a -> s {_cStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the cluster, the AWS account ID of the cluster owner, the @cluster@ namespace, and then the cluster name. For example, @arn:aws:ecs:region:012345678910:cluster/test@ .
cClusterARN :: Lens' Cluster (Maybe Text)
cClusterARN = lens _cClusterARN (\s a -> s {_cClusterARN = a})

-- | The resources attached to a cluster. When using a capacity provider with a cluster, the Auto Scaling plan that is created will be returned as a cluster attachment.
cAttachments :: Lens' Cluster [Attachment]
cAttachments = lens _cAttachments (\s a -> s {_cAttachments = a}) . _Default . _Coerce

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
cRunningTasksCount :: Lens' Cluster (Maybe Int)
cRunningTasksCount = lens _cRunningTasksCount (\s a -> s {_cRunningTasksCount = a})

-- | The default capacity provider strategy for the cluster. When services or tasks are run in the cluster with no launch type or capacity provider strategy specified, the default capacity provider strategy is used.
cDefaultCapacityProviderStrategy :: Lens' Cluster [CapacityProviderStrategyItem]
cDefaultCapacityProviderStrategy = lens _cDefaultCapacityProviderStrategy (\s a -> s {_cDefaultCapacityProviderStrategy = a}) . _Default . _Coerce

-- | The settings for the cluster. This parameter indicates whether CloudWatch Container Insights is enabled or disabled for a cluster.
cSettings :: Lens' Cluster [ClusterSetting]
cSettings = lens _cSettings (\s a -> s {_cSettings = a}) . _Default . _Coerce

-- | The number of container instances registered into the cluster. This includes container instances in both @ACTIVE@ and @DRAINING@ status.
cRegisteredContainerInstancesCount :: Lens' Cluster (Maybe Int)
cRegisteredContainerInstancesCount = lens _cRegisteredContainerInstancesCount (\s a -> s {_cRegisteredContainerInstancesCount = a})

-- | The number of tasks in the cluster that are in the @PENDING@ state.
cPendingTasksCount :: Lens' Cluster (Maybe Int)
cPendingTasksCount = lens _cPendingTasksCount (\s a -> s {_cPendingTasksCount = a})

-- | A user-generated string that you use to identify your cluster.
cClusterName :: Lens' Cluster (Maybe Text)
cClusterName = lens _cClusterName (\s a -> s {_cClusterName = a})

-- | Additional information about your clusters that are separated by launch type, including:     * runningEC2TasksCount     * RunningFargateTasksCount     * pendingEC2TasksCount     * pendingFargateTasksCount     * activeEC2ServiceCount     * activeFargateServiceCount     * drainingEC2ServiceCount     * drainingFargateServiceCount
cStatistics :: Lens' Cluster [KeyValuePair]
cStatistics = lens _cStatistics (\s a -> s {_cStatistics = a}) . _Default . _Coerce

-- | The status of the capacity providers associated with the cluster. The following are the states that will be returned:     * UPDATE_IN_PROGRESS    * The available capacity providers for the cluster are updating. This occurs when the Auto Scaling plan is provisioning or deprovisioning.     * UPDATE_COMPLETE    * The capacity providers have successfully updated.     * UPDATE_FAILED    * The capacity provider updates failed.
cAttachmentsStatus :: Lens' Cluster (Maybe Text)
cAttachmentsStatus = lens _cAttachmentsStatus (\s a -> s {_cAttachmentsStatus = a})

-- | The capacity providers associated with the cluster.
cCapacityProviders :: Lens' Cluster [Text]
cCapacityProviders = lens _cCapacityProviders (\s a -> s {_cCapacityProviders = a}) . _Default . _Coerce

-- | The number of services that are running on the cluster in an @ACTIVE@ state. You can view these services with 'ListServices' .
cActiveServicesCount :: Lens' Cluster (Maybe Int)
cActiveServicesCount = lens _cActiveServicesCount (\s a -> s {_cActiveServicesCount = a})

-- | The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
cTags :: Lens' Cluster [Tag]
cTags = lens _cTags (\s a -> s {_cTags = a}) . _Default . _Coerce

instance FromJSON Cluster where
  parseJSON =
    withObject
      "Cluster"
      ( \x ->
          Cluster'
            <$> (x .:? "status")
            <*> (x .:? "clusterArn")
            <*> (x .:? "attachments" .!= mempty)
            <*> (x .:? "runningTasksCount")
            <*> (x .:? "defaultCapacityProviderStrategy" .!= mempty)
            <*> (x .:? "settings" .!= mempty)
            <*> (x .:? "registeredContainerInstancesCount")
            <*> (x .:? "pendingTasksCount")
            <*> (x .:? "clusterName")
            <*> (x .:? "statistics" .!= mempty)
            <*> (x .:? "attachmentsStatus")
            <*> (x .:? "capacityProviders" .!= mempty)
            <*> (x .:? "activeServicesCount")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable Cluster

instance NFData Cluster
