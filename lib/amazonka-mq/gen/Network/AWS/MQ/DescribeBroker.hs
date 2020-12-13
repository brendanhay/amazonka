{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified broker.
module Network.AWS.MQ.DescribeBroker
  ( -- * Creating a request
    DescribeBroker (..),
    mkDescribeBroker,

    -- ** Request lenses
    dbBrokerId,

    -- * Destructuring the response
    DescribeBrokerResponse (..),
    mkDescribeBrokerResponse,

    -- ** Response lenses
    dbrsBrokerName,
    dbrsEngineVersion,
    dbrsPendingAuthenticationStrategy,
    dbrsBrokerState,
    dbrsPubliclyAccessible,
    dbrsAutoMinorVersionUpgrade,
    dbrsSecurityGroups,
    dbrsUsers,
    dbrsPendingSecurityGroups,
    dbrsSubnetIds,
    dbrsCreated,
    dbrsConfigurations,
    dbrsAuthenticationStrategy,
    dbrsPendingHostInstanceType,
    dbrsLdapServerMetadata,
    dbrsMaintenanceWindowStartTime,
    dbrsLogs,
    dbrsEncryptionOptions,
    dbrsDeploymentMode,
    dbrsPendingEngineVersion,
    dbrsBrokerId,
    dbrsPendingLdapServerMetadata,
    dbrsEngineType,
    dbrsBrokerARN,
    dbrsTags,
    dbrsBrokerInstances,
    dbrsHostInstanceType,
    dbrsStorageType,
    dbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBroker' smart constructor.
newtype DescribeBroker = DescribeBroker'
  { -- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
    brokerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBroker' with the minimum fields required to make a request.
--
-- * 'brokerId' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
mkDescribeBroker ::
  -- | 'brokerId'
  Lude.Text ->
  DescribeBroker
mkDescribeBroker pBrokerId_ =
  DescribeBroker' {brokerId = pBrokerId_}

-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBrokerId :: Lens.Lens' DescribeBroker Lude.Text
dbBrokerId = Lens.lens (brokerId :: DescribeBroker -> Lude.Text) (\s a -> s {brokerId = a} :: DescribeBroker)
{-# DEPRECATED dbBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

instance Lude.AWSRequest DescribeBroker where
  type Rs DescribeBroker = DescribeBrokerResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBrokerResponse'
            Lude.<$> (x Lude..?> "brokerName")
            Lude.<*> (x Lude..?> "engineVersion")
            Lude.<*> (x Lude..?> "pendingAuthenticationStrategy")
            Lude.<*> (x Lude..?> "brokerState")
            Lude.<*> (x Lude..?> "publiclyAccessible")
            Lude.<*> (x Lude..?> "autoMinorVersionUpgrade")
            Lude.<*> (x Lude..?> "securityGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "users" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "pendingSecurityGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "subnetIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "created")
            Lude.<*> (x Lude..?> "configurations")
            Lude.<*> (x Lude..?> "authenticationStrategy")
            Lude.<*> (x Lude..?> "pendingHostInstanceType")
            Lude.<*> (x Lude..?> "ldapServerMetadata")
            Lude.<*> (x Lude..?> "maintenanceWindowStartTime")
            Lude.<*> (x Lude..?> "logs")
            Lude.<*> (x Lude..?> "encryptionOptions")
            Lude.<*> (x Lude..?> "deploymentMode")
            Lude.<*> (x Lude..?> "pendingEngineVersion")
            Lude.<*> (x Lude..?> "brokerId")
            Lude.<*> (x Lude..?> "pendingLdapServerMetadata")
            Lude.<*> (x Lude..?> "engineType")
            Lude.<*> (x Lude..?> "brokerArn")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "brokerInstances" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "hostInstanceType")
            Lude.<*> (x Lude..?> "storageType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBroker where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeBroker where
  toPath DescribeBroker' {..} =
    Lude.mconcat ["/v1/brokers/", Lude.toBS brokerId]

instance Lude.ToQuery DescribeBroker where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeBrokerResponse' smart constructor.
data DescribeBrokerResponse = DescribeBrokerResponse'
  { -- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
    brokerName :: Lude.Maybe Lude.Text,
    -- | The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
    engineVersion :: Lude.Maybe Lude.Text,
    -- | The authentication strategy that will be applied when the broker is rebooted.
    pendingAuthenticationStrategy :: Lude.Maybe AuthenticationStrategy,
    -- | The status of the broker.
    brokerState :: Lude.Maybe BrokerState,
    -- | Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    -- | Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
    securityGroups :: Lude.Maybe [Lude.Text],
    -- | The list of all broker usernames for the specified broker.
    users :: Lude.Maybe [UserSummary],
    -- | The list of pending security groups to authorize connections to brokers.
    pendingSecurityGroups :: Lude.Maybe [Lude.Text],
    -- | The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
    subnetIds :: Lude.Maybe [Lude.Text],
    -- | The time when the broker was created.
    created :: Lude.Maybe Lude.Timestamp,
    -- | The list of all revisions for the specified configuration.
    configurations :: Lude.Maybe Configurations,
    -- | The authentication strategy used to secure the broker.
    authenticationStrategy :: Lude.Maybe AuthenticationStrategy,
    -- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
    pendingHostInstanceType :: Lude.Maybe Lude.Text,
    -- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
    ldapServerMetadata :: Lude.Maybe LdapServerMetadataOutput,
    -- | The parameters that determine the WeeklyStartTime.
    maintenanceWindowStartTime :: Lude.Maybe WeeklyStartTime,
    -- | The list of information about logs currently enabled and pending to be deployed for the specified broker.
    logs :: Lude.Maybe LogsSummary,
    -- | Encryption options for the broker.
    encryptionOptions :: Lude.Maybe EncryptionOptions,
    -- | Required. The deployment mode of the broker.
    deploymentMode :: Lude.Maybe DeploymentMode,
    -- | The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
    pendingEngineVersion :: Lude.Maybe Lude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Lude.Maybe Lude.Text,
    -- | The metadata of the LDAP server that will be used to authenticate and authorize connections to the broker once it is rebooted.
    pendingLdapServerMetadata :: Lude.Maybe LdapServerMetadataOutput,
    -- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
    engineType :: Lude.Maybe EngineType,
    -- | The Amazon Resource Name (ARN) of the broker.
    brokerARN :: Lude.Maybe Lude.Text,
    -- | The list of all tags associated with this broker.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A list of information about allocated brokers.
    brokerInstances :: Lude.Maybe [BrokerInstance],
    -- | The broker's instance type.
    hostInstanceType :: Lude.Maybe Lude.Text,
    -- | The broker's storage type.
    storageType :: Lude.Maybe BrokerStorageType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBrokerResponse' with the minimum fields required to make a request.
--
-- * 'brokerName' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
-- * 'engineVersion' - The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
-- * 'pendingAuthenticationStrategy' - The authentication strategy that will be applied when the broker is rebooted.
-- * 'brokerState' - The status of the broker.
-- * 'publiclyAccessible' - Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
-- * 'autoMinorVersionUpgrade' - Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
-- * 'securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
-- * 'users' - The list of all broker usernames for the specified broker.
-- * 'pendingSecurityGroups' - The list of pending security groups to authorize connections to brokers.
-- * 'subnetIds' - The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
-- * 'created' - The time when the broker was created.
-- * 'configurations' - The list of all revisions for the specified configuration.
-- * 'authenticationStrategy' - The authentication strategy used to secure the broker.
-- * 'pendingHostInstanceType' - The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
-- * 'ldapServerMetadata' - The metadata of the LDAP server used to authenticate and authorize connections to the broker.
-- * 'maintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
-- * 'logs' - The list of information about logs currently enabled and pending to be deployed for the specified broker.
-- * 'encryptionOptions' - Encryption options for the broker.
-- * 'deploymentMode' - Required. The deployment mode of the broker.
-- * 'pendingEngineVersion' - The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
-- * 'pendingLdapServerMetadata' - The metadata of the LDAP server that will be used to authenticate and authorize connections to the broker once it is rebooted.
-- * 'engineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
-- * 'brokerARN' - The Amazon Resource Name (ARN) of the broker.
-- * 'tags' - The list of all tags associated with this broker.
-- * 'brokerInstances' - A list of information about allocated brokers.
-- * 'hostInstanceType' - The broker's instance type.
-- * 'storageType' - The broker's storage type.
-- * 'responseStatus' - The response status code.
mkDescribeBrokerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBrokerResponse
mkDescribeBrokerResponse pResponseStatus_ =
  DescribeBrokerResponse'
    { brokerName = Lude.Nothing,
      engineVersion = Lude.Nothing,
      pendingAuthenticationStrategy = Lude.Nothing,
      brokerState = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      securityGroups = Lude.Nothing,
      users = Lude.Nothing,
      pendingSecurityGroups = Lude.Nothing,
      subnetIds = Lude.Nothing,
      created = Lude.Nothing,
      configurations = Lude.Nothing,
      authenticationStrategy = Lude.Nothing,
      pendingHostInstanceType = Lude.Nothing,
      ldapServerMetadata = Lude.Nothing,
      maintenanceWindowStartTime = Lude.Nothing,
      logs = Lude.Nothing,
      encryptionOptions = Lude.Nothing,
      deploymentMode = Lude.Nothing,
      pendingEngineVersion = Lude.Nothing,
      brokerId = Lude.Nothing,
      pendingLdapServerMetadata = Lude.Nothing,
      engineType = Lude.Nothing,
      brokerARN = Lude.Nothing,
      tags = Lude.Nothing,
      brokerInstances = Lude.Nothing,
      hostInstanceType = Lude.Nothing,
      storageType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- /Note:/ Consider using 'brokerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsBrokerName :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Lude.Text)
dbrsBrokerName = Lens.lens (brokerName :: DescribeBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {brokerName = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsBrokerName "Use generic-lens or generic-optics with 'brokerName' instead." #-}

-- | The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsEngineVersion :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Lude.Text)
dbrsEngineVersion = Lens.lens (engineVersion :: DescribeBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The authentication strategy that will be applied when the broker is rebooted.
--
-- /Note:/ Consider using 'pendingAuthenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsPendingAuthenticationStrategy :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe AuthenticationStrategy)
dbrsPendingAuthenticationStrategy = Lens.lens (pendingAuthenticationStrategy :: DescribeBrokerResponse -> Lude.Maybe AuthenticationStrategy) (\s a -> s {pendingAuthenticationStrategy = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsPendingAuthenticationStrategy "Use generic-lens or generic-optics with 'pendingAuthenticationStrategy' instead." #-}

-- | The status of the broker.
--
-- /Note:/ Consider using 'brokerState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsBrokerState :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe BrokerState)
dbrsBrokerState = Lens.lens (brokerState :: DescribeBrokerResponse -> Lude.Maybe BrokerState) (\s a -> s {brokerState = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsBrokerState "Use generic-lens or generic-optics with 'brokerState' instead." #-}

-- | Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsPubliclyAccessible :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Lude.Bool)
dbrsPubliclyAccessible = Lens.lens (publiclyAccessible :: DescribeBrokerResponse -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsAutoMinorVersionUpgrade :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Lude.Bool)
dbrsAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: DescribeBrokerResponse -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsSecurityGroups :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe [Lude.Text])
dbrsSecurityGroups = Lens.lens (securityGroups :: DescribeBrokerResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The list of all broker usernames for the specified broker.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsUsers :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe [UserSummary])
dbrsUsers = Lens.lens (users :: DescribeBrokerResponse -> Lude.Maybe [UserSummary]) (\s a -> s {users = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The list of pending security groups to authorize connections to brokers.
--
-- /Note:/ Consider using 'pendingSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsPendingSecurityGroups :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe [Lude.Text])
dbrsPendingSecurityGroups = Lens.lens (pendingSecurityGroups :: DescribeBrokerResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {pendingSecurityGroups = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsPendingSecurityGroups "Use generic-lens or generic-optics with 'pendingSecurityGroups' instead." #-}

-- | The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsSubnetIds :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe [Lude.Text])
dbrsSubnetIds = Lens.lens (subnetIds :: DescribeBrokerResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The time when the broker was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsCreated :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Lude.Timestamp)
dbrsCreated = Lens.lens (created :: DescribeBrokerResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The list of all revisions for the specified configuration.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsConfigurations :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Configurations)
dbrsConfigurations = Lens.lens (configurations :: DescribeBrokerResponse -> Lude.Maybe Configurations) (\s a -> s {configurations = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The authentication strategy used to secure the broker.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsAuthenticationStrategy :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe AuthenticationStrategy)
dbrsAuthenticationStrategy = Lens.lens (authenticationStrategy :: DescribeBrokerResponse -> Lude.Maybe AuthenticationStrategy) (\s a -> s {authenticationStrategy = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
--
-- /Note:/ Consider using 'pendingHostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsPendingHostInstanceType :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Lude.Text)
dbrsPendingHostInstanceType = Lens.lens (pendingHostInstanceType :: DescribeBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {pendingHostInstanceType = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsPendingHostInstanceType "Use generic-lens or generic-optics with 'pendingHostInstanceType' instead." #-}

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /Note:/ Consider using 'ldapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsLdapServerMetadata :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe LdapServerMetadataOutput)
dbrsLdapServerMetadata = Lens.lens (ldapServerMetadata :: DescribeBrokerResponse -> Lude.Maybe LdapServerMetadataOutput) (\s a -> s {ldapServerMetadata = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsLdapServerMetadata "Use generic-lens or generic-optics with 'ldapServerMetadata' instead." #-}

-- | The parameters that determine the WeeklyStartTime.
--
-- /Note:/ Consider using 'maintenanceWindowStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsMaintenanceWindowStartTime :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe WeeklyStartTime)
dbrsMaintenanceWindowStartTime = Lens.lens (maintenanceWindowStartTime :: DescribeBrokerResponse -> Lude.Maybe WeeklyStartTime) (\s a -> s {maintenanceWindowStartTime = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsMaintenanceWindowStartTime "Use generic-lens or generic-optics with 'maintenanceWindowStartTime' instead." #-}

-- | The list of information about logs currently enabled and pending to be deployed for the specified broker.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsLogs :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe LogsSummary)
dbrsLogs = Lens.lens (logs :: DescribeBrokerResponse -> Lude.Maybe LogsSummary) (\s a -> s {logs = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsLogs "Use generic-lens or generic-optics with 'logs' instead." #-}

-- | Encryption options for the broker.
--
-- /Note:/ Consider using 'encryptionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsEncryptionOptions :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe EncryptionOptions)
dbrsEncryptionOptions = Lens.lens (encryptionOptions :: DescribeBrokerResponse -> Lude.Maybe EncryptionOptions) (\s a -> s {encryptionOptions = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsEncryptionOptions "Use generic-lens or generic-optics with 'encryptionOptions' instead." #-}

-- | Required. The deployment mode of the broker.
--
-- /Note:/ Consider using 'deploymentMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsDeploymentMode :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe DeploymentMode)
dbrsDeploymentMode = Lens.lens (deploymentMode :: DescribeBrokerResponse -> Lude.Maybe DeploymentMode) (\s a -> s {deploymentMode = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsDeploymentMode "Use generic-lens or generic-optics with 'deploymentMode' instead." #-}

-- | The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'pendingEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsPendingEngineVersion :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Lude.Text)
dbrsPendingEngineVersion = Lens.lens (pendingEngineVersion :: DescribeBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {pendingEngineVersion = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsPendingEngineVersion "Use generic-lens or generic-optics with 'pendingEngineVersion' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsBrokerId :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Lude.Text)
dbrsBrokerId = Lens.lens (brokerId :: DescribeBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {brokerId = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The metadata of the LDAP server that will be used to authenticate and authorize connections to the broker once it is rebooted.
--
-- /Note:/ Consider using 'pendingLdapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsPendingLdapServerMetadata :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe LdapServerMetadataOutput)
dbrsPendingLdapServerMetadata = Lens.lens (pendingLdapServerMetadata :: DescribeBrokerResponse -> Lude.Maybe LdapServerMetadataOutput) (\s a -> s {pendingLdapServerMetadata = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsPendingLdapServerMetadata "Use generic-lens or generic-optics with 'pendingLdapServerMetadata' instead." #-}

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsEngineType :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe EngineType)
dbrsEngineType = Lens.lens (engineType :: DescribeBrokerResponse -> Lude.Maybe EngineType) (\s a -> s {engineType = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /Note:/ Consider using 'brokerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsBrokerARN :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Lude.Text)
dbrsBrokerARN = Lens.lens (brokerARN :: DescribeBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {brokerARN = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsBrokerARN "Use generic-lens or generic-optics with 'brokerARN' instead." #-}

-- | The list of all tags associated with this broker.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsTags :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dbrsTags = Lens.lens (tags :: DescribeBrokerResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of information about allocated brokers.
--
-- /Note:/ Consider using 'brokerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsBrokerInstances :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe [BrokerInstance])
dbrsBrokerInstances = Lens.lens (brokerInstances :: DescribeBrokerResponse -> Lude.Maybe [BrokerInstance]) (\s a -> s {brokerInstances = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsBrokerInstances "Use generic-lens or generic-optics with 'brokerInstances' instead." #-}

-- | The broker's instance type.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsHostInstanceType :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe Lude.Text)
dbrsHostInstanceType = Lens.lens (hostInstanceType :: DescribeBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {hostInstanceType = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

-- | The broker's storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsStorageType :: Lens.Lens' DescribeBrokerResponse (Lude.Maybe BrokerStorageType)
dbrsStorageType = Lens.lens (storageType :: DescribeBrokerResponse -> Lude.Maybe BrokerStorageType) (\s a -> s {storageType = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsResponseStatus :: Lens.Lens' DescribeBrokerResponse Lude.Int
dbrsResponseStatus = Lens.lens (responseStatus :: DescribeBrokerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBrokerResponse)
{-# DEPRECATED dbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
