{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.CreateBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a broker. Note: This API is asynchronous.
module Network.AWS.MQ.CreateBroker
  ( -- * Creating a request
    CreateBroker (..),
    mkCreateBroker,

    -- ** Request lenses
    cbBrokerName,
    cbEngineVersion,
    cbPubliclyAccessible,
    cbAutoMinorVersionUpgrade,
    cbSecurityGroups,
    cbUsers,
    cbSubnetIds,
    cbCreatorRequestId,
    cbAuthenticationStrategy,
    cbLdapServerMetadata,
    cbMaintenanceWindowStartTime,
    cbLogs,
    cbEncryptionOptions,
    cbDeploymentMode,
    cbConfiguration,
    cbEngineType,
    cbTags,
    cbHostInstanceType,
    cbStorageType,

    -- * Destructuring the response
    CreateBrokerResponse (..),
    mkCreateBrokerResponse,

    -- ** Response lenses
    cbrsBrokerId,
    cbrsBrokerARN,
    cbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Creates a broker using the specified properties.
--
-- /See:/ 'mkCreateBroker' smart constructor.
data CreateBroker = CreateBroker'
  { brokerName ::
      Lude.Maybe Lude.Text,
    engineVersion :: Lude.Maybe Lude.Text,
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    securityGroups :: Lude.Maybe [Lude.Text],
    users :: Lude.Maybe [User],
    subnetIds :: Lude.Maybe [Lude.Text],
    creatorRequestId :: Lude.Maybe Lude.Text,
    authenticationStrategy :: Lude.Maybe AuthenticationStrategy,
    ldapServerMetadata :: Lude.Maybe LdapServerMetadataInput,
    maintenanceWindowStartTime :: Lude.Maybe WeeklyStartTime,
    logs :: Lude.Maybe Logs,
    encryptionOptions :: Lude.Maybe EncryptionOptions,
    deploymentMode :: Lude.Maybe DeploymentMode,
    configuration :: Lude.Maybe ConfigurationId,
    engineType :: Lude.Maybe EngineType,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    hostInstanceType :: Lude.Maybe Lude.Text,
    storageType :: Lude.Maybe BrokerStorageType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBroker' with the minimum fields required to make a request.
--
-- * 'authenticationStrategy' - The authentication strategy used to secure the broker.
-- * 'autoMinorVersionUpgrade' - Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
-- * 'brokerName' - Required. The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
-- * 'configuration' - A list of information about the configuration.
-- * 'creatorRequestId' - The unique ID that the requester receives for the created broker. Amazon MQ passes your ID with the API action. Note: We recommend using a Universally Unique Identifier (UUID) for the creatorRequestId. You may omit the creatorRequestId if your application doesn't require idempotency.
-- * 'deploymentMode' - Required. The deployment mode of the broker.
-- * 'encryptionOptions' - Encryption options for the broker.
-- * 'engineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
-- * 'engineVersion' - Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
-- * 'hostInstanceType' - Required. The broker's instance type.
-- * 'ldapServerMetadata' - The metadata of the LDAP server used to authenticate and authorize connections to the broker.
-- * 'logs' - Enables Amazon CloudWatch logging for brokers.
-- * 'maintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
-- * 'publiclyAccessible' - Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
-- * 'securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
-- * 'storageType' - The broker's storage type.
-- * 'subnetIds' - The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
-- * 'tags' - Create tags when creating the broker.
-- * 'users' - Required. The list of broker users (persons or applications) who can access queues and topics. For RabbitMQ brokers, one and only one administrative user is accepted and created when a broker is first provisioned. All subsequent broker users are created by making RabbitMQ API calls directly to brokers or via the RabbitMQ Web Console. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
mkCreateBroker ::
  CreateBroker
mkCreateBroker =
  CreateBroker'
    { brokerName = Lude.Nothing,
      engineVersion = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      securityGroups = Lude.Nothing,
      users = Lude.Nothing,
      subnetIds = Lude.Nothing,
      creatorRequestId = Lude.Nothing,
      authenticationStrategy = Lude.Nothing,
      ldapServerMetadata = Lude.Nothing,
      maintenanceWindowStartTime = Lude.Nothing,
      logs = Lude.Nothing,
      encryptionOptions = Lude.Nothing,
      deploymentMode = Lude.Nothing,
      configuration = Lude.Nothing,
      engineType = Lude.Nothing,
      tags = Lude.Nothing,
      hostInstanceType = Lude.Nothing,
      storageType = Lude.Nothing
    }

-- | Required. The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- /Note:/ Consider using 'brokerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBrokerName :: Lens.Lens' CreateBroker (Lude.Maybe Lude.Text)
cbBrokerName = Lens.lens (brokerName :: CreateBroker -> Lude.Maybe Lude.Text) (\s a -> s {brokerName = a} :: CreateBroker)
{-# DEPRECATED cbBrokerName "Use generic-lens or generic-optics with 'brokerName' instead." #-}

-- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbEngineVersion :: Lens.Lens' CreateBroker (Lude.Maybe Lude.Text)
cbEngineVersion = Lens.lens (engineVersion :: CreateBroker -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CreateBroker)
{-# DEPRECATED cbEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbPubliclyAccessible :: Lens.Lens' CreateBroker (Lude.Maybe Lude.Bool)
cbPubliclyAccessible = Lens.lens (publiclyAccessible :: CreateBroker -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: CreateBroker)
{-# DEPRECATED cbPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbAutoMinorVersionUpgrade :: Lens.Lens' CreateBroker (Lude.Maybe Lude.Bool)
cbAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: CreateBroker -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: CreateBroker)
{-# DEPRECATED cbAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbSecurityGroups :: Lens.Lens' CreateBroker (Lude.Maybe [Lude.Text])
cbSecurityGroups = Lens.lens (securityGroups :: CreateBroker -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: CreateBroker)
{-# DEPRECATED cbSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | Required. The list of broker users (persons or applications) who can access queues and topics. For RabbitMQ brokers, one and only one administrative user is accepted and created when a broker is first provisioned. All subsequent broker users are created by making RabbitMQ API calls directly to brokers or via the RabbitMQ Web Console. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbUsers :: Lens.Lens' CreateBroker (Lude.Maybe [User])
cbUsers = Lens.lens (users :: CreateBroker -> Lude.Maybe [User]) (\s a -> s {users = a} :: CreateBroker)
{-# DEPRECATED cbUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbSubnetIds :: Lens.Lens' CreateBroker (Lude.Maybe [Lude.Text])
cbSubnetIds = Lens.lens (subnetIds :: CreateBroker -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: CreateBroker)
{-# DEPRECATED cbSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The unique ID that the requester receives for the created broker. Amazon MQ passes your ID with the API action. Note: We recommend using a Universally Unique Identifier (UUID) for the creatorRequestId. You may omit the creatorRequestId if your application doesn't require idempotency.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCreatorRequestId :: Lens.Lens' CreateBroker (Lude.Maybe Lude.Text)
cbCreatorRequestId = Lens.lens (creatorRequestId :: CreateBroker -> Lude.Maybe Lude.Text) (\s a -> s {creatorRequestId = a} :: CreateBroker)
{-# DEPRECATED cbCreatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead." #-}

-- | The authentication strategy used to secure the broker.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbAuthenticationStrategy :: Lens.Lens' CreateBroker (Lude.Maybe AuthenticationStrategy)
cbAuthenticationStrategy = Lens.lens (authenticationStrategy :: CreateBroker -> Lude.Maybe AuthenticationStrategy) (\s a -> s {authenticationStrategy = a} :: CreateBroker)
{-# DEPRECATED cbAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /Note:/ Consider using 'ldapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbLdapServerMetadata :: Lens.Lens' CreateBroker (Lude.Maybe LdapServerMetadataInput)
cbLdapServerMetadata = Lens.lens (ldapServerMetadata :: CreateBroker -> Lude.Maybe LdapServerMetadataInput) (\s a -> s {ldapServerMetadata = a} :: CreateBroker)
{-# DEPRECATED cbLdapServerMetadata "Use generic-lens or generic-optics with 'ldapServerMetadata' instead." #-}

-- | The parameters that determine the WeeklyStartTime.
--
-- /Note:/ Consider using 'maintenanceWindowStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbMaintenanceWindowStartTime :: Lens.Lens' CreateBroker (Lude.Maybe WeeklyStartTime)
cbMaintenanceWindowStartTime = Lens.lens (maintenanceWindowStartTime :: CreateBroker -> Lude.Maybe WeeklyStartTime) (\s a -> s {maintenanceWindowStartTime = a} :: CreateBroker)
{-# DEPRECATED cbMaintenanceWindowStartTime "Use generic-lens or generic-optics with 'maintenanceWindowStartTime' instead." #-}

-- | Enables Amazon CloudWatch logging for brokers.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbLogs :: Lens.Lens' CreateBroker (Lude.Maybe Logs)
cbLogs = Lens.lens (logs :: CreateBroker -> Lude.Maybe Logs) (\s a -> s {logs = a} :: CreateBroker)
{-# DEPRECATED cbLogs "Use generic-lens or generic-optics with 'logs' instead." #-}

-- | Encryption options for the broker.
--
-- /Note:/ Consider using 'encryptionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbEncryptionOptions :: Lens.Lens' CreateBroker (Lude.Maybe EncryptionOptions)
cbEncryptionOptions = Lens.lens (encryptionOptions :: CreateBroker -> Lude.Maybe EncryptionOptions) (\s a -> s {encryptionOptions = a} :: CreateBroker)
{-# DEPRECATED cbEncryptionOptions "Use generic-lens or generic-optics with 'encryptionOptions' instead." #-}

-- | Required. The deployment mode of the broker.
--
-- /Note:/ Consider using 'deploymentMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbDeploymentMode :: Lens.Lens' CreateBroker (Lude.Maybe DeploymentMode)
cbDeploymentMode = Lens.lens (deploymentMode :: CreateBroker -> Lude.Maybe DeploymentMode) (\s a -> s {deploymentMode = a} :: CreateBroker)
{-# DEPRECATED cbDeploymentMode "Use generic-lens or generic-optics with 'deploymentMode' instead." #-}

-- | A list of information about the configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbConfiguration :: Lens.Lens' CreateBroker (Lude.Maybe ConfigurationId)
cbConfiguration = Lens.lens (configuration :: CreateBroker -> Lude.Maybe ConfigurationId) (\s a -> s {configuration = a} :: CreateBroker)
{-# DEPRECATED cbConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbEngineType :: Lens.Lens' CreateBroker (Lude.Maybe EngineType)
cbEngineType = Lens.lens (engineType :: CreateBroker -> Lude.Maybe EngineType) (\s a -> s {engineType = a} :: CreateBroker)
{-# DEPRECATED cbEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | Create tags when creating the broker.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTags :: Lens.Lens' CreateBroker (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cbTags = Lens.lens (tags :: CreateBroker -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateBroker)
{-# DEPRECATED cbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Required. The broker's instance type.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbHostInstanceType :: Lens.Lens' CreateBroker (Lude.Maybe Lude.Text)
cbHostInstanceType = Lens.lens (hostInstanceType :: CreateBroker -> Lude.Maybe Lude.Text) (\s a -> s {hostInstanceType = a} :: CreateBroker)
{-# DEPRECATED cbHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

-- | The broker's storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbStorageType :: Lens.Lens' CreateBroker (Lude.Maybe BrokerStorageType)
cbStorageType = Lens.lens (storageType :: CreateBroker -> Lude.Maybe BrokerStorageType) (\s a -> s {storageType = a} :: CreateBroker)
{-# DEPRECATED cbStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Lude.AWSRequest CreateBroker where
  type Rs CreateBroker = CreateBrokerResponse
  request = Req.postJSON mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateBrokerResponse'
            Lude.<$> (x Lude..?> "brokerId")
            Lude.<*> (x Lude..?> "brokerArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateBroker where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBroker where
  toJSON CreateBroker' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("brokerName" Lude..=) Lude.<$> brokerName,
            ("engineVersion" Lude..=) Lude.<$> engineVersion,
            ("publiclyAccessible" Lude..=) Lude.<$> publiclyAccessible,
            ("autoMinorVersionUpgrade" Lude..=)
              Lude.<$> autoMinorVersionUpgrade,
            ("securityGroups" Lude..=) Lude.<$> securityGroups,
            ("users" Lude..=) Lude.<$> users,
            ("subnetIds" Lude..=) Lude.<$> subnetIds,
            ("creatorRequestId" Lude..=) Lude.<$> creatorRequestId,
            ("authenticationStrategy" Lude..=) Lude.<$> authenticationStrategy,
            ("ldapServerMetadata" Lude..=) Lude.<$> ldapServerMetadata,
            ("maintenanceWindowStartTime" Lude..=)
              Lude.<$> maintenanceWindowStartTime,
            ("logs" Lude..=) Lude.<$> logs,
            ("encryptionOptions" Lude..=) Lude.<$> encryptionOptions,
            ("deploymentMode" Lude..=) Lude.<$> deploymentMode,
            ("configuration" Lude..=) Lude.<$> configuration,
            ("engineType" Lude..=) Lude.<$> engineType,
            ("tags" Lude..=) Lude.<$> tags,
            ("hostInstanceType" Lude..=) Lude.<$> hostInstanceType,
            ("storageType" Lude..=) Lude.<$> storageType
          ]
      )

instance Lude.ToPath CreateBroker where
  toPath = Lude.const "/v1/brokers"

instance Lude.ToQuery CreateBroker where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateBrokerResponse' smart constructor.
data CreateBrokerResponse = CreateBrokerResponse'
  { brokerId ::
      Lude.Maybe Lude.Text,
    brokerARN :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBrokerResponse' with the minimum fields required to make a request.
--
-- * 'brokerARN' - The Amazon Resource Name (ARN) of the broker.
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
-- * 'responseStatus' - The response status code.
mkCreateBrokerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBrokerResponse
mkCreateBrokerResponse pResponseStatus_ =
  CreateBrokerResponse'
    { brokerId = Lude.Nothing,
      brokerARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsBrokerId :: Lens.Lens' CreateBrokerResponse (Lude.Maybe Lude.Text)
cbrsBrokerId = Lens.lens (brokerId :: CreateBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {brokerId = a} :: CreateBrokerResponse)
{-# DEPRECATED cbrsBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /Note:/ Consider using 'brokerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsBrokerARN :: Lens.Lens' CreateBrokerResponse (Lude.Maybe Lude.Text)
cbrsBrokerARN = Lens.lens (brokerARN :: CreateBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {brokerARN = a} :: CreateBrokerResponse)
{-# DEPRECATED cbrsBrokerARN "Use generic-lens or generic-optics with 'brokerARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsResponseStatus :: Lens.Lens' CreateBrokerResponse Lude.Int
cbrsResponseStatus = Lens.lens (responseStatus :: CreateBrokerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBrokerResponse)
{-# DEPRECATED cbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
