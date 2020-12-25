{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cbAuthenticationStrategy,
    cbAutoMinorVersionUpgrade,
    cbBrokerName,
    cbConfiguration,
    cbCreatorRequestId,
    cbDeploymentMode,
    cbEncryptionOptions,
    cbEngineType,
    cbEngineVersion,
    cbHostInstanceType,
    cbLdapServerMetadata,
    cbLogs,
    cbMaintenanceWindowStartTime,
    cbPubliclyAccessible,
    cbSecurityGroups,
    cbStorageType,
    cbSubnetIds,
    cbTags,
    cbUsers,

    -- * Destructuring the response
    CreateBrokerResponse (..),
    mkCreateBrokerResponse,

    -- ** Response lenses
    cbrrsBrokerArn,
    cbrrsBrokerId,
    cbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a broker using the specified properties.
--
-- /See:/ 'mkCreateBroker' smart constructor.
data CreateBroker = CreateBroker'
  { -- | The authentication strategy used to secure the broker.
    authenticationStrategy :: Core.Maybe Types.AuthenticationStrategy,
    -- | Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | Required. The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
    brokerName :: Core.Maybe Core.Text,
    -- | A list of information about the configuration.
    configuration :: Core.Maybe Types.ConfigurationId,
    -- | The unique ID that the requester receives for the created broker. Amazon MQ passes your ID with the API action. Note: We recommend using a Universally Unique Identifier (UUID) for the creatorRequestId. You may omit the creatorRequestId if your application doesn't require idempotency.
    creatorRequestId :: Core.Maybe Core.Text,
    -- | Required. The deployment mode of the broker.
    deploymentMode :: Core.Maybe Types.DeploymentMode,
    -- | Encryption options for the broker.
    encryptionOptions :: Core.Maybe Types.EncryptionOptions,
    -- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
    engineType :: Core.Maybe Types.EngineType,
    -- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
    engineVersion :: Core.Maybe Core.Text,
    -- | Required. The broker's instance type.
    hostInstanceType :: Core.Maybe Core.Text,
    -- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
    ldapServerMetadata :: Core.Maybe Types.LdapServerMetadataInput,
    -- | Enables Amazon CloudWatch logging for brokers.
    logs :: Core.Maybe Types.Logs,
    -- | The parameters that determine the WeeklyStartTime.
    maintenanceWindowStartTime :: Core.Maybe Types.WeeklyStartTime,
    -- | Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
    securityGroups :: Core.Maybe [Core.Text],
    -- | The broker's storage type.
    storageType :: Core.Maybe Types.BrokerStorageType,
    -- | The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
    subnetIds :: Core.Maybe [Core.Text],
    -- | Create tags when creating the broker.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Required. The list of broker users (persons or applications) who can access queues and topics. For RabbitMQ brokers, one and only one administrative user is accepted and created when a broker is first provisioned. All subsequent broker users are created by making RabbitMQ API calls directly to brokers or via the RabbitMQ Web Console. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    users :: Core.Maybe [Types.User]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBroker' value with any optional fields omitted.
mkCreateBroker ::
  CreateBroker
mkCreateBroker =
  CreateBroker'
    { authenticationStrategy = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      brokerName = Core.Nothing,
      configuration = Core.Nothing,
      creatorRequestId = Core.Nothing,
      deploymentMode = Core.Nothing,
      encryptionOptions = Core.Nothing,
      engineType = Core.Nothing,
      engineVersion = Core.Nothing,
      hostInstanceType = Core.Nothing,
      ldapServerMetadata = Core.Nothing,
      logs = Core.Nothing,
      maintenanceWindowStartTime = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      securityGroups = Core.Nothing,
      storageType = Core.Nothing,
      subnetIds = Core.Nothing,
      tags = Core.Nothing,
      users = Core.Nothing
    }

-- | The authentication strategy used to secure the broker.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbAuthenticationStrategy :: Lens.Lens' CreateBroker (Core.Maybe Types.AuthenticationStrategy)
cbAuthenticationStrategy = Lens.field @"authenticationStrategy"
{-# DEPRECATED cbAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbAutoMinorVersionUpgrade :: Lens.Lens' CreateBroker (Core.Maybe Core.Bool)
cbAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED cbAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | Required. The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- /Note:/ Consider using 'brokerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBrokerName :: Lens.Lens' CreateBroker (Core.Maybe Core.Text)
cbBrokerName = Lens.field @"brokerName"
{-# DEPRECATED cbBrokerName "Use generic-lens or generic-optics with 'brokerName' instead." #-}

-- | A list of information about the configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbConfiguration :: Lens.Lens' CreateBroker (Core.Maybe Types.ConfigurationId)
cbConfiguration = Lens.field @"configuration"
{-# DEPRECATED cbConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The unique ID that the requester receives for the created broker. Amazon MQ passes your ID with the API action. Note: We recommend using a Universally Unique Identifier (UUID) for the creatorRequestId. You may omit the creatorRequestId if your application doesn't require idempotency.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCreatorRequestId :: Lens.Lens' CreateBroker (Core.Maybe Core.Text)
cbCreatorRequestId = Lens.field @"creatorRequestId"
{-# DEPRECATED cbCreatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead." #-}

-- | Required. The deployment mode of the broker.
--
-- /Note:/ Consider using 'deploymentMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbDeploymentMode :: Lens.Lens' CreateBroker (Core.Maybe Types.DeploymentMode)
cbDeploymentMode = Lens.field @"deploymentMode"
{-# DEPRECATED cbDeploymentMode "Use generic-lens or generic-optics with 'deploymentMode' instead." #-}

-- | Encryption options for the broker.
--
-- /Note:/ Consider using 'encryptionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbEncryptionOptions :: Lens.Lens' CreateBroker (Core.Maybe Types.EncryptionOptions)
cbEncryptionOptions = Lens.field @"encryptionOptions"
{-# DEPRECATED cbEncryptionOptions "Use generic-lens or generic-optics with 'encryptionOptions' instead." #-}

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbEngineType :: Lens.Lens' CreateBroker (Core.Maybe Types.EngineType)
cbEngineType = Lens.field @"engineType"
{-# DEPRECATED cbEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbEngineVersion :: Lens.Lens' CreateBroker (Core.Maybe Core.Text)
cbEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED cbEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Required. The broker's instance type.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbHostInstanceType :: Lens.Lens' CreateBroker (Core.Maybe Core.Text)
cbHostInstanceType = Lens.field @"hostInstanceType"
{-# DEPRECATED cbHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /Note:/ Consider using 'ldapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbLdapServerMetadata :: Lens.Lens' CreateBroker (Core.Maybe Types.LdapServerMetadataInput)
cbLdapServerMetadata = Lens.field @"ldapServerMetadata"
{-# DEPRECATED cbLdapServerMetadata "Use generic-lens or generic-optics with 'ldapServerMetadata' instead." #-}

-- | Enables Amazon CloudWatch logging for brokers.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbLogs :: Lens.Lens' CreateBroker (Core.Maybe Types.Logs)
cbLogs = Lens.field @"logs"
{-# DEPRECATED cbLogs "Use generic-lens or generic-optics with 'logs' instead." #-}

-- | The parameters that determine the WeeklyStartTime.
--
-- /Note:/ Consider using 'maintenanceWindowStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbMaintenanceWindowStartTime :: Lens.Lens' CreateBroker (Core.Maybe Types.WeeklyStartTime)
cbMaintenanceWindowStartTime = Lens.field @"maintenanceWindowStartTime"
{-# DEPRECATED cbMaintenanceWindowStartTime "Use generic-lens or generic-optics with 'maintenanceWindowStartTime' instead." #-}

-- | Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbPubliclyAccessible :: Lens.Lens' CreateBroker (Core.Maybe Core.Bool)
cbPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# DEPRECATED cbPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbSecurityGroups :: Lens.Lens' CreateBroker (Core.Maybe [Core.Text])
cbSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED cbSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The broker's storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbStorageType :: Lens.Lens' CreateBroker (Core.Maybe Types.BrokerStorageType)
cbStorageType = Lens.field @"storageType"
{-# DEPRECATED cbStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbSubnetIds :: Lens.Lens' CreateBroker (Core.Maybe [Core.Text])
cbSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED cbSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | Create tags when creating the broker.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTags :: Lens.Lens' CreateBroker (Core.Maybe (Core.HashMap Core.Text Core.Text))
cbTags = Lens.field @"tags"
{-# DEPRECATED cbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Required. The list of broker users (persons or applications) who can access queues and topics. For RabbitMQ brokers, one and only one administrative user is accepted and created when a broker is first provisioned. All subsequent broker users are created by making RabbitMQ API calls directly to brokers or via the RabbitMQ Web Console. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbUsers :: Lens.Lens' CreateBroker (Core.Maybe [Types.User])
cbUsers = Lens.field @"users"
{-# DEPRECATED cbUsers "Use generic-lens or generic-optics with 'users' instead." #-}

instance Core.FromJSON CreateBroker where
  toJSON CreateBroker {..} =
    Core.object
      ( Core.catMaybes
          [ ("authenticationStrategy" Core..=)
              Core.<$> authenticationStrategy,
            ("autoMinorVersionUpgrade" Core..=)
              Core.<$> autoMinorVersionUpgrade,
            ("brokerName" Core..=) Core.<$> brokerName,
            ("configuration" Core..=) Core.<$> configuration,
            ("creatorRequestId" Core..=) Core.<$> creatorRequestId,
            ("deploymentMode" Core..=) Core.<$> deploymentMode,
            ("encryptionOptions" Core..=) Core.<$> encryptionOptions,
            ("engineType" Core..=) Core.<$> engineType,
            ("engineVersion" Core..=) Core.<$> engineVersion,
            ("hostInstanceType" Core..=) Core.<$> hostInstanceType,
            ("ldapServerMetadata" Core..=) Core.<$> ldapServerMetadata,
            ("logs" Core..=) Core.<$> logs,
            ("maintenanceWindowStartTime" Core..=)
              Core.<$> maintenanceWindowStartTime,
            ("publiclyAccessible" Core..=) Core.<$> publiclyAccessible,
            ("securityGroups" Core..=) Core.<$> securityGroups,
            ("storageType" Core..=) Core.<$> storageType,
            ("subnetIds" Core..=) Core.<$> subnetIds,
            ("tags" Core..=) Core.<$> tags,
            ("users" Core..=) Core.<$> users
          ]
      )

instance Core.AWSRequest CreateBroker where
  type Rs CreateBroker = CreateBrokerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/brokers",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBrokerResponse'
            Core.<$> (x Core..:? "brokerArn")
            Core.<*> (x Core..:? "brokerId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateBrokerResponse' smart constructor.
data CreateBrokerResponse = CreateBrokerResponse'
  { -- | The Amazon Resource Name (ARN) of the broker.
    brokerArn :: Core.Maybe Core.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBrokerResponse' value with any optional fields omitted.
mkCreateBrokerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateBrokerResponse
mkCreateBrokerResponse responseStatus =
  CreateBrokerResponse'
    { brokerArn = Core.Nothing,
      brokerId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /Note:/ Consider using 'brokerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsBrokerArn :: Lens.Lens' CreateBrokerResponse (Core.Maybe Core.Text)
cbrrsBrokerArn = Lens.field @"brokerArn"
{-# DEPRECATED cbrrsBrokerArn "Use generic-lens or generic-optics with 'brokerArn' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsBrokerId :: Lens.Lens' CreateBrokerResponse (Core.Maybe Core.Text)
cbrrsBrokerId = Lens.field @"brokerId"
{-# DEPRECATED cbrrsBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsResponseStatus :: Lens.Lens' CreateBrokerResponse Core.Int
cbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
