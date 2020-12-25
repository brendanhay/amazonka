{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.UpdateBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a pending configuration change to a broker.
module Network.AWS.MQ.UpdateBroker
  ( -- * Creating a request
    UpdateBroker (..),
    mkUpdateBroker,

    -- ** Request lenses
    ubBrokerId,
    ubAuthenticationStrategy,
    ubAutoMinorVersionUpgrade,
    ubConfiguration,
    ubEngineVersion,
    ubHostInstanceType,
    ubLdapServerMetadata,
    ubLogs,
    ubSecurityGroups,

    -- * Destructuring the response
    UpdateBrokerResponse (..),
    mkUpdateBrokerResponse,

    -- ** Response lenses
    ubrrsAuthenticationStrategy,
    ubrrsAutoMinorVersionUpgrade,
    ubrrsBrokerId,
    ubrrsConfiguration,
    ubrrsEngineVersion,
    ubrrsHostInstanceType,
    ubrrsLdapServerMetadata,
    ubrrsLogs,
    ubrrsSecurityGroups,
    ubrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates the broker using the specified properties.
--
-- /See:/ 'mkUpdateBroker' smart constructor.
data UpdateBroker = UpdateBroker'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text,
    -- | The authentication strategy used to secure the broker.
    authenticationStrategy :: Core.Maybe Types.AuthenticationStrategy,
    -- | Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | A list of information about the configuration.
    configuration :: Core.Maybe Types.ConfigurationId,
    -- | The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
    engineVersion :: Core.Maybe Core.Text,
    -- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
    hostInstanceType :: Core.Maybe Core.Text,
    -- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
    ldapServerMetadata :: Core.Maybe Types.LdapServerMetadataInput,
    -- | Enables Amazon CloudWatch logging for brokers.
    logs :: Core.Maybe Types.Logs,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
    securityGroups :: Core.Maybe [Core.Text]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBroker' value with any optional fields omitted.
mkUpdateBroker ::
  -- | 'brokerId'
  Core.Text ->
  UpdateBroker
mkUpdateBroker brokerId =
  UpdateBroker'
    { brokerId,
      authenticationStrategy = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      configuration = Core.Nothing,
      engineVersion = Core.Nothing,
      hostInstanceType = Core.Nothing,
      ldapServerMetadata = Core.Nothing,
      logs = Core.Nothing,
      securityGroups = Core.Nothing
    }

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubBrokerId :: Lens.Lens' UpdateBroker Core.Text
ubBrokerId = Lens.field @"brokerId"
{-# DEPRECATED ubBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The authentication strategy used to secure the broker.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubAuthenticationStrategy :: Lens.Lens' UpdateBroker (Core.Maybe Types.AuthenticationStrategy)
ubAuthenticationStrategy = Lens.field @"authenticationStrategy"
{-# DEPRECATED ubAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubAutoMinorVersionUpgrade :: Lens.Lens' UpdateBroker (Core.Maybe Core.Bool)
ubAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED ubAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | A list of information about the configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubConfiguration :: Lens.Lens' UpdateBroker (Core.Maybe Types.ConfigurationId)
ubConfiguration = Lens.field @"configuration"
{-# DEPRECATED ubConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubEngineVersion :: Lens.Lens' UpdateBroker (Core.Maybe Core.Text)
ubEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED ubEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubHostInstanceType :: Lens.Lens' UpdateBroker (Core.Maybe Core.Text)
ubHostInstanceType = Lens.field @"hostInstanceType"
{-# DEPRECATED ubHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /Note:/ Consider using 'ldapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubLdapServerMetadata :: Lens.Lens' UpdateBroker (Core.Maybe Types.LdapServerMetadataInput)
ubLdapServerMetadata = Lens.field @"ldapServerMetadata"
{-# DEPRECATED ubLdapServerMetadata "Use generic-lens or generic-optics with 'ldapServerMetadata' instead." #-}

-- | Enables Amazon CloudWatch logging for brokers.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubLogs :: Lens.Lens' UpdateBroker (Core.Maybe Types.Logs)
ubLogs = Lens.field @"logs"
{-# DEPRECATED ubLogs "Use generic-lens or generic-optics with 'logs' instead." #-}

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubSecurityGroups :: Lens.Lens' UpdateBroker (Core.Maybe [Core.Text])
ubSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED ubSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

instance Core.FromJSON UpdateBroker where
  toJSON UpdateBroker {..} =
    Core.object
      ( Core.catMaybes
          [ ("authenticationStrategy" Core..=)
              Core.<$> authenticationStrategy,
            ("autoMinorVersionUpgrade" Core..=)
              Core.<$> autoMinorVersionUpgrade,
            ("configuration" Core..=) Core.<$> configuration,
            ("engineVersion" Core..=) Core.<$> engineVersion,
            ("hostInstanceType" Core..=) Core.<$> hostInstanceType,
            ("ldapServerMetadata" Core..=) Core.<$> ldapServerMetadata,
            ("logs" Core..=) Core.<$> logs,
            ("securityGroups" Core..=) Core.<$> securityGroups
          ]
      )

instance Core.AWSRequest UpdateBroker where
  type Rs UpdateBroker = UpdateBrokerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/v1/brokers/" Core.<> (Core.toText brokerId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBrokerResponse'
            Core.<$> (x Core..:? "authenticationStrategy")
            Core.<*> (x Core..:? "autoMinorVersionUpgrade")
            Core.<*> (x Core..:? "brokerId")
            Core.<*> (x Core..:? "configuration")
            Core.<*> (x Core..:? "engineVersion")
            Core.<*> (x Core..:? "hostInstanceType")
            Core.<*> (x Core..:? "ldapServerMetadata")
            Core.<*> (x Core..:? "logs")
            Core.<*> (x Core..:? "securityGroups")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateBrokerResponse' smart constructor.
data UpdateBrokerResponse = UpdateBrokerResponse'
  { -- | The authentication strategy used to secure the broker.
    authenticationStrategy :: Core.Maybe Types.AuthenticationStrategy,
    -- | The new value of automatic upgrades to new minor version for brokers.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Maybe Core.Text,
    -- | The ID of the updated configuration.
    configuration :: Core.Maybe Types.ConfigurationId,
    -- | The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
    engineVersion :: Core.Maybe Core.Text,
    -- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
    hostInstanceType :: Core.Maybe Core.Text,
    -- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
    ldapServerMetadata :: Core.Maybe Types.LdapServerMetadataOutput,
    -- | The list of information about logs to be enabled for the specified broker.
    logs :: Core.Maybe Types.Logs,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
    securityGroups :: Core.Maybe [Core.Text],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBrokerResponse' value with any optional fields omitted.
mkUpdateBrokerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateBrokerResponse
mkUpdateBrokerResponse responseStatus =
  UpdateBrokerResponse'
    { authenticationStrategy = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      brokerId = Core.Nothing,
      configuration = Core.Nothing,
      engineVersion = Core.Nothing,
      hostInstanceType = Core.Nothing,
      ldapServerMetadata = Core.Nothing,
      logs = Core.Nothing,
      securityGroups = Core.Nothing,
      responseStatus
    }

-- | The authentication strategy used to secure the broker.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsAuthenticationStrategy :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Types.AuthenticationStrategy)
ubrrsAuthenticationStrategy = Lens.field @"authenticationStrategy"
{-# DEPRECATED ubrrsAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | The new value of automatic upgrades to new minor version for brokers.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsAutoMinorVersionUpgrade :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Bool)
ubrrsAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED ubrrsAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | Required. The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsBrokerId :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Text)
ubrrsBrokerId = Lens.field @"brokerId"
{-# DEPRECATED ubrrsBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The ID of the updated configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsConfiguration :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Types.ConfigurationId)
ubrrsConfiguration = Lens.field @"configuration"
{-# DEPRECATED ubrrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsEngineVersion :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Text)
ubrrsEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED ubrrsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsHostInstanceType :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Text)
ubrrsHostInstanceType = Lens.field @"hostInstanceType"
{-# DEPRECATED ubrrsHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /Note:/ Consider using 'ldapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsLdapServerMetadata :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Types.LdapServerMetadataOutput)
ubrrsLdapServerMetadata = Lens.field @"ldapServerMetadata"
{-# DEPRECATED ubrrsLdapServerMetadata "Use generic-lens or generic-optics with 'ldapServerMetadata' instead." #-}

-- | The list of information about logs to be enabled for the specified broker.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsLogs :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Types.Logs)
ubrrsLogs = Lens.field @"logs"
{-# DEPRECATED ubrrsLogs "Use generic-lens or generic-optics with 'logs' instead." #-}

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsSecurityGroups :: Lens.Lens' UpdateBrokerResponse (Core.Maybe [Core.Text])
ubrrsSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED ubrrsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsResponseStatus :: Lens.Lens' UpdateBrokerResponse Core.Int
ubrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ubrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
