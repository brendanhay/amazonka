{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateBroker (..)
    , mkUpdateBroker
    -- ** Request lenses
    , ubBrokerId
    , ubAuthenticationStrategy
    , ubAutoMinorVersionUpgrade
    , ubConfiguration
    , ubEngineVersion
    , ubHostInstanceType
    , ubLdapServerMetadata
    , ubLogs
    , ubSecurityGroups

    -- * Destructuring the response
    , UpdateBrokerResponse (..)
    , mkUpdateBrokerResponse
    -- ** Response lenses
    , ubrrsAuthenticationStrategy
    , ubrrsAutoMinorVersionUpgrade
    , ubrrsBrokerId
    , ubrrsConfiguration
    , ubrrsEngineVersion
    , ubrrsHostInstanceType
    , ubrrsLdapServerMetadata
    , ubrrsLogs
    , ubrrsSecurityGroups
    , ubrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates the broker using the specified properties.
--
-- /See:/ 'mkUpdateBroker' smart constructor.
data UpdateBroker = UpdateBroker'
  { brokerId :: Core.Text
    -- ^ The unique ID that Amazon MQ generates for the broker.
  , authenticationStrategy :: Core.Maybe Types.AuthenticationStrategy
    -- ^ The authentication strategy used to secure the broker.
  , autoMinorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
  , configuration :: Core.Maybe Types.ConfigurationId
    -- ^ A list of information about the configuration.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
  , hostInstanceType :: Core.Maybe Core.Text
    -- ^ The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
  , ldapServerMetadata :: Core.Maybe Types.LdapServerMetadataInput
    -- ^ The metadata of the LDAP server used to authenticate and authorize connections to the broker.
  , logs :: Core.Maybe Types.Logs
    -- ^ Enables Amazon CloudWatch logging for brokers.
  , securityGroups :: Core.Maybe [Core.Text]
    -- ^ The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBroker' value with any optional fields omitted.
mkUpdateBroker
    :: Core.Text -- ^ 'brokerId'
    -> UpdateBroker
mkUpdateBroker brokerId
  = UpdateBroker'{brokerId, authenticationStrategy = Core.Nothing,
                  autoMinorVersionUpgrade = Core.Nothing,
                  configuration = Core.Nothing, engineVersion = Core.Nothing,
                  hostInstanceType = Core.Nothing, ldapServerMetadata = Core.Nothing,
                  logs = Core.Nothing, securityGroups = Core.Nothing}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubBrokerId :: Lens.Lens' UpdateBroker Core.Text
ubBrokerId = Lens.field @"brokerId"
{-# INLINEABLE ubBrokerId #-}
{-# DEPRECATED brokerId "Use generic-lens or generic-optics with 'brokerId' instead"  #-}

-- | The authentication strategy used to secure the broker.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubAuthenticationStrategy :: Lens.Lens' UpdateBroker (Core.Maybe Types.AuthenticationStrategy)
ubAuthenticationStrategy = Lens.field @"authenticationStrategy"
{-# INLINEABLE ubAuthenticationStrategy #-}
{-# DEPRECATED authenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead"  #-}

-- | Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubAutoMinorVersionUpgrade :: Lens.Lens' UpdateBroker (Core.Maybe Core.Bool)
ubAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# INLINEABLE ubAutoMinorVersionUpgrade #-}
{-# DEPRECATED autoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead"  #-}

-- | A list of information about the configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubConfiguration :: Lens.Lens' UpdateBroker (Core.Maybe Types.ConfigurationId)
ubConfiguration = Lens.field @"configuration"
{-# INLINEABLE ubConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubEngineVersion :: Lens.Lens' UpdateBroker (Core.Maybe Core.Text)
ubEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE ubEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubHostInstanceType :: Lens.Lens' UpdateBroker (Core.Maybe Core.Text)
ubHostInstanceType = Lens.field @"hostInstanceType"
{-# INLINEABLE ubHostInstanceType #-}
{-# DEPRECATED hostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead"  #-}

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /Note:/ Consider using 'ldapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubLdapServerMetadata :: Lens.Lens' UpdateBroker (Core.Maybe Types.LdapServerMetadataInput)
ubLdapServerMetadata = Lens.field @"ldapServerMetadata"
{-# INLINEABLE ubLdapServerMetadata #-}
{-# DEPRECATED ldapServerMetadata "Use generic-lens or generic-optics with 'ldapServerMetadata' instead"  #-}

-- | Enables Amazon CloudWatch logging for brokers.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubLogs :: Lens.Lens' UpdateBroker (Core.Maybe Types.Logs)
ubLogs = Lens.field @"logs"
{-# INLINEABLE ubLogs #-}
{-# DEPRECATED logs "Use generic-lens or generic-optics with 'logs' instead"  #-}

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubSecurityGroups :: Lens.Lens' UpdateBroker (Core.Maybe [Core.Text])
ubSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE ubSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

instance Core.ToQuery UpdateBroker where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateBroker where
        toHeaders UpdateBroker{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateBroker where
        toJSON UpdateBroker{..}
          = Core.object
              (Core.catMaybes
                 [("authenticationStrategy" Core..=) Core.<$>
                    authenticationStrategy,
                  ("autoMinorVersionUpgrade" Core..=) Core.<$>
                    autoMinorVersionUpgrade,
                  ("configuration" Core..=) Core.<$> configuration,
                  ("engineVersion" Core..=) Core.<$> engineVersion,
                  ("hostInstanceType" Core..=) Core.<$> hostInstanceType,
                  ("ldapServerMetadata" Core..=) Core.<$> ldapServerMetadata,
                  ("logs" Core..=) Core.<$> logs,
                  ("securityGroups" Core..=) Core.<$> securityGroups])

instance Core.AWSRequest UpdateBroker where
        type Rs UpdateBroker = UpdateBrokerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/v1/brokers/" Core.<> Core.toText brokerId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateBrokerResponse' Core.<$>
                   (x Core..:? "authenticationStrategy") Core.<*>
                     x Core..:? "autoMinorVersionUpgrade"
                     Core.<*> x Core..:? "brokerId"
                     Core.<*> x Core..:? "configuration"
                     Core.<*> x Core..:? "engineVersion"
                     Core.<*> x Core..:? "hostInstanceType"
                     Core.<*> x Core..:? "ldapServerMetadata"
                     Core.<*> x Core..:? "logs"
                     Core.<*> x Core..:? "securityGroups"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateBrokerResponse' smart constructor.
data UpdateBrokerResponse = UpdateBrokerResponse'
  { authenticationStrategy :: Core.Maybe Types.AuthenticationStrategy
    -- ^ The authentication strategy used to secure the broker.
  , autoMinorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ The new value of automatic upgrades to new minor version for brokers.
  , brokerId :: Core.Maybe Core.Text
    -- ^ Required. The unique ID that Amazon MQ generates for the broker.
  , configuration :: Core.Maybe Types.ConfigurationId
    -- ^ The ID of the updated configuration.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
  , hostInstanceType :: Core.Maybe Core.Text
    -- ^ The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
  , ldapServerMetadata :: Core.Maybe Types.LdapServerMetadataOutput
    -- ^ The metadata of the LDAP server used to authenticate and authorize connections to the broker.
  , logs :: Core.Maybe Types.Logs
    -- ^ The list of information about logs to be enabled for the specified broker.
  , securityGroups :: Core.Maybe [Core.Text]
    -- ^ The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBrokerResponse' value with any optional fields omitted.
mkUpdateBrokerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateBrokerResponse
mkUpdateBrokerResponse responseStatus
  = UpdateBrokerResponse'{authenticationStrategy = Core.Nothing,
                          autoMinorVersionUpgrade = Core.Nothing, brokerId = Core.Nothing,
                          configuration = Core.Nothing, engineVersion = Core.Nothing,
                          hostInstanceType = Core.Nothing, ldapServerMetadata = Core.Nothing,
                          logs = Core.Nothing, securityGroups = Core.Nothing, responseStatus}

-- | The authentication strategy used to secure the broker.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsAuthenticationStrategy :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Types.AuthenticationStrategy)
ubrrsAuthenticationStrategy = Lens.field @"authenticationStrategy"
{-# INLINEABLE ubrrsAuthenticationStrategy #-}
{-# DEPRECATED authenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead"  #-}

-- | The new value of automatic upgrades to new minor version for brokers.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsAutoMinorVersionUpgrade :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Bool)
ubrrsAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# INLINEABLE ubrrsAutoMinorVersionUpgrade #-}
{-# DEPRECATED autoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead"  #-}

-- | Required. The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsBrokerId :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Text)
ubrrsBrokerId = Lens.field @"brokerId"
{-# INLINEABLE ubrrsBrokerId #-}
{-# DEPRECATED brokerId "Use generic-lens or generic-optics with 'brokerId' instead"  #-}

-- | The ID of the updated configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsConfiguration :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Types.ConfigurationId)
ubrrsConfiguration = Lens.field @"configuration"
{-# INLINEABLE ubrrsConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsEngineVersion :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Text)
ubrrsEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE ubrrsEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsHostInstanceType :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Text)
ubrrsHostInstanceType = Lens.field @"hostInstanceType"
{-# INLINEABLE ubrrsHostInstanceType #-}
{-# DEPRECATED hostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead"  #-}

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /Note:/ Consider using 'ldapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsLdapServerMetadata :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Types.LdapServerMetadataOutput)
ubrrsLdapServerMetadata = Lens.field @"ldapServerMetadata"
{-# INLINEABLE ubrrsLdapServerMetadata #-}
{-# DEPRECATED ldapServerMetadata "Use generic-lens or generic-optics with 'ldapServerMetadata' instead"  #-}

-- | The list of information about logs to be enabled for the specified broker.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsLogs :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Types.Logs)
ubrrsLogs = Lens.field @"logs"
{-# INLINEABLE ubrrsLogs #-}
{-# DEPRECATED logs "Use generic-lens or generic-optics with 'logs' instead"  #-}

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsSecurityGroups :: Lens.Lens' UpdateBrokerResponse (Core.Maybe [Core.Text])
ubrrsSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE ubrrsSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsResponseStatus :: Lens.Lens' UpdateBrokerResponse Core.Int
ubrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ubrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
