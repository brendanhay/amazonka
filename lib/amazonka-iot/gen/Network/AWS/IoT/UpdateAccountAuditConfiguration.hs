{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateAccountAuditConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures or reconfigures the Device Defender audit settings for this account. Settings include how audit notifications are sent and which audit checks are enabled or disabled.
module Network.AWS.IoT.UpdateAccountAuditConfiguration
    (
    -- * Creating a request
      UpdateAccountAuditConfiguration (..)
    , mkUpdateAccountAuditConfiguration
    -- ** Request lenses
    , uaacAuditCheckConfigurations
    , uaacAuditNotificationTargetConfigurations
    , uaacRoleArn

    -- * Destructuring the response
    , UpdateAccountAuditConfigurationResponse (..)
    , mkUpdateAccountAuditConfigurationResponse
    -- ** Response lenses
    , uaacrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAccountAuditConfiguration' smart constructor.
data UpdateAccountAuditConfiguration = UpdateAccountAuditConfiguration'
  { auditCheckConfigurations :: Core.Maybe (Core.HashMap Types.AuditCheckName Types.AuditCheckConfiguration)
    -- ^ Specifies which audit checks are enabled and disabled for this account. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are currently enabled.
--
-- Some data collection might start immediately when certain checks are enabled. When a check is disabled, any data collected so far in relation to the check is deleted.
-- You cannot disable a check if it is used by any scheduled audit. You must first delete the check from the scheduled audit or delete the scheduled audit itself.
-- On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required and must specify at least one enabled check.
  , auditNotificationTargetConfigurations :: Core.Maybe (Core.HashMap Types.AuditNotificationType Types.AuditNotificationTarget)
    -- ^ Information about the targets to which audit notifications are sent.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates and other items as required when performing an audit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAccountAuditConfiguration' value with any optional fields omitted.
mkUpdateAccountAuditConfiguration
    :: UpdateAccountAuditConfiguration
mkUpdateAccountAuditConfiguration
  = UpdateAccountAuditConfiguration'{auditCheckConfigurations =
                                       Core.Nothing,
                                     auditNotificationTargetConfigurations = Core.Nothing,
                                     roleArn = Core.Nothing}

-- | Specifies which audit checks are enabled and disabled for this account. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are currently enabled.
--
-- Some data collection might start immediately when certain checks are enabled. When a check is disabled, any data collected so far in relation to the check is deleted.
-- You cannot disable a check if it is used by any scheduled audit. You must first delete the check from the scheduled audit or delete the scheduled audit itself.
-- On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required and must specify at least one enabled check.
--
-- /Note:/ Consider using 'auditCheckConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaacAuditCheckConfigurations :: Lens.Lens' UpdateAccountAuditConfiguration (Core.Maybe (Core.HashMap Types.AuditCheckName Types.AuditCheckConfiguration))
uaacAuditCheckConfigurations = Lens.field @"auditCheckConfigurations"
{-# INLINEABLE uaacAuditCheckConfigurations #-}
{-# DEPRECATED auditCheckConfigurations "Use generic-lens or generic-optics with 'auditCheckConfigurations' instead"  #-}

-- | Information about the targets to which audit notifications are sent.
--
-- /Note:/ Consider using 'auditNotificationTargetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaacAuditNotificationTargetConfigurations :: Lens.Lens' UpdateAccountAuditConfiguration (Core.Maybe (Core.HashMap Types.AuditNotificationType Types.AuditNotificationTarget))
uaacAuditNotificationTargetConfigurations = Lens.field @"auditNotificationTargetConfigurations"
{-# INLINEABLE uaacAuditNotificationTargetConfigurations #-}
{-# DEPRECATED auditNotificationTargetConfigurations "Use generic-lens or generic-optics with 'auditNotificationTargetConfigurations' instead"  #-}

-- | The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates and other items as required when performing an audit.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaacRoleArn :: Lens.Lens' UpdateAccountAuditConfiguration (Core.Maybe Types.RoleArn)
uaacRoleArn = Lens.field @"roleArn"
{-# INLINEABLE uaacRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery UpdateAccountAuditConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAccountAuditConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateAccountAuditConfiguration where
        toJSON UpdateAccountAuditConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("auditCheckConfigurations" Core..=) Core.<$>
                    auditCheckConfigurations,
                  ("auditNotificationTargetConfigurations" Core..=) Core.<$>
                    auditNotificationTargetConfigurations,
                  ("roleArn" Core..=) Core.<$> roleArn])

instance Core.AWSRequest UpdateAccountAuditConfiguration where
        type Rs UpdateAccountAuditConfiguration =
             UpdateAccountAuditConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/audit/configuration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateAccountAuditConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAccountAuditConfigurationResponse' smart constructor.
newtype UpdateAccountAuditConfigurationResponse = UpdateAccountAuditConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAccountAuditConfigurationResponse' value with any optional fields omitted.
mkUpdateAccountAuditConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateAccountAuditConfigurationResponse
mkUpdateAccountAuditConfigurationResponse responseStatus
  = UpdateAccountAuditConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaacrrsResponseStatus :: Lens.Lens' UpdateAccountAuditConfigurationResponse Core.Int
uaacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uaacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
