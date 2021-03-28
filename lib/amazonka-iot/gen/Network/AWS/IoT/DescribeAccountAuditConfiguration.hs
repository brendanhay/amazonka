{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAccountAuditConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the Device Defender audit settings for this account. Settings include how audit notifications are sent and which audit checks are enabled or disabled.
module Network.AWS.IoT.DescribeAccountAuditConfiguration
    (
    -- * Creating a request
      DescribeAccountAuditConfiguration (..)
    , mkDescribeAccountAuditConfiguration

    -- * Destructuring the response
    , DescribeAccountAuditConfigurationResponse (..)
    , mkDescribeAccountAuditConfigurationResponse
    -- ** Response lenses
    , daacrrsAuditCheckConfigurations
    , daacrrsAuditNotificationTargetConfigurations
    , daacrrsRoleArn
    , daacrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAccountAuditConfiguration' smart constructor.
data DescribeAccountAuditConfiguration = DescribeAccountAuditConfiguration'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAuditConfiguration' value with any optional fields omitted.
mkDescribeAccountAuditConfiguration
    :: DescribeAccountAuditConfiguration
mkDescribeAccountAuditConfiguration
  = DescribeAccountAuditConfiguration'

instance Core.ToQuery DescribeAccountAuditConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAccountAuditConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAccountAuditConfiguration where
        type Rs DescribeAccountAuditConfiguration =
             DescribeAccountAuditConfigurationResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/audit/configuration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAccountAuditConfigurationResponse' Core.<$>
                   (x Core..:? "auditCheckConfigurations") Core.<*>
                     x Core..:? "auditNotificationTargetConfigurations"
                     Core.<*> x Core..:? "roleArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAccountAuditConfigurationResponse' smart constructor.
data DescribeAccountAuditConfigurationResponse = DescribeAccountAuditConfigurationResponse'
  { auditCheckConfigurations :: Core.Maybe (Core.HashMap Types.AuditCheckName Types.AuditCheckConfiguration)
    -- ^ Which audit checks are enabled and disabled for this account.
  , auditNotificationTargetConfigurations :: Core.Maybe (Core.HashMap Types.AuditNotificationType Types.AuditNotificationTarget)
    -- ^ Information about the targets to which audit notifications are sent for this account.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates, and other items as required when performing an audit.
--
-- On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAuditConfigurationResponse' value with any optional fields omitted.
mkDescribeAccountAuditConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAccountAuditConfigurationResponse
mkDescribeAccountAuditConfigurationResponse responseStatus
  = DescribeAccountAuditConfigurationResponse'{auditCheckConfigurations
                                                 = Core.Nothing,
                                               auditNotificationTargetConfigurations = Core.Nothing,
                                               roleArn = Core.Nothing, responseStatus}

-- | Which audit checks are enabled and disabled for this account.
--
-- /Note:/ Consider using 'auditCheckConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacrrsAuditCheckConfigurations :: Lens.Lens' DescribeAccountAuditConfigurationResponse (Core.Maybe (Core.HashMap Types.AuditCheckName Types.AuditCheckConfiguration))
daacrrsAuditCheckConfigurations = Lens.field @"auditCheckConfigurations"
{-# INLINEABLE daacrrsAuditCheckConfigurations #-}
{-# DEPRECATED auditCheckConfigurations "Use generic-lens or generic-optics with 'auditCheckConfigurations' instead"  #-}

-- | Information about the targets to which audit notifications are sent for this account.
--
-- /Note:/ Consider using 'auditNotificationTargetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacrrsAuditNotificationTargetConfigurations :: Lens.Lens' DescribeAccountAuditConfigurationResponse (Core.Maybe (Core.HashMap Types.AuditNotificationType Types.AuditNotificationTarget))
daacrrsAuditNotificationTargetConfigurations = Lens.field @"auditNotificationTargetConfigurations"
{-# INLINEABLE daacrrsAuditNotificationTargetConfigurations #-}
{-# DEPRECATED auditNotificationTargetConfigurations "Use generic-lens or generic-optics with 'auditNotificationTargetConfigurations' instead"  #-}

-- | The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates, and other items as required when performing an audit.
--
-- On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacrrsRoleArn :: Lens.Lens' DescribeAccountAuditConfigurationResponse (Core.Maybe Types.RoleArn)
daacrrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE daacrrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacrrsResponseStatus :: Lens.Lens' DescribeAccountAuditConfigurationResponse Core.Int
daacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
