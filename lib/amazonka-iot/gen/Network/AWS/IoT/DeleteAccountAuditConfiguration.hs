{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteAccountAuditConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the default settings for Device Defender audits for this account. Any configuration data you entered is deleted and all audit checks are reset to disabled. 
module Network.AWS.IoT.DeleteAccountAuditConfiguration
    (
    -- * Creating a request
      DeleteAccountAuditConfiguration (..)
    , mkDeleteAccountAuditConfiguration
    -- ** Request lenses
    , daacDeleteScheduledAudits

    -- * Destructuring the response
    , DeleteAccountAuditConfigurationResponse (..)
    , mkDeleteAccountAuditConfigurationResponse
    -- ** Response lenses
    , daacrfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAccountAuditConfiguration' smart constructor.
newtype DeleteAccountAuditConfiguration = DeleteAccountAuditConfiguration'
  { deleteScheduledAudits :: Core.Maybe Core.Bool
    -- ^ If true, all scheduled audits are deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccountAuditConfiguration' value with any optional fields omitted.
mkDeleteAccountAuditConfiguration
    :: DeleteAccountAuditConfiguration
mkDeleteAccountAuditConfiguration
  = DeleteAccountAuditConfiguration'{deleteScheduledAudits =
                                       Core.Nothing}

-- | If true, all scheduled audits are deleted.
--
-- /Note:/ Consider using 'deleteScheduledAudits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacDeleteScheduledAudits :: Lens.Lens' DeleteAccountAuditConfiguration (Core.Maybe Core.Bool)
daacDeleteScheduledAudits = Lens.field @"deleteScheduledAudits"
{-# INLINEABLE daacDeleteScheduledAudits #-}
{-# DEPRECATED deleteScheduledAudits "Use generic-lens or generic-optics with 'deleteScheduledAudits' instead"  #-}

instance Core.ToQuery DeleteAccountAuditConfiguration where
        toQuery DeleteAccountAuditConfiguration{..}
          = Core.maybe Core.mempty (Core.toQueryPair "deleteScheduledAudits")
              deleteScheduledAudits

instance Core.ToHeaders DeleteAccountAuditConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteAccountAuditConfiguration where
        type Rs DeleteAccountAuditConfiguration =
             DeleteAccountAuditConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/audit/configuration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteAccountAuditConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAccountAuditConfigurationResponse' smart constructor.
newtype DeleteAccountAuditConfigurationResponse = DeleteAccountAuditConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccountAuditConfigurationResponse' value with any optional fields omitted.
mkDeleteAccountAuditConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteAccountAuditConfigurationResponse
mkDeleteAccountAuditConfigurationResponse responseStatus
  = DeleteAccountAuditConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacrfrsResponseStatus :: Lens.Lens' DeleteAccountAuditConfigurationResponse Core.Int
daacrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daacrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
