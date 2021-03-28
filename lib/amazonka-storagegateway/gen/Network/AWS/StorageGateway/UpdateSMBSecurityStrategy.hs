{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateSMBSecurityStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the SMB security strategy on a file gateway. This action is only supported in file gateways.
module Network.AWS.StorageGateway.UpdateSMBSecurityStrategy
    (
    -- * Creating a request
      UpdateSMBSecurityStrategy (..)
    , mkUpdateSMBSecurityStrategy
    -- ** Request lenses
    , usmbssGatewayARN
    , usmbssSMBSecurityStrategy

    -- * Destructuring the response
    , UpdateSMBSecurityStrategyResponse (..)
    , mkUpdateSMBSecurityStrategyResponse
    -- ** Response lenses
    , usmbssrrsGatewayARN
    , usmbssrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkUpdateSMBSecurityStrategy' smart constructor.
data UpdateSMBSecurityStrategy = UpdateSMBSecurityStrategy'
  { gatewayARN :: Types.GatewayARN
  , sMBSecurityStrategy :: Types.SMBSecurityStrategy
    -- ^ Specifies the type of security strategy.
--
-- ClientSpecified: if you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment.
-- MandatorySigning: if you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer.
-- MandatoryEncryption: if you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSMBSecurityStrategy' value with any optional fields omitted.
mkUpdateSMBSecurityStrategy
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> Types.SMBSecurityStrategy -- ^ 'sMBSecurityStrategy'
    -> UpdateSMBSecurityStrategy
mkUpdateSMBSecurityStrategy gatewayARN sMBSecurityStrategy
  = UpdateSMBSecurityStrategy'{gatewayARN, sMBSecurityStrategy}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbssGatewayARN :: Lens.Lens' UpdateSMBSecurityStrategy Types.GatewayARN
usmbssGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE usmbssGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | Specifies the type of security strategy.
--
-- ClientSpecified: if you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment.
-- MandatorySigning: if you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer.
-- MandatoryEncryption: if you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
--
-- /Note:/ Consider using 'sMBSecurityStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbssSMBSecurityStrategy :: Lens.Lens' UpdateSMBSecurityStrategy Types.SMBSecurityStrategy
usmbssSMBSecurityStrategy = Lens.field @"sMBSecurityStrategy"
{-# INLINEABLE usmbssSMBSecurityStrategy #-}
{-# DEPRECATED sMBSecurityStrategy "Use generic-lens or generic-optics with 'sMBSecurityStrategy' instead"  #-}

instance Core.ToQuery UpdateSMBSecurityStrategy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSMBSecurityStrategy where
        toHeaders UpdateSMBSecurityStrategy{..}
          = Core.pure
              ("X-Amz-Target",
               "StorageGateway_20130630.UpdateSMBSecurityStrategy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateSMBSecurityStrategy where
        toJSON UpdateSMBSecurityStrategy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("SMBSecurityStrategy" Core..= sMBSecurityStrategy)])

instance Core.AWSRequest UpdateSMBSecurityStrategy where
        type Rs UpdateSMBSecurityStrategy =
             UpdateSMBSecurityStrategyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateSMBSecurityStrategyResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateSMBSecurityStrategyResponse' smart constructor.
data UpdateSMBSecurityStrategyResponse = UpdateSMBSecurityStrategyResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSMBSecurityStrategyResponse' value with any optional fields omitted.
mkUpdateSMBSecurityStrategyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSMBSecurityStrategyResponse
mkUpdateSMBSecurityStrategyResponse responseStatus
  = UpdateSMBSecurityStrategyResponse'{gatewayARN = Core.Nothing,
                                       responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbssrrsGatewayARN :: Lens.Lens' UpdateSMBSecurityStrategyResponse (Core.Maybe Types.GatewayARN)
usmbssrrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE usmbssrrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbssrrsResponseStatus :: Lens.Lens' UpdateSMBSecurityStrategyResponse Core.Int
usmbssrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usmbssrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
