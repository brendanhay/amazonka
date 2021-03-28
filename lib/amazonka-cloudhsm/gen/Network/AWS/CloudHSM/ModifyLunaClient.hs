{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyLunaClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Modifies the certificate used by the client.
-- This action can potentially start a workflow to install the new certificate on the client's HSMs.
module Network.AWS.CloudHSM.ModifyLunaClient
    (
    -- * Creating a request
      ModifyLunaClient (..)
    , mkModifyLunaClient
    -- ** Request lenses
    , mlcClientArn
    , mlcCertificate

    -- * Destructuring the response
    , ModifyLunaClientResponse (..)
    , mkModifyLunaClientResponse
    -- ** Response lenses
    , mlcrrsClientArn
    , mlcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyLunaClient' smart constructor.
data ModifyLunaClient = ModifyLunaClient'
  { clientArn :: Types.ClientArn
    -- ^ The ARN of the client.
  , certificate :: Types.Certificate
    -- ^ The new certificate for the client.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyLunaClient' value with any optional fields omitted.
mkModifyLunaClient
    :: Types.ClientArn -- ^ 'clientArn'
    -> Types.Certificate -- ^ 'certificate'
    -> ModifyLunaClient
mkModifyLunaClient clientArn certificate
  = ModifyLunaClient'{clientArn, certificate}

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcClientArn :: Lens.Lens' ModifyLunaClient Types.ClientArn
mlcClientArn = Lens.field @"clientArn"
{-# INLINEABLE mlcClientArn #-}
{-# DEPRECATED clientArn "Use generic-lens or generic-optics with 'clientArn' instead"  #-}

-- | The new certificate for the client.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcCertificate :: Lens.Lens' ModifyLunaClient Types.Certificate
mlcCertificate = Lens.field @"certificate"
{-# INLINEABLE mlcCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

instance Core.ToQuery ModifyLunaClient where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyLunaClient where
        toHeaders ModifyLunaClient{..}
          = Core.pure
              ("X-Amz-Target", "CloudHsmFrontendService.ModifyLunaClient")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyLunaClient where
        toJSON ModifyLunaClient{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClientArn" Core..= clientArn),
                  Core.Just ("Certificate" Core..= certificate)])

instance Core.AWSRequest ModifyLunaClient where
        type Rs ModifyLunaClient = ModifyLunaClientResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ModifyLunaClientResponse' Core.<$>
                   (x Core..:? "ClientArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyLunaClientResponse' smart constructor.
data ModifyLunaClientResponse = ModifyLunaClientResponse'
  { clientArn :: Core.Maybe Types.ClientArn
    -- ^ The ARN of the client.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyLunaClientResponse' value with any optional fields omitted.
mkModifyLunaClientResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyLunaClientResponse
mkModifyLunaClientResponse responseStatus
  = ModifyLunaClientResponse'{clientArn = Core.Nothing,
                              responseStatus}

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcrrsClientArn :: Lens.Lens' ModifyLunaClientResponse (Core.Maybe Types.ClientArn)
mlcrrsClientArn = Lens.field @"clientArn"
{-# INLINEABLE mlcrrsClientArn #-}
{-# DEPRECATED clientArn "Use generic-lens or generic-optics with 'clientArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcrrsResponseStatus :: Lens.Lens' ModifyLunaClientResponse Core.Int
mlcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mlcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
