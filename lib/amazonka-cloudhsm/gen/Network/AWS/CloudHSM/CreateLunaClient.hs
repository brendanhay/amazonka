{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.CreateLunaClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Creates an HSM client.
module Network.AWS.CloudHSM.CreateLunaClient
    (
    -- * Creating a request
      CreateLunaClient (..)
    , mkCreateLunaClient
    -- ** Request lenses
    , clcCertificate
    , clcLabel

    -- * Destructuring the response
    , CreateLunaClientResponse (..)
    , mkCreateLunaClientResponse
    -- ** Response lenses
    , clcrrsClientArn
    , clcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'CreateLunaClient' action.
--
-- /See:/ 'mkCreateLunaClient' smart constructor.
data CreateLunaClient = CreateLunaClient'
  { certificate :: Types.Certificate
    -- ^ The contents of a Base64-Encoded X.509 v3 certificate to be installed on the HSMs used by this client.
  , label :: Core.Maybe Types.ClientLabel
    -- ^ The label for the client.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLunaClient' value with any optional fields omitted.
mkCreateLunaClient
    :: Types.Certificate -- ^ 'certificate'
    -> CreateLunaClient
mkCreateLunaClient certificate
  = CreateLunaClient'{certificate, label = Core.Nothing}

-- | The contents of a Base64-Encoded X.509 v3 certificate to be installed on the HSMs used by this client.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcCertificate :: Lens.Lens' CreateLunaClient Types.Certificate
clcCertificate = Lens.field @"certificate"
{-# INLINEABLE clcCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The label for the client.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcLabel :: Lens.Lens' CreateLunaClient (Core.Maybe Types.ClientLabel)
clcLabel = Lens.field @"label"
{-# INLINEABLE clcLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

instance Core.ToQuery CreateLunaClient where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateLunaClient where
        toHeaders CreateLunaClient{..}
          = Core.pure
              ("X-Amz-Target", "CloudHsmFrontendService.CreateLunaClient")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateLunaClient where
        toJSON CreateLunaClient{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Certificate" Core..= certificate),
                  ("Label" Core..=) Core.<$> label])

instance Core.AWSRequest CreateLunaClient where
        type Rs CreateLunaClient = CreateLunaClientResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateLunaClientResponse' Core.<$>
                   (x Core..:? "ClientArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of the 'CreateLunaClient' action.
--
-- /See:/ 'mkCreateLunaClientResponse' smart constructor.
data CreateLunaClientResponse = CreateLunaClientResponse'
  { clientArn :: Core.Maybe Types.ClientArn
    -- ^ The ARN of the client.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLunaClientResponse' value with any optional fields omitted.
mkCreateLunaClientResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLunaClientResponse
mkCreateLunaClientResponse responseStatus
  = CreateLunaClientResponse'{clientArn = Core.Nothing,
                              responseStatus}

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcrrsClientArn :: Lens.Lens' CreateLunaClientResponse (Core.Maybe Types.ClientArn)
clcrrsClientArn = Lens.field @"clientArn"
{-# INLINEABLE clcrrsClientArn #-}
{-# DEPRECATED clientArn "Use generic-lens or generic-optics with 'clientArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcrrsResponseStatus :: Lens.Lens' CreateLunaClientResponse Core.Int
clcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
