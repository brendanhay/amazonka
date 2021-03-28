{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RegisterThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a thing in the device registry. RegisterThing calls other AWS IoT control plane APIs. These calls might exceed your account level <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_iot AWS IoT Throttling Limits> and cause throttle errors. Please contact <https://console.aws.amazon.com/support/home AWS Customer Support> to raise your throttling limits if necessary.
module Network.AWS.IoT.RegisterThing
    (
    -- * Creating a request
      RegisterThing (..)
    , mkRegisterThing
    -- ** Request lenses
    , rtTemplateBody
    , rtParameters

    -- * Destructuring the response
    , RegisterThingResponse (..)
    , mkRegisterThingResponse
    -- ** Response lenses
    , rtrrsCertificatePem
    , rtrrsResourceArns
    , rtrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterThing' smart constructor.
data RegisterThing = RegisterThing'
  { templateBody :: Types.TemplateBody
    -- ^ The provisioning template. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates> for more information.
  , parameters :: Core.Maybe (Core.HashMap Types.Parameter Types.Value)
    -- ^ The parameters for provisioning a thing. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates> for more information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterThing' value with any optional fields omitted.
mkRegisterThing
    :: Types.TemplateBody -- ^ 'templateBody'
    -> RegisterThing
mkRegisterThing templateBody
  = RegisterThing'{templateBody, parameters = Core.Nothing}

-- | The provisioning template. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates> for more information.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTemplateBody :: Lens.Lens' RegisterThing Types.TemplateBody
rtTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE rtTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | The parameters for provisioning a thing. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates> for more information.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtParameters :: Lens.Lens' RegisterThing (Core.Maybe (Core.HashMap Types.Parameter Types.Value))
rtParameters = Lens.field @"parameters"
{-# INLINEABLE rtParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.ToQuery RegisterThing where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterThing where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON RegisterThing where
        toJSON RegisterThing{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("templateBody" Core..= templateBody),
                  ("parameters" Core..=) Core.<$> parameters])

instance Core.AWSRequest RegisterThing where
        type Rs RegisterThing = RegisterThingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/things",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterThingResponse' Core.<$>
                   (x Core..:? "certificatePem") Core.<*> x Core..:? "resourceArns"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterThingResponse' smart constructor.
data RegisterThingResponse = RegisterThingResponse'
  { certificatePem :: Core.Maybe Types.CertificatePem
    -- ^ The certificate data, in PEM format.
  , resourceArns :: Core.Maybe (Core.HashMap Types.ResourceLogicalId Types.ResourceArn)
    -- ^ ARNs for the generated resources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterThingResponse' value with any optional fields omitted.
mkRegisterThingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterThingResponse
mkRegisterThingResponse responseStatus
  = RegisterThingResponse'{certificatePem = Core.Nothing,
                           resourceArns = Core.Nothing, responseStatus}

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsCertificatePem :: Lens.Lens' RegisterThingResponse (Core.Maybe Types.CertificatePem)
rtrrsCertificatePem = Lens.field @"certificatePem"
{-# INLINEABLE rtrrsCertificatePem #-}
{-# DEPRECATED certificatePem "Use generic-lens or generic-optics with 'certificatePem' instead"  #-}

-- | ARNs for the generated resources.
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResourceArns :: Lens.Lens' RegisterThingResponse (Core.Maybe (Core.HashMap Types.ResourceLogicalId Types.ResourceArn))
rtrrsResourceArns = Lens.field @"resourceArns"
{-# INLINEABLE rtrrsResourceArns #-}
{-# DEPRECATED resourceArns "Use generic-lens or generic-optics with 'resourceArns' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResponseStatus :: Lens.Lens' RegisterThingResponse Core.Int
rtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
