{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RegisterThing (..),
    mkRegisterThing,

    -- ** Request lenses
    rtTemplateBody,
    rtParameters,

    -- * Destructuring the response
    RegisterThingResponse (..),
    mkRegisterThingResponse,

    -- ** Response lenses
    rtrrsCertificatePem,
    rtrrsResourceArns,
    rtrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterThing' smart constructor.
data RegisterThing = RegisterThing'
  { -- | The provisioning template. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates> for more information.
    templateBody :: Types.TemplateBody,
    -- | The parameters for provisioning a thing. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates> for more information.
    parameters :: Core.Maybe (Core.HashMap Types.Parameter Types.Value)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterThing' value with any optional fields omitted.
mkRegisterThing ::
  -- | 'templateBody'
  Types.TemplateBody ->
  RegisterThing
mkRegisterThing templateBody =
  RegisterThing' {templateBody, parameters = Core.Nothing}

-- | The provisioning template. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates> for more information.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTemplateBody :: Lens.Lens' RegisterThing Types.TemplateBody
rtTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED rtTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The parameters for provisioning a thing. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates> for more information.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtParameters :: Lens.Lens' RegisterThing (Core.Maybe (Core.HashMap Types.Parameter Types.Value))
rtParameters = Lens.field @"parameters"
{-# DEPRECATED rtParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON RegisterThing where
  toJSON RegisterThing {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("templateBody" Core..= templateBody),
            ("parameters" Core..=) Core.<$> parameters
          ]
      )

instance Core.AWSRequest RegisterThing where
  type Rs RegisterThing = RegisterThingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/things",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterThingResponse'
            Core.<$> (x Core..:? "certificatePem")
            Core.<*> (x Core..:? "resourceArns")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterThingResponse' smart constructor.
data RegisterThingResponse = RegisterThingResponse'
  { -- | The certificate data, in PEM format.
    certificatePem :: Core.Maybe Types.CertificatePem,
    -- | ARNs for the generated resources.
    resourceArns :: Core.Maybe (Core.HashMap Types.ResourceLogicalId Types.ResourceArn),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterThingResponse' value with any optional fields omitted.
mkRegisterThingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterThingResponse
mkRegisterThingResponse responseStatus =
  RegisterThingResponse'
    { certificatePem = Core.Nothing,
      resourceArns = Core.Nothing,
      responseStatus
    }

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsCertificatePem :: Lens.Lens' RegisterThingResponse (Core.Maybe Types.CertificatePem)
rtrrsCertificatePem = Lens.field @"certificatePem"
{-# DEPRECATED rtrrsCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | ARNs for the generated resources.
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResourceArns :: Lens.Lens' RegisterThingResponse (Core.Maybe (Core.HashMap Types.ResourceLogicalId Types.ResourceArn))
rtrrsResourceArns = Lens.field @"resourceArns"
{-# DEPRECATED rtrrsResourceArns "Use generic-lens or generic-optics with 'resourceArns' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResponseStatus :: Lens.Lens' RegisterThingResponse Core.Int
rtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
