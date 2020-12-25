{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RegisterInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a set of tag keys to include in scheduled event notifications for your resources.
--
-- To remove tags, use .
module Network.AWS.EC2.RegisterInstanceEventNotificationAttributes
  ( -- * Creating a request
    RegisterInstanceEventNotificationAttributes (..),
    mkRegisterInstanceEventNotificationAttributes,

    -- ** Request lenses
    rienaDryRun,
    rienaInstanceTagAttribute,

    -- * Destructuring the response
    RegisterInstanceEventNotificationAttributesResponse (..),
    mkRegisterInstanceEventNotificationAttributesResponse,

    -- ** Response lenses
    rienarrsInstanceTagAttribute,
    rienarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterInstanceEventNotificationAttributes' smart constructor.
data RegisterInstanceEventNotificationAttributes = RegisterInstanceEventNotificationAttributes'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Information about the tag keys to register.
    instanceTagAttribute :: Core.Maybe Types.RegisterInstanceTagAttributeRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterInstanceEventNotificationAttributes' value with any optional fields omitted.
mkRegisterInstanceEventNotificationAttributes ::
  RegisterInstanceEventNotificationAttributes
mkRegisterInstanceEventNotificationAttributes =
  RegisterInstanceEventNotificationAttributes'
    { dryRun =
        Core.Nothing,
      instanceTagAttribute = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rienaDryRun :: Lens.Lens' RegisterInstanceEventNotificationAttributes (Core.Maybe Core.Bool)
rienaDryRun = Lens.field @"dryRun"
{-# DEPRECATED rienaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Information about the tag keys to register.
--
-- /Note:/ Consider using 'instanceTagAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rienaInstanceTagAttribute :: Lens.Lens' RegisterInstanceEventNotificationAttributes (Core.Maybe Types.RegisterInstanceTagAttributeRequest)
rienaInstanceTagAttribute = Lens.field @"instanceTagAttribute"
{-# DEPRECATED rienaInstanceTagAttribute "Use generic-lens or generic-optics with 'instanceTagAttribute' instead." #-}

instance
  Core.AWSRequest
    RegisterInstanceEventNotificationAttributes
  where
  type
    Rs RegisterInstanceEventNotificationAttributes =
      RegisterInstanceEventNotificationAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure
                ("Action", "RegisterInstanceEventNotificationAttributes")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> ( Core.toQueryValue "InstanceTagAttribute"
                            Core.<$> instanceTagAttribute
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          RegisterInstanceEventNotificationAttributesResponse'
            Core.<$> (x Core..@? "instanceTagAttribute")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterInstanceEventNotificationAttributesResponse' smart constructor.
data RegisterInstanceEventNotificationAttributesResponse = RegisterInstanceEventNotificationAttributesResponse'
  { -- | The resulting set of tag keys.
    instanceTagAttribute :: Core.Maybe Types.InstanceTagNotificationAttribute,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterInstanceEventNotificationAttributesResponse' value with any optional fields omitted.
mkRegisterInstanceEventNotificationAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterInstanceEventNotificationAttributesResponse
mkRegisterInstanceEventNotificationAttributesResponse
  responseStatus =
    RegisterInstanceEventNotificationAttributesResponse'
      { instanceTagAttribute =
          Core.Nothing,
        responseStatus
      }

-- | The resulting set of tag keys.
--
-- /Note:/ Consider using 'instanceTagAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rienarrsInstanceTagAttribute :: Lens.Lens' RegisterInstanceEventNotificationAttributesResponse (Core.Maybe Types.InstanceTagNotificationAttribute)
rienarrsInstanceTagAttribute = Lens.field @"instanceTagAttribute"
{-# DEPRECATED rienarrsInstanceTagAttribute "Use generic-lens or generic-optics with 'instanceTagAttribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rienarrsResponseStatus :: Lens.Lens' RegisterInstanceEventNotificationAttributesResponse Core.Int
rienarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rienarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
