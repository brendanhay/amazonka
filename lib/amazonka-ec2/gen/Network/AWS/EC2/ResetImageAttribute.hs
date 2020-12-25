{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an AMI to its default value.
module Network.AWS.EC2.ResetImageAttribute
  ( -- * Creating a request
    ResetImageAttribute (..),
    mkResetImageAttribute,

    -- ** Request lenses
    riafAttribute,
    riafImageId,
    riafDryRun,

    -- * Destructuring the response
    ResetImageAttributeResponse (..),
    mkResetImageAttributeResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ResetImageAttribute.
--
-- /See:/ 'mkResetImageAttribute' smart constructor.
data ResetImageAttribute = ResetImageAttribute'
  { -- | The attribute to reset (currently you can only reset the launch permission attribute).
    attribute :: Types.ResetImageAttributeName,
    -- | The ID of the AMI.
    imageId :: Types.ImageId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetImageAttribute' value with any optional fields omitted.
mkResetImageAttribute ::
  -- | 'attribute'
  Types.ResetImageAttributeName ->
  -- | 'imageId'
  Types.ImageId ->
  ResetImageAttribute
mkResetImageAttribute attribute imageId =
  ResetImageAttribute' {attribute, imageId, dryRun = Core.Nothing}

-- | The attribute to reset (currently you can only reset the launch permission attribute).
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riafAttribute :: Lens.Lens' ResetImageAttribute Types.ResetImageAttributeName
riafAttribute = Lens.field @"attribute"
{-# DEPRECATED riafAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riafImageId :: Lens.Lens' ResetImageAttribute Types.ImageId
riafImageId = Lens.field @"imageId"
{-# DEPRECATED riafImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riafDryRun :: Lens.Lens' ResetImageAttribute (Core.Maybe Core.Bool)
riafDryRun = Lens.field @"dryRun"
{-# DEPRECATED riafDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest ResetImageAttribute where
  type Rs ResetImageAttribute = ResetImageAttributeResponse
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
            ( Core.pure ("Action", "ResetImageAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Attribute" attribute)
                Core.<> (Core.toQueryValue "ImageId" imageId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull ResetImageAttributeResponse'

-- | /See:/ 'mkResetImageAttributeResponse' smart constructor.
data ResetImageAttributeResponse = ResetImageAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetImageAttributeResponse' value with any optional fields omitted.
mkResetImageAttributeResponse ::
  ResetImageAttributeResponse
mkResetImageAttributeResponse = ResetImageAttributeResponse'
