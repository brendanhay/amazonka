{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.ModifyTargetGroupAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attributes of the specified target group.
module Network.AWS.ELBv2.ModifyTargetGroupAttributes
  ( -- * Creating a request
    ModifyTargetGroupAttributes (..),
    mkModifyTargetGroupAttributes,

    -- ** Request lenses
    mtgaTargetGroupArn,
    mtgaAttributes,

    -- * Destructuring the response
    ModifyTargetGroupAttributesResponse (..),
    mkModifyTargetGroupAttributesResponse,

    -- ** Response lenses
    mtgarrsAttributes,
    mtgarrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTargetGroupAttributes' smart constructor.
data ModifyTargetGroupAttributes = ModifyTargetGroupAttributes'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Types.TargetGroupArn,
    -- | The attributes.
    attributes :: [Types.TargetGroupAttribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTargetGroupAttributes' value with any optional fields omitted.
mkModifyTargetGroupAttributes ::
  -- | 'targetGroupArn'
  Types.TargetGroupArn ->
  ModifyTargetGroupAttributes
mkModifyTargetGroupAttributes targetGroupArn =
  ModifyTargetGroupAttributes'
    { targetGroupArn,
      attributes = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgaTargetGroupArn :: Lens.Lens' ModifyTargetGroupAttributes Types.TargetGroupArn
mtgaTargetGroupArn = Lens.field @"targetGroupArn"
{-# DEPRECATED mtgaTargetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead." #-}

-- | The attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgaAttributes :: Lens.Lens' ModifyTargetGroupAttributes [Types.TargetGroupAttribute]
mtgaAttributes = Lens.field @"attributes"
{-# DEPRECATED mtgaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Core.AWSRequest ModifyTargetGroupAttributes where
  type
    Rs ModifyTargetGroupAttributes =
      ModifyTargetGroupAttributesResponse
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
            ( Core.pure ("Action", "ModifyTargetGroupAttributes")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "TargetGroupArn" targetGroupArn)
                Core.<> ( Core.toQueryValue
                            "Attributes"
                            (Core.toQueryList "member" attributes)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyTargetGroupAttributesResult"
      ( \s h x ->
          ModifyTargetGroupAttributesResponse'
            Core.<$> (x Core..@? "Attributes" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyTargetGroupAttributesResponse' smart constructor.
data ModifyTargetGroupAttributesResponse = ModifyTargetGroupAttributesResponse'
  { -- | Information about the attributes.
    attributes :: Core.Maybe [Types.TargetGroupAttribute],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTargetGroupAttributesResponse' value with any optional fields omitted.
mkModifyTargetGroupAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyTargetGroupAttributesResponse
mkModifyTargetGroupAttributesResponse responseStatus =
  ModifyTargetGroupAttributesResponse'
    { attributes = Core.Nothing,
      responseStatus
    }

-- | Information about the attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgarrsAttributes :: Lens.Lens' ModifyTargetGroupAttributesResponse (Core.Maybe [Types.TargetGroupAttribute])
mtgarrsAttributes = Lens.field @"attributes"
{-# DEPRECATED mtgarrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgarrsResponseStatus :: Lens.Lens' ModifyTargetGroupAttributesResponse Core.Int
mtgarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mtgarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
