{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the details of a 'Protection' object.
module Network.AWS.Shield.DescribeProtection
  ( -- * Creating a request
    DescribeProtection (..),
    mkDescribeProtection,

    -- ** Request lenses
    dpProtectionId,
    dpResourceArn,

    -- * Destructuring the response
    DescribeProtectionResponse (..),
    mkDescribeProtectionResponse,

    -- ** Response lenses
    dprrsProtection,
    dprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDescribeProtection' smart constructor.
data DescribeProtection = DescribeProtection'
  { -- | The unique identifier (ID) for the 'Protection' object that is described. When submitting the @DescribeProtection@ request you must provide either the @ResourceArn@ or the @ProtectionID@ , but not both.
    protectionId :: Core.Maybe Types.ProtectionId,
    -- | The ARN (Amazon Resource Name) of the AWS resource for the 'Protection' object that is described. When submitting the @DescribeProtection@ request you must provide either the @ResourceArn@ or the @ProtectionID@ , but not both.
    resourceArn :: Core.Maybe Types.ResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProtection' value with any optional fields omitted.
mkDescribeProtection ::
  DescribeProtection
mkDescribeProtection =
  DescribeProtection'
    { protectionId = Core.Nothing,
      resourceArn = Core.Nothing
    }

-- | The unique identifier (ID) for the 'Protection' object that is described. When submitting the @DescribeProtection@ request you must provide either the @ResourceArn@ or the @ProtectionID@ , but not both.
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpProtectionId :: Lens.Lens' DescribeProtection (Core.Maybe Types.ProtectionId)
dpProtectionId = Lens.field @"protectionId"
{-# DEPRECATED dpProtectionId "Use generic-lens or generic-optics with 'protectionId' instead." #-}

-- | The ARN (Amazon Resource Name) of the AWS resource for the 'Protection' object that is described. When submitting the @DescribeProtection@ request you must provide either the @ResourceArn@ or the @ProtectionID@ , but not both.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpResourceArn :: Lens.Lens' DescribeProtection (Core.Maybe Types.ResourceArn)
dpResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED dpResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON DescribeProtection where
  toJSON DescribeProtection {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProtectionId" Core..=) Core.<$> protectionId,
            ("ResourceArn" Core..=) Core.<$> resourceArn
          ]
      )

instance Core.AWSRequest DescribeProtection where
  type Rs DescribeProtection = DescribeProtectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSShield_20160616.DescribeProtection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProtectionResponse'
            Core.<$> (x Core..:? "Protection") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProtectionResponse' smart constructor.
data DescribeProtectionResponse = DescribeProtectionResponse'
  { -- | The 'Protection' object that is described.
    protection :: Core.Maybe Types.Protection,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProtectionResponse' value with any optional fields omitted.
mkDescribeProtectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeProtectionResponse
mkDescribeProtectionResponse responseStatus =
  DescribeProtectionResponse'
    { protection = Core.Nothing,
      responseStatus
    }

-- | The 'Protection' object that is described.
--
-- /Note:/ Consider using 'protection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsProtection :: Lens.Lens' DescribeProtectionResponse (Core.Maybe Types.Protection)
dprrsProtection = Lens.field @"protection"
{-# DEPRECATED dprrsProtection "Use generic-lens or generic-optics with 'protection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DescribeProtectionResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
