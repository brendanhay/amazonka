{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a profile that can be applied to one or more private device instances.
module Network.AWS.DeviceFarm.DeleteInstanceProfile
  ( -- * Creating a request
    DeleteInstanceProfile (..),
    mkDeleteInstanceProfile,

    -- ** Request lenses
    dipArn,

    -- * Destructuring the response
    DeleteInstanceProfileResponse (..),
    mkDeleteInstanceProfileResponse,

    -- ** Response lenses
    diprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInstanceProfile' smart constructor.
newtype DeleteInstanceProfile = DeleteInstanceProfile'
  { -- | The Amazon Resource Name (ARN) of the instance profile you are requesting to delete.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstanceProfile' value with any optional fields omitted.
mkDeleteInstanceProfile ::
  -- | 'arn'
  Types.Arn ->
  DeleteInstanceProfile
mkDeleteInstanceProfile arn = DeleteInstanceProfile' {arn}

-- | The Amazon Resource Name (ARN) of the instance profile you are requesting to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipArn :: Lens.Lens' DeleteInstanceProfile Types.Arn
dipArn = Lens.field @"arn"
{-# DEPRECATED dipArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON DeleteInstanceProfile where
  toJSON DeleteInstanceProfile {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteInstanceProfile where
  type Rs DeleteInstanceProfile = DeleteInstanceProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.DeleteInstanceProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteInstanceProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteInstanceProfileResponse' smart constructor.
newtype DeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstanceProfileResponse' value with any optional fields omitted.
mkDeleteInstanceProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteInstanceProfileResponse
mkDeleteInstanceProfileResponse responseStatus =
  DeleteInstanceProfileResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsResponseStatus :: Lens.Lens' DeleteInstanceProfileResponse Core.Int
diprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
