{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DeleteProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Shield Advanced 'Protection' .
module Network.AWS.Shield.DeleteProtection
  ( -- * Creating a request
    DeleteProtection (..),
    mkDeleteProtection,

    -- ** Request lenses
    dProtectionId,

    -- * Destructuring the response
    DeleteProtectionResponse (..),
    mkDeleteProtectionResponse,

    -- ** Response lenses
    dprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDeleteProtection' smart constructor.
newtype DeleteProtection = DeleteProtection'
  { -- | The unique identifier (ID) for the 'Protection' object to be deleted.
    protectionId :: Types.ProtectionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProtection' value with any optional fields omitted.
mkDeleteProtection ::
  -- | 'protectionId'
  Types.ProtectionId ->
  DeleteProtection
mkDeleteProtection protectionId = DeleteProtection' {protectionId}

-- | The unique identifier (ID) for the 'Protection' object to be deleted.
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dProtectionId :: Lens.Lens' DeleteProtection Types.ProtectionId
dProtectionId = Lens.field @"protectionId"
{-# DEPRECATED dProtectionId "Use generic-lens or generic-optics with 'protectionId' instead." #-}

instance Core.FromJSON DeleteProtection where
  toJSON DeleteProtection {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ProtectionId" Core..= protectionId)])

instance Core.AWSRequest DeleteProtection where
  type Rs DeleteProtection = DeleteProtectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSShield_20160616.DeleteProtection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProtectionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteProtectionResponse' smart constructor.
newtype DeleteProtectionResponse = DeleteProtectionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProtectionResponse' value with any optional fields omitted.
mkDeleteProtectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteProtectionResponse
mkDeleteProtectionResponse responseStatus =
  DeleteProtectionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsResponseStatus :: Lens.Lens' DeleteProtectionResponse Core.Int
dprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
