{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteHumanTaskUi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete a human task user interface (worker task template).
--
-- To see a list of human task user interfaces (work task templates) in your account, use . When you delete a worker task template, it no longer appears when you call @ListHumanTaskUis@ .
module Network.AWS.SageMaker.DeleteHumanTaskUi
  ( -- * Creating a request
    DeleteHumanTaskUi (..),
    mkDeleteHumanTaskUi,

    -- ** Request lenses
    dhtuHumanTaskUiName,

    -- * Destructuring the response
    DeleteHumanTaskUiResponse (..),
    mkDeleteHumanTaskUiResponse,

    -- ** Response lenses
    dhturrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteHumanTaskUi' smart constructor.
newtype DeleteHumanTaskUi = DeleteHumanTaskUi'
  { -- | The name of the human task user interface (work task template) you want to delete.
    humanTaskUiName :: Types.HumanTaskUiName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHumanTaskUi' value with any optional fields omitted.
mkDeleteHumanTaskUi ::
  -- | 'humanTaskUiName'
  Types.HumanTaskUiName ->
  DeleteHumanTaskUi
mkDeleteHumanTaskUi humanTaskUiName =
  DeleteHumanTaskUi' {humanTaskUiName}

-- | The name of the human task user interface (work task template) you want to delete.
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtuHumanTaskUiName :: Lens.Lens' DeleteHumanTaskUi Types.HumanTaskUiName
dhtuHumanTaskUiName = Lens.field @"humanTaskUiName"
{-# DEPRECATED dhtuHumanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead." #-}

instance Core.FromJSON DeleteHumanTaskUi where
  toJSON DeleteHumanTaskUi {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("HumanTaskUiName" Core..= humanTaskUiName)]
      )

instance Core.AWSRequest DeleteHumanTaskUi where
  type Rs DeleteHumanTaskUi = DeleteHumanTaskUiResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteHumanTaskUi")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteHumanTaskUiResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteHumanTaskUiResponse' smart constructor.
newtype DeleteHumanTaskUiResponse = DeleteHumanTaskUiResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHumanTaskUiResponse' value with any optional fields omitted.
mkDeleteHumanTaskUiResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteHumanTaskUiResponse
mkDeleteHumanTaskUiResponse responseStatus =
  DeleteHumanTaskUiResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturrsResponseStatus :: Lens.Lens' DeleteHumanTaskUiResponse Core.Int
dhturrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dhturrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
