{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified TagOption.
module Network.AWS.ServiceCatalog.UpdateTagOption
  ( -- * Creating a request
    UpdateTagOption (..),
    mkUpdateTagOption,

    -- ** Request lenses
    utoId,
    utoActive,
    utoValue,

    -- * Destructuring the response
    UpdateTagOptionResponse (..),
    mkUpdateTagOptionResponse,

    -- ** Response lenses
    utorrsTagOptionDetail,
    utorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkUpdateTagOption' smart constructor.
data UpdateTagOption = UpdateTagOption'
  { -- | The TagOption identifier.
    id :: Types.TagOptionId,
    -- | The updated active state.
    active :: Core.Maybe Core.Bool,
    -- | The updated value.
    value :: Core.Maybe Types.TagOptionValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTagOption' value with any optional fields omitted.
mkUpdateTagOption ::
  -- | 'id'
  Types.TagOptionId ->
  UpdateTagOption
mkUpdateTagOption id =
  UpdateTagOption' {id, active = Core.Nothing, value = Core.Nothing}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utoId :: Lens.Lens' UpdateTagOption Types.TagOptionId
utoId = Lens.field @"id"
{-# DEPRECATED utoId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The updated active state.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utoActive :: Lens.Lens' UpdateTagOption (Core.Maybe Core.Bool)
utoActive = Lens.field @"active"
{-# DEPRECATED utoActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The updated value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utoValue :: Lens.Lens' UpdateTagOption (Core.Maybe Types.TagOptionValue)
utoValue = Lens.field @"value"
{-# DEPRECATED utoValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON UpdateTagOption where
  toJSON UpdateTagOption {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            ("Active" Core..=) Core.<$> active,
            ("Value" Core..=) Core.<$> value
          ]
      )

instance Core.AWSRequest UpdateTagOption where
  type Rs UpdateTagOption = UpdateTagOptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.UpdateTagOption")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTagOptionResponse'
            Core.<$> (x Core..:? "TagOptionDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateTagOptionResponse' smart constructor.
data UpdateTagOptionResponse = UpdateTagOptionResponse'
  { -- | Information about the TagOption.
    tagOptionDetail :: Core.Maybe Types.TagOptionDetail,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTagOptionResponse' value with any optional fields omitted.
mkUpdateTagOptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateTagOptionResponse
mkUpdateTagOptionResponse responseStatus =
  UpdateTagOptionResponse'
    { tagOptionDetail = Core.Nothing,
      responseStatus
    }

-- | Information about the TagOption.
--
-- /Note:/ Consider using 'tagOptionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utorrsTagOptionDetail :: Lens.Lens' UpdateTagOptionResponse (Core.Maybe Types.TagOptionDetail)
utorrsTagOptionDetail = Lens.field @"tagOptionDetail"
{-# DEPRECATED utorrsTagOptionDetail "Use generic-lens or generic-optics with 'tagOptionDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utorrsResponseStatus :: Lens.Lens' UpdateTagOptionResponse Core.Int
utorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
