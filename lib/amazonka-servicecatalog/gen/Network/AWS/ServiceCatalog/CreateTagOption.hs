{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a TagOption.
module Network.AWS.ServiceCatalog.CreateTagOption
  ( -- * Creating a request
    CreateTagOption (..),
    mkCreateTagOption,

    -- ** Request lenses
    ctoKey,
    ctoValue,

    -- * Destructuring the response
    CreateTagOptionResponse (..),
    mkCreateTagOptionResponse,

    -- ** Response lenses
    ctorrsTagOptionDetail,
    ctorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCreateTagOption' smart constructor.
data CreateTagOption = CreateTagOption'
  { -- | The TagOption key.
    key :: Types.Key,
    -- | The TagOption value.
    value :: Types.TagOptionValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTagOption' value with any optional fields omitted.
mkCreateTagOption ::
  -- | 'key'
  Types.Key ->
  -- | 'value'
  Types.TagOptionValue ->
  CreateTagOption
mkCreateTagOption key value = CreateTagOption' {key, value}

-- | The TagOption key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctoKey :: Lens.Lens' CreateTagOption Types.Key
ctoKey = Lens.field @"key"
{-# DEPRECATED ctoKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The TagOption value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctoValue :: Lens.Lens' CreateTagOption Types.TagOptionValue
ctoValue = Lens.field @"value"
{-# DEPRECATED ctoValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON CreateTagOption where
  toJSON CreateTagOption {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Key" Core..= key), Core.Just ("Value" Core..= value)]
      )

instance Core.AWSRequest CreateTagOption where
  type Rs CreateTagOption = CreateTagOptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.CreateTagOption")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTagOptionResponse'
            Core.<$> (x Core..:? "TagOptionDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTagOptionResponse' smart constructor.
data CreateTagOptionResponse = CreateTagOptionResponse'
  { -- | Information about the TagOption.
    tagOptionDetail :: Core.Maybe Types.TagOptionDetail,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTagOptionResponse' value with any optional fields omitted.
mkCreateTagOptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTagOptionResponse
mkCreateTagOptionResponse responseStatus =
  CreateTagOptionResponse'
    { tagOptionDetail = Core.Nothing,
      responseStatus
    }

-- | Information about the TagOption.
--
-- /Note:/ Consider using 'tagOptionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctorrsTagOptionDetail :: Lens.Lens' CreateTagOptionResponse (Core.Maybe Types.TagOptionDetail)
ctorrsTagOptionDetail = Lens.field @"tagOptionDetail"
{-# DEPRECATED ctorrsTagOptionDetail "Use generic-lens or generic-optics with 'tagOptionDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctorrsResponseStatus :: Lens.Lens' CreateTagOptionResponse Core.Int
ctorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
