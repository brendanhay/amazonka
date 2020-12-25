{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.GetTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a custom terminology.
module Network.AWS.Translate.GetTerminology
  ( -- * Creating a request
    GetTerminology (..),
    mkGetTerminology,

    -- ** Request lenses
    gtName,
    gtTerminologyDataFormat,

    -- * Destructuring the response
    GetTerminologyResponse (..),
    mkGetTerminologyResponse,

    -- ** Response lenses
    gtrrsTerminologyDataLocation,
    gtrrsTerminologyProperties,
    gtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkGetTerminology' smart constructor.
data GetTerminology = GetTerminology'
  { -- | The name of the custom terminology being retrieved.
    name :: Types.Name,
    -- | The data format of the custom terminology being retrieved, either CSV or TMX.
    terminologyDataFormat :: Types.TerminologyDataFormat
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTerminology' value with any optional fields omitted.
mkGetTerminology ::
  -- | 'name'
  Types.Name ->
  -- | 'terminologyDataFormat'
  Types.TerminologyDataFormat ->
  GetTerminology
mkGetTerminology name terminologyDataFormat =
  GetTerminology' {name, terminologyDataFormat}

-- | The name of the custom terminology being retrieved.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtName :: Lens.Lens' GetTerminology Types.Name
gtName = Lens.field @"name"
{-# DEPRECATED gtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The data format of the custom terminology being retrieved, either CSV or TMX.
--
-- /Note:/ Consider using 'terminologyDataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTerminologyDataFormat :: Lens.Lens' GetTerminology Types.TerminologyDataFormat
gtTerminologyDataFormat = Lens.field @"terminologyDataFormat"
{-# DEPRECATED gtTerminologyDataFormat "Use generic-lens or generic-optics with 'terminologyDataFormat' instead." #-}

instance Core.FromJSON GetTerminology where
  toJSON GetTerminology {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("TerminologyDataFormat" Core..= terminologyDataFormat)
          ]
      )

instance Core.AWSRequest GetTerminology where
  type Rs GetTerminology = GetTerminologyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSShineFrontendService_20170701.GetTerminology")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTerminologyResponse'
            Core.<$> (x Core..:? "TerminologyDataLocation")
            Core.<*> (x Core..:? "TerminologyProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTerminologyResponse' smart constructor.
data GetTerminologyResponse = GetTerminologyResponse'
  { -- | The data location of the custom terminology being retrieved. The custom terminology file is returned in a presigned url that has a 30 minute expiration.
    terminologyDataLocation :: Core.Maybe Types.TerminologyDataLocation,
    -- | The properties of the custom terminology being retrieved.
    terminologyProperties :: Core.Maybe Types.TerminologyProperties,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetTerminologyResponse' value with any optional fields omitted.
mkGetTerminologyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTerminologyResponse
mkGetTerminologyResponse responseStatus =
  GetTerminologyResponse'
    { terminologyDataLocation = Core.Nothing,
      terminologyProperties = Core.Nothing,
      responseStatus
    }

-- | The data location of the custom terminology being retrieved. The custom terminology file is returned in a presigned url that has a 30 minute expiration.
--
-- /Note:/ Consider using 'terminologyDataLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTerminologyDataLocation :: Lens.Lens' GetTerminologyResponse (Core.Maybe Types.TerminologyDataLocation)
gtrrsTerminologyDataLocation = Lens.field @"terminologyDataLocation"
{-# DEPRECATED gtrrsTerminologyDataLocation "Use generic-lens or generic-optics with 'terminologyDataLocation' instead." #-}

-- | The properties of the custom terminology being retrieved.
--
-- /Note:/ Consider using 'terminologyProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTerminologyProperties :: Lens.Lens' GetTerminologyResponse (Core.Maybe Types.TerminologyProperties)
gtrrsTerminologyProperties = Lens.field @"terminologyProperties"
{-# DEPRECATED gtrrsTerminologyProperties "Use generic-lens or generic-optics with 'terminologyProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTerminologyResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
