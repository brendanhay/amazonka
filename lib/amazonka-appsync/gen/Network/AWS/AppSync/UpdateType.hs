{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Type@ object.
module Network.AWS.AppSync.UpdateType
  ( -- * Creating a request
    UpdateType (..),
    mkUpdateType,

    -- ** Request lenses
    utApiId,
    utTypeName,
    utFormat,
    utDefinition,

    -- * Destructuring the response
    UpdateTypeResponse (..),
    mkUpdateTypeResponse,

    -- ** Response lenses
    utrrsType,
    utrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateType' smart constructor.
data UpdateType = UpdateType'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The new type name.
    typeName :: Types.ResourceName,
    -- | The new type format: SDL or JSON.
    format :: Types.TypeDefinitionFormat,
    -- | The new definition.
    definition :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateType' value with any optional fields omitted.
mkUpdateType ::
  -- | 'apiId'
  Types.String ->
  -- | 'typeName'
  Types.ResourceName ->
  -- | 'format'
  Types.TypeDefinitionFormat ->
  UpdateType
mkUpdateType apiId typeName format =
  UpdateType' {apiId, typeName, format, definition = Core.Nothing}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utApiId :: Lens.Lens' UpdateType Types.String
utApiId = Lens.field @"apiId"
{-# DEPRECATED utApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The new type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTypeName :: Lens.Lens' UpdateType Types.ResourceName
utTypeName = Lens.field @"typeName"
{-# DEPRECATED utTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The new type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utFormat :: Lens.Lens' UpdateType Types.TypeDefinitionFormat
utFormat = Lens.field @"format"
{-# DEPRECATED utFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The new definition.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDefinition :: Lens.Lens' UpdateType (Core.Maybe Types.String)
utDefinition = Lens.field @"definition"
{-# DEPRECATED utDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

instance Core.FromJSON UpdateType where
  toJSON UpdateType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("format" Core..= format),
            ("definition" Core..=) Core.<$> definition
          ]
      )

instance Core.AWSRequest UpdateType where
  type Rs UpdateType = UpdateTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/types/")
                Core.<> (Core.toText typeName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTypeResponse'
            Core.<$> (x Core..:? "type") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateTypeResponse' smart constructor.
data UpdateTypeResponse = UpdateTypeResponse'
  { -- | The updated @Type@ object.
    type' :: Core.Maybe Types.Type,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTypeResponse' value with any optional fields omitted.
mkUpdateTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateTypeResponse
mkUpdateTypeResponse responseStatus =
  UpdateTypeResponse' {type' = Core.Nothing, responseStatus}

-- | The updated @Type@ object.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsType :: Lens.Lens' UpdateTypeResponse (Core.Maybe Types.Type)
utrrsType = Lens.field @"type'"
{-# DEPRECATED utrrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsResponseStatus :: Lens.Lens' UpdateTypeResponse Core.Int
utrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
