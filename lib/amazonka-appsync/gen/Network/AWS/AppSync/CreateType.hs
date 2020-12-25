{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Type@ object.
module Network.AWS.AppSync.CreateType
  ( -- * Creating a request
    CreateType (..),
    mkCreateType,

    -- ** Request lenses
    ctApiId,
    ctDefinition,
    ctFormat,

    -- * Destructuring the response
    CreateTypeResponse (..),
    mkCreateTypeResponse,

    -- ** Response lenses
    ctrrsType,
    ctrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateType' smart constructor.
data CreateType = CreateType'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The type definition, in GraphQL Schema Definition Language (SDL) format.
    --
    -- For more information, see the <http://graphql.org/learn/schema/ GraphQL SDL documentation> .
    definition :: Types.String,
    -- | The type format: SDL or JSON.
    format :: Types.TypeDefinitionFormat
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateType' value with any optional fields omitted.
mkCreateType ::
  -- | 'apiId'
  Types.String ->
  -- | 'definition'
  Types.String ->
  -- | 'format'
  Types.TypeDefinitionFormat ->
  CreateType
mkCreateType apiId definition format =
  CreateType' {apiId, definition, format}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctApiId :: Lens.Lens' CreateType Types.String
ctApiId = Lens.field @"apiId"
{-# DEPRECATED ctApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The type definition, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the <http://graphql.org/learn/schema/ GraphQL SDL documentation> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDefinition :: Lens.Lens' CreateType Types.String
ctDefinition = Lens.field @"definition"
{-# DEPRECATED ctDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctFormat :: Lens.Lens' CreateType Types.TypeDefinitionFormat
ctFormat = Lens.field @"format"
{-# DEPRECATED ctFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Core.FromJSON CreateType where
  toJSON CreateType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("definition" Core..= definition),
            Core.Just ("format" Core..= format)
          ]
      )

instance Core.AWSRequest CreateType where
  type Rs CreateType = CreateTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/types")),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTypeResponse'
            Core.<$> (x Core..:? "type") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTypeResponse' smart constructor.
data CreateTypeResponse = CreateTypeResponse'
  { -- | The @Type@ object.
    type' :: Core.Maybe Types.Type,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTypeResponse' value with any optional fields omitted.
mkCreateTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTypeResponse
mkCreateTypeResponse responseStatus =
  CreateTypeResponse' {type' = Core.Nothing, responseStatus}

-- | The @Type@ object.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsType :: Lens.Lens' CreateTypeResponse (Core.Maybe Types.Type)
ctrrsType = Lens.field @"type'"
{-# DEPRECATED ctrrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTypeResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
