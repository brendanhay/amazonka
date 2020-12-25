{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.StartSchemaCreation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new schema to your GraphQL API.
--
-- This operation is asynchronous. Use to determine when it has completed.
module Network.AWS.AppSync.StartSchemaCreation
  ( -- * Creating a request
    StartSchemaCreation (..),
    mkStartSchemaCreation,

    -- ** Request lenses
    sscApiId,
    sscDefinition,

    -- * Destructuring the response
    StartSchemaCreationResponse (..),
    mkStartSchemaCreationResponse,

    -- ** Response lenses
    sscrrsStatus,
    sscrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartSchemaCreation' smart constructor.
data StartSchemaCreation = StartSchemaCreation'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The schema definition, in GraphQL schema language format.
    definition :: Core.Base64
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSchemaCreation' value with any optional fields omitted.
mkStartSchemaCreation ::
  -- | 'apiId'
  Types.String ->
  -- | 'definition'
  Core.Base64 ->
  StartSchemaCreation
mkStartSchemaCreation apiId definition =
  StartSchemaCreation' {apiId, definition}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscApiId :: Lens.Lens' StartSchemaCreation Types.String
sscApiId = Lens.field @"apiId"
{-# DEPRECATED sscApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The schema definition, in GraphQL schema language format.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscDefinition :: Lens.Lens' StartSchemaCreation Core.Base64
sscDefinition = Lens.field @"definition"
{-# DEPRECATED sscDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

instance Core.FromJSON StartSchemaCreation where
  toJSON StartSchemaCreation {..} =
    Core.object
      (Core.catMaybes [Core.Just ("definition" Core..= definition)])

instance Core.AWSRequest StartSchemaCreation where
  type Rs StartSchemaCreation = StartSchemaCreationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId)
                Core.<> ("/schemacreation")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSchemaCreationResponse'
            Core.<$> (x Core..:? "status") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartSchemaCreationResponse' smart constructor.
data StartSchemaCreationResponse = StartSchemaCreationResponse'
  { -- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
    status :: Core.Maybe Types.SchemaStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSchemaCreationResponse' value with any optional fields omitted.
mkStartSchemaCreationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartSchemaCreationResponse
mkStartSchemaCreationResponse responseStatus =
  StartSchemaCreationResponse'
    { status = Core.Nothing,
      responseStatus
    }

-- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscrrsStatus :: Lens.Lens' StartSchemaCreationResponse (Core.Maybe Types.SchemaStatus)
sscrrsStatus = Lens.field @"status"
{-# DEPRECATED sscrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscrrsResponseStatus :: Lens.Lens' StartSchemaCreationResponse Core.Int
sscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
