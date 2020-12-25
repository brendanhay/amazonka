{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSchemaVersionsDiff
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the schema version difference in the specified difference type between two stored schema versions in the Schema Registry.
--
-- This API allows you to compare two schema versions between two schema definitions under the same schema.
module Network.AWS.Glue.GetSchemaVersionsDiff
  ( -- * Creating a request
    GetSchemaVersionsDiff (..),
    mkGetSchemaVersionsDiff,

    -- ** Request lenses
    gsvdSchemaId,
    gsvdFirstSchemaVersionNumber,
    gsvdSecondSchemaVersionNumber,
    gsvdSchemaDiffType,

    -- * Destructuring the response
    GetSchemaVersionsDiffResponse (..),
    mkGetSchemaVersionsDiffResponse,

    -- ** Response lenses
    gsvdrrsDiff,
    gsvdrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSchemaVersionsDiff' smart constructor.
data GetSchemaVersionsDiff = GetSchemaVersionsDiff'
  { -- | This is a wrapper structure to contain schema identity fields. The structure contains:
    --
    --
    --     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
    --
    --
    --     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
    schemaId :: Types.SchemaId,
    -- | The first of the two schema versions to be compared.
    firstSchemaVersionNumber :: Types.SchemaVersionNumber,
    -- | The second of the two schema versions to be compared.
    secondSchemaVersionNumber :: Types.SchemaVersionNumber,
    -- | Refers to @SYNTAX_DIFF@ , which is the currently supported diff type.
    schemaDiffType :: Types.SchemaDiffType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaVersionsDiff' value with any optional fields omitted.
mkGetSchemaVersionsDiff ::
  -- | 'schemaId'
  Types.SchemaId ->
  -- | 'firstSchemaVersionNumber'
  Types.SchemaVersionNumber ->
  -- | 'secondSchemaVersionNumber'
  Types.SchemaVersionNumber ->
  -- | 'schemaDiffType'
  Types.SchemaDiffType ->
  GetSchemaVersionsDiff
mkGetSchemaVersionsDiff
  schemaId
  firstSchemaVersionNumber
  secondSchemaVersionNumber
  schemaDiffType =
    GetSchemaVersionsDiff'
      { schemaId,
        firstSchemaVersionNumber,
        secondSchemaVersionNumber,
        schemaDiffType
      }

-- | This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdSchemaId :: Lens.Lens' GetSchemaVersionsDiff Types.SchemaId
gsvdSchemaId = Lens.field @"schemaId"
{-# DEPRECATED gsvdSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The first of the two schema versions to be compared.
--
-- /Note:/ Consider using 'firstSchemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdFirstSchemaVersionNumber :: Lens.Lens' GetSchemaVersionsDiff Types.SchemaVersionNumber
gsvdFirstSchemaVersionNumber = Lens.field @"firstSchemaVersionNumber"
{-# DEPRECATED gsvdFirstSchemaVersionNumber "Use generic-lens or generic-optics with 'firstSchemaVersionNumber' instead." #-}

-- | The second of the two schema versions to be compared.
--
-- /Note:/ Consider using 'secondSchemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdSecondSchemaVersionNumber :: Lens.Lens' GetSchemaVersionsDiff Types.SchemaVersionNumber
gsvdSecondSchemaVersionNumber = Lens.field @"secondSchemaVersionNumber"
{-# DEPRECATED gsvdSecondSchemaVersionNumber "Use generic-lens or generic-optics with 'secondSchemaVersionNumber' instead." #-}

-- | Refers to @SYNTAX_DIFF@ , which is the currently supported diff type.
--
-- /Note:/ Consider using 'schemaDiffType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdSchemaDiffType :: Lens.Lens' GetSchemaVersionsDiff Types.SchemaDiffType
gsvdSchemaDiffType = Lens.field @"schemaDiffType"
{-# DEPRECATED gsvdSchemaDiffType "Use generic-lens or generic-optics with 'schemaDiffType' instead." #-}

instance Core.FromJSON GetSchemaVersionsDiff where
  toJSON GetSchemaVersionsDiff {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaId" Core..= schemaId),
            Core.Just
              ("FirstSchemaVersionNumber" Core..= firstSchemaVersionNumber),
            Core.Just
              ("SecondSchemaVersionNumber" Core..= secondSchemaVersionNumber),
            Core.Just ("SchemaDiffType" Core..= schemaDiffType)
          ]
      )

instance Core.AWSRequest GetSchemaVersionsDiff where
  type Rs GetSchemaVersionsDiff = GetSchemaVersionsDiffResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetSchemaVersionsDiff")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaVersionsDiffResponse'
            Core.<$> (x Core..:? "Diff") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSchemaVersionsDiffResponse' smart constructor.
data GetSchemaVersionsDiffResponse = GetSchemaVersionsDiffResponse'
  { -- | The difference between schemas as a string in JsonPatch format.
    diff :: Core.Maybe Types.SchemaDefinitionDiff,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaVersionsDiffResponse' value with any optional fields omitted.
mkGetSchemaVersionsDiffResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSchemaVersionsDiffResponse
mkGetSchemaVersionsDiffResponse responseStatus =
  GetSchemaVersionsDiffResponse'
    { diff = Core.Nothing,
      responseStatus
    }

-- | The difference between schemas as a string in JsonPatch format.
--
-- /Note:/ Consider using 'diff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdrrsDiff :: Lens.Lens' GetSchemaVersionsDiffResponse (Core.Maybe Types.SchemaDefinitionDiff)
gsvdrrsDiff = Lens.field @"diff"
{-# DEPRECATED gsvdrrsDiff "Use generic-lens or generic-optics with 'diff' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdrrsResponseStatus :: Lens.Lens' GetSchemaVersionsDiffResponse Core.Int
gsvdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsvdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
