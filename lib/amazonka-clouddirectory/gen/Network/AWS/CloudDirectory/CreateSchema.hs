{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new schema in a development state. A schema can exist in three phases:
--
--
--     * /Development:/ This is a mutable phase of the schema. All new schemas are in the development phase. Once the schema is finalized, it can be published.
--
--
--     * /Published:/ Published schemas are immutable and have a version associated with them.
--
--
--     * /Applied:/ Applied schemas are mutable in a way that allows you to add new schema facets. You can also add new, nonrequired attributes to existing schema facets. You can apply only published schemas to directories.
module Network.AWS.CloudDirectory.CreateSchema
  ( -- * Creating a request
    CreateSchema (..),
    mkCreateSchema,

    -- ** Request lenses
    csName,

    -- * Destructuring the response
    CreateSchemaResponse (..),
    mkCreateSchemaResponse,

    -- ** Response lenses
    csrrsSchemaArn,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSchema' smart constructor.
newtype CreateSchema = CreateSchema'
  { -- | The name that is associated with the schema. This is unique to each account and in each region.
    name :: Types.SchemaName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSchema' value with any optional fields omitted.
mkCreateSchema ::
  -- | 'name'
  Types.SchemaName ->
  CreateSchema
mkCreateSchema name = CreateSchema' {name}

-- | The name that is associated with the schema. This is unique to each account and in each region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateSchema Types.SchemaName
csName = Lens.field @"name"
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON CreateSchema where
  toJSON CreateSchema {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest CreateSchema where
  type Rs CreateSchema = CreateSchemaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/schema/create",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSchemaResponse'
            Core.<$> (x Core..:? "SchemaArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
    schemaArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSchemaResponse' value with any optional fields omitted.
mkCreateSchemaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSchemaResponse
mkCreateSchemaResponse responseStatus =
  CreateSchemaResponse' {schemaArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSchemaArn :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.Arn)
csrrsSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED csrrsSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSchemaResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
