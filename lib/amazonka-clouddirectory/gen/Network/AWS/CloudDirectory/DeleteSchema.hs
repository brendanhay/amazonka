{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DeleteSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given schema. Schemas in a development and published state can only be deleted.
module Network.AWS.CloudDirectory.DeleteSchema
  ( -- * Creating a request
    DeleteSchema (..),
    mkDeleteSchema,

    -- ** Request lenses
    dsSchemaArn,

    -- * Destructuring the response
    DeleteSchemaResponse (..),
    mkDeleteSchemaResponse,

    -- ** Response lenses
    dsrrsSchemaArn,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSchema' smart constructor.
newtype DeleteSchema = DeleteSchema'
  { -- | The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
    schemaArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSchema' value with any optional fields omitted.
mkDeleteSchema ::
  -- | 'schemaArn'
  Types.Arn ->
  DeleteSchema
mkDeleteSchema schemaArn = DeleteSchema' {schemaArn}

-- | The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSchemaArn :: Lens.Lens' DeleteSchema Types.Arn
dsSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED dsSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

instance Core.FromJSON DeleteSchema where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DeleteSchema where
  type Rs DeleteSchema = DeleteSchemaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/schema",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amz-data-partition" schemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSchemaResponse'
            Core.<$> (x Core..:? "SchemaArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteSchemaResponse' smart constructor.
data DeleteSchemaResponse = DeleteSchemaResponse'
  { -- | The input ARN that is returned as part of the response. For more information, see 'arns' .
    schemaArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSchemaResponse' value with any optional fields omitted.
mkDeleteSchemaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSchemaResponse
mkDeleteSchemaResponse responseStatus =
  DeleteSchemaResponse' {schemaArn = Core.Nothing, responseStatus}

-- | The input ARN that is returned as part of the response. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSchemaArn :: Lens.Lens' DeleteSchemaResponse (Core.Maybe Types.Arn)
dsrrsSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED dsrrsSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteSchemaResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
