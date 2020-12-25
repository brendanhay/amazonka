{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpdateSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schema name with a new name. Only development schema names can be updated.
module Network.AWS.CloudDirectory.UpdateSchema
  ( -- * Creating a request
    UpdateSchema (..),
    mkUpdateSchema,

    -- ** Request lenses
    usSchemaArn,
    usName,

    -- * Destructuring the response
    UpdateSchemaResponse (..),
    mkUpdateSchemaResponse,

    -- ** Response lenses
    usrrsSchemaArn,
    usrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { -- | The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
    schemaArn :: Types.Arn,
    -- | The name of the schema.
    name :: Types.SchemaName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSchema' value with any optional fields omitted.
mkUpdateSchema ::
  -- | 'schemaArn'
  Types.Arn ->
  -- | 'name'
  Types.SchemaName ->
  UpdateSchema
mkUpdateSchema schemaArn name = UpdateSchema' {schemaArn, name}

-- | The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSchemaArn :: Lens.Lens' UpdateSchema Types.Arn
usSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED usSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usName :: Lens.Lens' UpdateSchema Types.SchemaName
usName = Lens.field @"name"
{-# DEPRECATED usName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateSchema where
  toJSON UpdateSchema {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest UpdateSchema where
  type Rs UpdateSchema = UpdateSchemaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/schema/update",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amz-data-partition" schemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSchemaResponse'
            Core.<$> (x Core..:? "SchemaArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { -- | The ARN that is associated with the updated schema. For more information, see 'arns' .
    schemaArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSchemaResponse' value with any optional fields omitted.
mkUpdateSchemaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSchemaResponse
mkUpdateSchemaResponse responseStatus =
  UpdateSchemaResponse' {schemaArn = Core.Nothing, responseStatus}

-- | The ARN that is associated with the updated schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsSchemaArn :: Lens.Lens' UpdateSchemaResponse (Core.Maybe Types.Arn)
usrrsSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED usrrsSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateSchemaResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
