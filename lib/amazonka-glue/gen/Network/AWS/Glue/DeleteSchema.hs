{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the entire schema set, including the schema set and all of its versions. To get the status of the delete operation, you can call @GetSchema@ API after the asynchronous call. Deleting a registry will disable all online operations for the schema, such as the @GetSchemaByDefinition@ , and @RegisterSchemaVersion@ APIs.
module Network.AWS.Glue.DeleteSchema
    (
    -- * Creating a request
      DeleteSchema (..)
    , mkDeleteSchema
    -- ** Request lenses
    , dsSchemaId

    -- * Destructuring the response
    , DeleteSchemaResponse (..)
    , mkDeleteSchemaResponse
    -- ** Response lenses
    , dsrrsSchemaArn
    , dsrrsSchemaName
    , dsrrsStatus
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSchema' smart constructor.
newtype DeleteSchema = DeleteSchema'
  { schemaId :: Types.SchemaId
    -- ^ This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSchema' value with any optional fields omitted.
mkDeleteSchema
    :: Types.SchemaId -- ^ 'schemaId'
    -> DeleteSchema
mkDeleteSchema schemaId = DeleteSchema'{schemaId}

-- | This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSchemaId :: Lens.Lens' DeleteSchema Types.SchemaId
dsSchemaId = Lens.field @"schemaId"
{-# INLINEABLE dsSchemaId #-}
{-# DEPRECATED schemaId "Use generic-lens or generic-optics with 'schemaId' instead"  #-}

instance Core.ToQuery DeleteSchema where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSchema where
        toHeaders DeleteSchema{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteSchema") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSchema where
        toJSON DeleteSchema{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SchemaId" Core..= schemaId)])

instance Core.AWSRequest DeleteSchema where
        type Rs DeleteSchema = DeleteSchemaResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteSchemaResponse' Core.<$>
                   (x Core..:? "SchemaArn") Core.<*> x Core..:? "SchemaName" Core.<*>
                     x Core..:? "Status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSchemaResponse' smart constructor.
data DeleteSchemaResponse = DeleteSchemaResponse'
  { schemaArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The Amazon Resource Name (ARN) of the schema being deleted.
  , schemaName :: Core.Maybe Types.SchemaName
    -- ^ The name of the schema being deleted.
  , status :: Core.Maybe Types.SchemaStatus
    -- ^ The status of the schema.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSchemaResponse' value with any optional fields omitted.
mkDeleteSchemaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSchemaResponse
mkDeleteSchemaResponse responseStatus
  = DeleteSchemaResponse'{schemaArn = Core.Nothing,
                          schemaName = Core.Nothing, status = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the schema being deleted.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSchemaArn :: Lens.Lens' DeleteSchemaResponse (Core.Maybe Types.GlueResourceArn)
dsrrsSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE dsrrsSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The name of the schema being deleted.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSchemaName :: Lens.Lens' DeleteSchemaResponse (Core.Maybe Types.SchemaName)
dsrrsSchemaName = Lens.field @"schemaName"
{-# INLINEABLE dsrrsSchemaName #-}
{-# DEPRECATED schemaName "Use generic-lens or generic-optics with 'schemaName' instead"  #-}

-- | The status of the schema.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsStatus :: Lens.Lens' DeleteSchemaResponse (Core.Maybe Types.SchemaStatus)
dsrrsStatus = Lens.field @"status"
{-# INLINEABLE dsrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteSchemaResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
