{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateSchema (..)
    , mkUpdateSchema
    -- ** Request lenses
    , usSchemaArn
    , usName

    -- * Destructuring the response
    , UpdateSchemaResponse (..)
    , mkUpdateSchemaResponse
    -- ** Response lenses
    , usrrsSchemaArn
    , usrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { schemaArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
  , name :: Types.SchemaName
    -- ^ The name of the schema.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSchema' value with any optional fields omitted.
mkUpdateSchema
    :: Types.Arn -- ^ 'schemaArn'
    -> Types.SchemaName -- ^ 'name'
    -> UpdateSchema
mkUpdateSchema schemaArn name = UpdateSchema'{schemaArn, name}

-- | The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSchemaArn :: Lens.Lens' UpdateSchema Types.Arn
usSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE usSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usName :: Lens.Lens' UpdateSchema Types.SchemaName
usName = Lens.field @"name"
{-# INLINEABLE usName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateSchema where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSchema where
        toHeaders UpdateSchema{..}
          = Core.toHeaders "x-amz-data-partition" schemaArn

instance Core.FromJSON UpdateSchema where
        toJSON UpdateSchema{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest UpdateSchema where
        type Rs UpdateSchema = UpdateSchemaResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/schema/update",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateSchemaResponse' Core.<$>
                   (x Core..:? "SchemaArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { schemaArn :: Core.Maybe Types.Arn
    -- ^ The ARN that is associated with the updated schema. For more information, see 'arns' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSchemaResponse' value with any optional fields omitted.
mkUpdateSchemaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSchemaResponse
mkUpdateSchemaResponse responseStatus
  = UpdateSchemaResponse'{schemaArn = Core.Nothing, responseStatus}

-- | The ARN that is associated with the updated schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsSchemaArn :: Lens.Lens' UpdateSchemaResponse (Core.Maybe Types.Arn)
usrrsSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE usrrsSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateSchemaResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
