{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.CloudDirectory.CreateSchema
    (
    -- * Creating a request
      CreateSchema (..)
    , mkCreateSchema
    -- ** Request lenses
    , csName

    -- * Destructuring the response
    , CreateSchemaResponse (..)
    , mkCreateSchemaResponse
    -- ** Response lenses
    , csrrsSchemaArn
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSchema' smart constructor.
newtype CreateSchema = CreateSchema'
  { name :: Types.SchemaName
    -- ^ The name that is associated with the schema. This is unique to each account and in each region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSchema' value with any optional fields omitted.
mkCreateSchema
    :: Types.SchemaName -- ^ 'name'
    -> CreateSchema
mkCreateSchema name = CreateSchema'{name}

-- | The name that is associated with the schema. This is unique to each account and in each region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateSchema Types.SchemaName
csName = Lens.field @"name"
{-# INLINEABLE csName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery CreateSchema where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSchema where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateSchema where
        toJSON CreateSchema{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest CreateSchema where
        type Rs CreateSchema = CreateSchemaResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/schema/create",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSchemaResponse' Core.<$>
                   (x Core..:? "SchemaArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { schemaArn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSchemaResponse' value with any optional fields omitted.
mkCreateSchemaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSchemaResponse
mkCreateSchemaResponse responseStatus
  = CreateSchemaResponse'{schemaArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSchemaArn :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.Arn)
csrrsSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE csrrsSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSchemaResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
