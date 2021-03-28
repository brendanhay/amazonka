{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.PutSchemaFromJson
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a schema to be updated using JSON upload. Only available for development schemas. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_jsonformat.html#schemas_json JSON Schema Format> for more information.
module Network.AWS.CloudDirectory.PutSchemaFromJson
    (
    -- * Creating a request
      PutSchemaFromJson (..)
    , mkPutSchemaFromJson
    -- ** Request lenses
    , psfjSchemaArn
    , psfjDocument

    -- * Destructuring the response
    , PutSchemaFromJsonResponse (..)
    , mkPutSchemaFromJsonResponse
    -- ** Response lenses
    , psfjrrsArn
    , psfjrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutSchemaFromJson' smart constructor.
data PutSchemaFromJson = PutSchemaFromJson'
  { schemaArn :: Types.Arn
    -- ^ The ARN of the schema to update.
  , document :: Types.SchemaJsonDocument
    -- ^ The replacement JSON schema.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSchemaFromJson' value with any optional fields omitted.
mkPutSchemaFromJson
    :: Types.Arn -- ^ 'schemaArn'
    -> Types.SchemaJsonDocument -- ^ 'document'
    -> PutSchemaFromJson
mkPutSchemaFromJson schemaArn document
  = PutSchemaFromJson'{schemaArn, document}

-- | The ARN of the schema to update.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfjSchemaArn :: Lens.Lens' PutSchemaFromJson Types.Arn
psfjSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE psfjSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The replacement JSON schema.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfjDocument :: Lens.Lens' PutSchemaFromJson Types.SchemaJsonDocument
psfjDocument = Lens.field @"document"
{-# INLINEABLE psfjDocument #-}
{-# DEPRECATED document "Use generic-lens or generic-optics with 'document' instead"  #-}

instance Core.ToQuery PutSchemaFromJson where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutSchemaFromJson where
        toHeaders PutSchemaFromJson{..}
          = Core.toHeaders "x-amz-data-partition" schemaArn

instance Core.FromJSON PutSchemaFromJson where
        toJSON PutSchemaFromJson{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Document" Core..= document)])

instance Core.AWSRequest PutSchemaFromJson where
        type Rs PutSchemaFromJson = PutSchemaFromJsonResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/schema/json",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutSchemaFromJsonResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutSchemaFromJsonResponse' smart constructor.
data PutSchemaFromJsonResponse = PutSchemaFromJsonResponse'
  { arn :: Core.Maybe Types.Arn
    -- ^ The ARN of the schema to update.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSchemaFromJsonResponse' value with any optional fields omitted.
mkPutSchemaFromJsonResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutSchemaFromJsonResponse
mkPutSchemaFromJsonResponse responseStatus
  = PutSchemaFromJsonResponse'{arn = Core.Nothing, responseStatus}

-- | The ARN of the schema to update.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfjrrsArn :: Lens.Lens' PutSchemaFromJsonResponse (Core.Maybe Types.Arn)
psfjrrsArn = Lens.field @"arn"
{-# INLINEABLE psfjrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfjrrsResponseStatus :: Lens.Lens' PutSchemaFromJsonResponse Core.Int
psfjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE psfjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
