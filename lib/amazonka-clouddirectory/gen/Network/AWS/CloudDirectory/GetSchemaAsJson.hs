{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetSchemaAsJson
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a JSON representation of the schema. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_jsonformat.html#schemas_json JSON Schema Format> for more information.
module Network.AWS.CloudDirectory.GetSchemaAsJson
    (
    -- * Creating a request
      GetSchemaAsJson (..)
    , mkGetSchemaAsJson
    -- ** Request lenses
    , gsajSchemaArn

    -- * Destructuring the response
    , GetSchemaAsJsonResponse (..)
    , mkGetSchemaAsJsonResponse
    -- ** Response lenses
    , gsajrrsDocument
    , gsajrrsName
    , gsajrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSchemaAsJson' smart constructor.
newtype GetSchemaAsJson = GetSchemaAsJson'
  { schemaArn :: Types.Arn
    -- ^ The ARN of the schema to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaAsJson' value with any optional fields omitted.
mkGetSchemaAsJson
    :: Types.Arn -- ^ 'schemaArn'
    -> GetSchemaAsJson
mkGetSchemaAsJson schemaArn = GetSchemaAsJson'{schemaArn}

-- | The ARN of the schema to retrieve.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsajSchemaArn :: Lens.Lens' GetSchemaAsJson Types.Arn
gsajSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE gsajSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

instance Core.ToQuery GetSchemaAsJson where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSchemaAsJson where
        toHeaders GetSchemaAsJson{..}
          = Core.toHeaders "x-amz-data-partition" schemaArn

instance Core.FromJSON GetSchemaAsJson where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetSchemaAsJson where
        type Rs GetSchemaAsJson = GetSchemaAsJsonResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/schema/json",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSchemaAsJsonResponse' Core.<$>
                   (x Core..:? "Document") Core.<*> x Core..:? "Name" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSchemaAsJsonResponse' smart constructor.
data GetSchemaAsJsonResponse = GetSchemaAsJsonResponse'
  { document :: Core.Maybe Types.SchemaJsonDocument
    -- ^ The JSON representation of the schema document.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the retrieved schema.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaAsJsonResponse' value with any optional fields omitted.
mkGetSchemaAsJsonResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSchemaAsJsonResponse
mkGetSchemaAsJsonResponse responseStatus
  = GetSchemaAsJsonResponse'{document = Core.Nothing,
                             name = Core.Nothing, responseStatus}

-- | The JSON representation of the schema document.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsajrrsDocument :: Lens.Lens' GetSchemaAsJsonResponse (Core.Maybe Types.SchemaJsonDocument)
gsajrrsDocument = Lens.field @"document"
{-# INLINEABLE gsajrrsDocument #-}
{-# DEPRECATED document "Use generic-lens or generic-optics with 'document' instead"  #-}

-- | The name of the retrieved schema.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsajrrsName :: Lens.Lens' GetSchemaAsJsonResponse (Core.Maybe Types.Name)
gsajrrsName = Lens.field @"name"
{-# INLINEABLE gsajrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsajrrsResponseStatus :: Lens.Lens' GetSchemaAsJsonResponse Core.Int
gsajrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsajrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
