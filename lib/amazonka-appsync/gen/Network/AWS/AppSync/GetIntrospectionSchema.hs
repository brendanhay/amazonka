{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetIntrospectionSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the introspection schema for a GraphQL API.
module Network.AWS.AppSync.GetIntrospectionSchema
    (
    -- * Creating a request
      GetIntrospectionSchema (..)
    , mkGetIntrospectionSchema
    -- ** Request lenses
    , gisApiId
    , gisFormat
    , gisIncludeDirectives

    -- * Destructuring the response
    , GetIntrospectionSchemaResponse (..)
    , mkGetIntrospectionSchemaResponse
    -- ** Response lenses
    , gisrrsSchema
    , gisrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetIntrospectionSchema' smart constructor.
data GetIntrospectionSchema = GetIntrospectionSchema'
  { apiId :: Core.Text
    -- ^ The API ID.
  , format :: Types.OutputType
    -- ^ The schema format: SDL or JSON.
  , includeDirectives :: Core.Maybe Core.Bool
    -- ^ A flag that specifies whether the schema introspection should contain directives.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIntrospectionSchema' value with any optional fields omitted.
mkGetIntrospectionSchema
    :: Core.Text -- ^ 'apiId'
    -> Types.OutputType -- ^ 'format'
    -> GetIntrospectionSchema
mkGetIntrospectionSchema apiId format
  = GetIntrospectionSchema'{apiId, format,
                            includeDirectives = Core.Nothing}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisApiId :: Lens.Lens' GetIntrospectionSchema Core.Text
gisApiId = Lens.field @"apiId"
{-# INLINEABLE gisApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The schema format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisFormat :: Lens.Lens' GetIntrospectionSchema Types.OutputType
gisFormat = Lens.field @"format"
{-# INLINEABLE gisFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | A flag that specifies whether the schema introspection should contain directives.
--
-- /Note:/ Consider using 'includeDirectives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisIncludeDirectives :: Lens.Lens' GetIntrospectionSchema (Core.Maybe Core.Bool)
gisIncludeDirectives = Lens.field @"includeDirectives"
{-# INLINEABLE gisIncludeDirectives #-}
{-# DEPRECATED includeDirectives "Use generic-lens or generic-optics with 'includeDirectives' instead"  #-}

instance Core.ToQuery GetIntrospectionSchema where
        toQuery GetIntrospectionSchema{..}
          = Core.toQueryPair "format" format Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "includeDirectives")
                includeDirectives

instance Core.ToHeaders GetIntrospectionSchema where
        toHeaders GetIntrospectionSchema{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetIntrospectionSchema where
        type Rs GetIntrospectionSchema = GetIntrospectionSchemaResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/schema",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBytes
              (\ s h x ->
                 GetIntrospectionSchemaResponse' Core.<$>
                   (Core.pure x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetIntrospectionSchemaResponse' smart constructor.
data GetIntrospectionSchemaResponse = GetIntrospectionSchemaResponse'
  { schema :: Core.Maybe Core.ByteString
    -- ^ The schema, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the <http://graphql.org/learn/schema/ GraphQL SDL documentation> .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIntrospectionSchemaResponse' value with any optional fields omitted.
mkGetIntrospectionSchemaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetIntrospectionSchemaResponse
mkGetIntrospectionSchemaResponse responseStatus
  = GetIntrospectionSchemaResponse'{schema = Core.Nothing,
                                    responseStatus}

-- | The schema, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the <http://graphql.org/learn/schema/ GraphQL SDL documentation> .
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsSchema :: Lens.Lens' GetIntrospectionSchemaResponse (Core.Maybe Core.ByteString)
gisrrsSchema = Lens.field @"schema"
{-# INLINEABLE gisrrsSchema #-}
{-# DEPRECATED schema "Use generic-lens or generic-optics with 'schema' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsResponseStatus :: Lens.Lens' GetIntrospectionSchemaResponse Core.Int
gisrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gisrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
