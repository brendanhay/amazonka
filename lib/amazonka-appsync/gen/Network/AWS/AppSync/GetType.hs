{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @Type@ object.
module Network.AWS.AppSync.GetType
    (
    -- * Creating a request
      GetType (..)
    , mkGetType
    -- ** Request lenses
    , gtApiId
    , gtTypeName
    , gtFormat

    -- * Destructuring the response
    , GetTypeResponse (..)
    , mkGetTypeResponse
    -- ** Response lenses
    , gtrrsType
    , gtrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetType' smart constructor.
data GetType = GetType'
  { apiId :: Core.Text
    -- ^ The API ID.
  , typeName :: Types.ResourceName
    -- ^ The type name.
  , format :: Types.TypeDefinitionFormat
    -- ^ The type format: SDL or JSON.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetType' value with any optional fields omitted.
mkGetType
    :: Core.Text -- ^ 'apiId'
    -> Types.ResourceName -- ^ 'typeName'
    -> Types.TypeDefinitionFormat -- ^ 'format'
    -> GetType
mkGetType apiId typeName format = GetType'{apiId, typeName, format}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtApiId :: Lens.Lens' GetType Core.Text
gtApiId = Lens.field @"apiId"
{-# INLINEABLE gtApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTypeName :: Lens.Lens' GetType Types.ResourceName
gtTypeName = Lens.field @"typeName"
{-# INLINEABLE gtTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | The type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtFormat :: Lens.Lens' GetType Types.TypeDefinitionFormat
gtFormat = Lens.field @"format"
{-# INLINEABLE gtFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

instance Core.ToQuery GetType where
        toQuery GetType{..} = Core.toQueryPair "format" format

instance Core.ToHeaders GetType where
        toHeaders GetType{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetType where
        type Rs GetType = GetTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/types/" Core.<>
                             Core.toText typeName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTypeResponse' Core.<$>
                   (x Core..:? "type") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTypeResponse' smart constructor.
data GetTypeResponse = GetTypeResponse'
  { type' :: Core.Maybe Types.Type
    -- ^ The @Type@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTypeResponse' value with any optional fields omitted.
mkGetTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTypeResponse
mkGetTypeResponse responseStatus
  = GetTypeResponse'{type' = Core.Nothing, responseStatus}

-- | The @Type@ object.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsType :: Lens.Lens' GetTypeResponse (Core.Maybe Types.Type)
gtrrsType = Lens.field @"type'"
{-# INLINEABLE gtrrsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTypeResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
