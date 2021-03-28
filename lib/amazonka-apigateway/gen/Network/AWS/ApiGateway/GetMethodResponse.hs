{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetMethodResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a 'MethodResponse' resource.
module Network.AWS.ApiGateway.GetMethodResponse
    (
    -- * Creating a request
      GetMethodResponse (..)
    , mkGetMethodResponse
    -- ** Request lenses
    , gmrRestApiId
    , gmrResourceId
    , gmrHttpMethod
    , gmrStatusCode

     -- * Destructuring the response
    , Types.MethodResponse (..)
    , Types.mkMethodResponse
    -- ** Response lenses
    , Types.mrResponseModels
    , Types.mrResponseParameters
    , Types.mrStatusCode
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe a 'MethodResponse' resource.
--
-- /See:/ 'mkGetMethodResponse' smart constructor.
data GetMethodResponse = GetMethodResponse'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] The 'Resource' identifier for the 'MethodResponse' resource.
  , httpMethod :: Core.Text
    -- ^ [Required] The HTTP verb of the 'Method' resource.
  , statusCode :: Types.StatusCode
    -- ^ [Required] The status code for the 'MethodResponse' resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMethodResponse' value with any optional fields omitted.
mkGetMethodResponse
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> Types.StatusCode -- ^ 'statusCode'
    -> GetMethodResponse
mkGetMethodResponse restApiId resourceId httpMethod statusCode
  = GetMethodResponse'{restApiId, resourceId, httpMethod, statusCode}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrRestApiId :: Lens.Lens' GetMethodResponse Core.Text
gmrRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gmrRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The 'Resource' identifier for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrResourceId :: Lens.Lens' GetMethodResponse Core.Text
gmrResourceId = Lens.field @"resourceId"
{-# INLINEABLE gmrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrHttpMethod :: Lens.Lens' GetMethodResponse Core.Text
gmrHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE gmrHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | [Required] The status code for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrStatusCode :: Lens.Lens' GetMethodResponse Types.StatusCode
gmrStatusCode = Lens.field @"statusCode"
{-# INLINEABLE gmrStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

instance Core.ToQuery GetMethodResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMethodResponse where
        toHeaders GetMethodResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetMethodResponse where
        type Rs GetMethodResponse = Types.MethodResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/responses/"
                             Core.<> Core.toText statusCode,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
