{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing 'Method' resource.
module Network.AWS.ApiGateway.GetMethod
    (
    -- * Creating a request
      GetMethod (..)
    , mkGetMethod
    -- ** Request lenses
    , gmfRestApiId
    , gmfResourceId
    , gmfHttpMethod

     -- * Destructuring the response
    , Types.Method (..)
    , Types.mkMethod
    -- ** Response lenses
    , Types.mApiKeyRequired
    , Types.mAuthorizationScopes
    , Types.mAuthorizationType
    , Types.mAuthorizerId
    , Types.mHttpMethod
    , Types.mMethodIntegration
    , Types.mMethodResponses
    , Types.mOperationName
    , Types.mRequestModels
    , Types.mRequestParameters
    , Types.mRequestValidatorId
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe an existing 'Method' resource.
--
-- /See:/ 'mkGetMethod' smart constructor.
data GetMethod = GetMethod'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] The 'Resource' identifier for the 'Method' resource.
  , httpMethod :: Core.Text
    -- ^ [Required] Specifies the method request's HTTP method type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMethod' value with any optional fields omitted.
mkGetMethod
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> GetMethod
mkGetMethod restApiId resourceId httpMethod
  = GetMethod'{restApiId, resourceId, httpMethod}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfRestApiId :: Lens.Lens' GetMethod Core.Text
gmfRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gmfRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The 'Resource' identifier for the 'Method' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfResourceId :: Lens.Lens' GetMethod Core.Text
gmfResourceId = Lens.field @"resourceId"
{-# INLINEABLE gmfResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] Specifies the method request's HTTP method type.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfHttpMethod :: Lens.Lens' GetMethod Core.Text
gmfHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE gmfHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

instance Core.ToQuery GetMethod where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMethod where
        toHeaders GetMethod{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetMethod where
        type Rs GetMethod = Types.Method
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
