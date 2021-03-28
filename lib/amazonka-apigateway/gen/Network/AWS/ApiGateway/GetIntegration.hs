{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetIntegration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the integration settings.
module Network.AWS.ApiGateway.GetIntegration
    (
    -- * Creating a request
      GetIntegration (..)
    , mkGetIntegration
    -- ** Request lenses
    , giRestApiId
    , giResourceId
    , giHttpMethod

     -- * Destructuring the response
    , Types.Integration (..)
    , Types.mkIntegration
    -- ** Response lenses
    , Types.iCacheKeyParameters
    , Types.iCacheNamespace
    , Types.iConnectionId
    , Types.iConnectionType
    , Types.iContentHandling
    , Types.iCredentials
    , Types.iHttpMethod
    , Types.iIntegrationResponses
    , Types.iPassthroughBehavior
    , Types.iRequestParameters
    , Types.iRequestTemplates
    , Types.iTimeoutInMillis
    , Types.iTlsConfig
    , Types.iType
    , Types.iUri
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to get the integration configuration.
--
-- /See:/ 'mkGetIntegration' smart constructor.
data GetIntegration = GetIntegration'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] Specifies a get integration request's resource identifier
  , httpMethod :: Core.Text
    -- ^ [Required] Specifies a get integration request's HTTP method.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIntegration' value with any optional fields omitted.
mkGetIntegration
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> GetIntegration
mkGetIntegration restApiId resourceId httpMethod
  = GetIntegration'{restApiId, resourceId, httpMethod}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giRestApiId :: Lens.Lens' GetIntegration Core.Text
giRestApiId = Lens.field @"restApiId"
{-# INLINEABLE giRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Specifies a get integration request's resource identifier
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giResourceId :: Lens.Lens' GetIntegration Core.Text
giResourceId = Lens.field @"resourceId"
{-# INLINEABLE giResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] Specifies a get integration request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giHttpMethod :: Lens.Lens' GetIntegration Core.Text
giHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE giHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

instance Core.ToQuery GetIntegration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetIntegration where
        toHeaders GetIntegration{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetIntegration where
        type Rs GetIntegration = Types.Integration
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/integration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
