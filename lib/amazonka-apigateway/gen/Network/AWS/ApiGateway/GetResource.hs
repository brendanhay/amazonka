{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about a resource.
module Network.AWS.ApiGateway.GetResource
    (
    -- * Creating a request
      GetResource (..)
    , mkGetResource
    -- ** Request lenses
    , grRestApiId
    , grResourceId
    , grEmbed

     -- * Destructuring the response
    , Types.Resource (..)
    , Types.mkResource
    -- ** Response lenses
    , Types.rId
    , Types.rParentId
    , Types.rPath
    , Types.rPathPart
    , Types.rResourceMethods
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to list information about a resource.
--
-- /See:/ 'mkGetResource' smart constructor.
data GetResource = GetResource'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] The identifier for the 'Resource' resource.
  , embed :: Core.Maybe [Core.Text]
    -- ^ A query parameter to retrieve the specified resources embedded in the returned 'Resource' representation in the response. This @embed@ parameter value is a list of comma-separated strings. Currently, the request supports only retrieval of the embedded 'Method' resources this way. The query parameter value must be a single-valued list and contain the @"methods"@ string. For example, @GET /restapis/{restapi_id}/resources/{resource_id}?embed=methods@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResource' value with any optional fields omitted.
mkGetResource
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> GetResource
mkGetResource restApiId resourceId
  = GetResource'{restApiId, resourceId, embed = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRestApiId :: Lens.Lens' GetResource Core.Text
grRestApiId = Lens.field @"restApiId"
{-# INLINEABLE grRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier for the 'Resource' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grResourceId :: Lens.Lens' GetResource Core.Text
grResourceId = Lens.field @"resourceId"
{-# INLINEABLE grResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | A query parameter to retrieve the specified resources embedded in the returned 'Resource' representation in the response. This @embed@ parameter value is a list of comma-separated strings. Currently, the request supports only retrieval of the embedded 'Method' resources this way. The query parameter value must be a single-valued list and contain the @"methods"@ string. For example, @GET /restapis/{restapi_id}/resources/{resource_id}?embed=methods@ .
--
-- /Note:/ Consider using 'embed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grEmbed :: Lens.Lens' GetResource (Core.Maybe [Core.Text])
grEmbed = Lens.field @"embed"
{-# INLINEABLE grEmbed #-}
{-# DEPRECATED embed "Use generic-lens or generic-optics with 'embed' instead"  #-}

instance Core.ToQuery GetResource where
        toQuery GetResource{..}
          = Core.toQueryPair "embed"
              (Core.maybe Core.mempty (Core.toQueryList "member") embed)

instance Core.ToHeaders GetResource where
        toHeaders GetResource{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetResource where
        type Rs GetResource = Types.Resource
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
