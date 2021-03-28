{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a 'Deployment' resource.
module Network.AWS.ApiGateway.GetDeployment
    (
    -- * Creating a request
      GetDeployment (..)
    , mkGetDeployment
    -- ** Request lenses
    , gRestApiId
    , gDeploymentId
    , gEmbed

     -- * Destructuring the response
    , Types.Deployment (..)
    , Types.mkDeployment
    -- ** Response lenses
    , Types.dApiSummary
    , Types.dCreatedDate
    , Types.dDescription
    , Types.dId
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to get information about a 'Deployment' resource.
--
-- /See:/ 'mkGetDeployment' smart constructor.
data GetDeployment = GetDeployment'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , deploymentId :: Core.Text
    -- ^ [Required] The identifier of the 'Deployment' resource to get information about.
  , embed :: Core.Maybe [Core.Text]
    -- ^ A query parameter to retrieve the specified embedded resources of the returned 'Deployment' resource in the response. In a REST API call, this @embed@ parameter value is a list of comma-separated strings, as in @GET /restapis/{restapi_id}/deployments/{deployment_id}?embed=var1,var2@ . The SDK and other platform-dependent libraries might use a different format for the list. Currently, this request supports only retrieval of the embedded API summary this way. Hence, the parameter value must be a single-valued list containing only the @"apisummary"@ string. For example, @GET /restapis/{restapi_id}/deployments/{deployment_id}?embed=apisummary@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeployment' value with any optional fields omitted.
mkGetDeployment
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'deploymentId'
    -> GetDeployment
mkGetDeployment restApiId deploymentId
  = GetDeployment'{restApiId, deploymentId, embed = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gRestApiId :: Lens.Lens' GetDeployment Core.Text
gRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the 'Deployment' resource to get information about.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDeploymentId :: Lens.Lens' GetDeployment Core.Text
gDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE gDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | A query parameter to retrieve the specified embedded resources of the returned 'Deployment' resource in the response. In a REST API call, this @embed@ parameter value is a list of comma-separated strings, as in @GET /restapis/{restapi_id}/deployments/{deployment_id}?embed=var1,var2@ . The SDK and other platform-dependent libraries might use a different format for the list. Currently, this request supports only retrieval of the embedded API summary this way. Hence, the parameter value must be a single-valued list containing only the @"apisummary"@ string. For example, @GET /restapis/{restapi_id}/deployments/{deployment_id}?embed=apisummary@ .
--
-- /Note:/ Consider using 'embed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gEmbed :: Lens.Lens' GetDeployment (Core.Maybe [Core.Text])
gEmbed = Lens.field @"embed"
{-# INLINEABLE gEmbed #-}
{-# DEPRECATED embed "Use generic-lens or generic-optics with 'embed' instead"  #-}

instance Core.ToQuery GetDeployment where
        toQuery GetDeployment{..}
          = Core.toQueryPair "embed"
              (Core.maybe Core.mempty (Core.toQueryList "member") embed)

instance Core.ToHeaders GetDeployment where
        toHeaders GetDeployment{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetDeployment where
        type Rs GetDeployment = Types.Deployment
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/deployments/"
                             Core.<> Core.toText deploymentId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
