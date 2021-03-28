{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.PutRestApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A feature of the API Gateway control service for updating an existing API with an input of external API definitions. The update can take the form of merging the supplied definition into the existing API or overwriting the existing API.
module Network.AWS.ApiGateway.PutRestApi
    (
    -- * Creating a request
      PutRestApi (..)
    , mkPutRestApi
    -- ** Request lenses
    , praRestApiId
    , praBody
    , praFailOnWarnings
    , praMode
    , praParameters

     -- * Destructuring the response
    , Types.RestApi (..)
    , Types.mkRestApi
    -- ** Response lenses
    , Types.raApiKeySource
    , Types.raBinaryMediaTypes
    , Types.raCreatedDate
    , Types.raDescription
    , Types.raDisableExecuteApiEndpoint
    , Types.raEndpointConfiguration
    , Types.raId
    , Types.raMinimumCompressionSize
    , Types.raName
    , Types.raPolicy
    , Types.raTags
    , Types.raVersion
    , Types.raWarnings
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A PUT request to update an existing API, with external API definitions specified as the request body.
--
-- /See:/ 'mkPutRestApi' smart constructor.
data PutRestApi = PutRestApi'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , body :: Core.ByteString
    -- ^ [Required] The PUT request body containing external API definitions. Currently, only OpenAPI definition JSON/YAML files are supported. The maximum size of the API definition file is 6MB.
  , failOnWarnings :: Core.Maybe Core.Bool
    -- ^ A query parameter to indicate whether to rollback the API update (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
  , mode :: Core.Maybe Types.PutMode
    -- ^ The @mode@ query parameter to specify the update mode. Valid values are "merge" and "overwrite". By default, the update mode is "merge".
  , parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Custom header parameters as part of the request. For example, to exclude 'DocumentationParts' from an imported API, set @ignore=documentation@ as a @parameters@ value, as in the AWS CLI command of @aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'@ .
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRestApi' value with any optional fields omitted.
mkPutRestApi
    :: Core.Text -- ^ 'restApiId'
    -> Core.ByteString -- ^ 'body'
    -> PutRestApi
mkPutRestApi restApiId body
  = PutRestApi'{restApiId, body, failOnWarnings = Core.Nothing,
                mode = Core.Nothing, parameters = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praRestApiId :: Lens.Lens' PutRestApi Core.Text
praRestApiId = Lens.field @"restApiId"
{-# INLINEABLE praRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The PUT request body containing external API definitions. Currently, only OpenAPI definition JSON/YAML files are supported. The maximum size of the API definition file is 6MB.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praBody :: Lens.Lens' PutRestApi Core.ByteString
praBody = Lens.field @"body"
{-# INLINEABLE praBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | A query parameter to indicate whether to rollback the API update (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
--
-- /Note:/ Consider using 'failOnWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praFailOnWarnings :: Lens.Lens' PutRestApi (Core.Maybe Core.Bool)
praFailOnWarnings = Lens.field @"failOnWarnings"
{-# INLINEABLE praFailOnWarnings #-}
{-# DEPRECATED failOnWarnings "Use generic-lens or generic-optics with 'failOnWarnings' instead"  #-}

-- | The @mode@ query parameter to specify the update mode. Valid values are "merge" and "overwrite". By default, the update mode is "merge".
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praMode :: Lens.Lens' PutRestApi (Core.Maybe Types.PutMode)
praMode = Lens.field @"mode"
{-# INLINEABLE praMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | Custom header parameters as part of the request. For example, to exclude 'DocumentationParts' from an imported API, set @ignore=documentation@ as a @parameters@ value, as in the AWS CLI command of @aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'@ .
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praParameters :: Lens.Lens' PutRestApi (Core.Maybe (Core.HashMap Core.Text Core.Text))
praParameters = Lens.field @"parameters"
{-# INLINEABLE praParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.ToQuery PutRestApi where
        toQuery PutRestApi{..}
          = Core.maybe Core.mempty (Core.toQueryPair "failonwarnings")
              failOnWarnings
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "mode") mode
              Core.<>
              Core.toQueryPair "parameters"
                (Core.maybe Core.mempty (Core.toQueryMap "entry" "key" "value")
                   parameters)

instance Core.ToHeaders PutRestApi where
        toHeaders PutRestApi{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest PutRestApi where
        type Rs PutRestApi = Types.RestApi
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/restapis/" Core.<> Core.toText restApiId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody body}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
