{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateRestApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'RestApi' resource.
module Network.AWS.ApiGateway.CreateRestApi
    (
    -- * Creating a request
      CreateRestApi (..)
    , mkCreateRestApi
    -- ** Request lenses
    , craName
    , craApiKeySource
    , craBinaryMediaTypes
    , craCloneFrom
    , craDescription
    , craDisableExecuteApiEndpoint
    , craEndpointConfiguration
    , craMinimumCompressionSize
    , craPolicy
    , craTags
    , craVersion

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

-- | The POST Request to add a new 'RestApi' resource to your collection.
--
-- /See:/ 'mkCreateRestApi' smart constructor.
data CreateRestApi = CreateRestApi'
  { name :: Core.Text
    -- ^ [Required] The name of the 'RestApi' .
  , apiKeySource :: Core.Maybe Types.ApiKeySourceType
    -- ^ The source of the API key for metering requests according to a usage plan. Valid values are: 
--
--     * @HEADER@ to read the API key from the @X-API-Key@ header of a request. 
--
--     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
--
--
  , binaryMediaTypes :: Core.Maybe [Core.Text]
    -- ^ The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
  , cloneFrom :: Core.Maybe Core.Text
    -- ^ The ID of the 'RestApi' that you want to clone from.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the 'RestApi' .
  , disableExecuteApiEndpoint :: Core.Maybe Core.Bool
    -- ^ Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
  , endpointConfiguration :: Core.Maybe Types.EndpointConfiguration
    -- ^ The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
  , minimumCompressionSize :: Core.Maybe Core.Int
    -- ^ A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
  , policy :: Core.Maybe Core.Text
    -- ^ 'Method' 
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
  , version :: Core.Maybe Core.Text
    -- ^ A version identifier for the API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRestApi' value with any optional fields omitted.
mkCreateRestApi
    :: Core.Text -- ^ 'name'
    -> CreateRestApi
mkCreateRestApi name
  = CreateRestApi'{name, apiKeySource = Core.Nothing,
                   binaryMediaTypes = Core.Nothing, cloneFrom = Core.Nothing,
                   description = Core.Nothing,
                   disableExecuteApiEndpoint = Core.Nothing,
                   endpointConfiguration = Core.Nothing,
                   minimumCompressionSize = Core.Nothing, policy = Core.Nothing,
                   tags = Core.Nothing, version = Core.Nothing}

-- | [Required] The name of the 'RestApi' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craName :: Lens.Lens' CreateRestApi Core.Text
craName = Lens.field @"name"
{-# INLINEABLE craName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The source of the API key for metering requests according to a usage plan. Valid values are: 
--
--     * @HEADER@ to read the API key from the @X-API-Key@ header of a request. 
--
--     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
--
--
--
-- /Note:/ Consider using 'apiKeySource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craApiKeySource :: Lens.Lens' CreateRestApi (Core.Maybe Types.ApiKeySourceType)
craApiKeySource = Lens.field @"apiKeySource"
{-# INLINEABLE craApiKeySource #-}
{-# DEPRECATED apiKeySource "Use generic-lens or generic-optics with 'apiKeySource' instead"  #-}

-- | The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
--
-- /Note:/ Consider using 'binaryMediaTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craBinaryMediaTypes :: Lens.Lens' CreateRestApi (Core.Maybe [Core.Text])
craBinaryMediaTypes = Lens.field @"binaryMediaTypes"
{-# INLINEABLE craBinaryMediaTypes #-}
{-# DEPRECATED binaryMediaTypes "Use generic-lens or generic-optics with 'binaryMediaTypes' instead"  #-}

-- | The ID of the 'RestApi' that you want to clone from.
--
-- /Note:/ Consider using 'cloneFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craCloneFrom :: Lens.Lens' CreateRestApi (Core.Maybe Core.Text)
craCloneFrom = Lens.field @"cloneFrom"
{-# INLINEABLE craCloneFrom #-}
{-# DEPRECATED cloneFrom "Use generic-lens or generic-optics with 'cloneFrom' instead"  #-}

-- | The description of the 'RestApi' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craDescription :: Lens.Lens' CreateRestApi (Core.Maybe Core.Text)
craDescription = Lens.field @"description"
{-# INLINEABLE craDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
--
-- /Note:/ Consider using 'disableExecuteApiEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craDisableExecuteApiEndpoint :: Lens.Lens' CreateRestApi (Core.Maybe Core.Bool)
craDisableExecuteApiEndpoint = Lens.field @"disableExecuteApiEndpoint"
{-# INLINEABLE craDisableExecuteApiEndpoint #-}
{-# DEPRECATED disableExecuteApiEndpoint "Use generic-lens or generic-optics with 'disableExecuteApiEndpoint' instead"  #-}

-- | The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craEndpointConfiguration :: Lens.Lens' CreateRestApi (Core.Maybe Types.EndpointConfiguration)
craEndpointConfiguration = Lens.field @"endpointConfiguration"
{-# INLINEABLE craEndpointConfiguration #-}
{-# DEPRECATED endpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead"  #-}

-- | A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
--
-- /Note:/ Consider using 'minimumCompressionSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craMinimumCompressionSize :: Lens.Lens' CreateRestApi (Core.Maybe Core.Int)
craMinimumCompressionSize = Lens.field @"minimumCompressionSize"
{-# INLINEABLE craMinimumCompressionSize #-}
{-# DEPRECATED minimumCompressionSize "Use generic-lens or generic-optics with 'minimumCompressionSize' instead"  #-}

-- | 'Method' 
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craPolicy :: Lens.Lens' CreateRestApi (Core.Maybe Core.Text)
craPolicy = Lens.field @"policy"
{-# INLINEABLE craPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craTags :: Lens.Lens' CreateRestApi (Core.Maybe (Core.HashMap Core.Text Core.Text))
craTags = Lens.field @"tags"
{-# INLINEABLE craTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A version identifier for the API.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craVersion :: Lens.Lens' CreateRestApi (Core.Maybe Core.Text)
craVersion = Lens.field @"version"
{-# INLINEABLE craVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery CreateRestApi where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRestApi where
        toHeaders CreateRestApi{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateRestApi where
        toJSON CreateRestApi{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("apiKeySource" Core..=) Core.<$> apiKeySource,
                  ("binaryMediaTypes" Core..=) Core.<$> binaryMediaTypes,
                  ("cloneFrom" Core..=) Core.<$> cloneFrom,
                  ("description" Core..=) Core.<$> description,
                  ("disableExecuteApiEndpoint" Core..=) Core.<$>
                    disableExecuteApiEndpoint,
                  ("endpointConfiguration" Core..=) Core.<$> endpointConfiguration,
                  ("minimumCompressionSize" Core..=) Core.<$> minimumCompressionSize,
                  ("policy" Core..=) Core.<$> policy, ("tags" Core..=) Core.<$> tags,
                  ("version" Core..=) Core.<$> version])

instance Core.AWSRequest CreateRestApi where
        type Rs CreateRestApi = Types.RestApi
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/restapis",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
