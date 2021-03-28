{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.RestApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.RestApi
  ( RestApi (..)
  -- * Smart constructor
  , mkRestApi
  -- * Lenses
  , raApiKeySource
  , raBinaryMediaTypes
  , raCreatedDate
  , raDescription
  , raDisableExecuteApiEndpoint
  , raEndpointConfiguration
  , raId
  , raMinimumCompressionSize
  , raName
  , raPolicy
  , raTags
  , raVersion
  , raWarnings
  ) where

import qualified Network.AWS.ApiGateway.Types.ApiKeySourceType as Types
import qualified Network.AWS.ApiGateway.Types.EndpointConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a REST API.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API> 
--
-- /See:/ 'mkRestApi' smart constructor.
data RestApi = RestApi'
  { apiKeySource :: Core.Maybe Types.ApiKeySourceType
    -- ^ The source of the API key for metering requests according to a usage plan. Valid values are: 
--
--     * @HEADER@ to read the API key from the @X-API-Key@ header of a request. 
--
--     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
--
--
  , binaryMediaTypes :: Core.Maybe [Core.Text]
    -- ^ The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the API was created.
  , description :: Core.Maybe Core.Text
    -- ^ The API's description.
  , disableExecuteApiEndpoint :: Core.Maybe Core.Bool
    -- ^ Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
  , endpointConfiguration :: Core.Maybe Types.EndpointConfiguration
    -- ^ The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
  , id :: Core.Maybe Core.Text
    -- ^ The API's identifier. This identifier is unique across all of your APIs in API Gateway.
  , minimumCompressionSize :: Core.Maybe Core.Int
    -- ^ A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
  , name :: Core.Maybe Core.Text
    -- ^ The API's name.
  , policy :: Core.Maybe Core.Text
    -- ^ A stringified JSON policy document that applies to this RestApi regardless of the caller and 'Method' configuration.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The collection of tags. Each tag element is associated with a given resource.
  , version :: Core.Maybe Core.Text
    -- ^ A version identifier for the API.
  , warnings :: Core.Maybe [Core.Text]
    -- ^ The warning messages reported when @failonwarnings@ is turned on during API import.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestApi' value with any optional fields omitted.
mkRestApi
    :: RestApi
mkRestApi
  = RestApi'{apiKeySource = Core.Nothing,
             binaryMediaTypes = Core.Nothing, createdDate = Core.Nothing,
             description = Core.Nothing,
             disableExecuteApiEndpoint = Core.Nothing,
             endpointConfiguration = Core.Nothing, id = Core.Nothing,
             minimumCompressionSize = Core.Nothing, name = Core.Nothing,
             policy = Core.Nothing, tags = Core.Nothing, version = Core.Nothing,
             warnings = Core.Nothing}

-- | The source of the API key for metering requests according to a usage plan. Valid values are: 
--
--     * @HEADER@ to read the API key from the @X-API-Key@ header of a request. 
--
--     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
--
--
--
-- /Note:/ Consider using 'apiKeySource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raApiKeySource :: Lens.Lens' RestApi (Core.Maybe Types.ApiKeySourceType)
raApiKeySource = Lens.field @"apiKeySource"
{-# INLINEABLE raApiKeySource #-}
{-# DEPRECATED apiKeySource "Use generic-lens or generic-optics with 'apiKeySource' instead"  #-}

-- | The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
--
-- /Note:/ Consider using 'binaryMediaTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raBinaryMediaTypes :: Lens.Lens' RestApi (Core.Maybe [Core.Text])
raBinaryMediaTypes = Lens.field @"binaryMediaTypes"
{-# INLINEABLE raBinaryMediaTypes #-}
{-# DEPRECATED binaryMediaTypes "Use generic-lens or generic-optics with 'binaryMediaTypes' instead"  #-}

-- | The timestamp when the API was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raCreatedDate :: Lens.Lens' RestApi (Core.Maybe Core.NominalDiffTime)
raCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE raCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The API's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raDescription :: Lens.Lens' RestApi (Core.Maybe Core.Text)
raDescription = Lens.field @"description"
{-# INLINEABLE raDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
--
-- /Note:/ Consider using 'disableExecuteApiEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raDisableExecuteApiEndpoint :: Lens.Lens' RestApi (Core.Maybe Core.Bool)
raDisableExecuteApiEndpoint = Lens.field @"disableExecuteApiEndpoint"
{-# INLINEABLE raDisableExecuteApiEndpoint #-}
{-# DEPRECATED disableExecuteApiEndpoint "Use generic-lens or generic-optics with 'disableExecuteApiEndpoint' instead"  #-}

-- | The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raEndpointConfiguration :: Lens.Lens' RestApi (Core.Maybe Types.EndpointConfiguration)
raEndpointConfiguration = Lens.field @"endpointConfiguration"
{-# INLINEABLE raEndpointConfiguration #-}
{-# DEPRECATED endpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead"  #-}

-- | The API's identifier. This identifier is unique across all of your APIs in API Gateway.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raId :: Lens.Lens' RestApi (Core.Maybe Core.Text)
raId = Lens.field @"id"
{-# INLINEABLE raId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
--
-- /Note:/ Consider using 'minimumCompressionSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raMinimumCompressionSize :: Lens.Lens' RestApi (Core.Maybe Core.Int)
raMinimumCompressionSize = Lens.field @"minimumCompressionSize"
{-# INLINEABLE raMinimumCompressionSize #-}
{-# DEPRECATED minimumCompressionSize "Use generic-lens or generic-optics with 'minimumCompressionSize' instead"  #-}

-- | The API's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raName :: Lens.Lens' RestApi (Core.Maybe Core.Text)
raName = Lens.field @"name"
{-# INLINEABLE raName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A stringified JSON policy document that applies to this RestApi regardless of the caller and 'Method' configuration.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raPolicy :: Lens.Lens' RestApi (Core.Maybe Core.Text)
raPolicy = Lens.field @"policy"
{-# INLINEABLE raPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTags :: Lens.Lens' RestApi (Core.Maybe (Core.HashMap Core.Text Core.Text))
raTags = Lens.field @"tags"
{-# INLINEABLE raTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A version identifier for the API.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raVersion :: Lens.Lens' RestApi (Core.Maybe Core.Text)
raVersion = Lens.field @"version"
{-# INLINEABLE raVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The warning messages reported when @failonwarnings@ is turned on during API import.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raWarnings :: Lens.Lens' RestApi (Core.Maybe [Core.Text])
raWarnings = Lens.field @"warnings"
{-# INLINEABLE raWarnings #-}
{-# DEPRECATED warnings "Use generic-lens or generic-optics with 'warnings' instead"  #-}

instance Core.FromJSON RestApi where
        parseJSON
          = Core.withObject "RestApi" Core.$
              \ x ->
                RestApi' Core.<$>
                  (x Core..:? "apiKeySource") Core.<*> x Core..:? "binaryMediaTypes"
                    Core.<*> x Core..:? "createdDate"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "disableExecuteApiEndpoint"
                    Core.<*> x Core..:? "endpointConfiguration"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "minimumCompressionSize"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "policy"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "version"
                    Core.<*> x Core..:? "warnings"
