{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.ImportRestApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A feature of the API Gateway control service for creating a new API from an external API definition file.
module Network.AWS.ApiGateway.ImportRestApi
    (
    -- * Creating a request
      ImportRestApi (..)
    , mkImportRestApi
    -- ** Request lenses
    , iraBody
    , iraFailOnWarnings
    , iraParameters

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

-- | A POST request to import an API to API Gateway using an input of an API definition file.
--
-- /See:/ 'mkImportRestApi' smart constructor.
data ImportRestApi = ImportRestApi'
  { body :: Core.ByteString
    -- ^ [Required] The POST request body containing external API definitions. Currently, only OpenAPI definition JSON/YAML files are supported. The maximum size of the API definition file is 6MB.
  , failOnWarnings :: Core.Maybe Core.Bool
    -- ^ A query parameter to indicate whether to rollback the API creation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
  , parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A key-value map of context-specific query string parameters specifying the behavior of different API importing operations. The following shows operation-specific parameters and their supported values.
--
-- To exclude 'DocumentationParts' from the import, set @parameters@ as @ignore=documentation@ .
-- To configure the endpoint type, set @parameters@ as @endpointConfigurationTypes=EDGE@ , @endpointConfigurationTypes=REGIONAL@ , or @endpointConfigurationTypes=PRIVATE@ . The default endpoint type is @EDGE@ .
-- To handle imported @basepath@ , set @parameters@ as @basepath=ignore@ , @basepath=prepend@ or @basepath=split@ .
-- For example, the AWS CLI command to exclude documentation from the imported API is:
-- @@aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'@ @ The AWS CLI command to set the regional endpoint on the imported API is:
-- @@aws apigateway import-rest-api --parameters endpointConfigurationTypes=REGIONAL --body 'file:///path/to/imported-api-body.json'@ @ 
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportRestApi' value with any optional fields omitted.
mkImportRestApi
    :: Core.ByteString -- ^ 'body'
    -> ImportRestApi
mkImportRestApi body
  = ImportRestApi'{body, failOnWarnings = Core.Nothing,
                   parameters = Core.Nothing}

-- | [Required] The POST request body containing external API definitions. Currently, only OpenAPI definition JSON/YAML files are supported. The maximum size of the API definition file is 6MB.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iraBody :: Lens.Lens' ImportRestApi Core.ByteString
iraBody = Lens.field @"body"
{-# INLINEABLE iraBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | A query parameter to indicate whether to rollback the API creation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
--
-- /Note:/ Consider using 'failOnWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iraFailOnWarnings :: Lens.Lens' ImportRestApi (Core.Maybe Core.Bool)
iraFailOnWarnings = Lens.field @"failOnWarnings"
{-# INLINEABLE iraFailOnWarnings #-}
{-# DEPRECATED failOnWarnings "Use generic-lens or generic-optics with 'failOnWarnings' instead"  #-}

-- | A key-value map of context-specific query string parameters specifying the behavior of different API importing operations. The following shows operation-specific parameters and their supported values.
--
-- To exclude 'DocumentationParts' from the import, set @parameters@ as @ignore=documentation@ .
-- To configure the endpoint type, set @parameters@ as @endpointConfigurationTypes=EDGE@ , @endpointConfigurationTypes=REGIONAL@ , or @endpointConfigurationTypes=PRIVATE@ . The default endpoint type is @EDGE@ .
-- To handle imported @basepath@ , set @parameters@ as @basepath=ignore@ , @basepath=prepend@ or @basepath=split@ .
-- For example, the AWS CLI command to exclude documentation from the imported API is:
-- @@aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'@ @ The AWS CLI command to set the regional endpoint on the imported API is:
-- @@aws apigateway import-rest-api --parameters endpointConfigurationTypes=REGIONAL --body 'file:///path/to/imported-api-body.json'@ @ 
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iraParameters :: Lens.Lens' ImportRestApi (Core.Maybe (Core.HashMap Core.Text Core.Text))
iraParameters = Lens.field @"parameters"
{-# INLINEABLE iraParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.ToQuery ImportRestApi where
        toQuery ImportRestApi{..}
          = Core.maybe Core.mempty (Core.toQueryPair "failonwarnings")
              failOnWarnings
              Core.<>
              Core.toQueryPair "parameters"
                (Core.maybe Core.mempty (Core.toQueryMap "entry" "key" "value")
                   parameters)
              Core.<> Core.toQueryPair "mode=import" ("" :: Core.Text)

instance Core.ToHeaders ImportRestApi where
        toHeaders ImportRestApi{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest ImportRestApi where
        type Rs ImportRestApi = Types.RestApi
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/restapis",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody body}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
