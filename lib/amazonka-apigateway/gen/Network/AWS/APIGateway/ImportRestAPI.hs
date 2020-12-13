{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.ImportRestAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A feature of the API Gateway control service for creating a new API from an external API definition file.
module Network.AWS.APIGateway.ImportRestAPI
  ( -- * Creating a request
    ImportRestAPI (..),
    mkImportRestAPI,

    -- ** Request lenses
    iraBody,
    iraFailOnWarnings,
    iraParameters,

    -- * Destructuring the response
    RestAPI (..),
    mkRestAPI,

    -- ** Response lenses
    raMinimumCompressionSize,
    raDisableExecuteAPIEndpoint,
    raBinaryMediaTypes,
    raWarnings,
    raCreatedDate,
    raName,
    raVersion,
    raApiKeySource,
    raId,
    raPolicy,
    raEndpointConfiguration,
    raDescription,
    raTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A POST request to import an API to API Gateway using an input of an API definition file.
--
-- /See:/ 'mkImportRestAPI' smart constructor.
data ImportRestAPI = ImportRestAPI'
  { -- | [Required] The POST request body containing external API definitions. Currently, only OpenAPI definition JSON/YAML files are supported. The maximum size of the API definition file is 6MB.
    body :: Lude.ByteString,
    -- | A query parameter to indicate whether to rollback the API creation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
    failOnWarnings :: Lude.Maybe Lude.Bool,
    -- | A key-value map of context-specific query string parameters specifying the behavior of different API importing operations. The following shows operation-specific parameters and their supported values.
    --
    -- To exclude 'DocumentationParts' from the import, set @parameters@ as @ignore=documentation@ .
    -- To configure the endpoint type, set @parameters@ as @endpointConfigurationTypes=EDGE@ , @endpointConfigurationTypes=REGIONAL@ , or @endpointConfigurationTypes=PRIVATE@ . The default endpoint type is @EDGE@ .
    -- To handle imported @basepath@ , set @parameters@ as @basepath=ignore@ , @basepath=prepend@ or @basepath=split@ .
    -- For example, the AWS CLI command to exclude documentation from the imported API is:
    -- @@aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'@ @ The AWS CLI command to set the regional endpoint on the imported API is:
    -- @@aws apigateway import-rest-api --parameters endpointConfigurationTypes=REGIONAL --body 'file:///path/to/imported-api-body.json'@ @
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportRestAPI' with the minimum fields required to make a request.
--
-- * 'body' - [Required] The POST request body containing external API definitions. Currently, only OpenAPI definition JSON/YAML files are supported. The maximum size of the API definition file is 6MB.
-- * 'failOnWarnings' - A query parameter to indicate whether to rollback the API creation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
-- * 'parameters' - A key-value map of context-specific query string parameters specifying the behavior of different API importing operations. The following shows operation-specific parameters and their supported values.
--
-- To exclude 'DocumentationParts' from the import, set @parameters@ as @ignore=documentation@ .
-- To configure the endpoint type, set @parameters@ as @endpointConfigurationTypes=EDGE@ , @endpointConfigurationTypes=REGIONAL@ , or @endpointConfigurationTypes=PRIVATE@ . The default endpoint type is @EDGE@ .
-- To handle imported @basepath@ , set @parameters@ as @basepath=ignore@ , @basepath=prepend@ or @basepath=split@ .
-- For example, the AWS CLI command to exclude documentation from the imported API is:
-- @@aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'@ @ The AWS CLI command to set the regional endpoint on the imported API is:
-- @@aws apigateway import-rest-api --parameters endpointConfigurationTypes=REGIONAL --body 'file:///path/to/imported-api-body.json'@ @
mkImportRestAPI ::
  -- | 'body'
  Lude.ByteString ->
  ImportRestAPI
mkImportRestAPI pBody_ =
  ImportRestAPI'
    { body = pBody_,
      failOnWarnings = Lude.Nothing,
      parameters = Lude.Nothing
    }

-- | [Required] The POST request body containing external API definitions. Currently, only OpenAPI definition JSON/YAML files are supported. The maximum size of the API definition file is 6MB.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iraBody :: Lens.Lens' ImportRestAPI Lude.ByteString
iraBody = Lens.lens (body :: ImportRestAPI -> Lude.ByteString) (\s a -> s {body = a} :: ImportRestAPI)
{-# DEPRECATED iraBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | A query parameter to indicate whether to rollback the API creation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
--
-- /Note:/ Consider using 'failOnWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iraFailOnWarnings :: Lens.Lens' ImportRestAPI (Lude.Maybe Lude.Bool)
iraFailOnWarnings = Lens.lens (failOnWarnings :: ImportRestAPI -> Lude.Maybe Lude.Bool) (\s a -> s {failOnWarnings = a} :: ImportRestAPI)
{-# DEPRECATED iraFailOnWarnings "Use generic-lens or generic-optics with 'failOnWarnings' instead." #-}

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
iraParameters :: Lens.Lens' ImportRestAPI (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
iraParameters = Lens.lens (parameters :: ImportRestAPI -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: ImportRestAPI)
{-# DEPRECATED iraParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Lude.AWSRequest ImportRestAPI where
  type Rs ImportRestAPI = RestAPI
  request = Req.postBody apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToBody ImportRestAPI where
  toBody = Lude.toBody Lude.. body

instance Lude.ToHeaders ImportRestAPI where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath ImportRestAPI where
  toPath = Lude.const "/restapis"

instance Lude.ToQuery ImportRestAPI where
  toQuery ImportRestAPI' {..} =
    Lude.mconcat
      [ "failonwarnings" Lude.=: failOnWarnings,
        "parameters"
          Lude.=: Lude.toQuery
            (Lude.toQueryMap "entry" "key" "value" Lude.<$> parameters),
        "mode=import"
      ]
