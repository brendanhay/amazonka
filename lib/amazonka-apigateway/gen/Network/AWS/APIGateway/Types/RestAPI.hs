-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.RestAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.RestAPI
  ( RestAPI (..),

    -- * Smart constructor
    mkRestAPI,

    -- * Lenses
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

import Network.AWS.APIGateway.Types.APIKeySourceType
import Network.AWS.APIGateway.Types.EndpointConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a REST API.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API>
--
-- /See:/ 'mkRestAPI' smart constructor.
data RestAPI = RestAPI'
  { minimumCompressionSize ::
      Lude.Maybe Lude.Int,
    disableExecuteAPIEndpoint :: Lude.Maybe Lude.Bool,
    binaryMediaTypes :: Lude.Maybe [Lude.Text],
    warnings :: Lude.Maybe [Lude.Text],
    createdDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    apiKeySource :: Lude.Maybe APIKeySourceType,
    id :: Lude.Maybe Lude.Text,
    policy :: Lude.Maybe Lude.Text,
    endpointConfiguration :: Lude.Maybe EndpointConfiguration,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestAPI' with the minimum fields required to make a request.
--
-- * 'apiKeySource' - The source of the API key for metering requests according to a usage plan. Valid values are:
--
--     * @HEADER@ to read the API key from the @X-API-Key@ header of a request.
--
--     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
--
--
-- * 'binaryMediaTypes' - The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
-- * 'createdDate' - The timestamp when the API was created.
-- * 'description' - The API's description.
-- * 'disableExecuteAPIEndpoint' - Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
-- * 'endpointConfiguration' - The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
-- * 'id' - The API's identifier. This identifier is unique across all of your APIs in API Gateway.
-- * 'minimumCompressionSize' - A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
-- * 'name' - The API's name.
-- * 'policy' - A stringified JSON policy document that applies to this RestApi regardless of the caller and 'Method' configuration.
-- * 'tags' - The collection of tags. Each tag element is associated with a given resource.
-- * 'version' - A version identifier for the API.
-- * 'warnings' - The warning messages reported when @failonwarnings@ is turned on during API import.
mkRestAPI ::
  RestAPI
mkRestAPI =
  RestAPI'
    { minimumCompressionSize = Lude.Nothing,
      disableExecuteAPIEndpoint = Lude.Nothing,
      binaryMediaTypes = Lude.Nothing,
      warnings = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      apiKeySource = Lude.Nothing,
      id = Lude.Nothing,
      policy = Lude.Nothing,
      endpointConfiguration = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
--
-- /Note:/ Consider using 'minimumCompressionSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raMinimumCompressionSize :: Lens.Lens' RestAPI (Lude.Maybe Lude.Int)
raMinimumCompressionSize = Lens.lens (minimumCompressionSize :: RestAPI -> Lude.Maybe Lude.Int) (\s a -> s {minimumCompressionSize = a} :: RestAPI)
{-# DEPRECATED raMinimumCompressionSize "Use generic-lens or generic-optics with 'minimumCompressionSize' instead." #-}

-- | Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
--
-- /Note:/ Consider using 'disableExecuteAPIEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raDisableExecuteAPIEndpoint :: Lens.Lens' RestAPI (Lude.Maybe Lude.Bool)
raDisableExecuteAPIEndpoint = Lens.lens (disableExecuteAPIEndpoint :: RestAPI -> Lude.Maybe Lude.Bool) (\s a -> s {disableExecuteAPIEndpoint = a} :: RestAPI)
{-# DEPRECATED raDisableExecuteAPIEndpoint "Use generic-lens or generic-optics with 'disableExecuteAPIEndpoint' instead." #-}

-- | The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
--
-- /Note:/ Consider using 'binaryMediaTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raBinaryMediaTypes :: Lens.Lens' RestAPI (Lude.Maybe [Lude.Text])
raBinaryMediaTypes = Lens.lens (binaryMediaTypes :: RestAPI -> Lude.Maybe [Lude.Text]) (\s a -> s {binaryMediaTypes = a} :: RestAPI)
{-# DEPRECATED raBinaryMediaTypes "Use generic-lens or generic-optics with 'binaryMediaTypes' instead." #-}

-- | The warning messages reported when @failonwarnings@ is turned on during API import.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raWarnings :: Lens.Lens' RestAPI (Lude.Maybe [Lude.Text])
raWarnings = Lens.lens (warnings :: RestAPI -> Lude.Maybe [Lude.Text]) (\s a -> s {warnings = a} :: RestAPI)
{-# DEPRECATED raWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | The timestamp when the API was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raCreatedDate :: Lens.Lens' RestAPI (Lude.Maybe Lude.Timestamp)
raCreatedDate = Lens.lens (createdDate :: RestAPI -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: RestAPI)
{-# DEPRECATED raCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The API's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raName :: Lens.Lens' RestAPI (Lude.Maybe Lude.Text)
raName = Lens.lens (name :: RestAPI -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RestAPI)
{-# DEPRECATED raName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A version identifier for the API.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raVersion :: Lens.Lens' RestAPI (Lude.Maybe Lude.Text)
raVersion = Lens.lens (version :: RestAPI -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: RestAPI)
{-# DEPRECATED raVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The source of the API key for metering requests according to a usage plan. Valid values are:
--
--     * @HEADER@ to read the API key from the @X-API-Key@ header of a request.
--
--     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
--
--
--
-- /Note:/ Consider using 'apiKeySource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raApiKeySource :: Lens.Lens' RestAPI (Lude.Maybe APIKeySourceType)
raApiKeySource = Lens.lens (apiKeySource :: RestAPI -> Lude.Maybe APIKeySourceType) (\s a -> s {apiKeySource = a} :: RestAPI)
{-# DEPRECATED raApiKeySource "Use generic-lens or generic-optics with 'apiKeySource' instead." #-}

-- | The API's identifier. This identifier is unique across all of your APIs in API Gateway.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raId :: Lens.Lens' RestAPI (Lude.Maybe Lude.Text)
raId = Lens.lens (id :: RestAPI -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: RestAPI)
{-# DEPRECATED raId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A stringified JSON policy document that applies to this RestApi regardless of the caller and 'Method' configuration.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raPolicy :: Lens.Lens' RestAPI (Lude.Maybe Lude.Text)
raPolicy = Lens.lens (policy :: RestAPI -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: RestAPI)
{-# DEPRECATED raPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raEndpointConfiguration :: Lens.Lens' RestAPI (Lude.Maybe EndpointConfiguration)
raEndpointConfiguration = Lens.lens (endpointConfiguration :: RestAPI -> Lude.Maybe EndpointConfiguration) (\s a -> s {endpointConfiguration = a} :: RestAPI)
{-# DEPRECATED raEndpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead." #-}

-- | The API's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raDescription :: Lens.Lens' RestAPI (Lude.Maybe Lude.Text)
raDescription = Lens.lens (description :: RestAPI -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RestAPI)
{-# DEPRECATED raDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTags :: Lens.Lens' RestAPI (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
raTags = Lens.lens (tags :: RestAPI -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: RestAPI)
{-# DEPRECATED raTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON RestAPI where
  parseJSON =
    Lude.withObject
      "RestAPI"
      ( \x ->
          RestAPI'
            Lude.<$> (x Lude..:? "minimumCompressionSize")
            Lude.<*> (x Lude..:? "disableExecuteApiEndpoint")
            Lude.<*> (x Lude..:? "binaryMediaTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "warnings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "apiKeySource")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "policy")
            Lude.<*> (x Lude..:? "endpointConfiguration")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
