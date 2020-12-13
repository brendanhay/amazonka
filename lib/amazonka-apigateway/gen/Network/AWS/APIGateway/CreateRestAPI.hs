{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateRestAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'RestApi' resource.
module Network.AWS.APIGateway.CreateRestAPI
  ( -- * Creating a request
    CreateRestAPI (..),
    mkCreateRestAPI,

    -- ** Request lenses
    craMinimumCompressionSize,
    craDisableExecuteAPIEndpoint,
    craBinaryMediaTypes,
    craName,
    craVersion,
    craApiKeySource,
    craCloneFrom,
    craPolicy,
    craEndpointConfiguration,
    craDescription,
    craTags,

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

-- | The POST Request to add a new 'RestApi' resource to your collection.
--
-- /See:/ 'mkCreateRestAPI' smart constructor.
data CreateRestAPI = CreateRestAPI'
  { -- | A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
    minimumCompressionSize :: Lude.Maybe Lude.Int,
    -- | Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
    disableExecuteAPIEndpoint :: Lude.Maybe Lude.Bool,
    -- | The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
    binaryMediaTypes :: Lude.Maybe [Lude.Text],
    -- | [Required] The name of the 'RestApi' .
    name :: Lude.Text,
    -- | A version identifier for the API.
    version :: Lude.Maybe Lude.Text,
    -- | The source of the API key for metering requests according to a usage plan. Valid values are:
    --
    --     * @HEADER@ to read the API key from the @X-API-Key@ header of a request.
    --
    --     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
    apiKeySource :: Lude.Maybe APIKeySourceType,
    -- | The ID of the 'RestApi' that you want to clone from.
    cloneFrom :: Lude.Maybe Lude.Text,
    -- | 'Method'
    policy :: Lude.Maybe Lude.Text,
    -- | The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
    endpointConfiguration :: Lude.Maybe EndpointConfiguration,
    -- | The description of the 'RestApi' .
    description :: Lude.Maybe Lude.Text,
    -- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRestAPI' with the minimum fields required to make a request.
--
-- * 'minimumCompressionSize' - A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
-- * 'disableExecuteAPIEndpoint' - Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
-- * 'binaryMediaTypes' - The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
-- * 'name' - [Required] The name of the 'RestApi' .
-- * 'version' - A version identifier for the API.
-- * 'apiKeySource' - The source of the API key for metering requests according to a usage plan. Valid values are:
--
--     * @HEADER@ to read the API key from the @X-API-Key@ header of a request.
--
--     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
--
--
-- * 'cloneFrom' - The ID of the 'RestApi' that you want to clone from.
-- * 'policy' - 'Method'
-- * 'endpointConfiguration' - The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
-- * 'description' - The description of the 'RestApi' .
-- * 'tags' - The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
mkCreateRestAPI ::
  -- | 'name'
  Lude.Text ->
  CreateRestAPI
mkCreateRestAPI pName_ =
  CreateRestAPI'
    { minimumCompressionSize = Lude.Nothing,
      disableExecuteAPIEndpoint = Lude.Nothing,
      binaryMediaTypes = Lude.Nothing,
      name = pName_,
      version = Lude.Nothing,
      apiKeySource = Lude.Nothing,
      cloneFrom = Lude.Nothing,
      policy = Lude.Nothing,
      endpointConfiguration = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
--
-- /Note:/ Consider using 'minimumCompressionSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craMinimumCompressionSize :: Lens.Lens' CreateRestAPI (Lude.Maybe Lude.Int)
craMinimumCompressionSize = Lens.lens (minimumCompressionSize :: CreateRestAPI -> Lude.Maybe Lude.Int) (\s a -> s {minimumCompressionSize = a} :: CreateRestAPI)
{-# DEPRECATED craMinimumCompressionSize "Use generic-lens or generic-optics with 'minimumCompressionSize' instead." #-}

-- | Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
--
-- /Note:/ Consider using 'disableExecuteAPIEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craDisableExecuteAPIEndpoint :: Lens.Lens' CreateRestAPI (Lude.Maybe Lude.Bool)
craDisableExecuteAPIEndpoint = Lens.lens (disableExecuteAPIEndpoint :: CreateRestAPI -> Lude.Maybe Lude.Bool) (\s a -> s {disableExecuteAPIEndpoint = a} :: CreateRestAPI)
{-# DEPRECATED craDisableExecuteAPIEndpoint "Use generic-lens or generic-optics with 'disableExecuteAPIEndpoint' instead." #-}

-- | The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
--
-- /Note:/ Consider using 'binaryMediaTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craBinaryMediaTypes :: Lens.Lens' CreateRestAPI (Lude.Maybe [Lude.Text])
craBinaryMediaTypes = Lens.lens (binaryMediaTypes :: CreateRestAPI -> Lude.Maybe [Lude.Text]) (\s a -> s {binaryMediaTypes = a} :: CreateRestAPI)
{-# DEPRECATED craBinaryMediaTypes "Use generic-lens or generic-optics with 'binaryMediaTypes' instead." #-}

-- | [Required] The name of the 'RestApi' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craName :: Lens.Lens' CreateRestAPI Lude.Text
craName = Lens.lens (name :: CreateRestAPI -> Lude.Text) (\s a -> s {name = a} :: CreateRestAPI)
{-# DEPRECATED craName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A version identifier for the API.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craVersion :: Lens.Lens' CreateRestAPI (Lude.Maybe Lude.Text)
craVersion = Lens.lens (version :: CreateRestAPI -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateRestAPI)
{-# DEPRECATED craVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The source of the API key for metering requests according to a usage plan. Valid values are:
--
--     * @HEADER@ to read the API key from the @X-API-Key@ header of a request.
--
--     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
--
--
--
-- /Note:/ Consider using 'apiKeySource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craApiKeySource :: Lens.Lens' CreateRestAPI (Lude.Maybe APIKeySourceType)
craApiKeySource = Lens.lens (apiKeySource :: CreateRestAPI -> Lude.Maybe APIKeySourceType) (\s a -> s {apiKeySource = a} :: CreateRestAPI)
{-# DEPRECATED craApiKeySource "Use generic-lens or generic-optics with 'apiKeySource' instead." #-}

-- | The ID of the 'RestApi' that you want to clone from.
--
-- /Note:/ Consider using 'cloneFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craCloneFrom :: Lens.Lens' CreateRestAPI (Lude.Maybe Lude.Text)
craCloneFrom = Lens.lens (cloneFrom :: CreateRestAPI -> Lude.Maybe Lude.Text) (\s a -> s {cloneFrom = a} :: CreateRestAPI)
{-# DEPRECATED craCloneFrom "Use generic-lens or generic-optics with 'cloneFrom' instead." #-}

-- | 'Method'
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craPolicy :: Lens.Lens' CreateRestAPI (Lude.Maybe Lude.Text)
craPolicy = Lens.lens (policy :: CreateRestAPI -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: CreateRestAPI)
{-# DEPRECATED craPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craEndpointConfiguration :: Lens.Lens' CreateRestAPI (Lude.Maybe EndpointConfiguration)
craEndpointConfiguration = Lens.lens (endpointConfiguration :: CreateRestAPI -> Lude.Maybe EndpointConfiguration) (\s a -> s {endpointConfiguration = a} :: CreateRestAPI)
{-# DEPRECATED craEndpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead." #-}

-- | The description of the 'RestApi' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craDescription :: Lens.Lens' CreateRestAPI (Lude.Maybe Lude.Text)
craDescription = Lens.lens (description :: CreateRestAPI -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateRestAPI)
{-# DEPRECATED craDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craTags :: Lens.Lens' CreateRestAPI (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
craTags = Lens.lens (tags :: CreateRestAPI -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateRestAPI)
{-# DEPRECATED craTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateRestAPI where
  type Rs CreateRestAPI = RestAPI
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateRestAPI where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateRestAPI where
  toJSON CreateRestAPI' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("minimumCompressionSize" Lude..=)
              Lude.<$> minimumCompressionSize,
            ("disableExecuteApiEndpoint" Lude..=)
              Lude.<$> disableExecuteAPIEndpoint,
            ("binaryMediaTypes" Lude..=) Lude.<$> binaryMediaTypes,
            Lude.Just ("name" Lude..= name),
            ("version" Lude..=) Lude.<$> version,
            ("apiKeySource" Lude..=) Lude.<$> apiKeySource,
            ("cloneFrom" Lude..=) Lude.<$> cloneFrom,
            ("policy" Lude..=) Lude.<$> policy,
            ("endpointConfiguration" Lude..=) Lude.<$> endpointConfiguration,
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateRestAPI where
  toPath = Lude.const "/restapis"

instance Lude.ToQuery CreateRestAPI where
  toQuery = Lude.const Lude.mempty
