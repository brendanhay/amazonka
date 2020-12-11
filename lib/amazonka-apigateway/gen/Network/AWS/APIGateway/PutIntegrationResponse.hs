{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.PutIntegrationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a put integration.
module Network.AWS.APIGateway.PutIntegrationResponse
  ( -- * Creating a request
    PutIntegrationResponse (..),
    mkPutIntegrationResponse,

    -- ** Request lenses
    piContentHandling,
    piResponseTemplates,
    piSelectionPattern,
    piResponseParameters,
    piRestAPIId,
    piResourceId,
    piHttpMethod,
    piStatusCode,

    -- * Destructuring the response
    IntegrationResponse (..),
    mkIntegrationResponse,

    -- ** Response lenses
    intContentHandling,
    intResponseTemplates,
    intSelectionPattern,
    intStatusCode,
    intResponseParameters,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a put integration response request.
--
-- /See:/ 'mkPutIntegrationResponse' smart constructor.
data PutIntegrationResponse = PutIntegrationResponse'
  { contentHandling ::
      Lude.Maybe ContentHandlingStrategy,
    responseTemplates ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    selectionPattern :: Lude.Maybe Lude.Text,
    responseParameters ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    restAPIId :: Lude.Text,
    resourceId :: Lude.Text,
    httpMethod :: Lude.Text,
    statusCode :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutIntegrationResponse' with the minimum fields required to make a request.
--
-- * 'contentHandling' - Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:
--
--
--     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.
--
--
--     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string.
--
--
-- If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
-- * 'httpMethod' - [Required] Specifies a put integration response request's HTTP method.
-- * 'resourceId' - [Required] Specifies a put integration response request's resource identifier.
-- * 'responseParameters' - A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ must be a valid and unique response header name and @JSON-expression@ a valid JSON expression without the @> @ prefix.
-- * 'responseTemplates' - Specifies a put integration response's templates.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'selectionPattern' - Specifies the selection pattern of a put integration response.
-- * 'statusCode' - [Required] Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
mkPutIntegrationResponse ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'statusCode'
  Lude.Text ->
  PutIntegrationResponse
mkPutIntegrationResponse
  pRestAPIId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    PutIntegrationResponse'
      { contentHandling = Lude.Nothing,
        responseTemplates = Lude.Nothing,
        selectionPattern = Lude.Nothing,
        responseParameters = Lude.Nothing,
        restAPIId = pRestAPIId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:
--
--
--     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.
--
--
--     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string.
--
--
-- If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
--
-- /Note:/ Consider using 'contentHandling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piContentHandling :: Lens.Lens' PutIntegrationResponse (Lude.Maybe ContentHandlingStrategy)
piContentHandling = Lens.lens (contentHandling :: PutIntegrationResponse -> Lude.Maybe ContentHandlingStrategy) (\s a -> s {contentHandling = a} :: PutIntegrationResponse)
{-# DEPRECATED piContentHandling "Use generic-lens or generic-optics with 'contentHandling' instead." #-}

-- | Specifies a put integration response's templates.
--
-- /Note:/ Consider using 'responseTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piResponseTemplates :: Lens.Lens' PutIntegrationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
piResponseTemplates = Lens.lens (responseTemplates :: PutIntegrationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {responseTemplates = a} :: PutIntegrationResponse)
{-# DEPRECATED piResponseTemplates "Use generic-lens or generic-optics with 'responseTemplates' instead." #-}

-- | Specifies the selection pattern of a put integration response.
--
-- /Note:/ Consider using 'selectionPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSelectionPattern :: Lens.Lens' PutIntegrationResponse (Lude.Maybe Lude.Text)
piSelectionPattern = Lens.lens (selectionPattern :: PutIntegrationResponse -> Lude.Maybe Lude.Text) (\s a -> s {selectionPattern = a} :: PutIntegrationResponse)
{-# DEPRECATED piSelectionPattern "Use generic-lens or generic-optics with 'selectionPattern' instead." #-}

-- | A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ must be a valid and unique response header name and @JSON-expression@ a valid JSON expression without the @> @ prefix.
--
-- /Note:/ Consider using 'responseParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piResponseParameters :: Lens.Lens' PutIntegrationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
piResponseParameters = Lens.lens (responseParameters :: PutIntegrationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {responseParameters = a} :: PutIntegrationResponse)
{-# DEPRECATED piResponseParameters "Use generic-lens or generic-optics with 'responseParameters' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRestAPIId :: Lens.Lens' PutIntegrationResponse Lude.Text
piRestAPIId = Lens.lens (restAPIId :: PutIntegrationResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: PutIntegrationResponse)
{-# DEPRECATED piRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] Specifies a put integration response request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piResourceId :: Lens.Lens' PutIntegrationResponse Lude.Text
piResourceId = Lens.lens (resourceId :: PutIntegrationResponse -> Lude.Text) (\s a -> s {resourceId = a} :: PutIntegrationResponse)
{-# DEPRECATED piResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] Specifies a put integration response request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piHttpMethod :: Lens.Lens' PutIntegrationResponse Lude.Text
piHttpMethod = Lens.lens (httpMethod :: PutIntegrationResponse -> Lude.Text) (\s a -> s {httpMethod = a} :: PutIntegrationResponse)
{-# DEPRECATED piHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | [Required] Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piStatusCode :: Lens.Lens' PutIntegrationResponse Lude.Text
piStatusCode = Lens.lens (statusCode :: PutIntegrationResponse -> Lude.Text) (\s a -> s {statusCode = a} :: PutIntegrationResponse)
{-# DEPRECATED piStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.AWSRequest PutIntegrationResponse where
  type Rs PutIntegrationResponse = IntegrationResponse
  request = Req.putJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders PutIntegrationResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON PutIntegrationResponse where
  toJSON PutIntegrationResponse' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("contentHandling" Lude..=) Lude.<$> contentHandling,
            ("responseTemplates" Lude..=) Lude.<$> responseTemplates,
            ("selectionPattern" Lude..=) Lude.<$> selectionPattern,
            ("responseParameters" Lude..=) Lude.<$> responseParameters
          ]
      )

instance Lude.ToPath PutIntegrationResponse where
  toPath PutIntegrationResponse' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod,
        "/integration/responses/",
        Lude.toBS statusCode
      ]

instance Lude.ToQuery PutIntegrationResponse where
  toQuery = Lude.const Lude.mempty
