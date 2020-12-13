{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.PutMethodResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a 'MethodResponse' to an existing 'Method' resource.
module Network.AWS.APIGateway.PutMethodResponse
  ( -- * Creating a request
    PutMethodResponse (..),
    mkPutMethodResponse,

    -- ** Request lenses
    pmResourceId,
    pmHttpMethod,
    pmResponseModels,
    pmRestAPIId,
    pmStatusCode,
    pmResponseParameters,

    -- * Destructuring the response
    MethodResponse (..),
    mkMethodResponse,

    -- ** Response lenses
    mResponseModels,
    mStatusCode,
    mResponseParameters,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to add a 'MethodResponse' to an existing 'Method' resource.
--
-- /See:/ 'mkPutMethodResponse' smart constructor.
data PutMethodResponse = PutMethodResponse'
  { -- | [Required] The 'Resource' identifier for the 'Method' resource.
    resourceId :: Lude.Text,
    -- | [Required] The HTTP verb of the 'Method' resource.
    httpMethod :: Lude.Text,
    -- | Specifies the 'Model' resources used for the response's content type. Response models are represented as a key/value map, with a content type as the key and a 'Model' name as the value.
    responseModels :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | [Required] The method response's status code.
    statusCode :: Lude.Text,
    -- | A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header name and the associated value is a Boolean flag indicating whether the method response parameter is required or not. The method response header names must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The response parameter names defined here are available in the integration response to be mapped from an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
    responseParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Bool))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMethodResponse' with the minimum fields required to make a request.
--
-- * 'resourceId' - [Required] The 'Resource' identifier for the 'Method' resource.
-- * 'httpMethod' - [Required] The HTTP verb of the 'Method' resource.
-- * 'responseModels' - Specifies the 'Model' resources used for the response's content type. Response models are represented as a key/value map, with a content type as the key and a 'Model' name as the value.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'statusCode' - [Required] The method response's status code.
-- * 'responseParameters' - A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header name and the associated value is a Boolean flag indicating whether the method response parameter is required or not. The method response header names must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The response parameter names defined here are available in the integration response to be mapped from an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
mkPutMethodResponse ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'statusCode'
  Lude.Text ->
  PutMethodResponse
mkPutMethodResponse
  pResourceId_
  pHttpMethod_
  pRestAPIId_
  pStatusCode_ =
    PutMethodResponse'
      { resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        responseModels = Lude.Nothing,
        restAPIId = pRestAPIId_,
        statusCode = pStatusCode_,
        responseParameters = Lude.Nothing
      }

-- | [Required] The 'Resource' identifier for the 'Method' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmResourceId :: Lens.Lens' PutMethodResponse Lude.Text
pmResourceId = Lens.lens (resourceId :: PutMethodResponse -> Lude.Text) (\s a -> s {resourceId = a} :: PutMethodResponse)
{-# DEPRECATED pmResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmHttpMethod :: Lens.Lens' PutMethodResponse Lude.Text
pmHttpMethod = Lens.lens (httpMethod :: PutMethodResponse -> Lude.Text) (\s a -> s {httpMethod = a} :: PutMethodResponse)
{-# DEPRECATED pmHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | Specifies the 'Model' resources used for the response's content type. Response models are represented as a key/value map, with a content type as the key and a 'Model' name as the value.
--
-- /Note:/ Consider using 'responseModels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmResponseModels :: Lens.Lens' PutMethodResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pmResponseModels = Lens.lens (responseModels :: PutMethodResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {responseModels = a} :: PutMethodResponse)
{-# DEPRECATED pmResponseModels "Use generic-lens or generic-optics with 'responseModels' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmRestAPIId :: Lens.Lens' PutMethodResponse Lude.Text
pmRestAPIId = Lens.lens (restAPIId :: PutMethodResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: PutMethodResponse)
{-# DEPRECATED pmRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The method response's status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmStatusCode :: Lens.Lens' PutMethodResponse Lude.Text
pmStatusCode = Lens.lens (statusCode :: PutMethodResponse -> Lude.Text) (\s a -> s {statusCode = a} :: PutMethodResponse)
{-# DEPRECATED pmStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header name and the associated value is a Boolean flag indicating whether the method response parameter is required or not. The method response header names must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The response parameter names defined here are available in the integration response to be mapped from an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
--
-- /Note:/ Consider using 'responseParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmResponseParameters :: Lens.Lens' PutMethodResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Bool)))
pmResponseParameters = Lens.lens (responseParameters :: PutMethodResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Bool))) (\s a -> s {responseParameters = a} :: PutMethodResponse)
{-# DEPRECATED pmResponseParameters "Use generic-lens or generic-optics with 'responseParameters' instead." #-}

instance Lude.AWSRequest PutMethodResponse where
  type Rs PutMethodResponse = MethodResponse
  request = Req.putJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders PutMethodResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON PutMethodResponse where
  toJSON PutMethodResponse' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("responseModels" Lude..=) Lude.<$> responseModels,
            ("responseParameters" Lude..=) Lude.<$> responseParameters
          ]
      )

instance Lude.ToPath PutMethodResponse where
  toPath PutMethodResponse' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod,
        "/responses/",
        Lude.toBS statusCode
      ]

instance Lude.ToQuery PutMethodResponse where
  toQuery = Lude.const Lude.mempty
