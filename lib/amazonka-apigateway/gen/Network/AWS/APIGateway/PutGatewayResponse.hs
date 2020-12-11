{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.PutGatewayResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a customization of a 'GatewayResponse' of a specified response type and status code on the given 'RestApi' .
module Network.AWS.APIGateway.PutGatewayResponse
  ( -- * Creating a request
    PutGatewayResponse (..),
    mkPutGatewayResponse,

    -- ** Request lenses
    pgResponseTemplates,
    pgStatusCode,
    pgResponseParameters,
    pgRestAPIId,
    pgResponseType,

    -- * Destructuring the response
    GatewayResponse (..),
    mkGatewayResponse,

    -- ** Response lenses
    gDefaultResponse,
    gResponseTemplates,
    gResponseType,
    gStatusCode,
    gResponseParameters,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Creates a customization of a 'GatewayResponse' of a specified response type and status code on the given 'RestApi' .
--
-- /See:/ 'mkPutGatewayResponse' smart constructor.
data PutGatewayResponse = PutGatewayResponse'
  { responseTemplates ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    statusCode :: Lude.Maybe Lude.Text,
    responseParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    restAPIId :: Lude.Text,
    responseType :: GatewayResponseType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutGatewayResponse' with the minimum fields required to make a request.
--
-- * 'responseParameters' - Response parameters (paths, query strings and headers) of the 'GatewayResponse' as a string-to-string map of key-value pairs.
--
--
-- * 'responseTemplates' - Response templates of the 'GatewayResponse' as a string-to-string map of key-value pairs.
--
--
-- * 'responseType' - [Required] The response type of the associated 'GatewayResponse' . Valid values are
--
--     * ACCESS_DENIED
--
--     * API_CONFIGURATION_ERROR
--
--     * AUTHORIZER_FAILURE
--
--     * AUTHORIZER_CONFIGURATION_ERROR
--
--     * BAD_REQUEST_PARAMETERS
--
--     * BAD_REQUEST_BODY
--
--     * DEFAULT_4XX
--
--     * DEFAULT_5XX
--
--     * EXPIRED_TOKEN
--
--     * INVALID_SIGNATURE
--
--     * INTEGRATION_FAILURE
--
--     * INTEGRATION_TIMEOUT
--
--     * INVALID_API_KEY
--
--     * MISSING_AUTHENTICATION_TOKEN
--
--     * QUOTA_EXCEEDED
--
--     * REQUEST_TOO_LARGE
--
--     * RESOURCE_NOT_FOUND
--
--     * THROTTLED
--
--     * UNAUTHORIZED
--
--     * UNSUPPORTED_MEDIA_TYPE
--
--
--
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'statusCode' - 'GatewayResponse'
mkPutGatewayResponse ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  PutGatewayResponse
mkPutGatewayResponse pRestAPIId_ pResponseType_ =
  PutGatewayResponse'
    { responseTemplates = Lude.Nothing,
      statusCode = Lude.Nothing,
      responseParameters = Lude.Nothing,
      restAPIId = pRestAPIId_,
      responseType = pResponseType_
    }

-- | Response templates of the 'GatewayResponse' as a string-to-string map of key-value pairs.
--
--
--
-- /Note:/ Consider using 'responseTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgResponseTemplates :: Lens.Lens' PutGatewayResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pgResponseTemplates = Lens.lens (responseTemplates :: PutGatewayResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {responseTemplates = a} :: PutGatewayResponse)
{-# DEPRECATED pgResponseTemplates "Use generic-lens or generic-optics with 'responseTemplates' instead." #-}

-- | 'GatewayResponse'
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgStatusCode :: Lens.Lens' PutGatewayResponse (Lude.Maybe Lude.Text)
pgStatusCode = Lens.lens (statusCode :: PutGatewayResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusCode = a} :: PutGatewayResponse)
{-# DEPRECATED pgStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | Response parameters (paths, query strings and headers) of the 'GatewayResponse' as a string-to-string map of key-value pairs.
--
--
--
-- /Note:/ Consider using 'responseParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgResponseParameters :: Lens.Lens' PutGatewayResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pgResponseParameters = Lens.lens (responseParameters :: PutGatewayResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {responseParameters = a} :: PutGatewayResponse)
{-# DEPRECATED pgResponseParameters "Use generic-lens or generic-optics with 'responseParameters' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgRestAPIId :: Lens.Lens' PutGatewayResponse Lude.Text
pgRestAPIId = Lens.lens (restAPIId :: PutGatewayResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: PutGatewayResponse)
{-# DEPRECATED pgRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The response type of the associated 'GatewayResponse' . Valid values are
--
--     * ACCESS_DENIED
--
--     * API_CONFIGURATION_ERROR
--
--     * AUTHORIZER_FAILURE
--
--     * AUTHORIZER_CONFIGURATION_ERROR
--
--     * BAD_REQUEST_PARAMETERS
--
--     * BAD_REQUEST_BODY
--
--     * DEFAULT_4XX
--
--     * DEFAULT_5XX
--
--     * EXPIRED_TOKEN
--
--     * INVALID_SIGNATURE
--
--     * INTEGRATION_FAILURE
--
--     * INTEGRATION_TIMEOUT
--
--     * INVALID_API_KEY
--
--     * MISSING_AUTHENTICATION_TOKEN
--
--     * QUOTA_EXCEEDED
--
--     * REQUEST_TOO_LARGE
--
--     * RESOURCE_NOT_FOUND
--
--     * THROTTLED
--
--     * UNAUTHORIZED
--
--     * UNSUPPORTED_MEDIA_TYPE
--
--
--
--
-- /Note:/ Consider using 'responseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgResponseType :: Lens.Lens' PutGatewayResponse GatewayResponseType
pgResponseType = Lens.lens (responseType :: PutGatewayResponse -> GatewayResponseType) (\s a -> s {responseType = a} :: PutGatewayResponse)
{-# DEPRECATED pgResponseType "Use generic-lens or generic-optics with 'responseType' instead." #-}

instance Lude.AWSRequest PutGatewayResponse where
  type Rs PutGatewayResponse = GatewayResponse
  request = Req.putJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders PutGatewayResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON PutGatewayResponse where
  toJSON PutGatewayResponse' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("responseTemplates" Lude..=) Lude.<$> responseTemplates,
            ("statusCode" Lude..=) Lude.<$> statusCode,
            ("responseParameters" Lude..=) Lude.<$> responseParameters
          ]
      )

instance Lude.ToPath PutGatewayResponse where
  toPath PutGatewayResponse' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/gatewayresponses/",
        Lude.toBS responseType
      ]

instance Lude.ToQuery PutGatewayResponse where
  toQuery = Lude.const Lude.mempty
