{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetGatewayResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a 'GatewayResponse' of a specified response type on the given 'RestApi' .
module Network.AWS.APIGateway.GetGatewayResponse
  ( -- * Creating a request
    GetGatewayResponse (..),
    mkGetGatewayResponse,

    -- ** Request lenses
    ggRestAPIId,
    ggResponseType,

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

-- | Gets a 'GatewayResponse' of a specified response type on the given 'RestApi' .
--
-- /See:/ 'mkGetGatewayResponse' smart constructor.
data GetGatewayResponse = GetGatewayResponse'
  { -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
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
    responseType :: GatewayResponseType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGatewayResponse' with the minimum fields required to make a request.
--
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
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
mkGetGatewayResponse ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  GetGatewayResponse
mkGetGatewayResponse pRestAPIId_ pResponseType_ =
  GetGatewayResponse'
    { restAPIId = pRestAPIId_,
      responseType = pResponseType_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggRestAPIId :: Lens.Lens' GetGatewayResponse Lude.Text
ggRestAPIId = Lens.lens (restAPIId :: GetGatewayResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: GetGatewayResponse)
{-# DEPRECATED ggRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

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
ggResponseType :: Lens.Lens' GetGatewayResponse GatewayResponseType
ggResponseType = Lens.lens (responseType :: GetGatewayResponse -> GatewayResponseType) (\s a -> s {responseType = a} :: GetGatewayResponse)
{-# DEPRECATED ggResponseType "Use generic-lens or generic-optics with 'responseType' instead." #-}

instance Lude.AWSRequest GetGatewayResponse where
  type Rs GetGatewayResponse = GatewayResponse
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetGatewayResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetGatewayResponse where
  toPath GetGatewayResponse' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/gatewayresponses/",
        Lude.toBS responseType
      ]

instance Lude.ToQuery GetGatewayResponse where
  toQuery = Lude.const Lude.mempty
