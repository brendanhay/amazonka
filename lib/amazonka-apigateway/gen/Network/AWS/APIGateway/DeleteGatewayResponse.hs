{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteGatewayResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Clears any customization of a 'GatewayResponse' of a specified response type on the given 'RestApi' and resets it with the default settings.
module Network.AWS.APIGateway.DeleteGatewayResponse
  ( -- * Creating a request
    DeleteGatewayResponse (..),
    mkDeleteGatewayResponse,

    -- ** Request lenses
    dgRestAPIId,
    dgResponseType,

    -- * Destructuring the response
    DeleteGatewayResponseResponse (..),
    mkDeleteGatewayResponseResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Clears any customization of a 'GatewayResponse' of a specified response type on the given 'RestApi' and resets it with the default settings.
--
-- /See:/ 'mkDeleteGatewayResponse' smart constructor.
data DeleteGatewayResponse = DeleteGatewayResponse'
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

-- | Creates a value of 'DeleteGatewayResponse' with the minimum fields required to make a request.
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
mkDeleteGatewayResponse ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  DeleteGatewayResponse
mkDeleteGatewayResponse pRestAPIId_ pResponseType_ =
  DeleteGatewayResponse'
    { restAPIId = pRestAPIId_,
      responseType = pResponseType_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgRestAPIId :: Lens.Lens' DeleteGatewayResponse Lude.Text
dgRestAPIId = Lens.lens (restAPIId :: DeleteGatewayResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteGatewayResponse)
{-# DEPRECATED dgRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

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
dgResponseType :: Lens.Lens' DeleteGatewayResponse GatewayResponseType
dgResponseType = Lens.lens (responseType :: DeleteGatewayResponse -> GatewayResponseType) (\s a -> s {responseType = a} :: DeleteGatewayResponse)
{-# DEPRECATED dgResponseType "Use generic-lens or generic-optics with 'responseType' instead." #-}

instance Lude.AWSRequest DeleteGatewayResponse where
  type Rs DeleteGatewayResponse = DeleteGatewayResponseResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteGatewayResponseResponse'

instance Lude.ToHeaders DeleteGatewayResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteGatewayResponse where
  toPath DeleteGatewayResponse' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/gatewayresponses/",
        Lude.toBS responseType
      ]

instance Lude.ToQuery DeleteGatewayResponse where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGatewayResponseResponse' smart constructor.
data DeleteGatewayResponseResponse = DeleteGatewayResponseResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGatewayResponseResponse' with the minimum fields required to make a request.
mkDeleteGatewayResponseResponse ::
  DeleteGatewayResponseResponse
mkDeleteGatewayResponseResponse = DeleteGatewayResponseResponse'
