{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateGatewayResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a 'GatewayResponse' of a specified response type on the given 'RestApi' .
module Network.AWS.APIGateway.UpdateGatewayResponse
  ( -- * Creating a request
    UpdateGatewayResponse (..),
    mkUpdateGatewayResponse,

    -- ** Request lenses
    ugRestAPIId,
    ugPatchOperations,
    ugResponseType,

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

-- | Updates a 'GatewayResponse' of a specified response type on the given 'RestApi' .
--
-- /See:/ 'mkUpdateGatewayResponse' smart constructor.
data UpdateGatewayResponse = UpdateGatewayResponse'
  { -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | A list of update operations to be applied to the specified resource and in the order specified in this list.
    patchOperations :: Lude.Maybe [PatchOperation],
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

-- | Creates a value of 'UpdateGatewayResponse' with the minimum fields required to make a request.
--
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
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
mkUpdateGatewayResponse ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  UpdateGatewayResponse
mkUpdateGatewayResponse pRestAPIId_ pResponseType_ =
  UpdateGatewayResponse'
    { restAPIId = pRestAPIId_,
      patchOperations = Lude.Nothing,
      responseType = pResponseType_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugRestAPIId :: Lens.Lens' UpdateGatewayResponse Lude.Text
ugRestAPIId = Lens.lens (restAPIId :: UpdateGatewayResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateGatewayResponse)
{-# DEPRECATED ugRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugPatchOperations :: Lens.Lens' UpdateGatewayResponse (Lude.Maybe [PatchOperation])
ugPatchOperations = Lens.lens (patchOperations :: UpdateGatewayResponse -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateGatewayResponse)
{-# DEPRECATED ugPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

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
ugResponseType :: Lens.Lens' UpdateGatewayResponse GatewayResponseType
ugResponseType = Lens.lens (responseType :: UpdateGatewayResponse -> GatewayResponseType) (\s a -> s {responseType = a} :: UpdateGatewayResponse)
{-# DEPRECATED ugResponseType "Use generic-lens or generic-optics with 'responseType' instead." #-}

instance Lude.AWSRequest UpdateGatewayResponse where
  type Rs UpdateGatewayResponse = GatewayResponse
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateGatewayResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateGatewayResponse where
  toJSON UpdateGatewayResponse' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateGatewayResponse where
  toPath UpdateGatewayResponse' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/gatewayresponses/",
        Lude.toBS responseType
      ]

instance Lude.ToQuery UpdateGatewayResponse where
  toQuery = Lude.const Lude.mempty
