{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetIntegrationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a get integration response.
module Network.AWS.APIGateway.GetIntegrationResponse
  ( -- * Creating a request
    GetIntegrationResponse (..),
    mkGetIntegrationResponse,

    -- ** Request lenses
    giResourceId,
    giHttpMethod,
    giRestAPIId,
    giStatusCode,

    -- * Destructuring the response
    IntegrationResponse (..),
    mkIntegrationResponse,

    -- ** Response lenses
    ifContentHandling,
    ifResponseTemplates,
    ifSelectionPattern,
    ifStatusCode,
    ifResponseParameters,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a get integration response request.
--
-- /See:/ 'mkGetIntegrationResponse' smart constructor.
data GetIntegrationResponse = GetIntegrationResponse'
  { -- | [Required] Specifies a get integration response request's resource identifier.
    resourceId :: Lude.Text,
    -- | [Required] Specifies a get integration response request's HTTP method.
    httpMethod :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | [Required] Specifies a get integration response request's status code.
    statusCode :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIntegrationResponse' with the minimum fields required to make a request.
--
-- * 'resourceId' - [Required] Specifies a get integration response request's resource identifier.
-- * 'httpMethod' - [Required] Specifies a get integration response request's HTTP method.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'statusCode' - [Required] Specifies a get integration response request's status code.
mkGetIntegrationResponse ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'statusCode'
  Lude.Text ->
  GetIntegrationResponse
mkGetIntegrationResponse
  pResourceId_
  pHttpMethod_
  pRestAPIId_
  pStatusCode_ =
    GetIntegrationResponse'
      { resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        restAPIId = pRestAPIId_,
        statusCode = pStatusCode_
      }

-- | [Required] Specifies a get integration response request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giResourceId :: Lens.Lens' GetIntegrationResponse Lude.Text
giResourceId = Lens.lens (resourceId :: GetIntegrationResponse -> Lude.Text) (\s a -> s {resourceId = a} :: GetIntegrationResponse)
{-# DEPRECATED giResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] Specifies a get integration response request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giHttpMethod :: Lens.Lens' GetIntegrationResponse Lude.Text
giHttpMethod = Lens.lens (httpMethod :: GetIntegrationResponse -> Lude.Text) (\s a -> s {httpMethod = a} :: GetIntegrationResponse)
{-# DEPRECATED giHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giRestAPIId :: Lens.Lens' GetIntegrationResponse Lude.Text
giRestAPIId = Lens.lens (restAPIId :: GetIntegrationResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: GetIntegrationResponse)
{-# DEPRECATED giRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] Specifies a get integration response request's status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giStatusCode :: Lens.Lens' GetIntegrationResponse Lude.Text
giStatusCode = Lens.lens (statusCode :: GetIntegrationResponse -> Lude.Text) (\s a -> s {statusCode = a} :: GetIntegrationResponse)
{-# DEPRECATED giStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.AWSRequest GetIntegrationResponse where
  type Rs GetIntegrationResponse = IntegrationResponse
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetIntegrationResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetIntegrationResponse where
  toPath GetIntegrationResponse' {..} =
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

instance Lude.ToQuery GetIntegrationResponse where
  toQuery = Lude.const Lude.mempty
