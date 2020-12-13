{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetMethodResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a 'MethodResponse' resource.
module Network.AWS.APIGateway.GetMethodResponse
  ( -- * Creating a request
    GetMethodResponse (..),
    mkGetMethodResponse,

    -- ** Request lenses
    gmgResourceId,
    gmgHttpMethod,
    gmgRestAPIId,
    gmgStatusCode,

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

-- | Request to describe a 'MethodResponse' resource.
--
-- /See:/ 'mkGetMethodResponse' smart constructor.
data GetMethodResponse = GetMethodResponse'
  { -- | [Required] The 'Resource' identifier for the 'MethodResponse' resource.
    resourceId :: Lude.Text,
    -- | [Required] The HTTP verb of the 'Method' resource.
    httpMethod :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | [Required] The status code for the 'MethodResponse' resource.
    statusCode :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMethodResponse' with the minimum fields required to make a request.
--
-- * 'resourceId' - [Required] The 'Resource' identifier for the 'MethodResponse' resource.
-- * 'httpMethod' - [Required] The HTTP verb of the 'Method' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'statusCode' - [Required] The status code for the 'MethodResponse' resource.
mkGetMethodResponse ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'statusCode'
  Lude.Text ->
  GetMethodResponse
mkGetMethodResponse
  pResourceId_
  pHttpMethod_
  pRestAPIId_
  pStatusCode_ =
    GetMethodResponse'
      { resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        restAPIId = pRestAPIId_,
        statusCode = pStatusCode_
      }

-- | [Required] The 'Resource' identifier for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmgResourceId :: Lens.Lens' GetMethodResponse Lude.Text
gmgResourceId = Lens.lens (resourceId :: GetMethodResponse -> Lude.Text) (\s a -> s {resourceId = a} :: GetMethodResponse)
{-# DEPRECATED gmgResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmgHttpMethod :: Lens.Lens' GetMethodResponse Lude.Text
gmgHttpMethod = Lens.lens (httpMethod :: GetMethodResponse -> Lude.Text) (\s a -> s {httpMethod = a} :: GetMethodResponse)
{-# DEPRECATED gmgHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmgRestAPIId :: Lens.Lens' GetMethodResponse Lude.Text
gmgRestAPIId = Lens.lens (restAPIId :: GetMethodResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: GetMethodResponse)
{-# DEPRECATED gmgRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The status code for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmgStatusCode :: Lens.Lens' GetMethodResponse Lude.Text
gmgStatusCode = Lens.lens (statusCode :: GetMethodResponse -> Lude.Text) (\s a -> s {statusCode = a} :: GetMethodResponse)
{-# DEPRECATED gmgStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.AWSRequest GetMethodResponse where
  type Rs GetMethodResponse = MethodResponse
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetMethodResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetMethodResponse where
  toPath GetMethodResponse' {..} =
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

instance Lude.ToQuery GetMethodResponse where
  toQuery = Lude.const Lude.mempty
