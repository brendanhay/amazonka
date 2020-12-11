{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetIntegration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the integration settings.
module Network.AWS.APIGateway.GetIntegration
  ( -- * Creating a request
    GetIntegration (..),
    mkGetIntegration,

    -- ** Request lenses
    giRestAPIId,
    giResourceId,
    giHttpMethod,

    -- * Destructuring the response
    Integration (..),
    mkIntegration,

    -- ** Response lenses
    iHttpMethod,
    iRequestTemplates,
    iCredentials,
    iConnectionId,
    iRequestParameters,
    iContentHandling,
    iPassthroughBehavior,
    iUri,
    iIntegrationResponses,
    iTlsConfig,
    iCacheNamespace,
    iTimeoutInMillis,
    iType,
    iConnectionType,
    iCacheKeyParameters,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to get the integration configuration.
--
-- /See:/ 'mkGetIntegration' smart constructor.
data GetIntegration = GetIntegration'
  { restAPIId :: Lude.Text,
    resourceId :: Lude.Text,
    httpMethod :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIntegration' with the minimum fields required to make a request.
--
-- * 'httpMethod' - [Required] Specifies a get integration request's HTTP method.
-- * 'resourceId' - [Required] Specifies a get integration request's resource identifier
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetIntegration ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  GetIntegration
mkGetIntegration pRestAPIId_ pResourceId_ pHttpMethod_ =
  GetIntegration'
    { restAPIId = pRestAPIId_,
      resourceId = pResourceId_,
      httpMethod = pHttpMethod_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giRestAPIId :: Lens.Lens' GetIntegration Lude.Text
giRestAPIId = Lens.lens (restAPIId :: GetIntegration -> Lude.Text) (\s a -> s {restAPIId = a} :: GetIntegration)
{-# DEPRECATED giRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] Specifies a get integration request's resource identifier
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giResourceId :: Lens.Lens' GetIntegration Lude.Text
giResourceId = Lens.lens (resourceId :: GetIntegration -> Lude.Text) (\s a -> s {resourceId = a} :: GetIntegration)
{-# DEPRECATED giResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] Specifies a get integration request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giHttpMethod :: Lens.Lens' GetIntegration Lude.Text
giHttpMethod = Lens.lens (httpMethod :: GetIntegration -> Lude.Text) (\s a -> s {httpMethod = a} :: GetIntegration)
{-# DEPRECATED giHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

instance Lude.AWSRequest GetIntegration where
  type Rs GetIntegration = Integration
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetIntegration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetIntegration where
  toPath GetIntegration' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod,
        "/integration"
      ]

instance Lude.ToQuery GetIntegration where
  toQuery = Lude.const Lude.mempty
