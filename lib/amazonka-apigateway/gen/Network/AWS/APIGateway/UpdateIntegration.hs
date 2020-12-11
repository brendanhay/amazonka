{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateIntegration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents an update integration.
module Network.AWS.APIGateway.UpdateIntegration
  ( -- * Creating a request
    UpdateIntegration (..),
    mkUpdateIntegration,

    -- ** Request lenses
    updPatchOperations,
    updRestAPIId,
    updResourceId,
    updHttpMethod,

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

-- | Represents an update integration request.
--
-- /See:/ 'mkUpdateIntegration' smart constructor.
data UpdateIntegration = UpdateIntegration'
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    restAPIId :: Lude.Text,
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

-- | Creates a value of 'UpdateIntegration' with the minimum fields required to make a request.
--
-- * 'httpMethod' - [Required] Represents an update integration request's HTTP method.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
-- * 'resourceId' - [Required] Represents an update integration request's resource identifier.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkUpdateIntegration ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  UpdateIntegration
mkUpdateIntegration pRestAPIId_ pResourceId_ pHttpMethod_ =
  UpdateIntegration'
    { patchOperations = Lude.Nothing,
      restAPIId = pRestAPIId_,
      resourceId = pResourceId_,
      httpMethod = pHttpMethod_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updPatchOperations :: Lens.Lens' UpdateIntegration (Lude.Maybe [PatchOperation])
updPatchOperations = Lens.lens (patchOperations :: UpdateIntegration -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateIntegration)
{-# DEPRECATED updPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updRestAPIId :: Lens.Lens' UpdateIntegration Lude.Text
updRestAPIId = Lens.lens (restAPIId :: UpdateIntegration -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateIntegration)
{-# DEPRECATED updRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] Represents an update integration request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updResourceId :: Lens.Lens' UpdateIntegration Lude.Text
updResourceId = Lens.lens (resourceId :: UpdateIntegration -> Lude.Text) (\s a -> s {resourceId = a} :: UpdateIntegration)
{-# DEPRECATED updResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] Represents an update integration request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updHttpMethod :: Lens.Lens' UpdateIntegration Lude.Text
updHttpMethod = Lens.lens (httpMethod :: UpdateIntegration -> Lude.Text) (\s a -> s {httpMethod = a} :: UpdateIntegration)
{-# DEPRECATED updHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

instance Lude.AWSRequest UpdateIntegration where
  type Rs UpdateIntegration = Integration
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateIntegration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateIntegration where
  toJSON UpdateIntegration' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateIntegration where
  toPath UpdateIntegration' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod,
        "/integration"
      ]

instance Lude.ToQuery UpdateIntegration where
  toQuery = Lude.const Lude.mempty
