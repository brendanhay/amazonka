{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateRestAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the specified API.
module Network.AWS.APIGateway.UpdateRestAPI
  ( -- * Creating a request
    UpdateRestAPI (..),
    mkUpdateRestAPI,

    -- ** Request lenses
    uraPatchOperations,
    uraRestAPIId,

    -- * Destructuring the response
    RestAPI (..),
    mkRestAPI,

    -- ** Response lenses
    raMinimumCompressionSize,
    raDisableExecuteAPIEndpoint,
    raBinaryMediaTypes,
    raWarnings,
    raCreatedDate,
    raName,
    raVersion,
    raApiKeySource,
    raId,
    raPolicy,
    raEndpointConfiguration,
    raDescription,
    raTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to update an existing 'RestApi' resource in your collection.
--
-- /See:/ 'mkUpdateRestAPI' smart constructor.
data UpdateRestAPI = UpdateRestAPI'
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    restAPIId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRestAPI' with the minimum fields required to make a request.
--
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkUpdateRestAPI ::
  -- | 'restAPIId'
  Lude.Text ->
  UpdateRestAPI
mkUpdateRestAPI pRestAPIId_ =
  UpdateRestAPI'
    { patchOperations = Lude.Nothing,
      restAPIId = pRestAPIId_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uraPatchOperations :: Lens.Lens' UpdateRestAPI (Lude.Maybe [PatchOperation])
uraPatchOperations = Lens.lens (patchOperations :: UpdateRestAPI -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateRestAPI)
{-# DEPRECATED uraPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uraRestAPIId :: Lens.Lens' UpdateRestAPI Lude.Text
uraRestAPIId = Lens.lens (restAPIId :: UpdateRestAPI -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateRestAPI)
{-# DEPRECATED uraRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest UpdateRestAPI where
  type Rs UpdateRestAPI = RestAPI
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateRestAPI where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateRestAPI where
  toJSON UpdateRestAPI' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateRestAPI where
  toPath UpdateRestAPI' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId]

instance Lude.ToQuery UpdateRestAPI where
  toQuery = Lude.const Lude.mempty
