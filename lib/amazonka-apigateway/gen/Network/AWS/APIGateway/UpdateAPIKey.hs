{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateAPIKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an 'ApiKey' resource.
module Network.AWS.APIGateway.UpdateAPIKey
  ( -- * Creating a request
    UpdateAPIKey (..),
    mkUpdateAPIKey,

    -- ** Request lenses
    uakApiKey,
    uakPatchOperations,

    -- * Destructuring the response
    APIKey (..),
    mkAPIKey,

    -- ** Response lenses
    akEnabled,
    akValue,
    akCustomerId,
    akCreatedDate,
    akName,
    akId,
    akStageKeys,
    akLastUpdatedDate,
    akDescription,
    akTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to change information about an 'ApiKey' resource.
--
-- /See:/ 'mkUpdateAPIKey' smart constructor.
data UpdateAPIKey = UpdateAPIKey'
  { -- | [Required] The identifier of the 'ApiKey' resource to be updated.
    apiKey :: Lude.Text,
    -- | A list of update operations to be applied to the specified resource and in the order specified in this list.
    patchOperations :: Lude.Maybe [PatchOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPIKey' with the minimum fields required to make a request.
--
-- * 'apiKey' - [Required] The identifier of the 'ApiKey' resource to be updated.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateAPIKey ::
  -- | 'apiKey'
  Lude.Text ->
  UpdateAPIKey
mkUpdateAPIKey pApiKey_ =
  UpdateAPIKey' {apiKey = pApiKey_, patchOperations = Lude.Nothing}

-- | [Required] The identifier of the 'ApiKey' resource to be updated.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakApiKey :: Lens.Lens' UpdateAPIKey Lude.Text
uakApiKey = Lens.lens (apiKey :: UpdateAPIKey -> Lude.Text) (\s a -> s {apiKey = a} :: UpdateAPIKey)
{-# DEPRECATED uakApiKey "Use generic-lens or generic-optics with 'apiKey' instead." #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakPatchOperations :: Lens.Lens' UpdateAPIKey (Lude.Maybe [PatchOperation])
uakPatchOperations = Lens.lens (patchOperations :: UpdateAPIKey -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateAPIKey)
{-# DEPRECATED uakPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

instance Lude.AWSRequest UpdateAPIKey where
  type Rs UpdateAPIKey = APIKey
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateAPIKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateAPIKey where
  toJSON UpdateAPIKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateAPIKey where
  toPath UpdateAPIKey' {..} =
    Lude.mconcat ["/apikeys/", Lude.toBS apiKey]

instance Lude.ToQuery UpdateAPIKey where
  toQuery = Lude.const Lude.mempty
