{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a model.
module Network.AWS.APIGateway.UpdateModel
  ( -- * Creating a request
    UpdateModel (..),
    mkUpdateModel,

    -- ** Request lenses
    uPatchOperations,
    uRestAPIId,
    uModelName,

    -- * Destructuring the response
    Model (..),
    mkModel,

    -- ** Response lenses
    mSchema,
    mName,
    mId,
    mDescription,
    mContentType,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to update an existing model in an existing 'RestApi' resource.
--
-- /See:/ 'mkUpdateModel' smart constructor.
data UpdateModel = UpdateModel'
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    restAPIId :: Lude.Text,
    modelName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateModel' with the minimum fields required to make a request.
--
-- * 'modelName' - [Required] The name of the model to update.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkUpdateModel ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'modelName'
  Lude.Text ->
  UpdateModel
mkUpdateModel pRestAPIId_ pModelName_ =
  UpdateModel'
    { patchOperations = Lude.Nothing,
      restAPIId = pRestAPIId_,
      modelName = pModelName_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPatchOperations :: Lens.Lens' UpdateModel (Lude.Maybe [PatchOperation])
uPatchOperations = Lens.lens (patchOperations :: UpdateModel -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateModel)
{-# DEPRECATED uPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRestAPIId :: Lens.Lens' UpdateModel Lude.Text
uRestAPIId = Lens.lens (restAPIId :: UpdateModel -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateModel)
{-# DEPRECATED uRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The name of the model to update.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uModelName :: Lens.Lens' UpdateModel Lude.Text
uModelName = Lens.lens (modelName :: UpdateModel -> Lude.Text) (\s a -> s {modelName = a} :: UpdateModel)
{-# DEPRECATED uModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

instance Lude.AWSRequest UpdateModel where
  type Rs UpdateModel = Model
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateModel where
  toJSON UpdateModel' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateModel where
  toPath UpdateModel' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/models/",
        Lude.toBS modelName
      ]

instance Lude.ToQuery UpdateModel where
  toQuery = Lude.const Lude.mempty
