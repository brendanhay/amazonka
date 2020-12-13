{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model.
module Network.AWS.APIGateway.DeleteModel
  ( -- * Creating a request
    DeleteModel (..),
    mkDeleteModel,

    -- ** Request lenses
    dModelName,
    dRestAPIId,

    -- * Destructuring the response
    DeleteModelResponse (..),
    mkDeleteModelResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to delete an existing model in an existing 'RestApi' resource.
--
-- /See:/ 'mkDeleteModel' smart constructor.
data DeleteModel = DeleteModel'
  { -- | [Required] The name of the model to delete.
    modelName :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteModel' with the minimum fields required to make a request.
--
-- * 'modelName' - [Required] The name of the model to delete.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkDeleteModel ::
  -- | 'modelName'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  DeleteModel
mkDeleteModel pModelName_ pRestAPIId_ =
  DeleteModel' {modelName = pModelName_, restAPIId = pRestAPIId_}

-- | [Required] The name of the model to delete.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelName :: Lens.Lens' DeleteModel Lude.Text
dModelName = Lens.lens (modelName :: DeleteModel -> Lude.Text) (\s a -> s {modelName = a} :: DeleteModel)
{-# DEPRECATED dModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRestAPIId :: Lens.Lens' DeleteModel Lude.Text
dRestAPIId = Lens.lens (restAPIId :: DeleteModel -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteModel)
{-# DEPRECATED dRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest DeleteModel where
  type Rs DeleteModel = DeleteModelResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteModelResponse'

instance Lude.ToHeaders DeleteModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteModel where
  toPath DeleteModel' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/models/",
        Lude.toBS modelName
      ]

instance Lude.ToQuery DeleteModel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteModelResponse' smart constructor.
data DeleteModelResponse = DeleteModelResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteModelResponse' with the minimum fields required to make a request.
mkDeleteModelResponse ::
  DeleteModelResponse
mkDeleteModelResponse = DeleteModelResponse'
