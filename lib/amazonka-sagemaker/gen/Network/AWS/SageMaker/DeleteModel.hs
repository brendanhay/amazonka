{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model. The @DeleteModel@ API deletes only the model entry that was created in Amazon SageMaker when you called the 'CreateModel' API. It does not delete model artifacts, inference code, or the IAM role that you specified when creating the model.
module Network.AWS.SageMaker.DeleteModel
  ( -- * Creating a request
    DeleteModel (..),
    mkDeleteModel,

    -- ** Request lenses
    dmModelName,

    -- * Destructuring the response
    DeleteModelResponse (..),
    mkDeleteModelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteModel' smart constructor.
newtype DeleteModel = DeleteModel' {modelName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteModel' with the minimum fields required to make a request.
--
-- * 'modelName' - The name of the model to delete.
mkDeleteModel ::
  -- | 'modelName'
  Lude.Text ->
  DeleteModel
mkDeleteModel pModelName_ = DeleteModel' {modelName = pModelName_}

-- | The name of the model to delete.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmModelName :: Lens.Lens' DeleteModel Lude.Text
dmModelName = Lens.lens (modelName :: DeleteModel -> Lude.Text) (\s a -> s {modelName = a} :: DeleteModel)
{-# DEPRECATED dmModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

instance Lude.AWSRequest DeleteModel where
  type Rs DeleteModel = DeleteModelResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteModelResponse'

instance Lude.ToHeaders DeleteModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteModel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteModel where
  toJSON DeleteModel' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ModelName" Lude..= modelName)])

instance Lude.ToPath DeleteModel where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteModel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteModelResponse' smart constructor.
data DeleteModelResponse = DeleteModelResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteModelResponse' with the minimum fields required to make a request.
mkDeleteModelResponse ::
  DeleteModelResponse
mkDeleteModelResponse = DeleteModelResponse'
