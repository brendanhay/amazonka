{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteMLModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the @DELETED@ status to an @MLModel@ , rendering it unusable.
--
-- After using the @DeleteMLModel@ operation, you can use the @GetMLModel@ operation to verify that the status of the @MLModel@ changed to DELETED.
-- __Caution:__ The result of the @DeleteMLModel@ operation is irreversible.
module Network.AWS.MachineLearning.DeleteMLModel
  ( -- * Creating a request
    DeleteMLModel (..),
    mkDeleteMLModel,

    -- ** Request lenses
    dmlmMLModelId,

    -- * Destructuring the response
    DeleteMLModelResponse (..),
    mkDeleteMLModelResponse,

    -- ** Response lenses
    dmlmrsMLModelId,
    dmlmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteMLModel' smart constructor.
newtype DeleteMLModel = DeleteMLModel' {mLModelId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMLModel' with the minimum fields required to make a request.
--
-- * 'mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ .
mkDeleteMLModel ::
  -- | 'mLModelId'
  Lude.Text ->
  DeleteMLModel
mkDeleteMLModel pMLModelId_ =
  DeleteMLModel' {mLModelId = pMLModelId_}

-- | A user-supplied ID that uniquely identifies the @MLModel@ .
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmMLModelId :: Lens.Lens' DeleteMLModel Lude.Text
dmlmMLModelId = Lens.lens (mLModelId :: DeleteMLModel -> Lude.Text) (\s a -> s {mLModelId = a} :: DeleteMLModel)
{-# DEPRECATED dmlmMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

instance Lude.AWSRequest DeleteMLModel where
  type Rs DeleteMLModel = DeleteMLModelResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteMLModelResponse'
            Lude.<$> (x Lude..?> "MLModelId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteMLModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DeleteMLModel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMLModel where
  toJSON DeleteMLModel' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("MLModelId" Lude..= mLModelId)])

instance Lude.ToPath DeleteMLModel where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMLModel where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DeleteMLModel@ operation.
--
-- You can use the @GetMLModel@ operation and check the value of the @Status@ parameter to see whether an @MLModel@ is marked as @DELETED@ .
--
-- /See:/ 'mkDeleteMLModelResponse' smart constructor.
data DeleteMLModelResponse = DeleteMLModelResponse'
  { mLModelId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMLModelResponse' with the minimum fields required to make a request.
--
-- * 'mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelID@ in the request.
-- * 'responseStatus' - The response status code.
mkDeleteMLModelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMLModelResponse
mkDeleteMLModelResponse pResponseStatus_ =
  DeleteMLModelResponse'
    { mLModelId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelID@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmrsMLModelId :: Lens.Lens' DeleteMLModelResponse (Lude.Maybe Lude.Text)
dmlmrsMLModelId = Lens.lens (mLModelId :: DeleteMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: DeleteMLModelResponse)
{-# DEPRECATED dmlmrsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmrsResponseStatus :: Lens.Lens' DeleteMLModelResponse Lude.Int
dmlmrsResponseStatus = Lens.lens (responseStatus :: DeleteMLModelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMLModelResponse)
{-# DEPRECATED dmlmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
