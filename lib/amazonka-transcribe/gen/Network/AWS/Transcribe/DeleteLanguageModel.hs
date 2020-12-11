{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.DeleteLanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom language model using its name.
module Network.AWS.Transcribe.DeleteLanguageModel
  ( -- * Creating a request
    DeleteLanguageModel (..),
    mkDeleteLanguageModel,

    -- ** Request lenses
    dlmModelName,

    -- * Destructuring the response
    DeleteLanguageModelResponse (..),
    mkDeleteLanguageModelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkDeleteLanguageModel' smart constructor.
newtype DeleteLanguageModel = DeleteLanguageModel'
  { modelName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLanguageModel' with the minimum fields required to make a request.
--
-- * 'modelName' - The name of the model you're choosing to delete.
mkDeleteLanguageModel ::
  -- | 'modelName'
  Lude.Text ->
  DeleteLanguageModel
mkDeleteLanguageModel pModelName_ =
  DeleteLanguageModel' {modelName = pModelName_}

-- | The name of the model you're choosing to delete.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlmModelName :: Lens.Lens' DeleteLanguageModel Lude.Text
dlmModelName = Lens.lens (modelName :: DeleteLanguageModel -> Lude.Text) (\s a -> s {modelName = a} :: DeleteLanguageModel)
{-# DEPRECATED dlmModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

instance Lude.AWSRequest DeleteLanguageModel where
  type Rs DeleteLanguageModel = DeleteLanguageModelResponse
  request = Req.postJSON transcribeService
  response = Res.receiveNull DeleteLanguageModelResponse'

instance Lude.ToHeaders DeleteLanguageModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.DeleteLanguageModel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLanguageModel where
  toJSON DeleteLanguageModel' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ModelName" Lude..= modelName)])

instance Lude.ToPath DeleteLanguageModel where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLanguageModel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLanguageModelResponse' smart constructor.
data DeleteLanguageModelResponse = DeleteLanguageModelResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLanguageModelResponse' with the minimum fields required to make a request.
mkDeleteLanguageModelResponse ::
  DeleteLanguageModelResponse
mkDeleteLanguageModelResponse = DeleteLanguageModelResponse'
