{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.DescribeLanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a single custom language model. Use this information to see details about the language model in your AWS account. You can also see whether the base language model used to create your custom language model has been updated. If Amazon Transcribe has updated the base model, you can create a new custom language model using the updated base model. If the language model wasn't created, you can use this operation to understand why Amazon Transcribe couldn't create it.
module Network.AWS.Transcribe.DescribeLanguageModel
  ( -- * Creating a request
    DescribeLanguageModel (..),
    mkDescribeLanguageModel,

    -- ** Request lenses
    dModelName,

    -- * Destructuring the response
    DescribeLanguageModelResponse (..),
    mkDescribeLanguageModelResponse,

    -- ** Response lenses
    dlmrsLanguageModel,
    dlmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkDescribeLanguageModel' smart constructor.
newtype DescribeLanguageModel = DescribeLanguageModel'
  { -- | The name of the custom language model you submit to get more information.
    modelName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLanguageModel' with the minimum fields required to make a request.
--
-- * 'modelName' - The name of the custom language model you submit to get more information.
mkDescribeLanguageModel ::
  -- | 'modelName'
  Lude.Text ->
  DescribeLanguageModel
mkDescribeLanguageModel pModelName_ =
  DescribeLanguageModel' {modelName = pModelName_}

-- | The name of the custom language model you submit to get more information.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelName :: Lens.Lens' DescribeLanguageModel Lude.Text
dModelName = Lens.lens (modelName :: DescribeLanguageModel -> Lude.Text) (\s a -> s {modelName = a} :: DescribeLanguageModel)
{-# DEPRECATED dModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

instance Lude.AWSRequest DescribeLanguageModel where
  type Rs DescribeLanguageModel = DescribeLanguageModelResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLanguageModelResponse'
            Lude.<$> (x Lude..?> "LanguageModel")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLanguageModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.DescribeLanguageModel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLanguageModel where
  toJSON DescribeLanguageModel' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ModelName" Lude..= modelName)])

instance Lude.ToPath DescribeLanguageModel where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLanguageModel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeLanguageModelResponse' smart constructor.
data DescribeLanguageModelResponse = DescribeLanguageModelResponse'
  { -- | The name of the custom language model you requested more information about.
    languageModel :: Lude.Maybe LanguageModel,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLanguageModelResponse' with the minimum fields required to make a request.
--
-- * 'languageModel' - The name of the custom language model you requested more information about.
-- * 'responseStatus' - The response status code.
mkDescribeLanguageModelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLanguageModelResponse
mkDescribeLanguageModelResponse pResponseStatus_ =
  DescribeLanguageModelResponse'
    { languageModel = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the custom language model you requested more information about.
--
-- /Note:/ Consider using 'languageModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlmrsLanguageModel :: Lens.Lens' DescribeLanguageModelResponse (Lude.Maybe LanguageModel)
dlmrsLanguageModel = Lens.lens (languageModel :: DescribeLanguageModelResponse -> Lude.Maybe LanguageModel) (\s a -> s {languageModel = a} :: DescribeLanguageModelResponse)
{-# DEPRECATED dlmrsLanguageModel "Use generic-lens or generic-optics with 'languageModel' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlmrsResponseStatus :: Lens.Lens' DescribeLanguageModelResponse Lude.Int
dlmrsResponseStatus = Lens.lens (responseStatus :: DescribeLanguageModelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLanguageModelResponse)
{-# DEPRECATED dlmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
