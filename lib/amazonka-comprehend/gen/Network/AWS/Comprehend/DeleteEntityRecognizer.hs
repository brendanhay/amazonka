{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DeleteEntityRecognizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an entity recognizer.
--
-- Only those recognizers that are in terminated states (IN_ERROR, TRAINED) will be deleted. If an active inference job is using the model, a @ResourceInUseException@ will be returned.
-- This is an asynchronous action that puts the recognizer into a DELETING state, and it is then removed by a background job. Once removed, the recognizer disappears from your account and is no longer available for use.
module Network.AWS.Comprehend.DeleteEntityRecognizer
  ( -- * Creating a request
    DeleteEntityRecognizer (..),
    mkDeleteEntityRecognizer,

    -- ** Request lenses
    derEntityRecognizerARN,

    -- * Destructuring the response
    DeleteEntityRecognizerResponse (..),
    mkDeleteEntityRecognizerResponse,

    -- ** Response lenses
    derrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEntityRecognizer' smart constructor.
newtype DeleteEntityRecognizer = DeleteEntityRecognizer'
  { -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEntityRecognizer' with the minimum fields required to make a request.
--
-- * 'entityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
mkDeleteEntityRecognizer ::
  -- | 'entityRecognizerARN'
  Lude.Text ->
  DeleteEntityRecognizer
mkDeleteEntityRecognizer pEntityRecognizerARN_ =
  DeleteEntityRecognizer'
    { entityRecognizerARN =
        pEntityRecognizerARN_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derEntityRecognizerARN :: Lens.Lens' DeleteEntityRecognizer Lude.Text
derEntityRecognizerARN = Lens.lens (entityRecognizerARN :: DeleteEntityRecognizer -> Lude.Text) (\s a -> s {entityRecognizerARN = a} :: DeleteEntityRecognizer)
{-# DEPRECATED derEntityRecognizerARN "Use generic-lens or generic-optics with 'entityRecognizerARN' instead." #-}

instance Lude.AWSRequest DeleteEntityRecognizer where
  type Rs DeleteEntityRecognizer = DeleteEntityRecognizerResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteEntityRecognizerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteEntityRecognizer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.DeleteEntityRecognizer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteEntityRecognizer where
  toJSON DeleteEntityRecognizer' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EntityRecognizerArn" Lude..= entityRecognizerARN)]
      )

instance Lude.ToPath DeleteEntityRecognizer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEntityRecognizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteEntityRecognizerResponse' smart constructor.
newtype DeleteEntityRecognizerResponse = DeleteEntityRecognizerResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEntityRecognizerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteEntityRecognizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteEntityRecognizerResponse
mkDeleteEntityRecognizerResponse pResponseStatus_ =
  DeleteEntityRecognizerResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DeleteEntityRecognizerResponse Lude.Int
derrsResponseStatus = Lens.lens (responseStatus :: DeleteEntityRecognizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEntityRecognizerResponse)
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
