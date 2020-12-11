{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopTrainingEntityRecognizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an entity recognizer training job while in progress.
--
-- If the training job state is @TRAINING@ , the job is marked for termination and put into the @STOP_REQUESTED@ state. If the training job completes before it can be stopped, it is put into the @TRAINED@ ; otherwise the training job is stopped and putted into the @STOPPED@ state and the service sends back an HTTP 200 response with an empty HTTP body.
module Network.AWS.Comprehend.StopTrainingEntityRecognizer
  ( -- * Creating a request
    StopTrainingEntityRecognizer (..),
    mkStopTrainingEntityRecognizer,

    -- ** Request lenses
    sterEntityRecognizerARN,

    -- * Destructuring the response
    StopTrainingEntityRecognizerResponse (..),
    mkStopTrainingEntityRecognizerResponse,

    -- ** Response lenses
    sterrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopTrainingEntityRecognizer' smart constructor.
newtype StopTrainingEntityRecognizer = StopTrainingEntityRecognizer'
  { entityRecognizerARN ::
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

-- | Creates a value of 'StopTrainingEntityRecognizer' with the minimum fields required to make a request.
--
-- * 'entityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer currently being trained.
mkStopTrainingEntityRecognizer ::
  -- | 'entityRecognizerARN'
  Lude.Text ->
  StopTrainingEntityRecognizer
mkStopTrainingEntityRecognizer pEntityRecognizerARN_ =
  StopTrainingEntityRecognizer'
    { entityRecognizerARN =
        pEntityRecognizerARN_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer currently being trained.
--
-- /Note:/ Consider using 'entityRecognizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sterEntityRecognizerARN :: Lens.Lens' StopTrainingEntityRecognizer Lude.Text
sterEntityRecognizerARN = Lens.lens (entityRecognizerARN :: StopTrainingEntityRecognizer -> Lude.Text) (\s a -> s {entityRecognizerARN = a} :: StopTrainingEntityRecognizer)
{-# DEPRECATED sterEntityRecognizerARN "Use generic-lens or generic-optics with 'entityRecognizerARN' instead." #-}

instance Lude.AWSRequest StopTrainingEntityRecognizer where
  type
    Rs StopTrainingEntityRecognizer =
      StopTrainingEntityRecognizerResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopTrainingEntityRecognizerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopTrainingEntityRecognizer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StopTrainingEntityRecognizer" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopTrainingEntityRecognizer where
  toJSON StopTrainingEntityRecognizer' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EntityRecognizerArn" Lude..= entityRecognizerARN)]
      )

instance Lude.ToPath StopTrainingEntityRecognizer where
  toPath = Lude.const "/"

instance Lude.ToQuery StopTrainingEntityRecognizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopTrainingEntityRecognizerResponse' smart constructor.
newtype StopTrainingEntityRecognizerResponse = StopTrainingEntityRecognizerResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTrainingEntityRecognizerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopTrainingEntityRecognizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopTrainingEntityRecognizerResponse
mkStopTrainingEntityRecognizerResponse pResponseStatus_ =
  StopTrainingEntityRecognizerResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sterrsResponseStatus :: Lens.Lens' StopTrainingEntityRecognizerResponse Lude.Int
sterrsResponseStatus = Lens.lens (responseStatus :: StopTrainingEntityRecognizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopTrainingEntityRecognizerResponse)
{-# DEPRECATED sterrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
