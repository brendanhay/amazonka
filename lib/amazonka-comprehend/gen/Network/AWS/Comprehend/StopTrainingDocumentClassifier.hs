{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopTrainingDocumentClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a document classifier training job while in progress.
--
-- If the training job state is @TRAINING@ , the job is marked for termination and put into the @STOP_REQUESTED@ state. If the training job completes before it can be stopped, it is put into the @TRAINED@ ; otherwise the training job is stopped and put into the @STOPPED@ state and the service sends back an HTTP 200 response with an empty HTTP body.
module Network.AWS.Comprehend.StopTrainingDocumentClassifier
  ( -- * Creating a request
    StopTrainingDocumentClassifier (..),
    mkStopTrainingDocumentClassifier,

    -- ** Request lenses
    stdcDocumentClassifierARN,

    -- * Destructuring the response
    StopTrainingDocumentClassifierResponse (..),
    mkStopTrainingDocumentClassifierResponse,

    -- ** Response lenses
    stdcrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopTrainingDocumentClassifier' smart constructor.
newtype StopTrainingDocumentClassifier = StopTrainingDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier currently being trained.
    documentClassifierARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTrainingDocumentClassifier' with the minimum fields required to make a request.
--
-- * 'documentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier currently being trained.
mkStopTrainingDocumentClassifier ::
  -- | 'documentClassifierARN'
  Lude.Text ->
  StopTrainingDocumentClassifier
mkStopTrainingDocumentClassifier pDocumentClassifierARN_ =
  StopTrainingDocumentClassifier'
    { documentClassifierARN =
        pDocumentClassifierARN_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier currently being trained.
--
-- /Note:/ Consider using 'documentClassifierARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdcDocumentClassifierARN :: Lens.Lens' StopTrainingDocumentClassifier Lude.Text
stdcDocumentClassifierARN = Lens.lens (documentClassifierARN :: StopTrainingDocumentClassifier -> Lude.Text) (\s a -> s {documentClassifierARN = a} :: StopTrainingDocumentClassifier)
{-# DEPRECATED stdcDocumentClassifierARN "Use generic-lens or generic-optics with 'documentClassifierARN' instead." #-}

instance Lude.AWSRequest StopTrainingDocumentClassifier where
  type
    Rs StopTrainingDocumentClassifier =
      StopTrainingDocumentClassifierResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopTrainingDocumentClassifierResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopTrainingDocumentClassifier where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StopTrainingDocumentClassifier" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopTrainingDocumentClassifier where
  toJSON StopTrainingDocumentClassifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("DocumentClassifierArn" Lude..= documentClassifierARN)
          ]
      )

instance Lude.ToPath StopTrainingDocumentClassifier where
  toPath = Lude.const "/"

instance Lude.ToQuery StopTrainingDocumentClassifier where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopTrainingDocumentClassifierResponse' smart constructor.
newtype StopTrainingDocumentClassifierResponse = StopTrainingDocumentClassifierResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTrainingDocumentClassifierResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopTrainingDocumentClassifierResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopTrainingDocumentClassifierResponse
mkStopTrainingDocumentClassifierResponse pResponseStatus_ =
  StopTrainingDocumentClassifierResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdcrsResponseStatus :: Lens.Lens' StopTrainingDocumentClassifierResponse Lude.Int
stdcrsResponseStatus = Lens.lens (responseStatus :: StopTrainingDocumentClassifierResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopTrainingDocumentClassifierResponse)
{-# DEPRECATED stdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
