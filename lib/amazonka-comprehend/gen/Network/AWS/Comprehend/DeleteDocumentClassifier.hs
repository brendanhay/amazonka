{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DeleteDocumentClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously created document classifier
--
-- Only those classifiers that are in terminated states (IN_ERROR, TRAINED) will be deleted. If an active inference job is using the model, a @ResourceInUseException@ will be returned.
-- This is an asynchronous action that puts the classifier into a DELETING state, and it is then removed by a background job. Once removed, the classifier disappears from your account and is no longer available for use.
module Network.AWS.Comprehend.DeleteDocumentClassifier
  ( -- * Creating a request
    DeleteDocumentClassifier (..),
    mkDeleteDocumentClassifier,

    -- ** Request lenses
    dDocumentClassifierARN,

    -- * Destructuring the response
    DeleteDocumentClassifierResponse (..),
    mkDeleteDocumentClassifierResponse,

    -- ** Response lenses
    ddcfrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDocumentClassifier' smart constructor.
newtype DeleteDocumentClassifier = DeleteDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDocumentClassifier' with the minimum fields required to make a request.
--
-- * 'documentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier.
mkDeleteDocumentClassifier ::
  -- | 'documentClassifierARN'
  Lude.Text ->
  DeleteDocumentClassifier
mkDeleteDocumentClassifier pDocumentClassifierARN_ =
  DeleteDocumentClassifier'
    { documentClassifierARN =
        pDocumentClassifierARN_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- /Note:/ Consider using 'documentClassifierARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDocumentClassifierARN :: Lens.Lens' DeleteDocumentClassifier Lude.Text
dDocumentClassifierARN = Lens.lens (documentClassifierARN :: DeleteDocumentClassifier -> Lude.Text) (\s a -> s {documentClassifierARN = a} :: DeleteDocumentClassifier)
{-# DEPRECATED dDocumentClassifierARN "Use generic-lens or generic-optics with 'documentClassifierARN' instead." #-}

instance Lude.AWSRequest DeleteDocumentClassifier where
  type Rs DeleteDocumentClassifier = DeleteDocumentClassifierResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDocumentClassifierResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDocumentClassifier where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DeleteDocumentClassifier" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDocumentClassifier where
  toJSON DeleteDocumentClassifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("DocumentClassifierArn" Lude..= documentClassifierARN)
          ]
      )

instance Lude.ToPath DeleteDocumentClassifier where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDocumentClassifier where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDocumentClassifierResponse' smart constructor.
newtype DeleteDocumentClassifierResponse = DeleteDocumentClassifierResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDocumentClassifierResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDocumentClassifierResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDocumentClassifierResponse
mkDeleteDocumentClassifierResponse pResponseStatus_ =
  DeleteDocumentClassifierResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcfrsResponseStatus :: Lens.Lens' DeleteDocumentClassifierResponse Lude.Int
ddcfrsResponseStatus = Lens.lens (responseStatus :: DeleteDocumentClassifierResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDocumentClassifierResponse)
{-# DEPRECATED ddcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
