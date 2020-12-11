{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeDocumentClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a document classifier.
module Network.AWS.Comprehend.DescribeDocumentClassifier
  ( -- * Creating a request
    DescribeDocumentClassifier (..),
    mkDescribeDocumentClassifier,

    -- ** Request lenses
    ddcDocumentClassifierARN,

    -- * Destructuring the response
    DescribeDocumentClassifierResponse (..),
    mkDescribeDocumentClassifierResponse,

    -- ** Response lenses
    ddcrsDocumentClassifierProperties,
    ddcrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDocumentClassifier' smart constructor.
newtype DescribeDocumentClassifier = DescribeDocumentClassifier'
  { documentClassifierARN ::
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

-- | Creates a value of 'DescribeDocumentClassifier' with the minimum fields required to make a request.
--
-- * 'documentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier. The operation returns this identifier in its response.
mkDescribeDocumentClassifier ::
  -- | 'documentClassifierARN'
  Lude.Text ->
  DescribeDocumentClassifier
mkDescribeDocumentClassifier pDocumentClassifierARN_ =
  DescribeDocumentClassifier'
    { documentClassifierARN =
        pDocumentClassifierARN_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'documentClassifierARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDocumentClassifierARN :: Lens.Lens' DescribeDocumentClassifier Lude.Text
ddcDocumentClassifierARN = Lens.lens (documentClassifierARN :: DescribeDocumentClassifier -> Lude.Text) (\s a -> s {documentClassifierARN = a} :: DescribeDocumentClassifier)
{-# DEPRECATED ddcDocumentClassifierARN "Use generic-lens or generic-optics with 'documentClassifierARN' instead." #-}

instance Lude.AWSRequest DescribeDocumentClassifier where
  type
    Rs DescribeDocumentClassifier =
      DescribeDocumentClassifierResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDocumentClassifierResponse'
            Lude.<$> (x Lude..?> "DocumentClassifierProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDocumentClassifier where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DescribeDocumentClassifier" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDocumentClassifier where
  toJSON DescribeDocumentClassifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("DocumentClassifierArn" Lude..= documentClassifierARN)
          ]
      )

instance Lude.ToPath DescribeDocumentClassifier where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDocumentClassifier where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDocumentClassifierResponse' smart constructor.
data DescribeDocumentClassifierResponse = DescribeDocumentClassifierResponse'
  { documentClassifierProperties ::
      Lude.Maybe
        DocumentClassifierProperties,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDocumentClassifierResponse' with the minimum fields required to make a request.
--
-- * 'documentClassifierProperties' - An object that contains the properties associated with a document classifier.
-- * 'responseStatus' - The response status code.
mkDescribeDocumentClassifierResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDocumentClassifierResponse
mkDescribeDocumentClassifierResponse pResponseStatus_ =
  DescribeDocumentClassifierResponse'
    { documentClassifierProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains the properties associated with a document classifier.
--
-- /Note:/ Consider using 'documentClassifierProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsDocumentClassifierProperties :: Lens.Lens' DescribeDocumentClassifierResponse (Lude.Maybe DocumentClassifierProperties)
ddcrsDocumentClassifierProperties = Lens.lens (documentClassifierProperties :: DescribeDocumentClassifierResponse -> Lude.Maybe DocumentClassifierProperties) (\s a -> s {documentClassifierProperties = a} :: DescribeDocumentClassifierResponse)
{-# DEPRECATED ddcrsDocumentClassifierProperties "Use generic-lens or generic-optics with 'documentClassifierProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsResponseStatus :: Lens.Lens' DescribeDocumentClassifierResponse Lude.Int
ddcrsResponseStatus = Lens.lens (responseStatus :: DescribeDocumentClassifierResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDocumentClassifierResponse)
{-# DEPRECATED ddcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
