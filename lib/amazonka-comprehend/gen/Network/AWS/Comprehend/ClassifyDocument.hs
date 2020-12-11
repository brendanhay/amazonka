{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ClassifyDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document classification request to analyze a single document in real-time, using a previously created and trained custom model and an endpoint.
module Network.AWS.Comprehend.ClassifyDocument
  ( -- * Creating a request
    ClassifyDocument (..),
    mkClassifyDocument,

    -- ** Request lenses
    cdText,
    cdEndpointARN,

    -- * Destructuring the response
    ClassifyDocumentResponse (..),
    mkClassifyDocumentResponse,

    -- ** Response lenses
    cdrsLabels,
    cdrsClasses,
    cdrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkClassifyDocument' smart constructor.
data ClassifyDocument = ClassifyDocument'
  { text ::
      Lude.Sensitive Lude.Text,
    endpointARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClassifyDocument' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Number (ARN) of the endpoint.
-- * 'text' - The document text to be analyzed.
mkClassifyDocument ::
  -- | 'text'
  Lude.Sensitive Lude.Text ->
  -- | 'endpointARN'
  Lude.Text ->
  ClassifyDocument
mkClassifyDocument pText_ pEndpointARN_ =
  ClassifyDocument' {text = pText_, endpointARN = pEndpointARN_}

-- | The document text to be analyzed.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdText :: Lens.Lens' ClassifyDocument (Lude.Sensitive Lude.Text)
cdText = Lens.lens (text :: ClassifyDocument -> Lude.Sensitive Lude.Text) (\s a -> s {text = a} :: ClassifyDocument)
{-# DEPRECATED cdText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The Amazon Resource Number (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEndpointARN :: Lens.Lens' ClassifyDocument Lude.Text
cdEndpointARN = Lens.lens (endpointARN :: ClassifyDocument -> Lude.Text) (\s a -> s {endpointARN = a} :: ClassifyDocument)
{-# DEPRECATED cdEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest ClassifyDocument where
  type Rs ClassifyDocument = ClassifyDocumentResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ClassifyDocumentResponse'
            Lude.<$> (x Lude..?> "Labels" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Classes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ClassifyDocument where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.ClassifyDocument" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ClassifyDocument where
  toJSON ClassifyDocument' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Text" Lude..= text),
            Lude.Just ("EndpointArn" Lude..= endpointARN)
          ]
      )

instance Lude.ToPath ClassifyDocument where
  toPath = Lude.const "/"

instance Lude.ToQuery ClassifyDocument where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkClassifyDocumentResponse' smart constructor.
data ClassifyDocumentResponse = ClassifyDocumentResponse'
  { labels ::
      Lude.Maybe [DocumentLabel],
    classes :: Lude.Maybe [DocumentClass],
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

-- | Creates a value of 'ClassifyDocumentResponse' with the minimum fields required to make a request.
--
-- * 'classes' - The classes used by the document being analyzed. These are used for multi-class trained models. Individual classes are mutually exclusive and each document is expected to have only a single class assigned to it. For example, an animal can be a dog or a cat, but not both at the same time.
-- * 'labels' - The labels used the document being analyzed. These are used for multi-label trained models. Individual labels represent different categories that are related in some manner and are not mutually exclusive. For example, a movie can be just an action movie, or it can be an action movie, a science fiction movie, and a comedy, all at the same time.
-- * 'responseStatus' - The response status code.
mkClassifyDocumentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ClassifyDocumentResponse
mkClassifyDocumentResponse pResponseStatus_ =
  ClassifyDocumentResponse'
    { labels = Lude.Nothing,
      classes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The labels used the document being analyzed. These are used for multi-label trained models. Individual labels represent different categories that are related in some manner and are not mutually exclusive. For example, a movie can be just an action movie, or it can be an action movie, a science fiction movie, and a comedy, all at the same time.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsLabels :: Lens.Lens' ClassifyDocumentResponse (Lude.Maybe [DocumentLabel])
cdrsLabels = Lens.lens (labels :: ClassifyDocumentResponse -> Lude.Maybe [DocumentLabel]) (\s a -> s {labels = a} :: ClassifyDocumentResponse)
{-# DEPRECATED cdrsLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The classes used by the document being analyzed. These are used for multi-class trained models. Individual classes are mutually exclusive and each document is expected to have only a single class assigned to it. For example, an animal can be a dog or a cat, but not both at the same time.
--
-- /Note:/ Consider using 'classes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsClasses :: Lens.Lens' ClassifyDocumentResponse (Lude.Maybe [DocumentClass])
cdrsClasses = Lens.lens (classes :: ClassifyDocumentResponse -> Lude.Maybe [DocumentClass]) (\s a -> s {classes = a} :: ClassifyDocumentResponse)
{-# DEPRECATED cdrsClasses "Use generic-lens or generic-optics with 'classes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' ClassifyDocumentResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: ClassifyDocumentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ClassifyDocumentResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
