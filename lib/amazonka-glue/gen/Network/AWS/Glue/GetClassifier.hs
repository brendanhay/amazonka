{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a classifier by name.
module Network.AWS.Glue.GetClassifier
  ( -- * Creating a request
    GetClassifier (..),
    mkGetClassifier,

    -- ** Request lenses
    gName,

    -- * Destructuring the response
    GetClassifierResponse (..),
    mkGetClassifierResponse,

    -- ** Response lenses
    gcrsClassifier,
    gcrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetClassifier' smart constructor.
newtype GetClassifier = GetClassifier'
  { -- | Name of the classifier to retrieve.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetClassifier' with the minimum fields required to make a request.
--
-- * 'name' - Name of the classifier to retrieve.
mkGetClassifier ::
  -- | 'name'
  Lude.Text ->
  GetClassifier
mkGetClassifier pName_ = GetClassifier' {name = pName_}

-- | Name of the classifier to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' GetClassifier Lude.Text
gName = Lens.lens (name :: GetClassifier -> Lude.Text) (\s a -> s {name = a} :: GetClassifier)
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetClassifier where
  type Rs GetClassifier = GetClassifierResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetClassifierResponse'
            Lude.<$> (x Lude..?> "Classifier") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetClassifier where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetClassifier" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetClassifier where
  toJSON GetClassifier' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath GetClassifier where
  toPath = Lude.const "/"

instance Lude.ToQuery GetClassifier where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetClassifierResponse' smart constructor.
data GetClassifierResponse = GetClassifierResponse'
  { -- | The requested classifier.
    classifier :: Lude.Maybe Classifier,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetClassifierResponse' with the minimum fields required to make a request.
--
-- * 'classifier' - The requested classifier.
-- * 'responseStatus' - The response status code.
mkGetClassifierResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetClassifierResponse
mkGetClassifierResponse pResponseStatus_ =
  GetClassifierResponse'
    { classifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested classifier.
--
-- /Note:/ Consider using 'classifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsClassifier :: Lens.Lens' GetClassifierResponse (Lude.Maybe Classifier)
gcrsClassifier = Lens.lens (classifier :: GetClassifierResponse -> Lude.Maybe Classifier) (\s a -> s {classifier = a} :: GetClassifierResponse)
{-# DEPRECATED gcrsClassifier "Use generic-lens or generic-optics with 'classifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetClassifierResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetClassifierResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetClassifierResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
