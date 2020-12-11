{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeEntityRecognizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about an entity recognizer including status, S3 buckets containing training data, recognizer metadata, metrics, and so on.
module Network.AWS.Comprehend.DescribeEntityRecognizer
  ( -- * Creating a request
    DescribeEntityRecognizer (..),
    mkDescribeEntityRecognizer,

    -- ** Request lenses
    dEntityRecognizerARN,

    -- * Destructuring the response
    DescribeEntityRecognizerResponse (..),
    mkDescribeEntityRecognizerResponse,

    -- ** Response lenses
    drsEntityRecognizerProperties,
    drsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEntityRecognizer' smart constructor.
newtype DescribeEntityRecognizer = DescribeEntityRecognizer'
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

-- | Creates a value of 'DescribeEntityRecognizer' with the minimum fields required to make a request.
--
-- * 'entityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
mkDescribeEntityRecognizer ::
  -- | 'entityRecognizerARN'
  Lude.Text ->
  DescribeEntityRecognizer
mkDescribeEntityRecognizer pEntityRecognizerARN_ =
  DescribeEntityRecognizer'
    { entityRecognizerARN =
        pEntityRecognizerARN_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEntityRecognizerARN :: Lens.Lens' DescribeEntityRecognizer Lude.Text
dEntityRecognizerARN = Lens.lens (entityRecognizerARN :: DescribeEntityRecognizer -> Lude.Text) (\s a -> s {entityRecognizerARN = a} :: DescribeEntityRecognizer)
{-# DEPRECATED dEntityRecognizerARN "Use generic-lens or generic-optics with 'entityRecognizerARN' instead." #-}

instance Lude.AWSRequest DescribeEntityRecognizer where
  type Rs DescribeEntityRecognizer = DescribeEntityRecognizerResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEntityRecognizerResponse'
            Lude.<$> (x Lude..?> "EntityRecognizerProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEntityRecognizer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DescribeEntityRecognizer" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEntityRecognizer where
  toJSON DescribeEntityRecognizer' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EntityRecognizerArn" Lude..= entityRecognizerARN)]
      )

instance Lude.ToPath DescribeEntityRecognizer where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEntityRecognizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEntityRecognizerResponse' smart constructor.
data DescribeEntityRecognizerResponse = DescribeEntityRecognizerResponse'
  { entityRecognizerProperties ::
      Lude.Maybe
        EntityRecognizerProperties,
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

-- | Creates a value of 'DescribeEntityRecognizerResponse' with the minimum fields required to make a request.
--
-- * 'entityRecognizerProperties' - Describes information associated with an entity recognizer.
-- * 'responseStatus' - The response status code.
mkDescribeEntityRecognizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEntityRecognizerResponse
mkDescribeEntityRecognizerResponse pResponseStatus_ =
  DescribeEntityRecognizerResponse'
    { entityRecognizerProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Describes information associated with an entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEntityRecognizerProperties :: Lens.Lens' DescribeEntityRecognizerResponse (Lude.Maybe EntityRecognizerProperties)
drsEntityRecognizerProperties = Lens.lens (entityRecognizerProperties :: DescribeEntityRecognizerResponse -> Lude.Maybe EntityRecognizerProperties) (\s a -> s {entityRecognizerProperties = a} :: DescribeEntityRecognizerResponse)
{-# DEPRECATED drsEntityRecognizerProperties "Use generic-lens or generic-optics with 'entityRecognizerProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeEntityRecognizerResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeEntityRecognizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEntityRecognizerResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
