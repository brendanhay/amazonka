{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListTestGridSessionArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of artifacts created during the session.
module Network.AWS.DeviceFarm.ListTestGridSessionArtifacts
  ( -- * Creating a request
    ListTestGridSessionArtifacts (..),
    mkListTestGridSessionArtifacts,

    -- ** Request lenses
    lMaxResult,
    lNextToken,
    lType,
    lSessionARN,

    -- * Destructuring the response
    ListTestGridSessionArtifactsResponse (..),
    mkListTestGridSessionArtifactsResponse,

    -- ** Response lenses
    lrsArtifacts,
    lrsNextToken,
    lrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTestGridSessionArtifacts' smart constructor.
data ListTestGridSessionArtifacts = ListTestGridSessionArtifacts'
  { maxResult ::
      Lude.Maybe Lude.Natural,
    nextToken :: Lude.Maybe Lude.Text,
    type' ::
      Lude.Maybe
        TestGridSessionArtifactCategory,
    sessionARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTestGridSessionArtifacts' with the minimum fields required to make a request.
--
-- * 'maxResult' - The maximum number of results to be returned by a request.
-- * 'nextToken' - Pagination token.
-- * 'sessionARN' - The ARN of a 'TestGridSession' .
-- * 'type'' - Limit results to a specified type of artifact.
mkListTestGridSessionArtifacts ::
  -- | 'sessionARN'
  Lude.Text ->
  ListTestGridSessionArtifacts
mkListTestGridSessionArtifacts pSessionARN_ =
  ListTestGridSessionArtifacts'
    { maxResult = Lude.Nothing,
      nextToken = Lude.Nothing,
      type' = Lude.Nothing,
      sessionARN = pSessionARN_
    }

-- | The maximum number of results to be returned by a request.
--
-- /Note:/ Consider using 'maxResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResult :: Lens.Lens' ListTestGridSessionArtifacts (Lude.Maybe Lude.Natural)
lMaxResult = Lens.lens (maxResult :: ListTestGridSessionArtifacts -> Lude.Maybe Lude.Natural) (\s a -> s {maxResult = a} :: ListTestGridSessionArtifacts)
{-# DEPRECATED lMaxResult "Use generic-lens or generic-optics with 'maxResult' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListTestGridSessionArtifacts (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListTestGridSessionArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTestGridSessionArtifacts)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Limit results to a specified type of artifact.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lType :: Lens.Lens' ListTestGridSessionArtifacts (Lude.Maybe TestGridSessionArtifactCategory)
lType = Lens.lens (type' :: ListTestGridSessionArtifacts -> Lude.Maybe TestGridSessionArtifactCategory) (\s a -> s {type' = a} :: ListTestGridSessionArtifacts)
{-# DEPRECATED lType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The ARN of a 'TestGridSession' .
--
-- /Note:/ Consider using 'sessionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSessionARN :: Lens.Lens' ListTestGridSessionArtifacts Lude.Text
lSessionARN = Lens.lens (sessionARN :: ListTestGridSessionArtifacts -> Lude.Text) (\s a -> s {sessionARN = a} :: ListTestGridSessionArtifacts)
{-# DEPRECATED lSessionARN "Use generic-lens or generic-optics with 'sessionARN' instead." #-}

instance Lude.AWSRequest ListTestGridSessionArtifacts where
  type
    Rs ListTestGridSessionArtifacts =
      ListTestGridSessionArtifactsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTestGridSessionArtifactsResponse'
            Lude.<$> (x Lude..?> "artifacts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTestGridSessionArtifacts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DeviceFarm_20150623.ListTestGridSessionArtifacts" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTestGridSessionArtifacts where
  toJSON ListTestGridSessionArtifacts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("maxResult" Lude..=) Lude.<$> maxResult,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("type" Lude..=) Lude.<$> type',
            Lude.Just ("sessionArn" Lude..= sessionARN)
          ]
      )

instance Lude.ToPath ListTestGridSessionArtifacts where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTestGridSessionArtifacts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTestGridSessionArtifactsResponse' smart constructor.
data ListTestGridSessionArtifactsResponse = ListTestGridSessionArtifactsResponse'
  { artifacts ::
      Lude.Maybe
        [TestGridSessionArtifact],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'ListTestGridSessionArtifactsResponse' with the minimum fields required to make a request.
--
-- * 'artifacts' - A list of test grid session artifacts for a 'TestGridSession' .
-- * 'nextToken' - Pagination token.
-- * 'responseStatus' - The response status code.
mkListTestGridSessionArtifactsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTestGridSessionArtifactsResponse
mkListTestGridSessionArtifactsResponse pResponseStatus_ =
  ListTestGridSessionArtifactsResponse'
    { artifacts = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of test grid session artifacts for a 'TestGridSession' .
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsArtifacts :: Lens.Lens' ListTestGridSessionArtifactsResponse (Lude.Maybe [TestGridSessionArtifact])
lrsArtifacts = Lens.lens (artifacts :: ListTestGridSessionArtifactsResponse -> Lude.Maybe [TestGridSessionArtifact]) (\s a -> s {artifacts = a} :: ListTestGridSessionArtifactsResponse)
{-# DEPRECATED lrsArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListTestGridSessionArtifactsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListTestGridSessionArtifactsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTestGridSessionArtifactsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListTestGridSessionArtifactsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListTestGridSessionArtifactsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTestGridSessionArtifactsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
