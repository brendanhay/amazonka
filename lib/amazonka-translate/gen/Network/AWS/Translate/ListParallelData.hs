{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.ListParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of your parallel data resources in Amazon Translate.
module Network.AWS.Translate.ListParallelData
  ( -- * Creating a request
    ListParallelData (..),
    mkListParallelData,

    -- ** Request lenses
    lpdNextToken,
    lpdMaxResults,

    -- * Destructuring the response
    ListParallelDataResponse (..),
    mkListParallelDataResponse,

    -- ** Response lenses
    lpdrsParallelDataPropertiesList,
    lpdrsNextToken,
    lpdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkListParallelData' smart constructor.
data ListParallelData = ListParallelData'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListParallelData' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of parallel data resources returned for each request.
-- * 'nextToken' - A string that specifies the next page of results to return in a paginated response.
mkListParallelData ::
  ListParallelData
mkListParallelData =
  ListParallelData'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A string that specifies the next page of results to return in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdNextToken :: Lens.Lens' ListParallelData (Lude.Maybe Lude.Text)
lpdNextToken = Lens.lens (nextToken :: ListParallelData -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListParallelData)
{-# DEPRECATED lpdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of parallel data resources returned for each request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdMaxResults :: Lens.Lens' ListParallelData (Lude.Maybe Lude.Natural)
lpdMaxResults = Lens.lens (maxResults :: ListParallelData -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListParallelData)
{-# DEPRECATED lpdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListParallelData where
  type Rs ListParallelData = ListParallelDataResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListParallelDataResponse'
            Lude.<$> (x Lude..?> "ParallelDataPropertiesList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListParallelData where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.ListParallelData" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListParallelData where
  toJSON ListParallelData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListParallelData where
  toPath = Lude.const "/"

instance Lude.ToQuery ListParallelData where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListParallelDataResponse' smart constructor.
data ListParallelDataResponse = ListParallelDataResponse'
  { parallelDataPropertiesList ::
      Lude.Maybe [ParallelDataProperties],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListParallelDataResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
-- * 'parallelDataPropertiesList' - The properties of the parallel data resources returned by this request.
-- * 'responseStatus' - The response status code.
mkListParallelDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListParallelDataResponse
mkListParallelDataResponse pResponseStatus_ =
  ListParallelDataResponse'
    { parallelDataPropertiesList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The properties of the parallel data resources returned by this request.
--
-- /Note:/ Consider using 'parallelDataPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrsParallelDataPropertiesList :: Lens.Lens' ListParallelDataResponse (Lude.Maybe [ParallelDataProperties])
lpdrsParallelDataPropertiesList = Lens.lens (parallelDataPropertiesList :: ListParallelDataResponse -> Lude.Maybe [ParallelDataProperties]) (\s a -> s {parallelDataPropertiesList = a} :: ListParallelDataResponse)
{-# DEPRECATED lpdrsParallelDataPropertiesList "Use generic-lens or generic-optics with 'parallelDataPropertiesList' instead." #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrsNextToken :: Lens.Lens' ListParallelDataResponse (Lude.Maybe Lude.Text)
lpdrsNextToken = Lens.lens (nextToken :: ListParallelDataResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListParallelDataResponse)
{-# DEPRECATED lpdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrsResponseStatus :: Lens.Lens' ListParallelDataResponse Lude.Int
lpdrsResponseStatus = Lens.lens (responseStatus :: ListParallelDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListParallelDataResponse)
{-# DEPRECATED lpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
