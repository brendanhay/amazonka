{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListLambdaFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the Lambda functions that show up in the drop-down options in the relevant contact flow blocks.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListLambdaFunctions
  ( -- * Creating a request
    ListLambdaFunctions (..),
    mkListLambdaFunctions,

    -- ** Request lenses
    llfNextToken,
    llfMaxResults,
    llfInstanceId,

    -- * Destructuring the response
    ListLambdaFunctionsResponse (..),
    mkListLambdaFunctionsResponse,

    -- ** Response lenses
    llfrsLambdaFunctions,
    llfrsNextToken,
    llfrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListLambdaFunctions' smart constructor.
data ListLambdaFunctions = ListLambdaFunctions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLambdaFunctions' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
mkListLambdaFunctions ::
  -- | 'instanceId'
  Lude.Text ->
  ListLambdaFunctions
mkListLambdaFunctions pInstanceId_ =
  ListLambdaFunctions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfNextToken :: Lens.Lens' ListLambdaFunctions (Lude.Maybe Lude.Text)
llfNextToken = Lens.lens (nextToken :: ListLambdaFunctions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLambdaFunctions)
{-# DEPRECATED llfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfMaxResults :: Lens.Lens' ListLambdaFunctions (Lude.Maybe Lude.Natural)
llfMaxResults = Lens.lens (maxResults :: ListLambdaFunctions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListLambdaFunctions)
{-# DEPRECATED llfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfInstanceId :: Lens.Lens' ListLambdaFunctions Lude.Text
llfInstanceId = Lens.lens (instanceId :: ListLambdaFunctions -> Lude.Text) (\s a -> s {instanceId = a} :: ListLambdaFunctions)
{-# DEPRECATED llfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Page.AWSPager ListLambdaFunctions where
  page rq rs
    | Page.stop (rs Lens.^. llfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. llfrsLambdaFunctions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& llfNextToken Lens..~ rs Lens.^. llfrsNextToken

instance Lude.AWSRequest ListLambdaFunctions where
  type Rs ListLambdaFunctions = ListLambdaFunctionsResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLambdaFunctionsResponse'
            Lude.<$> (x Lude..?> "LambdaFunctions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLambdaFunctions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListLambdaFunctions where
  toPath ListLambdaFunctions' {..} =
    Lude.mconcat
      ["/instance/", Lude.toBS instanceId, "/lambda-functions"]

instance Lude.ToQuery ListLambdaFunctions where
  toQuery ListLambdaFunctions' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListLambdaFunctionsResponse' smart constructor.
data ListLambdaFunctionsResponse = ListLambdaFunctionsResponse'
  { lambdaFunctions ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListLambdaFunctionsResponse' with the minimum fields required to make a request.
--
-- * 'lambdaFunctions' - The Lambdafunction ARNs associated with the specified instance.
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
mkListLambdaFunctionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLambdaFunctionsResponse
mkListLambdaFunctionsResponse pResponseStatus_ =
  ListLambdaFunctionsResponse'
    { lambdaFunctions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Lambdafunction ARNs associated with the specified instance.
--
-- /Note:/ Consider using 'lambdaFunctions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfrsLambdaFunctions :: Lens.Lens' ListLambdaFunctionsResponse (Lude.Maybe [Lude.Text])
llfrsLambdaFunctions = Lens.lens (lambdaFunctions :: ListLambdaFunctionsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {lambdaFunctions = a} :: ListLambdaFunctionsResponse)
{-# DEPRECATED llfrsLambdaFunctions "Use generic-lens or generic-optics with 'lambdaFunctions' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfrsNextToken :: Lens.Lens' ListLambdaFunctionsResponse (Lude.Maybe Lude.Text)
llfrsNextToken = Lens.lens (nextToken :: ListLambdaFunctionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLambdaFunctionsResponse)
{-# DEPRECATED llfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llfrsResponseStatus :: Lens.Lens' ListLambdaFunctionsResponse Lude.Int
llfrsResponseStatus = Lens.lens (responseStatus :: ListLambdaFunctionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLambdaFunctionsResponse)
{-# DEPRECATED llfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
