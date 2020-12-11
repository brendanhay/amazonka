{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List multiple functions.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListFunctions
  ( -- * Creating a request
    ListFunctions (..),
    mkListFunctions,

    -- ** Request lenses
    lfNextToken,
    lfMaxResults,
    lfApiId,

    -- * Destructuring the response
    ListFunctionsResponse (..),
    mkListFunctionsResponse,

    -- ** Response lenses
    lfrsNextToken,
    lfrsFunctions,
    lfrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFunctions' smart constructor.
data ListFunctions = ListFunctions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    apiId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFunctions' with the minimum fields required to make a request.
--
-- * 'apiId' - The GraphQL API ID.
-- * 'maxResults' - The maximum number of results you want the request to return.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListFunctions ::
  -- | 'apiId'
  Lude.Text ->
  ListFunctions
mkListFunctions pApiId_ =
  ListFunctions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      apiId = pApiId_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFunctions (Lude.Maybe Lude.Text)
lfNextToken = Lens.lens (nextToken :: ListFunctions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFunctions)
{-# DEPRECATED lfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxResults :: Lens.Lens' ListFunctions (Lude.Maybe Lude.Natural)
lfMaxResults = Lens.lens (maxResults :: ListFunctions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListFunctions)
{-# DEPRECATED lfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfApiId :: Lens.Lens' ListFunctions Lude.Text
lfApiId = Lens.lens (apiId :: ListFunctions -> Lude.Text) (\s a -> s {apiId = a} :: ListFunctions)
{-# DEPRECATED lfApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Page.AWSPager ListFunctions where
  page rq rs
    | Page.stop (rs Lens.^. lfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lfrsFunctions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfNextToken Lens..~ rs Lens.^. lfrsNextToken

instance Lude.AWSRequest ListFunctions where
  type Rs ListFunctions = ListFunctionsResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFunctionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "functions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFunctions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListFunctions where
  toPath ListFunctions' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/functions"]

instance Lude.ToQuery ListFunctions where
  toQuery ListFunctions' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListFunctionsResponse' smart constructor.
data ListFunctionsResponse = ListFunctionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    functions :: Lude.Maybe [FunctionConfiguration],
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

-- | Creates a value of 'ListFunctionsResponse' with the minimum fields required to make a request.
--
-- * 'functions' - A list of @Function@ objects.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkListFunctionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFunctionsResponse
mkListFunctionsResponse pResponseStatus_ =
  ListFunctionsResponse'
    { nextToken = Lude.Nothing,
      functions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsNextToken :: Lens.Lens' ListFunctionsResponse (Lude.Maybe Lude.Text)
lfrsNextToken = Lens.lens (nextToken :: ListFunctionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFunctionsResponse)
{-# DEPRECATED lfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @Function@ objects.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsFunctions :: Lens.Lens' ListFunctionsResponse (Lude.Maybe [FunctionConfiguration])
lfrsFunctions = Lens.lens (functions :: ListFunctionsResponse -> Lude.Maybe [FunctionConfiguration]) (\s a -> s {functions = a} :: ListFunctionsResponse)
{-# DEPRECATED lfrsFunctions "Use generic-lens or generic-optics with 'functions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsResponseStatus :: Lens.Lens' ListFunctionsResponse Lude.Int
lfrsResponseStatus = Lens.lens (responseStatus :: ListFunctionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFunctionsResponse)
{-# DEPRECATED lfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
