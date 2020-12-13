{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of stored connections to GitHub accounts.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
  ( -- * Creating a request
    ListGitHubAccountTokenNames (..),
    mkListGitHubAccountTokenNames,

    -- ** Request lenses
    lghatnNextToken,

    -- * Destructuring the response
    ListGitHubAccountTokenNamesResponse (..),
    mkListGitHubAccountTokenNamesResponse,

    -- ** Response lenses
    lghatnrsTokenNameList,
    lghatnrsNextToken,
    lghatnrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListGitHubAccountTokenNames@ operation.
--
-- /See:/ 'mkListGitHubAccountTokenNames' smart constructor.
newtype ListGitHubAccountTokenNames = ListGitHubAccountTokenNames'
  { -- | An identifier returned from the previous @ListGitHubAccountTokenNames@ call. It can be used to return the next set of names in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGitHubAccountTokenNames' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier returned from the previous @ListGitHubAccountTokenNames@ call. It can be used to return the next set of names in the list.
mkListGitHubAccountTokenNames ::
  ListGitHubAccountTokenNames
mkListGitHubAccountTokenNames =
  ListGitHubAccountTokenNames' {nextToken = Lude.Nothing}

-- | An identifier returned from the previous @ListGitHubAccountTokenNames@ call. It can be used to return the next set of names in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lghatnNextToken :: Lens.Lens' ListGitHubAccountTokenNames (Lude.Maybe Lude.Text)
lghatnNextToken = Lens.lens (nextToken :: ListGitHubAccountTokenNames -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGitHubAccountTokenNames)
{-# DEPRECATED lghatnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListGitHubAccountTokenNames where
  page rq rs
    | Page.stop (rs Lens.^. lghatnrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lghatnrsTokenNameList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lghatnNextToken Lens..~ rs Lens.^. lghatnrsNextToken

instance Lude.AWSRequest ListGitHubAccountTokenNames where
  type
    Rs ListGitHubAccountTokenNames =
      ListGitHubAccountTokenNamesResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGitHubAccountTokenNamesResponse'
            Lude.<$> (x Lude..?> "tokenNameList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGitHubAccountTokenNames where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.ListGitHubAccountTokenNames" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGitHubAccountTokenNames where
  toJSON ListGitHubAccountTokenNames' {..} =
    Lude.object
      (Lude.catMaybes [("nextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath ListGitHubAccountTokenNames where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGitHubAccountTokenNames where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @ListGitHubAccountTokenNames@ operation.
--
-- /See:/ 'mkListGitHubAccountTokenNamesResponse' smart constructor.
data ListGitHubAccountTokenNamesResponse = ListGitHubAccountTokenNamesResponse'
  { -- | A list of names of connections to GitHub accounts.
    tokenNameList :: Lude.Maybe [Lude.Text],
    -- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent @ListGitHubAccountTokenNames@ call to return the next set of names in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGitHubAccountTokenNamesResponse' with the minimum fields required to make a request.
--
-- * 'tokenNameList' - A list of names of connections to GitHub accounts.
-- * 'nextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent @ListGitHubAccountTokenNames@ call to return the next set of names in the list.
-- * 'responseStatus' - The response status code.
mkListGitHubAccountTokenNamesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGitHubAccountTokenNamesResponse
mkListGitHubAccountTokenNamesResponse pResponseStatus_ =
  ListGitHubAccountTokenNamesResponse'
    { tokenNameList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of names of connections to GitHub accounts.
--
-- /Note:/ Consider using 'tokenNameList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lghatnrsTokenNameList :: Lens.Lens' ListGitHubAccountTokenNamesResponse (Lude.Maybe [Lude.Text])
lghatnrsTokenNameList = Lens.lens (tokenNameList :: ListGitHubAccountTokenNamesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {tokenNameList = a} :: ListGitHubAccountTokenNamesResponse)
{-# DEPRECATED lghatnrsTokenNameList "Use generic-lens or generic-optics with 'tokenNameList' instead." #-}

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent @ListGitHubAccountTokenNames@ call to return the next set of names in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lghatnrsNextToken :: Lens.Lens' ListGitHubAccountTokenNamesResponse (Lude.Maybe Lude.Text)
lghatnrsNextToken = Lens.lens (nextToken :: ListGitHubAccountTokenNamesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGitHubAccountTokenNamesResponse)
{-# DEPRECATED lghatnrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lghatnrsResponseStatus :: Lens.Lens' ListGitHubAccountTokenNamesResponse Lude.Int
lghatnrsResponseStatus = Lens.lens (responseStatus :: ListGitHubAccountTokenNamesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGitHubAccountTokenNamesResponse)
{-# DEPRECATED lghatnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
