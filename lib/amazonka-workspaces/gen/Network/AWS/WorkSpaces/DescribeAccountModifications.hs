{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeAccountModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes modifications to the configuration of Bring Your Own License (BYOL) for the specified account.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeAccountModifications
  ( -- * Creating a request
    DescribeAccountModifications (..),
    mkDescribeAccountModifications,

    -- ** Request lenses
    damNextToken,

    -- * Destructuring the response
    DescribeAccountModificationsResponse (..),
    mkDescribeAccountModificationsResponse,

    -- ** Response lenses
    damrsAccountModifications,
    damrsNextToken,
    damrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeAccountModifications' smart constructor.
newtype DescribeAccountModifications = DescribeAccountModifications'
  { nextToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountModifications' with the minimum fields required to make a request.
--
-- * 'nextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
mkDescribeAccountModifications ::
  DescribeAccountModifications
mkDescribeAccountModifications =
  DescribeAccountModifications' {nextToken = Lude.Nothing}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damNextToken :: Lens.Lens' DescribeAccountModifications (Lude.Maybe Lude.Text)
damNextToken = Lens.lens (nextToken :: DescribeAccountModifications -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAccountModifications)
{-# DEPRECATED damNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager DescribeAccountModifications where
  page rq rs
    | Page.stop (rs Lens.^. damrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. damrsAccountModifications) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& damNextToken Lens..~ rs Lens.^. damrsNextToken

instance Lude.AWSRequest DescribeAccountModifications where
  type
    Rs DescribeAccountModifications =
      DescribeAccountModificationsResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAccountModificationsResponse'
            Lude.<$> (x Lude..?> "AccountModifications" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountModifications where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.DescribeAccountModifications" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAccountModifications where
  toJSON DescribeAccountModifications' {..} =
    Lude.object
      (Lude.catMaybes [("NextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath DescribeAccountModifications where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccountModifications where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAccountModificationsResponse' smart constructor.
data DescribeAccountModificationsResponse = DescribeAccountModificationsResponse'
  { accountModifications ::
      Lude.Maybe
        [AccountModification],
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

-- | Creates a value of 'DescribeAccountModificationsResponse' with the minimum fields required to make a request.
--
-- * 'accountModifications' - The list of modifications to the configuration of BYOL.
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
-- * 'responseStatus' - The response status code.
mkDescribeAccountModificationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountModificationsResponse
mkDescribeAccountModificationsResponse pResponseStatus_ =
  DescribeAccountModificationsResponse'
    { accountModifications =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of modifications to the configuration of BYOL.
--
-- /Note:/ Consider using 'accountModifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrsAccountModifications :: Lens.Lens' DescribeAccountModificationsResponse (Lude.Maybe [AccountModification])
damrsAccountModifications = Lens.lens (accountModifications :: DescribeAccountModificationsResponse -> Lude.Maybe [AccountModification]) (\s a -> s {accountModifications = a} :: DescribeAccountModificationsResponse)
{-# DEPRECATED damrsAccountModifications "Use generic-lens or generic-optics with 'accountModifications' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrsNextToken :: Lens.Lens' DescribeAccountModificationsResponse (Lude.Maybe Lude.Text)
damrsNextToken = Lens.lens (nextToken :: DescribeAccountModificationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAccountModificationsResponse)
{-# DEPRECATED damrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrsResponseStatus :: Lens.Lens' DescribeAccountModificationsResponse Lude.Int
damrsResponseStatus = Lens.lens (responseStatus :: DescribeAccountModificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountModificationsResponse)
{-# DEPRECATED damrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
