{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.ListEnvironments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of AWS Cloud9 development environment identifiers.
--
-- This operation returns paginated results.
module Network.AWS.Cloud9.ListEnvironments
  ( -- * Creating a request
    ListEnvironments (..),
    mkListEnvironments,

    -- ** Request lenses
    leNextToken,
    leMaxResults,

    -- * Destructuring the response
    ListEnvironmentsResponse (..),
    mkListEnvironmentsResponse,

    -- ** Response lenses
    lersEnvironmentIds,
    lersNextToken,
    lersResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListEnvironments' smart constructor.
data ListEnvironments = ListEnvironments'
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

-- | Creates a value of 'ListEnvironments' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of environments to get identifiers for.
-- * 'nextToken' - During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
mkListEnvironments ::
  ListEnvironments
mkListEnvironments =
  ListEnvironments'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListEnvironments (Lude.Maybe Lude.Text)
leNextToken = Lens.lens (nextToken :: ListEnvironments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEnvironments)
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of environments to get identifiers for.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListEnvironments (Lude.Maybe Lude.Natural)
leMaxResults = Lens.lens (maxResults :: ListEnvironments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListEnvironments)
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListEnvironments where
  page rq rs
    | Page.stop (rs Lens.^. lersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lersEnvironmentIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& leNextToken Lens..~ rs Lens.^. lersNextToken

instance Lude.AWSRequest ListEnvironments where
  type Rs ListEnvironments = ListEnvironmentsResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEnvironmentsResponse'
            Lude.<$> (x Lude..?> "environmentIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListEnvironments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.ListEnvironments" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListEnvironments where
  toJSON ListEnvironments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListEnvironments where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEnvironments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListEnvironmentsResponse' smart constructor.
data ListEnvironmentsResponse = ListEnvironmentsResponse'
  { environmentIds ::
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

-- | Creates a value of 'ListEnvironmentsResponse' with the minimum fields required to make a request.
--
-- * 'environmentIds' - The list of environment identifiers.
-- * 'nextToken' - If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
-- * 'responseStatus' - The response status code.
mkListEnvironmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEnvironmentsResponse
mkListEnvironmentsResponse pResponseStatus_ =
  ListEnvironmentsResponse'
    { environmentIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of environment identifiers.
--
-- /Note:/ Consider using 'environmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersEnvironmentIds :: Lens.Lens' ListEnvironmentsResponse (Lude.Maybe [Lude.Text])
lersEnvironmentIds = Lens.lens (environmentIds :: ListEnvironmentsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {environmentIds = a} :: ListEnvironmentsResponse)
{-# DEPRECATED lersEnvironmentIds "Use generic-lens or generic-optics with 'environmentIds' instead." #-}

-- | If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersNextToken :: Lens.Lens' ListEnvironmentsResponse (Lude.Maybe Lude.Text)
lersNextToken = Lens.lens (nextToken :: ListEnvironmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEnvironmentsResponse)
{-# DEPRECATED lersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersResponseStatus :: Lens.Lens' ListEnvironmentsResponse Lude.Int
lersResponseStatus = Lens.lens (responseStatus :: ListEnvironmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEnvironmentsResponse)
{-# DEPRECATED lersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
