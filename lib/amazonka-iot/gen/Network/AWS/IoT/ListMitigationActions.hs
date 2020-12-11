{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListMitigationActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all mitigation actions that match the specified filter criteria.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListMitigationActions
  ( -- * Creating a request
    ListMitigationActions (..),
    mkListMitigationActions,

    -- ** Request lenses
    lmaNextToken,
    lmaActionType,
    lmaMaxResults,

    -- * Destructuring the response
    ListMitigationActionsResponse (..),
    mkListMitigationActionsResponse,

    -- ** Response lenses
    lmarsActionIdentifiers,
    lmarsNextToken,
    lmarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListMitigationActions' smart constructor.
data ListMitigationActions = ListMitigationActions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    actionType :: Lude.Maybe MitigationActionType,
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

-- | Creates a value of 'ListMitigationActions' with the minimum fields required to make a request.
--
-- * 'actionType' - Specify a value to limit the result to mitigation actions with a specific action type.
-- * 'maxResults' - The maximum number of results to return at one time. The default is 25.
-- * 'nextToken' - The token for the next set of results.
mkListMitigationActions ::
  ListMitigationActions
mkListMitigationActions =
  ListMitigationActions'
    { nextToken = Lude.Nothing,
      actionType = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmaNextToken :: Lens.Lens' ListMitigationActions (Lude.Maybe Lude.Text)
lmaNextToken = Lens.lens (nextToken :: ListMitigationActions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMitigationActions)
{-# DEPRECATED lmaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify a value to limit the result to mitigation actions with a specific action type.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmaActionType :: Lens.Lens' ListMitigationActions (Lude.Maybe MitigationActionType)
lmaActionType = Lens.lens (actionType :: ListMitigationActions -> Lude.Maybe MitigationActionType) (\s a -> s {actionType = a} :: ListMitigationActions)
{-# DEPRECATED lmaActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmaMaxResults :: Lens.Lens' ListMitigationActions (Lude.Maybe Lude.Natural)
lmaMaxResults = Lens.lens (maxResults :: ListMitigationActions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMitigationActions)
{-# DEPRECATED lmaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListMitigationActions where
  page rq rs
    | Page.stop (rs Lens.^. lmarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmarsActionIdentifiers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmaNextToken Lens..~ rs Lens.^. lmarsNextToken

instance Lude.AWSRequest ListMitigationActions where
  type Rs ListMitigationActions = ListMitigationActionsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMitigationActionsResponse'
            Lude.<$> (x Lude..?> "actionIdentifiers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMitigationActions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListMitigationActions where
  toPath = Lude.const "/mitigationactions/actions"

instance Lude.ToQuery ListMitigationActions where
  toQuery ListMitigationActions' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "actionType" Lude.=: actionType,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListMitigationActionsResponse' smart constructor.
data ListMitigationActionsResponse = ListMitigationActionsResponse'
  { actionIdentifiers ::
      Lude.Maybe
        [MitigationActionIdentifier],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListMitigationActionsResponse' with the minimum fields required to make a request.
--
-- * 'actionIdentifiers' - A set of actions that matched the specified filter criteria.
-- * 'nextToken' - The token for the next set of results.
-- * 'responseStatus' - The response status code.
mkListMitigationActionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMitigationActionsResponse
mkListMitigationActionsResponse pResponseStatus_ =
  ListMitigationActionsResponse'
    { actionIdentifiers = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A set of actions that matched the specified filter criteria.
--
-- /Note:/ Consider using 'actionIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmarsActionIdentifiers :: Lens.Lens' ListMitigationActionsResponse (Lude.Maybe [MitigationActionIdentifier])
lmarsActionIdentifiers = Lens.lens (actionIdentifiers :: ListMitigationActionsResponse -> Lude.Maybe [MitigationActionIdentifier]) (\s a -> s {actionIdentifiers = a} :: ListMitigationActionsResponse)
{-# DEPRECATED lmarsActionIdentifiers "Use generic-lens or generic-optics with 'actionIdentifiers' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmarsNextToken :: Lens.Lens' ListMitigationActionsResponse (Lude.Maybe Lude.Text)
lmarsNextToken = Lens.lens (nextToken :: ListMitigationActionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMitigationActionsResponse)
{-# DEPRECATED lmarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmarsResponseStatus :: Lens.Lens' ListMitigationActionsResponse Lude.Int
lmarsResponseStatus = Lens.lens (responseStatus :: ListMitigationActionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMitigationActionsResponse)
{-# DEPRECATED lmarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
