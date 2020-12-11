{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListActionTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all AWS CodePipeline action types associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListActionTypes
  ( -- * Creating a request
    ListActionTypes (..),
    mkListActionTypes,

    -- ** Request lenses
    latActionOwnerFilter,
    latNextToken,

    -- * Destructuring the response
    ListActionTypesResponse (..),
    mkListActionTypesResponse,

    -- ** Response lenses
    latrsNextToken,
    latrsResponseStatus,
    latrsActionTypes,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListActionTypes@ action.
--
-- /See:/ 'mkListActionTypes' smart constructor.
data ListActionTypes = ListActionTypes'
  { actionOwnerFilter ::
      Lude.Maybe ActionOwner,
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActionTypes' with the minimum fields required to make a request.
--
-- * 'actionOwnerFilter' - Filters the list of action types to those created by a specified entity.
-- * 'nextToken' - An identifier that was returned from the previous list action types call, which can be used to return the next set of action types in the list.
mkListActionTypes ::
  ListActionTypes
mkListActionTypes =
  ListActionTypes'
    { actionOwnerFilter = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | Filters the list of action types to those created by a specified entity.
--
-- /Note:/ Consider using 'actionOwnerFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latActionOwnerFilter :: Lens.Lens' ListActionTypes (Lude.Maybe ActionOwner)
latActionOwnerFilter = Lens.lens (actionOwnerFilter :: ListActionTypes -> Lude.Maybe ActionOwner) (\s a -> s {actionOwnerFilter = a} :: ListActionTypes)
{-# DEPRECATED latActionOwnerFilter "Use generic-lens or generic-optics with 'actionOwnerFilter' instead." #-}

-- | An identifier that was returned from the previous list action types call, which can be used to return the next set of action types in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latNextToken :: Lens.Lens' ListActionTypes (Lude.Maybe Lude.Text)
latNextToken = Lens.lens (nextToken :: ListActionTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListActionTypes)
{-# DEPRECATED latNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListActionTypes where
  page rq rs
    | Page.stop (rs Lens.^. latrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. latrsActionTypes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& latNextToken Lens..~ rs Lens.^. latrsNextToken

instance Lude.AWSRequest ListActionTypes where
  type Rs ListActionTypes = ListActionTypesResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListActionTypesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "actionTypes" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListActionTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.ListActionTypes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListActionTypes where
  toJSON ListActionTypes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("actionOwnerFilter" Lude..=) Lude.<$> actionOwnerFilter,
            ("nextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath ListActionTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery ListActionTypes where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @ListActionTypes@ action.
--
-- /See:/ 'mkListActionTypesResponse' smart constructor.
data ListActionTypesResponse = ListActionTypesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    actionTypes :: [ActionType]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActionTypesResponse' with the minimum fields required to make a request.
--
-- * 'actionTypes' - Provides details of the action types.
-- * 'nextToken' - If the amount of returned information is significantly large, an identifier is also returned. It can be used in a subsequent list action types call to return the next set of action types in the list.
-- * 'responseStatus' - The response status code.
mkListActionTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListActionTypesResponse
mkListActionTypesResponse pResponseStatus_ =
  ListActionTypesResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      actionTypes = Lude.mempty
    }

-- | If the amount of returned information is significantly large, an identifier is also returned. It can be used in a subsequent list action types call to return the next set of action types in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsNextToken :: Lens.Lens' ListActionTypesResponse (Lude.Maybe Lude.Text)
latrsNextToken = Lens.lens (nextToken :: ListActionTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListActionTypesResponse)
{-# DEPRECATED latrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsResponseStatus :: Lens.Lens' ListActionTypesResponse Lude.Int
latrsResponseStatus = Lens.lens (responseStatus :: ListActionTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListActionTypesResponse)
{-# DEPRECATED latrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Provides details of the action types.
--
-- /Note:/ Consider using 'actionTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsActionTypes :: Lens.Lens' ListActionTypesResponse [ActionType]
latrsActionTypes = Lens.lens (actionTypes :: ListActionTypesResponse -> [ActionType]) (\s a -> s {actionTypes = a} :: ListActionTypesResponse)
{-# DEPRECATED latrsActionTypes "Use generic-lens or generic-optics with 'actionTypes' instead." #-}
