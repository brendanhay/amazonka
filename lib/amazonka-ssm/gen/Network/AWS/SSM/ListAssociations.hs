{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all State Manager associations in the current AWS account and Region. You can limit the results to a specific State Manager association document or instance by specifying a filter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListAssociations
  ( -- * Creating a request
    ListAssociations (..),
    mkListAssociations,

    -- ** Request lenses
    laAssociationFilterList,
    laNextToken,
    laMaxResults,

    -- * Destructuring the response
    ListAssociationsResponse (..),
    mkListAssociationsResponse,

    -- ** Response lenses
    larsNextToken,
    larsAssociations,
    larsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListAssociations' smart constructor.
data ListAssociations = ListAssociations'
  { associationFilterList ::
      Lude.Maybe (Lude.NonEmpty AssociationFilter),
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListAssociations' with the minimum fields required to make a request.
--
-- * 'associationFilterList' - One or more filters. Use a filter to return a more specific list of results.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkListAssociations ::
  ListAssociations
mkListAssociations =
  ListAssociations'
    { associationFilterList = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'associationFilterList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAssociationFilterList :: Lens.Lens' ListAssociations (Lude.Maybe (Lude.NonEmpty AssociationFilter))
laAssociationFilterList = Lens.lens (associationFilterList :: ListAssociations -> Lude.Maybe (Lude.NonEmpty AssociationFilter)) (\s a -> s {associationFilterList = a} :: ListAssociations)
{-# DEPRECATED laAssociationFilterList "Use generic-lens or generic-optics with 'associationFilterList' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListAssociations (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssociations)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListAssociations (Lude.Maybe Lude.Natural)
laMaxResults = Lens.lens (maxResults :: ListAssociations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAssociations)
{-# DEPRECATED laMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAssociations where
  page rq rs
    | Page.stop (rs Lens.^. larsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larsAssociations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. larsNextToken

instance Lude.AWSRequest ListAssociations where
  type Rs ListAssociations = ListAssociationsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAssociationsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Associations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAssociations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListAssociations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAssociations where
  toJSON ListAssociations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AssociationFilterList" Lude..=) Lude.<$> associationFilterList,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListAssociations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAssociations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAssociationsResponse' smart constructor.
data ListAssociationsResponse = ListAssociationsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    associations :: Lude.Maybe [Association],
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

-- | Creates a value of 'ListAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'associations' - The associations.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkListAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAssociationsResponse
mkListAssociationsResponse pResponseStatus_ =
  ListAssociationsResponse'
    { nextToken = Lude.Nothing,
      associations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListAssociationsResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssociationsResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The associations.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsAssociations :: Lens.Lens' ListAssociationsResponse (Lude.Maybe [Association])
larsAssociations = Lens.lens (associations :: ListAssociationsResponse -> Lude.Maybe [Association]) (\s a -> s {associations = a} :: ListAssociationsResponse)
{-# DEPRECATED larsAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListAssociationsResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAssociationsResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
