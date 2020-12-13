{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListQualificationTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListQualificationTypes@ operation returns a list of Qualification types, filtered by an optional search term.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListQualificationTypes
  ( -- * Creating a request
    ListQualificationTypes (..),
    mkListQualificationTypes,

    -- ** Request lenses
    lqtMustBeOwnedByCaller,
    lqtNextToken,
    lqtQuery,
    lqtMustBeRequestable,
    lqtMaxResults,

    -- * Destructuring the response
    ListQualificationTypesResponse (..),
    mkListQualificationTypesResponse,

    -- ** Response lenses
    lqtrsQualificationTypes,
    lqtrsNextToken,
    lqtrsNumResults,
    lqtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListQualificationTypes' smart constructor.
data ListQualificationTypes = ListQualificationTypes'
  { -- | Specifies that only Qualification types that the Requester created are returned. If false, the operation returns all Qualification types.
    mustBeOwnedByCaller :: Lude.Maybe Lude.Bool,
    nextToken :: Lude.Maybe Lude.Text,
    -- | A text query against all of the searchable attributes of Qualification types.
    query :: Lude.Maybe Lude.Text,
    -- | Specifies that only Qualification types that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test, are returned as results of the search. Some Qualification types, such as those assigned automatically by the system, cannot be requested directly by users. If false, all Qualification types, including those managed by the system, are considered. Valid values are True | False.
    mustBeRequestable :: Lude.Bool,
    -- | The maximum number of results to return in a single call.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQualificationTypes' with the minimum fields required to make a request.
--
-- * 'mustBeOwnedByCaller' - Specifies that only Qualification types that the Requester created are returned. If false, the operation returns all Qualification types.
-- * 'nextToken' -
-- * 'query' - A text query against all of the searchable attributes of Qualification types.
-- * 'mustBeRequestable' - Specifies that only Qualification types that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test, are returned as results of the search. Some Qualification types, such as those assigned automatically by the system, cannot be requested directly by users. If false, all Qualification types, including those managed by the system, are considered. Valid values are True | False.
-- * 'maxResults' - The maximum number of results to return in a single call.
mkListQualificationTypes ::
  -- | 'mustBeRequestable'
  Lude.Bool ->
  ListQualificationTypes
mkListQualificationTypes pMustBeRequestable_ =
  ListQualificationTypes'
    { mustBeOwnedByCaller = Lude.Nothing,
      nextToken = Lude.Nothing,
      query = Lude.Nothing,
      mustBeRequestable = pMustBeRequestable_,
      maxResults = Lude.Nothing
    }

-- | Specifies that only Qualification types that the Requester created are returned. If false, the operation returns all Qualification types.
--
-- /Note:/ Consider using 'mustBeOwnedByCaller' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtMustBeOwnedByCaller :: Lens.Lens' ListQualificationTypes (Lude.Maybe Lude.Bool)
lqtMustBeOwnedByCaller = Lens.lens (mustBeOwnedByCaller :: ListQualificationTypes -> Lude.Maybe Lude.Bool) (\s a -> s {mustBeOwnedByCaller = a} :: ListQualificationTypes)
{-# DEPRECATED lqtMustBeOwnedByCaller "Use generic-lens or generic-optics with 'mustBeOwnedByCaller' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtNextToken :: Lens.Lens' ListQualificationTypes (Lude.Maybe Lude.Text)
lqtNextToken = Lens.lens (nextToken :: ListQualificationTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQualificationTypes)
{-# DEPRECATED lqtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A text query against all of the searchable attributes of Qualification types.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtQuery :: Lens.Lens' ListQualificationTypes (Lude.Maybe Lude.Text)
lqtQuery = Lens.lens (query :: ListQualificationTypes -> Lude.Maybe Lude.Text) (\s a -> s {query = a} :: ListQualificationTypes)
{-# DEPRECATED lqtQuery "Use generic-lens or generic-optics with 'query' instead." #-}

-- | Specifies that only Qualification types that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test, are returned as results of the search. Some Qualification types, such as those assigned automatically by the system, cannot be requested directly by users. If false, all Qualification types, including those managed by the system, are considered. Valid values are True | False.
--
-- /Note:/ Consider using 'mustBeRequestable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtMustBeRequestable :: Lens.Lens' ListQualificationTypes Lude.Bool
lqtMustBeRequestable = Lens.lens (mustBeRequestable :: ListQualificationTypes -> Lude.Bool) (\s a -> s {mustBeRequestable = a} :: ListQualificationTypes)
{-# DEPRECATED lqtMustBeRequestable "Use generic-lens or generic-optics with 'mustBeRequestable' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtMaxResults :: Lens.Lens' ListQualificationTypes (Lude.Maybe Lude.Natural)
lqtMaxResults = Lens.lens (maxResults :: ListQualificationTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListQualificationTypes)
{-# DEPRECATED lqtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListQualificationTypes where
  page rq rs
    | Page.stop (rs Lens.^. lqtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lqtrsQualificationTypes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lqtNextToken Lens..~ rs Lens.^. lqtrsNextToken

instance Lude.AWSRequest ListQualificationTypes where
  type Rs ListQualificationTypes = ListQualificationTypesResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListQualificationTypesResponse'
            Lude.<$> (x Lude..?> "QualificationTypes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "NumResults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListQualificationTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.ListQualificationTypes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListQualificationTypes where
  toJSON ListQualificationTypes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MustBeOwnedByCaller" Lude..=) Lude.<$> mustBeOwnedByCaller,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Query" Lude..=) Lude.<$> query,
            Lude.Just ("MustBeRequestable" Lude..= mustBeRequestable),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListQualificationTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery ListQualificationTypes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListQualificationTypesResponse' smart constructor.
data ListQualificationTypesResponse = ListQualificationTypesResponse'
  { -- | The list of QualificationType elements returned by the query.
    qualificationTypes :: Lude.Maybe [QualificationType],
    nextToken :: Lude.Maybe Lude.Text,
    -- | The number of Qualification types on this page in the filtered results list, equivalent to the number of types this operation returns.
    numResults :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQualificationTypesResponse' with the minimum fields required to make a request.
--
-- * 'qualificationTypes' - The list of QualificationType elements returned by the query.
-- * 'nextToken' -
-- * 'numResults' - The number of Qualification types on this page in the filtered results list, equivalent to the number of types this operation returns.
-- * 'responseStatus' - The response status code.
mkListQualificationTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListQualificationTypesResponse
mkListQualificationTypesResponse pResponseStatus_ =
  ListQualificationTypesResponse'
    { qualificationTypes =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      numResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of QualificationType elements returned by the query.
--
-- /Note:/ Consider using 'qualificationTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrsQualificationTypes :: Lens.Lens' ListQualificationTypesResponse (Lude.Maybe [QualificationType])
lqtrsQualificationTypes = Lens.lens (qualificationTypes :: ListQualificationTypesResponse -> Lude.Maybe [QualificationType]) (\s a -> s {qualificationTypes = a} :: ListQualificationTypesResponse)
{-# DEPRECATED lqtrsQualificationTypes "Use generic-lens or generic-optics with 'qualificationTypes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrsNextToken :: Lens.Lens' ListQualificationTypesResponse (Lude.Maybe Lude.Text)
lqtrsNextToken = Lens.lens (nextToken :: ListQualificationTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQualificationTypesResponse)
{-# DEPRECATED lqtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of Qualification types on this page in the filtered results list, equivalent to the number of types this operation returns.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrsNumResults :: Lens.Lens' ListQualificationTypesResponse (Lude.Maybe Lude.Int)
lqtrsNumResults = Lens.lens (numResults :: ListQualificationTypesResponse -> Lude.Maybe Lude.Int) (\s a -> s {numResults = a} :: ListQualificationTypesResponse)
{-# DEPRECATED lqtrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrsResponseStatus :: Lens.Lens' ListQualificationTypesResponse Lude.Int
lqtrsResponseStatus = Lens.lens (responseStatus :: ListQualificationTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListQualificationTypesResponse)
{-# DEPRECATED lqtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
