{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAuditSuppressions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Device Defender audit listings.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditSuppressions
  ( -- * Creating a request
    ListAuditSuppressions (..),
    mkListAuditSuppressions,

    -- ** Request lenses
    lasCheckName,
    lasNextToken,
    lasAscendingOrder,
    lasMaxResults,
    lasResourceIdentifier,

    -- * Destructuring the response
    ListAuditSuppressionsResponse (..),
    mkListAuditSuppressionsResponse,

    -- ** Response lenses
    lasrsNextToken,
    lasrsSuppressions,
    lasrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAuditSuppressions' smart constructor.
data ListAuditSuppressions = ListAuditSuppressions'
  { checkName :: Lude.Maybe Lude.Text,
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Determines whether suppressions are listed in ascending order by expiration date or not. If parameter isn't provided, @ascendingOrder=true@ .
    ascendingOrder :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Lude.Maybe Lude.Natural,
    resourceIdentifier :: Lude.Maybe ResourceIdentifier
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuditSuppressions' with the minimum fields required to make a request.
--
-- * 'checkName' -
-- * 'nextToken' - The token for the next set of results.
-- * 'ascendingOrder' - Determines whether suppressions are listed in ascending order by expiration date or not. If parameter isn't provided, @ascendingOrder=true@ .
-- * 'maxResults' - The maximum number of results to return at one time. The default is 25.
-- * 'resourceIdentifier' -
mkListAuditSuppressions ::
  ListAuditSuppressions
mkListAuditSuppressions =
  ListAuditSuppressions'
    { checkName = Lude.Nothing,
      nextToken = Lude.Nothing,
      ascendingOrder = Lude.Nothing,
      maxResults = Lude.Nothing,
      resourceIdentifier = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasCheckName :: Lens.Lens' ListAuditSuppressions (Lude.Maybe Lude.Text)
lasCheckName = Lens.lens (checkName :: ListAuditSuppressions -> Lude.Maybe Lude.Text) (\s a -> s {checkName = a} :: ListAuditSuppressions)
{-# DEPRECATED lasCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasNextToken :: Lens.Lens' ListAuditSuppressions (Lude.Maybe Lude.Text)
lasNextToken = Lens.lens (nextToken :: ListAuditSuppressions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAuditSuppressions)
{-# DEPRECATED lasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Determines whether suppressions are listed in ascending order by expiration date or not. If parameter isn't provided, @ascendingOrder=true@ .
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasAscendingOrder :: Lens.Lens' ListAuditSuppressions (Lude.Maybe Lude.Bool)
lasAscendingOrder = Lens.lens (ascendingOrder :: ListAuditSuppressions -> Lude.Maybe Lude.Bool) (\s a -> s {ascendingOrder = a} :: ListAuditSuppressions)
{-# DEPRECATED lasAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasMaxResults :: Lens.Lens' ListAuditSuppressions (Lude.Maybe Lude.Natural)
lasMaxResults = Lens.lens (maxResults :: ListAuditSuppressions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAuditSuppressions)
{-# DEPRECATED lasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasResourceIdentifier :: Lens.Lens' ListAuditSuppressions (Lude.Maybe ResourceIdentifier)
lasResourceIdentifier = Lens.lens (resourceIdentifier :: ListAuditSuppressions -> Lude.Maybe ResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: ListAuditSuppressions)
{-# DEPRECATED lasResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Page.AWSPager ListAuditSuppressions where
  page rq rs
    | Page.stop (rs Lens.^. lasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lasrsSuppressions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lasNextToken Lens..~ rs Lens.^. lasrsNextToken

instance Lude.AWSRequest ListAuditSuppressions where
  type Rs ListAuditSuppressions = ListAuditSuppressionsResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAuditSuppressionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "suppressions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAuditSuppressions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListAuditSuppressions where
  toJSON ListAuditSuppressions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("checkName" Lude..=) Lude.<$> checkName,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("ascendingOrder" Lude..=) Lude.<$> ascendingOrder,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            ("resourceIdentifier" Lude..=) Lude.<$> resourceIdentifier
          ]
      )

instance Lude.ToPath ListAuditSuppressions where
  toPath = Lude.const "/audit/suppressions/list"

instance Lude.ToQuery ListAuditSuppressions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAuditSuppressionsResponse' smart constructor.
data ListAuditSuppressionsResponse = ListAuditSuppressionsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | List of audit suppressions.
    suppressions :: Lude.Maybe [AuditSuppression],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuditSuppressionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
-- * 'suppressions' - List of audit suppressions.
-- * 'responseStatus' - The response status code.
mkListAuditSuppressionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAuditSuppressionsResponse
mkListAuditSuppressionsResponse pResponseStatus_ =
  ListAuditSuppressionsResponse'
    { nextToken = Lude.Nothing,
      suppressions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsNextToken :: Lens.Lens' ListAuditSuppressionsResponse (Lude.Maybe Lude.Text)
lasrsNextToken = Lens.lens (nextToken :: ListAuditSuppressionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAuditSuppressionsResponse)
{-# DEPRECATED lasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of audit suppressions.
--
-- /Note:/ Consider using 'suppressions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsSuppressions :: Lens.Lens' ListAuditSuppressionsResponse (Lude.Maybe [AuditSuppression])
lasrsSuppressions = Lens.lens (suppressions :: ListAuditSuppressionsResponse -> Lude.Maybe [AuditSuppression]) (\s a -> s {suppressions = a} :: ListAuditSuppressionsResponse)
{-# DEPRECATED lasrsSuppressions "Use generic-lens or generic-optics with 'suppressions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsResponseStatus :: Lens.Lens' ListAuditSuppressionsResponse Lude.Int
lasrsResponseStatus = Lens.lens (responseStatus :: ListAuditSuppressionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAuditSuppressionsResponse)
{-# DEPRECATED lasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
