{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.GetUpgradeHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the complete history of the last 10 upgrades that were performed on the domain.
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.GetUpgradeHistory
  ( -- * Creating a request
    GetUpgradeHistory (..),
    mkGetUpgradeHistory,

    -- ** Request lenses
    guhNextToken,
    guhDomainName,
    guhMaxResults,

    -- * Destructuring the response
    GetUpgradeHistoryResponse (..),
    mkGetUpgradeHistoryResponse,

    -- ** Response lenses
    guhrsNextToken,
    guhrsUpgradeHistories,
    guhrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'GetUpgradeHistory' @ operation.
--
-- /See:/ 'mkGetUpgradeHistory' smart constructor.
data GetUpgradeHistory = GetUpgradeHistory'
  { nextToken :: Lude.Maybe Lude.Text,
    domainName :: Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUpgradeHistory' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'domainName' -
-- * 'maxResults' -
mkGetUpgradeHistory ::
  -- | 'domainName'
  Lude.Text ->
  GetUpgradeHistory
mkGetUpgradeHistory pDomainName_ =
  GetUpgradeHistory'
    { nextToken = Lude.Nothing,
      domainName = pDomainName_,
      maxResults = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guhNextToken :: Lens.Lens' GetUpgradeHistory (Lude.Maybe Lude.Text)
guhNextToken = Lens.lens (nextToken :: GetUpgradeHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetUpgradeHistory)
{-# DEPRECATED guhNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guhDomainName :: Lens.Lens' GetUpgradeHistory Lude.Text
guhDomainName = Lens.lens (domainName :: GetUpgradeHistory -> Lude.Text) (\s a -> s {domainName = a} :: GetUpgradeHistory)
{-# DEPRECATED guhDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guhMaxResults :: Lens.Lens' GetUpgradeHistory (Lude.Maybe Lude.Int)
guhMaxResults = Lens.lens (maxResults :: GetUpgradeHistory -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetUpgradeHistory)
{-# DEPRECATED guhMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetUpgradeHistory where
  page rq rs
    | Page.stop (rs Lens.^. guhrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. guhrsUpgradeHistories) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& guhNextToken Lens..~ rs Lens.^. guhrsNextToken

instance Lude.AWSRequest GetUpgradeHistory where
  type Rs GetUpgradeHistory = GetUpgradeHistoryResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUpgradeHistoryResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "UpgradeHistories" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUpgradeHistory where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetUpgradeHistory where
  toPath GetUpgradeHistory' {..} =
    Lude.mconcat
      ["/2015-01-01/es/upgradeDomain/", Lude.toBS domainName, "/history"]

instance Lude.ToQuery GetUpgradeHistory where
  toQuery GetUpgradeHistory' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Container for response returned by @'GetUpgradeHistory' @ operation.
--
-- /See:/ 'mkGetUpgradeHistoryResponse' smart constructor.
data GetUpgradeHistoryResponse = GetUpgradeHistoryResponse'
  { -- | Pagination token that needs to be supplied to the next call to get the next page of results
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of @'UpgradeHistory' @ objects corresponding to each Upgrade or Upgrade Eligibility Check performed on a domain returned as part of @'GetUpgradeHistoryResponse' @ object.
    upgradeHistories :: Lude.Maybe [UpgradeHistory],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUpgradeHistoryResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token that needs to be supplied to the next call to get the next page of results
-- * 'upgradeHistories' - A list of @'UpgradeHistory' @ objects corresponding to each Upgrade or Upgrade Eligibility Check performed on a domain returned as part of @'GetUpgradeHistoryResponse' @ object.
-- * 'responseStatus' - The response status code.
mkGetUpgradeHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUpgradeHistoryResponse
mkGetUpgradeHistoryResponse pResponseStatus_ =
  GetUpgradeHistoryResponse'
    { nextToken = Lude.Nothing,
      upgradeHistories = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Pagination token that needs to be supplied to the next call to get the next page of results
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guhrsNextToken :: Lens.Lens' GetUpgradeHistoryResponse (Lude.Maybe Lude.Text)
guhrsNextToken = Lens.lens (nextToken :: GetUpgradeHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetUpgradeHistoryResponse)
{-# DEPRECATED guhrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @'UpgradeHistory' @ objects corresponding to each Upgrade or Upgrade Eligibility Check performed on a domain returned as part of @'GetUpgradeHistoryResponse' @ object.
--
-- /Note:/ Consider using 'upgradeHistories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guhrsUpgradeHistories :: Lens.Lens' GetUpgradeHistoryResponse (Lude.Maybe [UpgradeHistory])
guhrsUpgradeHistories = Lens.lens (upgradeHistories :: GetUpgradeHistoryResponse -> Lude.Maybe [UpgradeHistory]) (\s a -> s {upgradeHistories = a} :: GetUpgradeHistoryResponse)
{-# DEPRECATED guhrsUpgradeHistories "Use generic-lens or generic-optics with 'upgradeHistories' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guhrsResponseStatus :: Lens.Lens' GetUpgradeHistoryResponse Lude.Int
guhrsResponseStatus = Lens.lens (responseStatus :: GetUpgradeHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUpgradeHistoryResponse)
{-# DEPRECATED guhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
