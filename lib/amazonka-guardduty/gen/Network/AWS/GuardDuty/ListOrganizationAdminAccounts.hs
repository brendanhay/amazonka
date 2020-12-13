{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListOrganizationAdminAccounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the accounts configured as GuardDuty delegated administrators.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListOrganizationAdminAccounts
  ( -- * Creating a request
    ListOrganizationAdminAccounts (..),
    mkListOrganizationAdminAccounts,

    -- ** Request lenses
    loaaNextToken,
    loaaMaxResults,

    -- * Destructuring the response
    ListOrganizationAdminAccountsResponse (..),
    mkListOrganizationAdminAccountsResponse,

    -- ** Response lenses
    loaarsAdminAccounts,
    loaarsNextToken,
    loaarsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListOrganizationAdminAccounts' smart constructor.
data ListOrganizationAdminAccounts = ListOrganizationAdminAccounts'
  { -- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOrganizationAdminAccounts' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
-- * 'maxResults' - The maximum number of results to return in the response.
mkListOrganizationAdminAccounts ::
  ListOrganizationAdminAccounts
mkListOrganizationAdminAccounts =
  ListOrganizationAdminAccounts'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaaNextToken :: Lens.Lens' ListOrganizationAdminAccounts (Lude.Maybe Lude.Text)
loaaNextToken = Lens.lens (nextToken :: ListOrganizationAdminAccounts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOrganizationAdminAccounts)
{-# DEPRECATED loaaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaaMaxResults :: Lens.Lens' ListOrganizationAdminAccounts (Lude.Maybe Lude.Natural)
loaaMaxResults = Lens.lens (maxResults :: ListOrganizationAdminAccounts -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListOrganizationAdminAccounts)
{-# DEPRECATED loaaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListOrganizationAdminAccounts where
  page rq rs
    | Page.stop (rs Lens.^. loaarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. loaarsAdminAccounts) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loaaNextToken Lens..~ rs Lens.^. loaarsNextToken

instance Lude.AWSRequest ListOrganizationAdminAccounts where
  type
    Rs ListOrganizationAdminAccounts =
      ListOrganizationAdminAccountsResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOrganizationAdminAccountsResponse'
            Lude.<$> (x Lude..?> "adminAccounts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOrganizationAdminAccounts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListOrganizationAdminAccounts where
  toPath = Lude.const "/admin"

instance Lude.ToQuery ListOrganizationAdminAccounts where
  toQuery ListOrganizationAdminAccounts' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListOrganizationAdminAccountsResponse' smart constructor.
data ListOrganizationAdminAccountsResponse = ListOrganizationAdminAccountsResponse'
  { -- | An AdminAccounts object that includes a list of accounts configured as GuardDuty delegated administrators.
    adminAccounts :: Lude.Maybe [AdminAccount],
    -- | The pagination parameter to be used on the next list operation to retrieve more items.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOrganizationAdminAccountsResponse' with the minimum fields required to make a request.
--
-- * 'adminAccounts' - An AdminAccounts object that includes a list of accounts configured as GuardDuty delegated administrators.
-- * 'nextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
-- * 'responseStatus' - The response status code.
mkListOrganizationAdminAccountsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOrganizationAdminAccountsResponse
mkListOrganizationAdminAccountsResponse pResponseStatus_ =
  ListOrganizationAdminAccountsResponse'
    { adminAccounts =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An AdminAccounts object that includes a list of accounts configured as GuardDuty delegated administrators.
--
-- /Note:/ Consider using 'adminAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaarsAdminAccounts :: Lens.Lens' ListOrganizationAdminAccountsResponse (Lude.Maybe [AdminAccount])
loaarsAdminAccounts = Lens.lens (adminAccounts :: ListOrganizationAdminAccountsResponse -> Lude.Maybe [AdminAccount]) (\s a -> s {adminAccounts = a} :: ListOrganizationAdminAccountsResponse)
{-# DEPRECATED loaarsAdminAccounts "Use generic-lens or generic-optics with 'adminAccounts' instead." #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaarsNextToken :: Lens.Lens' ListOrganizationAdminAccountsResponse (Lude.Maybe Lude.Text)
loaarsNextToken = Lens.lens (nextToken :: ListOrganizationAdminAccountsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOrganizationAdminAccountsResponse)
{-# DEPRECATED loaarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaarsResponseStatus :: Lens.Lens' ListOrganizationAdminAccountsResponse Lude.Int
loaarsResponseStatus = Lens.lens (responseStatus :: ListOrganizationAdminAccountsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOrganizationAdminAccountsResponse)
{-# DEPRECATED loaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
