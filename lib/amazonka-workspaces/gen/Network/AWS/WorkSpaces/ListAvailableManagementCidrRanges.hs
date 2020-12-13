{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of IP address ranges, specified as IPv4 CIDR blocks, that you can use for the network management interface when you enable Bring Your Own License (BYOL).
--
-- This operation can be run only by AWS accounts that are enabled for BYOL. If your account isn't enabled for BYOL, you'll receive an @AccessDeniedException@ error.
-- The management network interface is connected to a secure Amazon WorkSpaces management network. It is used for interactive streaming of the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon WorkSpaces to manage the WorkSpace.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges
  ( -- * Creating a request
    ListAvailableManagementCidrRanges (..),
    mkListAvailableManagementCidrRanges,

    -- ** Request lenses
    lamcrManagementCidrRangeConstraint,
    lamcrNextToken,
    lamcrMaxResults,

    -- * Destructuring the response
    ListAvailableManagementCidrRangesResponse (..),
    mkListAvailableManagementCidrRangesResponse,

    -- ** Response lenses
    lamcrrsManagementCidrRanges,
    lamcrrsNextToken,
    lamcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkListAvailableManagementCidrRanges' smart constructor.
data ListAvailableManagementCidrRanges = ListAvailableManagementCidrRanges'
  { -- | The IP address range to search. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block).
    managementCidrRangeConstraint :: Lude.Text,
    -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAvailableManagementCidrRanges' with the minimum fields required to make a request.
--
-- * 'managementCidrRangeConstraint' - The IP address range to search. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block).
-- * 'nextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
-- * 'maxResults' - The maximum number of items to return.
mkListAvailableManagementCidrRanges ::
  -- | 'managementCidrRangeConstraint'
  Lude.Text ->
  ListAvailableManagementCidrRanges
mkListAvailableManagementCidrRanges pManagementCidrRangeConstraint_ =
  ListAvailableManagementCidrRanges'
    { managementCidrRangeConstraint =
        pManagementCidrRangeConstraint_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The IP address range to search. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block).
--
-- /Note:/ Consider using 'managementCidrRangeConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrManagementCidrRangeConstraint :: Lens.Lens' ListAvailableManagementCidrRanges Lude.Text
lamcrManagementCidrRangeConstraint = Lens.lens (managementCidrRangeConstraint :: ListAvailableManagementCidrRanges -> Lude.Text) (\s a -> s {managementCidrRangeConstraint = a} :: ListAvailableManagementCidrRanges)
{-# DEPRECATED lamcrManagementCidrRangeConstraint "Use generic-lens or generic-optics with 'managementCidrRangeConstraint' instead." #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrNextToken :: Lens.Lens' ListAvailableManagementCidrRanges (Lude.Maybe Lude.Text)
lamcrNextToken = Lens.lens (nextToken :: ListAvailableManagementCidrRanges -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAvailableManagementCidrRanges)
{-# DEPRECATED lamcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrMaxResults :: Lens.Lens' ListAvailableManagementCidrRanges (Lude.Maybe Lude.Natural)
lamcrMaxResults = Lens.lens (maxResults :: ListAvailableManagementCidrRanges -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAvailableManagementCidrRanges)
{-# DEPRECATED lamcrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAvailableManagementCidrRanges where
  page rq rs
    | Page.stop (rs Lens.^. lamcrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lamcrrsManagementCidrRanges) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lamcrNextToken Lens..~ rs Lens.^. lamcrrsNextToken

instance Lude.AWSRequest ListAvailableManagementCidrRanges where
  type
    Rs ListAvailableManagementCidrRanges =
      ListAvailableManagementCidrRangesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAvailableManagementCidrRangesResponse'
            Lude.<$> (x Lude..?> "ManagementCidrRanges" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAvailableManagementCidrRanges where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.ListAvailableManagementCidrRanges" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAvailableManagementCidrRanges where
  toJSON ListAvailableManagementCidrRanges' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "ManagementCidrRangeConstraint"
                  Lude..= managementCidrRangeConstraint
              ),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListAvailableManagementCidrRanges where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAvailableManagementCidrRanges where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAvailableManagementCidrRangesResponse' smart constructor.
data ListAvailableManagementCidrRangesResponse = ListAvailableManagementCidrRangesResponse'
  { -- | The list of available IP address ranges, specified as IPv4 CIDR blocks.
    managementCidrRanges :: Lude.Maybe [Lude.Text],
    -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAvailableManagementCidrRangesResponse' with the minimum fields required to make a request.
--
-- * 'managementCidrRanges' - The list of available IP address ranges, specified as IPv4 CIDR blocks.
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
-- * 'responseStatus' - The response status code.
mkListAvailableManagementCidrRangesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAvailableManagementCidrRangesResponse
mkListAvailableManagementCidrRangesResponse pResponseStatus_ =
  ListAvailableManagementCidrRangesResponse'
    { managementCidrRanges =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of available IP address ranges, specified as IPv4 CIDR blocks.
--
-- /Note:/ Consider using 'managementCidrRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrrsManagementCidrRanges :: Lens.Lens' ListAvailableManagementCidrRangesResponse (Lude.Maybe [Lude.Text])
lamcrrsManagementCidrRanges = Lens.lens (managementCidrRanges :: ListAvailableManagementCidrRangesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {managementCidrRanges = a} :: ListAvailableManagementCidrRangesResponse)
{-# DEPRECATED lamcrrsManagementCidrRanges "Use generic-lens or generic-optics with 'managementCidrRanges' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrrsNextToken :: Lens.Lens' ListAvailableManagementCidrRangesResponse (Lude.Maybe Lude.Text)
lamcrrsNextToken = Lens.lens (nextToken :: ListAvailableManagementCidrRangesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAvailableManagementCidrRangesResponse)
{-# DEPRECATED lamcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrrsResponseStatus :: Lens.Lens' ListAvailableManagementCidrRangesResponse Lude.Int
lamcrrsResponseStatus = Lens.lens (responseStatus :: ListAvailableManagementCidrRangesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAvailableManagementCidrRangesResponse)
{-# DEPRECATED lamcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
