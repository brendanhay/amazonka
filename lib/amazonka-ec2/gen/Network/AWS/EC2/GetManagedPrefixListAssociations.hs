{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetManagedPrefixListAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the resources that are associated with the specified managed prefix list.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetManagedPrefixListAssociations
  ( -- * Creating a request
    GetManagedPrefixListAssociations (..),
    mkGetManagedPrefixListAssociations,

    -- ** Request lenses
    gmplaNextToken,
    gmplaPrefixListId,
    gmplaDryRun,
    gmplaMaxResults,

    -- * Destructuring the response
    GetManagedPrefixListAssociationsResponse (..),
    mkGetManagedPrefixListAssociationsResponse,

    -- ** Response lenses
    gmplarsNextToken,
    gmplarsPrefixListAssociations,
    gmplarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetManagedPrefixListAssociations' smart constructor.
data GetManagedPrefixListAssociations = GetManagedPrefixListAssociations'
  { -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the prefix list.
    prefixListId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetManagedPrefixListAssociations' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next page of results.
-- * 'prefixListId' - The ID of the prefix list.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkGetManagedPrefixListAssociations ::
  -- | 'prefixListId'
  Lude.Text ->
  GetManagedPrefixListAssociations
mkGetManagedPrefixListAssociations pPrefixListId_ =
  GetManagedPrefixListAssociations'
    { nextToken = Lude.Nothing,
      prefixListId = pPrefixListId_,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplaNextToken :: Lens.Lens' GetManagedPrefixListAssociations (Lude.Maybe Lude.Text)
gmplaNextToken = Lens.lens (nextToken :: GetManagedPrefixListAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetManagedPrefixListAssociations)
{-# DEPRECATED gmplaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplaPrefixListId :: Lens.Lens' GetManagedPrefixListAssociations Lude.Text
gmplaPrefixListId = Lens.lens (prefixListId :: GetManagedPrefixListAssociations -> Lude.Text) (\s a -> s {prefixListId = a} :: GetManagedPrefixListAssociations)
{-# DEPRECATED gmplaPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplaDryRun :: Lens.Lens' GetManagedPrefixListAssociations (Lude.Maybe Lude.Bool)
gmplaDryRun = Lens.lens (dryRun :: GetManagedPrefixListAssociations -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetManagedPrefixListAssociations)
{-# DEPRECATED gmplaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplaMaxResults :: Lens.Lens' GetManagedPrefixListAssociations (Lude.Maybe Lude.Natural)
gmplaMaxResults = Lens.lens (maxResults :: GetManagedPrefixListAssociations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetManagedPrefixListAssociations)
{-# DEPRECATED gmplaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetManagedPrefixListAssociations where
  page rq rs
    | Page.stop (rs Lens.^. gmplarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gmplarsPrefixListAssociations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gmplaNextToken Lens..~ rs Lens.^. gmplarsNextToken

instance Lude.AWSRequest GetManagedPrefixListAssociations where
  type
    Rs GetManagedPrefixListAssociations =
      GetManagedPrefixListAssociationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetManagedPrefixListAssociationsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "prefixListAssociationSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetManagedPrefixListAssociations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetManagedPrefixListAssociations where
  toPath = Lude.const "/"

instance Lude.ToQuery GetManagedPrefixListAssociations where
  toQuery GetManagedPrefixListAssociations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetManagedPrefixListAssociations" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "PrefixListId" Lude.=: prefixListId,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetManagedPrefixListAssociationsResponse' smart constructor.
data GetManagedPrefixListAssociationsResponse = GetManagedPrefixListAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the associations.
    prefixListAssociations :: Lude.Maybe [PrefixListAssociation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetManagedPrefixListAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'prefixListAssociations' - Information about the associations.
-- * 'responseStatus' - The response status code.
mkGetManagedPrefixListAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetManagedPrefixListAssociationsResponse
mkGetManagedPrefixListAssociationsResponse pResponseStatus_ =
  GetManagedPrefixListAssociationsResponse'
    { nextToken =
        Lude.Nothing,
      prefixListAssociations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplarsNextToken :: Lens.Lens' GetManagedPrefixListAssociationsResponse (Lude.Maybe Lude.Text)
gmplarsNextToken = Lens.lens (nextToken :: GetManagedPrefixListAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetManagedPrefixListAssociationsResponse)
{-# DEPRECATED gmplarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the associations.
--
-- /Note:/ Consider using 'prefixListAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplarsPrefixListAssociations :: Lens.Lens' GetManagedPrefixListAssociationsResponse (Lude.Maybe [PrefixListAssociation])
gmplarsPrefixListAssociations = Lens.lens (prefixListAssociations :: GetManagedPrefixListAssociationsResponse -> Lude.Maybe [PrefixListAssociation]) (\s a -> s {prefixListAssociations = a} :: GetManagedPrefixListAssociationsResponse)
{-# DEPRECATED gmplarsPrefixListAssociations "Use generic-lens or generic-optics with 'prefixListAssociations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplarsResponseStatus :: Lens.Lens' GetManagedPrefixListAssociationsResponse Lude.Int
gmplarsResponseStatus = Lens.lens (responseStatus :: GetManagedPrefixListAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetManagedPrefixListAssociationsResponse)
{-# DEPRECATED gmplarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
