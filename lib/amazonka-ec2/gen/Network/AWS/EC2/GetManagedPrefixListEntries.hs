{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetManagedPrefixListEntries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the entries for a specified managed prefix list.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetManagedPrefixListEntries
  ( -- * Creating a request
    GetManagedPrefixListEntries (..),
    mkGetManagedPrefixListEntries,

    -- ** Request lenses
    gmpleNextToken,
    gmpleTargetVersion,
    gmpleDryRun,
    gmpleMaxResults,
    gmplePrefixListId,

    -- * Destructuring the response
    GetManagedPrefixListEntriesResponse (..),
    mkGetManagedPrefixListEntriesResponse,

    -- ** Response lenses
    gmplersEntries,
    gmplersNextToken,
    gmplersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetManagedPrefixListEntries' smart constructor.
data GetManagedPrefixListEntries = GetManagedPrefixListEntries'
  { nextToken ::
      Lude.Maybe Lude.Text,
    targetVersion ::
      Lude.Maybe Lude.Integer,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults ::
      Lude.Maybe Lude.Natural,
    prefixListId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetManagedPrefixListEntries' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'prefixListId' - The ID of the prefix list.
-- * 'targetVersion' - The version of the prefix list for which to return the entries. The default is the current version.
mkGetManagedPrefixListEntries ::
  -- | 'prefixListId'
  Lude.Text ->
  GetManagedPrefixListEntries
mkGetManagedPrefixListEntries pPrefixListId_ =
  GetManagedPrefixListEntries'
    { nextToken = Lude.Nothing,
      targetVersion = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      prefixListId = pPrefixListId_
    }

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmpleNextToken :: Lens.Lens' GetManagedPrefixListEntries (Lude.Maybe Lude.Text)
gmpleNextToken = Lens.lens (nextToken :: GetManagedPrefixListEntries -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetManagedPrefixListEntries)
{-# DEPRECATED gmpleNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The version of the prefix list for which to return the entries. The default is the current version.
--
-- /Note:/ Consider using 'targetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmpleTargetVersion :: Lens.Lens' GetManagedPrefixListEntries (Lude.Maybe Lude.Integer)
gmpleTargetVersion = Lens.lens (targetVersion :: GetManagedPrefixListEntries -> Lude.Maybe Lude.Integer) (\s a -> s {targetVersion = a} :: GetManagedPrefixListEntries)
{-# DEPRECATED gmpleTargetVersion "Use generic-lens or generic-optics with 'targetVersion' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmpleDryRun :: Lens.Lens' GetManagedPrefixListEntries (Lude.Maybe Lude.Bool)
gmpleDryRun = Lens.lens (dryRun :: GetManagedPrefixListEntries -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetManagedPrefixListEntries)
{-# DEPRECATED gmpleDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmpleMaxResults :: Lens.Lens' GetManagedPrefixListEntries (Lude.Maybe Lude.Natural)
gmpleMaxResults = Lens.lens (maxResults :: GetManagedPrefixListEntries -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetManagedPrefixListEntries)
{-# DEPRECATED gmpleMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplePrefixListId :: Lens.Lens' GetManagedPrefixListEntries Lude.Text
gmplePrefixListId = Lens.lens (prefixListId :: GetManagedPrefixListEntries -> Lude.Text) (\s a -> s {prefixListId = a} :: GetManagedPrefixListEntries)
{-# DEPRECATED gmplePrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

instance Page.AWSPager GetManagedPrefixListEntries where
  page rq rs
    | Page.stop (rs Lens.^. gmplersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gmplersEntries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gmpleNextToken Lens..~ rs Lens.^. gmplersNextToken

instance Lude.AWSRequest GetManagedPrefixListEntries where
  type
    Rs GetManagedPrefixListEntries =
      GetManagedPrefixListEntriesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetManagedPrefixListEntriesResponse'
            Lude.<$> ( x Lude..@? "entrySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetManagedPrefixListEntries where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetManagedPrefixListEntries where
  toPath = Lude.const "/"

instance Lude.ToQuery GetManagedPrefixListEntries where
  toQuery GetManagedPrefixListEntries' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetManagedPrefixListEntries" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "TargetVersion" Lude.=: targetVersion,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "PrefixListId" Lude.=: prefixListId
      ]

-- | /See:/ 'mkGetManagedPrefixListEntriesResponse' smart constructor.
data GetManagedPrefixListEntriesResponse = GetManagedPrefixListEntriesResponse'
  { entries ::
      Lude.Maybe
        [PrefixListEntry],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetManagedPrefixListEntriesResponse' with the minimum fields required to make a request.
--
-- * 'entries' - Information about the prefix list entries.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkGetManagedPrefixListEntriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetManagedPrefixListEntriesResponse
mkGetManagedPrefixListEntriesResponse pResponseStatus_ =
  GetManagedPrefixListEntriesResponse'
    { entries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the prefix list entries.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplersEntries :: Lens.Lens' GetManagedPrefixListEntriesResponse (Lude.Maybe [PrefixListEntry])
gmplersEntries = Lens.lens (entries :: GetManagedPrefixListEntriesResponse -> Lude.Maybe [PrefixListEntry]) (\s a -> s {entries = a} :: GetManagedPrefixListEntriesResponse)
{-# DEPRECATED gmplersEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplersNextToken :: Lens.Lens' GetManagedPrefixListEntriesResponse (Lude.Maybe Lude.Text)
gmplersNextToken = Lens.lens (nextToken :: GetManagedPrefixListEntriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetManagedPrefixListEntriesResponse)
{-# DEPRECATED gmplersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplersResponseStatus :: Lens.Lens' GetManagedPrefixListEntriesResponse Lude.Int
gmplersResponseStatus = Lens.lens (responseStatus :: GetManagedPrefixListEntriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetManagedPrefixListEntriesResponse)
{-# DEPRECATED gmplersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
