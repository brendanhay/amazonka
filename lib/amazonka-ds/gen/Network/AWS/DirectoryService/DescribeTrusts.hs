{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeTrusts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the trust relationships for this account.
--
-- If no input parameters are provided, such as DirectoryId or TrustIds, this request describes all the trust relationships belonging to the account.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeTrusts
  ( -- * Creating a request
    DescribeTrusts (..),
    mkDescribeTrusts,

    -- ** Request lenses
    dtDirectoryId,
    dtNextToken,
    dtTrustIds,
    dtLimit,

    -- * Destructuring the response
    DescribeTrustsResponse (..),
    mkDescribeTrustsResponse,

    -- ** Response lenses
    dtrsNextToken,
    dtrsTrusts,
    dtrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Describes the trust relationships for a particular AWS Managed Microsoft AD directory. If no input parameters are are provided, such as directory ID or trust ID, this request describes all the trust relationships.
--
-- /See:/ 'mkDescribeTrusts' smart constructor.
data DescribeTrusts = DescribeTrusts'
  { directoryId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    trustIds :: Lude.Maybe [Lude.Text],
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrusts' with the minimum fields required to make a request.
--
-- * 'directoryId' - The Directory ID of the AWS directory that is a part of the requested trust relationship.
-- * 'limit' - The maximum number of objects to return.
-- * 'nextToken' - The /DescribeTrustsResult.NextToken/ value from a previous call to 'DescribeTrusts' . Pass null if this is the first call.
-- * 'trustIds' - A list of identifiers of the trust relationships for which to obtain the information. If this member is null, all trust relationships that belong to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
mkDescribeTrusts ::
  DescribeTrusts
mkDescribeTrusts =
  DescribeTrusts'
    { directoryId = Lude.Nothing,
      nextToken = Lude.Nothing,
      trustIds = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The Directory ID of the AWS directory that is a part of the requested trust relationship.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDirectoryId :: Lens.Lens' DescribeTrusts (Lude.Maybe Lude.Text)
dtDirectoryId = Lens.lens (directoryId :: DescribeTrusts -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: DescribeTrusts)
{-# DEPRECATED dtDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The /DescribeTrustsResult.NextToken/ value from a previous call to 'DescribeTrusts' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtNextToken :: Lens.Lens' DescribeTrusts (Lude.Maybe Lude.Text)
dtNextToken = Lens.lens (nextToken :: DescribeTrusts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTrusts)
{-# DEPRECATED dtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of identifiers of the trust relationships for which to obtain the information. If this member is null, all trust relationships that belong to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
--
-- /Note:/ Consider using 'trustIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTrustIds :: Lens.Lens' DescribeTrusts (Lude.Maybe [Lude.Text])
dtTrustIds = Lens.lens (trustIds :: DescribeTrusts -> Lude.Maybe [Lude.Text]) (\s a -> s {trustIds = a} :: DescribeTrusts)
{-# DEPRECATED dtTrustIds "Use generic-lens or generic-optics with 'trustIds' instead." #-}

-- | The maximum number of objects to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtLimit :: Lens.Lens' DescribeTrusts (Lude.Maybe Lude.Natural)
dtLimit = Lens.lens (limit :: DescribeTrusts -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeTrusts)
{-# DEPRECATED dtLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeTrusts where
  page rq rs
    | Page.stop (rs Lens.^. dtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtrsTrusts) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtNextToken Lens..~ rs Lens.^. dtrsNextToken

instance Lude.AWSRequest DescribeTrusts where
  type Rs DescribeTrusts = DescribeTrustsResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTrustsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Trusts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrusts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.DescribeTrusts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTrusts where
  toJSON DescribeTrusts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DirectoryId" Lude..=) Lude.<$> directoryId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("TrustIds" Lude..=) Lude.<$> trustIds,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeTrusts where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrusts where
  toQuery = Lude.const Lude.mempty

-- | The result of a DescribeTrust request.
--
-- /See:/ 'mkDescribeTrustsResponse' smart constructor.
data DescribeTrustsResponse = DescribeTrustsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    trusts :: Lude.Maybe [Trust],
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

-- | Creates a value of 'DescribeTrustsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'DescribeTrusts' to retrieve the next set of items.
-- * 'responseStatus' - The response status code.
-- * 'trusts' - The list of Trust objects that were retrieved.
--
-- It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
mkDescribeTrustsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrustsResponse
mkDescribeTrustsResponse pResponseStatus_ =
  DescribeTrustsResponse'
    { nextToken = Lude.Nothing,
      trusts = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'DescribeTrusts' to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsNextToken :: Lens.Lens' DescribeTrustsResponse (Lude.Maybe Lude.Text)
dtrsNextToken = Lens.lens (nextToken :: DescribeTrustsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTrustsResponse)
{-# DEPRECATED dtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of Trust objects that were retrieved.
--
-- It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
--
-- /Note:/ Consider using 'trusts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTrusts :: Lens.Lens' DescribeTrustsResponse (Lude.Maybe [Trust])
dtrsTrusts = Lens.lens (trusts :: DescribeTrustsResponse -> Lude.Maybe [Trust]) (\s a -> s {trusts = a} :: DescribeTrustsResponse)
{-# DEPRECATED dtrsTrusts "Use generic-lens or generic-optics with 'trusts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeTrustsResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeTrustsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrustsResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
