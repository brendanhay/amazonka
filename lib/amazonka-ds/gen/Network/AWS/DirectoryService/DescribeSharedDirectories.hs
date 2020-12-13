{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeSharedDirectories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the shared directories in your account.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeSharedDirectories
  ( -- * Creating a request
    DescribeSharedDirectories (..),
    mkDescribeSharedDirectories,

    -- ** Request lenses
    dsdSharedDirectoryIds,
    dsdNextToken,
    dsdLimit,
    dsdOwnerDirectoryId,

    -- * Destructuring the response
    DescribeSharedDirectoriesResponse (..),
    mkDescribeSharedDirectoriesResponse,

    -- ** Response lenses
    dsdrsSharedDirectories,
    dsdrsNextToken,
    dsdrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSharedDirectories' smart constructor.
data DescribeSharedDirectories = DescribeSharedDirectories'
  { -- | A list of identifiers of all shared directories in your account.
    sharedDirectoryIds :: Lude.Maybe [Lude.Text],
    -- | The @DescribeSharedDirectoriesResult.NextToken@ value from a previous call to 'DescribeSharedDirectories' . Pass null if this is the first call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The number of shared directories to return in the response object.
    limit :: Lude.Maybe Lude.Natural,
    -- | Returns the identifier of the directory in the directory owner account.
    ownerDirectoryId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSharedDirectories' with the minimum fields required to make a request.
--
-- * 'sharedDirectoryIds' - A list of identifiers of all shared directories in your account.
-- * 'nextToken' - The @DescribeSharedDirectoriesResult.NextToken@ value from a previous call to 'DescribeSharedDirectories' . Pass null if this is the first call.
-- * 'limit' - The number of shared directories to return in the response object.
-- * 'ownerDirectoryId' - Returns the identifier of the directory in the directory owner account.
mkDescribeSharedDirectories ::
  -- | 'ownerDirectoryId'
  Lude.Text ->
  DescribeSharedDirectories
mkDescribeSharedDirectories pOwnerDirectoryId_ =
  DescribeSharedDirectories'
    { sharedDirectoryIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      ownerDirectoryId = pOwnerDirectoryId_
    }

-- | A list of identifiers of all shared directories in your account.
--
-- /Note:/ Consider using 'sharedDirectoryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdSharedDirectoryIds :: Lens.Lens' DescribeSharedDirectories (Lude.Maybe [Lude.Text])
dsdSharedDirectoryIds = Lens.lens (sharedDirectoryIds :: DescribeSharedDirectories -> Lude.Maybe [Lude.Text]) (\s a -> s {sharedDirectoryIds = a} :: DescribeSharedDirectories)
{-# DEPRECATED dsdSharedDirectoryIds "Use generic-lens or generic-optics with 'sharedDirectoryIds' instead." #-}

-- | The @DescribeSharedDirectoriesResult.NextToken@ value from a previous call to 'DescribeSharedDirectories' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdNextToken :: Lens.Lens' DescribeSharedDirectories (Lude.Maybe Lude.Text)
dsdNextToken = Lens.lens (nextToken :: DescribeSharedDirectories -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSharedDirectories)
{-# DEPRECATED dsdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of shared directories to return in the response object.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdLimit :: Lens.Lens' DescribeSharedDirectories (Lude.Maybe Lude.Natural)
dsdLimit = Lens.lens (limit :: DescribeSharedDirectories -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeSharedDirectories)
{-# DEPRECATED dsdLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Returns the identifier of the directory in the directory owner account.
--
-- /Note:/ Consider using 'ownerDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdOwnerDirectoryId :: Lens.Lens' DescribeSharedDirectories Lude.Text
dsdOwnerDirectoryId = Lens.lens (ownerDirectoryId :: DescribeSharedDirectories -> Lude.Text) (\s a -> s {ownerDirectoryId = a} :: DescribeSharedDirectories)
{-# DEPRECATED dsdOwnerDirectoryId "Use generic-lens or generic-optics with 'ownerDirectoryId' instead." #-}

instance Page.AWSPager DescribeSharedDirectories where
  page rq rs
    | Page.stop (rs Lens.^. dsdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsdrsSharedDirectories) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsdNextToken Lens..~ rs Lens.^. dsdrsNextToken

instance Lude.AWSRequest DescribeSharedDirectories where
  type
    Rs DescribeSharedDirectories =
      DescribeSharedDirectoriesResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSharedDirectoriesResponse'
            Lude.<$> (x Lude..?> "SharedDirectories" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSharedDirectories where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DescribeSharedDirectories" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSharedDirectories where
  toJSON DescribeSharedDirectories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SharedDirectoryIds" Lude..=) Lude.<$> sharedDirectoryIds,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("OwnerDirectoryId" Lude..= ownerDirectoryId)
          ]
      )

instance Lude.ToPath DescribeSharedDirectories where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSharedDirectories where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSharedDirectoriesResponse' smart constructor.
data DescribeSharedDirectoriesResponse = DescribeSharedDirectoriesResponse'
  { -- | A list of all shared directories in your account.
    sharedDirectories :: Lude.Maybe [SharedDirectory],
    -- | If not null, token that indicates that more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeSharedDirectories' to retrieve the next set of items.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSharedDirectoriesResponse' with the minimum fields required to make a request.
--
-- * 'sharedDirectories' - A list of all shared directories in your account.
-- * 'nextToken' - If not null, token that indicates that more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeSharedDirectories' to retrieve the next set of items.
-- * 'responseStatus' - The response status code.
mkDescribeSharedDirectoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSharedDirectoriesResponse
mkDescribeSharedDirectoriesResponse pResponseStatus_ =
  DescribeSharedDirectoriesResponse'
    { sharedDirectories =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of all shared directories in your account.
--
-- /Note:/ Consider using 'sharedDirectories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrsSharedDirectories :: Lens.Lens' DescribeSharedDirectoriesResponse (Lude.Maybe [SharedDirectory])
dsdrsSharedDirectories = Lens.lens (sharedDirectories :: DescribeSharedDirectoriesResponse -> Lude.Maybe [SharedDirectory]) (\s a -> s {sharedDirectories = a} :: DescribeSharedDirectoriesResponse)
{-# DEPRECATED dsdrsSharedDirectories "Use generic-lens or generic-optics with 'sharedDirectories' instead." #-}

-- | If not null, token that indicates that more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeSharedDirectories' to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrsNextToken :: Lens.Lens' DescribeSharedDirectoriesResponse (Lude.Maybe Lude.Text)
dsdrsNextToken = Lens.lens (nextToken :: DescribeSharedDirectoriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSharedDirectoriesResponse)
{-# DEPRECATED dsdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrsResponseStatus :: Lens.Lens' DescribeSharedDirectoriesResponse Lude.Int
dsdrsResponseStatus = Lens.lens (responseStatus :: DescribeSharedDirectoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSharedDirectoriesResponse)
{-# DEPRECATED dsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
