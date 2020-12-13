{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeDirectories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the directories that belong to this account.
--
-- You can retrieve information about specific directories by passing the directory identifiers in the @DirectoryIds@ parameter. Otherwise, all directories that belong to the current account are returned.
-- This operation supports pagination with the use of the @NextToken@ request and response parameters. If more results are available, the @DescribeDirectoriesResult.NextToken@ member contains a token that you pass in the next call to 'DescribeDirectories' to retrieve the next set of items.
-- You can also specify a maximum number of return results with the @Limit@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeDirectories
  ( -- * Creating a request
    DescribeDirectories (..),
    mkDescribeDirectories,

    -- ** Request lenses
    ddNextToken,
    ddDirectoryIds,
    ddLimit,

    -- * Destructuring the response
    DescribeDirectoriesResponse (..),
    mkDescribeDirectoriesResponse,

    -- ** Response lenses
    ddrsDirectoryDescriptions,
    ddrsNextToken,
    ddrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'DescribeDirectories' operation.
--
-- /See:/ 'mkDescribeDirectories' smart constructor.
data DescribeDirectories = DescribeDirectories'
  { -- | The @DescribeDirectoriesResult.NextToken@ value from a previous call to 'DescribeDirectories' . Pass null if this is the first call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of identifiers of the directories for which to obtain the information. If this member is null, all directories that belong to the current account are returned.
    --
    -- An empty list results in an @InvalidParameterException@ being thrown.
    directoryIds :: Lude.Maybe [Lude.Text],
    -- | The maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectories' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @DescribeDirectoriesResult.NextToken@ value from a previous call to 'DescribeDirectories' . Pass null if this is the first call.
-- * 'directoryIds' - A list of identifiers of the directories for which to obtain the information. If this member is null, all directories that belong to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
-- * 'limit' - The maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
mkDescribeDirectories ::
  DescribeDirectories
mkDescribeDirectories =
  DescribeDirectories'
    { nextToken = Lude.Nothing,
      directoryIds = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The @DescribeDirectoriesResult.NextToken@ value from a previous call to 'DescribeDirectories' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddNextToken :: Lens.Lens' DescribeDirectories (Lude.Maybe Lude.Text)
ddNextToken = Lens.lens (nextToken :: DescribeDirectories -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectories)
{-# DEPRECATED ddNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of identifiers of the directories for which to obtain the information. If this member is null, all directories that belong to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
--
-- /Note:/ Consider using 'directoryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDirectoryIds :: Lens.Lens' DescribeDirectories (Lude.Maybe [Lude.Text])
ddDirectoryIds = Lens.lens (directoryIds :: DescribeDirectories -> Lude.Maybe [Lude.Text]) (\s a -> s {directoryIds = a} :: DescribeDirectories)
{-# DEPRECATED ddDirectoryIds "Use generic-lens or generic-optics with 'directoryIds' instead." #-}

-- | The maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddLimit :: Lens.Lens' DescribeDirectories (Lude.Maybe Lude.Natural)
ddLimit = Lens.lens (limit :: DescribeDirectories -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeDirectories)
{-# DEPRECATED ddLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeDirectories where
  page rq rs
    | Page.stop (rs Lens.^. ddrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ddrsDirectoryDescriptions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddNextToken Lens..~ rs Lens.^. ddrsNextToken

instance Lude.AWSRequest DescribeDirectories where
  type Rs DescribeDirectories = DescribeDirectoriesResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDirectoriesResponse'
            Lude.<$> (x Lude..?> "DirectoryDescriptions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDirectories where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DescribeDirectories" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDirectories where
  toJSON DescribeDirectories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("DirectoryIds" Lude..=) Lude.<$> directoryIds,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeDirectories where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDirectories where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'DescribeDirectories' operation.
--
-- /See:/ 'mkDescribeDirectoriesResponse' smart constructor.
data DescribeDirectoriesResponse = DescribeDirectoriesResponse'
  { -- | The list of 'DirectoryDescription' objects that were retrieved.
    --
    -- It is possible that this list contains less than the number of items specified in the @Limit@ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
    directoryDescriptions :: Lude.Maybe [DirectoryDescription],
    -- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeDirectories' to retrieve the next set of items.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectoriesResponse' with the minimum fields required to make a request.
--
-- * 'directoryDescriptions' - The list of 'DirectoryDescription' objects that were retrieved.
--
-- It is possible that this list contains less than the number of items specified in the @Limit@ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
-- * 'nextToken' - If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeDirectories' to retrieve the next set of items.
-- * 'responseStatus' - The response status code.
mkDescribeDirectoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDirectoriesResponse
mkDescribeDirectoriesResponse pResponseStatus_ =
  DescribeDirectoriesResponse'
    { directoryDescriptions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of 'DirectoryDescription' objects that were retrieved.
--
-- It is possible that this list contains less than the number of items specified in the @Limit@ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
--
-- /Note:/ Consider using 'directoryDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDirectoryDescriptions :: Lens.Lens' DescribeDirectoriesResponse (Lude.Maybe [DirectoryDescription])
ddrsDirectoryDescriptions = Lens.lens (directoryDescriptions :: DescribeDirectoriesResponse -> Lude.Maybe [DirectoryDescription]) (\s a -> s {directoryDescriptions = a} :: DescribeDirectoriesResponse)
{-# DEPRECATED ddrsDirectoryDescriptions "Use generic-lens or generic-optics with 'directoryDescriptions' instead." #-}

-- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeDirectories' to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsNextToken :: Lens.Lens' DescribeDirectoriesResponse (Lude.Maybe Lude.Text)
ddrsNextToken = Lens.lens (nextToken :: DescribeDirectoriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectoriesResponse)
{-# DEPRECATED ddrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DescribeDirectoriesResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DescribeDirectoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDirectoriesResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
