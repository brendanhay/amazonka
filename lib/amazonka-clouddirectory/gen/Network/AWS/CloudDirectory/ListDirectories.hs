{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListDirectories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists directories created within an account.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListDirectories
  ( -- * Creating a request
    ListDirectories (..),
    mkListDirectories,

    -- ** Request lenses
    ldState,
    ldNextToken,
    ldMaxResults,

    -- * Destructuring the response
    ListDirectoriesResponse (..),
    mkListDirectoriesResponse,

    -- ** Response lenses
    ldrsDirectories,
    ldrsNextToken,
    ldrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDirectories' smart constructor.
data ListDirectories = ListDirectories'
  { -- | The state of the directories in the list. Can be either Enabled, Disabled, or Deleted.
    state :: Lude.Maybe DirectoryState,
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDirectories' with the minimum fields required to make a request.
--
-- * 'state' - The state of the directories in the list. Can be either Enabled, Disabled, or Deleted.
-- * 'nextToken' - The pagination token.
-- * 'maxResults' - The maximum number of results to retrieve.
mkListDirectories ::
  ListDirectories
mkListDirectories =
  ListDirectories'
    { state = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The state of the directories in the list. Can be either Enabled, Disabled, or Deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldState :: Lens.Lens' ListDirectories (Lude.Maybe DirectoryState)
ldState = Lens.lens (state :: ListDirectories -> Lude.Maybe DirectoryState) (\s a -> s {state = a} :: ListDirectories)
{-# DEPRECATED ldState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDirectories (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDirectories -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDirectories)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDirectories (Lude.Maybe Lude.Natural)
ldMaxResults = Lens.lens (maxResults :: ListDirectories -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDirectories)
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDirectories where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDirectories) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDirectories where
  type Rs ListDirectories = ListDirectoriesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDirectoriesResponse'
            Lude.<$> (x Lude..?> "Directories" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDirectories where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListDirectories where
  toJSON ListDirectories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("state" Lude..=) Lude.<$> state,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDirectories where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/directory/list"

instance Lude.ToQuery ListDirectories where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDirectoriesResponse' smart constructor.
data ListDirectoriesResponse = ListDirectoriesResponse'
  { -- | Lists all directories that are associated with your account in pagination fashion.
    directories :: [Directory],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDirectoriesResponse' with the minimum fields required to make a request.
--
-- * 'directories' - Lists all directories that are associated with your account in pagination fashion.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListDirectoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDirectoriesResponse
mkListDirectoriesResponse pResponseStatus_ =
  ListDirectoriesResponse'
    { directories = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Lists all directories that are associated with your account in pagination fashion.
--
-- /Note:/ Consider using 'directories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDirectories :: Lens.Lens' ListDirectoriesResponse [Directory]
ldrsDirectories = Lens.lens (directories :: ListDirectoriesResponse -> [Directory]) (\s a -> s {directories = a} :: ListDirectoriesResponse)
{-# DEPRECATED ldrsDirectories "Use generic-lens or generic-optics with 'directories' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDirectoriesResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDirectoriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDirectoriesResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDirectoriesResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDirectoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDirectoriesResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
