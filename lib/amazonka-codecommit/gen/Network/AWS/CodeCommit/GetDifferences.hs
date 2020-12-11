{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetDifferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID, or other fully qualified reference). Results can be limited to a specified path.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetDifferences
  ( -- * Creating a request
    GetDifferences (..),
    mkGetDifferences,

    -- ** Request lenses
    gdAfterPath,
    gdNextToken,
    gdBeforeCommitSpecifier,
    gdBeforePath,
    gdMaxResults,
    gdRepositoryName,
    gdAfterCommitSpecifier,

    -- * Destructuring the response
    GetDifferencesResponse (..),
    mkGetDifferencesResponse,

    -- ** Response lenses
    gdrsNextToken,
    gdrsDifferences,
    gdrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDifferences' smart constructor.
data GetDifferences = GetDifferences'
  { afterPath ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    beforeCommitSpecifier :: Lude.Maybe Lude.Text,
    beforePath :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    repositoryName :: Lude.Text,
    afterCommitSpecifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDifferences' with the minimum fields required to make a request.
--
-- * 'afterCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit.
-- * 'afterPath' - The file path in which to check differences. Limits the results to this path. Can also be used to specify the changed name of a directory or folder, if it has changed. If not specified, differences are shown for all paths.
-- * 'beforeCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, the full commit ID). Optional. If not specified, all changes before the @afterCommitSpecifier@ value are shown. If you do not use @beforeCommitSpecifier@ in your request, consider limiting the results with @maxResults@ .
-- * 'beforePath' - The file path in which to check for differences. Limits the results to this path. Can also be used to specify the previous name of a directory or folder. If @beforePath@ and @afterPath@ are not specified, differences are shown for all paths.
-- * 'maxResults' - A non-zero, non-negative integer used to limit the number of returned results.
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'repositoryName' - The name of the repository where you want to get differences.
mkGetDifferences ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'afterCommitSpecifier'
  Lude.Text ->
  GetDifferences
mkGetDifferences pRepositoryName_ pAfterCommitSpecifier_ =
  GetDifferences'
    { afterPath = Lude.Nothing,
      nextToken = Lude.Nothing,
      beforeCommitSpecifier = Lude.Nothing,
      beforePath = Lude.Nothing,
      maxResults = Lude.Nothing,
      repositoryName = pRepositoryName_,
      afterCommitSpecifier = pAfterCommitSpecifier_
    }

-- | The file path in which to check differences. Limits the results to this path. Can also be used to specify the changed name of a directory or folder, if it has changed. If not specified, differences are shown for all paths.
--
-- /Note:/ Consider using 'afterPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAfterPath :: Lens.Lens' GetDifferences (Lude.Maybe Lude.Text)
gdAfterPath = Lens.lens (afterPath :: GetDifferences -> Lude.Maybe Lude.Text) (\s a -> s {afterPath = a} :: GetDifferences)
{-# DEPRECATED gdAfterPath "Use generic-lens or generic-optics with 'afterPath' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdNextToken :: Lens.Lens' GetDifferences (Lude.Maybe Lude.Text)
gdNextToken = Lens.lens (nextToken :: GetDifferences -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetDifferences)
{-# DEPRECATED gdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, the full commit ID). Optional. If not specified, all changes before the @afterCommitSpecifier@ value are shown. If you do not use @beforeCommitSpecifier@ in your request, consider limiting the results with @maxResults@ .
--
-- /Note:/ Consider using 'beforeCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdBeforeCommitSpecifier :: Lens.Lens' GetDifferences (Lude.Maybe Lude.Text)
gdBeforeCommitSpecifier = Lens.lens (beforeCommitSpecifier :: GetDifferences -> Lude.Maybe Lude.Text) (\s a -> s {beforeCommitSpecifier = a} :: GetDifferences)
{-# DEPRECATED gdBeforeCommitSpecifier "Use generic-lens or generic-optics with 'beforeCommitSpecifier' instead." #-}

-- | The file path in which to check for differences. Limits the results to this path. Can also be used to specify the previous name of a directory or folder. If @beforePath@ and @afterPath@ are not specified, differences are shown for all paths.
--
-- /Note:/ Consider using 'beforePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdBeforePath :: Lens.Lens' GetDifferences (Lude.Maybe Lude.Text)
gdBeforePath = Lens.lens (beforePath :: GetDifferences -> Lude.Maybe Lude.Text) (\s a -> s {beforePath = a} :: GetDifferences)
{-# DEPRECATED gdBeforePath "Use generic-lens or generic-optics with 'beforePath' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdMaxResults :: Lens.Lens' GetDifferences (Lude.Maybe Lude.Int)
gdMaxResults = Lens.lens (maxResults :: GetDifferences -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetDifferences)
{-# DEPRECATED gdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the repository where you want to get differences.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdRepositoryName :: Lens.Lens' GetDifferences Lude.Text
gdRepositoryName = Lens.lens (repositoryName :: GetDifferences -> Lude.Text) (\s a -> s {repositoryName = a} :: GetDifferences)
{-# DEPRECATED gdRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit.
--
-- /Note:/ Consider using 'afterCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAfterCommitSpecifier :: Lens.Lens' GetDifferences Lude.Text
gdAfterCommitSpecifier = Lens.lens (afterCommitSpecifier :: GetDifferences -> Lude.Text) (\s a -> s {afterCommitSpecifier = a} :: GetDifferences)
{-# DEPRECATED gdAfterCommitSpecifier "Use generic-lens or generic-optics with 'afterCommitSpecifier' instead." #-}

instance Page.AWSPager GetDifferences where
  page rq rs
    | Page.stop (rs Lens.^. gdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gdrsDifferences) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdNextToken Lens..~ rs Lens.^. gdrsNextToken

instance Lude.AWSRequest GetDifferences where
  type Rs GetDifferences = GetDifferencesResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDifferencesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "differences" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDifferences where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetDifferences" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDifferences where
  toJSON GetDifferences' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("afterPath" Lude..=) Lude.<$> afterPath,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("beforeCommitSpecifier" Lude..=) Lude.<$> beforeCommitSpecifier,
            ("beforePath" Lude..=) Lude.<$> beforePath,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("afterCommitSpecifier" Lude..= afterCommitSpecifier)
          ]
      )

instance Lude.ToPath GetDifferences where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDifferences where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDifferencesResponse' smart constructor.
data GetDifferencesResponse = GetDifferencesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    differences :: Lude.Maybe [Difference],
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

-- | Creates a value of 'GetDifferencesResponse' with the minimum fields required to make a request.
--
-- * 'differences' - A data type object that contains information about the differences, including whether the difference is added, modified, or deleted (A, D, M).
-- * 'nextToken' - An enumeration token that can be used in a request to return the next batch of the results.
-- * 'responseStatus' - The response status code.
mkGetDifferencesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDifferencesResponse
mkGetDifferencesResponse pResponseStatus_ =
  GetDifferencesResponse'
    { nextToken = Lude.Nothing,
      differences = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsNextToken :: Lens.Lens' GetDifferencesResponse (Lude.Maybe Lude.Text)
gdrsNextToken = Lens.lens (nextToken :: GetDifferencesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetDifferencesResponse)
{-# DEPRECATED gdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A data type object that contains information about the differences, including whether the difference is added, modified, or deleted (A, D, M).
--
-- /Note:/ Consider using 'differences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDifferences :: Lens.Lens' GetDifferencesResponse (Lude.Maybe [Difference])
gdrsDifferences = Lens.lens (differences :: GetDifferencesResponse -> Lude.Maybe [Difference]) (\s a -> s {differences = a} :: GetDifferencesResponse)
{-# DEPRECATED gdrsDifferences "Use generic-lens or generic-optics with 'differences' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDifferencesResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDifferencesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDifferencesResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
