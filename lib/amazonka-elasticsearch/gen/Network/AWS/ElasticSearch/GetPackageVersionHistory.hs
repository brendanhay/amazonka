{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.GetPackageVersionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of versions of the package, along with their creation time and commit message.
module Network.AWS.ElasticSearch.GetPackageVersionHistory
  ( -- * Creating a request
    GetPackageVersionHistory (..),
    mkGetPackageVersionHistory,

    -- ** Request lenses
    gpvhPackageId,
    gpvhNextToken,
    gpvhMaxResults,

    -- * Destructuring the response
    GetPackageVersionHistoryResponse (..),
    mkGetPackageVersionHistoryResponse,

    -- ** Response lenses
    gpvhrsPackageId,
    gpvhrsPackageVersionHistoryList,
    gpvhrsNextToken,
    gpvhrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'GetPackageVersionHistory' @ operation.
--
-- /See:/ 'mkGetPackageVersionHistory' smart constructor.
data GetPackageVersionHistory = GetPackageVersionHistory'
  { -- | Returns an audit history of versions of the package.
    packageId :: Lude.Text,
    -- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Limits results to a maximum number of versions.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPackageVersionHistory' with the minimum fields required to make a request.
--
-- * 'packageId' - Returns an audit history of versions of the package.
-- * 'nextToken' - Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
-- * 'maxResults' - Limits results to a maximum number of versions.
mkGetPackageVersionHistory ::
  -- | 'packageId'
  Lude.Text ->
  GetPackageVersionHistory
mkGetPackageVersionHistory pPackageId_ =
  GetPackageVersionHistory'
    { packageId = pPackageId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Returns an audit history of versions of the package.
--
-- /Note:/ Consider using 'packageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhPackageId :: Lens.Lens' GetPackageVersionHistory Lude.Text
gpvhPackageId = Lens.lens (packageId :: GetPackageVersionHistory -> Lude.Text) (\s a -> s {packageId = a} :: GetPackageVersionHistory)
{-# DEPRECATED gpvhPackageId "Use generic-lens or generic-optics with 'packageId' instead." #-}

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhNextToken :: Lens.Lens' GetPackageVersionHistory (Lude.Maybe Lude.Text)
gpvhNextToken = Lens.lens (nextToken :: GetPackageVersionHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetPackageVersionHistory)
{-# DEPRECATED gpvhNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Limits results to a maximum number of versions.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhMaxResults :: Lens.Lens' GetPackageVersionHistory (Lude.Maybe Lude.Int)
gpvhMaxResults = Lens.lens (maxResults :: GetPackageVersionHistory -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetPackageVersionHistory)
{-# DEPRECATED gpvhMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest GetPackageVersionHistory where
  type Rs GetPackageVersionHistory = GetPackageVersionHistoryResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPackageVersionHistoryResponse'
            Lude.<$> (x Lude..?> "PackageID")
            Lude.<*> (x Lude..?> "PackageVersionHistoryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPackageVersionHistory where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPackageVersionHistory where
  toPath GetPackageVersionHistory' {..} =
    Lude.mconcat
      ["/2015-01-01/packages/", Lude.toBS packageId, "/history"]

instance Lude.ToQuery GetPackageVersionHistory where
  toQuery GetPackageVersionHistory' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Container for response returned by @'GetPackageVersionHistory' @ operation.
--
-- /See:/ 'mkGetPackageVersionHistoryResponse' smart constructor.
data GetPackageVersionHistoryResponse = GetPackageVersionHistoryResponse'
  { packageId :: Lude.Maybe Lude.Text,
    -- | List of @PackageVersionHistory@ objects.
    packageVersionHistoryList :: Lude.Maybe [PackageVersionHistory],
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPackageVersionHistoryResponse' with the minimum fields required to make a request.
--
-- * 'packageId' -
-- * 'packageVersionHistoryList' - List of @PackageVersionHistory@ objects.
-- * 'nextToken' -
-- * 'responseStatus' - The response status code.
mkGetPackageVersionHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPackageVersionHistoryResponse
mkGetPackageVersionHistoryResponse pResponseStatus_ =
  GetPackageVersionHistoryResponse'
    { packageId = Lude.Nothing,
      packageVersionHistoryList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'packageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrsPackageId :: Lens.Lens' GetPackageVersionHistoryResponse (Lude.Maybe Lude.Text)
gpvhrsPackageId = Lens.lens (packageId :: GetPackageVersionHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {packageId = a} :: GetPackageVersionHistoryResponse)
{-# DEPRECATED gpvhrsPackageId "Use generic-lens or generic-optics with 'packageId' instead." #-}

-- | List of @PackageVersionHistory@ objects.
--
-- /Note:/ Consider using 'packageVersionHistoryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrsPackageVersionHistoryList :: Lens.Lens' GetPackageVersionHistoryResponse (Lude.Maybe [PackageVersionHistory])
gpvhrsPackageVersionHistoryList = Lens.lens (packageVersionHistoryList :: GetPackageVersionHistoryResponse -> Lude.Maybe [PackageVersionHistory]) (\s a -> s {packageVersionHistoryList = a} :: GetPackageVersionHistoryResponse)
{-# DEPRECATED gpvhrsPackageVersionHistoryList "Use generic-lens or generic-optics with 'packageVersionHistoryList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrsNextToken :: Lens.Lens' GetPackageVersionHistoryResponse (Lude.Maybe Lude.Text)
gpvhrsNextToken = Lens.lens (nextToken :: GetPackageVersionHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetPackageVersionHistoryResponse)
{-# DEPRECATED gpvhrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrsResponseStatus :: Lens.Lens' GetPackageVersionHistoryResponse Lude.Int
gpvhrsResponseStatus = Lens.lens (responseStatus :: GetPackageVersionHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPackageVersionHistoryResponse)
{-# DEPRECATED gpvhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
