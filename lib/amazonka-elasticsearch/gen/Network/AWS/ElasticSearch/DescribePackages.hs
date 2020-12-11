{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribePackages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all packages available to Amazon ES. Includes options for filtering, limiting the number of results, and pagination.
module Network.AWS.ElasticSearch.DescribePackages
  ( -- * Creating a request
    DescribePackages (..),
    mkDescribePackages,

    -- ** Request lenses
    dpFilters,
    dpNextToken,
    dpMaxResults,

    -- * Destructuring the response
    DescribePackagesResponse (..),
    mkDescribePackagesResponse,

    -- ** Response lenses
    dprsPackageDetailsList,
    dprsNextToken,
    dprsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'DescribePackage' @ operation.
--
-- /See:/ 'mkDescribePackages' smart constructor.
data DescribePackages = DescribePackages'
  { filters ::
      Lude.Maybe [DescribePackagesFilter],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePackages' with the minimum fields required to make a request.
--
-- * 'filters' - Only returns packages that match the @DescribePackagesFilterList@ values.
-- * 'maxResults' - Limits results to a maximum number of packages.
-- * 'nextToken' - Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
mkDescribePackages ::
  DescribePackages
mkDescribePackages =
  DescribePackages'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Only returns packages that match the @DescribePackagesFilterList@ values.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpFilters :: Lens.Lens' DescribePackages (Lude.Maybe [DescribePackagesFilter])
dpFilters = Lens.lens (filters :: DescribePackages -> Lude.Maybe [DescribePackagesFilter]) (\s a -> s {filters = a} :: DescribePackages)
{-# DEPRECATED dpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpNextToken :: Lens.Lens' DescribePackages (Lude.Maybe Lude.Text)
dpNextToken = Lens.lens (nextToken :: DescribePackages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePackages)
{-# DEPRECATED dpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Limits results to a maximum number of packages.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxResults :: Lens.Lens' DescribePackages (Lude.Maybe Lude.Int)
dpMaxResults = Lens.lens (maxResults :: DescribePackages -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribePackages)
{-# DEPRECATED dpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribePackages where
  type Rs DescribePackages = DescribePackagesResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePackagesResponse'
            Lude.<$> (x Lude..?> "PackageDetailsList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePackages where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DescribePackages where
  toJSON DescribePackages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribePackages where
  toPath = Lude.const "/2015-01-01/packages/describe"

instance Lude.ToQuery DescribePackages where
  toQuery = Lude.const Lude.mempty

-- | Container for response returned by @'DescribePackages' @ operation.
--
-- /See:/ 'mkDescribePackagesResponse' smart constructor.
data DescribePackagesResponse = DescribePackagesResponse'
  { packageDetailsList ::
      Lude.Maybe [PackageDetails],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribePackagesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Undocumented field.
-- * 'packageDetailsList' - List of @PackageDetails@ objects.
-- * 'responseStatus' - The response status code.
mkDescribePackagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePackagesResponse
mkDescribePackagesResponse pResponseStatus_ =
  DescribePackagesResponse'
    { packageDetailsList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of @PackageDetails@ objects.
--
-- /Note:/ Consider using 'packageDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsPackageDetailsList :: Lens.Lens' DescribePackagesResponse (Lude.Maybe [PackageDetails])
dprsPackageDetailsList = Lens.lens (packageDetailsList :: DescribePackagesResponse -> Lude.Maybe [PackageDetails]) (\s a -> s {packageDetailsList = a} :: DescribePackagesResponse)
{-# DEPRECATED dprsPackageDetailsList "Use generic-lens or generic-optics with 'packageDetailsList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsNextToken :: Lens.Lens' DescribePackagesResponse (Lude.Maybe Lude.Text)
dprsNextToken = Lens.lens (nextToken :: DescribePackagesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePackagesResponse)
{-# DEPRECATED dprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DescribePackagesResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DescribePackagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePackagesResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
