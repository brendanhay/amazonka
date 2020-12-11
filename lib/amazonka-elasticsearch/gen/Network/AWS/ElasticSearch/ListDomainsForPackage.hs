{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.ListDomainsForPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Amazon ES domains associated with the package.
module Network.AWS.ElasticSearch.ListDomainsForPackage
  ( -- * Creating a request
    ListDomainsForPackage (..),
    mkListDomainsForPackage,

    -- ** Request lenses
    ldfpNextToken,
    ldfpMaxResults,
    ldfpPackageId,

    -- * Destructuring the response
    ListDomainsForPackageResponse (..),
    mkListDomainsForPackageResponse,

    -- ** Response lenses
    ldfprsDomainPackageDetailsList,
    ldfprsNextToken,
    ldfprsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'ListDomainsForPackage' @ operation.
--
-- /See:/ 'mkListDomainsForPackage' smart constructor.
data ListDomainsForPackage = ListDomainsForPackage'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    packageId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomainsForPackage' with the minimum fields required to make a request.
--
-- * 'maxResults' - Limits results to a maximum number of domains.
-- * 'nextToken' - Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
-- * 'packageId' - The package for which to list domains.
mkListDomainsForPackage ::
  -- | 'packageId'
  Lude.Text ->
  ListDomainsForPackage
mkListDomainsForPackage pPackageId_ =
  ListDomainsForPackage'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      packageId = pPackageId_
    }

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfpNextToken :: Lens.Lens' ListDomainsForPackage (Lude.Maybe Lude.Text)
ldfpNextToken = Lens.lens (nextToken :: ListDomainsForPackage -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDomainsForPackage)
{-# DEPRECATED ldfpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Limits results to a maximum number of domains.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfpMaxResults :: Lens.Lens' ListDomainsForPackage (Lude.Maybe Lude.Int)
ldfpMaxResults = Lens.lens (maxResults :: ListDomainsForPackage -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListDomainsForPackage)
{-# DEPRECATED ldfpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The package for which to list domains.
--
-- /Note:/ Consider using 'packageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfpPackageId :: Lens.Lens' ListDomainsForPackage Lude.Text
ldfpPackageId = Lens.lens (packageId :: ListDomainsForPackage -> Lude.Text) (\s a -> s {packageId = a} :: ListDomainsForPackage)
{-# DEPRECATED ldfpPackageId "Use generic-lens or generic-optics with 'packageId' instead." #-}

instance Lude.AWSRequest ListDomainsForPackage where
  type Rs ListDomainsForPackage = ListDomainsForPackageResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDomainsForPackageResponse'
            Lude.<$> (x Lude..?> "DomainPackageDetailsList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDomainsForPackage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDomainsForPackage where
  toPath ListDomainsForPackage' {..} =
    Lude.mconcat
      ["/2015-01-01/packages/", Lude.toBS packageId, "/domains"]

instance Lude.ToQuery ListDomainsForPackage where
  toQuery ListDomainsForPackage' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Container for response parameters to @'ListDomainsForPackage' @ operation.
--
-- /See:/ 'mkListDomainsForPackageResponse' smart constructor.
data ListDomainsForPackageResponse = ListDomainsForPackageResponse'
  { domainPackageDetailsList ::
      Lude.Maybe
        [DomainPackageDetails],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListDomainsForPackageResponse' with the minimum fields required to make a request.
--
-- * 'domainPackageDetailsList' - List of @DomainPackageDetails@ objects.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkListDomainsForPackageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDomainsForPackageResponse
mkListDomainsForPackageResponse pResponseStatus_ =
  ListDomainsForPackageResponse'
    { domainPackageDetailsList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of @DomainPackageDetails@ objects.
--
-- /Note:/ Consider using 'domainPackageDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfprsDomainPackageDetailsList :: Lens.Lens' ListDomainsForPackageResponse (Lude.Maybe [DomainPackageDetails])
ldfprsDomainPackageDetailsList = Lens.lens (domainPackageDetailsList :: ListDomainsForPackageResponse -> Lude.Maybe [DomainPackageDetails]) (\s a -> s {domainPackageDetailsList = a} :: ListDomainsForPackageResponse)
{-# DEPRECATED ldfprsDomainPackageDetailsList "Use generic-lens or generic-optics with 'domainPackageDetailsList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfprsNextToken :: Lens.Lens' ListDomainsForPackageResponse (Lude.Maybe Lude.Text)
ldfprsNextToken = Lens.lens (nextToken :: ListDomainsForPackageResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDomainsForPackageResponse)
{-# DEPRECATED ldfprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfprsResponseStatus :: Lens.Lens' ListDomainsForPackageResponse Lude.Int
ldfprsResponseStatus = Lens.lens (responseStatus :: ListDomainsForPackageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDomainsForPackageResponse)
{-# DEPRECATED ldfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
