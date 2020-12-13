{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.ListPackagesForDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all packages associated with the Amazon ES domain.
module Network.AWS.ElasticSearch.ListPackagesForDomain
  ( -- * Creating a request
    ListPackagesForDomain (..),
    mkListPackagesForDomain,

    -- ** Request lenses
    lpfdNextToken,
    lpfdDomainName,
    lpfdMaxResults,

    -- * Destructuring the response
    ListPackagesForDomainResponse (..),
    mkListPackagesForDomainResponse,

    -- ** Response lenses
    lpfdrsDomainPackageDetailsList,
    lpfdrsNextToken,
    lpfdrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'ListPackagesForDomain' @ operation.
--
-- /See:/ 'mkListPackagesForDomain' smart constructor.
data ListPackagesForDomain = ListPackagesForDomain'
  { -- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the domain for which you want to list associated packages.
    domainName :: Lude.Text,
    -- | Limits results to a maximum number of packages.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPackagesForDomain' with the minimum fields required to make a request.
--
-- * 'nextToken' - Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
-- * 'domainName' - The name of the domain for which you want to list associated packages.
-- * 'maxResults' - Limits results to a maximum number of packages.
mkListPackagesForDomain ::
  -- | 'domainName'
  Lude.Text ->
  ListPackagesForDomain
mkListPackagesForDomain pDomainName_ =
  ListPackagesForDomain'
    { nextToken = Lude.Nothing,
      domainName = pDomainName_,
      maxResults = Lude.Nothing
    }

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdNextToken :: Lens.Lens' ListPackagesForDomain (Lude.Maybe Lude.Text)
lpfdNextToken = Lens.lens (nextToken :: ListPackagesForDomain -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPackagesForDomain)
{-# DEPRECATED lpfdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the domain for which you want to list associated packages.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdDomainName :: Lens.Lens' ListPackagesForDomain Lude.Text
lpfdDomainName = Lens.lens (domainName :: ListPackagesForDomain -> Lude.Text) (\s a -> s {domainName = a} :: ListPackagesForDomain)
{-# DEPRECATED lpfdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Limits results to a maximum number of packages.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdMaxResults :: Lens.Lens' ListPackagesForDomain (Lude.Maybe Lude.Int)
lpfdMaxResults = Lens.lens (maxResults :: ListPackagesForDomain -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListPackagesForDomain)
{-# DEPRECATED lpfdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListPackagesForDomain where
  type Rs ListPackagesForDomain = ListPackagesForDomainResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPackagesForDomainResponse'
            Lude.<$> (x Lude..?> "DomainPackageDetailsList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPackagesForDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPackagesForDomain where
  toPath ListPackagesForDomain' {..} =
    Lude.mconcat
      ["/2015-01-01/domain/", Lude.toBS domainName, "/packages"]

instance Lude.ToQuery ListPackagesForDomain where
  toQuery ListPackagesForDomain' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Container for response parameters to @'ListPackagesForDomain' @ operation.
--
-- /See:/ 'mkListPackagesForDomainResponse' smart constructor.
data ListPackagesForDomainResponse = ListPackagesForDomainResponse'
  { -- | List of @DomainPackageDetails@ objects.
    domainPackageDetailsList :: Lude.Maybe [DomainPackageDetails],
    -- | Pagination token that needs to be supplied to the next call to get the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPackagesForDomainResponse' with the minimum fields required to make a request.
--
-- * 'domainPackageDetailsList' - List of @DomainPackageDetails@ objects.
-- * 'nextToken' - Pagination token that needs to be supplied to the next call to get the next page of results.
-- * 'responseStatus' - The response status code.
mkListPackagesForDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPackagesForDomainResponse
mkListPackagesForDomainResponse pResponseStatus_ =
  ListPackagesForDomainResponse'
    { domainPackageDetailsList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of @DomainPackageDetails@ objects.
--
-- /Note:/ Consider using 'domainPackageDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdrsDomainPackageDetailsList :: Lens.Lens' ListPackagesForDomainResponse (Lude.Maybe [DomainPackageDetails])
lpfdrsDomainPackageDetailsList = Lens.lens (domainPackageDetailsList :: ListPackagesForDomainResponse -> Lude.Maybe [DomainPackageDetails]) (\s a -> s {domainPackageDetailsList = a} :: ListPackagesForDomainResponse)
{-# DEPRECATED lpfdrsDomainPackageDetailsList "Use generic-lens or generic-optics with 'domainPackageDetailsList' instead." #-}

-- | Pagination token that needs to be supplied to the next call to get the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdrsNextToken :: Lens.Lens' ListPackagesForDomainResponse (Lude.Maybe Lude.Text)
lpfdrsNextToken = Lens.lens (nextToken :: ListPackagesForDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPackagesForDomainResponse)
{-# DEPRECATED lpfdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdrsResponseStatus :: Lens.Lens' ListPackagesForDomainResponse Lude.Int
lpfdrsResponseStatus = Lens.lens (responseStatus :: ListPackagesForDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPackagesForDomainResponse)
{-# DEPRECATED lpfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
