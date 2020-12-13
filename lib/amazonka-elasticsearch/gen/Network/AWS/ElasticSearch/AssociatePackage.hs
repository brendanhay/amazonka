{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.AssociatePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a package with an Amazon ES domain.
module Network.AWS.ElasticSearch.AssociatePackage
  ( -- * Creating a request
    AssociatePackage (..),
    mkAssociatePackage,

    -- ** Request lenses
    apPackageId,
    apDomainName,

    -- * Destructuring the response
    AssociatePackageResponse (..),
    mkAssociatePackageResponse,

    -- ** Response lenses
    aprsDomainPackageDetails,
    aprsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'AssociatePackage' @ operation.
--
-- /See:/ 'mkAssociatePackage' smart constructor.
data AssociatePackage = AssociatePackage'
  { -- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
    packageId :: Lude.Text,
    -- | Name of the domain that you want to associate the package with.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociatePackage' with the minimum fields required to make a request.
--
-- * 'packageId' - Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
-- * 'domainName' - Name of the domain that you want to associate the package with.
mkAssociatePackage ::
  -- | 'packageId'
  Lude.Text ->
  -- | 'domainName'
  Lude.Text ->
  AssociatePackage
mkAssociatePackage pPackageId_ pDomainName_ =
  AssociatePackage'
    { packageId = pPackageId_,
      domainName = pDomainName_
    }

-- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
--
-- /Note:/ Consider using 'packageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPackageId :: Lens.Lens' AssociatePackage Lude.Text
apPackageId = Lens.lens (packageId :: AssociatePackage -> Lude.Text) (\s a -> s {packageId = a} :: AssociatePackage)
{-# DEPRECATED apPackageId "Use generic-lens or generic-optics with 'packageId' instead." #-}

-- | Name of the domain that you want to associate the package with.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apDomainName :: Lens.Lens' AssociatePackage Lude.Text
apDomainName = Lens.lens (domainName :: AssociatePackage -> Lude.Text) (\s a -> s {domainName = a} :: AssociatePackage)
{-# DEPRECATED apDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest AssociatePackage where
  type Rs AssociatePackage = AssociatePackageResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociatePackageResponse'
            Lude.<$> (x Lude..?> "DomainPackageDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociatePackage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AssociatePackage where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath AssociatePackage where
  toPath AssociatePackage' {..} =
    Lude.mconcat
      [ "/2015-01-01/packages/associate/",
        Lude.toBS packageId,
        "/",
        Lude.toBS domainName
      ]

instance Lude.ToQuery AssociatePackage where
  toQuery = Lude.const Lude.mempty

-- | Container for response returned by @'AssociatePackage' @ operation.
--
-- /See:/ 'mkAssociatePackageResponse' smart constructor.
data AssociatePackageResponse = AssociatePackageResponse'
  { -- | @DomainPackageDetails@
    domainPackageDetails :: Lude.Maybe DomainPackageDetails,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociatePackageResponse' with the minimum fields required to make a request.
--
-- * 'domainPackageDetails' - @DomainPackageDetails@
-- * 'responseStatus' - The response status code.
mkAssociatePackageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociatePackageResponse
mkAssociatePackageResponse pResponseStatus_ =
  AssociatePackageResponse'
    { domainPackageDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | @DomainPackageDetails@
--
-- /Note:/ Consider using 'domainPackageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprsDomainPackageDetails :: Lens.Lens' AssociatePackageResponse (Lude.Maybe DomainPackageDetails)
aprsDomainPackageDetails = Lens.lens (domainPackageDetails :: AssociatePackageResponse -> Lude.Maybe DomainPackageDetails) (\s a -> s {domainPackageDetails = a} :: AssociatePackageResponse)
{-# DEPRECATED aprsDomainPackageDetails "Use generic-lens or generic-optics with 'domainPackageDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprsResponseStatus :: Lens.Lens' AssociatePackageResponse Lude.Int
aprsResponseStatus = Lens.lens (responseStatus :: AssociatePackageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociatePackageResponse)
{-# DEPRECATED aprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
