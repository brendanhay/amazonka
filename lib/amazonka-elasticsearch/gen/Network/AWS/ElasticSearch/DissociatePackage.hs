{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DissociatePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dissociates a package from the Amazon ES domain.
module Network.AWS.ElasticSearch.DissociatePackage
  ( -- * Creating a request
    DissociatePackage (..),
    mkDissociatePackage,

    -- ** Request lenses
    dpPackageId,
    dpDomainName,

    -- * Destructuring the response
    DissociatePackageResponse (..),
    mkDissociatePackageResponse,

    -- ** Response lenses
    dpgrsDomainPackageDetails,
    dpgrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'DissociatePackage' @ operation.
--
-- /See:/ 'mkDissociatePackage' smart constructor.
data DissociatePackage = DissociatePackage'
  { -- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
    packageId :: Lude.Text,
    -- | Name of the domain that you want to associate the package with.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DissociatePackage' with the minimum fields required to make a request.
--
-- * 'packageId' - Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
-- * 'domainName' - Name of the domain that you want to associate the package with.
mkDissociatePackage ::
  -- | 'packageId'
  Lude.Text ->
  -- | 'domainName'
  Lude.Text ->
  DissociatePackage
mkDissociatePackage pPackageId_ pDomainName_ =
  DissociatePackage'
    { packageId = pPackageId_,
      domainName = pDomainName_
    }

-- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
--
-- /Note:/ Consider using 'packageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPackageId :: Lens.Lens' DissociatePackage Lude.Text
dpPackageId = Lens.lens (packageId :: DissociatePackage -> Lude.Text) (\s a -> s {packageId = a} :: DissociatePackage)
{-# DEPRECATED dpPackageId "Use generic-lens or generic-optics with 'packageId' instead." #-}

-- | Name of the domain that you want to associate the package with.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDomainName :: Lens.Lens' DissociatePackage Lude.Text
dpDomainName = Lens.lens (domainName :: DissociatePackage -> Lude.Text) (\s a -> s {domainName = a} :: DissociatePackage)
{-# DEPRECATED dpDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DissociatePackage where
  type Rs DissociatePackage = DissociatePackageResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DissociatePackageResponse'
            Lude.<$> (x Lude..?> "DomainPackageDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DissociatePackage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DissociatePackage where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DissociatePackage where
  toPath DissociatePackage' {..} =
    Lude.mconcat
      [ "/2015-01-01/packages/dissociate/",
        Lude.toBS packageId,
        "/",
        Lude.toBS domainName
      ]

instance Lude.ToQuery DissociatePackage where
  toQuery = Lude.const Lude.mempty

-- | Container for response returned by @'DissociatePackage' @ operation.
--
-- /See:/ 'mkDissociatePackageResponse' smart constructor.
data DissociatePackageResponse = DissociatePackageResponse'
  { -- | @DomainPackageDetails@
    domainPackageDetails :: Lude.Maybe DomainPackageDetails,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DissociatePackageResponse' with the minimum fields required to make a request.
--
-- * 'domainPackageDetails' - @DomainPackageDetails@
-- * 'responseStatus' - The response status code.
mkDissociatePackageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DissociatePackageResponse
mkDissociatePackageResponse pResponseStatus_ =
  DissociatePackageResponse'
    { domainPackageDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | @DomainPackageDetails@
--
-- /Note:/ Consider using 'domainPackageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrsDomainPackageDetails :: Lens.Lens' DissociatePackageResponse (Lude.Maybe DomainPackageDetails)
dpgrsDomainPackageDetails = Lens.lens (domainPackageDetails :: DissociatePackageResponse -> Lude.Maybe DomainPackageDetails) (\s a -> s {domainPackageDetails = a} :: DissociatePackageResponse)
{-# DEPRECATED dpgrsDomainPackageDetails "Use generic-lens or generic-optics with 'domainPackageDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrsResponseStatus :: Lens.Lens' DissociatePackageResponse Lude.Int
dpgrsResponseStatus = Lens.lens (responseStatus :: DissociatePackageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DissociatePackageResponse)
{-# DEPRECATED dpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
