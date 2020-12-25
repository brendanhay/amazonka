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
    dpPackageID,
    dpDomainName,

    -- * Destructuring the response
    DissociatePackageResponse (..),
    mkDissociatePackageResponse,

    -- ** Response lenses
    dprgrsDomainPackageDetails,
    dprgrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'DissociatePackage' @ operation.
--
-- /See:/ 'mkDissociatePackage' smart constructor.
data DissociatePackage = DissociatePackage'
  { -- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
    packageID :: Types.PackageID,
    -- | Name of the domain that you want to associate the package with.
    domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DissociatePackage' value with any optional fields omitted.
mkDissociatePackage ::
  -- | 'packageID'
  Types.PackageID ->
  -- | 'domainName'
  Types.DomainName ->
  DissociatePackage
mkDissociatePackage packageID domainName =
  DissociatePackage' {packageID, domainName}

-- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPackageID :: Lens.Lens' DissociatePackage Types.PackageID
dpPackageID = Lens.field @"packageID"
{-# DEPRECATED dpPackageID "Use generic-lens or generic-optics with 'packageID' instead." #-}

-- | Name of the domain that you want to associate the package with.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDomainName :: Lens.Lens' DissociatePackage Types.DomainName
dpDomainName = Lens.field @"domainName"
{-# DEPRECATED dpDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.FromJSON DissociatePackage where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DissociatePackage where
  type Rs DissociatePackage = DissociatePackageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2015-01-01/packages/dissociate/" Core.<> (Core.toText packageID)
                Core.<> ("/")
                Core.<> (Core.toText domainName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DissociatePackageResponse'
            Core.<$> (x Core..:? "DomainPackageDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Container for response returned by @'DissociatePackage' @ operation.
--
-- /See:/ 'mkDissociatePackageResponse' smart constructor.
data DissociatePackageResponse = DissociatePackageResponse'
  { -- | @DomainPackageDetails@
    domainPackageDetails :: Core.Maybe Types.DomainPackageDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DissociatePackageResponse' value with any optional fields omitted.
mkDissociatePackageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DissociatePackageResponse
mkDissociatePackageResponse responseStatus =
  DissociatePackageResponse'
    { domainPackageDetails = Core.Nothing,
      responseStatus
    }

-- | @DomainPackageDetails@
--
-- /Note:/ Consider using 'domainPackageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprgrsDomainPackageDetails :: Lens.Lens' DissociatePackageResponse (Core.Maybe Types.DomainPackageDetails)
dprgrsDomainPackageDetails = Lens.field @"domainPackageDetails"
{-# DEPRECATED dprgrsDomainPackageDetails "Use generic-lens or generic-optics with 'domainPackageDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprgrsResponseStatus :: Lens.Lens' DissociatePackageResponse Core.Int
dprgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
