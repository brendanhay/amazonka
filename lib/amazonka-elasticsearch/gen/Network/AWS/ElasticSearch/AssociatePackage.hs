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
    apPackageID,
    apDomainName,

    -- * Destructuring the response
    AssociatePackageResponse (..),
    mkAssociatePackageResponse,

    -- ** Response lenses
    aprrsDomainPackageDetails,
    aprrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'AssociatePackage' @ operation.
--
-- /See:/ 'mkAssociatePackage' smart constructor.
data AssociatePackage = AssociatePackage'
  { -- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
    packageID :: Types.PackageID,
    -- | Name of the domain that you want to associate the package with.
    domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociatePackage' value with any optional fields omitted.
mkAssociatePackage ::
  -- | 'packageID'
  Types.PackageID ->
  -- | 'domainName'
  Types.DomainName ->
  AssociatePackage
mkAssociatePackage packageID domainName =
  AssociatePackage' {packageID, domainName}

-- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPackageID :: Lens.Lens' AssociatePackage Types.PackageID
apPackageID = Lens.field @"packageID"
{-# DEPRECATED apPackageID "Use generic-lens or generic-optics with 'packageID' instead." #-}

-- | Name of the domain that you want to associate the package with.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apDomainName :: Lens.Lens' AssociatePackage Types.DomainName
apDomainName = Lens.field @"domainName"
{-# DEPRECATED apDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.FromJSON AssociatePackage where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest AssociatePackage where
  type Rs AssociatePackage = AssociatePackageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2015-01-01/packages/associate/" Core.<> (Core.toText packageID)
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
          AssociatePackageResponse'
            Core.<$> (x Core..:? "DomainPackageDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Container for response returned by @'AssociatePackage' @ operation.
--
-- /See:/ 'mkAssociatePackageResponse' smart constructor.
data AssociatePackageResponse = AssociatePackageResponse'
  { -- | @DomainPackageDetails@
    domainPackageDetails :: Core.Maybe Types.DomainPackageDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AssociatePackageResponse' value with any optional fields omitted.
mkAssociatePackageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociatePackageResponse
mkAssociatePackageResponse responseStatus =
  AssociatePackageResponse'
    { domainPackageDetails = Core.Nothing,
      responseStatus
    }

-- | @DomainPackageDetails@
--
-- /Note:/ Consider using 'domainPackageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprrsDomainPackageDetails :: Lens.Lens' AssociatePackageResponse (Core.Maybe Types.DomainPackageDetails)
aprrsDomainPackageDetails = Lens.field @"domainPackageDetails"
{-# DEPRECATED aprrsDomainPackageDetails "Use generic-lens or generic-optics with 'domainPackageDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprrsResponseStatus :: Lens.Lens' AssociatePackageResponse Core.Int
aprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
