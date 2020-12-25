{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DeletePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the package.
module Network.AWS.ElasticSearch.DeletePackage
  ( -- * Creating a request
    DeletePackage (..),
    mkDeletePackage,

    -- ** Request lenses
    dPackageID,

    -- * Destructuring the response
    DeletePackageResponse (..),
    mkDeletePackageResponse,

    -- ** Response lenses
    dprfrsPackageDetails,
    dprfrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'DeletePackage' @ operation.
--
-- /See:/ 'mkDeletePackage' smart constructor.
newtype DeletePackage = DeletePackage'
  { -- | Internal ID of the package that you want to delete. Use @DescribePackages@ to find this value.
    packageID :: Types.PackageID
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePackage' value with any optional fields omitted.
mkDeletePackage ::
  -- | 'packageID'
  Types.PackageID ->
  DeletePackage
mkDeletePackage packageID = DeletePackage' {packageID}

-- | Internal ID of the package that you want to delete. Use @DescribePackages@ to find this value.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPackageID :: Lens.Lens' DeletePackage Types.PackageID
dPackageID = Lens.field @"packageID"
{-# DEPRECATED dPackageID "Use generic-lens or generic-optics with 'packageID' instead." #-}

instance Core.AWSRequest DeletePackage where
  type Rs DeletePackage = DeletePackageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/2015-01-01/packages/" Core.<> (Core.toText packageID)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePackageResponse'
            Core.<$> (x Core..:? "PackageDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Container for response parameters to @'DeletePackage' @ operation.
--
-- /See:/ 'mkDeletePackageResponse' smart constructor.
data DeletePackageResponse = DeletePackageResponse'
  { -- | @PackageDetails@
    packageDetails :: Core.Maybe Types.PackageDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeletePackageResponse' value with any optional fields omitted.
mkDeletePackageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeletePackageResponse
mkDeletePackageResponse responseStatus =
  DeletePackageResponse'
    { packageDetails = Core.Nothing,
      responseStatus
    }

-- | @PackageDetails@
--
-- /Note:/ Consider using 'packageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsPackageDetails :: Lens.Lens' DeletePackageResponse (Core.Maybe Types.PackageDetails)
dprfrsPackageDetails = Lens.field @"packageDetails"
{-# DEPRECATED dprfrsPackageDetails "Use generic-lens or generic-optics with 'packageDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsResponseStatus :: Lens.Lens' DeletePackageResponse Core.Int
dprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
