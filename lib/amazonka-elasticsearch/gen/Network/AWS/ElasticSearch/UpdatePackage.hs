{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.UpdatePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a package for use with Amazon ES domains.
module Network.AWS.ElasticSearch.UpdatePackage
  ( -- * Creating a request
    UpdatePackage (..),
    mkUpdatePackage,

    -- ** Request lenses
    upPackageID,
    upPackageSource,
    upCommitMessage,
    upPackageDescription,

    -- * Destructuring the response
    UpdatePackageResponse (..),
    mkUpdatePackageResponse,

    -- ** Response lenses
    uprrsPackageDetails,
    uprrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'UpdatePackage' @ operation.
--
-- /See:/ 'mkUpdatePackage' smart constructor.
data UpdatePackage = UpdatePackage'
  { -- | Unique identifier for the package.
    packageID :: Types.PackageID,
    packageSource :: Types.PackageSource,
    -- | An info message for the new version which will be shown as part of @GetPackageVersionHistoryResponse@ .
    commitMessage :: Core.Maybe Types.CommitMessage,
    -- | New description of the package.
    packageDescription :: Core.Maybe Types.PackageDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePackage' value with any optional fields omitted.
mkUpdatePackage ::
  -- | 'packageID'
  Types.PackageID ->
  -- | 'packageSource'
  Types.PackageSource ->
  UpdatePackage
mkUpdatePackage packageID packageSource =
  UpdatePackage'
    { packageID,
      packageSource,
      commitMessage = Core.Nothing,
      packageDescription = Core.Nothing
    }

-- | Unique identifier for the package.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPackageID :: Lens.Lens' UpdatePackage Types.PackageID
upPackageID = Lens.field @"packageID"
{-# DEPRECATED upPackageID "Use generic-lens or generic-optics with 'packageID' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'packageSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPackageSource :: Lens.Lens' UpdatePackage Types.PackageSource
upPackageSource = Lens.field @"packageSource"
{-# DEPRECATED upPackageSource "Use generic-lens or generic-optics with 'packageSource' instead." #-}

-- | An info message for the new version which will be shown as part of @GetPackageVersionHistoryResponse@ .
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upCommitMessage :: Lens.Lens' UpdatePackage (Core.Maybe Types.CommitMessage)
upCommitMessage = Lens.field @"commitMessage"
{-# DEPRECATED upCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | New description of the package.
--
-- /Note:/ Consider using 'packageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPackageDescription :: Lens.Lens' UpdatePackage (Core.Maybe Types.PackageDescription)
upPackageDescription = Lens.field @"packageDescription"
{-# DEPRECATED upPackageDescription "Use generic-lens or generic-optics with 'packageDescription' instead." #-}

instance Core.FromJSON UpdatePackage where
  toJSON UpdatePackage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PackageID" Core..= packageID),
            Core.Just ("PackageSource" Core..= packageSource),
            ("CommitMessage" Core..=) Core.<$> commitMessage,
            ("PackageDescription" Core..=) Core.<$> packageDescription
          ]
      )

instance Core.AWSRequest UpdatePackage where
  type Rs UpdatePackage = UpdatePackageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2015-01-01/packages/update",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePackageResponse'
            Core.<$> (x Core..:? "PackageDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Container for response returned by @'UpdatePackage' @ operation.
--
-- /See:/ 'mkUpdatePackageResponse' smart constructor.
data UpdatePackageResponse = UpdatePackageResponse'
  { -- | Information about the package @PackageDetails@ .
    packageDetails :: Core.Maybe Types.PackageDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdatePackageResponse' value with any optional fields omitted.
mkUpdatePackageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdatePackageResponse
mkUpdatePackageResponse responseStatus =
  UpdatePackageResponse'
    { packageDetails = Core.Nothing,
      responseStatus
    }

-- | Information about the package @PackageDetails@ .
--
-- /Note:/ Consider using 'packageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsPackageDetails :: Lens.Lens' UpdatePackageResponse (Core.Maybe Types.PackageDetails)
uprrsPackageDetails = Lens.field @"packageDetails"
{-# DEPRECATED uprrsPackageDetails "Use generic-lens or generic-optics with 'packageDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdatePackageResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
