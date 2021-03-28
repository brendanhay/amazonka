{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.CreatePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a package for use with Amazon ES domains.
module Network.AWS.ElasticSearch.CreatePackage
    (
    -- * Creating a request
      CreatePackage (..)
    , mkCreatePackage
    -- ** Request lenses
    , cpPackageName
    , cpPackageType
    , cpPackageSource
    , cpPackageDescription

    -- * Destructuring the response
    , CreatePackageResponse (..)
    , mkCreatePackageResponse
    -- ** Response lenses
    , cprrsPackageDetails
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'CreatePackage' @ operation. 
--
-- /See:/ 'mkCreatePackage' smart constructor.
data CreatePackage = CreatePackage'
  { packageName :: Types.PackageName
    -- ^ Unique identifier for the package.
  , packageType :: Types.PackageType
    -- ^ Type of package. Currently supports only TXT-DICTIONARY.
  , packageSource :: Types.PackageSource
    -- ^ The customer S3 location @PackageSource@ for importing the package.
  , packageDescription :: Core.Maybe Types.PackageDescription
    -- ^ Description of the package.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePackage' value with any optional fields omitted.
mkCreatePackage
    :: Types.PackageName -- ^ 'packageName'
    -> Types.PackageType -- ^ 'packageType'
    -> Types.PackageSource -- ^ 'packageSource'
    -> CreatePackage
mkCreatePackage packageName packageType packageSource
  = CreatePackage'{packageName, packageType, packageSource,
                   packageDescription = Core.Nothing}

-- | Unique identifier for the package.
--
-- /Note:/ Consider using 'packageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPackageName :: Lens.Lens' CreatePackage Types.PackageName
cpPackageName = Lens.field @"packageName"
{-# INLINEABLE cpPackageName #-}
{-# DEPRECATED packageName "Use generic-lens or generic-optics with 'packageName' instead"  #-}

-- | Type of package. Currently supports only TXT-DICTIONARY.
--
-- /Note:/ Consider using 'packageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPackageType :: Lens.Lens' CreatePackage Types.PackageType
cpPackageType = Lens.field @"packageType"
{-# INLINEABLE cpPackageType #-}
{-# DEPRECATED packageType "Use generic-lens or generic-optics with 'packageType' instead"  #-}

-- | The customer S3 location @PackageSource@ for importing the package.
--
-- /Note:/ Consider using 'packageSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPackageSource :: Lens.Lens' CreatePackage Types.PackageSource
cpPackageSource = Lens.field @"packageSource"
{-# INLINEABLE cpPackageSource #-}
{-# DEPRECATED packageSource "Use generic-lens or generic-optics with 'packageSource' instead"  #-}

-- | Description of the package.
--
-- /Note:/ Consider using 'packageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPackageDescription :: Lens.Lens' CreatePackage (Core.Maybe Types.PackageDescription)
cpPackageDescription = Lens.field @"packageDescription"
{-# INLINEABLE cpPackageDescription #-}
{-# DEPRECATED packageDescription "Use generic-lens or generic-optics with 'packageDescription' instead"  #-}

instance Core.ToQuery CreatePackage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePackage where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreatePackage where
        toJSON CreatePackage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PackageName" Core..= packageName),
                  Core.Just ("PackageType" Core..= packageType),
                  Core.Just ("PackageSource" Core..= packageSource),
                  ("PackageDescription" Core..=) Core.<$> packageDescription])

instance Core.AWSRequest CreatePackage where
        type Rs CreatePackage = CreatePackageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2015-01-01/packages",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePackageResponse' Core.<$>
                   (x Core..:? "PackageDetails") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Container for response returned by @'CreatePackage' @ operation. 
--
-- /See:/ 'mkCreatePackageResponse' smart constructor.
data CreatePackageResponse = CreatePackageResponse'
  { packageDetails :: Core.Maybe Types.PackageDetails
    -- ^ Information about the package @PackageDetails@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreatePackageResponse' value with any optional fields omitted.
mkCreatePackageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePackageResponse
mkCreatePackageResponse responseStatus
  = CreatePackageResponse'{packageDetails = Core.Nothing,
                           responseStatus}

-- | Information about the package @PackageDetails@ .
--
-- /Note:/ Consider using 'packageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPackageDetails :: Lens.Lens' CreatePackageResponse (Core.Maybe Types.PackageDetails)
cprrsPackageDetails = Lens.field @"packageDetails"
{-# INLINEABLE cprrsPackageDetails #-}
{-# DEPRECATED packageDetails "Use generic-lens or generic-optics with 'packageDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePackageResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
