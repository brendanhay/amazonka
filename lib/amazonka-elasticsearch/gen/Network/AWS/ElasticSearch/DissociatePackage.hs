{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DissociatePackage (..)
    , mkDissociatePackage
    -- ** Request lenses
    , dpPackageID
    , dpDomainName

    -- * Destructuring the response
    , DissociatePackageResponse (..)
    , mkDissociatePackageResponse
    -- ** Response lenses
    , dprgrsDomainPackageDetails
    , dprgrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'DissociatePackage' @ operation. 
--
-- /See:/ 'mkDissociatePackage' smart constructor.
data DissociatePackage = DissociatePackage'
  { packageID :: Types.PackageID
    -- ^ Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
  , domainName :: Types.DomainName
    -- ^ Name of the domain that you want to associate the package with.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DissociatePackage' value with any optional fields omitted.
mkDissociatePackage
    :: Types.PackageID -- ^ 'packageID'
    -> Types.DomainName -- ^ 'domainName'
    -> DissociatePackage
mkDissociatePackage packageID domainName
  = DissociatePackage'{packageID, domainName}

-- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPackageID :: Lens.Lens' DissociatePackage Types.PackageID
dpPackageID = Lens.field @"packageID"
{-# INLINEABLE dpPackageID #-}
{-# DEPRECATED packageID "Use generic-lens or generic-optics with 'packageID' instead"  #-}

-- | Name of the domain that you want to associate the package with.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDomainName :: Lens.Lens' DissociatePackage Types.DomainName
dpDomainName = Lens.field @"domainName"
{-# INLINEABLE dpDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery DissociatePackage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DissociatePackage where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON DissociatePackage where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DissociatePackage where
        type Rs DissociatePackage = DissociatePackageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2015-01-01/packages/dissociate/" Core.<> Core.toText packageID
                             Core.<> "/"
                             Core.<> Core.toText domainName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DissociatePackageResponse' Core.<$>
                   (x Core..:? "DomainPackageDetails") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Container for response returned by @'DissociatePackage' @ operation. 
--
-- /See:/ 'mkDissociatePackageResponse' smart constructor.
data DissociatePackageResponse = DissociatePackageResponse'
  { domainPackageDetails :: Core.Maybe Types.DomainPackageDetails
    -- ^ @DomainPackageDetails@ 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DissociatePackageResponse' value with any optional fields omitted.
mkDissociatePackageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DissociatePackageResponse
mkDissociatePackageResponse responseStatus
  = DissociatePackageResponse'{domainPackageDetails = Core.Nothing,
                               responseStatus}

-- | @DomainPackageDetails@ 
--
-- /Note:/ Consider using 'domainPackageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprgrsDomainPackageDetails :: Lens.Lens' DissociatePackageResponse (Core.Maybe Types.DomainPackageDetails)
dprgrsDomainPackageDetails = Lens.field @"domainPackageDetails"
{-# INLINEABLE dprgrsDomainPackageDetails #-}
{-# DEPRECATED domainPackageDetails "Use generic-lens or generic-optics with 'domainPackageDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprgrsResponseStatus :: Lens.Lens' DissociatePackageResponse Core.Int
dprgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
