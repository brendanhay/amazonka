{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import instance task using metadata from the specified disk image. @ImportInstance@ only supports single-volume VMs. To import multi-volume VMs, use 'ImportImage' . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/ec2-cli-vmimport-export.html Importing a Virtual Machine Using the Amazon EC2 CLI> .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
module Network.AWS.EC2.ImportInstance
    (
    -- * Creating a request
      ImportInstance (..)
    , mkImportInstance
    -- ** Request lenses
    , iiPlatform
    , iiDescription
    , iiDiskImages
    , iiDryRun
    , iiLaunchSpecification

    -- * Destructuring the response
    , ImportInstanceResponse (..)
    , mkImportInstanceResponse
    -- ** Response lenses
    , iirrsConversionTask
    , iirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportInstance' smart constructor.
data ImportInstance = ImportInstance'
  { platform :: Types.PlatformValues
    -- ^ The instance operating system.
  , description :: Core.Maybe Core.Text
    -- ^ A description for the instance being imported.
  , diskImages :: Core.Maybe [Types.DiskImage]
    -- ^ The disk image.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , launchSpecification :: Core.Maybe Types.ImportInstanceLaunchSpecification
    -- ^ The launch specification.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportInstance' value with any optional fields omitted.
mkImportInstance
    :: Types.PlatformValues -- ^ 'platform'
    -> ImportInstance
mkImportInstance platform
  = ImportInstance'{platform, description = Core.Nothing,
                    diskImages = Core.Nothing, dryRun = Core.Nothing,
                    launchSpecification = Core.Nothing}

-- | The instance operating system.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiPlatform :: Lens.Lens' ImportInstance Types.PlatformValues
iiPlatform = Lens.field @"platform"
{-# INLINEABLE iiPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | A description for the instance being imported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDescription :: Lens.Lens' ImportInstance (Core.Maybe Core.Text)
iiDescription = Lens.field @"description"
{-# INLINEABLE iiDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The disk image.
--
-- /Note:/ Consider using 'diskImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDiskImages :: Lens.Lens' ImportInstance (Core.Maybe [Types.DiskImage])
iiDiskImages = Lens.field @"diskImages"
{-# INLINEABLE iiDiskImages #-}
{-# DEPRECATED diskImages "Use generic-lens or generic-optics with 'diskImages' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDryRun :: Lens.Lens' ImportInstance (Core.Maybe Core.Bool)
iiDryRun = Lens.field @"dryRun"
{-# INLINEABLE iiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The launch specification.
--
-- /Note:/ Consider using 'launchSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiLaunchSpecification :: Lens.Lens' ImportInstance (Core.Maybe Types.ImportInstanceLaunchSpecification)
iiLaunchSpecification = Lens.field @"launchSpecification"
{-# INLINEABLE iiLaunchSpecification #-}
{-# DEPRECATED launchSpecification "Use generic-lens or generic-optics with 'launchSpecification' instead"  #-}

instance Core.ToQuery ImportInstance where
        toQuery ImportInstance{..}
          = Core.toQueryPair "Action" ("ImportInstance" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Platform" platform
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "DiskImage") diskImages
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchSpecification")
                launchSpecification

instance Core.ToHeaders ImportInstance where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ImportInstance where
        type Rs ImportInstance = ImportInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ImportInstanceResponse' Core.<$>
                   (x Core..@? "conversionTask") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportInstanceResponse' smart constructor.
data ImportInstanceResponse = ImportInstanceResponse'
  { conversionTask :: Core.Maybe Types.ConversionTask
    -- ^ Information about the conversion task.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportInstanceResponse' value with any optional fields omitted.
mkImportInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportInstanceResponse
mkImportInstanceResponse responseStatus
  = ImportInstanceResponse'{conversionTask = Core.Nothing,
                            responseStatus}

-- | Information about the conversion task.
--
-- /Note:/ Consider using 'conversionTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iirrsConversionTask :: Lens.Lens' ImportInstanceResponse (Core.Maybe Types.ConversionTask)
iirrsConversionTask = Lens.field @"conversionTask"
{-# INLINEABLE iirrsConversionTask #-}
{-# DEPRECATED conversionTask "Use generic-lens or generic-optics with 'conversionTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iirrsResponseStatus :: Lens.Lens' ImportInstanceResponse Core.Int
iirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE iirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
