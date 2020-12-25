{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.BundleInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Bundles an Amazon instance store-backed Windows instance.
--
-- During bundling, only the root device volume (C:\) is bundled. Data on other instance store volumes is not preserved.
module Network.AWS.EC2.BundleInstance
  ( -- * Creating a request
    BundleInstance (..),
    mkBundleInstance,

    -- ** Request lenses
    biInstanceId,
    biStorage,
    biDryRun,

    -- * Destructuring the response
    BundleInstanceResponse (..),
    mkBundleInstanceResponse,

    -- ** Response lenses
    birrsBundleTask,
    birrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for BundleInstance.
--
-- /See:/ 'mkBundleInstance' smart constructor.
data BundleInstance = BundleInstance'
  { -- | The ID of the instance to bundle.
    --
    -- Type: String
    -- Default: None
    -- Required: Yes
    instanceId :: Types.InstanceId,
    -- | The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
    storage :: Types.Storage,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BundleInstance' value with any optional fields omitted.
mkBundleInstance ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'storage'
  Types.Storage ->
  BundleInstance
mkBundleInstance instanceId storage =
  BundleInstance' {instanceId, storage, dryRun = Core.Nothing}

-- | The ID of the instance to bundle.
--
-- Type: String
-- Default: None
-- Required: Yes
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biInstanceId :: Lens.Lens' BundleInstance Types.InstanceId
biInstanceId = Lens.field @"instanceId"
{-# DEPRECATED biInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biStorage :: Lens.Lens' BundleInstance Types.Storage
biStorage = Lens.field @"storage"
{-# DEPRECATED biStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biDryRun :: Lens.Lens' BundleInstance (Core.Maybe Core.Bool)
biDryRun = Lens.field @"dryRun"
{-# DEPRECATED biDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest BundleInstance where
  type Rs BundleInstance = BundleInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "BundleInstance")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> (Core.toQueryValue "Storage" storage)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          BundleInstanceResponse'
            Core.<$> (x Core..@? "bundleInstanceTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of BundleInstance.
--
-- /See:/ 'mkBundleInstanceResponse' smart constructor.
data BundleInstanceResponse = BundleInstanceResponse'
  { -- | Information about the bundle task.
    bundleTask :: Core.Maybe Types.BundleTask,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BundleInstanceResponse' value with any optional fields omitted.
mkBundleInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BundleInstanceResponse
mkBundleInstanceResponse responseStatus =
  BundleInstanceResponse'
    { bundleTask = Core.Nothing,
      responseStatus
    }

-- | Information about the bundle task.
--
-- /Note:/ Consider using 'bundleTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
birrsBundleTask :: Lens.Lens' BundleInstanceResponse (Core.Maybe Types.BundleTask)
birrsBundleTask = Lens.field @"bundleTask"
{-# DEPRECATED birrsBundleTask "Use generic-lens or generic-optics with 'bundleTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
birrsResponseStatus :: Lens.Lens' BundleInstanceResponse Core.Int
birrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED birrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
