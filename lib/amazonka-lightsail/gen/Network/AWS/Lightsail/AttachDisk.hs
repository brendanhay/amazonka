{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AttachDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a block storage disk to a running or stopped Lightsail instance and exposes it to the instance with the specified disk name.
--
-- The @attach disk@ operation supports tag-based access control via resource tags applied to the resource identified by @disk name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.AttachDisk
    (
    -- * Creating a request
      AttachDisk (..)
    , mkAttachDisk
    -- ** Request lenses
    , adDiskName
    , adInstanceName
    , adDiskPath

    -- * Destructuring the response
    , AttachDiskResponse (..)
    , mkAttachDiskResponse
    -- ** Response lenses
    , adrrsOperations
    , adrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachDisk' smart constructor.
data AttachDisk = AttachDisk'
  { diskName :: Types.ResourceName
    -- ^ The unique Lightsail disk name (e.g., @my-disk@ ).
  , instanceName :: Types.ResourceName
    -- ^ The name of the Lightsail instance where you want to utilize the storage disk.
  , diskPath :: Types.NonEmptyString
    -- ^ The disk path to expose to the instance (e.g., @/dev/xvdf@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachDisk' value with any optional fields omitted.
mkAttachDisk
    :: Types.ResourceName -- ^ 'diskName'
    -> Types.ResourceName -- ^ 'instanceName'
    -> Types.NonEmptyString -- ^ 'diskPath'
    -> AttachDisk
mkAttachDisk diskName instanceName diskPath
  = AttachDisk'{diskName, instanceName, diskPath}

-- | The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDiskName :: Lens.Lens' AttachDisk Types.ResourceName
adDiskName = Lens.field @"diskName"
{-# INLINEABLE adDiskName #-}
{-# DEPRECATED diskName "Use generic-lens or generic-optics with 'diskName' instead"  #-}

-- | The name of the Lightsail instance where you want to utilize the storage disk.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adInstanceName :: Lens.Lens' AttachDisk Types.ResourceName
adInstanceName = Lens.field @"instanceName"
{-# INLINEABLE adInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

-- | The disk path to expose to the instance (e.g., @/dev/xvdf@ ).
--
-- /Note:/ Consider using 'diskPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDiskPath :: Lens.Lens' AttachDisk Types.NonEmptyString
adDiskPath = Lens.field @"diskPath"
{-# INLINEABLE adDiskPath #-}
{-# DEPRECATED diskPath "Use generic-lens or generic-optics with 'diskPath' instead"  #-}

instance Core.ToQuery AttachDisk where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AttachDisk where
        toHeaders AttachDisk{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.AttachDisk")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AttachDisk where
        toJSON AttachDisk{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("diskName" Core..= diskName),
                  Core.Just ("instanceName" Core..= instanceName),
                  Core.Just ("diskPath" Core..= diskPath)])

instance Core.AWSRequest AttachDisk where
        type Rs AttachDisk = AttachDiskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AttachDiskResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachDiskResponse' smart constructor.
data AttachDiskResponse = AttachDiskResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AttachDiskResponse' value with any optional fields omitted.
mkAttachDiskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AttachDiskResponse
mkAttachDiskResponse responseStatus
  = AttachDiskResponse'{operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrrsOperations :: Lens.Lens' AttachDiskResponse (Core.Maybe [Types.Operation])
adrrsOperations = Lens.field @"operations"
{-# INLINEABLE adrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrrsResponseStatus :: Lens.Lens' AttachDiskResponse Core.Int
adrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE adrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
