{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteChapCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes Challenge-Handshake Authentication Protocol (CHAP) credentials for a specified iSCSI target and initiator pair. This operation is supported in volume and tape gateway types.
module Network.AWS.StorageGateway.DeleteChapCredentials
    (
    -- * Creating a request
      DeleteChapCredentials (..)
    , mkDeleteChapCredentials
    -- ** Request lenses
    , dTargetARN
    , dInitiatorName

    -- * Destructuring the response
    , DeleteChapCredentialsResponse (..)
    , mkDeleteChapCredentialsResponse
    -- ** Response lenses
    , drsInitiatorName
    , drsTargetARN
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'DeleteChapCredentialsInput$InitiatorName' 
--
--
--     * 'DeleteChapCredentialsInput$TargetARN' 
--
--
--
-- /See:/ 'mkDeleteChapCredentials' smart constructor.
data DeleteChapCredentials = DeleteChapCredentials'
  { targetARN :: Types.TargetARN
    -- ^ The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
  , initiatorName :: Types.InitiatorName
    -- ^ The iSCSI initiator that connects to the target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteChapCredentials' value with any optional fields omitted.
mkDeleteChapCredentials
    :: Types.TargetARN -- ^ 'targetARN'
    -> Types.InitiatorName -- ^ 'initiatorName'
    -> DeleteChapCredentials
mkDeleteChapCredentials targetARN initiatorName
  = DeleteChapCredentials'{targetARN, initiatorName}

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTargetARN :: Lens.Lens' DeleteChapCredentials Types.TargetARN
dTargetARN = Lens.field @"targetARN"
{-# INLINEABLE dTargetARN #-}
{-# DEPRECATED targetARN "Use generic-lens or generic-optics with 'targetARN' instead"  #-}

-- | The iSCSI initiator that connects to the target.
--
-- /Note:/ Consider using 'initiatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInitiatorName :: Lens.Lens' DeleteChapCredentials Types.InitiatorName
dInitiatorName = Lens.field @"initiatorName"
{-# INLINEABLE dInitiatorName #-}
{-# DEPRECATED initiatorName "Use generic-lens or generic-optics with 'initiatorName' instead"  #-}

instance Core.ToQuery DeleteChapCredentials where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteChapCredentials where
        toHeaders DeleteChapCredentials{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DeleteChapCredentials")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteChapCredentials where
        toJSON DeleteChapCredentials{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TargetARN" Core..= targetARN),
                  Core.Just ("InitiatorName" Core..= initiatorName)])

instance Core.AWSRequest DeleteChapCredentials where
        type Rs DeleteChapCredentials = DeleteChapCredentialsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteChapCredentialsResponse' Core.<$>
                   (x Core..:? "InitiatorName") Core.<*> x Core..:? "TargetARN"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDeleteChapCredentialsResponse' smart constructor.
data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse'
  { initiatorName :: Core.Maybe Types.IqnName
    -- ^ The iSCSI initiator that connects to the target.
  , targetARN :: Core.Maybe Types.TargetARN
    -- ^ The Amazon Resource Name (ARN) of the target.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteChapCredentialsResponse' value with any optional fields omitted.
mkDeleteChapCredentialsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteChapCredentialsResponse
mkDeleteChapCredentialsResponse responseStatus
  = DeleteChapCredentialsResponse'{initiatorName = Core.Nothing,
                                   targetARN = Core.Nothing, responseStatus}

-- | The iSCSI initiator that connects to the target.
--
-- /Note:/ Consider using 'initiatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsInitiatorName :: Lens.Lens' DeleteChapCredentialsResponse (Core.Maybe Types.IqnName)
drsInitiatorName = Lens.field @"initiatorName"
{-# INLINEABLE drsInitiatorName #-}
{-# DEPRECATED initiatorName "Use generic-lens or generic-optics with 'initiatorName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTargetARN :: Lens.Lens' DeleteChapCredentialsResponse (Core.Maybe Types.TargetARN)
drsTargetARN = Lens.field @"targetARN"
{-# INLINEABLE drsTargetARN #-}
{-# DEPRECATED targetARN "Use generic-lens or generic-optics with 'targetARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteChapCredentialsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
