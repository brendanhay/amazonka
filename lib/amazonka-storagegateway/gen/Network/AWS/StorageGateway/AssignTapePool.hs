{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AssignTapePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns a tape to a tape pool for archiving. The tape assigned to a pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the S3 storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
module Network.AWS.StorageGateway.AssignTapePool
    (
    -- * Creating a request
      AssignTapePool (..)
    , mkAssignTapePool
    -- ** Request lenses
    , atpTapeARN
    , atpPoolId
    , atpBypassGovernanceRetention

    -- * Destructuring the response
    , AssignTapePoolResponse (..)
    , mkAssignTapePoolResponse
    -- ** Response lenses
    , atprrsTapeARN
    , atprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkAssignTapePool' smart constructor.
data AssignTapePool = AssignTapePool'
  { tapeARN :: Types.TapeARN
    -- ^ The unique Amazon Resource Name (ARN) of the virtual tape that you want to add to the tape pool.
  , poolId :: Types.PoolId
    -- ^ The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
  , bypassGovernanceRetention :: Core.Maybe Core.Bool
    -- ^ Set permissions to bypass governance retention. If the lock type of the archived tape is @Governance@ , the tape's archived age is not older than @RetentionLockInDays@ , and the user does not already have @BypassGovernanceRetention@ , setting this to TRUE enables the user to bypass the retention lock. This parameter is set to true by default for calls from the console.
--
-- Valid values: @TRUE@ | @FALSE@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssignTapePool' value with any optional fields omitted.
mkAssignTapePool
    :: Types.TapeARN -- ^ 'tapeARN'
    -> Types.PoolId -- ^ 'poolId'
    -> AssignTapePool
mkAssignTapePool tapeARN poolId
  = AssignTapePool'{tapeARN, poolId,
                    bypassGovernanceRetention = Core.Nothing}

-- | The unique Amazon Resource Name (ARN) of the virtual tape that you want to add to the tape pool.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpTapeARN :: Lens.Lens' AssignTapePool Types.TapeARN
atpTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE atpTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpPoolId :: Lens.Lens' AssignTapePool Types.PoolId
atpPoolId = Lens.field @"poolId"
{-# INLINEABLE atpPoolId #-}
{-# DEPRECATED poolId "Use generic-lens or generic-optics with 'poolId' instead"  #-}

-- | Set permissions to bypass governance retention. If the lock type of the archived tape is @Governance@ , the tape's archived age is not older than @RetentionLockInDays@ , and the user does not already have @BypassGovernanceRetention@ , setting this to TRUE enables the user to bypass the retention lock. This parameter is set to true by default for calls from the console.
--
-- Valid values: @TRUE@ | @FALSE@ 
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpBypassGovernanceRetention :: Lens.Lens' AssignTapePool (Core.Maybe Core.Bool)
atpBypassGovernanceRetention = Lens.field @"bypassGovernanceRetention"
{-# INLINEABLE atpBypassGovernanceRetention #-}
{-# DEPRECATED bypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead"  #-}

instance Core.ToQuery AssignTapePool where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssignTapePool where
        toHeaders AssignTapePool{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.AssignTapePool")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssignTapePool where
        toJSON AssignTapePool{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TapeARN" Core..= tapeARN),
                  Core.Just ("PoolId" Core..= poolId),
                  ("BypassGovernanceRetention" Core..=) Core.<$>
                    bypassGovernanceRetention])

instance Core.AWSRequest AssignTapePool where
        type Rs AssignTapePool = AssignTapePoolResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AssignTapePoolResponse' Core.<$>
                   (x Core..:? "TapeARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssignTapePoolResponse' smart constructor.
data AssignTapePoolResponse = AssignTapePoolResponse'
  { tapeARN :: Core.Maybe Types.TapeARN
    -- ^ The unique Amazon Resource Names (ARN) of the virtual tape that was added to the tape pool.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssignTapePoolResponse' value with any optional fields omitted.
mkAssignTapePoolResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssignTapePoolResponse
mkAssignTapePoolResponse responseStatus
  = AssignTapePoolResponse'{tapeARN = Core.Nothing, responseStatus}

-- | The unique Amazon Resource Names (ARN) of the virtual tape that was added to the tape pool.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atprrsTapeARN :: Lens.Lens' AssignTapePoolResponse (Core.Maybe Types.TapeARN)
atprrsTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE atprrsTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atprrsResponseStatus :: Lens.Lens' AssignTapePoolResponse Core.Int
atprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
