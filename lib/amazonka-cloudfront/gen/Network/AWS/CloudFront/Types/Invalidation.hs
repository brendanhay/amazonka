{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Invalidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Invalidation
  ( Invalidation (..)
  -- * Smart constructor
  , mkInvalidation
  -- * Lenses
  , iId
  , iStatus
  , iCreateTime
  , iInvalidationBatch
  ) where

import qualified Network.AWS.CloudFront.Types.InvalidationBatch as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An invalidation. 
--
-- /See:/ 'mkInvalidation' smart constructor.
data Invalidation = Invalidation'
  { id :: Core.Text
    -- ^ The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
  , status :: Core.Text
    -- ^ The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
  , createTime :: Core.UTCTime
    -- ^ The date and time the invalidation request was first made. 
  , invalidationBatch :: Types.InvalidationBatch
    -- ^ The current invalidation information for the batch request. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Invalidation' value with any optional fields omitted.
mkInvalidation
    :: Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'status'
    -> Core.UTCTime -- ^ 'createTime'
    -> Types.InvalidationBatch -- ^ 'invalidationBatch'
    -> Invalidation
mkInvalidation id status createTime invalidationBatch
  = Invalidation'{id, status, createTime, invalidationBatch}

-- | The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Invalidation Core.Text
iId = Lens.field @"id"
{-# INLINEABLE iId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatus :: Lens.Lens' Invalidation Core.Text
iStatus = Lens.field @"status"
{-# INLINEABLE iStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The date and time the invalidation request was first made. 
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreateTime :: Lens.Lens' Invalidation Core.UTCTime
iCreateTime = Lens.field @"createTime"
{-# INLINEABLE iCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The current invalidation information for the batch request. 
--
-- /Note:/ Consider using 'invalidationBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInvalidationBatch :: Lens.Lens' Invalidation Types.InvalidationBatch
iInvalidationBatch = Lens.field @"invalidationBatch"
{-# INLINEABLE iInvalidationBatch #-}
{-# DEPRECATED invalidationBatch "Use generic-lens or generic-optics with 'invalidationBatch' instead"  #-}

instance Core.FromXML Invalidation where
        parseXML x
          = Invalidation' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "Status" Core.<*>
                x Core..@ "CreateTime"
                Core.<*> x Core..@ "InvalidationBatch"
