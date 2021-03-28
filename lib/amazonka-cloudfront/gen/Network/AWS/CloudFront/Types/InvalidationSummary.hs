{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.InvalidationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.InvalidationSummary
  ( InvalidationSummary (..)
  -- * Smart constructor
  , mkInvalidationSummary
  -- * Lenses
  , isId
  , isCreateTime
  , isStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of an invalidation request.
--
-- /See:/ 'mkInvalidationSummary' smart constructor.
data InvalidationSummary = InvalidationSummary'
  { id :: Core.Text
    -- ^ The unique ID for an invalidation request.
  , createTime :: Core.UTCTime
    -- ^ The time that an invalidation request was created.
  , status :: Core.Text
    -- ^ The status of an invalidation request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InvalidationSummary' value with any optional fields omitted.
mkInvalidationSummary
    :: Core.Text -- ^ 'id'
    -> Core.UTCTime -- ^ 'createTime'
    -> Core.Text -- ^ 'status'
    -> InvalidationSummary
mkInvalidationSummary id createTime status
  = InvalidationSummary'{id, createTime, status}

-- | The unique ID for an invalidation request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isId :: Lens.Lens' InvalidationSummary Core.Text
isId = Lens.field @"id"
{-# INLINEABLE isId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The time that an invalidation request was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCreateTime :: Lens.Lens' InvalidationSummary Core.UTCTime
isCreateTime = Lens.field @"createTime"
{-# INLINEABLE isCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The status of an invalidation request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isStatus :: Lens.Lens' InvalidationSummary Core.Text
isStatus = Lens.field @"status"
{-# INLINEABLE isStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML InvalidationSummary where
        parseXML x
          = InvalidationSummary' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "CreateTime" Core.<*>
                x Core..@ "Status"
