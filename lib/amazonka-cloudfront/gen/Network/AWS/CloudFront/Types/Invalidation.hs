{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Invalidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Invalidation
  ( Invalidation (..),

    -- * Smart constructor
    mkInvalidation,

    -- * Lenses
    iId,
    iStatus,
    iCreateTime,
    iInvalidationBatch,
  )
where

import qualified Network.AWS.CloudFront.Types.Id as Types
import qualified Network.AWS.CloudFront.Types.InvalidationBatch as Types
import qualified Network.AWS.CloudFront.Types.Status as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An invalidation.
--
-- /See:/ 'mkInvalidation' smart constructor.
data Invalidation = Invalidation'
  { -- | The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
    id :: Types.Id,
    -- | The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
    status :: Types.Status,
    -- | The date and time the invalidation request was first made.
    createTime :: Core.UTCTime,
    -- | The current invalidation information for the batch request.
    invalidationBatch :: Types.InvalidationBatch
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Invalidation' value with any optional fields omitted.
mkInvalidation ::
  -- | 'id'
  Types.Id ->
  -- | 'status'
  Types.Status ->
  -- | 'createTime'
  Core.UTCTime ->
  -- | 'invalidationBatch'
  Types.InvalidationBatch ->
  Invalidation
mkInvalidation id status createTime invalidationBatch =
  Invalidation' {id, status, createTime, invalidationBatch}

-- | The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Invalidation Types.Id
iId = Lens.field @"id"
{-# DEPRECATED iId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatus :: Lens.Lens' Invalidation Types.Status
iStatus = Lens.field @"status"
{-# DEPRECATED iStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time the invalidation request was first made.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreateTime :: Lens.Lens' Invalidation Core.UTCTime
iCreateTime = Lens.field @"createTime"
{-# DEPRECATED iCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The current invalidation information for the batch request.
--
-- /Note:/ Consider using 'invalidationBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInvalidationBatch :: Lens.Lens' Invalidation Types.InvalidationBatch
iInvalidationBatch = Lens.field @"invalidationBatch"
{-# DEPRECATED iInvalidationBatch "Use generic-lens or generic-optics with 'invalidationBatch' instead." #-}

instance Core.FromXML Invalidation where
  parseXML x =
    Invalidation'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@ "CreateTime")
      Core.<*> (x Core..@ "InvalidationBatch")
