{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateTapePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom tape pool. You can use custom tape pool to enable tape retention lock on tapes that are archived in the custom pool.
module Network.AWS.StorageGateway.CreateTapePool
  ( -- * Creating a request
    CreateTapePool (..),
    mkCreateTapePool,

    -- ** Request lenses
    ctpPoolName,
    ctpStorageClass,
    ctpRetentionLockTimeInDays,
    ctpRetentionLockType,
    ctpTags,

    -- * Destructuring the response
    CreateTapePoolResponse (..),
    mkCreateTapePoolResponse,

    -- ** Response lenses
    ctprrsPoolARN,
    ctprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkCreateTapePool' smart constructor.
data CreateTapePool = CreateTapePool'
  { -- | The name of the new custom tape pool.
    poolName :: Types.PoolName,
    -- | The storage class that is associated with the new custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
    storageClass :: Types.TapeStorageClass,
    -- | Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
    retentionLockTimeInDays :: Core.Maybe Core.Natural,
    -- | Tape retention lock can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
    retentionLockType :: Core.Maybe Types.RetentionLockType,
    -- | A list of up to 50 tags that can be assigned to tape pool. Each tag is a key-value pair.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTapePool' value with any optional fields omitted.
mkCreateTapePool ::
  -- | 'poolName'
  Types.PoolName ->
  -- | 'storageClass'
  Types.TapeStorageClass ->
  CreateTapePool
mkCreateTapePool poolName storageClass =
  CreateTapePool'
    { poolName,
      storageClass,
      retentionLockTimeInDays = Core.Nothing,
      retentionLockType = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the new custom tape pool.
--
-- /Note:/ Consider using 'poolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpPoolName :: Lens.Lens' CreateTapePool Types.PoolName
ctpPoolName = Lens.field @"poolName"
{-# DEPRECATED ctpPoolName "Use generic-lens or generic-optics with 'poolName' instead." #-}

-- | The storage class that is associated with the new custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpStorageClass :: Lens.Lens' CreateTapePool Types.TapeStorageClass
ctpStorageClass = Lens.field @"storageClass"
{-# DEPRECATED ctpStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
--
-- /Note:/ Consider using 'retentionLockTimeInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpRetentionLockTimeInDays :: Lens.Lens' CreateTapePool (Core.Maybe Core.Natural)
ctpRetentionLockTimeInDays = Lens.field @"retentionLockTimeInDays"
{-# DEPRECATED ctpRetentionLockTimeInDays "Use generic-lens or generic-optics with 'retentionLockTimeInDays' instead." #-}

-- | Tape retention lock can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
--
-- /Note:/ Consider using 'retentionLockType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpRetentionLockType :: Lens.Lens' CreateTapePool (Core.Maybe Types.RetentionLockType)
ctpRetentionLockType = Lens.field @"retentionLockType"
{-# DEPRECATED ctpRetentionLockType "Use generic-lens or generic-optics with 'retentionLockType' instead." #-}

-- | A list of up to 50 tags that can be assigned to tape pool. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpTags :: Lens.Lens' CreateTapePool (Core.Maybe [Types.Tag])
ctpTags = Lens.field @"tags"
{-# DEPRECATED ctpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateTapePool where
  toJSON CreateTapePool {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PoolName" Core..= poolName),
            Core.Just ("StorageClass" Core..= storageClass),
            ("RetentionLockTimeInDays" Core..=)
              Core.<$> retentionLockTimeInDays,
            ("RetentionLockType" Core..=) Core.<$> retentionLockType,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateTapePool where
  type Rs CreateTapePool = CreateTapePoolResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.CreateTapePool")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTapePoolResponse'
            Core.<$> (x Core..:? "PoolARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTapePoolResponse' smart constructor.
data CreateTapePoolResponse = CreateTapePoolResponse'
  { -- | The unique Amazon Resource Name (ARN) that represents the custom tape pool. Use the 'ListTapePools' operation to return a list of tape pools for your account and AWS Region.
    poolARN :: Core.Maybe Types.PoolARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTapePoolResponse' value with any optional fields omitted.
mkCreateTapePoolResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTapePoolResponse
mkCreateTapePoolResponse responseStatus =
  CreateTapePoolResponse' {poolARN = Core.Nothing, responseStatus}

-- | The unique Amazon Resource Name (ARN) that represents the custom tape pool. Use the 'ListTapePools' operation to return a list of tape pools for your account and AWS Region.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctprrsPoolARN :: Lens.Lens' CreateTapePoolResponse (Core.Maybe Types.PoolARN)
ctprrsPoolARN = Lens.field @"poolARN"
{-# DEPRECATED ctprrsPoolARN "Use generic-lens or generic-optics with 'poolARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctprrsResponseStatus :: Lens.Lens' CreateTapePoolResponse Core.Int
ctprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
