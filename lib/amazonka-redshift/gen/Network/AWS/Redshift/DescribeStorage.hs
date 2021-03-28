{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns account level backups storage size and provisional storage.
module Network.AWS.Redshift.DescribeStorage
    (
    -- * Creating a request
      DescribeStorage (..)
    , mkDescribeStorage

    -- * Destructuring the response
    , DescribeStorageResponse (..)
    , mkDescribeStorageResponse
    -- ** Response lenses
    , dsrrsTotalBackupSizeInMegaBytes
    , dsrrsTotalProvisionedStorageInMegaBytes
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStorage' smart constructor.
data DescribeStorage = DescribeStorage'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStorage' value with any optional fields omitted.
mkDescribeStorage
    :: DescribeStorage
mkDescribeStorage = DescribeStorage'

instance Core.ToQuery DescribeStorage where
        toQuery DescribeStorage{..}
          = Core.toQueryPair "Action" ("DescribeStorage" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)

instance Core.ToHeaders DescribeStorage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeStorage where
        type Rs DescribeStorage = DescribeStorageResponse
        toRequest x@_
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
          = Response.receiveXMLWrapper "DescribeStorageResult"
              (\ s h x ->
                 DescribeStorageResponse' Core.<$>
                   (x Core..@? "TotalBackupSizeInMegaBytes") Core.<*>
                     x Core..@? "TotalProvisionedStorageInMegaBytes"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeStorageResponse' smart constructor.
data DescribeStorageResponse = DescribeStorageResponse'
  { totalBackupSizeInMegaBytes :: Core.Maybe Core.Double
    -- ^ The total amount of storage currently used for snapshots.
  , totalProvisionedStorageInMegaBytes :: Core.Maybe Core.Double
    -- ^ The total amount of storage currently provisioned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStorageResponse' value with any optional fields omitted.
mkDescribeStorageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeStorageResponse
mkDescribeStorageResponse responseStatus
  = DescribeStorageResponse'{totalBackupSizeInMegaBytes =
                               Core.Nothing,
                             totalProvisionedStorageInMegaBytes = Core.Nothing, responseStatus}

-- | The total amount of storage currently used for snapshots.
--
-- /Note:/ Consider using 'totalBackupSizeInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsTotalBackupSizeInMegaBytes :: Lens.Lens' DescribeStorageResponse (Core.Maybe Core.Double)
dsrrsTotalBackupSizeInMegaBytes = Lens.field @"totalBackupSizeInMegaBytes"
{-# INLINEABLE dsrrsTotalBackupSizeInMegaBytes #-}
{-# DEPRECATED totalBackupSizeInMegaBytes "Use generic-lens or generic-optics with 'totalBackupSizeInMegaBytes' instead"  #-}

-- | The total amount of storage currently provisioned.
--
-- /Note:/ Consider using 'totalProvisionedStorageInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsTotalProvisionedStorageInMegaBytes :: Lens.Lens' DescribeStorageResponse (Core.Maybe Core.Double)
dsrrsTotalProvisionedStorageInMegaBytes = Lens.field @"totalProvisionedStorageInMegaBytes"
{-# INLINEABLE dsrrsTotalProvisionedStorageInMegaBytes #-}
{-# DEPRECATED totalProvisionedStorageInMegaBytes "Use generic-lens or generic-optics with 'totalProvisionedStorageInMegaBytes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeStorageResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
