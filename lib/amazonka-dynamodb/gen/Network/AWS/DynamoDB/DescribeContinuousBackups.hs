{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeContinuousBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the status of continuous backups and point in time recovery on the specified table. Continuous backups are @ENABLED@ on all tables at table creation. If point in time recovery is enabled, @PointInTimeRecoveryStatus@ will be set to ENABLED.
--
-- After continuous backups and point in time recovery are enabled, you can restore to any point in time within @EarliestRestorableDateTime@ and @LatestRestorableDateTime@ . 
-- @LatestRestorableDateTime@ is typically 5 minutes before the current time. You can restore your table to any point in time during the last 35 days. 
-- You can call @DescribeContinuousBackups@ at a maximum rate of 10 times per second.
module Network.AWS.DynamoDB.DescribeContinuousBackups
    (
    -- * Creating a request
      DescribeContinuousBackups (..)
    , mkDescribeContinuousBackups
    -- ** Request lenses
    , dcbTableName

    -- * Destructuring the response
    , DescribeContinuousBackupsResponse (..)
    , mkDescribeContinuousBackupsResponse
    -- ** Response lenses
    , dcbrrsContinuousBackupsDescription
    , dcbrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeContinuousBackups' smart constructor.
newtype DescribeContinuousBackups = DescribeContinuousBackups'
  { tableName :: Types.TableName
    -- ^ Name of the table for which the customer wants to check the continuous backups and point in time recovery settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContinuousBackups' value with any optional fields omitted.
mkDescribeContinuousBackups
    :: Types.TableName -- ^ 'tableName'
    -> DescribeContinuousBackups
mkDescribeContinuousBackups tableName
  = DescribeContinuousBackups'{tableName}

-- | Name of the table for which the customer wants to check the continuous backups and point in time recovery settings.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbTableName :: Lens.Lens' DescribeContinuousBackups Types.TableName
dcbTableName = Lens.field @"tableName"
{-# INLINEABLE dcbTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

instance Core.ToQuery DescribeContinuousBackups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeContinuousBackups where
        toHeaders DescribeContinuousBackups{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDB_20120810.DescribeContinuousBackups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DescribeContinuousBackups where
        toJSON DescribeContinuousBackups{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TableName" Core..= tableName)])

instance Core.AWSRequest DescribeContinuousBackups where
        type Rs DescribeContinuousBackups =
             DescribeContinuousBackupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeContinuousBackupsResponse' Core.<$>
                   (x Core..:? "ContinuousBackupsDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeContinuousBackupsResponse' smart constructor.
data DescribeContinuousBackupsResponse = DescribeContinuousBackupsResponse'
  { continuousBackupsDescription :: Core.Maybe Types.ContinuousBackupsDescription
    -- ^ Represents the continuous backups and point in time recovery settings on the table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeContinuousBackupsResponse' value with any optional fields omitted.
mkDescribeContinuousBackupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeContinuousBackupsResponse
mkDescribeContinuousBackupsResponse responseStatus
  = DescribeContinuousBackupsResponse'{continuousBackupsDescription =
                                         Core.Nothing,
                                       responseStatus}

-- | Represents the continuous backups and point in time recovery settings on the table.
--
-- /Note:/ Consider using 'continuousBackupsDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrrsContinuousBackupsDescription :: Lens.Lens' DescribeContinuousBackupsResponse (Core.Maybe Types.ContinuousBackupsDescription)
dcbrrsContinuousBackupsDescription = Lens.field @"continuousBackupsDescription"
{-# INLINEABLE dcbrrsContinuousBackupsDescription #-}
{-# DEPRECATED continuousBackupsDescription "Use generic-lens or generic-optics with 'continuousBackupsDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrrsResponseStatus :: Lens.Lens' DescribeContinuousBackupsResponse Core.Int
dcbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
