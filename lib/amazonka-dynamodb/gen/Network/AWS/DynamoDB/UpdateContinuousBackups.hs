{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateContinuousBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @UpdateContinuousBackups@ enables or disables point in time recovery for the specified table. A successful @UpdateContinuousBackups@ call returns the current @ContinuousBackupsDescription@ . Continuous backups are @ENABLED@ on all tables at table creation. If point in time recovery is enabled, @PointInTimeRecoveryStatus@ will be set to ENABLED.
--
-- Once continuous backups and point in time recovery are enabled, you can restore to any point in time within @EarliestRestorableDateTime@ and @LatestRestorableDateTime@ . 
-- @LatestRestorableDateTime@ is typically 5 minutes before the current time. You can restore your table to any point in time during the last 35 days. 
module Network.AWS.DynamoDB.UpdateContinuousBackups
    (
    -- * Creating a request
      UpdateContinuousBackups (..)
    , mkUpdateContinuousBackups
    -- ** Request lenses
    , ucbTableName
    , ucbPointInTimeRecoverySpecification

    -- * Destructuring the response
    , UpdateContinuousBackupsResponse (..)
    , mkUpdateContinuousBackupsResponse
    -- ** Response lenses
    , ucbrrsContinuousBackupsDescription
    , ucbrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateContinuousBackups' smart constructor.
data UpdateContinuousBackups = UpdateContinuousBackups'
  { tableName :: Types.TableName
    -- ^ The name of the table.
  , pointInTimeRecoverySpecification :: Types.PointInTimeRecoverySpecification
    -- ^ Represents the settings used to enable point in time recovery.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContinuousBackups' value with any optional fields omitted.
mkUpdateContinuousBackups
    :: Types.TableName -- ^ 'tableName'
    -> Types.PointInTimeRecoverySpecification -- ^ 'pointInTimeRecoverySpecification'
    -> UpdateContinuousBackups
mkUpdateContinuousBackups tableName
  pointInTimeRecoverySpecification
  = UpdateContinuousBackups'{tableName,
                             pointInTimeRecoverySpecification}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucbTableName :: Lens.Lens' UpdateContinuousBackups Types.TableName
ucbTableName = Lens.field @"tableName"
{-# INLINEABLE ucbTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | Represents the settings used to enable point in time recovery.
--
-- /Note:/ Consider using 'pointInTimeRecoverySpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucbPointInTimeRecoverySpecification :: Lens.Lens' UpdateContinuousBackups Types.PointInTimeRecoverySpecification
ucbPointInTimeRecoverySpecification = Lens.field @"pointInTimeRecoverySpecification"
{-# INLINEABLE ucbPointInTimeRecoverySpecification #-}
{-# DEPRECATED pointInTimeRecoverySpecification "Use generic-lens or generic-optics with 'pointInTimeRecoverySpecification' instead"  #-}

instance Core.ToQuery UpdateContinuousBackups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateContinuousBackups where
        toHeaders UpdateContinuousBackups{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDB_20120810.UpdateContinuousBackups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON UpdateContinuousBackups where
        toJSON UpdateContinuousBackups{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TableName" Core..= tableName),
                  Core.Just
                    ("PointInTimeRecoverySpecification" Core..=
                       pointInTimeRecoverySpecification)])

instance Core.AWSRequest UpdateContinuousBackups where
        type Rs UpdateContinuousBackups = UpdateContinuousBackupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateContinuousBackupsResponse' Core.<$>
                   (x Core..:? "ContinuousBackupsDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateContinuousBackupsResponse' smart constructor.
data UpdateContinuousBackupsResponse = UpdateContinuousBackupsResponse'
  { continuousBackupsDescription :: Core.Maybe Types.ContinuousBackupsDescription
    -- ^ Represents the continuous backups and point in time recovery settings on the table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateContinuousBackupsResponse' value with any optional fields omitted.
mkUpdateContinuousBackupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateContinuousBackupsResponse
mkUpdateContinuousBackupsResponse responseStatus
  = UpdateContinuousBackupsResponse'{continuousBackupsDescription =
                                       Core.Nothing,
                                     responseStatus}

-- | Represents the continuous backups and point in time recovery settings on the table.
--
-- /Note:/ Consider using 'continuousBackupsDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucbrrsContinuousBackupsDescription :: Lens.Lens' UpdateContinuousBackupsResponse (Core.Maybe Types.ContinuousBackupsDescription)
ucbrrsContinuousBackupsDescription = Lens.field @"continuousBackupsDescription"
{-# INLINEABLE ucbrrsContinuousBackupsDescription #-}
{-# DEPRECATED continuousBackupsDescription "Use generic-lens or generic-optics with 'continuousBackupsDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucbrrsResponseStatus :: Lens.Lens' UpdateContinuousBackupsResponse Core.Int
ucbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
