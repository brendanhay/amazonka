{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.BulkDeploymentMetrics
  ( BulkDeploymentMetrics (..)
  -- * Smart constructor
  , mkBulkDeploymentMetrics
  -- * Lenses
  , bdmInvalidInputRecords
  , bdmRecordsProcessed
  , bdmRetryAttempts
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Relevant metrics on input records processed during bulk deployment.
--
-- /See:/ 'mkBulkDeploymentMetrics' smart constructor.
data BulkDeploymentMetrics = BulkDeploymentMetrics'
  { invalidInputRecords :: Core.Maybe Core.Int
    -- ^ The total number of records that returned a non-retryable error. For example, this can occur if a group record from the input file uses an invalid format or specifies a nonexistent group version, or if the execution role doesn't grant permission to deploy a group or group version.
  , recordsProcessed :: Core.Maybe Core.Int
    -- ^ The total number of group records from the input file that have been processed so far, or attempted.
  , retryAttempts :: Core.Maybe Core.Int
    -- ^ The total number of deployment attempts that returned a retryable error. For example, a retry is triggered if the attempt to deploy a group returns a throttling error. ''StartBulkDeployment'' retries a group deployment up to five times.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BulkDeploymentMetrics' value with any optional fields omitted.
mkBulkDeploymentMetrics
    :: BulkDeploymentMetrics
mkBulkDeploymentMetrics
  = BulkDeploymentMetrics'{invalidInputRecords = Core.Nothing,
                           recordsProcessed = Core.Nothing, retryAttempts = Core.Nothing}

-- | The total number of records that returned a non-retryable error. For example, this can occur if a group record from the input file uses an invalid format or specifies a nonexistent group version, or if the execution role doesn't grant permission to deploy a group or group version.
--
-- /Note:/ Consider using 'invalidInputRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmInvalidInputRecords :: Lens.Lens' BulkDeploymentMetrics (Core.Maybe Core.Int)
bdmInvalidInputRecords = Lens.field @"invalidInputRecords"
{-# INLINEABLE bdmInvalidInputRecords #-}
{-# DEPRECATED invalidInputRecords "Use generic-lens or generic-optics with 'invalidInputRecords' instead"  #-}

-- | The total number of group records from the input file that have been processed so far, or attempted.
--
-- /Note:/ Consider using 'recordsProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmRecordsProcessed :: Lens.Lens' BulkDeploymentMetrics (Core.Maybe Core.Int)
bdmRecordsProcessed = Lens.field @"recordsProcessed"
{-# INLINEABLE bdmRecordsProcessed #-}
{-# DEPRECATED recordsProcessed "Use generic-lens or generic-optics with 'recordsProcessed' instead"  #-}

-- | The total number of deployment attempts that returned a retryable error. For example, a retry is triggered if the attempt to deploy a group returns a throttling error. ''StartBulkDeployment'' retries a group deployment up to five times.
--
-- /Note:/ Consider using 'retryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmRetryAttempts :: Lens.Lens' BulkDeploymentMetrics (Core.Maybe Core.Int)
bdmRetryAttempts = Lens.field @"retryAttempts"
{-# INLINEABLE bdmRetryAttempts #-}
{-# DEPRECATED retryAttempts "Use generic-lens or generic-optics with 'retryAttempts' instead"  #-}

instance Core.FromJSON BulkDeploymentMetrics where
        parseJSON
          = Core.withObject "BulkDeploymentMetrics" Core.$
              \ x ->
                BulkDeploymentMetrics' Core.<$>
                  (x Core..:? "InvalidInputRecords") Core.<*>
                    x Core..:? "RecordsProcessed"
                    Core.<*> x Core..:? "RetryAttempts"
