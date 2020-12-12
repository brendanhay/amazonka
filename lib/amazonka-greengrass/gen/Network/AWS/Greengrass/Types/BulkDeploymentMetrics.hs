{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeploymentMetrics
  ( BulkDeploymentMetrics (..),

    -- * Smart constructor
    mkBulkDeploymentMetrics,

    -- * Lenses
    bdmRecordsProcessed,
    bdmRetryAttempts,
    bdmInvalidInputRecords,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Relevant metrics on input records processed during bulk deployment.
--
-- /See:/ 'mkBulkDeploymentMetrics' smart constructor.
data BulkDeploymentMetrics = BulkDeploymentMetrics'
  { recordsProcessed ::
      Lude.Maybe Lude.Int,
    retryAttempts :: Lude.Maybe Lude.Int,
    invalidInputRecords :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BulkDeploymentMetrics' with the minimum fields required to make a request.
--
-- * 'invalidInputRecords' - The total number of records that returned a non-retryable error. For example, this can occur if a group record from the input file uses an invalid format or specifies a nonexistent group version, or if the execution role doesn't grant permission to deploy a group or group version.
-- * 'recordsProcessed' - The total number of group records from the input file that have been processed so far, or attempted.
-- * 'retryAttempts' - The total number of deployment attempts that returned a retryable error. For example, a retry is triggered if the attempt to deploy a group returns a throttling error. ''StartBulkDeployment'' retries a group deployment up to five times.
mkBulkDeploymentMetrics ::
  BulkDeploymentMetrics
mkBulkDeploymentMetrics =
  BulkDeploymentMetrics'
    { recordsProcessed = Lude.Nothing,
      retryAttempts = Lude.Nothing,
      invalidInputRecords = Lude.Nothing
    }

-- | The total number of group records from the input file that have been processed so far, or attempted.
--
-- /Note:/ Consider using 'recordsProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmRecordsProcessed :: Lens.Lens' BulkDeploymentMetrics (Lude.Maybe Lude.Int)
bdmRecordsProcessed = Lens.lens (recordsProcessed :: BulkDeploymentMetrics -> Lude.Maybe Lude.Int) (\s a -> s {recordsProcessed = a} :: BulkDeploymentMetrics)
{-# DEPRECATED bdmRecordsProcessed "Use generic-lens or generic-optics with 'recordsProcessed' instead." #-}

-- | The total number of deployment attempts that returned a retryable error. For example, a retry is triggered if the attempt to deploy a group returns a throttling error. ''StartBulkDeployment'' retries a group deployment up to five times.
--
-- /Note:/ Consider using 'retryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmRetryAttempts :: Lens.Lens' BulkDeploymentMetrics (Lude.Maybe Lude.Int)
bdmRetryAttempts = Lens.lens (retryAttempts :: BulkDeploymentMetrics -> Lude.Maybe Lude.Int) (\s a -> s {retryAttempts = a} :: BulkDeploymentMetrics)
{-# DEPRECATED bdmRetryAttempts "Use generic-lens or generic-optics with 'retryAttempts' instead." #-}

-- | The total number of records that returned a non-retryable error. For example, this can occur if a group record from the input file uses an invalid format or specifies a nonexistent group version, or if the execution role doesn't grant permission to deploy a group or group version.
--
-- /Note:/ Consider using 'invalidInputRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmInvalidInputRecords :: Lens.Lens' BulkDeploymentMetrics (Lude.Maybe Lude.Int)
bdmInvalidInputRecords = Lens.lens (invalidInputRecords :: BulkDeploymentMetrics -> Lude.Maybe Lude.Int) (\s a -> s {invalidInputRecords = a} :: BulkDeploymentMetrics)
{-# DEPRECATED bdmInvalidInputRecords "Use generic-lens or generic-optics with 'invalidInputRecords' instead." #-}

instance Lude.FromJSON BulkDeploymentMetrics where
  parseJSON =
    Lude.withObject
      "BulkDeploymentMetrics"
      ( \x ->
          BulkDeploymentMetrics'
            Lude.<$> (x Lude..:? "RecordsProcessed")
            Lude.<*> (x Lude..:? "RetryAttempts")
            Lude.<*> (x Lude..:? "InvalidInputRecords")
      )
