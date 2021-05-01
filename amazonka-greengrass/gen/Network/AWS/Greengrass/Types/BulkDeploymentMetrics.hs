{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeploymentMetrics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Relevant metrics on input records processed during bulk deployment.
--
-- /See:/ 'newBulkDeploymentMetrics' smart constructor.
data BulkDeploymentMetrics = BulkDeploymentMetrics'
  { -- | The total number of group records from the input file that have been
    -- processed so far, or attempted.
    recordsProcessed :: Prelude.Maybe Prelude.Int,
    -- | The total number of deployment attempts that returned a retryable error.
    -- For example, a retry is triggered if the attempt to deploy a group
    -- returns a throttling error. \'\'StartBulkDeployment\'\' retries a group
    -- deployment up to five times.
    retryAttempts :: Prelude.Maybe Prelude.Int,
    -- | The total number of records that returned a non-retryable error. For
    -- example, this can occur if a group record from the input file uses an
    -- invalid format or specifies a nonexistent group version, or if the
    -- execution role doesn\'t grant permission to deploy a group or group
    -- version.
    invalidInputRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BulkDeploymentMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordsProcessed', 'bulkDeploymentMetrics_recordsProcessed' - The total number of group records from the input file that have been
-- processed so far, or attempted.
--
-- 'retryAttempts', 'bulkDeploymentMetrics_retryAttempts' - The total number of deployment attempts that returned a retryable error.
-- For example, a retry is triggered if the attempt to deploy a group
-- returns a throttling error. \'\'StartBulkDeployment\'\' retries a group
-- deployment up to five times.
--
-- 'invalidInputRecords', 'bulkDeploymentMetrics_invalidInputRecords' - The total number of records that returned a non-retryable error. For
-- example, this can occur if a group record from the input file uses an
-- invalid format or specifies a nonexistent group version, or if the
-- execution role doesn\'t grant permission to deploy a group or group
-- version.
newBulkDeploymentMetrics ::
  BulkDeploymentMetrics
newBulkDeploymentMetrics =
  BulkDeploymentMetrics'
    { recordsProcessed =
        Prelude.Nothing,
      retryAttempts = Prelude.Nothing,
      invalidInputRecords = Prelude.Nothing
    }

-- | The total number of group records from the input file that have been
-- processed so far, or attempted.
bulkDeploymentMetrics_recordsProcessed :: Lens.Lens' BulkDeploymentMetrics (Prelude.Maybe Prelude.Int)
bulkDeploymentMetrics_recordsProcessed = Lens.lens (\BulkDeploymentMetrics' {recordsProcessed} -> recordsProcessed) (\s@BulkDeploymentMetrics' {} a -> s {recordsProcessed = a} :: BulkDeploymentMetrics)

-- | The total number of deployment attempts that returned a retryable error.
-- For example, a retry is triggered if the attempt to deploy a group
-- returns a throttling error. \'\'StartBulkDeployment\'\' retries a group
-- deployment up to five times.
bulkDeploymentMetrics_retryAttempts :: Lens.Lens' BulkDeploymentMetrics (Prelude.Maybe Prelude.Int)
bulkDeploymentMetrics_retryAttempts = Lens.lens (\BulkDeploymentMetrics' {retryAttempts} -> retryAttempts) (\s@BulkDeploymentMetrics' {} a -> s {retryAttempts = a} :: BulkDeploymentMetrics)

-- | The total number of records that returned a non-retryable error. For
-- example, this can occur if a group record from the input file uses an
-- invalid format or specifies a nonexistent group version, or if the
-- execution role doesn\'t grant permission to deploy a group or group
-- version.
bulkDeploymentMetrics_invalidInputRecords :: Lens.Lens' BulkDeploymentMetrics (Prelude.Maybe Prelude.Int)
bulkDeploymentMetrics_invalidInputRecords = Lens.lens (\BulkDeploymentMetrics' {invalidInputRecords} -> invalidInputRecords) (\s@BulkDeploymentMetrics' {} a -> s {invalidInputRecords = a} :: BulkDeploymentMetrics)

instance Prelude.FromJSON BulkDeploymentMetrics where
  parseJSON =
    Prelude.withObject
      "BulkDeploymentMetrics"
      ( \x ->
          BulkDeploymentMetrics'
            Prelude.<$> (x Prelude..:? "RecordsProcessed")
            Prelude.<*> (x Prelude..:? "RetryAttempts")
            Prelude.<*> (x Prelude..:? "InvalidInputRecords")
      )

instance Prelude.Hashable BulkDeploymentMetrics

instance Prelude.NFData BulkDeploymentMetrics
