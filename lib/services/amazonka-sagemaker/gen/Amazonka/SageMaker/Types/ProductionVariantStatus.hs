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
-- Module      : Amazonka.SageMaker.Types.ProductionVariantStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProductionVariantStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.VariantStatus

-- | Describes the status of the production variant.
--
-- /See:/ 'newProductionVariantStatus' smart constructor.
data ProductionVariantStatus = ProductionVariantStatus'
  { -- | The start time of the current status change.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | A message that describes the status of the production variant.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The endpoint variant status which describes the current deployment stage
    -- status or operational status.
    --
    -- -   @Creating@: Creating inference resources for the production variant.
    --
    -- -   @Deleting@: Terminating inference resources for the production
    --     variant.
    --
    -- -   @Updating@: Updating capacity for the production variant.
    --
    -- -   @ActivatingTraffic@: Turning on traffic for the production variant.
    --
    -- -   @Baking@: Waiting period to monitor the CloudWatch alarms in the
    --     automatic rollback configuration.
    status :: VariantStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductionVariantStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'productionVariantStatus_startTime' - The start time of the current status change.
--
-- 'statusMessage', 'productionVariantStatus_statusMessage' - A message that describes the status of the production variant.
--
-- 'status', 'productionVariantStatus_status' - The endpoint variant status which describes the current deployment stage
-- status or operational status.
--
-- -   @Creating@: Creating inference resources for the production variant.
--
-- -   @Deleting@: Terminating inference resources for the production
--     variant.
--
-- -   @Updating@: Updating capacity for the production variant.
--
-- -   @ActivatingTraffic@: Turning on traffic for the production variant.
--
-- -   @Baking@: Waiting period to monitor the CloudWatch alarms in the
--     automatic rollback configuration.
newProductionVariantStatus ::
  -- | 'status'
  VariantStatus ->
  ProductionVariantStatus
newProductionVariantStatus pStatus_ =
  ProductionVariantStatus'
    { startTime =
        Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      status = pStatus_
    }

-- | The start time of the current status change.
productionVariantStatus_startTime :: Lens.Lens' ProductionVariantStatus (Prelude.Maybe Prelude.UTCTime)
productionVariantStatus_startTime = Lens.lens (\ProductionVariantStatus' {startTime} -> startTime) (\s@ProductionVariantStatus' {} a -> s {startTime = a} :: ProductionVariantStatus) Prelude.. Lens.mapping Data._Time

-- | A message that describes the status of the production variant.
productionVariantStatus_statusMessage :: Lens.Lens' ProductionVariantStatus (Prelude.Maybe Prelude.Text)
productionVariantStatus_statusMessage = Lens.lens (\ProductionVariantStatus' {statusMessage} -> statusMessage) (\s@ProductionVariantStatus' {} a -> s {statusMessage = a} :: ProductionVariantStatus)

-- | The endpoint variant status which describes the current deployment stage
-- status or operational status.
--
-- -   @Creating@: Creating inference resources for the production variant.
--
-- -   @Deleting@: Terminating inference resources for the production
--     variant.
--
-- -   @Updating@: Updating capacity for the production variant.
--
-- -   @ActivatingTraffic@: Turning on traffic for the production variant.
--
-- -   @Baking@: Waiting period to monitor the CloudWatch alarms in the
--     automatic rollback configuration.
productionVariantStatus_status :: Lens.Lens' ProductionVariantStatus VariantStatus
productionVariantStatus_status = Lens.lens (\ProductionVariantStatus' {status} -> status) (\s@ProductionVariantStatus' {} a -> s {status = a} :: ProductionVariantStatus)

instance Data.FromJSON ProductionVariantStatus where
  parseJSON =
    Data.withObject
      "ProductionVariantStatus"
      ( \x ->
          ProductionVariantStatus'
            Prelude.<$> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable ProductionVariantStatus where
  hashWithSalt _salt ProductionVariantStatus' {..} =
    _salt
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` status

instance Prelude.NFData ProductionVariantStatus where
  rnf ProductionVariantStatus' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf status
