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
-- Module      : Amazonka.GreengrassV2.Types.IoTJobAbortCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.IoTJobAbortCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GreengrassV2.Types.IoTJobAbortAction
import Amazonka.GreengrassV2.Types.IoTJobExecutionFailureType
import qualified Amazonka.Prelude as Prelude

-- | Contains criteria that define when and how to cancel a job.
--
-- The deployment stops if the following conditions are true:
--
-- 1.  The number of things that receive the deployment exceeds the
--     @minNumberOfExecutedThings@.
--
-- 2.  The percentage of failures with type @failureType@ exceeds the
--     @thresholdPercentage@.
--
-- /See:/ 'newIoTJobAbortCriteria' smart constructor.
data IoTJobAbortCriteria = IoTJobAbortCriteria'
  { -- | The type of job deployment failure that can cancel a job.
    failureType :: IoTJobExecutionFailureType,
    -- | The action to perform when the criteria are met.
    action :: IoTJobAbortAction,
    -- | The minimum percentage of @failureType@ failures that occur before the
    -- job can cancel.
    --
    -- This parameter supports up to two digits after the decimal (for example,
    -- you can specify @10.9@ or @10.99@, but not @10.999@).
    thresholdPercentage :: Prelude.Double,
    -- | The minimum number of things that receive the configuration before the
    -- job can cancel.
    minNumberOfExecutedThings :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IoTJobAbortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureType', 'ioTJobAbortCriteria_failureType' - The type of job deployment failure that can cancel a job.
--
-- 'action', 'ioTJobAbortCriteria_action' - The action to perform when the criteria are met.
--
-- 'thresholdPercentage', 'ioTJobAbortCriteria_thresholdPercentage' - The minimum percentage of @failureType@ failures that occur before the
-- job can cancel.
--
-- This parameter supports up to two digits after the decimal (for example,
-- you can specify @10.9@ or @10.99@, but not @10.999@).
--
-- 'minNumberOfExecutedThings', 'ioTJobAbortCriteria_minNumberOfExecutedThings' - The minimum number of things that receive the configuration before the
-- job can cancel.
newIoTJobAbortCriteria ::
  -- | 'failureType'
  IoTJobExecutionFailureType ->
  -- | 'action'
  IoTJobAbortAction ->
  -- | 'thresholdPercentage'
  Prelude.Double ->
  -- | 'minNumberOfExecutedThings'
  Prelude.Natural ->
  IoTJobAbortCriteria
newIoTJobAbortCriteria
  pFailureType_
  pAction_
  pThresholdPercentage_
  pMinNumberOfExecutedThings_ =
    IoTJobAbortCriteria'
      { failureType = pFailureType_,
        action = pAction_,
        thresholdPercentage = pThresholdPercentage_,
        minNumberOfExecutedThings =
          pMinNumberOfExecutedThings_
      }

-- | The type of job deployment failure that can cancel a job.
ioTJobAbortCriteria_failureType :: Lens.Lens' IoTJobAbortCriteria IoTJobExecutionFailureType
ioTJobAbortCriteria_failureType = Lens.lens (\IoTJobAbortCriteria' {failureType} -> failureType) (\s@IoTJobAbortCriteria' {} a -> s {failureType = a} :: IoTJobAbortCriteria)

-- | The action to perform when the criteria are met.
ioTJobAbortCriteria_action :: Lens.Lens' IoTJobAbortCriteria IoTJobAbortAction
ioTJobAbortCriteria_action = Lens.lens (\IoTJobAbortCriteria' {action} -> action) (\s@IoTJobAbortCriteria' {} a -> s {action = a} :: IoTJobAbortCriteria)

-- | The minimum percentage of @failureType@ failures that occur before the
-- job can cancel.
--
-- This parameter supports up to two digits after the decimal (for example,
-- you can specify @10.9@ or @10.99@, but not @10.999@).
ioTJobAbortCriteria_thresholdPercentage :: Lens.Lens' IoTJobAbortCriteria Prelude.Double
ioTJobAbortCriteria_thresholdPercentage = Lens.lens (\IoTJobAbortCriteria' {thresholdPercentage} -> thresholdPercentage) (\s@IoTJobAbortCriteria' {} a -> s {thresholdPercentage = a} :: IoTJobAbortCriteria)

-- | The minimum number of things that receive the configuration before the
-- job can cancel.
ioTJobAbortCriteria_minNumberOfExecutedThings :: Lens.Lens' IoTJobAbortCriteria Prelude.Natural
ioTJobAbortCriteria_minNumberOfExecutedThings = Lens.lens (\IoTJobAbortCriteria' {minNumberOfExecutedThings} -> minNumberOfExecutedThings) (\s@IoTJobAbortCriteria' {} a -> s {minNumberOfExecutedThings = a} :: IoTJobAbortCriteria)

instance Core.FromJSON IoTJobAbortCriteria where
  parseJSON =
    Core.withObject
      "IoTJobAbortCriteria"
      ( \x ->
          IoTJobAbortCriteria'
            Prelude.<$> (x Core..: "failureType")
            Prelude.<*> (x Core..: "action")
            Prelude.<*> (x Core..: "thresholdPercentage")
            Prelude.<*> (x Core..: "minNumberOfExecutedThings")
      )

instance Prelude.Hashable IoTJobAbortCriteria where
  hashWithSalt _salt IoTJobAbortCriteria' {..} =
    _salt `Prelude.hashWithSalt` failureType
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` thresholdPercentage
      `Prelude.hashWithSalt` minNumberOfExecutedThings

instance Prelude.NFData IoTJobAbortCriteria where
  rnf IoTJobAbortCriteria' {..} =
    Prelude.rnf failureType
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf thresholdPercentage
      `Prelude.seq` Prelude.rnf minNumberOfExecutedThings

instance Core.ToJSON IoTJobAbortCriteria where
  toJSON IoTJobAbortCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("failureType" Core..= failureType),
            Prelude.Just ("action" Core..= action),
            Prelude.Just
              ("thresholdPercentage" Core..= thresholdPercentage),
            Prelude.Just
              ( "minNumberOfExecutedThings"
                  Core..= minNumberOfExecutedThings
              )
          ]
      )
