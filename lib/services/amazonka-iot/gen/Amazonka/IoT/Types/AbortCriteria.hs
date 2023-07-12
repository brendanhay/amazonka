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
-- Module      : Amazonka.IoT.Types.AbortCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AbortCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AbortAction
import Amazonka.IoT.Types.JobExecutionFailureType
import qualified Amazonka.Prelude as Prelude

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'newAbortCriteria' smart constructor.
data AbortCriteria = AbortCriteria'
  { -- | The type of job execution failures that can initiate a job abort.
    failureType :: JobExecutionFailureType,
    -- | The type of job action to take to initiate the job abort.
    action :: AbortAction,
    -- | The minimum percentage of job execution failures that must occur to
    -- initiate the job abort.
    --
    -- Amazon Web Services IoT Core supports up to two digits after the decimal
    -- (for example, 10.9 and 10.99, but not 10.999).
    thresholdPercentage :: Prelude.Double,
    -- | The minimum number of things which must receive job execution
    -- notifications before the job can be aborted.
    minNumberOfExecutedThings :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureType', 'abortCriteria_failureType' - The type of job execution failures that can initiate a job abort.
--
-- 'action', 'abortCriteria_action' - The type of job action to take to initiate the job abort.
--
-- 'thresholdPercentage', 'abortCriteria_thresholdPercentage' - The minimum percentage of job execution failures that must occur to
-- initiate the job abort.
--
-- Amazon Web Services IoT Core supports up to two digits after the decimal
-- (for example, 10.9 and 10.99, but not 10.999).
--
-- 'minNumberOfExecutedThings', 'abortCriteria_minNumberOfExecutedThings' - The minimum number of things which must receive job execution
-- notifications before the job can be aborted.
newAbortCriteria ::
  -- | 'failureType'
  JobExecutionFailureType ->
  -- | 'action'
  AbortAction ->
  -- | 'thresholdPercentage'
  Prelude.Double ->
  -- | 'minNumberOfExecutedThings'
  Prelude.Natural ->
  AbortCriteria
newAbortCriteria
  pFailureType_
  pAction_
  pThresholdPercentage_
  pMinNumberOfExecutedThings_ =
    AbortCriteria'
      { failureType = pFailureType_,
        action = pAction_,
        thresholdPercentage = pThresholdPercentage_,
        minNumberOfExecutedThings =
          pMinNumberOfExecutedThings_
      }

-- | The type of job execution failures that can initiate a job abort.
abortCriteria_failureType :: Lens.Lens' AbortCriteria JobExecutionFailureType
abortCriteria_failureType = Lens.lens (\AbortCriteria' {failureType} -> failureType) (\s@AbortCriteria' {} a -> s {failureType = a} :: AbortCriteria)

-- | The type of job action to take to initiate the job abort.
abortCriteria_action :: Lens.Lens' AbortCriteria AbortAction
abortCriteria_action = Lens.lens (\AbortCriteria' {action} -> action) (\s@AbortCriteria' {} a -> s {action = a} :: AbortCriteria)

-- | The minimum percentage of job execution failures that must occur to
-- initiate the job abort.
--
-- Amazon Web Services IoT Core supports up to two digits after the decimal
-- (for example, 10.9 and 10.99, but not 10.999).
abortCriteria_thresholdPercentage :: Lens.Lens' AbortCriteria Prelude.Double
abortCriteria_thresholdPercentage = Lens.lens (\AbortCriteria' {thresholdPercentage} -> thresholdPercentage) (\s@AbortCriteria' {} a -> s {thresholdPercentage = a} :: AbortCriteria)

-- | The minimum number of things which must receive job execution
-- notifications before the job can be aborted.
abortCriteria_minNumberOfExecutedThings :: Lens.Lens' AbortCriteria Prelude.Natural
abortCriteria_minNumberOfExecutedThings = Lens.lens (\AbortCriteria' {minNumberOfExecutedThings} -> minNumberOfExecutedThings) (\s@AbortCriteria' {} a -> s {minNumberOfExecutedThings = a} :: AbortCriteria)

instance Data.FromJSON AbortCriteria where
  parseJSON =
    Data.withObject
      "AbortCriteria"
      ( \x ->
          AbortCriteria'
            Prelude.<$> (x Data..: "failureType")
            Prelude.<*> (x Data..: "action")
            Prelude.<*> (x Data..: "thresholdPercentage")
            Prelude.<*> (x Data..: "minNumberOfExecutedThings")
      )

instance Prelude.Hashable AbortCriteria where
  hashWithSalt _salt AbortCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` failureType
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` thresholdPercentage
      `Prelude.hashWithSalt` minNumberOfExecutedThings

instance Prelude.NFData AbortCriteria where
  rnf AbortCriteria' {..} =
    Prelude.rnf failureType
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf thresholdPercentage
      `Prelude.seq` Prelude.rnf minNumberOfExecutedThings

instance Data.ToJSON AbortCriteria where
  toJSON AbortCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("failureType" Data..= failureType),
            Prelude.Just ("action" Data..= action),
            Prelude.Just
              ("thresholdPercentage" Data..= thresholdPercentage),
            Prelude.Just
              ( "minNumberOfExecutedThings"
                  Data..= minNumberOfExecutedThings
              )
          ]
      )
