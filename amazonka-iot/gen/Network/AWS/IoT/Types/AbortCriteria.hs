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
-- Module      : Network.AWS.IoT.Types.AbortCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AbortCriteria where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AbortAction
import Network.AWS.IoT.Types.JobExecutionFailureType
import qualified Network.AWS.Lens as Lens

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
    -- AWS IoT supports up to two digits after the decimal (for example, 10.9
    -- and 10.99, but not 10.999).
    thresholdPercentage :: Core.Double,
    -- | The minimum number of things which must receive job execution
    -- notifications before the job can be aborted.
    minNumberOfExecutedThings :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- AWS IoT supports up to two digits after the decimal (for example, 10.9
-- and 10.99, but not 10.999).
--
-- 'minNumberOfExecutedThings', 'abortCriteria_minNumberOfExecutedThings' - The minimum number of things which must receive job execution
-- notifications before the job can be aborted.
newAbortCriteria ::
  -- | 'failureType'
  JobExecutionFailureType ->
  -- | 'action'
  AbortAction ->
  -- | 'thresholdPercentage'
  Core.Double ->
  -- | 'minNumberOfExecutedThings'
  Core.Natural ->
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
-- AWS IoT supports up to two digits after the decimal (for example, 10.9
-- and 10.99, but not 10.999).
abortCriteria_thresholdPercentage :: Lens.Lens' AbortCriteria Core.Double
abortCriteria_thresholdPercentage = Lens.lens (\AbortCriteria' {thresholdPercentage} -> thresholdPercentage) (\s@AbortCriteria' {} a -> s {thresholdPercentage = a} :: AbortCriteria)

-- | The minimum number of things which must receive job execution
-- notifications before the job can be aborted.
abortCriteria_minNumberOfExecutedThings :: Lens.Lens' AbortCriteria Core.Natural
abortCriteria_minNumberOfExecutedThings = Lens.lens (\AbortCriteria' {minNumberOfExecutedThings} -> minNumberOfExecutedThings) (\s@AbortCriteria' {} a -> s {minNumberOfExecutedThings = a} :: AbortCriteria)

instance Core.FromJSON AbortCriteria where
  parseJSON =
    Core.withObject
      "AbortCriteria"
      ( \x ->
          AbortCriteria'
            Core.<$> (x Core..: "failureType")
            Core.<*> (x Core..: "action")
            Core.<*> (x Core..: "thresholdPercentage")
            Core.<*> (x Core..: "minNumberOfExecutedThings")
      )

instance Core.Hashable AbortCriteria

instance Core.NFData AbortCriteria

instance Core.ToJSON AbortCriteria where
  toJSON AbortCriteria' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("failureType" Core..= failureType),
            Core.Just ("action" Core..= action),
            Core.Just
              ("thresholdPercentage" Core..= thresholdPercentage),
            Core.Just
              ( "minNumberOfExecutedThings"
                  Core..= minNumberOfExecutedThings
              )
          ]
      )
