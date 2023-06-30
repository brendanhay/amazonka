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
-- Module      : Amazonka.IoT.Types.AwsJobAbortCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AwsJobAbortCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AwsJobAbortCriteriaAbortAction
import Amazonka.IoT.Types.AwsJobAbortCriteriaFailureType
import qualified Amazonka.Prelude as Prelude

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'newAwsJobAbortCriteria' smart constructor.
data AwsJobAbortCriteria = AwsJobAbortCriteria'
  { -- | The type of job execution failures that can initiate a job abort.
    failureType :: AwsJobAbortCriteriaFailureType,
    -- | The type of job action to take to initiate the job abort.
    action :: AwsJobAbortCriteriaAbortAction,
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
-- Create a value of 'AwsJobAbortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureType', 'awsJobAbortCriteria_failureType' - The type of job execution failures that can initiate a job abort.
--
-- 'action', 'awsJobAbortCriteria_action' - The type of job action to take to initiate the job abort.
--
-- 'thresholdPercentage', 'awsJobAbortCriteria_thresholdPercentage' - The minimum percentage of job execution failures that must occur to
-- initiate the job abort.
--
-- Amazon Web Services IoT Core supports up to two digits after the decimal
-- (for example, 10.9 and 10.99, but not 10.999).
--
-- 'minNumberOfExecutedThings', 'awsJobAbortCriteria_minNumberOfExecutedThings' - The minimum number of things which must receive job execution
-- notifications before the job can be aborted.
newAwsJobAbortCriteria ::
  -- | 'failureType'
  AwsJobAbortCriteriaFailureType ->
  -- | 'action'
  AwsJobAbortCriteriaAbortAction ->
  -- | 'thresholdPercentage'
  Prelude.Double ->
  -- | 'minNumberOfExecutedThings'
  Prelude.Natural ->
  AwsJobAbortCriteria
newAwsJobAbortCriteria
  pFailureType_
  pAction_
  pThresholdPercentage_
  pMinNumberOfExecutedThings_ =
    AwsJobAbortCriteria'
      { failureType = pFailureType_,
        action = pAction_,
        thresholdPercentage = pThresholdPercentage_,
        minNumberOfExecutedThings =
          pMinNumberOfExecutedThings_
      }

-- | The type of job execution failures that can initiate a job abort.
awsJobAbortCriteria_failureType :: Lens.Lens' AwsJobAbortCriteria AwsJobAbortCriteriaFailureType
awsJobAbortCriteria_failureType = Lens.lens (\AwsJobAbortCriteria' {failureType} -> failureType) (\s@AwsJobAbortCriteria' {} a -> s {failureType = a} :: AwsJobAbortCriteria)

-- | The type of job action to take to initiate the job abort.
awsJobAbortCriteria_action :: Lens.Lens' AwsJobAbortCriteria AwsJobAbortCriteriaAbortAction
awsJobAbortCriteria_action = Lens.lens (\AwsJobAbortCriteria' {action} -> action) (\s@AwsJobAbortCriteria' {} a -> s {action = a} :: AwsJobAbortCriteria)

-- | The minimum percentage of job execution failures that must occur to
-- initiate the job abort.
--
-- Amazon Web Services IoT Core supports up to two digits after the decimal
-- (for example, 10.9 and 10.99, but not 10.999).
awsJobAbortCriteria_thresholdPercentage :: Lens.Lens' AwsJobAbortCriteria Prelude.Double
awsJobAbortCriteria_thresholdPercentage = Lens.lens (\AwsJobAbortCriteria' {thresholdPercentage} -> thresholdPercentage) (\s@AwsJobAbortCriteria' {} a -> s {thresholdPercentage = a} :: AwsJobAbortCriteria)

-- | The minimum number of things which must receive job execution
-- notifications before the job can be aborted.
awsJobAbortCriteria_minNumberOfExecutedThings :: Lens.Lens' AwsJobAbortCriteria Prelude.Natural
awsJobAbortCriteria_minNumberOfExecutedThings = Lens.lens (\AwsJobAbortCriteria' {minNumberOfExecutedThings} -> minNumberOfExecutedThings) (\s@AwsJobAbortCriteria' {} a -> s {minNumberOfExecutedThings = a} :: AwsJobAbortCriteria)

instance Prelude.Hashable AwsJobAbortCriteria where
  hashWithSalt _salt AwsJobAbortCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` failureType
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` thresholdPercentage
      `Prelude.hashWithSalt` minNumberOfExecutedThings

instance Prelude.NFData AwsJobAbortCriteria where
  rnf AwsJobAbortCriteria' {..} =
    Prelude.rnf failureType
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf thresholdPercentage
      `Prelude.seq` Prelude.rnf minNumberOfExecutedThings

instance Data.ToJSON AwsJobAbortCriteria where
  toJSON AwsJobAbortCriteria' {..} =
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
