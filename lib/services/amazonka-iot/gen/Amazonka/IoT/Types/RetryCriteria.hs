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
-- Module      : Amazonka.IoT.Types.RetryCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.RetryCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.RetryableFailureType
import qualified Amazonka.Prelude as Prelude

-- | The criteria that determines how many retries are allowed for each
-- failure type for a job.
--
-- /See:/ 'newRetryCriteria' smart constructor.
data RetryCriteria = RetryCriteria'
  { -- | The type of job execution failures that can initiate a job retry.
    failureType :: RetryableFailureType,
    -- | The number of retries allowed for a failure type for the job.
    numberOfRetries :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureType', 'retryCriteria_failureType' - The type of job execution failures that can initiate a job retry.
--
-- 'numberOfRetries', 'retryCriteria_numberOfRetries' - The number of retries allowed for a failure type for the job.
newRetryCriteria ::
  -- | 'failureType'
  RetryableFailureType ->
  -- | 'numberOfRetries'
  Prelude.Natural ->
  RetryCriteria
newRetryCriteria pFailureType_ pNumberOfRetries_ =
  RetryCriteria'
    { failureType = pFailureType_,
      numberOfRetries = pNumberOfRetries_
    }

-- | The type of job execution failures that can initiate a job retry.
retryCriteria_failureType :: Lens.Lens' RetryCriteria RetryableFailureType
retryCriteria_failureType = Lens.lens (\RetryCriteria' {failureType} -> failureType) (\s@RetryCriteria' {} a -> s {failureType = a} :: RetryCriteria)

-- | The number of retries allowed for a failure type for the job.
retryCriteria_numberOfRetries :: Lens.Lens' RetryCriteria Prelude.Natural
retryCriteria_numberOfRetries = Lens.lens (\RetryCriteria' {numberOfRetries} -> numberOfRetries) (\s@RetryCriteria' {} a -> s {numberOfRetries = a} :: RetryCriteria)

instance Core.FromJSON RetryCriteria where
  parseJSON =
    Core.withObject
      "RetryCriteria"
      ( \x ->
          RetryCriteria'
            Prelude.<$> (x Core..: "failureType")
            Prelude.<*> (x Core..: "numberOfRetries")
      )

instance Prelude.Hashable RetryCriteria where
  hashWithSalt _salt RetryCriteria' {..} =
    _salt `Prelude.hashWithSalt` failureType
      `Prelude.hashWithSalt` numberOfRetries

instance Prelude.NFData RetryCriteria where
  rnf RetryCriteria' {..} =
    Prelude.rnf failureType
      `Prelude.seq` Prelude.rnf numberOfRetries

instance Core.ToJSON RetryCriteria where
  toJSON RetryCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("failureType" Core..= failureType),
            Prelude.Just
              ("numberOfRetries" Core..= numberOfRetries)
          ]
      )
