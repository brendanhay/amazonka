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
-- Module      : Amazonka.KafkaConnect.Types.WorkerConfigurationRevisionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.WorkerConfigurationRevisionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of a worker configuration revision.
--
-- /See:/ 'newWorkerConfigurationRevisionSummary' smart constructor.
data WorkerConfigurationRevisionSummary = WorkerConfigurationRevisionSummary'
  { -- | The time that a worker configuration revision was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The description of a worker configuration revision.
    description :: Prelude.Maybe Prelude.Text,
    -- | The revision of a worker configuration.
    revision :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerConfigurationRevisionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'workerConfigurationRevisionSummary_creationTime' - The time that a worker configuration revision was created.
--
-- 'description', 'workerConfigurationRevisionSummary_description' - The description of a worker configuration revision.
--
-- 'revision', 'workerConfigurationRevisionSummary_revision' - The revision of a worker configuration.
newWorkerConfigurationRevisionSummary ::
  WorkerConfigurationRevisionSummary
newWorkerConfigurationRevisionSummary =
  WorkerConfigurationRevisionSummary'
    { creationTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      revision = Prelude.Nothing
    }

-- | The time that a worker configuration revision was created.
workerConfigurationRevisionSummary_creationTime :: Lens.Lens' WorkerConfigurationRevisionSummary (Prelude.Maybe Prelude.UTCTime)
workerConfigurationRevisionSummary_creationTime = Lens.lens (\WorkerConfigurationRevisionSummary' {creationTime} -> creationTime) (\s@WorkerConfigurationRevisionSummary' {} a -> s {creationTime = a} :: WorkerConfigurationRevisionSummary) Prelude.. Lens.mapping Data._Time

-- | The description of a worker configuration revision.
workerConfigurationRevisionSummary_description :: Lens.Lens' WorkerConfigurationRevisionSummary (Prelude.Maybe Prelude.Text)
workerConfigurationRevisionSummary_description = Lens.lens (\WorkerConfigurationRevisionSummary' {description} -> description) (\s@WorkerConfigurationRevisionSummary' {} a -> s {description = a} :: WorkerConfigurationRevisionSummary)

-- | The revision of a worker configuration.
workerConfigurationRevisionSummary_revision :: Lens.Lens' WorkerConfigurationRevisionSummary (Prelude.Maybe Prelude.Integer)
workerConfigurationRevisionSummary_revision = Lens.lens (\WorkerConfigurationRevisionSummary' {revision} -> revision) (\s@WorkerConfigurationRevisionSummary' {} a -> s {revision = a} :: WorkerConfigurationRevisionSummary)

instance
  Data.FromJSON
    WorkerConfigurationRevisionSummary
  where
  parseJSON =
    Data.withObject
      "WorkerConfigurationRevisionSummary"
      ( \x ->
          WorkerConfigurationRevisionSummary'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "revision")
      )

instance
  Prelude.Hashable
    WorkerConfigurationRevisionSummary
  where
  hashWithSalt
    _salt
    WorkerConfigurationRevisionSummary' {..} =
      _salt
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` revision

instance
  Prelude.NFData
    WorkerConfigurationRevisionSummary
  where
  rnf WorkerConfigurationRevisionSummary' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf revision
