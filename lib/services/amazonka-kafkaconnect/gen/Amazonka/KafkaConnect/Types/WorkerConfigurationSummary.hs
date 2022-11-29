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
-- Module      : Amazonka.KafkaConnect.Types.WorkerConfigurationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.WorkerConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KafkaConnect.Types.WorkerConfigurationRevisionSummary
import qualified Amazonka.Prelude as Prelude

-- | The summary of a worker configuration.
--
-- /See:/ 'newWorkerConfigurationSummary' smart constructor.
data WorkerConfigurationSummary = WorkerConfigurationSummary'
  { -- | The latest revision of a worker configuration.
    latestRevision :: Prelude.Maybe WorkerConfigurationRevisionSummary,
    -- | The name of the worker configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of a worker configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the worker configuration.
    workerConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The time that a worker configuration was created.
    creationTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestRevision', 'workerConfigurationSummary_latestRevision' - The latest revision of a worker configuration.
--
-- 'name', 'workerConfigurationSummary_name' - The name of the worker configuration.
--
-- 'description', 'workerConfigurationSummary_description' - The description of a worker configuration.
--
-- 'workerConfigurationArn', 'workerConfigurationSummary_workerConfigurationArn' - The Amazon Resource Name (ARN) of the worker configuration.
--
-- 'creationTime', 'workerConfigurationSummary_creationTime' - The time that a worker configuration was created.
newWorkerConfigurationSummary ::
  WorkerConfigurationSummary
newWorkerConfigurationSummary =
  WorkerConfigurationSummary'
    { latestRevision =
        Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      workerConfigurationArn = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The latest revision of a worker configuration.
workerConfigurationSummary_latestRevision :: Lens.Lens' WorkerConfigurationSummary (Prelude.Maybe WorkerConfigurationRevisionSummary)
workerConfigurationSummary_latestRevision = Lens.lens (\WorkerConfigurationSummary' {latestRevision} -> latestRevision) (\s@WorkerConfigurationSummary' {} a -> s {latestRevision = a} :: WorkerConfigurationSummary)

-- | The name of the worker configuration.
workerConfigurationSummary_name :: Lens.Lens' WorkerConfigurationSummary (Prelude.Maybe Prelude.Text)
workerConfigurationSummary_name = Lens.lens (\WorkerConfigurationSummary' {name} -> name) (\s@WorkerConfigurationSummary' {} a -> s {name = a} :: WorkerConfigurationSummary)

-- | The description of a worker configuration.
workerConfigurationSummary_description :: Lens.Lens' WorkerConfigurationSummary (Prelude.Maybe Prelude.Text)
workerConfigurationSummary_description = Lens.lens (\WorkerConfigurationSummary' {description} -> description) (\s@WorkerConfigurationSummary' {} a -> s {description = a} :: WorkerConfigurationSummary)

-- | The Amazon Resource Name (ARN) of the worker configuration.
workerConfigurationSummary_workerConfigurationArn :: Lens.Lens' WorkerConfigurationSummary (Prelude.Maybe Prelude.Text)
workerConfigurationSummary_workerConfigurationArn = Lens.lens (\WorkerConfigurationSummary' {workerConfigurationArn} -> workerConfigurationArn) (\s@WorkerConfigurationSummary' {} a -> s {workerConfigurationArn = a} :: WorkerConfigurationSummary)

-- | The time that a worker configuration was created.
workerConfigurationSummary_creationTime :: Lens.Lens' WorkerConfigurationSummary (Prelude.Maybe Prelude.UTCTime)
workerConfigurationSummary_creationTime = Lens.lens (\WorkerConfigurationSummary' {creationTime} -> creationTime) (\s@WorkerConfigurationSummary' {} a -> s {creationTime = a} :: WorkerConfigurationSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON WorkerConfigurationSummary where
  parseJSON =
    Core.withObject
      "WorkerConfigurationSummary"
      ( \x ->
          WorkerConfigurationSummary'
            Prelude.<$> (x Core..:? "latestRevision")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "workerConfigurationArn")
            Prelude.<*> (x Core..:? "creationTime")
      )

instance Prelude.Hashable WorkerConfigurationSummary where
  hashWithSalt _salt WorkerConfigurationSummary' {..} =
    _salt `Prelude.hashWithSalt` latestRevision
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` workerConfigurationArn
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData WorkerConfigurationSummary where
  rnf WorkerConfigurationSummary' {..} =
    Prelude.rnf latestRevision
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf workerConfigurationArn
      `Prelude.seq` Prelude.rnf creationTime
