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
-- Module      : Amazonka.KafkaConnect.Types.WorkerConfigurationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.WorkerConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The description of the worker configuration.
--
-- /See:/ 'newWorkerConfigurationDescription' smart constructor.
data WorkerConfigurationDescription = WorkerConfigurationDescription'
  { -- | The Amazon Resource Name (ARN) of the worker configuration.
    workerConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The revision of the worker configuration.
    revision :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workerConfigurationArn', 'workerConfigurationDescription_workerConfigurationArn' - The Amazon Resource Name (ARN) of the worker configuration.
--
-- 'revision', 'workerConfigurationDescription_revision' - The revision of the worker configuration.
newWorkerConfigurationDescription ::
  WorkerConfigurationDescription
newWorkerConfigurationDescription =
  WorkerConfigurationDescription'
    { workerConfigurationArn =
        Prelude.Nothing,
      revision = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the worker configuration.
workerConfigurationDescription_workerConfigurationArn :: Lens.Lens' WorkerConfigurationDescription (Prelude.Maybe Prelude.Text)
workerConfigurationDescription_workerConfigurationArn = Lens.lens (\WorkerConfigurationDescription' {workerConfigurationArn} -> workerConfigurationArn) (\s@WorkerConfigurationDescription' {} a -> s {workerConfigurationArn = a} :: WorkerConfigurationDescription)

-- | The revision of the worker configuration.
workerConfigurationDescription_revision :: Lens.Lens' WorkerConfigurationDescription (Prelude.Maybe Prelude.Integer)
workerConfigurationDescription_revision = Lens.lens (\WorkerConfigurationDescription' {revision} -> revision) (\s@WorkerConfigurationDescription' {} a -> s {revision = a} :: WorkerConfigurationDescription)

instance Core.FromJSON WorkerConfigurationDescription where
  parseJSON =
    Core.withObject
      "WorkerConfigurationDescription"
      ( \x ->
          WorkerConfigurationDescription'
            Prelude.<$> (x Core..:? "workerConfigurationArn")
            Prelude.<*> (x Core..:? "revision")
      )

instance
  Prelude.Hashable
    WorkerConfigurationDescription
  where
  hashWithSalt
    salt'
    WorkerConfigurationDescription' {..} =
      salt' `Prelude.hashWithSalt` revision
        `Prelude.hashWithSalt` workerConfigurationArn

instance
  Prelude.NFData
    WorkerConfigurationDescription
  where
  rnf WorkerConfigurationDescription' {..} =
    Prelude.rnf workerConfigurationArn
      `Prelude.seq` Prelude.rnf revision
