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
-- Module      : Amazonka.KafkaConnect.Types.WorkerConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.WorkerConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the workers, which are the processes that run the
-- connector logic.
--
-- /See:/ 'newWorkerConfiguration' smart constructor.
data WorkerConfiguration = WorkerConfiguration'
  { -- | The revision of the worker configuration.
    revision :: Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the worker configuration.
    workerConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revision', 'workerConfiguration_revision' - The revision of the worker configuration.
--
-- 'workerConfigurationArn', 'workerConfiguration_workerConfigurationArn' - The Amazon Resource Name (ARN) of the worker configuration.
newWorkerConfiguration ::
  -- | 'revision'
  Prelude.Natural ->
  -- | 'workerConfigurationArn'
  Prelude.Text ->
  WorkerConfiguration
newWorkerConfiguration
  pRevision_
  pWorkerConfigurationArn_ =
    WorkerConfiguration'
      { revision = pRevision_,
        workerConfigurationArn = pWorkerConfigurationArn_
      }

-- | The revision of the worker configuration.
workerConfiguration_revision :: Lens.Lens' WorkerConfiguration Prelude.Natural
workerConfiguration_revision = Lens.lens (\WorkerConfiguration' {revision} -> revision) (\s@WorkerConfiguration' {} a -> s {revision = a} :: WorkerConfiguration)

-- | The Amazon Resource Name (ARN) of the worker configuration.
workerConfiguration_workerConfigurationArn :: Lens.Lens' WorkerConfiguration Prelude.Text
workerConfiguration_workerConfigurationArn = Lens.lens (\WorkerConfiguration' {workerConfigurationArn} -> workerConfigurationArn) (\s@WorkerConfiguration' {} a -> s {workerConfigurationArn = a} :: WorkerConfiguration)

instance Prelude.Hashable WorkerConfiguration where
  hashWithSalt _salt WorkerConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` workerConfigurationArn

instance Prelude.NFData WorkerConfiguration where
  rnf WorkerConfiguration' {..} =
    Prelude.rnf revision `Prelude.seq`
      Prelude.rnf workerConfigurationArn

instance Data.ToJSON WorkerConfiguration where
  toJSON WorkerConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("revision" Data..= revision),
            Prelude.Just
              ( "workerConfigurationArn"
                  Data..= workerConfigurationArn
              )
          ]
      )
