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
-- Module      : Amazonka.LakeFormation.Types.StorageOptimizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.StorageOptimizer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.OptimizerType
import qualified Amazonka.Prelude as Prelude

-- | A structure describing the configuration and details of a storage
-- optimizer.
--
-- /See:/ 'newStorageOptimizer' smart constructor.
data StorageOptimizer = StorageOptimizer'
  { -- | A map of the storage optimizer configuration. Currently contains only
    -- one key-value pair: @is_enabled@ indicates true or false for
    -- acceleration.
    config :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A message that contains information about any error (if present).
    --
    -- When an acceleration result has an enabled status, the error message is
    -- empty.
    --
    -- When an acceleration result has a disabled status, the message describes
    -- an error or simply indicates \"disabled by the user\".
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | When an acceleration result has an enabled status, contains the details
    -- of the last job run.
    lastRunDetails :: Prelude.Maybe Prelude.Text,
    -- | The specific type of storage optimizer. The supported value is
    -- @compaction@.
    storageOptimizerType :: Prelude.Maybe OptimizerType,
    -- | A message that contains information about any warnings (if present).
    warnings :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageOptimizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'config', 'storageOptimizer_config' - A map of the storage optimizer configuration. Currently contains only
-- one key-value pair: @is_enabled@ indicates true or false for
-- acceleration.
--
-- 'errorMessage', 'storageOptimizer_errorMessage' - A message that contains information about any error (if present).
--
-- When an acceleration result has an enabled status, the error message is
-- empty.
--
-- When an acceleration result has a disabled status, the message describes
-- an error or simply indicates \"disabled by the user\".
--
-- 'lastRunDetails', 'storageOptimizer_lastRunDetails' - When an acceleration result has an enabled status, contains the details
-- of the last job run.
--
-- 'storageOptimizerType', 'storageOptimizer_storageOptimizerType' - The specific type of storage optimizer. The supported value is
-- @compaction@.
--
-- 'warnings', 'storageOptimizer_warnings' - A message that contains information about any warnings (if present).
newStorageOptimizer ::
  StorageOptimizer
newStorageOptimizer =
  StorageOptimizer'
    { config = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      lastRunDetails = Prelude.Nothing,
      storageOptimizerType = Prelude.Nothing,
      warnings = Prelude.Nothing
    }

-- | A map of the storage optimizer configuration. Currently contains only
-- one key-value pair: @is_enabled@ indicates true or false for
-- acceleration.
storageOptimizer_config :: Lens.Lens' StorageOptimizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
storageOptimizer_config = Lens.lens (\StorageOptimizer' {config} -> config) (\s@StorageOptimizer' {} a -> s {config = a} :: StorageOptimizer) Prelude.. Lens.mapping Lens.coerced

-- | A message that contains information about any error (if present).
--
-- When an acceleration result has an enabled status, the error message is
-- empty.
--
-- When an acceleration result has a disabled status, the message describes
-- an error or simply indicates \"disabled by the user\".
storageOptimizer_errorMessage :: Lens.Lens' StorageOptimizer (Prelude.Maybe Prelude.Text)
storageOptimizer_errorMessage = Lens.lens (\StorageOptimizer' {errorMessage} -> errorMessage) (\s@StorageOptimizer' {} a -> s {errorMessage = a} :: StorageOptimizer)

-- | When an acceleration result has an enabled status, contains the details
-- of the last job run.
storageOptimizer_lastRunDetails :: Lens.Lens' StorageOptimizer (Prelude.Maybe Prelude.Text)
storageOptimizer_lastRunDetails = Lens.lens (\StorageOptimizer' {lastRunDetails} -> lastRunDetails) (\s@StorageOptimizer' {} a -> s {lastRunDetails = a} :: StorageOptimizer)

-- | The specific type of storage optimizer. The supported value is
-- @compaction@.
storageOptimizer_storageOptimizerType :: Lens.Lens' StorageOptimizer (Prelude.Maybe OptimizerType)
storageOptimizer_storageOptimizerType = Lens.lens (\StorageOptimizer' {storageOptimizerType} -> storageOptimizerType) (\s@StorageOptimizer' {} a -> s {storageOptimizerType = a} :: StorageOptimizer)

-- | A message that contains information about any warnings (if present).
storageOptimizer_warnings :: Lens.Lens' StorageOptimizer (Prelude.Maybe Prelude.Text)
storageOptimizer_warnings = Lens.lens (\StorageOptimizer' {warnings} -> warnings) (\s@StorageOptimizer' {} a -> s {warnings = a} :: StorageOptimizer)

instance Data.FromJSON StorageOptimizer where
  parseJSON =
    Data.withObject
      "StorageOptimizer"
      ( \x ->
          StorageOptimizer'
            Prelude.<$> (x Data..:? "Config" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "LastRunDetails")
            Prelude.<*> (x Data..:? "StorageOptimizerType")
            Prelude.<*> (x Data..:? "Warnings")
      )

instance Prelude.Hashable StorageOptimizer where
  hashWithSalt _salt StorageOptimizer' {..} =
    _salt
      `Prelude.hashWithSalt` config
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` lastRunDetails
      `Prelude.hashWithSalt` storageOptimizerType
      `Prelude.hashWithSalt` warnings

instance Prelude.NFData StorageOptimizer where
  rnf StorageOptimizer' {..} =
    Prelude.rnf config
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf lastRunDetails
      `Prelude.seq` Prelude.rnf storageOptimizerType
      `Prelude.seq` Prelude.rnf warnings
