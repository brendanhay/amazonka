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
-- Module      : Amazonka.FSx.Types.UpdateSvmActiveDirectoryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.UpdateSvmActiveDirectoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.SelfManagedActiveDirectoryConfigurationUpdates
import qualified Amazonka.Prelude as Prelude

-- | Updates the Microsoft Active Directory (AD) configuration of an SVM
-- joined to an AD. Please note, account credentials are not returned in
-- the response payload.
--
-- /See:/ 'newUpdateSvmActiveDirectoryConfiguration' smart constructor.
data UpdateSvmActiveDirectoryConfiguration = UpdateSvmActiveDirectoryConfiguration'
  { selfManagedActiveDirectoryConfiguration :: Prelude.Maybe SelfManagedActiveDirectoryConfigurationUpdates
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSvmActiveDirectoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selfManagedActiveDirectoryConfiguration', 'updateSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration' - Undocumented member.
newUpdateSvmActiveDirectoryConfiguration ::
  UpdateSvmActiveDirectoryConfiguration
newUpdateSvmActiveDirectoryConfiguration =
  UpdateSvmActiveDirectoryConfiguration'
    { selfManagedActiveDirectoryConfiguration =
        Prelude.Nothing
    }

-- | Undocumented member.
updateSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration :: Lens.Lens' UpdateSvmActiveDirectoryConfiguration (Prelude.Maybe SelfManagedActiveDirectoryConfigurationUpdates)
updateSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration = Lens.lens (\UpdateSvmActiveDirectoryConfiguration' {selfManagedActiveDirectoryConfiguration} -> selfManagedActiveDirectoryConfiguration) (\s@UpdateSvmActiveDirectoryConfiguration' {} a -> s {selfManagedActiveDirectoryConfiguration = a} :: UpdateSvmActiveDirectoryConfiguration)

instance
  Prelude.Hashable
    UpdateSvmActiveDirectoryConfiguration
  where
  hashWithSalt
    _salt
    UpdateSvmActiveDirectoryConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` selfManagedActiveDirectoryConfiguration

instance
  Prelude.NFData
    UpdateSvmActiveDirectoryConfiguration
  where
  rnf UpdateSvmActiveDirectoryConfiguration' {..} =
    Prelude.rnf selfManagedActiveDirectoryConfiguration

instance
  Data.ToJSON
    UpdateSvmActiveDirectoryConfiguration
  where
  toJSON UpdateSvmActiveDirectoryConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SelfManagedActiveDirectoryConfiguration" Data..=)
              Prelude.<$> selfManagedActiveDirectoryConfiguration
          ]
      )
