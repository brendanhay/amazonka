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
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.UpdateSvmActiveDirectoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.SelfManagedActiveDirectoryConfigurationUpdates
import qualified Amazonka.Prelude as Prelude

-- | Specifies updates to an FSx for ONTAP storage virtual machine\'s (SVM)
-- Microsoft Active Directory (AD) configuration. Note that account
-- credentials are not returned in the response payload.
--
-- /See:/ 'newUpdateSvmActiveDirectoryConfiguration' smart constructor.
data UpdateSvmActiveDirectoryConfiguration = UpdateSvmActiveDirectoryConfiguration'
  { -- | Specifies an updated NetBIOS name of the AD computer object
    -- @NetBiosName@ to which an SVM is joined.
    netBiosName :: Prelude.Maybe Prelude.Text,
    selfManagedActiveDirectoryConfiguration :: Prelude.Maybe SelfManagedActiveDirectoryConfigurationUpdates
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
-- 'netBiosName', 'updateSvmActiveDirectoryConfiguration_netBiosName' - Specifies an updated NetBIOS name of the AD computer object
-- @NetBiosName@ to which an SVM is joined.
--
-- 'selfManagedActiveDirectoryConfiguration', 'updateSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration' - Undocumented member.
newUpdateSvmActiveDirectoryConfiguration ::
  UpdateSvmActiveDirectoryConfiguration
newUpdateSvmActiveDirectoryConfiguration =
  UpdateSvmActiveDirectoryConfiguration'
    { netBiosName =
        Prelude.Nothing,
      selfManagedActiveDirectoryConfiguration =
        Prelude.Nothing
    }

-- | Specifies an updated NetBIOS name of the AD computer object
-- @NetBiosName@ to which an SVM is joined.
updateSvmActiveDirectoryConfiguration_netBiosName :: Lens.Lens' UpdateSvmActiveDirectoryConfiguration (Prelude.Maybe Prelude.Text)
updateSvmActiveDirectoryConfiguration_netBiosName = Lens.lens (\UpdateSvmActiveDirectoryConfiguration' {netBiosName} -> netBiosName) (\s@UpdateSvmActiveDirectoryConfiguration' {} a -> s {netBiosName = a} :: UpdateSvmActiveDirectoryConfiguration)

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
        `Prelude.hashWithSalt` netBiosName
        `Prelude.hashWithSalt` selfManagedActiveDirectoryConfiguration

instance
  Prelude.NFData
    UpdateSvmActiveDirectoryConfiguration
  where
  rnf UpdateSvmActiveDirectoryConfiguration' {..} =
    Prelude.rnf netBiosName
      `Prelude.seq` Prelude.rnf selfManagedActiveDirectoryConfiguration

instance
  Data.ToJSON
    UpdateSvmActiveDirectoryConfiguration
  where
  toJSON UpdateSvmActiveDirectoryConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NetBiosName" Data..=) Prelude.<$> netBiosName,
            ("SelfManagedActiveDirectoryConfiguration" Data..=)
              Prelude.<$> selfManagedActiveDirectoryConfiguration
          ]
      )
