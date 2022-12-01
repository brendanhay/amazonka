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
-- Module      : Amazonka.FSx.Types.CreateSvmActiveDirectoryConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateSvmActiveDirectoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.SelfManagedActiveDirectoryConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration that Amazon FSx uses to join the ONTAP storage virtual
-- machine (SVM) to your self-managed (including on-premises) Microsoft
-- Active Directory (AD) directory.
--
-- /See:/ 'newCreateSvmActiveDirectoryConfiguration' smart constructor.
data CreateSvmActiveDirectoryConfiguration = CreateSvmActiveDirectoryConfiguration'
  { selfManagedActiveDirectoryConfiguration :: Prelude.Maybe SelfManagedActiveDirectoryConfiguration,
    -- | The NetBIOS name of the Active Directory computer object that will be
    -- created for your SVM.
    netBiosName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSvmActiveDirectoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selfManagedActiveDirectoryConfiguration', 'createSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration' - Undocumented member.
--
-- 'netBiosName', 'createSvmActiveDirectoryConfiguration_netBiosName' - The NetBIOS name of the Active Directory computer object that will be
-- created for your SVM.
newCreateSvmActiveDirectoryConfiguration ::
  -- | 'netBiosName'
  Prelude.Text ->
  CreateSvmActiveDirectoryConfiguration
newCreateSvmActiveDirectoryConfiguration
  pNetBiosName_ =
    CreateSvmActiveDirectoryConfiguration'
      { selfManagedActiveDirectoryConfiguration =
          Prelude.Nothing,
        netBiosName = pNetBiosName_
      }

-- | Undocumented member.
createSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration :: Lens.Lens' CreateSvmActiveDirectoryConfiguration (Prelude.Maybe SelfManagedActiveDirectoryConfiguration)
createSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration = Lens.lens (\CreateSvmActiveDirectoryConfiguration' {selfManagedActiveDirectoryConfiguration} -> selfManagedActiveDirectoryConfiguration) (\s@CreateSvmActiveDirectoryConfiguration' {} a -> s {selfManagedActiveDirectoryConfiguration = a} :: CreateSvmActiveDirectoryConfiguration)

-- | The NetBIOS name of the Active Directory computer object that will be
-- created for your SVM.
createSvmActiveDirectoryConfiguration_netBiosName :: Lens.Lens' CreateSvmActiveDirectoryConfiguration Prelude.Text
createSvmActiveDirectoryConfiguration_netBiosName = Lens.lens (\CreateSvmActiveDirectoryConfiguration' {netBiosName} -> netBiosName) (\s@CreateSvmActiveDirectoryConfiguration' {} a -> s {netBiosName = a} :: CreateSvmActiveDirectoryConfiguration)

instance
  Prelude.Hashable
    CreateSvmActiveDirectoryConfiguration
  where
  hashWithSalt
    _salt
    CreateSvmActiveDirectoryConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` selfManagedActiveDirectoryConfiguration
        `Prelude.hashWithSalt` netBiosName

instance
  Prelude.NFData
    CreateSvmActiveDirectoryConfiguration
  where
  rnf CreateSvmActiveDirectoryConfiguration' {..} =
    Prelude.rnf selfManagedActiveDirectoryConfiguration
      `Prelude.seq` Prelude.rnf netBiosName

instance
  Core.ToJSON
    CreateSvmActiveDirectoryConfiguration
  where
  toJSON CreateSvmActiveDirectoryConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SelfManagedActiveDirectoryConfiguration" Core..=)
              Prelude.<$> selfManagedActiveDirectoryConfiguration,
            Prelude.Just ("NetBiosName" Core..= netBiosName)
          ]
      )
