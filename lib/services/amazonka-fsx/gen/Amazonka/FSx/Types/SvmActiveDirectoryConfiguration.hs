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
-- Module      : Amazonka.FSx.Types.SvmActiveDirectoryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.SvmActiveDirectoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.SelfManagedActiveDirectoryAttributes
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of the Microsoft Active Directory (AD)
-- directory to which the Amazon FSx for ONTAP storage virtual machine
-- (SVM) is joined. Pleae note, account credentials are not returned in the
-- response payload.
--
-- /See:/ 'newSvmActiveDirectoryConfiguration' smart constructor.
data SvmActiveDirectoryConfiguration = SvmActiveDirectoryConfiguration'
  { -- | The NetBIOS name of the Active Directory computer object that is joined
    -- to your SVM.
    netBiosName :: Prelude.Maybe Prelude.Text,
    selfManagedActiveDirectoryConfiguration :: Prelude.Maybe SelfManagedActiveDirectoryAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SvmActiveDirectoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'netBiosName', 'svmActiveDirectoryConfiguration_netBiosName' - The NetBIOS name of the Active Directory computer object that is joined
-- to your SVM.
--
-- 'selfManagedActiveDirectoryConfiguration', 'svmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration' - Undocumented member.
newSvmActiveDirectoryConfiguration ::
  SvmActiveDirectoryConfiguration
newSvmActiveDirectoryConfiguration =
  SvmActiveDirectoryConfiguration'
    { netBiosName =
        Prelude.Nothing,
      selfManagedActiveDirectoryConfiguration =
        Prelude.Nothing
    }

-- | The NetBIOS name of the Active Directory computer object that is joined
-- to your SVM.
svmActiveDirectoryConfiguration_netBiosName :: Lens.Lens' SvmActiveDirectoryConfiguration (Prelude.Maybe Prelude.Text)
svmActiveDirectoryConfiguration_netBiosName = Lens.lens (\SvmActiveDirectoryConfiguration' {netBiosName} -> netBiosName) (\s@SvmActiveDirectoryConfiguration' {} a -> s {netBiosName = a} :: SvmActiveDirectoryConfiguration)

-- | Undocumented member.
svmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration :: Lens.Lens' SvmActiveDirectoryConfiguration (Prelude.Maybe SelfManagedActiveDirectoryAttributes)
svmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration = Lens.lens (\SvmActiveDirectoryConfiguration' {selfManagedActiveDirectoryConfiguration} -> selfManagedActiveDirectoryConfiguration) (\s@SvmActiveDirectoryConfiguration' {} a -> s {selfManagedActiveDirectoryConfiguration = a} :: SvmActiveDirectoryConfiguration)

instance
  Data.FromJSON
    SvmActiveDirectoryConfiguration
  where
  parseJSON =
    Data.withObject
      "SvmActiveDirectoryConfiguration"
      ( \x ->
          SvmActiveDirectoryConfiguration'
            Prelude.<$> (x Data..:? "NetBiosName")
            Prelude.<*> ( x
                            Data..:? "SelfManagedActiveDirectoryConfiguration"
                        )
      )

instance
  Prelude.Hashable
    SvmActiveDirectoryConfiguration
  where
  hashWithSalt
    _salt
    SvmActiveDirectoryConfiguration' {..} =
      _salt `Prelude.hashWithSalt` netBiosName
        `Prelude.hashWithSalt` selfManagedActiveDirectoryConfiguration

instance
  Prelude.NFData
    SvmActiveDirectoryConfiguration
  where
  rnf SvmActiveDirectoryConfiguration' {..} =
    Prelude.rnf netBiosName
      `Prelude.seq` Prelude.rnf selfManagedActiveDirectoryConfiguration
