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
-- Module      : Amazonka.DataSync.Types.SmbMountOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.SmbMountOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.SmbVersion
import qualified Amazonka.Prelude as Prelude

-- | Specifies how DataSync can access a location using the SMB protocol.
--
-- /See:/ 'newSmbMountOptions' smart constructor.
data SmbMountOptions = SmbMountOptions'
  { -- | Specifies the SMB version that you want DataSync to use when mounting
    -- your SMB share. If you don\'t specify a version, DataSync defaults to
    -- @AUTOMATIC@ and chooses a version based on negotiation with the SMB
    -- server.
    version :: Prelude.Maybe SmbVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SmbMountOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'smbMountOptions_version' - Specifies the SMB version that you want DataSync to use when mounting
-- your SMB share. If you don\'t specify a version, DataSync defaults to
-- @AUTOMATIC@ and chooses a version based on negotiation with the SMB
-- server.
newSmbMountOptions ::
  SmbMountOptions
newSmbMountOptions =
  SmbMountOptions' {version = Prelude.Nothing}

-- | Specifies the SMB version that you want DataSync to use when mounting
-- your SMB share. If you don\'t specify a version, DataSync defaults to
-- @AUTOMATIC@ and chooses a version based on negotiation with the SMB
-- server.
smbMountOptions_version :: Lens.Lens' SmbMountOptions (Prelude.Maybe SmbVersion)
smbMountOptions_version = Lens.lens (\SmbMountOptions' {version} -> version) (\s@SmbMountOptions' {} a -> s {version = a} :: SmbMountOptions)

instance Data.FromJSON SmbMountOptions where
  parseJSON =
    Data.withObject
      "SmbMountOptions"
      ( \x ->
          SmbMountOptions' Prelude.<$> (x Data..:? "Version")
      )

instance Prelude.Hashable SmbMountOptions where
  hashWithSalt _salt SmbMountOptions' {..} =
    _salt `Prelude.hashWithSalt` version

instance Prelude.NFData SmbMountOptions where
  rnf SmbMountOptions' {..} = Prelude.rnf version

instance Data.ToJSON SmbMountOptions where
  toJSON SmbMountOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Version" Data..=) Prelude.<$> version]
      )
