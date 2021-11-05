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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.SmbMountOptions where

import qualified Amazonka.Core as Core
import Amazonka.DataSync.Types.SmbVersion
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the mount options that are available for DataSync to access
-- an SMB location.
--
-- /See:/ 'newSmbMountOptions' smart constructor.
data SmbMountOptions = SmbMountOptions'
  { -- | The specific SMB version that you want DataSync to use to mount your SMB
    -- share. If you don\'t specify a version, DataSync defaults to
    -- @AUTOMATIC@. That is, DataSync automatically selects a version based on
    -- negotiation with the SMB server.
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
-- 'version', 'smbMountOptions_version' - The specific SMB version that you want DataSync to use to mount your SMB
-- share. If you don\'t specify a version, DataSync defaults to
-- @AUTOMATIC@. That is, DataSync automatically selects a version based on
-- negotiation with the SMB server.
newSmbMountOptions ::
  SmbMountOptions
newSmbMountOptions =
  SmbMountOptions' {version = Prelude.Nothing}

-- | The specific SMB version that you want DataSync to use to mount your SMB
-- share. If you don\'t specify a version, DataSync defaults to
-- @AUTOMATIC@. That is, DataSync automatically selects a version based on
-- negotiation with the SMB server.
smbMountOptions_version :: Lens.Lens' SmbMountOptions (Prelude.Maybe SmbVersion)
smbMountOptions_version = Lens.lens (\SmbMountOptions' {version} -> version) (\s@SmbMountOptions' {} a -> s {version = a} :: SmbMountOptions)

instance Core.FromJSON SmbMountOptions where
  parseJSON =
    Core.withObject
      "SmbMountOptions"
      ( \x ->
          SmbMountOptions' Prelude.<$> (x Core..:? "Version")
      )

instance Prelude.Hashable SmbMountOptions

instance Prelude.NFData SmbMountOptions

instance Core.ToJSON SmbMountOptions where
  toJSON SmbMountOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Version" Core..=) Prelude.<$> version]
      )
