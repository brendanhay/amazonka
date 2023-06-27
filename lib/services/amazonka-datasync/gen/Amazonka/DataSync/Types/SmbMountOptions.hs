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
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.SmbMountOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.SmbVersion
import qualified Amazonka.Prelude as Prelude

-- | Specifies the version of the Server Message Block (SMB) protocol that
-- DataSync uses to access an SMB file server.
--
-- /See:/ 'newSmbMountOptions' smart constructor.
data SmbMountOptions = SmbMountOptions'
  { -- | By default, DataSync automatically chooses an SMB protocol version based
    -- on negotiation with your SMB file server. You also can configure
    -- DataSync to use a specific SMB version, but we recommend doing this only
    -- if DataSync has trouble negotiating with the SMB file server
    -- automatically.
    --
    -- These are the following options for configuring the SMB version:
    --
    -- -   @AUTOMATIC@ (default): DataSync and the SMB file server negotiate
    --     the highest version of SMB that they mutually support between 2.1
    --     and 3.1.1.
    --
    --     This is the recommended option. If you instead choose a specific
    --     version that your file server doesn\'t support, you may get an
    --     @Operation Not Supported@ error.
    --
    -- -   @SMB3@: Restricts the protocol negotiation to only SMB version
    --     3.0.2.
    --
    -- -   @SMB2@: Restricts the protocol negotiation to only SMB version 2.1.
    --
    -- -   @SMB2_0@: Restricts the protocol negotiation to only SMB version
    --     2.0.
    --
    -- -   @SMB1@: Restricts the protocol negotiation to only SMB version 1.0.
    --
    --     The @SMB1@ option isn\'t available when
    --     <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateLocationFsxOntap.html creating an Amazon FSx for NetApp ONTAP location>.
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
-- 'version', 'smbMountOptions_version' - By default, DataSync automatically chooses an SMB protocol version based
-- on negotiation with your SMB file server. You also can configure
-- DataSync to use a specific SMB version, but we recommend doing this only
-- if DataSync has trouble negotiating with the SMB file server
-- automatically.
--
-- These are the following options for configuring the SMB version:
--
-- -   @AUTOMATIC@ (default): DataSync and the SMB file server negotiate
--     the highest version of SMB that they mutually support between 2.1
--     and 3.1.1.
--
--     This is the recommended option. If you instead choose a specific
--     version that your file server doesn\'t support, you may get an
--     @Operation Not Supported@ error.
--
-- -   @SMB3@: Restricts the protocol negotiation to only SMB version
--     3.0.2.
--
-- -   @SMB2@: Restricts the protocol negotiation to only SMB version 2.1.
--
-- -   @SMB2_0@: Restricts the protocol negotiation to only SMB version
--     2.0.
--
-- -   @SMB1@: Restricts the protocol negotiation to only SMB version 1.0.
--
--     The @SMB1@ option isn\'t available when
--     <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateLocationFsxOntap.html creating an Amazon FSx for NetApp ONTAP location>.
newSmbMountOptions ::
  SmbMountOptions
newSmbMountOptions =
  SmbMountOptions' {version = Prelude.Nothing}

-- | By default, DataSync automatically chooses an SMB protocol version based
-- on negotiation with your SMB file server. You also can configure
-- DataSync to use a specific SMB version, but we recommend doing this only
-- if DataSync has trouble negotiating with the SMB file server
-- automatically.
--
-- These are the following options for configuring the SMB version:
--
-- -   @AUTOMATIC@ (default): DataSync and the SMB file server negotiate
--     the highest version of SMB that they mutually support between 2.1
--     and 3.1.1.
--
--     This is the recommended option. If you instead choose a specific
--     version that your file server doesn\'t support, you may get an
--     @Operation Not Supported@ error.
--
-- -   @SMB3@: Restricts the protocol negotiation to only SMB version
--     3.0.2.
--
-- -   @SMB2@: Restricts the protocol negotiation to only SMB version 2.1.
--
-- -   @SMB2_0@: Restricts the protocol negotiation to only SMB version
--     2.0.
--
-- -   @SMB1@: Restricts the protocol negotiation to only SMB version 1.0.
--
--     The @SMB1@ option isn\'t available when
--     <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateLocationFsxOntap.html creating an Amazon FSx for NetApp ONTAP location>.
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
