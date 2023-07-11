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
-- Module      : Amazonka.DataSync.Types.FsxProtocolNfs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.FsxProtocolNfs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.NfsMountOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies the Network File System (NFS) protocol configuration that
-- DataSync uses to access your Amazon FSx for OpenZFS or Amazon FSx for
-- NetApp ONTAP file system.
--
-- /See:/ 'newFsxProtocolNfs' smart constructor.
data FsxProtocolNfs = FsxProtocolNfs'
  { mountOptions :: Prelude.Maybe NfsMountOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FsxProtocolNfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mountOptions', 'fsxProtocolNfs_mountOptions' - Undocumented member.
newFsxProtocolNfs ::
  FsxProtocolNfs
newFsxProtocolNfs =
  FsxProtocolNfs' {mountOptions = Prelude.Nothing}

-- | Undocumented member.
fsxProtocolNfs_mountOptions :: Lens.Lens' FsxProtocolNfs (Prelude.Maybe NfsMountOptions)
fsxProtocolNfs_mountOptions = Lens.lens (\FsxProtocolNfs' {mountOptions} -> mountOptions) (\s@FsxProtocolNfs' {} a -> s {mountOptions = a} :: FsxProtocolNfs)

instance Data.FromJSON FsxProtocolNfs where
  parseJSON =
    Data.withObject
      "FsxProtocolNfs"
      ( \x ->
          FsxProtocolNfs'
            Prelude.<$> (x Data..:? "MountOptions")
      )

instance Prelude.Hashable FsxProtocolNfs where
  hashWithSalt _salt FsxProtocolNfs' {..} =
    _salt `Prelude.hashWithSalt` mountOptions

instance Prelude.NFData FsxProtocolNfs where
  rnf FsxProtocolNfs' {..} = Prelude.rnf mountOptions

instance Data.ToJSON FsxProtocolNfs where
  toJSON FsxProtocolNfs' {..} =
    Data.object
      ( Prelude.catMaybes
          [("MountOptions" Data..=) Prelude.<$> mountOptions]
      )
