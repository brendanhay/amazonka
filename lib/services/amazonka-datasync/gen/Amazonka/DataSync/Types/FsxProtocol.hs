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
-- Module      : Amazonka.DataSync.Types.FsxProtocol
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.FsxProtocol where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.FsxProtocolNfs
import Amazonka.DataSync.Types.FsxProtocolSmb
import qualified Amazonka.Prelude as Prelude

-- | Specifies the data transfer protocol that DataSync uses to access your
-- Amazon FSx file system.
--
-- /See:/ 'newFsxProtocol' smart constructor.
data FsxProtocol = FsxProtocol'
  { -- | Specifies the Network File System (NFS) protocol configuration that
    -- DataSync uses to access your FSx for OpenZFS file system or FSx for
    -- ONTAP file system\'s storage virtual machine (SVM).
    nfs :: Prelude.Maybe FsxProtocolNfs,
    -- | Specifies the Server Message Block (SMB) protocol configuration that
    -- DataSync uses to access your FSx for ONTAP file system\'s SVM.
    smb :: Prelude.Maybe FsxProtocolSmb
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FsxProtocol' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nfs', 'fsxProtocol_nfs' - Specifies the Network File System (NFS) protocol configuration that
-- DataSync uses to access your FSx for OpenZFS file system or FSx for
-- ONTAP file system\'s storage virtual machine (SVM).
--
-- 'smb', 'fsxProtocol_smb' - Specifies the Server Message Block (SMB) protocol configuration that
-- DataSync uses to access your FSx for ONTAP file system\'s SVM.
newFsxProtocol ::
  FsxProtocol
newFsxProtocol =
  FsxProtocol'
    { nfs = Prelude.Nothing,
      smb = Prelude.Nothing
    }

-- | Specifies the Network File System (NFS) protocol configuration that
-- DataSync uses to access your FSx for OpenZFS file system or FSx for
-- ONTAP file system\'s storage virtual machine (SVM).
fsxProtocol_nfs :: Lens.Lens' FsxProtocol (Prelude.Maybe FsxProtocolNfs)
fsxProtocol_nfs = Lens.lens (\FsxProtocol' {nfs} -> nfs) (\s@FsxProtocol' {} a -> s {nfs = a} :: FsxProtocol)

-- | Specifies the Server Message Block (SMB) protocol configuration that
-- DataSync uses to access your FSx for ONTAP file system\'s SVM.
fsxProtocol_smb :: Lens.Lens' FsxProtocol (Prelude.Maybe FsxProtocolSmb)
fsxProtocol_smb = Lens.lens (\FsxProtocol' {smb} -> smb) (\s@FsxProtocol' {} a -> s {smb = a} :: FsxProtocol)

instance Data.FromJSON FsxProtocol where
  parseJSON =
    Data.withObject
      "FsxProtocol"
      ( \x ->
          FsxProtocol'
            Prelude.<$> (x Data..:? "NFS")
            Prelude.<*> (x Data..:? "SMB")
      )

instance Prelude.Hashable FsxProtocol where
  hashWithSalt _salt FsxProtocol' {..} =
    _salt
      `Prelude.hashWithSalt` nfs
      `Prelude.hashWithSalt` smb

instance Prelude.NFData FsxProtocol where
  rnf FsxProtocol' {..} =
    Prelude.rnf nfs `Prelude.seq` Prelude.rnf smb

instance Data.ToJSON FsxProtocol where
  toJSON FsxProtocol' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NFS" Data..=) Prelude.<$> nfs,
            ("SMB" Data..=) Prelude.<$> smb
          ]
      )
