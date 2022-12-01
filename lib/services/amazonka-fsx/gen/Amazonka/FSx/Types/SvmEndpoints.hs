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
-- Module      : Amazonka.FSx.Types.SvmEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.SvmEndpoints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.SvmEndpoint
import qualified Amazonka.Prelude as Prelude

-- | An Amazon FSx for NetApp ONTAP storage virtual machine (SVM) has the
-- following endpoints that are used to access data or to manage the SVM
-- using the NetApp ONTAP CLI, REST API, or NetApp CloudManager.
--
-- /See:/ 'newSvmEndpoints' smart constructor.
data SvmEndpoints = SvmEndpoints'
  { -- | An endpoint for connecting using the Server Message Block (SMB)
    -- protocol.
    smb :: Prelude.Maybe SvmEndpoint,
    -- | An endpoint for connecting using the Internet Small Computer Systems
    -- Interface (iSCSI) protocol.
    iscsi :: Prelude.Maybe SvmEndpoint,
    -- | An endpoint for connecting using the Network File System (NFS) protocol.
    nfs :: Prelude.Maybe SvmEndpoint,
    -- | An endpoint for managing SVMs using the NetApp ONTAP CLI, NetApp ONTAP
    -- API, or NetApp CloudManager.
    management :: Prelude.Maybe SvmEndpoint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SvmEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'smb', 'svmEndpoints_smb' - An endpoint for connecting using the Server Message Block (SMB)
-- protocol.
--
-- 'iscsi', 'svmEndpoints_iscsi' - An endpoint for connecting using the Internet Small Computer Systems
-- Interface (iSCSI) protocol.
--
-- 'nfs', 'svmEndpoints_nfs' - An endpoint for connecting using the Network File System (NFS) protocol.
--
-- 'management', 'svmEndpoints_management' - An endpoint for managing SVMs using the NetApp ONTAP CLI, NetApp ONTAP
-- API, or NetApp CloudManager.
newSvmEndpoints ::
  SvmEndpoints
newSvmEndpoints =
  SvmEndpoints'
    { smb = Prelude.Nothing,
      iscsi = Prelude.Nothing,
      nfs = Prelude.Nothing,
      management = Prelude.Nothing
    }

-- | An endpoint for connecting using the Server Message Block (SMB)
-- protocol.
svmEndpoints_smb :: Lens.Lens' SvmEndpoints (Prelude.Maybe SvmEndpoint)
svmEndpoints_smb = Lens.lens (\SvmEndpoints' {smb} -> smb) (\s@SvmEndpoints' {} a -> s {smb = a} :: SvmEndpoints)

-- | An endpoint for connecting using the Internet Small Computer Systems
-- Interface (iSCSI) protocol.
svmEndpoints_iscsi :: Lens.Lens' SvmEndpoints (Prelude.Maybe SvmEndpoint)
svmEndpoints_iscsi = Lens.lens (\SvmEndpoints' {iscsi} -> iscsi) (\s@SvmEndpoints' {} a -> s {iscsi = a} :: SvmEndpoints)

-- | An endpoint for connecting using the Network File System (NFS) protocol.
svmEndpoints_nfs :: Lens.Lens' SvmEndpoints (Prelude.Maybe SvmEndpoint)
svmEndpoints_nfs = Lens.lens (\SvmEndpoints' {nfs} -> nfs) (\s@SvmEndpoints' {} a -> s {nfs = a} :: SvmEndpoints)

-- | An endpoint for managing SVMs using the NetApp ONTAP CLI, NetApp ONTAP
-- API, or NetApp CloudManager.
svmEndpoints_management :: Lens.Lens' SvmEndpoints (Prelude.Maybe SvmEndpoint)
svmEndpoints_management = Lens.lens (\SvmEndpoints' {management} -> management) (\s@SvmEndpoints' {} a -> s {management = a} :: SvmEndpoints)

instance Core.FromJSON SvmEndpoints where
  parseJSON =
    Core.withObject
      "SvmEndpoints"
      ( \x ->
          SvmEndpoints'
            Prelude.<$> (x Core..:? "Smb")
            Prelude.<*> (x Core..:? "Iscsi")
            Prelude.<*> (x Core..:? "Nfs")
            Prelude.<*> (x Core..:? "Management")
      )

instance Prelude.Hashable SvmEndpoints where
  hashWithSalt _salt SvmEndpoints' {..} =
    _salt `Prelude.hashWithSalt` smb
      `Prelude.hashWithSalt` iscsi
      `Prelude.hashWithSalt` nfs
      `Prelude.hashWithSalt` management

instance Prelude.NFData SvmEndpoints where
  rnf SvmEndpoints' {..} =
    Prelude.rnf smb
      `Prelude.seq` Prelude.rnf iscsi
      `Prelude.seq` Prelude.rnf nfs
      `Prelude.seq` Prelude.rnf management
