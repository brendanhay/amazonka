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
-- Module      : Amazonka.SSOAdmin.Types.PermissionSetProvisioningStatusMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.PermissionSetProvisioningStatusMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSOAdmin.Types.StatusValues

-- | Provides information about the permission set provisioning status.
--
-- /See:/ 'newPermissionSetProvisioningStatusMetadata' smart constructor.
data PermissionSetProvisioningStatusMetadata = PermissionSetProvisioningStatusMetadata'
  { -- | The date that the permission set was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The identifier for tracking the request operation that is generated by
    -- the universally unique identifier (UUID) workflow.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The status of the permission set provisioning process.
    status :: Prelude.Maybe StatusValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PermissionSetProvisioningStatusMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'permissionSetProvisioningStatusMetadata_createdDate' - The date that the permission set was created.
--
-- 'requestId', 'permissionSetProvisioningStatusMetadata_requestId' - The identifier for tracking the request operation that is generated by
-- the universally unique identifier (UUID) workflow.
--
-- 'status', 'permissionSetProvisioningStatusMetadata_status' - The status of the permission set provisioning process.
newPermissionSetProvisioningStatusMetadata ::
  PermissionSetProvisioningStatusMetadata
newPermissionSetProvisioningStatusMetadata =
  PermissionSetProvisioningStatusMetadata'
    { createdDate =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date that the permission set was created.
permissionSetProvisioningStatusMetadata_createdDate :: Lens.Lens' PermissionSetProvisioningStatusMetadata (Prelude.Maybe Prelude.UTCTime)
permissionSetProvisioningStatusMetadata_createdDate = Lens.lens (\PermissionSetProvisioningStatusMetadata' {createdDate} -> createdDate) (\s@PermissionSetProvisioningStatusMetadata' {} a -> s {createdDate = a} :: PermissionSetProvisioningStatusMetadata) Prelude.. Lens.mapping Data._Time

-- | The identifier for tracking the request operation that is generated by
-- the universally unique identifier (UUID) workflow.
permissionSetProvisioningStatusMetadata_requestId :: Lens.Lens' PermissionSetProvisioningStatusMetadata (Prelude.Maybe Prelude.Text)
permissionSetProvisioningStatusMetadata_requestId = Lens.lens (\PermissionSetProvisioningStatusMetadata' {requestId} -> requestId) (\s@PermissionSetProvisioningStatusMetadata' {} a -> s {requestId = a} :: PermissionSetProvisioningStatusMetadata)

-- | The status of the permission set provisioning process.
permissionSetProvisioningStatusMetadata_status :: Lens.Lens' PermissionSetProvisioningStatusMetadata (Prelude.Maybe StatusValues)
permissionSetProvisioningStatusMetadata_status = Lens.lens (\PermissionSetProvisioningStatusMetadata' {status} -> status) (\s@PermissionSetProvisioningStatusMetadata' {} a -> s {status = a} :: PermissionSetProvisioningStatusMetadata)

instance
  Data.FromJSON
    PermissionSetProvisioningStatusMetadata
  where
  parseJSON =
    Data.withObject
      "PermissionSetProvisioningStatusMetadata"
      ( \x ->
          PermissionSetProvisioningStatusMetadata'
            Prelude.<$> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "RequestId")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    PermissionSetProvisioningStatusMetadata
  where
  hashWithSalt
    _salt
    PermissionSetProvisioningStatusMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` createdDate
        `Prelude.hashWithSalt` requestId
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    PermissionSetProvisioningStatusMetadata
  where
  rnf PermissionSetProvisioningStatusMetadata' {..} =
    Prelude.rnf createdDate `Prelude.seq`
      Prelude.rnf requestId `Prelude.seq`
        Prelude.rnf status
