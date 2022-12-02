{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudHSMV2.ModifyBackupAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes for AWS CloudHSM backup.
module Amazonka.CloudHSMV2.ModifyBackupAttributes
  ( -- * Creating a Request
    ModifyBackupAttributes (..),
    newModifyBackupAttributes,

    -- * Request Lenses
    modifyBackupAttributes_backupId,
    modifyBackupAttributes_neverExpires,

    -- * Destructuring the Response
    ModifyBackupAttributesResponse (..),
    newModifyBackupAttributesResponse,

    -- * Response Lenses
    modifyBackupAttributesResponse_backup,
    modifyBackupAttributesResponse_httpStatus,
  )
where

import Amazonka.CloudHSMV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyBackupAttributes' smart constructor.
data ModifyBackupAttributes = ModifyBackupAttributes'
  { -- | The identifier (ID) of the backup to modify. To find the ID of a backup,
    -- use the DescribeBackups operation.
    backupId :: Prelude.Text,
    -- | Specifies whether the service should exempt a backup from the retention
    -- policy for the cluster. @True@ exempts a backup from the retention
    -- policy. @False@ means the service applies the backup retention policy
    -- defined at the cluster.
    neverExpires :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyBackupAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupId', 'modifyBackupAttributes_backupId' - The identifier (ID) of the backup to modify. To find the ID of a backup,
-- use the DescribeBackups operation.
--
-- 'neverExpires', 'modifyBackupAttributes_neverExpires' - Specifies whether the service should exempt a backup from the retention
-- policy for the cluster. @True@ exempts a backup from the retention
-- policy. @False@ means the service applies the backup retention policy
-- defined at the cluster.
newModifyBackupAttributes ::
  -- | 'backupId'
  Prelude.Text ->
  -- | 'neverExpires'
  Prelude.Bool ->
  ModifyBackupAttributes
newModifyBackupAttributes pBackupId_ pNeverExpires_ =
  ModifyBackupAttributes'
    { backupId = pBackupId_,
      neverExpires = pNeverExpires_
    }

-- | The identifier (ID) of the backup to modify. To find the ID of a backup,
-- use the DescribeBackups operation.
modifyBackupAttributes_backupId :: Lens.Lens' ModifyBackupAttributes Prelude.Text
modifyBackupAttributes_backupId = Lens.lens (\ModifyBackupAttributes' {backupId} -> backupId) (\s@ModifyBackupAttributes' {} a -> s {backupId = a} :: ModifyBackupAttributes)

-- | Specifies whether the service should exempt a backup from the retention
-- policy for the cluster. @True@ exempts a backup from the retention
-- policy. @False@ means the service applies the backup retention policy
-- defined at the cluster.
modifyBackupAttributes_neverExpires :: Lens.Lens' ModifyBackupAttributes Prelude.Bool
modifyBackupAttributes_neverExpires = Lens.lens (\ModifyBackupAttributes' {neverExpires} -> neverExpires) (\s@ModifyBackupAttributes' {} a -> s {neverExpires = a} :: ModifyBackupAttributes)

instance Core.AWSRequest ModifyBackupAttributes where
  type
    AWSResponse ModifyBackupAttributes =
      ModifyBackupAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyBackupAttributesResponse'
            Prelude.<$> (x Data..?> "Backup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyBackupAttributes where
  hashWithSalt _salt ModifyBackupAttributes' {..} =
    _salt `Prelude.hashWithSalt` backupId
      `Prelude.hashWithSalt` neverExpires

instance Prelude.NFData ModifyBackupAttributes where
  rnf ModifyBackupAttributes' {..} =
    Prelude.rnf backupId
      `Prelude.seq` Prelude.rnf neverExpires

instance Data.ToHeaders ModifyBackupAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BaldrApiService.ModifyBackupAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyBackupAttributes where
  toJSON ModifyBackupAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("BackupId" Data..= backupId),
            Prelude.Just ("NeverExpires" Data..= neverExpires)
          ]
      )

instance Data.ToPath ModifyBackupAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyBackupAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyBackupAttributesResponse' smart constructor.
data ModifyBackupAttributesResponse = ModifyBackupAttributesResponse'
  { backup :: Prelude.Maybe Backup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyBackupAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backup', 'modifyBackupAttributesResponse_backup' - Undocumented member.
--
-- 'httpStatus', 'modifyBackupAttributesResponse_httpStatus' - The response's http status code.
newModifyBackupAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyBackupAttributesResponse
newModifyBackupAttributesResponse pHttpStatus_ =
  ModifyBackupAttributesResponse'
    { backup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyBackupAttributesResponse_backup :: Lens.Lens' ModifyBackupAttributesResponse (Prelude.Maybe Backup)
modifyBackupAttributesResponse_backup = Lens.lens (\ModifyBackupAttributesResponse' {backup} -> backup) (\s@ModifyBackupAttributesResponse' {} a -> s {backup = a} :: ModifyBackupAttributesResponse)

-- | The response's http status code.
modifyBackupAttributesResponse_httpStatus :: Lens.Lens' ModifyBackupAttributesResponse Prelude.Int
modifyBackupAttributesResponse_httpStatus = Lens.lens (\ModifyBackupAttributesResponse' {httpStatus} -> httpStatus) (\s@ModifyBackupAttributesResponse' {} a -> s {httpStatus = a} :: ModifyBackupAttributesResponse)

instance
  Prelude.NFData
    ModifyBackupAttributesResponse
  where
  rnf ModifyBackupAttributesResponse' {..} =
    Prelude.rnf backup
      `Prelude.seq` Prelude.rnf httpStatus
