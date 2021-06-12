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
-- Module      : Network.AWS.CloudHSMv2.ModifyBackupAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes for AWS CloudHSM backup.
module Network.AWS.CloudHSMv2.ModifyBackupAttributes
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

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyBackupAttributes' smart constructor.
data ModifyBackupAttributes = ModifyBackupAttributes'
  { -- | The identifier (ID) of the backup to modify. To find the ID of a backup,
    -- use the DescribeBackups operation.
    backupId :: Core.Text,
    -- | Specifies whether the service should exempt a backup from the retention
    -- policy for the cluster. @True@ exempts a backup from the retention
    -- policy. @False@ means the service applies the backup retention policy
    -- defined at the cluster.
    neverExpires :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'neverExpires'
  Core.Bool ->
  ModifyBackupAttributes
newModifyBackupAttributes pBackupId_ pNeverExpires_ =
  ModifyBackupAttributes'
    { backupId = pBackupId_,
      neverExpires = pNeverExpires_
    }

-- | The identifier (ID) of the backup to modify. To find the ID of a backup,
-- use the DescribeBackups operation.
modifyBackupAttributes_backupId :: Lens.Lens' ModifyBackupAttributes Core.Text
modifyBackupAttributes_backupId = Lens.lens (\ModifyBackupAttributes' {backupId} -> backupId) (\s@ModifyBackupAttributes' {} a -> s {backupId = a} :: ModifyBackupAttributes)

-- | Specifies whether the service should exempt a backup from the retention
-- policy for the cluster. @True@ exempts a backup from the retention
-- policy. @False@ means the service applies the backup retention policy
-- defined at the cluster.
modifyBackupAttributes_neverExpires :: Lens.Lens' ModifyBackupAttributes Core.Bool
modifyBackupAttributes_neverExpires = Lens.lens (\ModifyBackupAttributes' {neverExpires} -> neverExpires) (\s@ModifyBackupAttributes' {} a -> s {neverExpires = a} :: ModifyBackupAttributes)

instance Core.AWSRequest ModifyBackupAttributes where
  type
    AWSResponse ModifyBackupAttributes =
      ModifyBackupAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyBackupAttributesResponse'
            Core.<$> (x Core..?> "Backup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyBackupAttributes

instance Core.NFData ModifyBackupAttributes

instance Core.ToHeaders ModifyBackupAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "BaldrApiService.ModifyBackupAttributes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyBackupAttributes where
  toJSON ModifyBackupAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BackupId" Core..= backupId),
            Core.Just ("NeverExpires" Core..= neverExpires)
          ]
      )

instance Core.ToPath ModifyBackupAttributes where
  toPath = Core.const "/"

instance Core.ToQuery ModifyBackupAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyBackupAttributesResponse' smart constructor.
data ModifyBackupAttributesResponse = ModifyBackupAttributesResponse'
  { backup :: Core.Maybe Backup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyBackupAttributesResponse
newModifyBackupAttributesResponse pHttpStatus_ =
  ModifyBackupAttributesResponse'
    { backup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyBackupAttributesResponse_backup :: Lens.Lens' ModifyBackupAttributesResponse (Core.Maybe Backup)
modifyBackupAttributesResponse_backup = Lens.lens (\ModifyBackupAttributesResponse' {backup} -> backup) (\s@ModifyBackupAttributesResponse' {} a -> s {backup = a} :: ModifyBackupAttributesResponse)

-- | The response's http status code.
modifyBackupAttributesResponse_httpStatus :: Lens.Lens' ModifyBackupAttributesResponse Core.Int
modifyBackupAttributesResponse_httpStatus = Lens.lens (\ModifyBackupAttributesResponse' {httpStatus} -> httpStatus) (\s@ModifyBackupAttributesResponse' {} a -> s {httpStatus = a} :: ModifyBackupAttributesResponse)

instance Core.NFData ModifyBackupAttributesResponse
