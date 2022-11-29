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
-- Module      : Amazonka.OpsWorksCM.UpdateServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a server.
--
-- This operation is synchronous.
module Amazonka.OpsWorksCM.UpdateServer
  ( -- * Creating a Request
    UpdateServer (..),
    newUpdateServer,

    -- * Request Lenses
    updateServer_preferredBackupWindow,
    updateServer_backupRetentionCount,
    updateServer_preferredMaintenanceWindow,
    updateServer_disableAutomatedBackup,
    updateServer_serverName,

    -- * Destructuring the Response
    UpdateServerResponse (..),
    newUpdateServerResponse,

    -- * Response Lenses
    updateServerResponse_server,
    updateServerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorksCM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServer' smart constructor.
data UpdateServer = UpdateServer'
  { preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | Sets the number of automated backups that you want to keep.
    backupRetentionCount :: Prelude.Maybe Prelude.Int,
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | Setting DisableAutomatedBackup to @true@ disables automated or scheduled
    -- backups. Automated backups are enabled by default.
    disableAutomatedBackup :: Prelude.Maybe Prelude.Bool,
    -- | The name of the server to update.
    serverName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredBackupWindow', 'updateServer_preferredBackupWindow' - Undocumented member.
--
-- 'backupRetentionCount', 'updateServer_backupRetentionCount' - Sets the number of automated backups that you want to keep.
--
-- 'preferredMaintenanceWindow', 'updateServer_preferredMaintenanceWindow' - Undocumented member.
--
-- 'disableAutomatedBackup', 'updateServer_disableAutomatedBackup' - Setting DisableAutomatedBackup to @true@ disables automated or scheduled
-- backups. Automated backups are enabled by default.
--
-- 'serverName', 'updateServer_serverName' - The name of the server to update.
newUpdateServer ::
  -- | 'serverName'
  Prelude.Text ->
  UpdateServer
newUpdateServer pServerName_ =
  UpdateServer'
    { preferredBackupWindow =
        Prelude.Nothing,
      backupRetentionCount = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      disableAutomatedBackup = Prelude.Nothing,
      serverName = pServerName_
    }

-- | Undocumented member.
updateServer_preferredBackupWindow :: Lens.Lens' UpdateServer (Prelude.Maybe Prelude.Text)
updateServer_preferredBackupWindow = Lens.lens (\UpdateServer' {preferredBackupWindow} -> preferredBackupWindow) (\s@UpdateServer' {} a -> s {preferredBackupWindow = a} :: UpdateServer)

-- | Sets the number of automated backups that you want to keep.
updateServer_backupRetentionCount :: Lens.Lens' UpdateServer (Prelude.Maybe Prelude.Int)
updateServer_backupRetentionCount = Lens.lens (\UpdateServer' {backupRetentionCount} -> backupRetentionCount) (\s@UpdateServer' {} a -> s {backupRetentionCount = a} :: UpdateServer)

-- | Undocumented member.
updateServer_preferredMaintenanceWindow :: Lens.Lens' UpdateServer (Prelude.Maybe Prelude.Text)
updateServer_preferredMaintenanceWindow = Lens.lens (\UpdateServer' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@UpdateServer' {} a -> s {preferredMaintenanceWindow = a} :: UpdateServer)

-- | Setting DisableAutomatedBackup to @true@ disables automated or scheduled
-- backups. Automated backups are enabled by default.
updateServer_disableAutomatedBackup :: Lens.Lens' UpdateServer (Prelude.Maybe Prelude.Bool)
updateServer_disableAutomatedBackup = Lens.lens (\UpdateServer' {disableAutomatedBackup} -> disableAutomatedBackup) (\s@UpdateServer' {} a -> s {disableAutomatedBackup = a} :: UpdateServer)

-- | The name of the server to update.
updateServer_serverName :: Lens.Lens' UpdateServer Prelude.Text
updateServer_serverName = Lens.lens (\UpdateServer' {serverName} -> serverName) (\s@UpdateServer' {} a -> s {serverName = a} :: UpdateServer)

instance Core.AWSRequest UpdateServer where
  type AWSResponse UpdateServer = UpdateServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServerResponse'
            Prelude.<$> (x Core..?> "Server")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServer where
  hashWithSalt _salt UpdateServer' {..} =
    _salt `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` backupRetentionCount
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` disableAutomatedBackup
      `Prelude.hashWithSalt` serverName

instance Prelude.NFData UpdateServer where
  rnf UpdateServer' {..} =
    Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf backupRetentionCount
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf disableAutomatedBackup
      `Prelude.seq` Prelude.rnf serverName

instance Core.ToHeaders UpdateServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorksCM_V2016_11_01.UpdateServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateServer where
  toJSON UpdateServer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PreferredBackupWindow" Core..=)
              Prelude.<$> preferredBackupWindow,
            ("BackupRetentionCount" Core..=)
              Prelude.<$> backupRetentionCount,
            ("PreferredMaintenanceWindow" Core..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("DisableAutomatedBackup" Core..=)
              Prelude.<$> disableAutomatedBackup,
            Prelude.Just ("ServerName" Core..= serverName)
          ]
      )

instance Core.ToPath UpdateServer where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServerResponse' smart constructor.
data UpdateServerResponse = UpdateServerResponse'
  { -- | Contains the response to a @UpdateServer@ request.
    server :: Prelude.Maybe Server,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'server', 'updateServerResponse_server' - Contains the response to a @UpdateServer@ request.
--
-- 'httpStatus', 'updateServerResponse_httpStatus' - The response's http status code.
newUpdateServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServerResponse
newUpdateServerResponse pHttpStatus_ =
  UpdateServerResponse'
    { server = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the response to a @UpdateServer@ request.
updateServerResponse_server :: Lens.Lens' UpdateServerResponse (Prelude.Maybe Server)
updateServerResponse_server = Lens.lens (\UpdateServerResponse' {server} -> server) (\s@UpdateServerResponse' {} a -> s {server = a} :: UpdateServerResponse)

-- | The response's http status code.
updateServerResponse_httpStatus :: Lens.Lens' UpdateServerResponse Prelude.Int
updateServerResponse_httpStatus = Lens.lens (\UpdateServerResponse' {httpStatus} -> httpStatus) (\s@UpdateServerResponse' {} a -> s {httpStatus = a} :: UpdateServerResponse)

instance Prelude.NFData UpdateServerResponse where
  rnf UpdateServerResponse' {..} =
    Prelude.rnf server
      `Prelude.seq` Prelude.rnf httpStatus
