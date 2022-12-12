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
-- Module      : Amazonka.OpsWorksCM.RestoreServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a backup to a server that is in a @CONNECTION_LOST@, @HEALTHY@,
-- @RUNNING@, @UNHEALTHY@, or @TERMINATED@ state. When you run
-- RestoreServer, the server\'s EC2 instance is deleted, and a new EC2
-- instance is configured. RestoreServer maintains the existing server
-- endpoint, so configuration management of the server\'s client devices
-- (nodes) should continue to work.
--
-- Restoring from a backup is performed by creating a new EC2 instance. If
-- restoration is successful, and the server is in a @HEALTHY@ state, AWS
-- OpsWorks CM switches traffic over to the new instance. After restoration
-- is finished, the old EC2 instance is maintained in a @Running@ or
-- @Stopped@ state, but is eventually terminated.
--
-- This operation is asynchronous.
--
-- An @InvalidStateException@ is thrown when the server is not in a valid
-- state. A @ResourceNotFoundException@ is thrown when the server does not
-- exist. A @ValidationException@ is raised when parameters of the request
-- are not valid.
module Amazonka.OpsWorksCM.RestoreServer
  ( -- * Creating a Request
    RestoreServer (..),
    newRestoreServer,

    -- * Request Lenses
    restoreServer_instanceType,
    restoreServer_keyPair,
    restoreServer_backupId,
    restoreServer_serverName,

    -- * Destructuring the Response
    RestoreServerResponse (..),
    newRestoreServerResponse,

    -- * Response Lenses
    restoreServerResponse_server,
    restoreServerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorksCM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreServer' smart constructor.
data RestoreServer = RestoreServer'
  { -- | The type of instance to restore. Valid values must be specified in the
    -- following format: @^([cm][34]|t2).*@ For example, @m5.large@. Valid
    -- values are @m5.large@, @r5.xlarge@, and @r5.2xlarge@. If you do not
    -- specify this parameter, RestoreServer uses the instance type from the
    -- specified backup.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair to set on the new EC2 instance. This can be
    -- helpful if the administrator no longer has the SSH key.
    keyPair :: Prelude.Maybe Prelude.Text,
    -- | The ID of the backup that you want to use to restore a server.
    backupId :: Prelude.Text,
    -- | The name of the server that you want to restore.
    serverName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'restoreServer_instanceType' - The type of instance to restore. Valid values must be specified in the
-- following format: @^([cm][34]|t2).*@ For example, @m5.large@. Valid
-- values are @m5.large@, @r5.xlarge@, and @r5.2xlarge@. If you do not
-- specify this parameter, RestoreServer uses the instance type from the
-- specified backup.
--
-- 'keyPair', 'restoreServer_keyPair' - The name of the key pair to set on the new EC2 instance. This can be
-- helpful if the administrator no longer has the SSH key.
--
-- 'backupId', 'restoreServer_backupId' - The ID of the backup that you want to use to restore a server.
--
-- 'serverName', 'restoreServer_serverName' - The name of the server that you want to restore.
newRestoreServer ::
  -- | 'backupId'
  Prelude.Text ->
  -- | 'serverName'
  Prelude.Text ->
  RestoreServer
newRestoreServer pBackupId_ pServerName_ =
  RestoreServer'
    { instanceType = Prelude.Nothing,
      keyPair = Prelude.Nothing,
      backupId = pBackupId_,
      serverName = pServerName_
    }

-- | The type of instance to restore. Valid values must be specified in the
-- following format: @^([cm][34]|t2).*@ For example, @m5.large@. Valid
-- values are @m5.large@, @r5.xlarge@, and @r5.2xlarge@. If you do not
-- specify this parameter, RestoreServer uses the instance type from the
-- specified backup.
restoreServer_instanceType :: Lens.Lens' RestoreServer (Prelude.Maybe Prelude.Text)
restoreServer_instanceType = Lens.lens (\RestoreServer' {instanceType} -> instanceType) (\s@RestoreServer' {} a -> s {instanceType = a} :: RestoreServer)

-- | The name of the key pair to set on the new EC2 instance. This can be
-- helpful if the administrator no longer has the SSH key.
restoreServer_keyPair :: Lens.Lens' RestoreServer (Prelude.Maybe Prelude.Text)
restoreServer_keyPair = Lens.lens (\RestoreServer' {keyPair} -> keyPair) (\s@RestoreServer' {} a -> s {keyPair = a} :: RestoreServer)

-- | The ID of the backup that you want to use to restore a server.
restoreServer_backupId :: Lens.Lens' RestoreServer Prelude.Text
restoreServer_backupId = Lens.lens (\RestoreServer' {backupId} -> backupId) (\s@RestoreServer' {} a -> s {backupId = a} :: RestoreServer)

-- | The name of the server that you want to restore.
restoreServer_serverName :: Lens.Lens' RestoreServer Prelude.Text
restoreServer_serverName = Lens.lens (\RestoreServer' {serverName} -> serverName) (\s@RestoreServer' {} a -> s {serverName = a} :: RestoreServer)

instance Core.AWSRequest RestoreServer where
  type
    AWSResponse RestoreServer =
      RestoreServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreServerResponse'
            Prelude.<$> (x Data..?> "Server")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreServer where
  hashWithSalt _salt RestoreServer' {..} =
    _salt `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` keyPair
      `Prelude.hashWithSalt` backupId
      `Prelude.hashWithSalt` serverName

instance Prelude.NFData RestoreServer where
  rnf RestoreServer' {..} =
    Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf keyPair
      `Prelude.seq` Prelude.rnf backupId
      `Prelude.seq` Prelude.rnf serverName

instance Data.ToHeaders RestoreServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorksCM_V2016_11_01.RestoreServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RestoreServer where
  toJSON RestoreServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceType" Data..=) Prelude.<$> instanceType,
            ("KeyPair" Data..=) Prelude.<$> keyPair,
            Prelude.Just ("BackupId" Data..= backupId),
            Prelude.Just ("ServerName" Data..= serverName)
          ]
      )

instance Data.ToPath RestoreServer where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreServerResponse' smart constructor.
data RestoreServerResponse = RestoreServerResponse'
  { server :: Prelude.Maybe Server,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'server', 'restoreServerResponse_server' - Undocumented member.
--
-- 'httpStatus', 'restoreServerResponse_httpStatus' - The response's http status code.
newRestoreServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreServerResponse
newRestoreServerResponse pHttpStatus_ =
  RestoreServerResponse'
    { server = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreServerResponse_server :: Lens.Lens' RestoreServerResponse (Prelude.Maybe Server)
restoreServerResponse_server = Lens.lens (\RestoreServerResponse' {server} -> server) (\s@RestoreServerResponse' {} a -> s {server = a} :: RestoreServerResponse)

-- | The response's http status code.
restoreServerResponse_httpStatus :: Lens.Lens' RestoreServerResponse Prelude.Int
restoreServerResponse_httpStatus = Lens.lens (\RestoreServerResponse' {httpStatus} -> httpStatus) (\s@RestoreServerResponse' {} a -> s {httpStatus = a} :: RestoreServerResponse)

instance Prelude.NFData RestoreServerResponse where
  rnf RestoreServerResponse' {..} =
    Prelude.rnf server
      `Prelude.seq` Prelude.rnf httpStatus
