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
-- Module      : Amazonka.Transfer.ImportSshPublicKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a Secure Shell (SSH) public key to a user account identified by a
-- @UserName@ value assigned to the specific file transfer protocol-enabled
-- server, identified by @ServerId@.
--
-- The response returns the @UserName@ value, the @ServerId@ value, and the
-- name of the @SshPublicKeyId@.
module Amazonka.Transfer.ImportSshPublicKey
  ( -- * Creating a Request
    ImportSshPublicKey (..),
    newImportSshPublicKey,

    -- * Request Lenses
    importSshPublicKey_serverId,
    importSshPublicKey_sshPublicKeyBody,
    importSshPublicKey_userName,

    -- * Destructuring the Response
    ImportSshPublicKeyResponse (..),
    newImportSshPublicKeyResponse,

    -- * Response Lenses
    importSshPublicKeyResponse_httpStatus,
    importSshPublicKeyResponse_serverId,
    importSshPublicKeyResponse_sshPublicKeyId,
    importSshPublicKeyResponse_userName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newImportSshPublicKey' smart constructor.
data ImportSshPublicKey = ImportSshPublicKey'
  { -- | A system-assigned unique identifier for a server.
    serverId :: Prelude.Text,
    -- | The public key portion of an SSH key pair.
    --
    -- Transfer Family accepts RSA, ECDSA, and ED25519 keys.
    sshPublicKeyBody :: Prelude.Text,
    -- | The name of the user account that is assigned to one or more servers.
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSshPublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'importSshPublicKey_serverId' - A system-assigned unique identifier for a server.
--
-- 'sshPublicKeyBody', 'importSshPublicKey_sshPublicKeyBody' - The public key portion of an SSH key pair.
--
-- Transfer Family accepts RSA, ECDSA, and ED25519 keys.
--
-- 'userName', 'importSshPublicKey_userName' - The name of the user account that is assigned to one or more servers.
newImportSshPublicKey ::
  -- | 'serverId'
  Prelude.Text ->
  -- | 'sshPublicKeyBody'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  ImportSshPublicKey
newImportSshPublicKey
  pServerId_
  pSshPublicKeyBody_
  pUserName_ =
    ImportSshPublicKey'
      { serverId = pServerId_,
        sshPublicKeyBody = pSshPublicKeyBody_,
        userName = pUserName_
      }

-- | A system-assigned unique identifier for a server.
importSshPublicKey_serverId :: Lens.Lens' ImportSshPublicKey Prelude.Text
importSshPublicKey_serverId = Lens.lens (\ImportSshPublicKey' {serverId} -> serverId) (\s@ImportSshPublicKey' {} a -> s {serverId = a} :: ImportSshPublicKey)

-- | The public key portion of an SSH key pair.
--
-- Transfer Family accepts RSA, ECDSA, and ED25519 keys.
importSshPublicKey_sshPublicKeyBody :: Lens.Lens' ImportSshPublicKey Prelude.Text
importSshPublicKey_sshPublicKeyBody = Lens.lens (\ImportSshPublicKey' {sshPublicKeyBody} -> sshPublicKeyBody) (\s@ImportSshPublicKey' {} a -> s {sshPublicKeyBody = a} :: ImportSshPublicKey)

-- | The name of the user account that is assigned to one or more servers.
importSshPublicKey_userName :: Lens.Lens' ImportSshPublicKey Prelude.Text
importSshPublicKey_userName = Lens.lens (\ImportSshPublicKey' {userName} -> userName) (\s@ImportSshPublicKey' {} a -> s {userName = a} :: ImportSshPublicKey)

instance Core.AWSRequest ImportSshPublicKey where
  type
    AWSResponse ImportSshPublicKey =
      ImportSshPublicKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportSshPublicKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ServerId")
            Prelude.<*> (x Data..:> "SshPublicKeyId")
            Prelude.<*> (x Data..:> "UserName")
      )

instance Prelude.Hashable ImportSshPublicKey where
  hashWithSalt _salt ImportSshPublicKey' {..} =
    _salt
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` sshPublicKeyBody
      `Prelude.hashWithSalt` userName

instance Prelude.NFData ImportSshPublicKey where
  rnf ImportSshPublicKey' {..} =
    Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf sshPublicKeyBody
      `Prelude.seq` Prelude.rnf userName

instance Data.ToHeaders ImportSshPublicKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.ImportSshPublicKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportSshPublicKey where
  toJSON ImportSshPublicKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServerId" Data..= serverId),
            Prelude.Just
              ("SshPublicKeyBody" Data..= sshPublicKeyBody),
            Prelude.Just ("UserName" Data..= userName)
          ]
      )

instance Data.ToPath ImportSshPublicKey where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportSshPublicKey where
  toQuery = Prelude.const Prelude.mempty

-- | Identifies the user, the server they belong to, and the identifier of
-- the SSH public key associated with that user. A user can have more than
-- one key on each server that they are associated with.
--
-- /See:/ 'newImportSshPublicKeyResponse' smart constructor.
data ImportSshPublicKeyResponse = ImportSshPublicKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A system-assigned unique identifier for a server.
    serverId :: Prelude.Text,
    -- | The name given to a public key by the system that was imported.
    sshPublicKeyId :: Prelude.Text,
    -- | A user name assigned to the @ServerID@ value that you specified.
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSshPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'importSshPublicKeyResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'importSshPublicKeyResponse_serverId' - A system-assigned unique identifier for a server.
--
-- 'sshPublicKeyId', 'importSshPublicKeyResponse_sshPublicKeyId' - The name given to a public key by the system that was imported.
--
-- 'userName', 'importSshPublicKeyResponse_userName' - A user name assigned to the @ServerID@ value that you specified.
newImportSshPublicKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  -- | 'sshPublicKeyId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  ImportSshPublicKeyResponse
newImportSshPublicKeyResponse
  pHttpStatus_
  pServerId_
  pSshPublicKeyId_
  pUserName_ =
    ImportSshPublicKeyResponse'
      { httpStatus =
          pHttpStatus_,
        serverId = pServerId_,
        sshPublicKeyId = pSshPublicKeyId_,
        userName = pUserName_
      }

-- | The response's http status code.
importSshPublicKeyResponse_httpStatus :: Lens.Lens' ImportSshPublicKeyResponse Prelude.Int
importSshPublicKeyResponse_httpStatus = Lens.lens (\ImportSshPublicKeyResponse' {httpStatus} -> httpStatus) (\s@ImportSshPublicKeyResponse' {} a -> s {httpStatus = a} :: ImportSshPublicKeyResponse)

-- | A system-assigned unique identifier for a server.
importSshPublicKeyResponse_serverId :: Lens.Lens' ImportSshPublicKeyResponse Prelude.Text
importSshPublicKeyResponse_serverId = Lens.lens (\ImportSshPublicKeyResponse' {serverId} -> serverId) (\s@ImportSshPublicKeyResponse' {} a -> s {serverId = a} :: ImportSshPublicKeyResponse)

-- | The name given to a public key by the system that was imported.
importSshPublicKeyResponse_sshPublicKeyId :: Lens.Lens' ImportSshPublicKeyResponse Prelude.Text
importSshPublicKeyResponse_sshPublicKeyId = Lens.lens (\ImportSshPublicKeyResponse' {sshPublicKeyId} -> sshPublicKeyId) (\s@ImportSshPublicKeyResponse' {} a -> s {sshPublicKeyId = a} :: ImportSshPublicKeyResponse)

-- | A user name assigned to the @ServerID@ value that you specified.
importSshPublicKeyResponse_userName :: Lens.Lens' ImportSshPublicKeyResponse Prelude.Text
importSshPublicKeyResponse_userName = Lens.lens (\ImportSshPublicKeyResponse' {userName} -> userName) (\s@ImportSshPublicKeyResponse' {} a -> s {userName = a} :: ImportSshPublicKeyResponse)

instance Prelude.NFData ImportSshPublicKeyResponse where
  rnf ImportSshPublicKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf sshPublicKeyId
      `Prelude.seq` Prelude.rnf userName
