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
-- Module      : Amazonka.IAM.UpdateSSHPublicKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the status of an IAM user\'s SSH public key to active or inactive.
-- SSH public keys that are inactive cannot be used for authentication.
-- This operation can be used to disable a user\'s SSH public key as part
-- of a key rotation work flow.
--
-- The SSH public key affected by this operation is used only for
-- authenticating the associated IAM user to an CodeCommit repository. For
-- more information about using SSH keys to authenticate to an CodeCommit
-- repository, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up CodeCommit for SSH connections>
-- in the /CodeCommit User Guide/.
module Amazonka.IAM.UpdateSSHPublicKey
  ( -- * Creating a Request
    UpdateSSHPublicKey (..),
    newUpdateSSHPublicKey,

    -- * Request Lenses
    updateSSHPublicKey_userName,
    updateSSHPublicKey_sSHPublicKeyId,
    updateSSHPublicKey_status,

    -- * Destructuring the Response
    UpdateSSHPublicKeyResponse (..),
    newUpdateSSHPublicKeyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSSHPublicKey' smart constructor.
data UpdateSSHPublicKey = UpdateSSHPublicKey'
  { -- | The name of the IAM user associated with the SSH public key.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
    -- | The unique identifier for the SSH public key.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that can consist of any upper or lowercased letter or digit.
    sSHPublicKeyId :: Prelude.Text,
    -- | The status to assign to the SSH public key. @Active@ means that the key
    -- can be used for authentication with an CodeCommit repository. @Inactive@
    -- means that the key cannot be used.
    status :: StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSSHPublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'updateSSHPublicKey_userName' - The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'sSHPublicKeyId', 'updateSSHPublicKey_sSHPublicKeyId' - The unique identifier for the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
--
-- 'status', 'updateSSHPublicKey_status' - The status to assign to the SSH public key. @Active@ means that the key
-- can be used for authentication with an CodeCommit repository. @Inactive@
-- means that the key cannot be used.
newUpdateSSHPublicKey ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'sSHPublicKeyId'
  Prelude.Text ->
  -- | 'status'
  StatusType ->
  UpdateSSHPublicKey
newUpdateSSHPublicKey
  pUserName_
  pSSHPublicKeyId_
  pStatus_ =
    UpdateSSHPublicKey'
      { userName = pUserName_,
        sSHPublicKeyId = pSSHPublicKeyId_,
        status = pStatus_
      }

-- | The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateSSHPublicKey_userName :: Lens.Lens' UpdateSSHPublicKey Prelude.Text
updateSSHPublicKey_userName = Lens.lens (\UpdateSSHPublicKey' {userName} -> userName) (\s@UpdateSSHPublicKey' {} a -> s {userName = a} :: UpdateSSHPublicKey)

-- | The unique identifier for the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
updateSSHPublicKey_sSHPublicKeyId :: Lens.Lens' UpdateSSHPublicKey Prelude.Text
updateSSHPublicKey_sSHPublicKeyId = Lens.lens (\UpdateSSHPublicKey' {sSHPublicKeyId} -> sSHPublicKeyId) (\s@UpdateSSHPublicKey' {} a -> s {sSHPublicKeyId = a} :: UpdateSSHPublicKey)

-- | The status to assign to the SSH public key. @Active@ means that the key
-- can be used for authentication with an CodeCommit repository. @Inactive@
-- means that the key cannot be used.
updateSSHPublicKey_status :: Lens.Lens' UpdateSSHPublicKey StatusType
updateSSHPublicKey_status = Lens.lens (\UpdateSSHPublicKey' {status} -> status) (\s@UpdateSSHPublicKey' {} a -> s {status = a} :: UpdateSSHPublicKey)

instance Core.AWSRequest UpdateSSHPublicKey where
  type
    AWSResponse UpdateSSHPublicKey =
      UpdateSSHPublicKeyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull UpdateSSHPublicKeyResponse'

instance Prelude.Hashable UpdateSSHPublicKey where
  hashWithSalt _salt UpdateSSHPublicKey' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` sSHPublicKeyId
      `Prelude.hashWithSalt` status

instance Prelude.NFData UpdateSSHPublicKey where
  rnf UpdateSSHPublicKey' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf sSHPublicKeyId
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders UpdateSSHPublicKey where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateSSHPublicKey where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSSHPublicKey where
  toQuery UpdateSSHPublicKey' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdateSSHPublicKey" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName,
        "SSHPublicKeyId" Data.=: sSHPublicKeyId,
        "Status" Data.=: status
      ]

-- | /See:/ 'newUpdateSSHPublicKeyResponse' smart constructor.
data UpdateSSHPublicKeyResponse = UpdateSSHPublicKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSSHPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateSSHPublicKeyResponse ::
  UpdateSSHPublicKeyResponse
newUpdateSSHPublicKeyResponse =
  UpdateSSHPublicKeyResponse'

instance Prelude.NFData UpdateSSHPublicKeyResponse where
  rnf _ = ()
