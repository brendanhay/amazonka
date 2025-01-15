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
-- Module      : Amazonka.IAM.DeleteSSHPublicKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified SSH public key.
--
-- The SSH public key deleted by this operation is used only for
-- authenticating the associated IAM user to an CodeCommit repository. For
-- more information about using SSH keys to authenticate to an CodeCommit
-- repository, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up CodeCommit for SSH connections>
-- in the /CodeCommit User Guide/.
module Amazonka.IAM.DeleteSSHPublicKey
  ( -- * Creating a Request
    DeleteSSHPublicKey (..),
    newDeleteSSHPublicKey,

    -- * Request Lenses
    deleteSSHPublicKey_userName,
    deleteSSHPublicKey_sSHPublicKeyId,

    -- * Destructuring the Response
    DeleteSSHPublicKeyResponse (..),
    newDeleteSSHPublicKeyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSSHPublicKey' smart constructor.
data DeleteSSHPublicKey = DeleteSSHPublicKey'
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
    sSHPublicKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSSHPublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deleteSSHPublicKey_userName' - The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'sSHPublicKeyId', 'deleteSSHPublicKey_sSHPublicKeyId' - The unique identifier for the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
newDeleteSSHPublicKey ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'sSHPublicKeyId'
  Prelude.Text ->
  DeleteSSHPublicKey
newDeleteSSHPublicKey pUserName_ pSSHPublicKeyId_ =
  DeleteSSHPublicKey'
    { userName = pUserName_,
      sSHPublicKeyId = pSSHPublicKeyId_
    }

-- | The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteSSHPublicKey_userName :: Lens.Lens' DeleteSSHPublicKey Prelude.Text
deleteSSHPublicKey_userName = Lens.lens (\DeleteSSHPublicKey' {userName} -> userName) (\s@DeleteSSHPublicKey' {} a -> s {userName = a} :: DeleteSSHPublicKey)

-- | The unique identifier for the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
deleteSSHPublicKey_sSHPublicKeyId :: Lens.Lens' DeleteSSHPublicKey Prelude.Text
deleteSSHPublicKey_sSHPublicKeyId = Lens.lens (\DeleteSSHPublicKey' {sSHPublicKeyId} -> sSHPublicKeyId) (\s@DeleteSSHPublicKey' {} a -> s {sSHPublicKeyId = a} :: DeleteSSHPublicKey)

instance Core.AWSRequest DeleteSSHPublicKey where
  type
    AWSResponse DeleteSSHPublicKey =
      DeleteSSHPublicKeyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteSSHPublicKeyResponse'

instance Prelude.Hashable DeleteSSHPublicKey where
  hashWithSalt _salt DeleteSSHPublicKey' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` sSHPublicKeyId

instance Prelude.NFData DeleteSSHPublicKey where
  rnf DeleteSSHPublicKey' {..} =
    Prelude.rnf userName `Prelude.seq`
      Prelude.rnf sSHPublicKeyId

instance Data.ToHeaders DeleteSSHPublicKey where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteSSHPublicKey where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSSHPublicKey where
  toQuery DeleteSSHPublicKey' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteSSHPublicKey" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName,
        "SSHPublicKeyId" Data.=: sSHPublicKeyId
      ]

-- | /See:/ 'newDeleteSSHPublicKeyResponse' smart constructor.
data DeleteSSHPublicKeyResponse = DeleteSSHPublicKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSSHPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSSHPublicKeyResponse ::
  DeleteSSHPublicKeyResponse
newDeleteSSHPublicKeyResponse =
  DeleteSSHPublicKeyResponse'

instance Prelude.NFData DeleteSSHPublicKeyResponse where
  rnf _ = ()
