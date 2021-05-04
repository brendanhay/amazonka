{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IAM.DeleteSSHPublicKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified SSH public key.
--
-- The SSH public key deleted by this operation is used only for
-- authenticating the associated IAM user to an AWS CodeCommit repository.
-- For more information about using SSH keys to authenticate to an AWS
-- CodeCommit repository, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH connections>
-- in the /AWS CodeCommit User Guide/.
module Network.AWS.IAM.DeleteSSHPublicKey
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteSSHPublicKey where
  type
    Rs DeleteSSHPublicKey =
      DeleteSSHPublicKeyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteSSHPublicKeyResponse'

instance Prelude.Hashable DeleteSSHPublicKey

instance Prelude.NFData DeleteSSHPublicKey

instance Prelude.ToHeaders DeleteSSHPublicKey where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteSSHPublicKey where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteSSHPublicKey where
  toQuery DeleteSSHPublicKey' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteSSHPublicKey" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "SSHPublicKeyId" Prelude.=: sSHPublicKeyId
      ]

-- | /See:/ 'newDeleteSSHPublicKeyResponse' smart constructor.
data DeleteSSHPublicKeyResponse = DeleteSSHPublicKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSSHPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSSHPublicKeyResponse ::
  DeleteSSHPublicKeyResponse
newDeleteSSHPublicKeyResponse =
  DeleteSSHPublicKeyResponse'

instance Prelude.NFData DeleteSSHPublicKeyResponse
