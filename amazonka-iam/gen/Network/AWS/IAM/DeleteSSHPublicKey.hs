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

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
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
    userName :: Core.Text,
    -- | The unique identifier for the SSH public key.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that can consist of any upper or lowercased letter or digit.
    sSHPublicKeyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'sSHPublicKeyId'
  Core.Text ->
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
deleteSSHPublicKey_userName :: Lens.Lens' DeleteSSHPublicKey Core.Text
deleteSSHPublicKey_userName = Lens.lens (\DeleteSSHPublicKey' {userName} -> userName) (\s@DeleteSSHPublicKey' {} a -> s {userName = a} :: DeleteSSHPublicKey)

-- | The unique identifier for the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
deleteSSHPublicKey_sSHPublicKeyId :: Lens.Lens' DeleteSSHPublicKey Core.Text
deleteSSHPublicKey_sSHPublicKeyId = Lens.lens (\DeleteSSHPublicKey' {sSHPublicKeyId} -> sSHPublicKeyId) (\s@DeleteSSHPublicKey' {} a -> s {sSHPublicKeyId = a} :: DeleteSSHPublicKey)

instance Core.AWSRequest DeleteSSHPublicKey where
  type
    AWSResponse DeleteSSHPublicKey =
      DeleteSSHPublicKeyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteSSHPublicKeyResponse'

instance Core.Hashable DeleteSSHPublicKey

instance Core.NFData DeleteSSHPublicKey

instance Core.ToHeaders DeleteSSHPublicKey where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteSSHPublicKey where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSSHPublicKey where
  toQuery DeleteSSHPublicKey' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteSSHPublicKey" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "UserName" Core.=: userName,
        "SSHPublicKeyId" Core.=: sSHPublicKeyId
      ]

-- | /See:/ 'newDeleteSSHPublicKeyResponse' smart constructor.
data DeleteSSHPublicKeyResponse = DeleteSSHPublicKeyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSSHPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSSHPublicKeyResponse ::
  DeleteSSHPublicKeyResponse
newDeleteSSHPublicKeyResponse =
  DeleteSSHPublicKeyResponse'

instance Core.NFData DeleteSSHPublicKeyResponse
