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
-- Module      : Network.AWS.IAM.UpdateSSHPublicKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the status of an IAM user\'s SSH public key to active or inactive.
-- SSH public keys that are inactive cannot be used for authentication.
-- This operation can be used to disable a user\'s SSH public key as part
-- of a key rotation work flow.
--
-- The SSH public key affected by this operation is used only for
-- authenticating the associated IAM user to an AWS CodeCommit repository.
-- For more information about using SSH keys to authenticate to an AWS
-- CodeCommit repository, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH connections>
-- in the /AWS CodeCommit User Guide/.
module Network.AWS.IAM.UpdateSSHPublicKey
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    -- can be used for authentication with an AWS CodeCommit repository.
    -- @Inactive@ means that the key cannot be used.
    status :: StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- can be used for authentication with an AWS CodeCommit repository.
-- @Inactive@ means that the key cannot be used.
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
-- can be used for authentication with an AWS CodeCommit repository.
-- @Inactive@ means that the key cannot be used.
updateSSHPublicKey_status :: Lens.Lens' UpdateSSHPublicKey StatusType
updateSSHPublicKey_status = Lens.lens (\UpdateSSHPublicKey' {status} -> status) (\s@UpdateSSHPublicKey' {} a -> s {status = a} :: UpdateSSHPublicKey)

instance Prelude.AWSRequest UpdateSSHPublicKey where
  type
    Rs UpdateSSHPublicKey =
      UpdateSSHPublicKeyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull UpdateSSHPublicKeyResponse'

instance Prelude.Hashable UpdateSSHPublicKey

instance Prelude.NFData UpdateSSHPublicKey

instance Prelude.ToHeaders UpdateSSHPublicKey where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateSSHPublicKey where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateSSHPublicKey where
  toQuery UpdateSSHPublicKey' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateSSHPublicKey" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "SSHPublicKeyId" Prelude.=: sSHPublicKeyId,
        "Status" Prelude.=: status
      ]

-- | /See:/ 'newUpdateSSHPublicKeyResponse' smart constructor.
data UpdateSSHPublicKeyResponse = UpdateSSHPublicKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSSHPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateSSHPublicKeyResponse ::
  UpdateSSHPublicKeyResponse
newUpdateSSHPublicKeyResponse =
  UpdateSSHPublicKeyResponse'

instance Prelude.NFData UpdateSSHPublicKeyResponse
