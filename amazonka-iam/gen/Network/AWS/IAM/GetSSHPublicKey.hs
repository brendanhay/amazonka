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
-- Module      : Network.AWS.IAM.GetSSHPublicKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified SSH public key, including metadata about the
-- key.
--
-- The SSH public key retrieved by this operation is used only for
-- authenticating the associated IAM user to an AWS CodeCommit repository.
-- For more information about using SSH keys to authenticate to an AWS
-- CodeCommit repository, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH connections>
-- in the /AWS CodeCommit User Guide/.
module Network.AWS.IAM.GetSSHPublicKey
  ( -- * Creating a Request
    GetSSHPublicKey (..),
    newGetSSHPublicKey,

    -- * Request Lenses
    getSSHPublicKey_userName,
    getSSHPublicKey_sSHPublicKeyId,
    getSSHPublicKey_encoding,

    -- * Destructuring the Response
    GetSSHPublicKeyResponse (..),
    newGetSSHPublicKeyResponse,

    -- * Response Lenses
    getSSHPublicKeyResponse_sSHPublicKey,
    getSSHPublicKeyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSSHPublicKey' smart constructor.
data GetSSHPublicKey = GetSSHPublicKey'
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
    -- | Specifies the public key encoding format to use in the response. To
    -- retrieve the public key in ssh-rsa format, use @SSH@. To retrieve the
    -- public key in PEM format, use @PEM@.
    encoding :: EncodingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSSHPublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'getSSHPublicKey_userName' - The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'sSHPublicKeyId', 'getSSHPublicKey_sSHPublicKeyId' - The unique identifier for the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
--
-- 'encoding', 'getSSHPublicKey_encoding' - Specifies the public key encoding format to use in the response. To
-- retrieve the public key in ssh-rsa format, use @SSH@. To retrieve the
-- public key in PEM format, use @PEM@.
newGetSSHPublicKey ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'sSHPublicKeyId'
  Prelude.Text ->
  -- | 'encoding'
  EncodingType ->
  GetSSHPublicKey
newGetSSHPublicKey
  pUserName_
  pSSHPublicKeyId_
  pEncoding_ =
    GetSSHPublicKey'
      { userName = pUserName_,
        sSHPublicKeyId = pSSHPublicKeyId_,
        encoding = pEncoding_
      }

-- | The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getSSHPublicKey_userName :: Lens.Lens' GetSSHPublicKey Prelude.Text
getSSHPublicKey_userName = Lens.lens (\GetSSHPublicKey' {userName} -> userName) (\s@GetSSHPublicKey' {} a -> s {userName = a} :: GetSSHPublicKey)

-- | The unique identifier for the SSH public key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
getSSHPublicKey_sSHPublicKeyId :: Lens.Lens' GetSSHPublicKey Prelude.Text
getSSHPublicKey_sSHPublicKeyId = Lens.lens (\GetSSHPublicKey' {sSHPublicKeyId} -> sSHPublicKeyId) (\s@GetSSHPublicKey' {} a -> s {sSHPublicKeyId = a} :: GetSSHPublicKey)

-- | Specifies the public key encoding format to use in the response. To
-- retrieve the public key in ssh-rsa format, use @SSH@. To retrieve the
-- public key in PEM format, use @PEM@.
getSSHPublicKey_encoding :: Lens.Lens' GetSSHPublicKey EncodingType
getSSHPublicKey_encoding = Lens.lens (\GetSSHPublicKey' {encoding} -> encoding) (\s@GetSSHPublicKey' {} a -> s {encoding = a} :: GetSSHPublicKey)

instance Core.AWSRequest GetSSHPublicKey where
  type
    AWSResponse GetSSHPublicKey =
      GetSSHPublicKeyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetSSHPublicKeyResult"
      ( \s h x ->
          GetSSHPublicKeyResponse'
            Prelude.<$> (x Core..@? "SSHPublicKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSSHPublicKey

instance Prelude.NFData GetSSHPublicKey

instance Core.ToHeaders GetSSHPublicKey where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetSSHPublicKey where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSSHPublicKey where
  toQuery GetSSHPublicKey' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetSSHPublicKey" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Core.=: userName,
        "SSHPublicKeyId" Core.=: sSHPublicKeyId,
        "Encoding" Core.=: encoding
      ]

-- | Contains the response to a successful GetSSHPublicKey request.
--
-- /See:/ 'newGetSSHPublicKeyResponse' smart constructor.
data GetSSHPublicKeyResponse = GetSSHPublicKeyResponse'
  { -- | A structure containing details about the SSH public key.
    sSHPublicKey :: Prelude.Maybe SSHPublicKey,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSSHPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sSHPublicKey', 'getSSHPublicKeyResponse_sSHPublicKey' - A structure containing details about the SSH public key.
--
-- 'httpStatus', 'getSSHPublicKeyResponse_httpStatus' - The response's http status code.
newGetSSHPublicKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSSHPublicKeyResponse
newGetSSHPublicKeyResponse pHttpStatus_ =
  GetSSHPublicKeyResponse'
    { sSHPublicKey =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure containing details about the SSH public key.
getSSHPublicKeyResponse_sSHPublicKey :: Lens.Lens' GetSSHPublicKeyResponse (Prelude.Maybe SSHPublicKey)
getSSHPublicKeyResponse_sSHPublicKey = Lens.lens (\GetSSHPublicKeyResponse' {sSHPublicKey} -> sSHPublicKey) (\s@GetSSHPublicKeyResponse' {} a -> s {sSHPublicKey = a} :: GetSSHPublicKeyResponse)

-- | The response's http status code.
getSSHPublicKeyResponse_httpStatus :: Lens.Lens' GetSSHPublicKeyResponse Prelude.Int
getSSHPublicKeyResponse_httpStatus = Lens.lens (\GetSSHPublicKeyResponse' {httpStatus} -> httpStatus) (\s@GetSSHPublicKeyResponse' {} a -> s {httpStatus = a} :: GetSSHPublicKeyResponse)

instance Prelude.NFData GetSSHPublicKeyResponse
