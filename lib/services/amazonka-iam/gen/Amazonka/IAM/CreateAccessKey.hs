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
-- Module      : Amazonka.IAM.CreateAccessKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Web Services secret access key and corresponding
-- Amazon Web Services access key ID for the specified user. The default
-- status for new keys is @Active@.
--
-- If you do not specify a user name, IAM determines the user name
-- implicitly based on the Amazon Web Services access key ID signing the
-- request. This operation works for access keys under the Amazon Web
-- Services account. Consequently, you can use this operation to manage
-- Amazon Web Services account root user credentials. This is true even if
-- the Amazon Web Services account has no associated users.
--
-- For information about quotas on the number of keys you can create, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>
-- in the /IAM User Guide/.
--
-- To ensure the security of your Amazon Web Services account, the secret
-- access key is accessible only during key and user creation. You must
-- save the key (for example, in a text file) if you want to be able to
-- access it again. If a secret key is lost, you can delete the access keys
-- for the associated user and then create new keys.
module Amazonka.IAM.CreateAccessKey
  ( -- * Creating a Request
    CreateAccessKey (..),
    newCreateAccessKey,

    -- * Request Lenses
    createAccessKey_userName,

    -- * Destructuring the Response
    CreateAccessKeyResponse (..),
    newCreateAccessKeyResponse,

    -- * Response Lenses
    createAccessKeyResponse_httpStatus,
    createAccessKeyResponse_accessKey,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccessKey' smart constructor.
data CreateAccessKey = CreateAccessKey'
  { -- | The name of the IAM user that the new key will belong to.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'createAccessKey_userName' - The name of the IAM user that the new key will belong to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newCreateAccessKey ::
  CreateAccessKey
newCreateAccessKey =
  CreateAccessKey' {userName = Prelude.Nothing}

-- | The name of the IAM user that the new key will belong to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
createAccessKey_userName :: Lens.Lens' CreateAccessKey (Prelude.Maybe Prelude.Text)
createAccessKey_userName = Lens.lens (\CreateAccessKey' {userName} -> userName) (\s@CreateAccessKey' {} a -> s {userName = a} :: CreateAccessKey)

instance Core.AWSRequest CreateAccessKey where
  type
    AWSResponse CreateAccessKey =
      CreateAccessKeyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateAccessKeyResult"
      ( \s h x ->
          CreateAccessKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "AccessKey")
      )

instance Prelude.Hashable CreateAccessKey where
  hashWithSalt _salt CreateAccessKey' {..} =
    _salt `Prelude.hashWithSalt` userName

instance Prelude.NFData CreateAccessKey where
  rnf CreateAccessKey' {..} = Prelude.rnf userName

instance Core.ToHeaders CreateAccessKey where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateAccessKey where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateAccessKey where
  toQuery CreateAccessKey' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateAccessKey" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Core.=: userName
      ]

-- | Contains the response to a successful CreateAccessKey request.
--
-- /See:/ 'newCreateAccessKeyResponse' smart constructor.
data CreateAccessKeyResponse = CreateAccessKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure with details about the access key.
    accessKey :: AccessKeyInfo
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAccessKeyResponse_httpStatus' - The response's http status code.
--
-- 'accessKey', 'createAccessKeyResponse_accessKey' - A structure with details about the access key.
newCreateAccessKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accessKey'
  AccessKeyInfo ->
  CreateAccessKeyResponse
newCreateAccessKeyResponse pHttpStatus_ pAccessKey_ =
  CreateAccessKeyResponse'
    { httpStatus = pHttpStatus_,
      accessKey = pAccessKey_
    }

-- | The response's http status code.
createAccessKeyResponse_httpStatus :: Lens.Lens' CreateAccessKeyResponse Prelude.Int
createAccessKeyResponse_httpStatus = Lens.lens (\CreateAccessKeyResponse' {httpStatus} -> httpStatus) (\s@CreateAccessKeyResponse' {} a -> s {httpStatus = a} :: CreateAccessKeyResponse)

-- | A structure with details about the access key.
createAccessKeyResponse_accessKey :: Lens.Lens' CreateAccessKeyResponse AccessKeyInfo
createAccessKeyResponse_accessKey = Lens.lens (\CreateAccessKeyResponse' {accessKey} -> accessKey) (\s@CreateAccessKeyResponse' {} a -> s {accessKey = a} :: CreateAccessKeyResponse)

instance Prelude.NFData CreateAccessKeyResponse where
  rnf CreateAccessKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accessKey
