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
-- Module      : Amazonka.FinSpace.CreateKxUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user in FinSpace kdb environment with an associated IAM role.
module Amazonka.FinSpace.CreateKxUser
  ( -- * Creating a Request
    CreateKxUser (..),
    newCreateKxUser,

    -- * Request Lenses
    createKxUser_clientToken,
    createKxUser_tags,
    createKxUser_environmentId,
    createKxUser_userName,
    createKxUser_iamRole,

    -- * Destructuring the Response
    CreateKxUserResponse (..),
    newCreateKxUserResponse,

    -- * Response Lenses
    createKxUserResponse_environmentId,
    createKxUserResponse_iamRole,
    createKxUserResponse_userArn,
    createKxUserResponse_userName,
    createKxUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKxUser' smart constructor.
data CreateKxUser = CreateKxUser'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs to label the user. You can add up to 50 tags
    -- to a user.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique identifier for the kdb environment where you want to create a
    -- user.
    environmentId :: Prelude.Text,
    -- | A unique identifier for the user.
    userName :: Prelude.Text,
    -- | The IAM role ARN that will be associated with the user.
    iamRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKxUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createKxUser_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'tags', 'createKxUser_tags' - A list of key-value pairs to label the user. You can add up to 50 tags
-- to a user.
--
-- 'environmentId', 'createKxUser_environmentId' - A unique identifier for the kdb environment where you want to create a
-- user.
--
-- 'userName', 'createKxUser_userName' - A unique identifier for the user.
--
-- 'iamRole', 'createKxUser_iamRole' - The IAM role ARN that will be associated with the user.
newCreateKxUser ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  -- | 'iamRole'
  Prelude.Text ->
  CreateKxUser
newCreateKxUser pEnvironmentId_ pUserName_ pIamRole_ =
  CreateKxUser'
    { clientToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      environmentId = pEnvironmentId_,
      userName = pUserName_,
      iamRole = pIamRole_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
createKxUser_clientToken :: Lens.Lens' CreateKxUser (Prelude.Maybe Prelude.Text)
createKxUser_clientToken = Lens.lens (\CreateKxUser' {clientToken} -> clientToken) (\s@CreateKxUser' {} a -> s {clientToken = a} :: CreateKxUser)

-- | A list of key-value pairs to label the user. You can add up to 50 tags
-- to a user.
createKxUser_tags :: Lens.Lens' CreateKxUser (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createKxUser_tags = Lens.lens (\CreateKxUser' {tags} -> tags) (\s@CreateKxUser' {} a -> s {tags = a} :: CreateKxUser) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the kdb environment where you want to create a
-- user.
createKxUser_environmentId :: Lens.Lens' CreateKxUser Prelude.Text
createKxUser_environmentId = Lens.lens (\CreateKxUser' {environmentId} -> environmentId) (\s@CreateKxUser' {} a -> s {environmentId = a} :: CreateKxUser)

-- | A unique identifier for the user.
createKxUser_userName :: Lens.Lens' CreateKxUser Prelude.Text
createKxUser_userName = Lens.lens (\CreateKxUser' {userName} -> userName) (\s@CreateKxUser' {} a -> s {userName = a} :: CreateKxUser)

-- | The IAM role ARN that will be associated with the user.
createKxUser_iamRole :: Lens.Lens' CreateKxUser Prelude.Text
createKxUser_iamRole = Lens.lens (\CreateKxUser' {iamRole} -> iamRole) (\s@CreateKxUser' {} a -> s {iamRole = a} :: CreateKxUser)

instance Core.AWSRequest CreateKxUser where
  type AWSResponse CreateKxUser = CreateKxUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKxUserResponse'
            Prelude.<$> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "iamRole")
            Prelude.<*> (x Data..?> "userArn")
            Prelude.<*> (x Data..?> "userName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateKxUser where
  hashWithSalt _salt CreateKxUser' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` iamRole

instance Prelude.NFData CreateKxUser where
  rnf CreateKxUser' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf iamRole

instance Data.ToHeaders CreateKxUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateKxUser where
  toJSON CreateKxUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("userName" Data..= userName),
            Prelude.Just ("iamRole" Data..= iamRole)
          ]
      )

instance Data.ToPath CreateKxUser where
  toPath CreateKxUser' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/users"
      ]

instance Data.ToQuery CreateKxUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKxUserResponse' smart constructor.
data CreateKxUserResponse = CreateKxUserResponse'
  { -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The IAM role ARN that will be associated with the user.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the user. For more
    -- information about ARNs and how to use ARNs in policies, see
    -- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
    -- /IAM User Guide/.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the user.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKxUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'createKxUserResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'iamRole', 'createKxUserResponse_iamRole' - The IAM role ARN that will be associated with the user.
--
-- 'userArn', 'createKxUserResponse_userArn' - The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
-- /IAM User Guide/.
--
-- 'userName', 'createKxUserResponse_userName' - A unique identifier for the user.
--
-- 'httpStatus', 'createKxUserResponse_httpStatus' - The response's http status code.
newCreateKxUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateKxUserResponse
newCreateKxUserResponse pHttpStatus_ =
  CreateKxUserResponse'
    { environmentId =
        Prelude.Nothing,
      iamRole = Prelude.Nothing,
      userArn = Prelude.Nothing,
      userName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the kdb environment.
createKxUserResponse_environmentId :: Lens.Lens' CreateKxUserResponse (Prelude.Maybe Prelude.Text)
createKxUserResponse_environmentId = Lens.lens (\CreateKxUserResponse' {environmentId} -> environmentId) (\s@CreateKxUserResponse' {} a -> s {environmentId = a} :: CreateKxUserResponse)

-- | The IAM role ARN that will be associated with the user.
createKxUserResponse_iamRole :: Lens.Lens' CreateKxUserResponse (Prelude.Maybe Prelude.Text)
createKxUserResponse_iamRole = Lens.lens (\CreateKxUserResponse' {iamRole} -> iamRole) (\s@CreateKxUserResponse' {} a -> s {iamRole = a} :: CreateKxUserResponse)

-- | The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
-- /IAM User Guide/.
createKxUserResponse_userArn :: Lens.Lens' CreateKxUserResponse (Prelude.Maybe Prelude.Text)
createKxUserResponse_userArn = Lens.lens (\CreateKxUserResponse' {userArn} -> userArn) (\s@CreateKxUserResponse' {} a -> s {userArn = a} :: CreateKxUserResponse)

-- | A unique identifier for the user.
createKxUserResponse_userName :: Lens.Lens' CreateKxUserResponse (Prelude.Maybe Prelude.Text)
createKxUserResponse_userName = Lens.lens (\CreateKxUserResponse' {userName} -> userName) (\s@CreateKxUserResponse' {} a -> s {userName = a} :: CreateKxUserResponse)

-- | The response's http status code.
createKxUserResponse_httpStatus :: Lens.Lens' CreateKxUserResponse Prelude.Int
createKxUserResponse_httpStatus = Lens.lens (\CreateKxUserResponse' {httpStatus} -> httpStatus) (\s@CreateKxUserResponse' {} a -> s {httpStatus = a} :: CreateKxUserResponse)

instance Prelude.NFData CreateKxUserResponse where
  rnf CreateKxUserResponse' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf httpStatus
