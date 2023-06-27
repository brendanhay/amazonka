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
-- Module      : Amazonka.FinSpace.UpdateKxUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the user details. You can only update the IAM role associated
-- with a user.
module Amazonka.FinSpace.UpdateKxUser
  ( -- * Creating a Request
    UpdateKxUser (..),
    newUpdateKxUser,

    -- * Request Lenses
    updateKxUser_clientToken,
    updateKxUser_environmentId,
    updateKxUser_userName,
    updateKxUser_iamRole,

    -- * Destructuring the Response
    UpdateKxUserResponse (..),
    newUpdateKxUserResponse,

    -- * Response Lenses
    updateKxUserResponse_environmentId,
    updateKxUserResponse_iamRole,
    updateKxUserResponse_userArn,
    updateKxUserResponse_userName,
    updateKxUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateKxUser' smart constructor.
data UpdateKxUser = UpdateKxUser'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | A unique identifier for the user.
    userName :: Prelude.Text,
    -- | The IAM role ARN that is associated with the user.
    iamRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKxUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateKxUser_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'environmentId', 'updateKxUser_environmentId' - A unique identifier for the kdb environment.
--
-- 'userName', 'updateKxUser_userName' - A unique identifier for the user.
--
-- 'iamRole', 'updateKxUser_iamRole' - The IAM role ARN that is associated with the user.
newUpdateKxUser ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  -- | 'iamRole'
  Prelude.Text ->
  UpdateKxUser
newUpdateKxUser pEnvironmentId_ pUserName_ pIamRole_ =
  UpdateKxUser'
    { clientToken = Prelude.Nothing,
      environmentId = pEnvironmentId_,
      userName = pUserName_,
      iamRole = pIamRole_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
updateKxUser_clientToken :: Lens.Lens' UpdateKxUser (Prelude.Maybe Prelude.Text)
updateKxUser_clientToken = Lens.lens (\UpdateKxUser' {clientToken} -> clientToken) (\s@UpdateKxUser' {} a -> s {clientToken = a} :: UpdateKxUser)

-- | A unique identifier for the kdb environment.
updateKxUser_environmentId :: Lens.Lens' UpdateKxUser Prelude.Text
updateKxUser_environmentId = Lens.lens (\UpdateKxUser' {environmentId} -> environmentId) (\s@UpdateKxUser' {} a -> s {environmentId = a} :: UpdateKxUser)

-- | A unique identifier for the user.
updateKxUser_userName :: Lens.Lens' UpdateKxUser Prelude.Text
updateKxUser_userName = Lens.lens (\UpdateKxUser' {userName} -> userName) (\s@UpdateKxUser' {} a -> s {userName = a} :: UpdateKxUser)

-- | The IAM role ARN that is associated with the user.
updateKxUser_iamRole :: Lens.Lens' UpdateKxUser Prelude.Text
updateKxUser_iamRole = Lens.lens (\UpdateKxUser' {iamRole} -> iamRole) (\s@UpdateKxUser' {} a -> s {iamRole = a} :: UpdateKxUser)

instance Core.AWSRequest UpdateKxUser where
  type AWSResponse UpdateKxUser = UpdateKxUserResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateKxUserResponse'
            Prelude.<$> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "iamRole")
            Prelude.<*> (x Data..?> "userArn")
            Prelude.<*> (x Data..?> "userName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateKxUser where
  hashWithSalt _salt UpdateKxUser' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` iamRole

instance Prelude.NFData UpdateKxUser where
  rnf UpdateKxUser' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf iamRole

instance Data.ToHeaders UpdateKxUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateKxUser where
  toJSON UpdateKxUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("iamRole" Data..= iamRole)
          ]
      )

instance Data.ToPath UpdateKxUser where
  toPath UpdateKxUser' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/users/",
        Data.toBS userName
      ]

instance Data.ToQuery UpdateKxUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateKxUserResponse' smart constructor.
data UpdateKxUserResponse = UpdateKxUserResponse'
  { -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The IAM role ARN that is associated with the user.
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
-- Create a value of 'UpdateKxUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'updateKxUserResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'iamRole', 'updateKxUserResponse_iamRole' - The IAM role ARN that is associated with the user.
--
-- 'userArn', 'updateKxUserResponse_userArn' - The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
-- /IAM User Guide/.
--
-- 'userName', 'updateKxUserResponse_userName' - A unique identifier for the user.
--
-- 'httpStatus', 'updateKxUserResponse_httpStatus' - The response's http status code.
newUpdateKxUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateKxUserResponse
newUpdateKxUserResponse pHttpStatus_ =
  UpdateKxUserResponse'
    { environmentId =
        Prelude.Nothing,
      iamRole = Prelude.Nothing,
      userArn = Prelude.Nothing,
      userName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the kdb environment.
updateKxUserResponse_environmentId :: Lens.Lens' UpdateKxUserResponse (Prelude.Maybe Prelude.Text)
updateKxUserResponse_environmentId = Lens.lens (\UpdateKxUserResponse' {environmentId} -> environmentId) (\s@UpdateKxUserResponse' {} a -> s {environmentId = a} :: UpdateKxUserResponse)

-- | The IAM role ARN that is associated with the user.
updateKxUserResponse_iamRole :: Lens.Lens' UpdateKxUserResponse (Prelude.Maybe Prelude.Text)
updateKxUserResponse_iamRole = Lens.lens (\UpdateKxUserResponse' {iamRole} -> iamRole) (\s@UpdateKxUserResponse' {} a -> s {iamRole = a} :: UpdateKxUserResponse)

-- | The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
-- /IAM User Guide/.
updateKxUserResponse_userArn :: Lens.Lens' UpdateKxUserResponse (Prelude.Maybe Prelude.Text)
updateKxUserResponse_userArn = Lens.lens (\UpdateKxUserResponse' {userArn} -> userArn) (\s@UpdateKxUserResponse' {} a -> s {userArn = a} :: UpdateKxUserResponse)

-- | A unique identifier for the user.
updateKxUserResponse_userName :: Lens.Lens' UpdateKxUserResponse (Prelude.Maybe Prelude.Text)
updateKxUserResponse_userName = Lens.lens (\UpdateKxUserResponse' {userName} -> userName) (\s@UpdateKxUserResponse' {} a -> s {userName = a} :: UpdateKxUserResponse)

-- | The response's http status code.
updateKxUserResponse_httpStatus :: Lens.Lens' UpdateKxUserResponse Prelude.Int
updateKxUserResponse_httpStatus = Lens.lens (\UpdateKxUserResponse' {httpStatus} -> httpStatus) (\s@UpdateKxUserResponse' {} a -> s {httpStatus = a} :: UpdateKxUserResponse)

instance Prelude.NFData UpdateKxUserResponse where
  rnf UpdateKxUserResponse' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf httpStatus
