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
-- Module      : Amazonka.FinSpace.GetKxUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified kdb user.
module Amazonka.FinSpace.GetKxUser
  ( -- * Creating a Request
    GetKxUser (..),
    newGetKxUser,

    -- * Request Lenses
    getKxUser_userName,
    getKxUser_environmentId,

    -- * Destructuring the Response
    GetKxUserResponse (..),
    newGetKxUserResponse,

    -- * Response Lenses
    getKxUserResponse_environmentId,
    getKxUserResponse_iamRole,
    getKxUserResponse_userArn,
    getKxUserResponse_userName,
    getKxUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKxUser' smart constructor.
data GetKxUser = GetKxUser'
  { -- | A unique identifier for the user.
    userName :: Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'getKxUser_userName' - A unique identifier for the user.
--
-- 'environmentId', 'getKxUser_environmentId' - A unique identifier for the kdb environment.
newGetKxUser ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  GetKxUser
newGetKxUser pUserName_ pEnvironmentId_ =
  GetKxUser'
    { userName = pUserName_,
      environmentId = pEnvironmentId_
    }

-- | A unique identifier for the user.
getKxUser_userName :: Lens.Lens' GetKxUser Prelude.Text
getKxUser_userName = Lens.lens (\GetKxUser' {userName} -> userName) (\s@GetKxUser' {} a -> s {userName = a} :: GetKxUser)

-- | A unique identifier for the kdb environment.
getKxUser_environmentId :: Lens.Lens' GetKxUser Prelude.Text
getKxUser_environmentId = Lens.lens (\GetKxUser' {environmentId} -> environmentId) (\s@GetKxUser' {} a -> s {environmentId = a} :: GetKxUser)

instance Core.AWSRequest GetKxUser where
  type AWSResponse GetKxUser = GetKxUserResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKxUserResponse'
            Prelude.<$> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "iamRole")
            Prelude.<*> (x Data..?> "userArn")
            Prelude.<*> (x Data..?> "userName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKxUser where
  hashWithSalt _salt GetKxUser' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData GetKxUser where
  rnf GetKxUser' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf environmentId

instance Data.ToHeaders GetKxUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetKxUser where
  toPath GetKxUser' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/users/",
        Data.toBS userName
      ]

instance Data.ToQuery GetKxUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKxUserResponse' smart constructor.
data GetKxUserResponse = GetKxUserResponse'
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
-- Create a value of 'GetKxUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'getKxUserResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'iamRole', 'getKxUserResponse_iamRole' - The IAM role ARN that is associated with the user.
--
-- 'userArn', 'getKxUserResponse_userArn' - The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
-- /IAM User Guide/.
--
-- 'userName', 'getKxUserResponse_userName' - A unique identifier for the user.
--
-- 'httpStatus', 'getKxUserResponse_httpStatus' - The response's http status code.
newGetKxUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKxUserResponse
newGetKxUserResponse pHttpStatus_ =
  GetKxUserResponse'
    { environmentId = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      userArn = Prelude.Nothing,
      userName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the kdb environment.
getKxUserResponse_environmentId :: Lens.Lens' GetKxUserResponse (Prelude.Maybe Prelude.Text)
getKxUserResponse_environmentId = Lens.lens (\GetKxUserResponse' {environmentId} -> environmentId) (\s@GetKxUserResponse' {} a -> s {environmentId = a} :: GetKxUserResponse)

-- | The IAM role ARN that is associated with the user.
getKxUserResponse_iamRole :: Lens.Lens' GetKxUserResponse (Prelude.Maybe Prelude.Text)
getKxUserResponse_iamRole = Lens.lens (\GetKxUserResponse' {iamRole} -> iamRole) (\s@GetKxUserResponse' {} a -> s {iamRole = a} :: GetKxUserResponse)

-- | The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
-- /IAM User Guide/.
getKxUserResponse_userArn :: Lens.Lens' GetKxUserResponse (Prelude.Maybe Prelude.Text)
getKxUserResponse_userArn = Lens.lens (\GetKxUserResponse' {userArn} -> userArn) (\s@GetKxUserResponse' {} a -> s {userArn = a} :: GetKxUserResponse)

-- | A unique identifier for the user.
getKxUserResponse_userName :: Lens.Lens' GetKxUserResponse (Prelude.Maybe Prelude.Text)
getKxUserResponse_userName = Lens.lens (\GetKxUserResponse' {userName} -> userName) (\s@GetKxUserResponse' {} a -> s {userName = a} :: GetKxUserResponse)

-- | The response's http status code.
getKxUserResponse_httpStatus :: Lens.Lens' GetKxUserResponse Prelude.Int
getKxUserResponse_httpStatus = Lens.lens (\GetKxUserResponse' {httpStatus} -> httpStatus) (\s@GetKxUserResponse' {} a -> s {httpStatus = a} :: GetKxUserResponse)

instance Prelude.NFData GetKxUserResponse where
  rnf GetKxUserResponse' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf httpStatus
