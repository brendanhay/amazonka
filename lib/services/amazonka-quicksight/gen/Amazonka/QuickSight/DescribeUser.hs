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
-- Module      : Amazonka.QuickSight.DescribeUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a user, given the user name.
module Amazonka.QuickSight.DescribeUser
  ( -- * Creating a Request
    DescribeUser (..),
    newDescribeUser,

    -- * Request Lenses
    describeUser_userName,
    describeUser_awsAccountId,
    describeUser_namespace,

    -- * Destructuring the Response
    DescribeUserResponse (..),
    newDescribeUserResponse,

    -- * Response Lenses
    describeUserResponse_requestId,
    describeUserResponse_user,
    describeUserResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | The name of the user that you want to describe.
    userName :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the user is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace. Currently, you should set this to @default@.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'describeUser_userName' - The name of the user that you want to describe.
--
-- 'awsAccountId', 'describeUser_awsAccountId' - The ID for the Amazon Web Services account that the user is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'describeUser_namespace' - The namespace. Currently, you should set this to @default@.
newDescribeUser ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  DescribeUser
newDescribeUser pUserName_ pAwsAccountId_ pNamespace_ =
  DescribeUser'
    { userName = pUserName_,
      awsAccountId = pAwsAccountId_,
      namespace = pNamespace_
    }

-- | The name of the user that you want to describe.
describeUser_userName :: Lens.Lens' DescribeUser Prelude.Text
describeUser_userName = Lens.lens (\DescribeUser' {userName} -> userName) (\s@DescribeUser' {} a -> s {userName = a} :: DescribeUser)

-- | The ID for the Amazon Web Services account that the user is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
describeUser_awsAccountId :: Lens.Lens' DescribeUser Prelude.Text
describeUser_awsAccountId = Lens.lens (\DescribeUser' {awsAccountId} -> awsAccountId) (\s@DescribeUser' {} a -> s {awsAccountId = a} :: DescribeUser)

-- | The namespace. Currently, you should set this to @default@.
describeUser_namespace :: Lens.Lens' DescribeUser Prelude.Text
describeUser_namespace = Lens.lens (\DescribeUser' {namespace} -> namespace) (\s@DescribeUser' {} a -> s {namespace = a} :: DescribeUser)

instance Core.AWSRequest DescribeUser where
  type AWSResponse DescribeUser = DescribeUserResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUser where
  hashWithSalt _salt DescribeUser' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DescribeUser where
  rnf DescribeUser' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders DescribeUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeUser where
  toPath DescribeUser' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/users/",
        Data.toBS userName
      ]

instance Data.ToQuery DescribeUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The user name.
    user :: Prelude.Maybe User,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeUserResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'user', 'describeUserResponse_user' - The user name.
--
-- 'status', 'describeUserResponse_status' - The HTTP status of the request.
newDescribeUserResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeUserResponse
newDescribeUserResponse pStatus_ =
  DescribeUserResponse'
    { requestId = Prelude.Nothing,
      user = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
describeUserResponse_requestId :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_requestId = Lens.lens (\DescribeUserResponse' {requestId} -> requestId) (\s@DescribeUserResponse' {} a -> s {requestId = a} :: DescribeUserResponse)

-- | The user name.
describeUserResponse_user :: Lens.Lens' DescribeUserResponse (Prelude.Maybe User)
describeUserResponse_user = Lens.lens (\DescribeUserResponse' {user} -> user) (\s@DescribeUserResponse' {} a -> s {user = a} :: DescribeUserResponse)

-- | The HTTP status of the request.
describeUserResponse_status :: Lens.Lens' DescribeUserResponse Prelude.Int
describeUserResponse_status = Lens.lens (\DescribeUserResponse' {status} -> status) (\s@DescribeUserResponse' {} a -> s {status = a} :: DescribeUserResponse)

instance Prelude.NFData DescribeUserResponse where
  rnf DescribeUserResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf user
      `Prelude.seq` Prelude.rnf status
