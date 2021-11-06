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
-- Module      : Amazonka.IdentityStore.DescribeUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the user metadata and attributes from @UserId@ in an identity
-- store.
module Amazonka.IdentityStore.DescribeUser
  ( -- * Creating a Request
    DescribeUser (..),
    newDescribeUser,

    -- * Request Lenses
    describeUser_identityStoreId,
    describeUser_userId,

    -- * Destructuring the Response
    DescribeUserResponse (..),
    newDescribeUserResponse,

    -- * Response Lenses
    describeUserResponse_httpStatus,
    describeUserResponse_userName,
    describeUserResponse_userId,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IdentityStore.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | The globally unique identifier for the identity store, such as
    -- @d-1234567890@. In this example, @d-@ is a fixed prefix, and
    -- @1234567890@ is a randomly generated string that contains number and
    -- lower case letters. This value is generated at the time that a new
    -- identity store is created.
    identityStoreId :: Prelude.Text,
    -- | The identifier for a user in the identity store.
    userId :: Prelude.Text
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
-- 'identityStoreId', 'describeUser_identityStoreId' - The globally unique identifier for the identity store, such as
-- @d-1234567890@. In this example, @d-@ is a fixed prefix, and
-- @1234567890@ is a randomly generated string that contains number and
-- lower case letters. This value is generated at the time that a new
-- identity store is created.
--
-- 'userId', 'describeUser_userId' - The identifier for a user in the identity store.
newDescribeUser ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DescribeUser
newDescribeUser pIdentityStoreId_ pUserId_ =
  DescribeUser'
    { identityStoreId = pIdentityStoreId_,
      userId = pUserId_
    }

-- | The globally unique identifier for the identity store, such as
-- @d-1234567890@. In this example, @d-@ is a fixed prefix, and
-- @1234567890@ is a randomly generated string that contains number and
-- lower case letters. This value is generated at the time that a new
-- identity store is created.
describeUser_identityStoreId :: Lens.Lens' DescribeUser Prelude.Text
describeUser_identityStoreId = Lens.lens (\DescribeUser' {identityStoreId} -> identityStoreId) (\s@DescribeUser' {} a -> s {identityStoreId = a} :: DescribeUser)

-- | The identifier for a user in the identity store.
describeUser_userId :: Lens.Lens' DescribeUser Prelude.Text
describeUser_userId = Lens.lens (\DescribeUser' {userId} -> userId) (\s@DescribeUser' {} a -> s {userId = a} :: DescribeUser)

instance Core.AWSRequest DescribeUser where
  type AWSResponse DescribeUser = DescribeUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "UserName")
            Prelude.<*> (x Core..:> "UserId")
      )

instance Prelude.Hashable DescribeUser

instance Prelude.NFData DescribeUser

instance Core.ToHeaders DescribeUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIdentityStore.DescribeUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeUser where
  toJSON DescribeUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Core..= identityStoreId),
            Prelude.Just ("UserId" Core..= userId)
          ]
      )

instance Core.ToPath DescribeUser where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains the user’s user name value. The length limit is 128 characters.
    -- This value can consist of letters, accented characters, symbols,
    -- numbers, and punctuation. The characters @\<>;:%@ are excluded. This
    -- value is specified at the time the user is created and stored as an
    -- attribute of the user object in the identity store.
    userName :: Core.Sensitive Prelude.Text,
    -- | The identifier for a user in the identity store.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeUserResponse_httpStatus' - The response's http status code.
--
-- 'userName', 'describeUserResponse_userName' - Contains the user’s user name value. The length limit is 128 characters.
-- This value can consist of letters, accented characters, symbols,
-- numbers, and punctuation. The characters @\<>;:%@ are excluded. This
-- value is specified at the time the user is created and stored as an
-- attribute of the user object in the identity store.
--
-- 'userId', 'describeUserResponse_userId' - The identifier for a user in the identity store.
newDescribeUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'userName'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DescribeUserResponse
newDescribeUserResponse
  pHttpStatus_
  pUserName_
  pUserId_ =
    DescribeUserResponse'
      { httpStatus = pHttpStatus_,
        userName = Core._Sensitive Lens.# pUserName_,
        userId = pUserId_
      }

-- | The response's http status code.
describeUserResponse_httpStatus :: Lens.Lens' DescribeUserResponse Prelude.Int
describeUserResponse_httpStatus = Lens.lens (\DescribeUserResponse' {httpStatus} -> httpStatus) (\s@DescribeUserResponse' {} a -> s {httpStatus = a} :: DescribeUserResponse)

-- | Contains the user’s user name value. The length limit is 128 characters.
-- This value can consist of letters, accented characters, symbols,
-- numbers, and punctuation. The characters @\<>;:%@ are excluded. This
-- value is specified at the time the user is created and stored as an
-- attribute of the user object in the identity store.
describeUserResponse_userName :: Lens.Lens' DescribeUserResponse Prelude.Text
describeUserResponse_userName = Lens.lens (\DescribeUserResponse' {userName} -> userName) (\s@DescribeUserResponse' {} a -> s {userName = a} :: DescribeUserResponse) Prelude.. Core._Sensitive

-- | The identifier for a user in the identity store.
describeUserResponse_userId :: Lens.Lens' DescribeUserResponse Prelude.Text
describeUserResponse_userId = Lens.lens (\DescribeUserResponse' {userId} -> userId) (\s@DescribeUserResponse' {} a -> s {userId = a} :: DescribeUserResponse)

instance Prelude.NFData DescribeUserResponse
