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
-- Module      : Amazonka.Rekognition.CreateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new User within a collection specified by @CollectionId@.
-- Takes @UserId@ as a parameter, which is a user provided ID which should
-- be unique within the collection. The provided @UserId@ will alias the
-- system generated UUID to make the @UserId@ more user friendly.
--
-- Uses a @ClientToken@, an idempotency token that ensures a call to
-- @CreateUser@ completes only once. If the value is not supplied, the AWS
-- SDK generates an idempotency token for the requests. This prevents
-- retries after a network error results from making multiple @CreateUser@
-- calls.
module Amazonka.Rekognition.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_clientRequestToken,
    createUser_collectionId,
    createUser_userId,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | Idempotent token used to identify the request to @CreateUser@. If you
    -- use the same token with multiple @CreateUser@ requests, the same
    -- response is returned. Use ClientRequestToken to prevent the same request
    -- from being processed more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of an existing collection to which the new UserID needs to be
    -- created.
    collectionId :: Prelude.Text,
    -- | ID for the UserID to be created. This ID needs to be unique within the
    -- collection.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createUser_clientRequestToken' - Idempotent token used to identify the request to @CreateUser@. If you
-- use the same token with multiple @CreateUser@ requests, the same
-- response is returned. Use ClientRequestToken to prevent the same request
-- from being processed more than once.
--
-- 'collectionId', 'createUser_collectionId' - The ID of an existing collection to which the new UserID needs to be
-- created.
--
-- 'userId', 'createUser_userId' - ID for the UserID to be created. This ID needs to be unique within the
-- collection.
newCreateUser ::
  -- | 'collectionId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  CreateUser
newCreateUser pCollectionId_ pUserId_ =
  CreateUser'
    { clientRequestToken = Prelude.Nothing,
      collectionId = pCollectionId_,
      userId = pUserId_
    }

-- | Idempotent token used to identify the request to @CreateUser@. If you
-- use the same token with multiple @CreateUser@ requests, the same
-- response is returned. Use ClientRequestToken to prevent the same request
-- from being processed more than once.
createUser_clientRequestToken :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_clientRequestToken = Lens.lens (\CreateUser' {clientRequestToken} -> clientRequestToken) (\s@CreateUser' {} a -> s {clientRequestToken = a} :: CreateUser)

-- | The ID of an existing collection to which the new UserID needs to be
-- created.
createUser_collectionId :: Lens.Lens' CreateUser Prelude.Text
createUser_collectionId = Lens.lens (\CreateUser' {collectionId} -> collectionId) (\s@CreateUser' {} a -> s {collectionId = a} :: CreateUser)

-- | ID for the UserID to be created. This ID needs to be unique within the
-- collection.
createUser_userId :: Lens.Lens' CreateUser Prelude.Text
createUser_userId = Lens.lens (\CreateUser' {userId} -> userId) (\s@CreateUser' {} a -> s {userId = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser where
  hashWithSalt _salt CreateUser' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` collectionId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData CreateUser where
  rnf CreateUser' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf collectionId
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders CreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.CreateUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("CollectionId" Data..= collectionId),
            Prelude.Just ("UserId" Data..= userId)
          ]
      )

instance Data.ToPath CreateUser where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse where
  rnf CreateUserResponse' {..} = Prelude.rnf httpStatus
