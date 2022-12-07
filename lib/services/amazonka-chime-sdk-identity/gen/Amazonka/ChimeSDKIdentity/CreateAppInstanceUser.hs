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
-- Module      : Amazonka.ChimeSDKIdentity.CreateAppInstanceUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user under an Amazon Chime @AppInstance@. The request consists
-- of a unique @appInstanceUserId@ and @Name@ for that user.
module Amazonka.ChimeSDKIdentity.CreateAppInstanceUser
  ( -- * Creating a Request
    CreateAppInstanceUser (..),
    newCreateAppInstanceUser,

    -- * Request Lenses
    createAppInstanceUser_tags,
    createAppInstanceUser_metadata,
    createAppInstanceUser_appInstanceArn,
    createAppInstanceUser_appInstanceUserId,
    createAppInstanceUser_name,
    createAppInstanceUser_clientRequestToken,

    -- * Destructuring the Response
    CreateAppInstanceUserResponse (..),
    newCreateAppInstanceUserResponse,

    -- * Response Lenses
    createAppInstanceUserResponse_appInstanceUserArn,
    createAppInstanceUserResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAppInstanceUser' smart constructor.
data CreateAppInstanceUser = CreateAppInstanceUser'
  { -- | Tags assigned to the @AppInstanceUser@.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The request\'s metadata. Limited to a 1KB string in UTF-8.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the @AppInstance@ request.
    appInstanceArn :: Prelude.Text,
    -- | The user ID of the @AppInstance@.
    appInstanceUserId :: Data.Sensitive Prelude.Text,
    -- | The user\'s name.
    name :: Data.Sensitive Prelude.Text,
    -- | The token assigned to the user requesting an @AppInstance@.
    clientRequestToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppInstanceUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAppInstanceUser_tags' - Tags assigned to the @AppInstanceUser@.
--
-- 'metadata', 'createAppInstanceUser_metadata' - The request\'s metadata. Limited to a 1KB string in UTF-8.
--
-- 'appInstanceArn', 'createAppInstanceUser_appInstanceArn' - The ARN of the @AppInstance@ request.
--
-- 'appInstanceUserId', 'createAppInstanceUser_appInstanceUserId' - The user ID of the @AppInstance@.
--
-- 'name', 'createAppInstanceUser_name' - The user\'s name.
--
-- 'clientRequestToken', 'createAppInstanceUser_clientRequestToken' - The token assigned to the user requesting an @AppInstance@.
newCreateAppInstanceUser ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  -- | 'appInstanceUserId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateAppInstanceUser
newCreateAppInstanceUser
  pAppInstanceArn_
  pAppInstanceUserId_
  pName_
  pClientRequestToken_ =
    CreateAppInstanceUser'
      { tags = Prelude.Nothing,
        metadata = Prelude.Nothing,
        appInstanceArn = pAppInstanceArn_,
        appInstanceUserId =
          Data._Sensitive Lens.# pAppInstanceUserId_,
        name = Data._Sensitive Lens.# pName_,
        clientRequestToken =
          Data._Sensitive Lens.# pClientRequestToken_
      }

-- | Tags assigned to the @AppInstanceUser@.
createAppInstanceUser_tags :: Lens.Lens' CreateAppInstanceUser (Prelude.Maybe (Prelude.NonEmpty Tag))
createAppInstanceUser_tags = Lens.lens (\CreateAppInstanceUser' {tags} -> tags) (\s@CreateAppInstanceUser' {} a -> s {tags = a} :: CreateAppInstanceUser) Prelude.. Lens.mapping Lens.coerced

-- | The request\'s metadata. Limited to a 1KB string in UTF-8.
createAppInstanceUser_metadata :: Lens.Lens' CreateAppInstanceUser (Prelude.Maybe Prelude.Text)
createAppInstanceUser_metadata = Lens.lens (\CreateAppInstanceUser' {metadata} -> metadata) (\s@CreateAppInstanceUser' {} a -> s {metadata = a} :: CreateAppInstanceUser) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the @AppInstance@ request.
createAppInstanceUser_appInstanceArn :: Lens.Lens' CreateAppInstanceUser Prelude.Text
createAppInstanceUser_appInstanceArn = Lens.lens (\CreateAppInstanceUser' {appInstanceArn} -> appInstanceArn) (\s@CreateAppInstanceUser' {} a -> s {appInstanceArn = a} :: CreateAppInstanceUser)

-- | The user ID of the @AppInstance@.
createAppInstanceUser_appInstanceUserId :: Lens.Lens' CreateAppInstanceUser Prelude.Text
createAppInstanceUser_appInstanceUserId = Lens.lens (\CreateAppInstanceUser' {appInstanceUserId} -> appInstanceUserId) (\s@CreateAppInstanceUser' {} a -> s {appInstanceUserId = a} :: CreateAppInstanceUser) Prelude.. Data._Sensitive

-- | The user\'s name.
createAppInstanceUser_name :: Lens.Lens' CreateAppInstanceUser Prelude.Text
createAppInstanceUser_name = Lens.lens (\CreateAppInstanceUser' {name} -> name) (\s@CreateAppInstanceUser' {} a -> s {name = a} :: CreateAppInstanceUser) Prelude.. Data._Sensitive

-- | The token assigned to the user requesting an @AppInstance@.
createAppInstanceUser_clientRequestToken :: Lens.Lens' CreateAppInstanceUser Prelude.Text
createAppInstanceUser_clientRequestToken = Lens.lens (\CreateAppInstanceUser' {clientRequestToken} -> clientRequestToken) (\s@CreateAppInstanceUser' {} a -> s {clientRequestToken = a} :: CreateAppInstanceUser) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateAppInstanceUser where
  type
    AWSResponse CreateAppInstanceUser =
      CreateAppInstanceUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppInstanceUserResponse'
            Prelude.<$> (x Data..?> "AppInstanceUserArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAppInstanceUser where
  hashWithSalt _salt CreateAppInstanceUser' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` appInstanceArn
      `Prelude.hashWithSalt` appInstanceUserId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateAppInstanceUser where
  rnf CreateAppInstanceUser' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf appInstanceArn
      `Prelude.seq` Prelude.rnf appInstanceUserId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateAppInstanceUser where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateAppInstanceUser where
  toJSON CreateAppInstanceUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("Metadata" Data..=) Prelude.<$> metadata,
            Prelude.Just
              ("AppInstanceArn" Data..= appInstanceArn),
            Prelude.Just
              ("AppInstanceUserId" Data..= appInstanceUserId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateAppInstanceUser where
  toPath = Prelude.const "/app-instance-users"

instance Data.ToQuery CreateAppInstanceUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppInstanceUserResponse' smart constructor.
data CreateAppInstanceUserResponse = CreateAppInstanceUserResponse'
  { -- | The user\'s ARN.
    appInstanceUserArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppInstanceUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'createAppInstanceUserResponse_appInstanceUserArn' - The user\'s ARN.
--
-- 'httpStatus', 'createAppInstanceUserResponse_httpStatus' - The response's http status code.
newCreateAppInstanceUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAppInstanceUserResponse
newCreateAppInstanceUserResponse pHttpStatus_ =
  CreateAppInstanceUserResponse'
    { appInstanceUserArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user\'s ARN.
createAppInstanceUserResponse_appInstanceUserArn :: Lens.Lens' CreateAppInstanceUserResponse (Prelude.Maybe Prelude.Text)
createAppInstanceUserResponse_appInstanceUserArn = Lens.lens (\CreateAppInstanceUserResponse' {appInstanceUserArn} -> appInstanceUserArn) (\s@CreateAppInstanceUserResponse' {} a -> s {appInstanceUserArn = a} :: CreateAppInstanceUserResponse)

-- | The response's http status code.
createAppInstanceUserResponse_httpStatus :: Lens.Lens' CreateAppInstanceUserResponse Prelude.Int
createAppInstanceUserResponse_httpStatus = Lens.lens (\CreateAppInstanceUserResponse' {httpStatus} -> httpStatus) (\s@CreateAppInstanceUserResponse' {} a -> s {httpStatus = a} :: CreateAppInstanceUserResponse)

instance Prelude.NFData CreateAppInstanceUserResponse where
  rnf CreateAppInstanceUserResponse' {..} =
    Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf httpStatus
