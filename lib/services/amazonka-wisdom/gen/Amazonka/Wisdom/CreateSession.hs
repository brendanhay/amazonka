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
-- Module      : Amazonka.Wisdom.CreateSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a session. A session is a contextual container used for
-- generating recommendations. Amazon Connect creates a new Wisdom session
-- for each contact on which Wisdom is enabled.
module Amazonka.Wisdom.CreateSession
  ( -- * Creating a Request
    CreateSession (..),
    newCreateSession,

    -- * Request Lenses
    createSession_clientToken,
    createSession_description,
    createSession_tags,
    createSession_assistantId,
    createSession_name,

    -- * Destructuring the Response
    CreateSessionResponse (..),
    newCreateSessionResponse,

    -- * Response Lenses
    createSessionResponse_session,
    createSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newCreateSession' smart constructor.
data CreateSession = CreateSession'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text,
    -- | The name of the session.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createSession_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createSession_description' - The description.
--
-- 'tags', 'createSession_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'assistantId', 'createSession_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'name', 'createSession_name' - The name of the session.
newCreateSession ::
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateSession
newCreateSession pAssistantId_ pName_ =
  CreateSession'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      assistantId = pAssistantId_,
      name = pName_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createSession_clientToken :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Text)
createSession_clientToken = Lens.lens (\CreateSession' {clientToken} -> clientToken) (\s@CreateSession' {} a -> s {clientToken = a} :: CreateSession)

-- | The description.
createSession_description :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Text)
createSession_description = Lens.lens (\CreateSession' {description} -> description) (\s@CreateSession' {} a -> s {description = a} :: CreateSession)

-- | The tags used to organize, track, or control access for this resource.
createSession_tags :: Lens.Lens' CreateSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSession_tags = Lens.lens (\CreateSession' {tags} -> tags) (\s@CreateSession' {} a -> s {tags = a} :: CreateSession) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
createSession_assistantId :: Lens.Lens' CreateSession Prelude.Text
createSession_assistantId = Lens.lens (\CreateSession' {assistantId} -> assistantId) (\s@CreateSession' {} a -> s {assistantId = a} :: CreateSession)

-- | The name of the session.
createSession_name :: Lens.Lens' CreateSession Prelude.Text
createSession_name = Lens.lens (\CreateSession' {name} -> name) (\s@CreateSession' {} a -> s {name = a} :: CreateSession)

instance Core.AWSRequest CreateSession where
  type
    AWSResponse CreateSession =
      CreateSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSessionResponse'
            Prelude.<$> (x Data..?> "session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSession where
  hashWithSalt _salt CreateSession' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateSession where
  rnf CreateSession' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf assistantId `Prelude.seq`
            Prelude.rnf name

instance Data.ToHeaders CreateSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSession where
  toJSON CreateSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateSession where
  toPath CreateSession' {..} =
    Prelude.mconcat
      ["/assistants/", Data.toBS assistantId, "/sessions"]

instance Data.ToQuery CreateSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSessionResponse' smart constructor.
data CreateSessionResponse = CreateSessionResponse'
  { -- | The session.
    session :: Prelude.Maybe SessionData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'session', 'createSessionResponse_session' - The session.
--
-- 'httpStatus', 'createSessionResponse_httpStatus' - The response's http status code.
newCreateSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSessionResponse
newCreateSessionResponse pHttpStatus_ =
  CreateSessionResponse'
    { session = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session.
createSessionResponse_session :: Lens.Lens' CreateSessionResponse (Prelude.Maybe SessionData)
createSessionResponse_session = Lens.lens (\CreateSessionResponse' {session} -> session) (\s@CreateSessionResponse' {} a -> s {session = a} :: CreateSessionResponse)

-- | The response's http status code.
createSessionResponse_httpStatus :: Lens.Lens' CreateSessionResponse Prelude.Int
createSessionResponse_httpStatus = Lens.lens (\CreateSessionResponse' {httpStatus} -> httpStatus) (\s@CreateSessionResponse' {} a -> s {httpStatus = a} :: CreateSessionResponse)

instance Prelude.NFData CreateSessionResponse where
  rnf CreateSessionResponse' {..} =
    Prelude.rnf session `Prelude.seq`
      Prelude.rnf httpStatus
