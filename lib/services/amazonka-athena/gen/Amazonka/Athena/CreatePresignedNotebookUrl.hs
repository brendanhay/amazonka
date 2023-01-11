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
-- Module      : Amazonka.Athena.CreatePresignedNotebookUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an authentication token and the URL at which the notebook can be
-- accessed. During programmatic access, @CreatePresignedNotebookUrl@ must
-- be called every 10 minutes to refresh the authentication token.
module Amazonka.Athena.CreatePresignedNotebookUrl
  ( -- * Creating a Request
    CreatePresignedNotebookUrl (..),
    newCreatePresignedNotebookUrl,

    -- * Request Lenses
    createPresignedNotebookUrl_sessionId,

    -- * Destructuring the Response
    CreatePresignedNotebookUrlResponse (..),
    newCreatePresignedNotebookUrlResponse,

    -- * Response Lenses
    createPresignedNotebookUrlResponse_httpStatus,
    createPresignedNotebookUrlResponse_notebookUrl,
    createPresignedNotebookUrlResponse_authToken,
    createPresignedNotebookUrlResponse_authTokenExpirationTime,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePresignedNotebookUrl' smart constructor.
data CreatePresignedNotebookUrl = CreatePresignedNotebookUrl'
  { -- | The session ID.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePresignedNotebookUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'createPresignedNotebookUrl_sessionId' - The session ID.
newCreatePresignedNotebookUrl ::
  -- | 'sessionId'
  Prelude.Text ->
  CreatePresignedNotebookUrl
newCreatePresignedNotebookUrl pSessionId_ =
  CreatePresignedNotebookUrl'
    { sessionId =
        pSessionId_
    }

-- | The session ID.
createPresignedNotebookUrl_sessionId :: Lens.Lens' CreatePresignedNotebookUrl Prelude.Text
createPresignedNotebookUrl_sessionId = Lens.lens (\CreatePresignedNotebookUrl' {sessionId} -> sessionId) (\s@CreatePresignedNotebookUrl' {} a -> s {sessionId = a} :: CreatePresignedNotebookUrl)

instance Core.AWSRequest CreatePresignedNotebookUrl where
  type
    AWSResponse CreatePresignedNotebookUrl =
      CreatePresignedNotebookUrlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePresignedNotebookUrlResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "NotebookUrl")
            Prelude.<*> (x Data..:> "AuthToken")
            Prelude.<*> (x Data..:> "AuthTokenExpirationTime")
      )

instance Prelude.Hashable CreatePresignedNotebookUrl where
  hashWithSalt _salt CreatePresignedNotebookUrl' {..} =
    _salt `Prelude.hashWithSalt` sessionId

instance Prelude.NFData CreatePresignedNotebookUrl where
  rnf CreatePresignedNotebookUrl' {..} =
    Prelude.rnf sessionId

instance Data.ToHeaders CreatePresignedNotebookUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.CreatePresignedNotebookUrl" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePresignedNotebookUrl where
  toJSON CreatePresignedNotebookUrl' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SessionId" Data..= sessionId)]
      )

instance Data.ToPath CreatePresignedNotebookUrl where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePresignedNotebookUrl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePresignedNotebookUrlResponse' smart constructor.
data CreatePresignedNotebookUrlResponse = CreatePresignedNotebookUrlResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The URL of the notebook. The URL includes the authentication token and
    -- notebook file name and points directly to the opened notebook.
    notebookUrl :: Prelude.Text,
    -- | The authentication token for the notebook.
    authToken :: Prelude.Text,
    -- | The UTC epoch time when the authentication token expires.
    authTokenExpirationTime :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePresignedNotebookUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPresignedNotebookUrlResponse_httpStatus' - The response's http status code.
--
-- 'notebookUrl', 'createPresignedNotebookUrlResponse_notebookUrl' - The URL of the notebook. The URL includes the authentication token and
-- notebook file name and points directly to the opened notebook.
--
-- 'authToken', 'createPresignedNotebookUrlResponse_authToken' - The authentication token for the notebook.
--
-- 'authTokenExpirationTime', 'createPresignedNotebookUrlResponse_authTokenExpirationTime' - The UTC epoch time when the authentication token expires.
newCreatePresignedNotebookUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'notebookUrl'
  Prelude.Text ->
  -- | 'authToken'
  Prelude.Text ->
  -- | 'authTokenExpirationTime'
  Prelude.Integer ->
  CreatePresignedNotebookUrlResponse
newCreatePresignedNotebookUrlResponse
  pHttpStatus_
  pNotebookUrl_
  pAuthToken_
  pAuthTokenExpirationTime_ =
    CreatePresignedNotebookUrlResponse'
      { httpStatus =
          pHttpStatus_,
        notebookUrl = pNotebookUrl_,
        authToken = pAuthToken_,
        authTokenExpirationTime =
          pAuthTokenExpirationTime_
      }

-- | The response's http status code.
createPresignedNotebookUrlResponse_httpStatus :: Lens.Lens' CreatePresignedNotebookUrlResponse Prelude.Int
createPresignedNotebookUrlResponse_httpStatus = Lens.lens (\CreatePresignedNotebookUrlResponse' {httpStatus} -> httpStatus) (\s@CreatePresignedNotebookUrlResponse' {} a -> s {httpStatus = a} :: CreatePresignedNotebookUrlResponse)

-- | The URL of the notebook. The URL includes the authentication token and
-- notebook file name and points directly to the opened notebook.
createPresignedNotebookUrlResponse_notebookUrl :: Lens.Lens' CreatePresignedNotebookUrlResponse Prelude.Text
createPresignedNotebookUrlResponse_notebookUrl = Lens.lens (\CreatePresignedNotebookUrlResponse' {notebookUrl} -> notebookUrl) (\s@CreatePresignedNotebookUrlResponse' {} a -> s {notebookUrl = a} :: CreatePresignedNotebookUrlResponse)

-- | The authentication token for the notebook.
createPresignedNotebookUrlResponse_authToken :: Lens.Lens' CreatePresignedNotebookUrlResponse Prelude.Text
createPresignedNotebookUrlResponse_authToken = Lens.lens (\CreatePresignedNotebookUrlResponse' {authToken} -> authToken) (\s@CreatePresignedNotebookUrlResponse' {} a -> s {authToken = a} :: CreatePresignedNotebookUrlResponse)

-- | The UTC epoch time when the authentication token expires.
createPresignedNotebookUrlResponse_authTokenExpirationTime :: Lens.Lens' CreatePresignedNotebookUrlResponse Prelude.Integer
createPresignedNotebookUrlResponse_authTokenExpirationTime = Lens.lens (\CreatePresignedNotebookUrlResponse' {authTokenExpirationTime} -> authTokenExpirationTime) (\s@CreatePresignedNotebookUrlResponse' {} a -> s {authTokenExpirationTime = a} :: CreatePresignedNotebookUrlResponse)

instance
  Prelude.NFData
    CreatePresignedNotebookUrlResponse
  where
  rnf CreatePresignedNotebookUrlResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf notebookUrl
      `Prelude.seq` Prelude.rnf authToken
      `Prelude.seq` Prelude.rnf authTokenExpirationTime
