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
-- Module      : Amazonka.AmplifyBackend.DeleteToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the challenge token based on the given appId and sessionId.
module Amazonka.AmplifyBackend.DeleteToken
  ( -- * Creating a Request
    DeleteToken (..),
    newDeleteToken,

    -- * Request Lenses
    deleteToken_sessionId,
    deleteToken_appId,

    -- * Destructuring the Response
    DeleteTokenResponse (..),
    newDeleteTokenResponse,

    -- * Response Lenses
    deleteTokenResponse_isSuccess,
    deleteTokenResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteToken' smart constructor.
data DeleteToken = DeleteToken'
  { -- | The session ID.
    sessionId :: Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'deleteToken_sessionId' - The session ID.
--
-- 'appId', 'deleteToken_appId' - The app ID.
newDeleteToken ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'appId'
  Prelude.Text ->
  DeleteToken
newDeleteToken pSessionId_ pAppId_ =
  DeleteToken'
    { sessionId = pSessionId_,
      appId = pAppId_
    }

-- | The session ID.
deleteToken_sessionId :: Lens.Lens' DeleteToken Prelude.Text
deleteToken_sessionId = Lens.lens (\DeleteToken' {sessionId} -> sessionId) (\s@DeleteToken' {} a -> s {sessionId = a} :: DeleteToken)

-- | The app ID.
deleteToken_appId :: Lens.Lens' DeleteToken Prelude.Text
deleteToken_appId = Lens.lens (\DeleteToken' {appId} -> appId) (\s@DeleteToken' {} a -> s {appId = a} :: DeleteToken)

instance Core.AWSRequest DeleteToken where
  type AWSResponse DeleteToken = DeleteTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTokenResponse'
            Prelude.<$> (x Data..?> "isSuccess")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteToken where
  hashWithSalt _salt DeleteToken' {..} =
    _salt
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` appId

instance Prelude.NFData DeleteToken where
  rnf DeleteToken' {..} =
    Prelude.rnf sessionId `Prelude.seq`
      Prelude.rnf appId

instance Data.ToHeaders DeleteToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteToken where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DeleteToken where
  toPath DeleteToken' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/challenge/",
        Data.toBS sessionId,
        "/remove"
      ]

instance Data.ToQuery DeleteToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTokenResponse' smart constructor.
data DeleteTokenResponse = DeleteTokenResponse'
  { -- | Indicates whether the request succeeded or failed.
    isSuccess :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isSuccess', 'deleteTokenResponse_isSuccess' - Indicates whether the request succeeded or failed.
--
-- 'httpStatus', 'deleteTokenResponse_httpStatus' - The response's http status code.
newDeleteTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTokenResponse
newDeleteTokenResponse pHttpStatus_ =
  DeleteTokenResponse'
    { isSuccess = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether the request succeeded or failed.
deleteTokenResponse_isSuccess :: Lens.Lens' DeleteTokenResponse (Prelude.Maybe Prelude.Bool)
deleteTokenResponse_isSuccess = Lens.lens (\DeleteTokenResponse' {isSuccess} -> isSuccess) (\s@DeleteTokenResponse' {} a -> s {isSuccess = a} :: DeleteTokenResponse)

-- | The response's http status code.
deleteTokenResponse_httpStatus :: Lens.Lens' DeleteTokenResponse Prelude.Int
deleteTokenResponse_httpStatus = Lens.lens (\DeleteTokenResponse' {httpStatus} -> httpStatus) (\s@DeleteTokenResponse' {} a -> s {httpStatus = a} :: DeleteTokenResponse)

instance Prelude.NFData DeleteTokenResponse where
  rnf DeleteTokenResponse' {..} =
    Prelude.rnf isSuccess `Prelude.seq`
      Prelude.rnf httpStatus
