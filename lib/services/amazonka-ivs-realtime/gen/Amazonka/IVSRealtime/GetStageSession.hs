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
-- Module      : Amazonka.IVSRealtime.GetStageSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information for the specified stage session.
module Amazonka.IVSRealtime.GetStageSession
  ( -- * Creating a Request
    GetStageSession (..),
    newGetStageSession,

    -- * Request Lenses
    getStageSession_sessionId,
    getStageSession_stageArn,

    -- * Destructuring the Response
    GetStageSessionResponse (..),
    newGetStageSessionResponse,

    -- * Response Lenses
    getStageSessionResponse_stageSession,
    getStageSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStageSession' smart constructor.
data GetStageSession = GetStageSession'
  { -- | ID of a session within the stage.
    sessionId :: Prelude.Text,
    -- | ARN of the stage for which the information is to be retrieved.
    stageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStageSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'getStageSession_sessionId' - ID of a session within the stage.
--
-- 'stageArn', 'getStageSession_stageArn' - ARN of the stage for which the information is to be retrieved.
newGetStageSession ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'stageArn'
  Prelude.Text ->
  GetStageSession
newGetStageSession pSessionId_ pStageArn_ =
  GetStageSession'
    { sessionId = pSessionId_,
      stageArn = pStageArn_
    }

-- | ID of a session within the stage.
getStageSession_sessionId :: Lens.Lens' GetStageSession Prelude.Text
getStageSession_sessionId = Lens.lens (\GetStageSession' {sessionId} -> sessionId) (\s@GetStageSession' {} a -> s {sessionId = a} :: GetStageSession)

-- | ARN of the stage for which the information is to be retrieved.
getStageSession_stageArn :: Lens.Lens' GetStageSession Prelude.Text
getStageSession_stageArn = Lens.lens (\GetStageSession' {stageArn} -> stageArn) (\s@GetStageSession' {} a -> s {stageArn = a} :: GetStageSession)

instance Core.AWSRequest GetStageSession where
  type
    AWSResponse GetStageSession =
      GetStageSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStageSessionResponse'
            Prelude.<$> (x Data..?> "stageSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStageSession where
  hashWithSalt _salt GetStageSession' {..} =
    _salt
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` stageArn

instance Prelude.NFData GetStageSession where
  rnf GetStageSession' {..} =
    Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf stageArn

instance Data.ToHeaders GetStageSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetStageSession where
  toJSON GetStageSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("sessionId" Data..= sessionId),
            Prelude.Just ("stageArn" Data..= stageArn)
          ]
      )

instance Data.ToPath GetStageSession where
  toPath = Prelude.const "/GetStageSession"

instance Data.ToQuery GetStageSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStageSessionResponse' smart constructor.
data GetStageSessionResponse = GetStageSessionResponse'
  { -- | The stage session that is returned.
    stageSession :: Prelude.Maybe StageSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStageSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageSession', 'getStageSessionResponse_stageSession' - The stage session that is returned.
--
-- 'httpStatus', 'getStageSessionResponse_httpStatus' - The response's http status code.
newGetStageSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStageSessionResponse
newGetStageSessionResponse pHttpStatus_ =
  GetStageSessionResponse'
    { stageSession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The stage session that is returned.
getStageSessionResponse_stageSession :: Lens.Lens' GetStageSessionResponse (Prelude.Maybe StageSession)
getStageSessionResponse_stageSession = Lens.lens (\GetStageSessionResponse' {stageSession} -> stageSession) (\s@GetStageSessionResponse' {} a -> s {stageSession = a} :: GetStageSessionResponse)

-- | The response's http status code.
getStageSessionResponse_httpStatus :: Lens.Lens' GetStageSessionResponse Prelude.Int
getStageSessionResponse_httpStatus = Lens.lens (\GetStageSessionResponse' {httpStatus} -> httpStatus) (\s@GetStageSessionResponse' {} a -> s {httpStatus = a} :: GetStageSessionResponse)

instance Prelude.NFData GetStageSessionResponse where
  rnf GetStageSessionResponse' {..} =
    Prelude.rnf stageSession
      `Prelude.seq` Prelude.rnf httpStatus
