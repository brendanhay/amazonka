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
-- Module      : Amazonka.GamesParks.GetGeneratedCodeJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a job that is generating code for a snapshot.
module Amazonka.GamesParks.GetGeneratedCodeJob
  ( -- * Creating a Request
    GetGeneratedCodeJob (..),
    newGetGeneratedCodeJob,

    -- * Request Lenses
    getGeneratedCodeJob_gameName,
    getGeneratedCodeJob_jobId,
    getGeneratedCodeJob_snapshotId,

    -- * Destructuring the Response
    GetGeneratedCodeJobResponse (..),
    newGetGeneratedCodeJobResponse,

    -- * Response Lenses
    getGeneratedCodeJobResponse_generatedCodeJob,
    getGeneratedCodeJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGeneratedCodeJob' smart constructor.
data GetGeneratedCodeJob = GetGeneratedCodeJob'
  { -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The identifier of the code generation job.
    jobId :: Prelude.Text,
    -- | The identifier of the snapshot for the code generation job.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGeneratedCodeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'getGeneratedCodeJob_gameName' - The name of the game.
--
-- 'jobId', 'getGeneratedCodeJob_jobId' - The identifier of the code generation job.
--
-- 'snapshotId', 'getGeneratedCodeJob_snapshotId' - The identifier of the snapshot for the code generation job.
newGetGeneratedCodeJob ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  -- | 'snapshotId'
  Prelude.Text ->
  GetGeneratedCodeJob
newGetGeneratedCodeJob
  pGameName_
  pJobId_
  pSnapshotId_ =
    GetGeneratedCodeJob'
      { gameName = pGameName_,
        jobId = pJobId_,
        snapshotId = pSnapshotId_
      }

-- | The name of the game.
getGeneratedCodeJob_gameName :: Lens.Lens' GetGeneratedCodeJob Prelude.Text
getGeneratedCodeJob_gameName = Lens.lens (\GetGeneratedCodeJob' {gameName} -> gameName) (\s@GetGeneratedCodeJob' {} a -> s {gameName = a} :: GetGeneratedCodeJob)

-- | The identifier of the code generation job.
getGeneratedCodeJob_jobId :: Lens.Lens' GetGeneratedCodeJob Prelude.Text
getGeneratedCodeJob_jobId = Lens.lens (\GetGeneratedCodeJob' {jobId} -> jobId) (\s@GetGeneratedCodeJob' {} a -> s {jobId = a} :: GetGeneratedCodeJob)

-- | The identifier of the snapshot for the code generation job.
getGeneratedCodeJob_snapshotId :: Lens.Lens' GetGeneratedCodeJob Prelude.Text
getGeneratedCodeJob_snapshotId = Lens.lens (\GetGeneratedCodeJob' {snapshotId} -> snapshotId) (\s@GetGeneratedCodeJob' {} a -> s {snapshotId = a} :: GetGeneratedCodeJob)

instance Core.AWSRequest GetGeneratedCodeJob where
  type
    AWSResponse GetGeneratedCodeJob =
      GetGeneratedCodeJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGeneratedCodeJobResponse'
            Prelude.<$> (x Data..?> "GeneratedCodeJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGeneratedCodeJob where
  hashWithSalt _salt GetGeneratedCodeJob' {..} =
    _salt `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData GetGeneratedCodeJob where
  rnf GetGeneratedCodeJob' {..} =
    Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders GetGeneratedCodeJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetGeneratedCodeJob where
  toPath GetGeneratedCodeJob' {..} =
    Prelude.mconcat
      [ "/game/",
        Data.toBS gameName,
        "/snapshot/",
        Data.toBS snapshotId,
        "/generated-sdk-code-job/",
        Data.toBS jobId
      ]

instance Data.ToQuery GetGeneratedCodeJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGeneratedCodeJobResponse' smart constructor.
data GetGeneratedCodeJobResponse = GetGeneratedCodeJobResponse'
  { -- | Details about the generated code job.
    generatedCodeJob :: Prelude.Maybe GeneratedCodeJobDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGeneratedCodeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generatedCodeJob', 'getGeneratedCodeJobResponse_generatedCodeJob' - Details about the generated code job.
--
-- 'httpStatus', 'getGeneratedCodeJobResponse_httpStatus' - The response's http status code.
newGetGeneratedCodeJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGeneratedCodeJobResponse
newGetGeneratedCodeJobResponse pHttpStatus_ =
  GetGeneratedCodeJobResponse'
    { generatedCodeJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the generated code job.
getGeneratedCodeJobResponse_generatedCodeJob :: Lens.Lens' GetGeneratedCodeJobResponse (Prelude.Maybe GeneratedCodeJobDetails)
getGeneratedCodeJobResponse_generatedCodeJob = Lens.lens (\GetGeneratedCodeJobResponse' {generatedCodeJob} -> generatedCodeJob) (\s@GetGeneratedCodeJobResponse' {} a -> s {generatedCodeJob = a} :: GetGeneratedCodeJobResponse)

-- | The response's http status code.
getGeneratedCodeJobResponse_httpStatus :: Lens.Lens' GetGeneratedCodeJobResponse Prelude.Int
getGeneratedCodeJobResponse_httpStatus = Lens.lens (\GetGeneratedCodeJobResponse' {httpStatus} -> httpStatus) (\s@GetGeneratedCodeJobResponse' {} a -> s {httpStatus = a} :: GetGeneratedCodeJobResponse)

instance Prelude.NFData GetGeneratedCodeJobResponse where
  rnf GetGeneratedCodeJobResponse' {..} =
    Prelude.rnf generatedCodeJob
      `Prelude.seq` Prelude.rnf httpStatus
