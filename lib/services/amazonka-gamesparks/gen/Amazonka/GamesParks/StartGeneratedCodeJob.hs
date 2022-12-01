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
-- Module      : Amazonka.GamesParks.StartGeneratedCodeJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous process that generates client code for
-- system-defined and custom messages. The resulting code is collected as a
-- .zip file and uploaded to a pre-signed Amazon S3 URL.
module Amazonka.GamesParks.StartGeneratedCodeJob
  ( -- * Creating a Request
    StartGeneratedCodeJob (..),
    newStartGeneratedCodeJob,

    -- * Request Lenses
    startGeneratedCodeJob_gameName,
    startGeneratedCodeJob_generator,
    startGeneratedCodeJob_snapshotId,

    -- * Destructuring the Response
    StartGeneratedCodeJobResponse (..),
    newStartGeneratedCodeJobResponse,

    -- * Response Lenses
    startGeneratedCodeJobResponse_generatedCodeJobId,
    startGeneratedCodeJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartGeneratedCodeJob' smart constructor.
data StartGeneratedCodeJob = StartGeneratedCodeJob'
  { -- | The name of the game.
    gameName :: Prelude.Text,
    -- | Properties of the generator to use for the job.
    generator :: Generator,
    -- | The identifier of the snapshot for which to generate code.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartGeneratedCodeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'startGeneratedCodeJob_gameName' - The name of the game.
--
-- 'generator', 'startGeneratedCodeJob_generator' - Properties of the generator to use for the job.
--
-- 'snapshotId', 'startGeneratedCodeJob_snapshotId' - The identifier of the snapshot for which to generate code.
newStartGeneratedCodeJob ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'generator'
  Generator ->
  -- | 'snapshotId'
  Prelude.Text ->
  StartGeneratedCodeJob
newStartGeneratedCodeJob
  pGameName_
  pGenerator_
  pSnapshotId_ =
    StartGeneratedCodeJob'
      { gameName = pGameName_,
        generator = pGenerator_,
        snapshotId = pSnapshotId_
      }

-- | The name of the game.
startGeneratedCodeJob_gameName :: Lens.Lens' StartGeneratedCodeJob Prelude.Text
startGeneratedCodeJob_gameName = Lens.lens (\StartGeneratedCodeJob' {gameName} -> gameName) (\s@StartGeneratedCodeJob' {} a -> s {gameName = a} :: StartGeneratedCodeJob)

-- | Properties of the generator to use for the job.
startGeneratedCodeJob_generator :: Lens.Lens' StartGeneratedCodeJob Generator
startGeneratedCodeJob_generator = Lens.lens (\StartGeneratedCodeJob' {generator} -> generator) (\s@StartGeneratedCodeJob' {} a -> s {generator = a} :: StartGeneratedCodeJob)

-- | The identifier of the snapshot for which to generate code.
startGeneratedCodeJob_snapshotId :: Lens.Lens' StartGeneratedCodeJob Prelude.Text
startGeneratedCodeJob_snapshotId = Lens.lens (\StartGeneratedCodeJob' {snapshotId} -> snapshotId) (\s@StartGeneratedCodeJob' {} a -> s {snapshotId = a} :: StartGeneratedCodeJob)

instance Core.AWSRequest StartGeneratedCodeJob where
  type
    AWSResponse StartGeneratedCodeJob =
      StartGeneratedCodeJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartGeneratedCodeJobResponse'
            Prelude.<$> (x Core..?> "GeneratedCodeJobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartGeneratedCodeJob where
  hashWithSalt _salt StartGeneratedCodeJob' {..} =
    _salt `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` generator
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData StartGeneratedCodeJob where
  rnf StartGeneratedCodeJob' {..} =
    Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf generator
      `Prelude.seq` Prelude.rnf snapshotId

instance Core.ToHeaders StartGeneratedCodeJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartGeneratedCodeJob where
  toJSON StartGeneratedCodeJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Generator" Core..= generator)]
      )

instance Core.ToPath StartGeneratedCodeJob where
  toPath StartGeneratedCodeJob' {..} =
    Prelude.mconcat
      [ "/game/",
        Core.toBS gameName,
        "/snapshot/",
        Core.toBS snapshotId,
        "/generated-sdk-code-job"
      ]

instance Core.ToQuery StartGeneratedCodeJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartGeneratedCodeJobResponse' smart constructor.
data StartGeneratedCodeJobResponse = StartGeneratedCodeJobResponse'
  { -- | The identifier of the code generation job. You can use this identifier
    -- in the @GetGeneratedCodeJob@ operation.
    generatedCodeJobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartGeneratedCodeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generatedCodeJobId', 'startGeneratedCodeJobResponse_generatedCodeJobId' - The identifier of the code generation job. You can use this identifier
-- in the @GetGeneratedCodeJob@ operation.
--
-- 'httpStatus', 'startGeneratedCodeJobResponse_httpStatus' - The response's http status code.
newStartGeneratedCodeJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartGeneratedCodeJobResponse
newStartGeneratedCodeJobResponse pHttpStatus_ =
  StartGeneratedCodeJobResponse'
    { generatedCodeJobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the code generation job. You can use this identifier
-- in the @GetGeneratedCodeJob@ operation.
startGeneratedCodeJobResponse_generatedCodeJobId :: Lens.Lens' StartGeneratedCodeJobResponse (Prelude.Maybe Prelude.Text)
startGeneratedCodeJobResponse_generatedCodeJobId = Lens.lens (\StartGeneratedCodeJobResponse' {generatedCodeJobId} -> generatedCodeJobId) (\s@StartGeneratedCodeJobResponse' {} a -> s {generatedCodeJobId = a} :: StartGeneratedCodeJobResponse)

-- | The response's http status code.
startGeneratedCodeJobResponse_httpStatus :: Lens.Lens' StartGeneratedCodeJobResponse Prelude.Int
startGeneratedCodeJobResponse_httpStatus = Lens.lens (\StartGeneratedCodeJobResponse' {httpStatus} -> httpStatus) (\s@StartGeneratedCodeJobResponse' {} a -> s {httpStatus = a} :: StartGeneratedCodeJobResponse)

instance Prelude.NFData StartGeneratedCodeJobResponse where
  rnf StartGeneratedCodeJobResponse' {..} =
    Prelude.rnf generatedCodeJobId
      `Prelude.seq` Prelude.rnf httpStatus
