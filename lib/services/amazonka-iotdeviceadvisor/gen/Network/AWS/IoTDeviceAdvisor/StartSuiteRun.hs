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
-- Module      : Network.AWS.IoTDeviceAdvisor.StartSuiteRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a Device Advisor test suite run.
module Network.AWS.IoTDeviceAdvisor.StartSuiteRun
  ( -- * Creating a Request
    StartSuiteRun (..),
    newStartSuiteRun,

    -- * Request Lenses
    startSuiteRun_suiteRunConfiguration,
    startSuiteRun_suiteDefinitionVersion,
    startSuiteRun_tags,
    startSuiteRun_suiteDefinitionId,

    -- * Destructuring the Response
    StartSuiteRunResponse (..),
    newStartSuiteRunResponse,

    -- * Response Lenses
    startSuiteRunResponse_createdAt,
    startSuiteRunResponse_suiteRunArn,
    startSuiteRunResponse_suiteRunId,
    startSuiteRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTDeviceAdvisor.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartSuiteRun' smart constructor.
data StartSuiteRun = StartSuiteRun'
  { -- | Suite run configuration.
    suiteRunConfiguration :: Prelude.Maybe SuiteRunConfiguration,
    -- | Suite definition version of the test suite.
    suiteDefinitionVersion :: Prelude.Maybe Prelude.Text,
    -- | The tags to be attached to the suite run.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Suite definition Id of the test suite.
    suiteDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSuiteRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suiteRunConfiguration', 'startSuiteRun_suiteRunConfiguration' - Suite run configuration.
--
-- 'suiteDefinitionVersion', 'startSuiteRun_suiteDefinitionVersion' - Suite definition version of the test suite.
--
-- 'tags', 'startSuiteRun_tags' - The tags to be attached to the suite run.
--
-- 'suiteDefinitionId', 'startSuiteRun_suiteDefinitionId' - Suite definition Id of the test suite.
newStartSuiteRun ::
  -- | 'suiteDefinitionId'
  Prelude.Text ->
  StartSuiteRun
newStartSuiteRun pSuiteDefinitionId_ =
  StartSuiteRun'
    { suiteRunConfiguration =
        Prelude.Nothing,
      suiteDefinitionVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      suiteDefinitionId = pSuiteDefinitionId_
    }

-- | Suite run configuration.
startSuiteRun_suiteRunConfiguration :: Lens.Lens' StartSuiteRun (Prelude.Maybe SuiteRunConfiguration)
startSuiteRun_suiteRunConfiguration = Lens.lens (\StartSuiteRun' {suiteRunConfiguration} -> suiteRunConfiguration) (\s@StartSuiteRun' {} a -> s {suiteRunConfiguration = a} :: StartSuiteRun)

-- | Suite definition version of the test suite.
startSuiteRun_suiteDefinitionVersion :: Lens.Lens' StartSuiteRun (Prelude.Maybe Prelude.Text)
startSuiteRun_suiteDefinitionVersion = Lens.lens (\StartSuiteRun' {suiteDefinitionVersion} -> suiteDefinitionVersion) (\s@StartSuiteRun' {} a -> s {suiteDefinitionVersion = a} :: StartSuiteRun)

-- | The tags to be attached to the suite run.
startSuiteRun_tags :: Lens.Lens' StartSuiteRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startSuiteRun_tags = Lens.lens (\StartSuiteRun' {tags} -> tags) (\s@StartSuiteRun' {} a -> s {tags = a} :: StartSuiteRun) Prelude.. Lens.mapping Lens.coerced

-- | Suite definition Id of the test suite.
startSuiteRun_suiteDefinitionId :: Lens.Lens' StartSuiteRun Prelude.Text
startSuiteRun_suiteDefinitionId = Lens.lens (\StartSuiteRun' {suiteDefinitionId} -> suiteDefinitionId) (\s@StartSuiteRun' {} a -> s {suiteDefinitionId = a} :: StartSuiteRun)

instance Core.AWSRequest StartSuiteRun where
  type
    AWSResponse StartSuiteRun =
      StartSuiteRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSuiteRunResponse'
            Prelude.<$> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "suiteRunArn")
            Prelude.<*> (x Core..?> "suiteRunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSuiteRun

instance Prelude.NFData StartSuiteRun

instance Core.ToHeaders StartSuiteRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartSuiteRun where
  toJSON StartSuiteRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("suiteRunConfiguration" Core..=)
              Prelude.<$> suiteRunConfiguration,
            ("suiteDefinitionVersion" Core..=)
              Prelude.<$> suiteDefinitionVersion,
            ("tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath StartSuiteRun where
  toPath StartSuiteRun' {..} =
    Prelude.mconcat
      [ "/suiteDefinitions/",
        Core.toBS suiteDefinitionId,
        "/suiteRuns"
      ]

instance Core.ToQuery StartSuiteRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSuiteRunResponse' smart constructor.
data StartSuiteRunResponse = StartSuiteRunResponse'
  { -- | Date (in Unix epoch time) when the suite run was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Amazon resource name of the started suite run.
    suiteRunArn :: Prelude.Maybe Prelude.Text,
    -- | Suite Run Id of the started suite run.
    suiteRunId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSuiteRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'startSuiteRunResponse_createdAt' - Date (in Unix epoch time) when the suite run was created.
--
-- 'suiteRunArn', 'startSuiteRunResponse_suiteRunArn' - Amazon resource name of the started suite run.
--
-- 'suiteRunId', 'startSuiteRunResponse_suiteRunId' - Suite Run Id of the started suite run.
--
-- 'httpStatus', 'startSuiteRunResponse_httpStatus' - The response's http status code.
newStartSuiteRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSuiteRunResponse
newStartSuiteRunResponse pHttpStatus_ =
  StartSuiteRunResponse'
    { createdAt = Prelude.Nothing,
      suiteRunArn = Prelude.Nothing,
      suiteRunId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Date (in Unix epoch time) when the suite run was created.
startSuiteRunResponse_createdAt :: Lens.Lens' StartSuiteRunResponse (Prelude.Maybe Prelude.UTCTime)
startSuiteRunResponse_createdAt = Lens.lens (\StartSuiteRunResponse' {createdAt} -> createdAt) (\s@StartSuiteRunResponse' {} a -> s {createdAt = a} :: StartSuiteRunResponse) Prelude.. Lens.mapping Core._Time

-- | Amazon resource name of the started suite run.
startSuiteRunResponse_suiteRunArn :: Lens.Lens' StartSuiteRunResponse (Prelude.Maybe Prelude.Text)
startSuiteRunResponse_suiteRunArn = Lens.lens (\StartSuiteRunResponse' {suiteRunArn} -> suiteRunArn) (\s@StartSuiteRunResponse' {} a -> s {suiteRunArn = a} :: StartSuiteRunResponse)

-- | Suite Run Id of the started suite run.
startSuiteRunResponse_suiteRunId :: Lens.Lens' StartSuiteRunResponse (Prelude.Maybe Prelude.Text)
startSuiteRunResponse_suiteRunId = Lens.lens (\StartSuiteRunResponse' {suiteRunId} -> suiteRunId) (\s@StartSuiteRunResponse' {} a -> s {suiteRunId = a} :: StartSuiteRunResponse)

-- | The response's http status code.
startSuiteRunResponse_httpStatus :: Lens.Lens' StartSuiteRunResponse Prelude.Int
startSuiteRunResponse_httpStatus = Lens.lens (\StartSuiteRunResponse' {httpStatus} -> httpStatus) (\s@StartSuiteRunResponse' {} a -> s {httpStatus = a} :: StartSuiteRunResponse)

instance Prelude.NFData StartSuiteRunResponse
