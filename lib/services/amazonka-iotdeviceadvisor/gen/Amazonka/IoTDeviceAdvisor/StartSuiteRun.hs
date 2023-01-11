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
-- Module      : Amazonka.IoTDeviceAdvisor.StartSuiteRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a Device Advisor test suite run.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions StartSuiteRun>
-- action.
module Amazonka.IoTDeviceAdvisor.StartSuiteRun
  ( -- * Creating a Request
    StartSuiteRun (..),
    newStartSuiteRun,

    -- * Request Lenses
    startSuiteRun_suiteDefinitionVersion,
    startSuiteRun_tags,
    startSuiteRun_suiteDefinitionId,
    startSuiteRun_suiteRunConfiguration,

    -- * Destructuring the Response
    StartSuiteRunResponse (..),
    newStartSuiteRunResponse,

    -- * Response Lenses
    startSuiteRunResponse_createdAt,
    startSuiteRunResponse_endpoint,
    startSuiteRunResponse_suiteRunArn,
    startSuiteRunResponse_suiteRunId,
    startSuiteRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSuiteRun' smart constructor.
data StartSuiteRun = StartSuiteRun'
  { -- | Suite definition version of the test suite.
    suiteDefinitionVersion :: Prelude.Maybe Prelude.Text,
    -- | The tags to be attached to the suite run.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Suite definition ID of the test suite.
    suiteDefinitionId :: Prelude.Text,
    -- | Suite run configuration.
    suiteRunConfiguration :: SuiteRunConfiguration
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
-- 'suiteDefinitionVersion', 'startSuiteRun_suiteDefinitionVersion' - Suite definition version of the test suite.
--
-- 'tags', 'startSuiteRun_tags' - The tags to be attached to the suite run.
--
-- 'suiteDefinitionId', 'startSuiteRun_suiteDefinitionId' - Suite definition ID of the test suite.
--
-- 'suiteRunConfiguration', 'startSuiteRun_suiteRunConfiguration' - Suite run configuration.
newStartSuiteRun ::
  -- | 'suiteDefinitionId'
  Prelude.Text ->
  -- | 'suiteRunConfiguration'
  SuiteRunConfiguration ->
  StartSuiteRun
newStartSuiteRun
  pSuiteDefinitionId_
  pSuiteRunConfiguration_ =
    StartSuiteRun'
      { suiteDefinitionVersion =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        suiteDefinitionId = pSuiteDefinitionId_,
        suiteRunConfiguration = pSuiteRunConfiguration_
      }

-- | Suite definition version of the test suite.
startSuiteRun_suiteDefinitionVersion :: Lens.Lens' StartSuiteRun (Prelude.Maybe Prelude.Text)
startSuiteRun_suiteDefinitionVersion = Lens.lens (\StartSuiteRun' {suiteDefinitionVersion} -> suiteDefinitionVersion) (\s@StartSuiteRun' {} a -> s {suiteDefinitionVersion = a} :: StartSuiteRun)

-- | The tags to be attached to the suite run.
startSuiteRun_tags :: Lens.Lens' StartSuiteRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startSuiteRun_tags = Lens.lens (\StartSuiteRun' {tags} -> tags) (\s@StartSuiteRun' {} a -> s {tags = a} :: StartSuiteRun) Prelude.. Lens.mapping Lens.coerced

-- | Suite definition ID of the test suite.
startSuiteRun_suiteDefinitionId :: Lens.Lens' StartSuiteRun Prelude.Text
startSuiteRun_suiteDefinitionId = Lens.lens (\StartSuiteRun' {suiteDefinitionId} -> suiteDefinitionId) (\s@StartSuiteRun' {} a -> s {suiteDefinitionId = a} :: StartSuiteRun)

-- | Suite run configuration.
startSuiteRun_suiteRunConfiguration :: Lens.Lens' StartSuiteRun SuiteRunConfiguration
startSuiteRun_suiteRunConfiguration = Lens.lens (\StartSuiteRun' {suiteRunConfiguration} -> suiteRunConfiguration) (\s@StartSuiteRun' {} a -> s {suiteRunConfiguration = a} :: StartSuiteRun)

instance Core.AWSRequest StartSuiteRun where
  type
    AWSResponse StartSuiteRun =
      StartSuiteRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSuiteRunResponse'
            Prelude.<$> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "endpoint")
            Prelude.<*> (x Data..?> "suiteRunArn")
            Prelude.<*> (x Data..?> "suiteRunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSuiteRun where
  hashWithSalt _salt StartSuiteRun' {..} =
    _salt `Prelude.hashWithSalt` suiteDefinitionVersion
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` suiteDefinitionId
      `Prelude.hashWithSalt` suiteRunConfiguration

instance Prelude.NFData StartSuiteRun where
  rnf StartSuiteRun' {..} =
    Prelude.rnf suiteDefinitionVersion
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf suiteDefinitionId
      `Prelude.seq` Prelude.rnf suiteRunConfiguration

instance Data.ToHeaders StartSuiteRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSuiteRun where
  toJSON StartSuiteRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("suiteDefinitionVersion" Data..=)
              Prelude.<$> suiteDefinitionVersion,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "suiteRunConfiguration"
                  Data..= suiteRunConfiguration
              )
          ]
      )

instance Data.ToPath StartSuiteRun where
  toPath StartSuiteRun' {..} =
    Prelude.mconcat
      [ "/suiteDefinitions/",
        Data.toBS suiteDefinitionId,
        "/suiteRuns"
      ]

instance Data.ToQuery StartSuiteRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSuiteRunResponse' smart constructor.
data StartSuiteRunResponse = StartSuiteRunResponse'
  { -- | Starts a Device Advisor test suite run based on suite create time.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The response of an Device Advisor test endpoint.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the started suite run.
    suiteRunArn :: Prelude.Maybe Prelude.Text,
    -- | Suite Run ID of the started suite run.
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
-- 'createdAt', 'startSuiteRunResponse_createdAt' - Starts a Device Advisor test suite run based on suite create time.
--
-- 'endpoint', 'startSuiteRunResponse_endpoint' - The response of an Device Advisor test endpoint.
--
-- 'suiteRunArn', 'startSuiteRunResponse_suiteRunArn' - Amazon Resource Name (ARN) of the started suite run.
--
-- 'suiteRunId', 'startSuiteRunResponse_suiteRunId' - Suite Run ID of the started suite run.
--
-- 'httpStatus', 'startSuiteRunResponse_httpStatus' - The response's http status code.
newStartSuiteRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSuiteRunResponse
newStartSuiteRunResponse pHttpStatus_ =
  StartSuiteRunResponse'
    { createdAt = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      suiteRunArn = Prelude.Nothing,
      suiteRunId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Starts a Device Advisor test suite run based on suite create time.
startSuiteRunResponse_createdAt :: Lens.Lens' StartSuiteRunResponse (Prelude.Maybe Prelude.UTCTime)
startSuiteRunResponse_createdAt = Lens.lens (\StartSuiteRunResponse' {createdAt} -> createdAt) (\s@StartSuiteRunResponse' {} a -> s {createdAt = a} :: StartSuiteRunResponse) Prelude.. Lens.mapping Data._Time

-- | The response of an Device Advisor test endpoint.
startSuiteRunResponse_endpoint :: Lens.Lens' StartSuiteRunResponse (Prelude.Maybe Prelude.Text)
startSuiteRunResponse_endpoint = Lens.lens (\StartSuiteRunResponse' {endpoint} -> endpoint) (\s@StartSuiteRunResponse' {} a -> s {endpoint = a} :: StartSuiteRunResponse)

-- | Amazon Resource Name (ARN) of the started suite run.
startSuiteRunResponse_suiteRunArn :: Lens.Lens' StartSuiteRunResponse (Prelude.Maybe Prelude.Text)
startSuiteRunResponse_suiteRunArn = Lens.lens (\StartSuiteRunResponse' {suiteRunArn} -> suiteRunArn) (\s@StartSuiteRunResponse' {} a -> s {suiteRunArn = a} :: StartSuiteRunResponse)

-- | Suite Run ID of the started suite run.
startSuiteRunResponse_suiteRunId :: Lens.Lens' StartSuiteRunResponse (Prelude.Maybe Prelude.Text)
startSuiteRunResponse_suiteRunId = Lens.lens (\StartSuiteRunResponse' {suiteRunId} -> suiteRunId) (\s@StartSuiteRunResponse' {} a -> s {suiteRunId = a} :: StartSuiteRunResponse)

-- | The response's http status code.
startSuiteRunResponse_httpStatus :: Lens.Lens' StartSuiteRunResponse Prelude.Int
startSuiteRunResponse_httpStatus = Lens.lens (\StartSuiteRunResponse' {httpStatus} -> httpStatus) (\s@StartSuiteRunResponse' {} a -> s {httpStatus = a} :: StartSuiteRunResponse)

instance Prelude.NFData StartSuiteRunResponse where
  rnf StartSuiteRunResponse' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf suiteRunArn
      `Prelude.seq` Prelude.rnf suiteRunId
      `Prelude.seq` Prelude.rnf httpStatus
