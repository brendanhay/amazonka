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
-- Module      : Amazonka.Glue.StartBlueprintRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new run of the specified blueprint.
module Amazonka.Glue.StartBlueprintRun
  ( -- * Creating a Request
    StartBlueprintRun (..),
    newStartBlueprintRun,

    -- * Request Lenses
    startBlueprintRun_parameters,
    startBlueprintRun_blueprintName,
    startBlueprintRun_roleArn,

    -- * Destructuring the Response
    StartBlueprintRunResponse (..),
    newStartBlueprintRunResponse,

    -- * Response Lenses
    startBlueprintRunResponse_runId,
    startBlueprintRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartBlueprintRun' smart constructor.
data StartBlueprintRun = StartBlueprintRun'
  { -- | Specifies the parameters as a @BlueprintParameters@ object.
    parameters :: Prelude.Maybe Prelude.Text,
    -- | The name of the blueprint.
    blueprintName :: Prelude.Text,
    -- | Specifies the IAM role used to create the workflow.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBlueprintRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'startBlueprintRun_parameters' - Specifies the parameters as a @BlueprintParameters@ object.
--
-- 'blueprintName', 'startBlueprintRun_blueprintName' - The name of the blueprint.
--
-- 'roleArn', 'startBlueprintRun_roleArn' - Specifies the IAM role used to create the workflow.
newStartBlueprintRun ::
  -- | 'blueprintName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  StartBlueprintRun
newStartBlueprintRun pBlueprintName_ pRoleArn_ =
  StartBlueprintRun'
    { parameters = Prelude.Nothing,
      blueprintName = pBlueprintName_,
      roleArn = pRoleArn_
    }

-- | Specifies the parameters as a @BlueprintParameters@ object.
startBlueprintRun_parameters :: Lens.Lens' StartBlueprintRun (Prelude.Maybe Prelude.Text)
startBlueprintRun_parameters = Lens.lens (\StartBlueprintRun' {parameters} -> parameters) (\s@StartBlueprintRun' {} a -> s {parameters = a} :: StartBlueprintRun)

-- | The name of the blueprint.
startBlueprintRun_blueprintName :: Lens.Lens' StartBlueprintRun Prelude.Text
startBlueprintRun_blueprintName = Lens.lens (\StartBlueprintRun' {blueprintName} -> blueprintName) (\s@StartBlueprintRun' {} a -> s {blueprintName = a} :: StartBlueprintRun)

-- | Specifies the IAM role used to create the workflow.
startBlueprintRun_roleArn :: Lens.Lens' StartBlueprintRun Prelude.Text
startBlueprintRun_roleArn = Lens.lens (\StartBlueprintRun' {roleArn} -> roleArn) (\s@StartBlueprintRun' {} a -> s {roleArn = a} :: StartBlueprintRun)

instance Core.AWSRequest StartBlueprintRun where
  type
    AWSResponse StartBlueprintRun =
      StartBlueprintRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBlueprintRunResponse'
            Prelude.<$> (x Data..?> "RunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartBlueprintRun where
  hashWithSalt _salt StartBlueprintRun' {..} =
    _salt
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` blueprintName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData StartBlueprintRun where
  rnf StartBlueprintRun' {..} =
    Prelude.rnf parameters `Prelude.seq`
      Prelude.rnf blueprintName `Prelude.seq`
        Prelude.rnf roleArn

instance Data.ToHeaders StartBlueprintRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.StartBlueprintRun" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartBlueprintRun where
  toJSON StartBlueprintRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("BlueprintName" Data..= blueprintName),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath StartBlueprintRun where
  toPath = Prelude.const "/"

instance Data.ToQuery StartBlueprintRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartBlueprintRunResponse' smart constructor.
data StartBlueprintRunResponse = StartBlueprintRunResponse'
  { -- | The run ID for this blueprint run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBlueprintRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'startBlueprintRunResponse_runId' - The run ID for this blueprint run.
--
-- 'httpStatus', 'startBlueprintRunResponse_httpStatus' - The response's http status code.
newStartBlueprintRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartBlueprintRunResponse
newStartBlueprintRunResponse pHttpStatus_ =
  StartBlueprintRunResponse'
    { runId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The run ID for this blueprint run.
startBlueprintRunResponse_runId :: Lens.Lens' StartBlueprintRunResponse (Prelude.Maybe Prelude.Text)
startBlueprintRunResponse_runId = Lens.lens (\StartBlueprintRunResponse' {runId} -> runId) (\s@StartBlueprintRunResponse' {} a -> s {runId = a} :: StartBlueprintRunResponse)

-- | The response's http status code.
startBlueprintRunResponse_httpStatus :: Lens.Lens' StartBlueprintRunResponse Prelude.Int
startBlueprintRunResponse_httpStatus = Lens.lens (\StartBlueprintRunResponse' {httpStatus} -> httpStatus) (\s@StartBlueprintRunResponse' {} a -> s {httpStatus = a} :: StartBlueprintRunResponse)

instance Prelude.NFData StartBlueprintRunResponse where
  rnf StartBlueprintRunResponse' {..} =
    Prelude.rnf runId `Prelude.seq`
      Prelude.rnf httpStatus
