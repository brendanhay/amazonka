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
-- Module      : Amazonka.Glue.StartWorkflowRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new run of the specified workflow.
module Amazonka.Glue.StartWorkflowRun
  ( -- * Creating a Request
    StartWorkflowRun (..),
    newStartWorkflowRun,

    -- * Request Lenses
    startWorkflowRun_runProperties,
    startWorkflowRun_name,

    -- * Destructuring the Response
    StartWorkflowRunResponse (..),
    newStartWorkflowRunResponse,

    -- * Response Lenses
    startWorkflowRunResponse_runId,
    startWorkflowRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartWorkflowRun' smart constructor.
data StartWorkflowRun = StartWorkflowRun'
  { -- | The workflow run properties for the new workflow run.
    runProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the workflow to start.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartWorkflowRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runProperties', 'startWorkflowRun_runProperties' - The workflow run properties for the new workflow run.
--
-- 'name', 'startWorkflowRun_name' - The name of the workflow to start.
newStartWorkflowRun ::
  -- | 'name'
  Prelude.Text ->
  StartWorkflowRun
newStartWorkflowRun pName_ =
  StartWorkflowRun'
    { runProperties = Prelude.Nothing,
      name = pName_
    }

-- | The workflow run properties for the new workflow run.
startWorkflowRun_runProperties :: Lens.Lens' StartWorkflowRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startWorkflowRun_runProperties = Lens.lens (\StartWorkflowRun' {runProperties} -> runProperties) (\s@StartWorkflowRun' {} a -> s {runProperties = a} :: StartWorkflowRun) Prelude.. Lens.mapping Lens.coerced

-- | The name of the workflow to start.
startWorkflowRun_name :: Lens.Lens' StartWorkflowRun Prelude.Text
startWorkflowRun_name = Lens.lens (\StartWorkflowRun' {name} -> name) (\s@StartWorkflowRun' {} a -> s {name = a} :: StartWorkflowRun)

instance Core.AWSRequest StartWorkflowRun where
  type
    AWSResponse StartWorkflowRun =
      StartWorkflowRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartWorkflowRunResponse'
            Prelude.<$> (x Data..?> "RunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartWorkflowRun where
  hashWithSalt _salt StartWorkflowRun' {..} =
    _salt `Prelude.hashWithSalt` runProperties
      `Prelude.hashWithSalt` name

instance Prelude.NFData StartWorkflowRun where
  rnf StartWorkflowRun' {..} =
    Prelude.rnf runProperties
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders StartWorkflowRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.StartWorkflowRun" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartWorkflowRun where
  toJSON StartWorkflowRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RunProperties" Data..=) Prelude.<$> runProperties,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath StartWorkflowRun where
  toPath = Prelude.const "/"

instance Data.ToQuery StartWorkflowRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartWorkflowRunResponse' smart constructor.
data StartWorkflowRunResponse = StartWorkflowRunResponse'
  { -- | An Id for the new run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartWorkflowRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'startWorkflowRunResponse_runId' - An Id for the new run.
--
-- 'httpStatus', 'startWorkflowRunResponse_httpStatus' - The response's http status code.
newStartWorkflowRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartWorkflowRunResponse
newStartWorkflowRunResponse pHttpStatus_ =
  StartWorkflowRunResponse'
    { runId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Id for the new run.
startWorkflowRunResponse_runId :: Lens.Lens' StartWorkflowRunResponse (Prelude.Maybe Prelude.Text)
startWorkflowRunResponse_runId = Lens.lens (\StartWorkflowRunResponse' {runId} -> runId) (\s@StartWorkflowRunResponse' {} a -> s {runId = a} :: StartWorkflowRunResponse)

-- | The response's http status code.
startWorkflowRunResponse_httpStatus :: Lens.Lens' StartWorkflowRunResponse Prelude.Int
startWorkflowRunResponse_httpStatus = Lens.lens (\StartWorkflowRunResponse' {httpStatus} -> httpStatus) (\s@StartWorkflowRunResponse' {} a -> s {httpStatus = a} :: StartWorkflowRunResponse)

instance Prelude.NFData StartWorkflowRunResponse where
  rnf StartWorkflowRunResponse' {..} =
    Prelude.rnf runId
      `Prelude.seq` Prelude.rnf httpStatus
