{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.StartWorkflowRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new run of the specified workflow.
module Network.AWS.Glue.StartWorkflowRun
  ( -- * Creating a Request
    StartWorkflowRun (..),
    newStartWorkflowRun,

    -- * Request Lenses
    startWorkflowRun_name,

    -- * Destructuring the Response
    StartWorkflowRunResponse (..),
    newStartWorkflowRunResponse,

    -- * Response Lenses
    startWorkflowRunResponse_runId,
    startWorkflowRunResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartWorkflowRun' smart constructor.
data StartWorkflowRun = StartWorkflowRun'
  { -- | The name of the workflow to start.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartWorkflowRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startWorkflowRun_name' - The name of the workflow to start.
newStartWorkflowRun ::
  -- | 'name'
  Prelude.Text ->
  StartWorkflowRun
newStartWorkflowRun pName_ =
  StartWorkflowRun' {name = pName_}

-- | The name of the workflow to start.
startWorkflowRun_name :: Lens.Lens' StartWorkflowRun Prelude.Text
startWorkflowRun_name = Lens.lens (\StartWorkflowRun' {name} -> name) (\s@StartWorkflowRun' {} a -> s {name = a} :: StartWorkflowRun)

instance Prelude.AWSRequest StartWorkflowRun where
  type Rs StartWorkflowRun = StartWorkflowRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartWorkflowRunResponse'
            Prelude.<$> (x Prelude..?> "RunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartWorkflowRun

instance Prelude.NFData StartWorkflowRun

instance Prelude.ToHeaders StartWorkflowRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.StartWorkflowRun" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartWorkflowRun where
  toJSON StartWorkflowRun' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath StartWorkflowRun where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartWorkflowRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartWorkflowRunResponse' smart constructor.
data StartWorkflowRunResponse = StartWorkflowRunResponse'
  { -- | An Id for the new run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData StartWorkflowRunResponse
