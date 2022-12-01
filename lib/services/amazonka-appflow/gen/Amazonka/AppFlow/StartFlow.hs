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
-- Module      : Amazonka.AppFlow.StartFlow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates an existing flow. For on-demand flows, this operation runs the
-- flow immediately. For schedule and event-triggered flows, this operation
-- activates the flow.
module Amazonka.AppFlow.StartFlow
  ( -- * Creating a Request
    StartFlow (..),
    newStartFlow,

    -- * Request Lenses
    startFlow_flowName,

    -- * Destructuring the Response
    StartFlowResponse (..),
    newStartFlowResponse,

    -- * Response Lenses
    startFlowResponse_executionId,
    startFlowResponse_flowStatus,
    startFlowResponse_flowArn,
    startFlowResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFlow' smart constructor.
data StartFlow = StartFlow'
  { -- | The specified name of the flow. Spaces are not allowed. Use underscores
    -- (_) or hyphens (-) only.
    flowName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowName', 'startFlow_flowName' - The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
newStartFlow ::
  -- | 'flowName'
  Prelude.Text ->
  StartFlow
newStartFlow pFlowName_ =
  StartFlow' {flowName = pFlowName_}

-- | The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
startFlow_flowName :: Lens.Lens' StartFlow Prelude.Text
startFlow_flowName = Lens.lens (\StartFlow' {flowName} -> flowName) (\s@StartFlow' {} a -> s {flowName = a} :: StartFlow)

instance Core.AWSRequest StartFlow where
  type AWSResponse StartFlow = StartFlowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFlowResponse'
            Prelude.<$> (x Core..?> "executionId")
            Prelude.<*> (x Core..?> "flowStatus")
            Prelude.<*> (x Core..?> "flowArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFlow where
  hashWithSalt _salt StartFlow' {..} =
    _salt `Prelude.hashWithSalt` flowName

instance Prelude.NFData StartFlow where
  rnf StartFlow' {..} = Prelude.rnf flowName

instance Core.ToHeaders StartFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartFlow where
  toJSON StartFlow' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("flowName" Core..= flowName)]
      )

instance Core.ToPath StartFlow where
  toPath = Prelude.const "/start-flow"

instance Core.ToQuery StartFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFlowResponse' smart constructor.
data StartFlowResponse = StartFlowResponse'
  { -- | Returns the internal execution ID of an on-demand flow when the flow is
    -- started. For scheduled or event-triggered flows, this value is null.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | Indicates the current status of the flow.
    flowStatus :: Prelude.Maybe FlowStatus,
    -- | The flow\'s Amazon Resource Name (ARN).
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionId', 'startFlowResponse_executionId' - Returns the internal execution ID of an on-demand flow when the flow is
-- started. For scheduled or event-triggered flows, this value is null.
--
-- 'flowStatus', 'startFlowResponse_flowStatus' - Indicates the current status of the flow.
--
-- 'flowArn', 'startFlowResponse_flowArn' - The flow\'s Amazon Resource Name (ARN).
--
-- 'httpStatus', 'startFlowResponse_httpStatus' - The response's http status code.
newStartFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartFlowResponse
newStartFlowResponse pHttpStatus_ =
  StartFlowResponse'
    { executionId = Prelude.Nothing,
      flowStatus = Prelude.Nothing,
      flowArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the internal execution ID of an on-demand flow when the flow is
-- started. For scheduled or event-triggered flows, this value is null.
startFlowResponse_executionId :: Lens.Lens' StartFlowResponse (Prelude.Maybe Prelude.Text)
startFlowResponse_executionId = Lens.lens (\StartFlowResponse' {executionId} -> executionId) (\s@StartFlowResponse' {} a -> s {executionId = a} :: StartFlowResponse)

-- | Indicates the current status of the flow.
startFlowResponse_flowStatus :: Lens.Lens' StartFlowResponse (Prelude.Maybe FlowStatus)
startFlowResponse_flowStatus = Lens.lens (\StartFlowResponse' {flowStatus} -> flowStatus) (\s@StartFlowResponse' {} a -> s {flowStatus = a} :: StartFlowResponse)

-- | The flow\'s Amazon Resource Name (ARN).
startFlowResponse_flowArn :: Lens.Lens' StartFlowResponse (Prelude.Maybe Prelude.Text)
startFlowResponse_flowArn = Lens.lens (\StartFlowResponse' {flowArn} -> flowArn) (\s@StartFlowResponse' {} a -> s {flowArn = a} :: StartFlowResponse)

-- | The response's http status code.
startFlowResponse_httpStatus :: Lens.Lens' StartFlowResponse Prelude.Int
startFlowResponse_httpStatus = Lens.lens (\StartFlowResponse' {httpStatus} -> httpStatus) (\s@StartFlowResponse' {} a -> s {httpStatus = a} :: StartFlowResponse)

instance Prelude.NFData StartFlowResponse where
  rnf StartFlowResponse' {..} =
    Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf flowStatus
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf httpStatus
