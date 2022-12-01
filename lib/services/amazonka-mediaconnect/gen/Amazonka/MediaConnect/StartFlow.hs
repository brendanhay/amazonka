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
-- Module      : Amazonka.MediaConnect.StartFlow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a flow.
module Amazonka.MediaConnect.StartFlow
  ( -- * Creating a Request
    StartFlow (..),
    newStartFlow,

    -- * Request Lenses
    startFlow_flowArn,

    -- * Destructuring the Response
    StartFlowResponse (..),
    newStartFlowResponse,

    -- * Response Lenses
    startFlowResponse_status,
    startFlowResponse_flowArn,
    startFlowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFlow' smart constructor.
data StartFlow = StartFlow'
  { -- | The ARN of the flow that you want to start.
    flowArn :: Prelude.Text
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
-- 'flowArn', 'startFlow_flowArn' - The ARN of the flow that you want to start.
newStartFlow ::
  -- | 'flowArn'
  Prelude.Text ->
  StartFlow
newStartFlow pFlowArn_ =
  StartFlow' {flowArn = pFlowArn_}

-- | The ARN of the flow that you want to start.
startFlow_flowArn :: Lens.Lens' StartFlow Prelude.Text
startFlow_flowArn = Lens.lens (\StartFlow' {flowArn} -> flowArn) (\s@StartFlow' {} a -> s {flowArn = a} :: StartFlow)

instance Core.AWSRequest StartFlow where
  type AWSResponse StartFlow = StartFlowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFlowResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "flowArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFlow where
  hashWithSalt _salt StartFlow' {..} =
    _salt `Prelude.hashWithSalt` flowArn

instance Prelude.NFData StartFlow where
  rnf StartFlow' {..} = Prelude.rnf flowArn

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
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath StartFlow where
  toPath StartFlow' {..} =
    Prelude.mconcat
      ["/v1/flows/start/", Core.toBS flowArn]

instance Core.ToQuery StartFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFlowResponse' smart constructor.
data StartFlowResponse = StartFlowResponse'
  { -- | The status of the flow when the StartFlow process begins.
    status :: Prelude.Maybe Status,
    -- | The ARN of the flow that you started.
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
-- 'status', 'startFlowResponse_status' - The status of the flow when the StartFlow process begins.
--
-- 'flowArn', 'startFlowResponse_flowArn' - The ARN of the flow that you started.
--
-- 'httpStatus', 'startFlowResponse_httpStatus' - The response's http status code.
newStartFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartFlowResponse
newStartFlowResponse pHttpStatus_ =
  StartFlowResponse'
    { status = Prelude.Nothing,
      flowArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the flow when the StartFlow process begins.
startFlowResponse_status :: Lens.Lens' StartFlowResponse (Prelude.Maybe Status)
startFlowResponse_status = Lens.lens (\StartFlowResponse' {status} -> status) (\s@StartFlowResponse' {} a -> s {status = a} :: StartFlowResponse)

-- | The ARN of the flow that you started.
startFlowResponse_flowArn :: Lens.Lens' StartFlowResponse (Prelude.Maybe Prelude.Text)
startFlowResponse_flowArn = Lens.lens (\StartFlowResponse' {flowArn} -> flowArn) (\s@StartFlowResponse' {} a -> s {flowArn = a} :: StartFlowResponse)

-- | The response's http status code.
startFlowResponse_httpStatus :: Lens.Lens' StartFlowResponse Prelude.Int
startFlowResponse_httpStatus = Lens.lens (\StartFlowResponse' {httpStatus} -> httpStatus) (\s@StartFlowResponse' {} a -> s {httpStatus = a} :: StartFlowResponse)

instance Prelude.NFData StartFlowResponse where
  rnf StartFlowResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf httpStatus
