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
-- Module      : Amazonka.MediaConnect.StopFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a flow.
module Amazonka.MediaConnect.StopFlow
  ( -- * Creating a Request
    StopFlow (..),
    newStopFlow,

    -- * Request Lenses
    stopFlow_flowArn,

    -- * Destructuring the Response
    StopFlowResponse (..),
    newStopFlowResponse,

    -- * Response Lenses
    stopFlowResponse_flowArn,
    stopFlowResponse_status,
    stopFlowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopFlow' smart constructor.
data StopFlow = StopFlow'
  { -- | The ARN of the flow that you want to stop.
    flowArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'stopFlow_flowArn' - The ARN of the flow that you want to stop.
newStopFlow ::
  -- | 'flowArn'
  Prelude.Text ->
  StopFlow
newStopFlow pFlowArn_ =
  StopFlow' {flowArn = pFlowArn_}

-- | The ARN of the flow that you want to stop.
stopFlow_flowArn :: Lens.Lens' StopFlow Prelude.Text
stopFlow_flowArn = Lens.lens (\StopFlow' {flowArn} -> flowArn) (\s@StopFlow' {} a -> s {flowArn = a} :: StopFlow)

instance Core.AWSRequest StopFlow where
  type AWSResponse StopFlow = StopFlowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopFlowResponse'
            Prelude.<$> (x Data..?> "flowArn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopFlow where
  hashWithSalt _salt StopFlow' {..} =
    _salt `Prelude.hashWithSalt` flowArn

instance Prelude.NFData StopFlow where
  rnf StopFlow' {..} = Prelude.rnf flowArn

instance Data.ToHeaders StopFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopFlow where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopFlow where
  toPath StopFlow' {..} =
    Prelude.mconcat
      ["/v1/flows/stop/", Data.toBS flowArn]

instance Data.ToQuery StopFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopFlowResponse' smart constructor.
data StopFlowResponse = StopFlowResponse'
  { -- | The ARN of the flow that you stopped.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the flow when the StopFlow process begins.
    status :: Prelude.Maybe Status,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'stopFlowResponse_flowArn' - The ARN of the flow that you stopped.
--
-- 'status', 'stopFlowResponse_status' - The status of the flow when the StopFlow process begins.
--
-- 'httpStatus', 'stopFlowResponse_httpStatus' - The response's http status code.
newStopFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopFlowResponse
newStopFlowResponse pHttpStatus_ =
  StopFlowResponse'
    { flowArn = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the flow that you stopped.
stopFlowResponse_flowArn :: Lens.Lens' StopFlowResponse (Prelude.Maybe Prelude.Text)
stopFlowResponse_flowArn = Lens.lens (\StopFlowResponse' {flowArn} -> flowArn) (\s@StopFlowResponse' {} a -> s {flowArn = a} :: StopFlowResponse)

-- | The status of the flow when the StopFlow process begins.
stopFlowResponse_status :: Lens.Lens' StopFlowResponse (Prelude.Maybe Status)
stopFlowResponse_status = Lens.lens (\StopFlowResponse' {status} -> status) (\s@StopFlowResponse' {} a -> s {status = a} :: StopFlowResponse)

-- | The response's http status code.
stopFlowResponse_httpStatus :: Lens.Lens' StopFlowResponse Prelude.Int
stopFlowResponse_httpStatus = Lens.lens (\StopFlowResponse' {httpStatus} -> httpStatus) (\s@StopFlowResponse' {} a -> s {httpStatus = a} :: StopFlowResponse)

instance Prelude.NFData StopFlowResponse where
  rnf StopFlowResponse' {..} =
    Prelude.rnf flowArn `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf httpStatus
