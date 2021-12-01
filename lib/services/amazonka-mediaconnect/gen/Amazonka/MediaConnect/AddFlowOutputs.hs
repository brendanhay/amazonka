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
-- Module      : Amazonka.MediaConnect.AddFlowOutputs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds outputs to an existing flow. You can create up to 50 outputs per
-- flow.
module Amazonka.MediaConnect.AddFlowOutputs
  ( -- * Creating a Request
    AddFlowOutputs (..),
    newAddFlowOutputs,

    -- * Request Lenses
    addFlowOutputs_flowArn,
    addFlowOutputs_outputs,

    -- * Destructuring the Response
    AddFlowOutputsResponse (..),
    newAddFlowOutputsResponse,

    -- * Response Lenses
    addFlowOutputsResponse_flowArn,
    addFlowOutputsResponse_outputs,
    addFlowOutputsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to add outputs to the specified flow.
--
-- /See:/ 'newAddFlowOutputs' smart constructor.
data AddFlowOutputs = AddFlowOutputs'
  { -- | The flow that you want to add outputs to.
    flowArn :: Prelude.Text,
    -- | A list of outputs that you want to add.
    outputs :: [AddOutputRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddFlowOutputs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'addFlowOutputs_flowArn' - The flow that you want to add outputs to.
--
-- 'outputs', 'addFlowOutputs_outputs' - A list of outputs that you want to add.
newAddFlowOutputs ::
  -- | 'flowArn'
  Prelude.Text ->
  AddFlowOutputs
newAddFlowOutputs pFlowArn_ =
  AddFlowOutputs'
    { flowArn = pFlowArn_,
      outputs = Prelude.mempty
    }

-- | The flow that you want to add outputs to.
addFlowOutputs_flowArn :: Lens.Lens' AddFlowOutputs Prelude.Text
addFlowOutputs_flowArn = Lens.lens (\AddFlowOutputs' {flowArn} -> flowArn) (\s@AddFlowOutputs' {} a -> s {flowArn = a} :: AddFlowOutputs)

-- | A list of outputs that you want to add.
addFlowOutputs_outputs :: Lens.Lens' AddFlowOutputs [AddOutputRequest]
addFlowOutputs_outputs = Lens.lens (\AddFlowOutputs' {outputs} -> outputs) (\s@AddFlowOutputs' {} a -> s {outputs = a} :: AddFlowOutputs) Prelude.. Lens.coerced

instance Core.AWSRequest AddFlowOutputs where
  type
    AWSResponse AddFlowOutputs =
      AddFlowOutputsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddFlowOutputsResponse'
            Prelude.<$> (x Core..?> "flowArn")
            Prelude.<*> (x Core..?> "outputs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddFlowOutputs where
  hashWithSalt salt' AddFlowOutputs' {..} =
    salt' `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` flowArn

instance Prelude.NFData AddFlowOutputs where
  rnf AddFlowOutputs' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf outputs

instance Core.ToHeaders AddFlowOutputs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AddFlowOutputs where
  toJSON AddFlowOutputs' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("outputs" Core..= outputs)]
      )

instance Core.ToPath AddFlowOutputs where
  toPath AddFlowOutputs' {..} =
    Prelude.mconcat
      ["/v1/flows/", Core.toBS flowArn, "/outputs"]

instance Core.ToQuery AddFlowOutputs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddFlowOutputsResponse' smart constructor.
data AddFlowOutputsResponse = AddFlowOutputsResponse'
  { -- | The ARN of the flow that these outputs were added to.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The details of the newly added outputs.
    outputs :: Prelude.Maybe [Output],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddFlowOutputsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'addFlowOutputsResponse_flowArn' - The ARN of the flow that these outputs were added to.
--
-- 'outputs', 'addFlowOutputsResponse_outputs' - The details of the newly added outputs.
--
-- 'httpStatus', 'addFlowOutputsResponse_httpStatus' - The response's http status code.
newAddFlowOutputsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddFlowOutputsResponse
newAddFlowOutputsResponse pHttpStatus_ =
  AddFlowOutputsResponse'
    { flowArn = Prelude.Nothing,
      outputs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the flow that these outputs were added to.
addFlowOutputsResponse_flowArn :: Lens.Lens' AddFlowOutputsResponse (Prelude.Maybe Prelude.Text)
addFlowOutputsResponse_flowArn = Lens.lens (\AddFlowOutputsResponse' {flowArn} -> flowArn) (\s@AddFlowOutputsResponse' {} a -> s {flowArn = a} :: AddFlowOutputsResponse)

-- | The details of the newly added outputs.
addFlowOutputsResponse_outputs :: Lens.Lens' AddFlowOutputsResponse (Prelude.Maybe [Output])
addFlowOutputsResponse_outputs = Lens.lens (\AddFlowOutputsResponse' {outputs} -> outputs) (\s@AddFlowOutputsResponse' {} a -> s {outputs = a} :: AddFlowOutputsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addFlowOutputsResponse_httpStatus :: Lens.Lens' AddFlowOutputsResponse Prelude.Int
addFlowOutputsResponse_httpStatus = Lens.lens (\AddFlowOutputsResponse' {httpStatus} -> httpStatus) (\s@AddFlowOutputsResponse' {} a -> s {httpStatus = a} :: AddFlowOutputsResponse)

instance Prelude.NFData AddFlowOutputsResponse where
  rnf AddFlowOutputsResponse' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf outputs
