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
-- Module      : Amazonka.MediaConnect.AddBridgeOutputs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds outputs to an existing bridge.
module Amazonka.MediaConnect.AddBridgeOutputs
  ( -- * Creating a Request
    AddBridgeOutputs (..),
    newAddBridgeOutputs,

    -- * Request Lenses
    addBridgeOutputs_bridgeArn,
    addBridgeOutputs_outputs,

    -- * Destructuring the Response
    AddBridgeOutputsResponse (..),
    newAddBridgeOutputsResponse,

    -- * Response Lenses
    addBridgeOutputsResponse_bridgeArn,
    addBridgeOutputsResponse_outputs,
    addBridgeOutputsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to add outputs to the specified bridge.
--
-- /See:/ 'newAddBridgeOutputs' smart constructor.
data AddBridgeOutputs = AddBridgeOutputs'
  { -- | The ARN of the bridge that you want to update.
    bridgeArn :: Prelude.Text,
    -- | The outputs that you want to add to this bridge.
    outputs :: [AddBridgeOutputRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddBridgeOutputs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'addBridgeOutputs_bridgeArn' - The ARN of the bridge that you want to update.
--
-- 'outputs', 'addBridgeOutputs_outputs' - The outputs that you want to add to this bridge.
newAddBridgeOutputs ::
  -- | 'bridgeArn'
  Prelude.Text ->
  AddBridgeOutputs
newAddBridgeOutputs pBridgeArn_ =
  AddBridgeOutputs'
    { bridgeArn = pBridgeArn_,
      outputs = Prelude.mempty
    }

-- | The ARN of the bridge that you want to update.
addBridgeOutputs_bridgeArn :: Lens.Lens' AddBridgeOutputs Prelude.Text
addBridgeOutputs_bridgeArn = Lens.lens (\AddBridgeOutputs' {bridgeArn} -> bridgeArn) (\s@AddBridgeOutputs' {} a -> s {bridgeArn = a} :: AddBridgeOutputs)

-- | The outputs that you want to add to this bridge.
addBridgeOutputs_outputs :: Lens.Lens' AddBridgeOutputs [AddBridgeOutputRequest]
addBridgeOutputs_outputs = Lens.lens (\AddBridgeOutputs' {outputs} -> outputs) (\s@AddBridgeOutputs' {} a -> s {outputs = a} :: AddBridgeOutputs) Prelude.. Lens.coerced

instance Core.AWSRequest AddBridgeOutputs where
  type
    AWSResponse AddBridgeOutputs =
      AddBridgeOutputsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddBridgeOutputsResponse'
            Prelude.<$> (x Data..?> "bridgeArn")
            Prelude.<*> (x Data..?> "outputs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddBridgeOutputs where
  hashWithSalt _salt AddBridgeOutputs' {..} =
    _salt
      `Prelude.hashWithSalt` bridgeArn
      `Prelude.hashWithSalt` outputs

instance Prelude.NFData AddBridgeOutputs where
  rnf AddBridgeOutputs' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf outputs

instance Data.ToHeaders AddBridgeOutputs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddBridgeOutputs where
  toJSON AddBridgeOutputs' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("outputs" Data..= outputs)]
      )

instance Data.ToPath AddBridgeOutputs where
  toPath AddBridgeOutputs' {..} =
    Prelude.mconcat
      ["/v1/bridges/", Data.toBS bridgeArn, "/outputs"]

instance Data.ToQuery AddBridgeOutputs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddBridgeOutputsResponse' smart constructor.
data AddBridgeOutputsResponse = AddBridgeOutputsResponse'
  { -- | The Amazon Resource Number (ARN) of the bridge.
    bridgeArn :: Prelude.Maybe Prelude.Text,
    -- | The outputs that you added to this bridge.
    outputs :: Prelude.Maybe [BridgeOutput],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddBridgeOutputsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'addBridgeOutputsResponse_bridgeArn' - The Amazon Resource Number (ARN) of the bridge.
--
-- 'outputs', 'addBridgeOutputsResponse_outputs' - The outputs that you added to this bridge.
--
-- 'httpStatus', 'addBridgeOutputsResponse_httpStatus' - The response's http status code.
newAddBridgeOutputsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddBridgeOutputsResponse
newAddBridgeOutputsResponse pHttpStatus_ =
  AddBridgeOutputsResponse'
    { bridgeArn =
        Prelude.Nothing,
      outputs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the bridge.
addBridgeOutputsResponse_bridgeArn :: Lens.Lens' AddBridgeOutputsResponse (Prelude.Maybe Prelude.Text)
addBridgeOutputsResponse_bridgeArn = Lens.lens (\AddBridgeOutputsResponse' {bridgeArn} -> bridgeArn) (\s@AddBridgeOutputsResponse' {} a -> s {bridgeArn = a} :: AddBridgeOutputsResponse)

-- | The outputs that you added to this bridge.
addBridgeOutputsResponse_outputs :: Lens.Lens' AddBridgeOutputsResponse (Prelude.Maybe [BridgeOutput])
addBridgeOutputsResponse_outputs = Lens.lens (\AddBridgeOutputsResponse' {outputs} -> outputs) (\s@AddBridgeOutputsResponse' {} a -> s {outputs = a} :: AddBridgeOutputsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addBridgeOutputsResponse_httpStatus :: Lens.Lens' AddBridgeOutputsResponse Prelude.Int
addBridgeOutputsResponse_httpStatus = Lens.lens (\AddBridgeOutputsResponse' {httpStatus} -> httpStatus) (\s@AddBridgeOutputsResponse' {} a -> s {httpStatus = a} :: AddBridgeOutputsResponse)

instance Prelude.NFData AddBridgeOutputsResponse where
  rnf AddBridgeOutputsResponse' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf httpStatus
