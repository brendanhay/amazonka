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
-- Module      : Amazonka.MediaConnect.RemoveFlowOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an output from an existing flow. This request can be made only
-- on an output that does not have an entitlement associated with it. If
-- the output has an entitlement, you must revoke the entitlement instead.
-- When an entitlement is revoked from a flow, the service automatically
-- removes the associated output.
module Amazonka.MediaConnect.RemoveFlowOutput
  ( -- * Creating a Request
    RemoveFlowOutput (..),
    newRemoveFlowOutput,

    -- * Request Lenses
    removeFlowOutput_flowArn,
    removeFlowOutput_outputArn,

    -- * Destructuring the Response
    RemoveFlowOutputResponse (..),
    newRemoveFlowOutputResponse,

    -- * Response Lenses
    removeFlowOutputResponse_flowArn,
    removeFlowOutputResponse_outputArn,
    removeFlowOutputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveFlowOutput' smart constructor.
data RemoveFlowOutput = RemoveFlowOutput'
  { -- | The flow that you want to remove an output from.
    flowArn :: Prelude.Text,
    -- | The ARN of the output that you want to remove.
    outputArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveFlowOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'removeFlowOutput_flowArn' - The flow that you want to remove an output from.
--
-- 'outputArn', 'removeFlowOutput_outputArn' - The ARN of the output that you want to remove.
newRemoveFlowOutput ::
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'outputArn'
  Prelude.Text ->
  RemoveFlowOutput
newRemoveFlowOutput pFlowArn_ pOutputArn_ =
  RemoveFlowOutput'
    { flowArn = pFlowArn_,
      outputArn = pOutputArn_
    }

-- | The flow that you want to remove an output from.
removeFlowOutput_flowArn :: Lens.Lens' RemoveFlowOutput Prelude.Text
removeFlowOutput_flowArn = Lens.lens (\RemoveFlowOutput' {flowArn} -> flowArn) (\s@RemoveFlowOutput' {} a -> s {flowArn = a} :: RemoveFlowOutput)

-- | The ARN of the output that you want to remove.
removeFlowOutput_outputArn :: Lens.Lens' RemoveFlowOutput Prelude.Text
removeFlowOutput_outputArn = Lens.lens (\RemoveFlowOutput' {outputArn} -> outputArn) (\s@RemoveFlowOutput' {} a -> s {outputArn = a} :: RemoveFlowOutput)

instance Core.AWSRequest RemoveFlowOutput where
  type
    AWSResponse RemoveFlowOutput =
      RemoveFlowOutputResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveFlowOutputResponse'
            Prelude.<$> (x Data..?> "flowArn")
            Prelude.<*> (x Data..?> "outputArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveFlowOutput where
  hashWithSalt _salt RemoveFlowOutput' {..} =
    _salt `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` outputArn

instance Prelude.NFData RemoveFlowOutput where
  rnf RemoveFlowOutput' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf outputArn

instance Data.ToHeaders RemoveFlowOutput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RemoveFlowOutput where
  toPath RemoveFlowOutput' {..} =
    Prelude.mconcat
      [ "/v1/flows/",
        Data.toBS flowArn,
        "/outputs/",
        Data.toBS outputArn
      ]

instance Data.ToQuery RemoveFlowOutput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveFlowOutputResponse' smart constructor.
data RemoveFlowOutputResponse = RemoveFlowOutputResponse'
  { -- | The ARN of the flow that is associated with the output you removed.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the output that was removed.
    outputArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveFlowOutputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'removeFlowOutputResponse_flowArn' - The ARN of the flow that is associated with the output you removed.
--
-- 'outputArn', 'removeFlowOutputResponse_outputArn' - The ARN of the output that was removed.
--
-- 'httpStatus', 'removeFlowOutputResponse_httpStatus' - The response's http status code.
newRemoveFlowOutputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveFlowOutputResponse
newRemoveFlowOutputResponse pHttpStatus_ =
  RemoveFlowOutputResponse'
    { flowArn =
        Prelude.Nothing,
      outputArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the flow that is associated with the output you removed.
removeFlowOutputResponse_flowArn :: Lens.Lens' RemoveFlowOutputResponse (Prelude.Maybe Prelude.Text)
removeFlowOutputResponse_flowArn = Lens.lens (\RemoveFlowOutputResponse' {flowArn} -> flowArn) (\s@RemoveFlowOutputResponse' {} a -> s {flowArn = a} :: RemoveFlowOutputResponse)

-- | The ARN of the output that was removed.
removeFlowOutputResponse_outputArn :: Lens.Lens' RemoveFlowOutputResponse (Prelude.Maybe Prelude.Text)
removeFlowOutputResponse_outputArn = Lens.lens (\RemoveFlowOutputResponse' {outputArn} -> outputArn) (\s@RemoveFlowOutputResponse' {} a -> s {outputArn = a} :: RemoveFlowOutputResponse)

-- | The response's http status code.
removeFlowOutputResponse_httpStatus :: Lens.Lens' RemoveFlowOutputResponse Prelude.Int
removeFlowOutputResponse_httpStatus = Lens.lens (\RemoveFlowOutputResponse' {httpStatus} -> httpStatus) (\s@RemoveFlowOutputResponse' {} a -> s {httpStatus = a} :: RemoveFlowOutputResponse)

instance Prelude.NFData RemoveFlowOutputResponse where
  rnf RemoveFlowOutputResponse' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf outputArn
      `Prelude.seq` Prelude.rnf httpStatus
