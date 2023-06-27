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
-- Module      : Amazonka.MediaConnect.RemoveBridgeOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an output from a bridge.
module Amazonka.MediaConnect.RemoveBridgeOutput
  ( -- * Creating a Request
    RemoveBridgeOutput (..),
    newRemoveBridgeOutput,

    -- * Request Lenses
    removeBridgeOutput_outputName,
    removeBridgeOutput_bridgeArn,

    -- * Destructuring the Response
    RemoveBridgeOutputResponse (..),
    newRemoveBridgeOutputResponse,

    -- * Response Lenses
    removeBridgeOutputResponse_bridgeArn,
    removeBridgeOutputResponse_outputName,
    removeBridgeOutputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveBridgeOutput' smart constructor.
data RemoveBridgeOutput = RemoveBridgeOutput'
  { -- | The name of the bridge output that you want to remove.
    outputName :: Prelude.Text,
    -- | The ARN of the bridge that you want to update.
    bridgeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveBridgeOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputName', 'removeBridgeOutput_outputName' - The name of the bridge output that you want to remove.
--
-- 'bridgeArn', 'removeBridgeOutput_bridgeArn' - The ARN of the bridge that you want to update.
newRemoveBridgeOutput ::
  -- | 'outputName'
  Prelude.Text ->
  -- | 'bridgeArn'
  Prelude.Text ->
  RemoveBridgeOutput
newRemoveBridgeOutput pOutputName_ pBridgeArn_ =
  RemoveBridgeOutput'
    { outputName = pOutputName_,
      bridgeArn = pBridgeArn_
    }

-- | The name of the bridge output that you want to remove.
removeBridgeOutput_outputName :: Lens.Lens' RemoveBridgeOutput Prelude.Text
removeBridgeOutput_outputName = Lens.lens (\RemoveBridgeOutput' {outputName} -> outputName) (\s@RemoveBridgeOutput' {} a -> s {outputName = a} :: RemoveBridgeOutput)

-- | The ARN of the bridge that you want to update.
removeBridgeOutput_bridgeArn :: Lens.Lens' RemoveBridgeOutput Prelude.Text
removeBridgeOutput_bridgeArn = Lens.lens (\RemoveBridgeOutput' {bridgeArn} -> bridgeArn) (\s@RemoveBridgeOutput' {} a -> s {bridgeArn = a} :: RemoveBridgeOutput)

instance Core.AWSRequest RemoveBridgeOutput where
  type
    AWSResponse RemoveBridgeOutput =
      RemoveBridgeOutputResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveBridgeOutputResponse'
            Prelude.<$> (x Data..?> "bridgeArn")
            Prelude.<*> (x Data..?> "outputName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveBridgeOutput where
  hashWithSalt _salt RemoveBridgeOutput' {..} =
    _salt
      `Prelude.hashWithSalt` outputName
      `Prelude.hashWithSalt` bridgeArn

instance Prelude.NFData RemoveBridgeOutput where
  rnf RemoveBridgeOutput' {..} =
    Prelude.rnf outputName
      `Prelude.seq` Prelude.rnf bridgeArn

instance Data.ToHeaders RemoveBridgeOutput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RemoveBridgeOutput where
  toPath RemoveBridgeOutput' {..} =
    Prelude.mconcat
      [ "/v1/bridges/",
        Data.toBS bridgeArn,
        "/outputs/",
        Data.toBS outputName
      ]

instance Data.ToQuery RemoveBridgeOutput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveBridgeOutputResponse' smart constructor.
data RemoveBridgeOutputResponse = RemoveBridgeOutputResponse'
  { bridgeArn :: Prelude.Maybe Prelude.Text,
    outputName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveBridgeOutputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'removeBridgeOutputResponse_bridgeArn' - Undocumented member.
--
-- 'outputName', 'removeBridgeOutputResponse_outputName' - Undocumented member.
--
-- 'httpStatus', 'removeBridgeOutputResponse_httpStatus' - The response's http status code.
newRemoveBridgeOutputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveBridgeOutputResponse
newRemoveBridgeOutputResponse pHttpStatus_ =
  RemoveBridgeOutputResponse'
    { bridgeArn =
        Prelude.Nothing,
      outputName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
removeBridgeOutputResponse_bridgeArn :: Lens.Lens' RemoveBridgeOutputResponse (Prelude.Maybe Prelude.Text)
removeBridgeOutputResponse_bridgeArn = Lens.lens (\RemoveBridgeOutputResponse' {bridgeArn} -> bridgeArn) (\s@RemoveBridgeOutputResponse' {} a -> s {bridgeArn = a} :: RemoveBridgeOutputResponse)

-- | Undocumented member.
removeBridgeOutputResponse_outputName :: Lens.Lens' RemoveBridgeOutputResponse (Prelude.Maybe Prelude.Text)
removeBridgeOutputResponse_outputName = Lens.lens (\RemoveBridgeOutputResponse' {outputName} -> outputName) (\s@RemoveBridgeOutputResponse' {} a -> s {outputName = a} :: RemoveBridgeOutputResponse)

-- | The response's http status code.
removeBridgeOutputResponse_httpStatus :: Lens.Lens' RemoveBridgeOutputResponse Prelude.Int
removeBridgeOutputResponse_httpStatus = Lens.lens (\RemoveBridgeOutputResponse' {httpStatus} -> httpStatus) (\s@RemoveBridgeOutputResponse' {} a -> s {httpStatus = a} :: RemoveBridgeOutputResponse)

instance Prelude.NFData RemoveBridgeOutputResponse where
  rnf RemoveBridgeOutputResponse' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf outputName
      `Prelude.seq` Prelude.rnf httpStatus
