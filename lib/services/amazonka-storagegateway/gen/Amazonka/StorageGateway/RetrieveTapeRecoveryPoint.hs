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
-- Module      : Amazonka.StorageGateway.RetrieveTapeRecoveryPoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the recovery point for the specified virtual tape. This
-- operation is only supported in the tape gateway type.
--
-- A recovery point is a point in time view of a virtual tape at which all
-- the data on the tape is consistent. If your gateway crashes, virtual
-- tapes that have recovery points can be recovered to a new gateway.
--
-- The virtual tape can be retrieved to only one gateway. The retrieved
-- tape is read-only. The virtual tape can be retrieved to only a tape
-- gateway. There is no charge for retrieving recovery points.
module Amazonka.StorageGateway.RetrieveTapeRecoveryPoint
  ( -- * Creating a Request
    RetrieveTapeRecoveryPoint (..),
    newRetrieveTapeRecoveryPoint,

    -- * Request Lenses
    retrieveTapeRecoveryPoint_tapeARN,
    retrieveTapeRecoveryPoint_gatewayARN,

    -- * Destructuring the Response
    RetrieveTapeRecoveryPointResponse (..),
    newRetrieveTapeRecoveryPointResponse,

    -- * Response Lenses
    retrieveTapeRecoveryPointResponse_tapeARN,
    retrieveTapeRecoveryPointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | RetrieveTapeRecoveryPointInput
--
-- /See:/ 'newRetrieveTapeRecoveryPoint' smart constructor.
data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which you want to
    -- retrieve the recovery point.
    tapeARN :: Prelude.Text,
    gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetrieveTapeRecoveryPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'retrieveTapeRecoveryPoint_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape for which you want to
-- retrieve the recovery point.
--
-- 'gatewayARN', 'retrieveTapeRecoveryPoint_gatewayARN' - Undocumented member.
newRetrieveTapeRecoveryPoint ::
  -- | 'tapeARN'
  Prelude.Text ->
  -- | 'gatewayARN'
  Prelude.Text ->
  RetrieveTapeRecoveryPoint
newRetrieveTapeRecoveryPoint pTapeARN_ pGatewayARN_ =
  RetrieveTapeRecoveryPoint'
    { tapeARN = pTapeARN_,
      gatewayARN = pGatewayARN_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which you want to
-- retrieve the recovery point.
retrieveTapeRecoveryPoint_tapeARN :: Lens.Lens' RetrieveTapeRecoveryPoint Prelude.Text
retrieveTapeRecoveryPoint_tapeARN = Lens.lens (\RetrieveTapeRecoveryPoint' {tapeARN} -> tapeARN) (\s@RetrieveTapeRecoveryPoint' {} a -> s {tapeARN = a} :: RetrieveTapeRecoveryPoint)

-- | Undocumented member.
retrieveTapeRecoveryPoint_gatewayARN :: Lens.Lens' RetrieveTapeRecoveryPoint Prelude.Text
retrieveTapeRecoveryPoint_gatewayARN = Lens.lens (\RetrieveTapeRecoveryPoint' {gatewayARN} -> gatewayARN) (\s@RetrieveTapeRecoveryPoint' {} a -> s {gatewayARN = a} :: RetrieveTapeRecoveryPoint)

instance Core.AWSRequest RetrieveTapeRecoveryPoint where
  type
    AWSResponse RetrieveTapeRecoveryPoint =
      RetrieveTapeRecoveryPointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RetrieveTapeRecoveryPointResponse'
            Prelude.<$> (x Core..?> "TapeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RetrieveTapeRecoveryPoint where
  hashWithSalt _salt RetrieveTapeRecoveryPoint' {..} =
    _salt `Prelude.hashWithSalt` tapeARN
      `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData RetrieveTapeRecoveryPoint where
  rnf RetrieveTapeRecoveryPoint' {..} =
    Prelude.rnf tapeARN
      `Prelude.seq` Prelude.rnf gatewayARN

instance Core.ToHeaders RetrieveTapeRecoveryPoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.RetrieveTapeRecoveryPoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RetrieveTapeRecoveryPoint where
  toJSON RetrieveTapeRecoveryPoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TapeARN" Core..= tapeARN),
            Prelude.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.ToPath RetrieveTapeRecoveryPoint where
  toPath = Prelude.const "/"

instance Core.ToQuery RetrieveTapeRecoveryPoint where
  toQuery = Prelude.const Prelude.mempty

-- | RetrieveTapeRecoveryPointOutput
--
-- /See:/ 'newRetrieveTapeRecoveryPointResponse' smart constructor.
data RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which the
    -- recovery point was retrieved.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetrieveTapeRecoveryPointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'retrieveTapeRecoveryPointResponse_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape for which the
-- recovery point was retrieved.
--
-- 'httpStatus', 'retrieveTapeRecoveryPointResponse_httpStatus' - The response's http status code.
newRetrieveTapeRecoveryPointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RetrieveTapeRecoveryPointResponse
newRetrieveTapeRecoveryPointResponse pHttpStatus_ =
  RetrieveTapeRecoveryPointResponse'
    { tapeARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which the
-- recovery point was retrieved.
retrieveTapeRecoveryPointResponse_tapeARN :: Lens.Lens' RetrieveTapeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
retrieveTapeRecoveryPointResponse_tapeARN = Lens.lens (\RetrieveTapeRecoveryPointResponse' {tapeARN} -> tapeARN) (\s@RetrieveTapeRecoveryPointResponse' {} a -> s {tapeARN = a} :: RetrieveTapeRecoveryPointResponse)

-- | The response's http status code.
retrieveTapeRecoveryPointResponse_httpStatus :: Lens.Lens' RetrieveTapeRecoveryPointResponse Prelude.Int
retrieveTapeRecoveryPointResponse_httpStatus = Lens.lens (\RetrieveTapeRecoveryPointResponse' {httpStatus} -> httpStatus) (\s@RetrieveTapeRecoveryPointResponse' {} a -> s {httpStatus = a} :: RetrieveTapeRecoveryPointResponse)

instance
  Prelude.NFData
    RetrieveTapeRecoveryPointResponse
  where
  rnf RetrieveTapeRecoveryPointResponse' {..} =
    Prelude.rnf tapeARN
      `Prelude.seq` Prelude.rnf httpStatus
