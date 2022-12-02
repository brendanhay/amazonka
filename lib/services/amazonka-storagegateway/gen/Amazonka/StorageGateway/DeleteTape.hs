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
-- Module      : Amazonka.StorageGateway.DeleteTape
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape. This operation is only supported in
-- the tape gateway type.
module Amazonka.StorageGateway.DeleteTape
  ( -- * Creating a Request
    DeleteTape (..),
    newDeleteTape,

    -- * Request Lenses
    deleteTape_bypassGovernanceRetention,
    deleteTape_gatewayARN,
    deleteTape_tapeARN,

    -- * Destructuring the Response
    DeleteTapeResponse (..),
    newDeleteTapeResponse,

    -- * Response Lenses
    deleteTapeResponse_tapeARN,
    deleteTapeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | DeleteTapeInput
--
-- /See:/ 'newDeleteTape' smart constructor.
data DeleteTape = DeleteTape'
  { -- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool
    -- with tape retention lock. Only archived tapes with tape retention lock
    -- set to @governance@ can be deleted. Archived tapes with tape retention
    -- lock set to @compliance@ can\'t be deleted.
    bypassGovernanceRetention :: Prelude.Maybe Prelude.Bool,
    -- | The unique Amazon Resource Name (ARN) of the gateway that the virtual
    -- tape to delete is associated with. Use the ListGateways operation to
    -- return a list of gateways for your account and Amazon Web Services
    -- Region.
    gatewayARN :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape to delete.
    tapeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTape' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bypassGovernanceRetention', 'deleteTape_bypassGovernanceRetention' - Set to @TRUE@ to delete an archived tape that belongs to a custom pool
-- with tape retention lock. Only archived tapes with tape retention lock
-- set to @governance@ can be deleted. Archived tapes with tape retention
-- lock set to @compliance@ can\'t be deleted.
--
-- 'gatewayARN', 'deleteTape_gatewayARN' - The unique Amazon Resource Name (ARN) of the gateway that the virtual
-- tape to delete is associated with. Use the ListGateways operation to
-- return a list of gateways for your account and Amazon Web Services
-- Region.
--
-- 'tapeARN', 'deleteTape_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape to delete.
newDeleteTape ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'tapeARN'
  Prelude.Text ->
  DeleteTape
newDeleteTape pGatewayARN_ pTapeARN_ =
  DeleteTape'
    { bypassGovernanceRetention =
        Prelude.Nothing,
      gatewayARN = pGatewayARN_,
      tapeARN = pTapeARN_
    }

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool
-- with tape retention lock. Only archived tapes with tape retention lock
-- set to @governance@ can be deleted. Archived tapes with tape retention
-- lock set to @compliance@ can\'t be deleted.
deleteTape_bypassGovernanceRetention :: Lens.Lens' DeleteTape (Prelude.Maybe Prelude.Bool)
deleteTape_bypassGovernanceRetention = Lens.lens (\DeleteTape' {bypassGovernanceRetention} -> bypassGovernanceRetention) (\s@DeleteTape' {} a -> s {bypassGovernanceRetention = a} :: DeleteTape)

-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual
-- tape to delete is associated with. Use the ListGateways operation to
-- return a list of gateways for your account and Amazon Web Services
-- Region.
deleteTape_gatewayARN :: Lens.Lens' DeleteTape Prelude.Text
deleteTape_gatewayARN = Lens.lens (\DeleteTape' {gatewayARN} -> gatewayARN) (\s@DeleteTape' {} a -> s {gatewayARN = a} :: DeleteTape)

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
deleteTape_tapeARN :: Lens.Lens' DeleteTape Prelude.Text
deleteTape_tapeARN = Lens.lens (\DeleteTape' {tapeARN} -> tapeARN) (\s@DeleteTape' {} a -> s {tapeARN = a} :: DeleteTape)

instance Core.AWSRequest DeleteTape where
  type AWSResponse DeleteTape = DeleteTapeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapeResponse'
            Prelude.<$> (x Data..?> "TapeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTape where
  hashWithSalt _salt DeleteTape' {..} =
    _salt
      `Prelude.hashWithSalt` bypassGovernanceRetention
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` tapeARN

instance Prelude.NFData DeleteTape where
  rnf DeleteTape' {..} =
    Prelude.rnf bypassGovernanceRetention
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf tapeARN

instance Data.ToHeaders DeleteTape where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DeleteTape" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTape where
  toJSON DeleteTape' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BypassGovernanceRetention" Data..=)
              Prelude.<$> bypassGovernanceRetention,
            Prelude.Just ("GatewayARN" Data..= gatewayARN),
            Prelude.Just ("TapeARN" Data..= tapeARN)
          ]
      )

instance Data.ToPath DeleteTape where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTape where
  toQuery = Prelude.const Prelude.mempty

-- | DeleteTapeOutput
--
-- /See:/ 'newDeleteTapeResponse' smart constructor.
data DeleteTapeResponse = DeleteTapeResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted virtual tape.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTapeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'deleteTapeResponse_tapeARN' - The Amazon Resource Name (ARN) of the deleted virtual tape.
--
-- 'httpStatus', 'deleteTapeResponse_httpStatus' - The response's http status code.
newDeleteTapeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTapeResponse
newDeleteTapeResponse pHttpStatus_ =
  DeleteTapeResponse'
    { tapeARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted virtual tape.
deleteTapeResponse_tapeARN :: Lens.Lens' DeleteTapeResponse (Prelude.Maybe Prelude.Text)
deleteTapeResponse_tapeARN = Lens.lens (\DeleteTapeResponse' {tapeARN} -> tapeARN) (\s@DeleteTapeResponse' {} a -> s {tapeARN = a} :: DeleteTapeResponse)

-- | The response's http status code.
deleteTapeResponse_httpStatus :: Lens.Lens' DeleteTapeResponse Prelude.Int
deleteTapeResponse_httpStatus = Lens.lens (\DeleteTapeResponse' {httpStatus} -> httpStatus) (\s@DeleteTapeResponse' {} a -> s {httpStatus = a} :: DeleteTapeResponse)

instance Prelude.NFData DeleteTapeResponse where
  rnf DeleteTapeResponse' {..} =
    Prelude.rnf tapeARN
      `Prelude.seq` Prelude.rnf httpStatus
