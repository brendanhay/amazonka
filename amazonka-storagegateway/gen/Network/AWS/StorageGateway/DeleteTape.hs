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
-- Module      : Network.AWS.StorageGateway.DeleteTape
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape. This operation is only supported in
-- the tape gateway type.
module Network.AWS.StorageGateway.DeleteTape
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DeleteTapeInput
--
-- /See:/ 'newDeleteTape' smart constructor.
data DeleteTape = DeleteTape'
  { -- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool
    -- with tape retention lock. Only archived tapes with tape retention lock
    -- set to @governance@ can be deleted. Archived tapes with tape retention
    -- lock set to @compliance@ can\'t be deleted.
    bypassGovernanceRetention :: Core.Maybe Core.Bool,
    -- | The unique Amazon Resource Name (ARN) of the gateway that the virtual
    -- tape to delete is associated with. Use the ListGateways operation to
    -- return a list of gateways for your account and AWS Region.
    gatewayARN :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape to delete.
    tapeARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- return a list of gateways for your account and AWS Region.
--
-- 'tapeARN', 'deleteTape_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape to delete.
newDeleteTape ::
  -- | 'gatewayARN'
  Core.Text ->
  -- | 'tapeARN'
  Core.Text ->
  DeleteTape
newDeleteTape pGatewayARN_ pTapeARN_ =
  DeleteTape'
    { bypassGovernanceRetention =
        Core.Nothing,
      gatewayARN = pGatewayARN_,
      tapeARN = pTapeARN_
    }

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool
-- with tape retention lock. Only archived tapes with tape retention lock
-- set to @governance@ can be deleted. Archived tapes with tape retention
-- lock set to @compliance@ can\'t be deleted.
deleteTape_bypassGovernanceRetention :: Lens.Lens' DeleteTape (Core.Maybe Core.Bool)
deleteTape_bypassGovernanceRetention = Lens.lens (\DeleteTape' {bypassGovernanceRetention} -> bypassGovernanceRetention) (\s@DeleteTape' {} a -> s {bypassGovernanceRetention = a} :: DeleteTape)

-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual
-- tape to delete is associated with. Use the ListGateways operation to
-- return a list of gateways for your account and AWS Region.
deleteTape_gatewayARN :: Lens.Lens' DeleteTape Core.Text
deleteTape_gatewayARN = Lens.lens (\DeleteTape' {gatewayARN} -> gatewayARN) (\s@DeleteTape' {} a -> s {gatewayARN = a} :: DeleteTape)

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
deleteTape_tapeARN :: Lens.Lens' DeleteTape Core.Text
deleteTape_tapeARN = Lens.lens (\DeleteTape' {tapeARN} -> tapeARN) (\s@DeleteTape' {} a -> s {tapeARN = a} :: DeleteTape)

instance Core.AWSRequest DeleteTape where
  type AWSResponse DeleteTape = DeleteTapeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapeResponse'
            Core.<$> (x Core..?> "TapeARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTape

instance Core.NFData DeleteTape

instance Core.ToHeaders DeleteTape where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DeleteTape" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteTape where
  toJSON DeleteTape' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BypassGovernanceRetention" Core..=)
              Core.<$> bypassGovernanceRetention,
            Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("TapeARN" Core..= tapeARN)
          ]
      )

instance Core.ToPath DeleteTape where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTape where
  toQuery = Core.const Core.mempty

-- | DeleteTapeOutput
--
-- /See:/ 'newDeleteTapeResponse' smart constructor.
data DeleteTapeResponse = DeleteTapeResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted virtual tape.
    tapeARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteTapeResponse
newDeleteTapeResponse pHttpStatus_ =
  DeleteTapeResponse'
    { tapeARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted virtual tape.
deleteTapeResponse_tapeARN :: Lens.Lens' DeleteTapeResponse (Core.Maybe Core.Text)
deleteTapeResponse_tapeARN = Lens.lens (\DeleteTapeResponse' {tapeARN} -> tapeARN) (\s@DeleteTapeResponse' {} a -> s {tapeARN = a} :: DeleteTapeResponse)

-- | The response's http status code.
deleteTapeResponse_httpStatus :: Lens.Lens' DeleteTapeResponse Core.Int
deleteTapeResponse_httpStatus = Lens.lens (\DeleteTapeResponse' {httpStatus} -> httpStatus) (\s@DeleteTapeResponse' {} a -> s {httpStatus = a} :: DeleteTapeResponse)

instance Core.NFData DeleteTapeResponse
