{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    bypassGovernanceRetention :: Prelude.Maybe Prelude.Bool,
    -- | The unique Amazon Resource Name (ARN) of the gateway that the virtual
    -- tape to delete is associated with. Use the ListGateways operation to
    -- return a list of gateways for your account and AWS Region.
    gatewayARN :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape to delete.
    tapeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- return a list of gateways for your account and AWS Region.
deleteTape_gatewayARN :: Lens.Lens' DeleteTape Prelude.Text
deleteTape_gatewayARN = Lens.lens (\DeleteTape' {gatewayARN} -> gatewayARN) (\s@DeleteTape' {} a -> s {gatewayARN = a} :: DeleteTape)

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
deleteTape_tapeARN :: Lens.Lens' DeleteTape Prelude.Text
deleteTape_tapeARN = Lens.lens (\DeleteTape' {tapeARN} -> tapeARN) (\s@DeleteTape' {} a -> s {tapeARN = a} :: DeleteTape)

instance Prelude.AWSRequest DeleteTape where
  type Rs DeleteTape = DeleteTapeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapeResponse'
            Prelude.<$> (x Prelude..?> "TapeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTape

instance Prelude.NFData DeleteTape

instance Prelude.ToHeaders DeleteTape where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DeleteTape" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTape where
  toJSON DeleteTape' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("BypassGovernanceRetention" Prelude..=)
              Prelude.<$> bypassGovernanceRetention,
            Prelude.Just ("GatewayARN" Prelude..= gatewayARN),
            Prelude.Just ("TapeARN" Prelude..= tapeARN)
          ]
      )

instance Prelude.ToPath DeleteTape where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTape where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteTapeResponse
