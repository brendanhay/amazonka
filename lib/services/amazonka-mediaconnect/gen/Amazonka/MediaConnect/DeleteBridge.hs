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
-- Module      : Amazonka.MediaConnect.DeleteBridge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a bridge. Before you can delete a bridge, you must stop the
-- bridge.
module Amazonka.MediaConnect.DeleteBridge
  ( -- * Creating a Request
    DeleteBridge (..),
    newDeleteBridge,

    -- * Request Lenses
    deleteBridge_bridgeArn,

    -- * Destructuring the Response
    DeleteBridgeResponse (..),
    newDeleteBridgeResponse,

    -- * Response Lenses
    deleteBridgeResponse_bridgeArn,
    deleteBridgeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBridge' smart constructor.
data DeleteBridge = DeleteBridge'
  { -- | The ARN of the bridge that you want to delete.
    bridgeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBridge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'deleteBridge_bridgeArn' - The ARN of the bridge that you want to delete.
newDeleteBridge ::
  -- | 'bridgeArn'
  Prelude.Text ->
  DeleteBridge
newDeleteBridge pBridgeArn_ =
  DeleteBridge' {bridgeArn = pBridgeArn_}

-- | The ARN of the bridge that you want to delete.
deleteBridge_bridgeArn :: Lens.Lens' DeleteBridge Prelude.Text
deleteBridge_bridgeArn = Lens.lens (\DeleteBridge' {bridgeArn} -> bridgeArn) (\s@DeleteBridge' {} a -> s {bridgeArn = a} :: DeleteBridge)

instance Core.AWSRequest DeleteBridge where
  type AWSResponse DeleteBridge = DeleteBridgeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBridgeResponse'
            Prelude.<$> (x Data..?> "bridgeArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBridge where
  hashWithSalt _salt DeleteBridge' {..} =
    _salt `Prelude.hashWithSalt` bridgeArn

instance Prelude.NFData DeleteBridge where
  rnf DeleteBridge' {..} = Prelude.rnf bridgeArn

instance Data.ToHeaders DeleteBridge where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBridge where
  toPath DeleteBridge' {..} =
    Prelude.mconcat
      ["/v1/bridges/", Data.toBS bridgeArn]

instance Data.ToQuery DeleteBridge where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBridgeResponse' smart constructor.
data DeleteBridgeResponse = DeleteBridgeResponse'
  { -- | The Amazon Resource Number (ARN) of the deleted bridge.
    bridgeArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBridgeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'deleteBridgeResponse_bridgeArn' - The Amazon Resource Number (ARN) of the deleted bridge.
--
-- 'httpStatus', 'deleteBridgeResponse_httpStatus' - The response's http status code.
newDeleteBridgeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBridgeResponse
newDeleteBridgeResponse pHttpStatus_ =
  DeleteBridgeResponse'
    { bridgeArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the deleted bridge.
deleteBridgeResponse_bridgeArn :: Lens.Lens' DeleteBridgeResponse (Prelude.Maybe Prelude.Text)
deleteBridgeResponse_bridgeArn = Lens.lens (\DeleteBridgeResponse' {bridgeArn} -> bridgeArn) (\s@DeleteBridgeResponse' {} a -> s {bridgeArn = a} :: DeleteBridgeResponse)

-- | The response's http status code.
deleteBridgeResponse_httpStatus :: Lens.Lens' DeleteBridgeResponse Prelude.Int
deleteBridgeResponse_httpStatus = Lens.lens (\DeleteBridgeResponse' {httpStatus} -> httpStatus) (\s@DeleteBridgeResponse' {} a -> s {httpStatus = a} :: DeleteBridgeResponse)

instance Prelude.NFData DeleteBridgeResponse where
  rnf DeleteBridgeResponse' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf httpStatus
