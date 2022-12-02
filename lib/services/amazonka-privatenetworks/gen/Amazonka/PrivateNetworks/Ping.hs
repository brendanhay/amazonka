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
-- Module      : Amazonka.PrivateNetworks.Ping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the health of the service.
module Amazonka.PrivateNetworks.Ping
  ( -- * Creating a Request
    Ping (..),
    newPing,

    -- * Destructuring the Response
    PingResponse (..),
    newPingResponse,

    -- * Response Lenses
    pingResponse_status,
    pingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPing' smart constructor.
data Ping = Ping'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPing ::
  Ping
newPing = Ping'

instance Core.AWSRequest Ping where
  type AWSResponse Ping = PingResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PingResponse'
            Prelude.<$> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Ping where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData Ping where
  rnf _ = ()

instance Data.ToHeaders Ping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath Ping where
  toPath = Prelude.const "/ping"

instance Data.ToQuery Ping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPingResponse' smart constructor.
data PingResponse = PingResponse'
  { -- | Information about the health of the service.
    status :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'pingResponse_status' - Information about the health of the service.
--
-- 'httpStatus', 'pingResponse_httpStatus' - The response's http status code.
newPingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PingResponse
newPingResponse pHttpStatus_ =
  PingResponse'
    { status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the health of the service.
pingResponse_status :: Lens.Lens' PingResponse (Prelude.Maybe Prelude.Text)
pingResponse_status = Lens.lens (\PingResponse' {status} -> status) (\s@PingResponse' {} a -> s {status = a} :: PingResponse)

-- | The response's http status code.
pingResponse_httpStatus :: Lens.Lens' PingResponse Prelude.Int
pingResponse_httpStatus = Lens.lens (\PingResponse' {httpStatus} -> httpStatus) (\s@PingResponse' {} a -> s {httpStatus = a} :: PingResponse)

instance Prelude.NFData PingResponse where
  rnf PingResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
