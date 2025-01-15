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
-- Module      : Amazonka.IoTWireless.GetMulticastGroupSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a multicast group session.
module Amazonka.IoTWireless.GetMulticastGroupSession
  ( -- * Creating a Request
    GetMulticastGroupSession (..),
    newGetMulticastGroupSession,

    -- * Request Lenses
    getMulticastGroupSession_id,

    -- * Destructuring the Response
    GetMulticastGroupSessionResponse (..),
    newGetMulticastGroupSessionResponse,

    -- * Response Lenses
    getMulticastGroupSessionResponse_loRaWAN,
    getMulticastGroupSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMulticastGroupSession' smart constructor.
data GetMulticastGroupSession = GetMulticastGroupSession'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMulticastGroupSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getMulticastGroupSession_id' - Undocumented member.
newGetMulticastGroupSession ::
  -- | 'id'
  Prelude.Text ->
  GetMulticastGroupSession
newGetMulticastGroupSession pId_ =
  GetMulticastGroupSession' {id = pId_}

-- | Undocumented member.
getMulticastGroupSession_id :: Lens.Lens' GetMulticastGroupSession Prelude.Text
getMulticastGroupSession_id = Lens.lens (\GetMulticastGroupSession' {id} -> id) (\s@GetMulticastGroupSession' {} a -> s {id = a} :: GetMulticastGroupSession)

instance Core.AWSRequest GetMulticastGroupSession where
  type
    AWSResponse GetMulticastGroupSession =
      GetMulticastGroupSessionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMulticastGroupSessionResponse'
            Prelude.<$> (x Data..?> "LoRaWAN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMulticastGroupSession where
  hashWithSalt _salt GetMulticastGroupSession' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetMulticastGroupSession where
  rnf GetMulticastGroupSession' {..} = Prelude.rnf id

instance Data.ToHeaders GetMulticastGroupSession where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetMulticastGroupSession where
  toPath GetMulticastGroupSession' {..} =
    Prelude.mconcat
      ["/multicast-groups/", Data.toBS id, "/session"]

instance Data.ToQuery GetMulticastGroupSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMulticastGroupSessionResponse' smart constructor.
data GetMulticastGroupSessionResponse = GetMulticastGroupSessionResponse'
  { loRaWAN :: Prelude.Maybe LoRaWANMulticastSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMulticastGroupSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loRaWAN', 'getMulticastGroupSessionResponse_loRaWAN' - Undocumented member.
--
-- 'httpStatus', 'getMulticastGroupSessionResponse_httpStatus' - The response's http status code.
newGetMulticastGroupSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMulticastGroupSessionResponse
newGetMulticastGroupSessionResponse pHttpStatus_ =
  GetMulticastGroupSessionResponse'
    { loRaWAN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getMulticastGroupSessionResponse_loRaWAN :: Lens.Lens' GetMulticastGroupSessionResponse (Prelude.Maybe LoRaWANMulticastSession)
getMulticastGroupSessionResponse_loRaWAN = Lens.lens (\GetMulticastGroupSessionResponse' {loRaWAN} -> loRaWAN) (\s@GetMulticastGroupSessionResponse' {} a -> s {loRaWAN = a} :: GetMulticastGroupSessionResponse)

-- | The response's http status code.
getMulticastGroupSessionResponse_httpStatus :: Lens.Lens' GetMulticastGroupSessionResponse Prelude.Int
getMulticastGroupSessionResponse_httpStatus = Lens.lens (\GetMulticastGroupSessionResponse' {httpStatus} -> httpStatus) (\s@GetMulticastGroupSessionResponse' {} a -> s {httpStatus = a} :: GetMulticastGroupSessionResponse)

instance
  Prelude.NFData
    GetMulticastGroupSessionResponse
  where
  rnf GetMulticastGroupSessionResponse' {..} =
    Prelude.rnf loRaWAN `Prelude.seq`
      Prelude.rnf httpStatus
