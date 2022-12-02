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
-- Module      : Amazonka.IoTWireless.StartMulticastGroupSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a multicast group session.
module Amazonka.IoTWireless.StartMulticastGroupSession
  ( -- * Creating a Request
    StartMulticastGroupSession (..),
    newStartMulticastGroupSession,

    -- * Request Lenses
    startMulticastGroupSession_id,
    startMulticastGroupSession_loRaWAN,

    -- * Destructuring the Response
    StartMulticastGroupSessionResponse (..),
    newStartMulticastGroupSessionResponse,

    -- * Response Lenses
    startMulticastGroupSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartMulticastGroupSession' smart constructor.
data StartMulticastGroupSession = StartMulticastGroupSession'
  { id :: Prelude.Text,
    loRaWAN :: LoRaWANMulticastSession
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMulticastGroupSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'startMulticastGroupSession_id' - Undocumented member.
--
-- 'loRaWAN', 'startMulticastGroupSession_loRaWAN' - Undocumented member.
newStartMulticastGroupSession ::
  -- | 'id'
  Prelude.Text ->
  -- | 'loRaWAN'
  LoRaWANMulticastSession ->
  StartMulticastGroupSession
newStartMulticastGroupSession pId_ pLoRaWAN_ =
  StartMulticastGroupSession'
    { id = pId_,
      loRaWAN = pLoRaWAN_
    }

-- | Undocumented member.
startMulticastGroupSession_id :: Lens.Lens' StartMulticastGroupSession Prelude.Text
startMulticastGroupSession_id = Lens.lens (\StartMulticastGroupSession' {id} -> id) (\s@StartMulticastGroupSession' {} a -> s {id = a} :: StartMulticastGroupSession)

-- | Undocumented member.
startMulticastGroupSession_loRaWAN :: Lens.Lens' StartMulticastGroupSession LoRaWANMulticastSession
startMulticastGroupSession_loRaWAN = Lens.lens (\StartMulticastGroupSession' {loRaWAN} -> loRaWAN) (\s@StartMulticastGroupSession' {} a -> s {loRaWAN = a} :: StartMulticastGroupSession)

instance Core.AWSRequest StartMulticastGroupSession where
  type
    AWSResponse StartMulticastGroupSession =
      StartMulticastGroupSessionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartMulticastGroupSessionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMulticastGroupSession where
  hashWithSalt _salt StartMulticastGroupSession' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` loRaWAN

instance Prelude.NFData StartMulticastGroupSession where
  rnf StartMulticastGroupSession' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf loRaWAN

instance Data.ToHeaders StartMulticastGroupSession where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartMulticastGroupSession where
  toJSON StartMulticastGroupSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LoRaWAN" Data..= loRaWAN)]
      )

instance Data.ToPath StartMulticastGroupSession where
  toPath StartMulticastGroupSession' {..} =
    Prelude.mconcat
      ["/multicast-groups/", Data.toBS id, "/session"]

instance Data.ToQuery StartMulticastGroupSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMulticastGroupSessionResponse' smart constructor.
data StartMulticastGroupSessionResponse = StartMulticastGroupSessionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMulticastGroupSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startMulticastGroupSessionResponse_httpStatus' - The response's http status code.
newStartMulticastGroupSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartMulticastGroupSessionResponse
newStartMulticastGroupSessionResponse pHttpStatus_ =
  StartMulticastGroupSessionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startMulticastGroupSessionResponse_httpStatus :: Lens.Lens' StartMulticastGroupSessionResponse Prelude.Int
startMulticastGroupSessionResponse_httpStatus = Lens.lens (\StartMulticastGroupSessionResponse' {httpStatus} -> httpStatus) (\s@StartMulticastGroupSessionResponse' {} a -> s {httpStatus = a} :: StartMulticastGroupSessionResponse)

instance
  Prelude.NFData
    StartMulticastGroupSessionResponse
  where
  rnf StartMulticastGroupSessionResponse' {..} =
    Prelude.rnf httpStatus
