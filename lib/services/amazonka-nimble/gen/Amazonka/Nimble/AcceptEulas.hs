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
-- Module      : Amazonka.Nimble.AcceptEulas
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept EULAs.
module Amazonka.Nimble.AcceptEulas
  ( -- * Creating a Request
    AcceptEulas (..),
    newAcceptEulas,

    -- * Request Lenses
    acceptEulas_clientToken,
    acceptEulas_eulaIds,
    acceptEulas_studioId,

    -- * Destructuring the Response
    AcceptEulasResponse (..),
    newAcceptEulasResponse,

    -- * Response Lenses
    acceptEulasResponse_eulaAcceptances,
    acceptEulasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptEulas' smart constructor.
data AcceptEulas = AcceptEulas'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the AWS
    -- SDK automatically generates a client token and uses it for the request
    -- to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The EULA ID.
    eulaIds :: Prelude.Maybe [Prelude.Text],
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptEulas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'acceptEulas_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
--
-- 'eulaIds', 'acceptEulas_eulaIds' - The EULA ID.
--
-- 'studioId', 'acceptEulas_studioId' - The studio ID.
newAcceptEulas ::
  -- | 'studioId'
  Prelude.Text ->
  AcceptEulas
newAcceptEulas pStudioId_ =
  AcceptEulas'
    { clientToken = Prelude.Nothing,
      eulaIds = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
acceptEulas_clientToken :: Lens.Lens' AcceptEulas (Prelude.Maybe Prelude.Text)
acceptEulas_clientToken = Lens.lens (\AcceptEulas' {clientToken} -> clientToken) (\s@AcceptEulas' {} a -> s {clientToken = a} :: AcceptEulas)

-- | The EULA ID.
acceptEulas_eulaIds :: Lens.Lens' AcceptEulas (Prelude.Maybe [Prelude.Text])
acceptEulas_eulaIds = Lens.lens (\AcceptEulas' {eulaIds} -> eulaIds) (\s@AcceptEulas' {} a -> s {eulaIds = a} :: AcceptEulas) Prelude.. Lens.mapping Lens.coerced

-- | The studio ID.
acceptEulas_studioId :: Lens.Lens' AcceptEulas Prelude.Text
acceptEulas_studioId = Lens.lens (\AcceptEulas' {studioId} -> studioId) (\s@AcceptEulas' {} a -> s {studioId = a} :: AcceptEulas)

instance Core.AWSRequest AcceptEulas where
  type AWSResponse AcceptEulas = AcceptEulasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptEulasResponse'
            Prelude.<$> ( x Data..?> "eulaAcceptances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptEulas where
  hashWithSalt _salt AcceptEulas' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` eulaIds
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData AcceptEulas where
  rnf AcceptEulas' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf eulaIds
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders AcceptEulas where
  toHeaders AcceptEulas' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON AcceptEulas where
  toJSON AcceptEulas' {..} =
    Data.object
      ( Prelude.catMaybes
          [("eulaIds" Data..=) Prelude.<$> eulaIds]
      )

instance Data.ToPath AcceptEulas where
  toPath AcceptEulas' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/eula-acceptances"
      ]

instance Data.ToQuery AcceptEulas where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptEulasResponse' smart constructor.
data AcceptEulasResponse = AcceptEulasResponse'
  { -- | A collection of EULA acceptances.
    eulaAcceptances :: Prelude.Maybe [EulaAcceptance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptEulasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eulaAcceptances', 'acceptEulasResponse_eulaAcceptances' - A collection of EULA acceptances.
--
-- 'httpStatus', 'acceptEulasResponse_httpStatus' - The response's http status code.
newAcceptEulasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptEulasResponse
newAcceptEulasResponse pHttpStatus_ =
  AcceptEulasResponse'
    { eulaAcceptances =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of EULA acceptances.
acceptEulasResponse_eulaAcceptances :: Lens.Lens' AcceptEulasResponse (Prelude.Maybe [EulaAcceptance])
acceptEulasResponse_eulaAcceptances = Lens.lens (\AcceptEulasResponse' {eulaAcceptances} -> eulaAcceptances) (\s@AcceptEulasResponse' {} a -> s {eulaAcceptances = a} :: AcceptEulasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
acceptEulasResponse_httpStatus :: Lens.Lens' AcceptEulasResponse Prelude.Int
acceptEulasResponse_httpStatus = Lens.lens (\AcceptEulasResponse' {httpStatus} -> httpStatus) (\s@AcceptEulasResponse' {} a -> s {httpStatus = a} :: AcceptEulasResponse)

instance Prelude.NFData AcceptEulasResponse where
  rnf AcceptEulasResponse' {..} =
    Prelude.rnf eulaAcceptances
      `Prelude.seq` Prelude.rnf httpStatus
