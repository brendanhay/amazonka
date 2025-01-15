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
-- Module      : Amazonka.Route53.GetDNSSEC
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DNSSEC for a specific hosted zone, including
-- the key-signing keys (KSKs) in the hosted zone.
module Amazonka.Route53.GetDNSSEC
  ( -- * Creating a Request
    GetDNSSEC (..),
    newGetDNSSEC,

    -- * Request Lenses
    getDNSSEC_hostedZoneId,

    -- * Destructuring the Response
    GetDNSSECResponse (..),
    newGetDNSSECResponse,

    -- * Response Lenses
    getDNSSECResponse_httpStatus,
    getDNSSECResponse_status,
    getDNSSECResponse_keySigningKeys,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newGetDNSSEC' smart constructor.
data GetDNSSEC = GetDNSSEC'
  { -- | A unique string used to identify a hosted zone.
    hostedZoneId :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDNSSEC' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'getDNSSEC_hostedZoneId' - A unique string used to identify a hosted zone.
newGetDNSSEC ::
  -- | 'hostedZoneId'
  ResourceId ->
  GetDNSSEC
newGetDNSSEC pHostedZoneId_ =
  GetDNSSEC' {hostedZoneId = pHostedZoneId_}

-- | A unique string used to identify a hosted zone.
getDNSSEC_hostedZoneId :: Lens.Lens' GetDNSSEC ResourceId
getDNSSEC_hostedZoneId = Lens.lens (\GetDNSSEC' {hostedZoneId} -> hostedZoneId) (\s@GetDNSSEC' {} a -> s {hostedZoneId = a} :: GetDNSSEC)

instance Core.AWSRequest GetDNSSEC where
  type AWSResponse GetDNSSEC = GetDNSSECResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetDNSSECResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Status")
            Prelude.<*> ( x Data..@? "KeySigningKeys" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable GetDNSSEC where
  hashWithSalt _salt GetDNSSEC' {..} =
    _salt `Prelude.hashWithSalt` hostedZoneId

instance Prelude.NFData GetDNSSEC where
  rnf GetDNSSEC' {..} = Prelude.rnf hostedZoneId

instance Data.ToHeaders GetDNSSEC where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetDNSSEC where
  toPath GetDNSSEC' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Data.toBS hostedZoneId,
        "/dnssec"
      ]

instance Data.ToQuery GetDNSSEC where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDNSSECResponse' smart constructor.
data GetDNSSECResponse = GetDNSSECResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A string repesenting the status of DNSSEC.
    status :: DNSSECStatus,
    -- | The key-signing keys (KSKs) in your account.
    keySigningKeys :: [KeySigningKey]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDNSSECResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDNSSECResponse_httpStatus' - The response's http status code.
--
-- 'status', 'getDNSSECResponse_status' - A string repesenting the status of DNSSEC.
--
-- 'keySigningKeys', 'getDNSSECResponse_keySigningKeys' - The key-signing keys (KSKs) in your account.
newGetDNSSECResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  DNSSECStatus ->
  GetDNSSECResponse
newGetDNSSECResponse pHttpStatus_ pStatus_ =
  GetDNSSECResponse'
    { httpStatus = pHttpStatus_,
      status = pStatus_,
      keySigningKeys = Prelude.mempty
    }

-- | The response's http status code.
getDNSSECResponse_httpStatus :: Lens.Lens' GetDNSSECResponse Prelude.Int
getDNSSECResponse_httpStatus = Lens.lens (\GetDNSSECResponse' {httpStatus} -> httpStatus) (\s@GetDNSSECResponse' {} a -> s {httpStatus = a} :: GetDNSSECResponse)

-- | A string repesenting the status of DNSSEC.
getDNSSECResponse_status :: Lens.Lens' GetDNSSECResponse DNSSECStatus
getDNSSECResponse_status = Lens.lens (\GetDNSSECResponse' {status} -> status) (\s@GetDNSSECResponse' {} a -> s {status = a} :: GetDNSSECResponse)

-- | The key-signing keys (KSKs) in your account.
getDNSSECResponse_keySigningKeys :: Lens.Lens' GetDNSSECResponse [KeySigningKey]
getDNSSECResponse_keySigningKeys = Lens.lens (\GetDNSSECResponse' {keySigningKeys} -> keySigningKeys) (\s@GetDNSSECResponse' {} a -> s {keySigningKeys = a} :: GetDNSSECResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetDNSSECResponse where
  rnf GetDNSSECResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf keySigningKeys
