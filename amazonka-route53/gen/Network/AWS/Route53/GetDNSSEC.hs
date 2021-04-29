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
-- Module      : Network.AWS.Route53.GetDNSSEC
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DNSSEC for a specific hosted zone, including
-- the key-signing keys (KSKs) in the hosted zone.
module Network.AWS.Route53.GetDNSSEC
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | /See:/ 'newGetDNSSEC' smart constructor.
data GetDNSSEC = GetDNSSEC'
  { -- | A unique string used to identify a hosted zone.
    hostedZoneId :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetDNSSEC where
  type Rs GetDNSSEC = GetDNSSECResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetDNSSECResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "Status")
            Prelude.<*> ( x Prelude..@? "KeySigningKeys"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.parseXMLList "member"
                        )
      )

instance Prelude.Hashable GetDNSSEC

instance Prelude.NFData GetDNSSEC

instance Prelude.ToHeaders GetDNSSEC where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetDNSSEC where
  toPath GetDNSSEC' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Prelude.toBS hostedZoneId,
        "/dnssec"
      ]

instance Prelude.ToQuery GetDNSSEC where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
getDNSSECResponse_keySigningKeys = Lens.lens (\GetDNSSECResponse' {keySigningKeys} -> keySigningKeys) (\s@GetDNSSECResponse' {} a -> s {keySigningKeys = a} :: GetDNSSECResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData GetDNSSECResponse
