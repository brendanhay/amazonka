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
-- Module      : Amazonka.SESV2.PutDedicatedIpWarmupAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.PutDedicatedIpWarmupAttributes
  ( -- * Creating a Request
    PutDedicatedIpWarmupAttributes (..),
    newPutDedicatedIpWarmupAttributes,

    -- * Request Lenses
    putDedicatedIpWarmupAttributes_ip,
    putDedicatedIpWarmupAttributes_warmupPercentage,

    -- * Destructuring the Response
    PutDedicatedIpWarmupAttributesResponse (..),
    newPutDedicatedIpWarmupAttributesResponse,

    -- * Response Lenses
    putDedicatedIpWarmupAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to change the warm-up attributes for a dedicated IP address.
-- This operation is useful when you want to resume the warm-up process for
-- an existing IP address.
--
-- /See:/ 'newPutDedicatedIpWarmupAttributes' smart constructor.
data PutDedicatedIpWarmupAttributes = PutDedicatedIpWarmupAttributes'
  { -- | The dedicated IP address that you want to update the warm-up attributes
    -- for.
    ip :: Prelude.Text,
    -- | The warm-up percentage that you want to associate with the dedicated IP
    -- address.
    warmupPercentage :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDedicatedIpWarmupAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ip', 'putDedicatedIpWarmupAttributes_ip' - The dedicated IP address that you want to update the warm-up attributes
-- for.
--
-- 'warmupPercentage', 'putDedicatedIpWarmupAttributes_warmupPercentage' - The warm-up percentage that you want to associate with the dedicated IP
-- address.
newPutDedicatedIpWarmupAttributes ::
  -- | 'ip'
  Prelude.Text ->
  -- | 'warmupPercentage'
  Prelude.Int ->
  PutDedicatedIpWarmupAttributes
newPutDedicatedIpWarmupAttributes
  pIp_
  pWarmupPercentage_ =
    PutDedicatedIpWarmupAttributes'
      { ip = pIp_,
        warmupPercentage = pWarmupPercentage_
      }

-- | The dedicated IP address that you want to update the warm-up attributes
-- for.
putDedicatedIpWarmupAttributes_ip :: Lens.Lens' PutDedicatedIpWarmupAttributes Prelude.Text
putDedicatedIpWarmupAttributes_ip = Lens.lens (\PutDedicatedIpWarmupAttributes' {ip} -> ip) (\s@PutDedicatedIpWarmupAttributes' {} a -> s {ip = a} :: PutDedicatedIpWarmupAttributes)

-- | The warm-up percentage that you want to associate with the dedicated IP
-- address.
putDedicatedIpWarmupAttributes_warmupPercentage :: Lens.Lens' PutDedicatedIpWarmupAttributes Prelude.Int
putDedicatedIpWarmupAttributes_warmupPercentage = Lens.lens (\PutDedicatedIpWarmupAttributes' {warmupPercentage} -> warmupPercentage) (\s@PutDedicatedIpWarmupAttributes' {} a -> s {warmupPercentage = a} :: PutDedicatedIpWarmupAttributes)

instance
  Core.AWSRequest
    PutDedicatedIpWarmupAttributes
  where
  type
    AWSResponse PutDedicatedIpWarmupAttributes =
      PutDedicatedIpWarmupAttributesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutDedicatedIpWarmupAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutDedicatedIpWarmupAttributes
  where
  hashWithSalt
    _salt
    PutDedicatedIpWarmupAttributes' {..} =
      _salt `Prelude.hashWithSalt` ip
        `Prelude.hashWithSalt` warmupPercentage

instance
  Prelude.NFData
    PutDedicatedIpWarmupAttributes
  where
  rnf PutDedicatedIpWarmupAttributes' {..} =
    Prelude.rnf ip
      `Prelude.seq` Prelude.rnf warmupPercentage

instance
  Data.ToHeaders
    PutDedicatedIpWarmupAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDedicatedIpWarmupAttributes where
  toJSON PutDedicatedIpWarmupAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WarmupPercentage" Data..= warmupPercentage)
          ]
      )

instance Data.ToPath PutDedicatedIpWarmupAttributes where
  toPath PutDedicatedIpWarmupAttributes' {..} =
    Prelude.mconcat
      ["/v2/email/dedicated-ips/", Data.toBS ip, "/warmup"]

instance Data.ToQuery PutDedicatedIpWarmupAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutDedicatedIpWarmupAttributesResponse' smart constructor.
data PutDedicatedIpWarmupAttributesResponse = PutDedicatedIpWarmupAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDedicatedIpWarmupAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putDedicatedIpWarmupAttributesResponse_httpStatus' - The response's http status code.
newPutDedicatedIpWarmupAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDedicatedIpWarmupAttributesResponse
newPutDedicatedIpWarmupAttributesResponse
  pHttpStatus_ =
    PutDedicatedIpWarmupAttributesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putDedicatedIpWarmupAttributesResponse_httpStatus :: Lens.Lens' PutDedicatedIpWarmupAttributesResponse Prelude.Int
putDedicatedIpWarmupAttributesResponse_httpStatus = Lens.lens (\PutDedicatedIpWarmupAttributesResponse' {httpStatus} -> httpStatus) (\s@PutDedicatedIpWarmupAttributesResponse' {} a -> s {httpStatus = a} :: PutDedicatedIpWarmupAttributesResponse)

instance
  Prelude.NFData
    PutDedicatedIpWarmupAttributesResponse
  where
  rnf PutDedicatedIpWarmupAttributesResponse' {..} =
    Prelude.rnf httpStatus
