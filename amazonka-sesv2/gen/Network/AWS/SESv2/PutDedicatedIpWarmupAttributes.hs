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
-- Module      : Network.AWS.SESv2.PutDedicatedIpWarmupAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.PutDedicatedIpWarmupAttributes
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

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
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutDedicatedIpWarmupAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutDedicatedIpWarmupAttributes

instance
  Prelude.NFData
    PutDedicatedIpWarmupAttributes

instance
  Core.ToHeaders
    PutDedicatedIpWarmupAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutDedicatedIpWarmupAttributes where
  toJSON PutDedicatedIpWarmupAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WarmupPercentage" Core..= warmupPercentage)
          ]
      )

instance Core.ToPath PutDedicatedIpWarmupAttributes where
  toPath PutDedicatedIpWarmupAttributes' {..} =
    Prelude.mconcat
      ["/v2/email/dedicated-ips/", Core.toBS ip, "/warmup"]

instance Core.ToQuery PutDedicatedIpWarmupAttributes where
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
