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
-- Module      : Amazonka.SESV2.PutAccountDedicatedIpWarmupAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable or disable the automatic warm-up feature for dedicated IP
-- addresses.
module Amazonka.SESV2.PutAccountDedicatedIpWarmupAttributes
  ( -- * Creating a Request
    PutAccountDedicatedIpWarmupAttributes (..),
    newPutAccountDedicatedIpWarmupAttributes,

    -- * Request Lenses
    putAccountDedicatedIpWarmupAttributes_autoWarmupEnabled,

    -- * Destructuring the Response
    PutAccountDedicatedIpWarmupAttributesResponse (..),
    newPutAccountDedicatedIpWarmupAttributesResponse,

    -- * Response Lenses
    putAccountDedicatedIpWarmupAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to enable or disable the automatic IP address warm-up feature.
--
-- /See:/ 'newPutAccountDedicatedIpWarmupAttributes' smart constructor.
data PutAccountDedicatedIpWarmupAttributes = PutAccountDedicatedIpWarmupAttributes'
  { -- | Enables or disables the automatic warm-up feature for dedicated IP
    -- addresses that are associated with your Amazon SES account in the
    -- current Amazon Web Services Region. Set to @true@ to enable the
    -- automatic warm-up feature, or set to @false@ to disable it.
    autoWarmupEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountDedicatedIpWarmupAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoWarmupEnabled', 'putAccountDedicatedIpWarmupAttributes_autoWarmupEnabled' - Enables or disables the automatic warm-up feature for dedicated IP
-- addresses that are associated with your Amazon SES account in the
-- current Amazon Web Services Region. Set to @true@ to enable the
-- automatic warm-up feature, or set to @false@ to disable it.
newPutAccountDedicatedIpWarmupAttributes ::
  PutAccountDedicatedIpWarmupAttributes
newPutAccountDedicatedIpWarmupAttributes =
  PutAccountDedicatedIpWarmupAttributes'
    { autoWarmupEnabled =
        Prelude.Nothing
    }

-- | Enables or disables the automatic warm-up feature for dedicated IP
-- addresses that are associated with your Amazon SES account in the
-- current Amazon Web Services Region. Set to @true@ to enable the
-- automatic warm-up feature, or set to @false@ to disable it.
putAccountDedicatedIpWarmupAttributes_autoWarmupEnabled :: Lens.Lens' PutAccountDedicatedIpWarmupAttributes (Prelude.Maybe Prelude.Bool)
putAccountDedicatedIpWarmupAttributes_autoWarmupEnabled = Lens.lens (\PutAccountDedicatedIpWarmupAttributes' {autoWarmupEnabled} -> autoWarmupEnabled) (\s@PutAccountDedicatedIpWarmupAttributes' {} a -> s {autoWarmupEnabled = a} :: PutAccountDedicatedIpWarmupAttributes)

instance
  Core.AWSRequest
    PutAccountDedicatedIpWarmupAttributes
  where
  type
    AWSResponse
      PutAccountDedicatedIpWarmupAttributes =
      PutAccountDedicatedIpWarmupAttributesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccountDedicatedIpWarmupAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutAccountDedicatedIpWarmupAttributes
  where
  hashWithSalt
    _salt
    PutAccountDedicatedIpWarmupAttributes' {..} =
      _salt `Prelude.hashWithSalt` autoWarmupEnabled

instance
  Prelude.NFData
    PutAccountDedicatedIpWarmupAttributes
  where
  rnf PutAccountDedicatedIpWarmupAttributes' {..} =
    Prelude.rnf autoWarmupEnabled

instance
  Data.ToHeaders
    PutAccountDedicatedIpWarmupAttributes
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

instance
  Data.ToJSON
    PutAccountDedicatedIpWarmupAttributes
  where
  toJSON PutAccountDedicatedIpWarmupAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoWarmupEnabled" Data..=)
              Prelude.<$> autoWarmupEnabled
          ]
      )

instance
  Data.ToPath
    PutAccountDedicatedIpWarmupAttributes
  where
  toPath =
    Prelude.const
      "/v2/email/account/dedicated-ips/warmup"

instance
  Data.ToQuery
    PutAccountDedicatedIpWarmupAttributes
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutAccountDedicatedIpWarmupAttributesResponse' smart constructor.
data PutAccountDedicatedIpWarmupAttributesResponse = PutAccountDedicatedIpWarmupAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountDedicatedIpWarmupAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAccountDedicatedIpWarmupAttributesResponse_httpStatus' - The response's http status code.
newPutAccountDedicatedIpWarmupAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccountDedicatedIpWarmupAttributesResponse
newPutAccountDedicatedIpWarmupAttributesResponse
  pHttpStatus_ =
    PutAccountDedicatedIpWarmupAttributesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putAccountDedicatedIpWarmupAttributesResponse_httpStatus :: Lens.Lens' PutAccountDedicatedIpWarmupAttributesResponse Prelude.Int
putAccountDedicatedIpWarmupAttributesResponse_httpStatus = Lens.lens (\PutAccountDedicatedIpWarmupAttributesResponse' {httpStatus} -> httpStatus) (\s@PutAccountDedicatedIpWarmupAttributesResponse' {} a -> s {httpStatus = a} :: PutAccountDedicatedIpWarmupAttributesResponse)

instance
  Prelude.NFData
    PutAccountDedicatedIpWarmupAttributesResponse
  where
  rnf
    PutAccountDedicatedIpWarmupAttributesResponse' {..} =
      Prelude.rnf httpStatus
