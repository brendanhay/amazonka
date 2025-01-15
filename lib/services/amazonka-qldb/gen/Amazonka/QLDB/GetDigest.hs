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
-- Module      : Amazonka.QLDB.GetDigest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the digest of a ledger at the latest committed block in the
-- journal. The response includes a 256-bit hash value and a block address.
module Amazonka.QLDB.GetDigest
  ( -- * Creating a Request
    GetDigest (..),
    newGetDigest,

    -- * Request Lenses
    getDigest_name,

    -- * Destructuring the Response
    GetDigestResponse (..),
    newGetDigestResponse,

    -- * Response Lenses
    getDigestResponse_httpStatus,
    getDigestResponse_digest,
    getDigestResponse_digestTipAddress,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDigest' smart constructor.
data GetDigest = GetDigest'
  { -- | The name of the ledger.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDigest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getDigest_name' - The name of the ledger.
newGetDigest ::
  -- | 'name'
  Prelude.Text ->
  GetDigest
newGetDigest pName_ = GetDigest' {name = pName_}

-- | The name of the ledger.
getDigest_name :: Lens.Lens' GetDigest Prelude.Text
getDigest_name = Lens.lens (\GetDigest' {name} -> name) (\s@GetDigest' {} a -> s {name = a} :: GetDigest)

instance Core.AWSRequest GetDigest where
  type AWSResponse GetDigest = GetDigestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDigestResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Digest")
            Prelude.<*> (x Data..:> "DigestTipAddress")
      )

instance Prelude.Hashable GetDigest where
  hashWithSalt _salt GetDigest' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetDigest where
  rnf GetDigest' {..} = Prelude.rnf name

instance Data.ToHeaders GetDigest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDigest where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetDigest where
  toPath GetDigest' {..} =
    Prelude.mconcat
      ["/ledgers/", Data.toBS name, "/digest"]

instance Data.ToQuery GetDigest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDigestResponse' smart constructor.
data GetDigestResponse = GetDigestResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The 256-bit hash value representing the digest returned by a @GetDigest@
    -- request.
    digest :: Data.Base64,
    -- | The latest block location covered by the digest that you requested. An
    -- address is an Amazon Ion structure that has two fields: @strandId@ and
    -- @sequenceNo@.
    digestTipAddress :: Data.Sensitive ValueHolder
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDigestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDigestResponse_httpStatus' - The response's http status code.
--
-- 'digest', 'getDigestResponse_digest' - The 256-bit hash value representing the digest returned by a @GetDigest@
-- request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'digestTipAddress', 'getDigestResponse_digestTipAddress' - The latest block location covered by the digest that you requested. An
-- address is an Amazon Ion structure that has two fields: @strandId@ and
-- @sequenceNo@.
newGetDigestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'digest'
  Prelude.ByteString ->
  -- | 'digestTipAddress'
  ValueHolder ->
  GetDigestResponse
newGetDigestResponse
  pHttpStatus_
  pDigest_
  pDigestTipAddress_ =
    GetDigestResponse'
      { httpStatus = pHttpStatus_,
        digest = Data._Base64 Lens.# pDigest_,
        digestTipAddress =
          Data._Sensitive Lens.# pDigestTipAddress_
      }

-- | The response's http status code.
getDigestResponse_httpStatus :: Lens.Lens' GetDigestResponse Prelude.Int
getDigestResponse_httpStatus = Lens.lens (\GetDigestResponse' {httpStatus} -> httpStatus) (\s@GetDigestResponse' {} a -> s {httpStatus = a} :: GetDigestResponse)

-- | The 256-bit hash value representing the digest returned by a @GetDigest@
-- request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getDigestResponse_digest :: Lens.Lens' GetDigestResponse Prelude.ByteString
getDigestResponse_digest = Lens.lens (\GetDigestResponse' {digest} -> digest) (\s@GetDigestResponse' {} a -> s {digest = a} :: GetDigestResponse) Prelude.. Data._Base64

-- | The latest block location covered by the digest that you requested. An
-- address is an Amazon Ion structure that has two fields: @strandId@ and
-- @sequenceNo@.
getDigestResponse_digestTipAddress :: Lens.Lens' GetDigestResponse ValueHolder
getDigestResponse_digestTipAddress = Lens.lens (\GetDigestResponse' {digestTipAddress} -> digestTipAddress) (\s@GetDigestResponse' {} a -> s {digestTipAddress = a} :: GetDigestResponse) Prelude.. Data._Sensitive

instance Prelude.NFData GetDigestResponse where
  rnf GetDigestResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf digest `Prelude.seq`
        Prelude.rnf digestTipAddress
