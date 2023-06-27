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
-- Module      : Amazonka.PaymentCryptography.GetKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the key material for an Amazon Web Services Payment Cryptography
-- key, including the immutable and mutable data specified when the key was
-- created.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   CreateKey
--
-- -   DeleteKey
--
-- -   ListKeys
module Amazonka.PaymentCryptography.GetKey
  ( -- * Creating a Request
    GetKey (..),
    newGetKey,

    -- * Request Lenses
    getKey_keyIdentifier,

    -- * Destructuring the Response
    GetKeyResponse (..),
    newGetKeyResponse,

    -- * Response Lenses
    getKeyResponse_httpStatus,
    getKeyResponse_key,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKey' smart constructor.
data GetKey = GetKey'
  { -- | The @KeyARN@ of the Amazon Web Services Payment Cryptography key.
    keyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyIdentifier', 'getKey_keyIdentifier' - The @KeyARN@ of the Amazon Web Services Payment Cryptography key.
newGetKey ::
  -- | 'keyIdentifier'
  Prelude.Text ->
  GetKey
newGetKey pKeyIdentifier_ =
  GetKey' {keyIdentifier = pKeyIdentifier_}

-- | The @KeyARN@ of the Amazon Web Services Payment Cryptography key.
getKey_keyIdentifier :: Lens.Lens' GetKey Prelude.Text
getKey_keyIdentifier = Lens.lens (\GetKey' {keyIdentifier} -> keyIdentifier) (\s@GetKey' {} a -> s {keyIdentifier = a} :: GetKey)

instance Core.AWSRequest GetKey where
  type AWSResponse GetKey = GetKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Key")
      )

instance Prelude.Hashable GetKey where
  hashWithSalt _salt GetKey' {..} =
    _salt `Prelude.hashWithSalt` keyIdentifier

instance Prelude.NFData GetKey where
  rnf GetKey' {..} = Prelude.rnf keyIdentifier

instance Data.ToHeaders GetKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.GetKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetKey where
  toJSON GetKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KeyIdentifier" Data..= keyIdentifier)
          ]
      )

instance Data.ToPath GetKey where
  toPath = Prelude.const "/"

instance Data.ToQuery GetKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKeyResponse' smart constructor.
data GetKeyResponse = GetKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The key material, including the immutable and mutable data for the key.
    key :: Key
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getKeyResponse_httpStatus' - The response's http status code.
--
-- 'key', 'getKeyResponse_key' - The key material, including the immutable and mutable data for the key.
newGetKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'key'
  Key ->
  GetKeyResponse
newGetKeyResponse pHttpStatus_ pKey_ =
  GetKeyResponse'
    { httpStatus = pHttpStatus_,
      key = pKey_
    }

-- | The response's http status code.
getKeyResponse_httpStatus :: Lens.Lens' GetKeyResponse Prelude.Int
getKeyResponse_httpStatus = Lens.lens (\GetKeyResponse' {httpStatus} -> httpStatus) (\s@GetKeyResponse' {} a -> s {httpStatus = a} :: GetKeyResponse)

-- | The key material, including the immutable and mutable data for the key.
getKeyResponse_key :: Lens.Lens' GetKeyResponse Key
getKeyResponse_key = Lens.lens (\GetKeyResponse' {key} -> key) (\s@GetKeyResponse' {} a -> s {key = a} :: GetKeyResponse)

instance Prelude.NFData GetKeyResponse where
  rnf GetKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf key
