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
-- Module      : Amazonka.PaymentCryptography.StartKeyUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables an Amazon Web Services Payment Cryptography key, which makes it
-- active for cryptographic operations within Amazon Web Services Payment
-- Cryptography
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   StopKeyUsage
module Amazonka.PaymentCryptography.StartKeyUsage
  ( -- * Creating a Request
    StartKeyUsage (..),
    newStartKeyUsage,

    -- * Request Lenses
    startKeyUsage_keyIdentifier,

    -- * Destructuring the Response
    StartKeyUsageResponse (..),
    newStartKeyUsageResponse,

    -- * Response Lenses
    startKeyUsageResponse_httpStatus,
    startKeyUsageResponse_key,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartKeyUsage' smart constructor.
data StartKeyUsage = StartKeyUsage'
  { -- | The @KeyArn@ of the key.
    keyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartKeyUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyIdentifier', 'startKeyUsage_keyIdentifier' - The @KeyArn@ of the key.
newStartKeyUsage ::
  -- | 'keyIdentifier'
  Prelude.Text ->
  StartKeyUsage
newStartKeyUsage pKeyIdentifier_ =
  StartKeyUsage' {keyIdentifier = pKeyIdentifier_}

-- | The @KeyArn@ of the key.
startKeyUsage_keyIdentifier :: Lens.Lens' StartKeyUsage Prelude.Text
startKeyUsage_keyIdentifier = Lens.lens (\StartKeyUsage' {keyIdentifier} -> keyIdentifier) (\s@StartKeyUsage' {} a -> s {keyIdentifier = a} :: StartKeyUsage)

instance Core.AWSRequest StartKeyUsage where
  type
    AWSResponse StartKeyUsage =
      StartKeyUsageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartKeyUsageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Key")
      )

instance Prelude.Hashable StartKeyUsage where
  hashWithSalt _salt StartKeyUsage' {..} =
    _salt `Prelude.hashWithSalt` keyIdentifier

instance Prelude.NFData StartKeyUsage where
  rnf StartKeyUsage' {..} = Prelude.rnf keyIdentifier

instance Data.ToHeaders StartKeyUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.StartKeyUsage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartKeyUsage where
  toJSON StartKeyUsage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KeyIdentifier" Data..= keyIdentifier)
          ]
      )

instance Data.ToPath StartKeyUsage where
  toPath = Prelude.const "/"

instance Data.ToQuery StartKeyUsage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartKeyUsageResponse' smart constructor.
data StartKeyUsageResponse = StartKeyUsageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @KeyARN@ of the Amazon Web Services Payment Cryptography key
    -- activated for use.
    key :: Key
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartKeyUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startKeyUsageResponse_httpStatus' - The response's http status code.
--
-- 'key', 'startKeyUsageResponse_key' - The @KeyARN@ of the Amazon Web Services Payment Cryptography key
-- activated for use.
newStartKeyUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'key'
  Key ->
  StartKeyUsageResponse
newStartKeyUsageResponse pHttpStatus_ pKey_ =
  StartKeyUsageResponse'
    { httpStatus = pHttpStatus_,
      key = pKey_
    }

-- | The response's http status code.
startKeyUsageResponse_httpStatus :: Lens.Lens' StartKeyUsageResponse Prelude.Int
startKeyUsageResponse_httpStatus = Lens.lens (\StartKeyUsageResponse' {httpStatus} -> httpStatus) (\s@StartKeyUsageResponse' {} a -> s {httpStatus = a} :: StartKeyUsageResponse)

-- | The @KeyARN@ of the Amazon Web Services Payment Cryptography key
-- activated for use.
startKeyUsageResponse_key :: Lens.Lens' StartKeyUsageResponse Key
startKeyUsageResponse_key = Lens.lens (\StartKeyUsageResponse' {key} -> key) (\s@StartKeyUsageResponse' {} a -> s {key = a} :: StartKeyUsageResponse)

instance Prelude.NFData StartKeyUsageResponse where
  rnf StartKeyUsageResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf key
