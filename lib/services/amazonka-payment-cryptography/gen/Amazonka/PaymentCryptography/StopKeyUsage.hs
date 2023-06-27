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
-- Module      : Amazonka.PaymentCryptography.StopKeyUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an Amazon Web Services Payment Cryptography key, which makes it
-- inactive within Amazon Web Services Payment Cryptography.
--
-- You can use this operation instead of DeleteKey to deactivate a key. You
-- can enable the key in the future by calling StartKeyUsage.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   DeleteKey
--
-- -   StartKeyUsage
module Amazonka.PaymentCryptography.StopKeyUsage
  ( -- * Creating a Request
    StopKeyUsage (..),
    newStopKeyUsage,

    -- * Request Lenses
    stopKeyUsage_keyIdentifier,

    -- * Destructuring the Response
    StopKeyUsageResponse (..),
    newStopKeyUsageResponse,

    -- * Response Lenses
    stopKeyUsageResponse_httpStatus,
    stopKeyUsageResponse_key,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopKeyUsage' smart constructor.
data StopKeyUsage = StopKeyUsage'
  { -- | The @KeyArn@ of the key.
    keyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopKeyUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyIdentifier', 'stopKeyUsage_keyIdentifier' - The @KeyArn@ of the key.
newStopKeyUsage ::
  -- | 'keyIdentifier'
  Prelude.Text ->
  StopKeyUsage
newStopKeyUsage pKeyIdentifier_ =
  StopKeyUsage' {keyIdentifier = pKeyIdentifier_}

-- | The @KeyArn@ of the key.
stopKeyUsage_keyIdentifier :: Lens.Lens' StopKeyUsage Prelude.Text
stopKeyUsage_keyIdentifier = Lens.lens (\StopKeyUsage' {keyIdentifier} -> keyIdentifier) (\s@StopKeyUsage' {} a -> s {keyIdentifier = a} :: StopKeyUsage)

instance Core.AWSRequest StopKeyUsage where
  type AWSResponse StopKeyUsage = StopKeyUsageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopKeyUsageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Key")
      )

instance Prelude.Hashable StopKeyUsage where
  hashWithSalt _salt StopKeyUsage' {..} =
    _salt `Prelude.hashWithSalt` keyIdentifier

instance Prelude.NFData StopKeyUsage where
  rnf StopKeyUsage' {..} = Prelude.rnf keyIdentifier

instance Data.ToHeaders StopKeyUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.StopKeyUsage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopKeyUsage where
  toJSON StopKeyUsage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KeyIdentifier" Data..= keyIdentifier)
          ]
      )

instance Data.ToPath StopKeyUsage where
  toPath = Prelude.const "/"

instance Data.ToQuery StopKeyUsage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopKeyUsageResponse' smart constructor.
data StopKeyUsageResponse = StopKeyUsageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @KeyARN@ of the key.
    key :: Key
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopKeyUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopKeyUsageResponse_httpStatus' - The response's http status code.
--
-- 'key', 'stopKeyUsageResponse_key' - The @KeyARN@ of the key.
newStopKeyUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'key'
  Key ->
  StopKeyUsageResponse
newStopKeyUsageResponse pHttpStatus_ pKey_ =
  StopKeyUsageResponse'
    { httpStatus = pHttpStatus_,
      key = pKey_
    }

-- | The response's http status code.
stopKeyUsageResponse_httpStatus :: Lens.Lens' StopKeyUsageResponse Prelude.Int
stopKeyUsageResponse_httpStatus = Lens.lens (\StopKeyUsageResponse' {httpStatus} -> httpStatus) (\s@StopKeyUsageResponse' {} a -> s {httpStatus = a} :: StopKeyUsageResponse)

-- | The @KeyARN@ of the key.
stopKeyUsageResponse_key :: Lens.Lens' StopKeyUsageResponse Key
stopKeyUsageResponse_key = Lens.lens (\StopKeyUsageResponse' {key} -> key) (\s@StopKeyUsageResponse' {} a -> s {key = a} :: StopKeyUsageResponse)

instance Prelude.NFData StopKeyUsageResponse where
  rnf StopKeyUsageResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf key
