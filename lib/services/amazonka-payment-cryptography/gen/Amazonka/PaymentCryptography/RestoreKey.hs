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
-- Module      : Amazonka.PaymentCryptography.RestoreKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a scheduled key deletion during the waiting period. Use this
-- operation to restore a @Key@ that is scheduled for deletion.
--
-- During the waiting period, the @KeyState@ is @DELETE_PENDING@ and
-- @deletePendingTimestamp@ contains the date and time after which the
-- @Key@ will be deleted. After @Key@ is restored, the @KeyState@ is
-- @CREATE_COMPLETE@, and the value for @deletePendingTimestamp@ is
-- removed.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   DeleteKey
--
-- -   StartKeyUsage
--
-- -   StopKeyUsage
module Amazonka.PaymentCryptography.RestoreKey
  ( -- * Creating a Request
    RestoreKey (..),
    newRestoreKey,

    -- * Request Lenses
    restoreKey_keyIdentifier,

    -- * Destructuring the Response
    RestoreKeyResponse (..),
    newRestoreKeyResponse,

    -- * Response Lenses
    restoreKeyResponse_httpStatus,
    restoreKeyResponse_key,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreKey' smart constructor.
data RestoreKey = RestoreKey'
  { -- | The @KeyARN@ of the key to be restored within Amazon Web Services
    -- Payment Cryptography.
    keyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyIdentifier', 'restoreKey_keyIdentifier' - The @KeyARN@ of the key to be restored within Amazon Web Services
-- Payment Cryptography.
newRestoreKey ::
  -- | 'keyIdentifier'
  Prelude.Text ->
  RestoreKey
newRestoreKey pKeyIdentifier_ =
  RestoreKey' {keyIdentifier = pKeyIdentifier_}

-- | The @KeyARN@ of the key to be restored within Amazon Web Services
-- Payment Cryptography.
restoreKey_keyIdentifier :: Lens.Lens' RestoreKey Prelude.Text
restoreKey_keyIdentifier = Lens.lens (\RestoreKey' {keyIdentifier} -> keyIdentifier) (\s@RestoreKey' {} a -> s {keyIdentifier = a} :: RestoreKey)

instance Core.AWSRequest RestoreKey where
  type AWSResponse RestoreKey = RestoreKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Key")
      )

instance Prelude.Hashable RestoreKey where
  hashWithSalt _salt RestoreKey' {..} =
    _salt `Prelude.hashWithSalt` keyIdentifier

instance Prelude.NFData RestoreKey where
  rnf RestoreKey' {..} = Prelude.rnf keyIdentifier

instance Data.ToHeaders RestoreKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.RestoreKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RestoreKey where
  toJSON RestoreKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KeyIdentifier" Data..= keyIdentifier)
          ]
      )

instance Data.ToPath RestoreKey where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreKeyResponse' smart constructor.
data RestoreKeyResponse = RestoreKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The key material of the restored key. The @KeyState@ will change to
    -- @CREATE_COMPLETE@ and value for @DeletePendingTimestamp@ gets removed.
    key :: Key
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'restoreKeyResponse_httpStatus' - The response's http status code.
--
-- 'key', 'restoreKeyResponse_key' - The key material of the restored key. The @KeyState@ will change to
-- @CREATE_COMPLETE@ and value for @DeletePendingTimestamp@ gets removed.
newRestoreKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'key'
  Key ->
  RestoreKeyResponse
newRestoreKeyResponse pHttpStatus_ pKey_ =
  RestoreKeyResponse'
    { httpStatus = pHttpStatus_,
      key = pKey_
    }

-- | The response's http status code.
restoreKeyResponse_httpStatus :: Lens.Lens' RestoreKeyResponse Prelude.Int
restoreKeyResponse_httpStatus = Lens.lens (\RestoreKeyResponse' {httpStatus} -> httpStatus) (\s@RestoreKeyResponse' {} a -> s {httpStatus = a} :: RestoreKeyResponse)

-- | The key material of the restored key. The @KeyState@ will change to
-- @CREATE_COMPLETE@ and value for @DeletePendingTimestamp@ gets removed.
restoreKeyResponse_key :: Lens.Lens' RestoreKeyResponse Key
restoreKeyResponse_key = Lens.lens (\RestoreKeyResponse' {key} -> key) (\s@RestoreKeyResponse' {} a -> s {key = a} :: RestoreKeyResponse)

instance Prelude.NFData RestoreKeyResponse where
  rnf RestoreKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf key
