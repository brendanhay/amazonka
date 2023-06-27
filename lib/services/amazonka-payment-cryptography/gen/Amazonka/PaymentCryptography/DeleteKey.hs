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
-- Module      : Amazonka.PaymentCryptography.DeleteKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the key material and all metadata associated with Amazon Web
-- Services Payment Cryptography key.
--
-- Key deletion is irreversible. After a key is deleted, you can\'t perform
-- cryptographic operations using the key. For example, you can\'t decrypt
-- data that was encrypted by a deleted Amazon Web Services Payment
-- Cryptography key, and the data may become unrecoverable. Because key
-- deletion is destructive, Amazon Web Services Payment Cryptography has a
-- safety mechanism to prevent accidental deletion of a key. When you call
-- this operation, Amazon Web Services Payment Cryptography disables the
-- specified key but doesn\'t delete it until after a waiting period. The
-- default waiting period is 7 days. To set a different waiting period, set
-- @DeleteKeyInDays@. During the waiting period, the @KeyState@ is
-- @DELETE_PENDING@. After the key is deleted, the @KeyState@ is
-- @DELETE_COMPLETE@.
--
-- If you delete key material, you can use ImportKey to reimport the same
-- key material into the Amazon Web Services Payment Cryptography key.
--
-- You should delete a key only when you are sure that you don\'t need to
-- use it anymore and no other parties are utilizing this key. If you
-- aren\'t sure, consider deactivating it instead by calling StopKeyUsage.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   RestoreKey
--
-- -   StartKeyUsage
--
-- -   StopKeyUsage
module Amazonka.PaymentCryptography.DeleteKey
  ( -- * Creating a Request
    DeleteKey (..),
    newDeleteKey,

    -- * Request Lenses
    deleteKey_deleteKeyInDays,
    deleteKey_keyIdentifier,

    -- * Destructuring the Response
    DeleteKeyResponse (..),
    newDeleteKeyResponse,

    -- * Response Lenses
    deleteKeyResponse_httpStatus,
    deleteKeyResponse_key,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKey' smart constructor.
data DeleteKey = DeleteKey'
  { -- | The waiting period for key deletion. The default value is seven days.
    deleteKeyInDays :: Prelude.Maybe Prelude.Natural,
    -- | The @KeyARN@ of the key that is scheduled for deletion.
    keyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteKeyInDays', 'deleteKey_deleteKeyInDays' - The waiting period for key deletion. The default value is seven days.
--
-- 'keyIdentifier', 'deleteKey_keyIdentifier' - The @KeyARN@ of the key that is scheduled for deletion.
newDeleteKey ::
  -- | 'keyIdentifier'
  Prelude.Text ->
  DeleteKey
newDeleteKey pKeyIdentifier_ =
  DeleteKey'
    { deleteKeyInDays = Prelude.Nothing,
      keyIdentifier = pKeyIdentifier_
    }

-- | The waiting period for key deletion. The default value is seven days.
deleteKey_deleteKeyInDays :: Lens.Lens' DeleteKey (Prelude.Maybe Prelude.Natural)
deleteKey_deleteKeyInDays = Lens.lens (\DeleteKey' {deleteKeyInDays} -> deleteKeyInDays) (\s@DeleteKey' {} a -> s {deleteKeyInDays = a} :: DeleteKey)

-- | The @KeyARN@ of the key that is scheduled for deletion.
deleteKey_keyIdentifier :: Lens.Lens' DeleteKey Prelude.Text
deleteKey_keyIdentifier = Lens.lens (\DeleteKey' {keyIdentifier} -> keyIdentifier) (\s@DeleteKey' {} a -> s {keyIdentifier = a} :: DeleteKey)

instance Core.AWSRequest DeleteKey where
  type AWSResponse DeleteKey = DeleteKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Key")
      )

instance Prelude.Hashable DeleteKey where
  hashWithSalt _salt DeleteKey' {..} =
    _salt
      `Prelude.hashWithSalt` deleteKeyInDays
      `Prelude.hashWithSalt` keyIdentifier

instance Prelude.NFData DeleteKey where
  rnf DeleteKey' {..} =
    Prelude.rnf deleteKeyInDays
      `Prelude.seq` Prelude.rnf keyIdentifier

instance Data.ToHeaders DeleteKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.DeleteKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteKey where
  toJSON DeleteKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteKeyInDays" Data..=)
              Prelude.<$> deleteKeyInDays,
            Prelude.Just
              ("KeyIdentifier" Data..= keyIdentifier)
          ]
      )

instance Data.ToPath DeleteKey where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteKeyResponse' smart constructor.
data DeleteKeyResponse = DeleteKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @KeyARN@ of the key that is scheduled for deletion.
    key :: Key
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteKeyResponse_httpStatus' - The response's http status code.
--
-- 'key', 'deleteKeyResponse_key' - The @KeyARN@ of the key that is scheduled for deletion.
newDeleteKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'key'
  Key ->
  DeleteKeyResponse
newDeleteKeyResponse pHttpStatus_ pKey_ =
  DeleteKeyResponse'
    { httpStatus = pHttpStatus_,
      key = pKey_
    }

-- | The response's http status code.
deleteKeyResponse_httpStatus :: Lens.Lens' DeleteKeyResponse Prelude.Int
deleteKeyResponse_httpStatus = Lens.lens (\DeleteKeyResponse' {httpStatus} -> httpStatus) (\s@DeleteKeyResponse' {} a -> s {httpStatus = a} :: DeleteKeyResponse)

-- | The @KeyARN@ of the key that is scheduled for deletion.
deleteKeyResponse_key :: Lens.Lens' DeleteKeyResponse Key
deleteKeyResponse_key = Lens.lens (\DeleteKeyResponse' {key} -> key) (\s@DeleteKeyResponse' {} a -> s {key = a} :: DeleteKeyResponse)

instance Prelude.NFData DeleteKeyResponse where
  rnf DeleteKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf key
