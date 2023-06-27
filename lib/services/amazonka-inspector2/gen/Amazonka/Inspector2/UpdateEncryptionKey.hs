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
-- Module      : Amazonka.Inspector2.UpdateEncryptionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an encryption key. A @ResourceNotFoundException@ means that an
-- AWS owned key is being used for encryption.
module Amazonka.Inspector2.UpdateEncryptionKey
  ( -- * Creating a Request
    UpdateEncryptionKey (..),
    newUpdateEncryptionKey,

    -- * Request Lenses
    updateEncryptionKey_kmsKeyId,
    updateEncryptionKey_resourceType,
    updateEncryptionKey_scanType,

    -- * Destructuring the Response
    UpdateEncryptionKeyResponse (..),
    newUpdateEncryptionKeyResponse,

    -- * Response Lenses
    updateEncryptionKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEncryptionKey' smart constructor.
data UpdateEncryptionKey = UpdateEncryptionKey'
  { -- | A KMS key ID for the encryption key.
    kmsKeyId :: Prelude.Text,
    -- | The resource type for the encryption key.
    resourceType :: ResourceType,
    -- | The scan type for the encryption key.
    scanType :: ScanType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEncryptionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'updateEncryptionKey_kmsKeyId' - A KMS key ID for the encryption key.
--
-- 'resourceType', 'updateEncryptionKey_resourceType' - The resource type for the encryption key.
--
-- 'scanType', 'updateEncryptionKey_scanType' - The scan type for the encryption key.
newUpdateEncryptionKey ::
  -- | 'kmsKeyId'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'scanType'
  ScanType ->
  UpdateEncryptionKey
newUpdateEncryptionKey
  pKmsKeyId_
  pResourceType_
  pScanType_ =
    UpdateEncryptionKey'
      { kmsKeyId = pKmsKeyId_,
        resourceType = pResourceType_,
        scanType = pScanType_
      }

-- | A KMS key ID for the encryption key.
updateEncryptionKey_kmsKeyId :: Lens.Lens' UpdateEncryptionKey Prelude.Text
updateEncryptionKey_kmsKeyId = Lens.lens (\UpdateEncryptionKey' {kmsKeyId} -> kmsKeyId) (\s@UpdateEncryptionKey' {} a -> s {kmsKeyId = a} :: UpdateEncryptionKey)

-- | The resource type for the encryption key.
updateEncryptionKey_resourceType :: Lens.Lens' UpdateEncryptionKey ResourceType
updateEncryptionKey_resourceType = Lens.lens (\UpdateEncryptionKey' {resourceType} -> resourceType) (\s@UpdateEncryptionKey' {} a -> s {resourceType = a} :: UpdateEncryptionKey)

-- | The scan type for the encryption key.
updateEncryptionKey_scanType :: Lens.Lens' UpdateEncryptionKey ScanType
updateEncryptionKey_scanType = Lens.lens (\UpdateEncryptionKey' {scanType} -> scanType) (\s@UpdateEncryptionKey' {} a -> s {scanType = a} :: UpdateEncryptionKey)

instance Core.AWSRequest UpdateEncryptionKey where
  type
    AWSResponse UpdateEncryptionKey =
      UpdateEncryptionKeyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEncryptionKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEncryptionKey where
  hashWithSalt _salt UpdateEncryptionKey' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` scanType

instance Prelude.NFData UpdateEncryptionKey where
  rnf UpdateEncryptionKey' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf scanType

instance Data.ToHeaders UpdateEncryptionKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEncryptionKey where
  toJSON UpdateEncryptionKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("kmsKeyId" Data..= kmsKeyId),
            Prelude.Just ("resourceType" Data..= resourceType),
            Prelude.Just ("scanType" Data..= scanType)
          ]
      )

instance Data.ToPath UpdateEncryptionKey where
  toPath = Prelude.const "/encryptionkey/update"

instance Data.ToQuery UpdateEncryptionKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEncryptionKeyResponse' smart constructor.
data UpdateEncryptionKeyResponse = UpdateEncryptionKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEncryptionKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEncryptionKeyResponse_httpStatus' - The response's http status code.
newUpdateEncryptionKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEncryptionKeyResponse
newUpdateEncryptionKeyResponse pHttpStatus_ =
  UpdateEncryptionKeyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateEncryptionKeyResponse_httpStatus :: Lens.Lens' UpdateEncryptionKeyResponse Prelude.Int
updateEncryptionKeyResponse_httpStatus = Lens.lens (\UpdateEncryptionKeyResponse' {httpStatus} -> httpStatus) (\s@UpdateEncryptionKeyResponse' {} a -> s {httpStatus = a} :: UpdateEncryptionKeyResponse)

instance Prelude.NFData UpdateEncryptionKeyResponse where
  rnf UpdateEncryptionKeyResponse' {..} =
    Prelude.rnf httpStatus
