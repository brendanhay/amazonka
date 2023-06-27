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
-- Module      : Amazonka.Inspector2.GetEncryptionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an encryption key.
module Amazonka.Inspector2.GetEncryptionKey
  ( -- * Creating a Request
    GetEncryptionKey (..),
    newGetEncryptionKey,

    -- * Request Lenses
    getEncryptionKey_resourceType,
    getEncryptionKey_scanType,

    -- * Destructuring the Response
    GetEncryptionKeyResponse (..),
    newGetEncryptionKeyResponse,

    -- * Response Lenses
    getEncryptionKeyResponse_httpStatus,
    getEncryptionKeyResponse_kmsKeyId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEncryptionKey' smart constructor.
data GetEncryptionKey = GetEncryptionKey'
  { -- | The resource type the key encrypts.
    resourceType :: ResourceType,
    -- | The scan type the key encrypts.
    scanType :: ScanType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEncryptionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'getEncryptionKey_resourceType' - The resource type the key encrypts.
--
-- 'scanType', 'getEncryptionKey_scanType' - The scan type the key encrypts.
newGetEncryptionKey ::
  -- | 'resourceType'
  ResourceType ->
  -- | 'scanType'
  ScanType ->
  GetEncryptionKey
newGetEncryptionKey pResourceType_ pScanType_ =
  GetEncryptionKey'
    { resourceType = pResourceType_,
      scanType = pScanType_
    }

-- | The resource type the key encrypts.
getEncryptionKey_resourceType :: Lens.Lens' GetEncryptionKey ResourceType
getEncryptionKey_resourceType = Lens.lens (\GetEncryptionKey' {resourceType} -> resourceType) (\s@GetEncryptionKey' {} a -> s {resourceType = a} :: GetEncryptionKey)

-- | The scan type the key encrypts.
getEncryptionKey_scanType :: Lens.Lens' GetEncryptionKey ScanType
getEncryptionKey_scanType = Lens.lens (\GetEncryptionKey' {scanType} -> scanType) (\s@GetEncryptionKey' {} a -> s {scanType = a} :: GetEncryptionKey)

instance Core.AWSRequest GetEncryptionKey where
  type
    AWSResponse GetEncryptionKey =
      GetEncryptionKeyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEncryptionKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "kmsKeyId")
      )

instance Prelude.Hashable GetEncryptionKey where
  hashWithSalt _salt GetEncryptionKey' {..} =
    _salt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` scanType

instance Prelude.NFData GetEncryptionKey where
  rnf GetEncryptionKey' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf scanType

instance Data.ToHeaders GetEncryptionKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEncryptionKey where
  toPath = Prelude.const "/encryptionkey/get"

instance Data.ToQuery GetEncryptionKey where
  toQuery GetEncryptionKey' {..} =
    Prelude.mconcat
      [ "resourceType" Data.=: resourceType,
        "scanType" Data.=: scanType
      ]

-- | /See:/ 'newGetEncryptionKeyResponse' smart constructor.
data GetEncryptionKeyResponse = GetEncryptionKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A kms key ID.
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEncryptionKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getEncryptionKeyResponse_httpStatus' - The response's http status code.
--
-- 'kmsKeyId', 'getEncryptionKeyResponse_kmsKeyId' - A kms key ID.
newGetEncryptionKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  GetEncryptionKeyResponse
newGetEncryptionKeyResponse pHttpStatus_ pKmsKeyId_ =
  GetEncryptionKeyResponse'
    { httpStatus =
        pHttpStatus_,
      kmsKeyId = pKmsKeyId_
    }

-- | The response's http status code.
getEncryptionKeyResponse_httpStatus :: Lens.Lens' GetEncryptionKeyResponse Prelude.Int
getEncryptionKeyResponse_httpStatus = Lens.lens (\GetEncryptionKeyResponse' {httpStatus} -> httpStatus) (\s@GetEncryptionKeyResponse' {} a -> s {httpStatus = a} :: GetEncryptionKeyResponse)

-- | A kms key ID.
getEncryptionKeyResponse_kmsKeyId :: Lens.Lens' GetEncryptionKeyResponse Prelude.Text
getEncryptionKeyResponse_kmsKeyId = Lens.lens (\GetEncryptionKeyResponse' {kmsKeyId} -> kmsKeyId) (\s@GetEncryptionKeyResponse' {} a -> s {kmsKeyId = a} :: GetEncryptionKeyResponse)

instance Prelude.NFData GetEncryptionKeyResponse where
  rnf GetEncryptionKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf kmsKeyId
