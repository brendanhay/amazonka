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
-- Module      : Amazonka.Inspector2.ResetEncryptionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an encryption key. After the key is reset your resources will be
-- encrypted by an Amazon Web Services owned key.
module Amazonka.Inspector2.ResetEncryptionKey
  ( -- * Creating a Request
    ResetEncryptionKey (..),
    newResetEncryptionKey,

    -- * Request Lenses
    resetEncryptionKey_resourceType,
    resetEncryptionKey_scanType,

    -- * Destructuring the Response
    ResetEncryptionKeyResponse (..),
    newResetEncryptionKeyResponse,

    -- * Response Lenses
    resetEncryptionKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetEncryptionKey' smart constructor.
data ResetEncryptionKey = ResetEncryptionKey'
  { -- | The resource type the key encrypts.
    resourceType :: ResourceType,
    -- | The scan type the key encrypts.
    scanType :: ScanType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetEncryptionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'resetEncryptionKey_resourceType' - The resource type the key encrypts.
--
-- 'scanType', 'resetEncryptionKey_scanType' - The scan type the key encrypts.
newResetEncryptionKey ::
  -- | 'resourceType'
  ResourceType ->
  -- | 'scanType'
  ScanType ->
  ResetEncryptionKey
newResetEncryptionKey pResourceType_ pScanType_ =
  ResetEncryptionKey'
    { resourceType = pResourceType_,
      scanType = pScanType_
    }

-- | The resource type the key encrypts.
resetEncryptionKey_resourceType :: Lens.Lens' ResetEncryptionKey ResourceType
resetEncryptionKey_resourceType = Lens.lens (\ResetEncryptionKey' {resourceType} -> resourceType) (\s@ResetEncryptionKey' {} a -> s {resourceType = a} :: ResetEncryptionKey)

-- | The scan type the key encrypts.
resetEncryptionKey_scanType :: Lens.Lens' ResetEncryptionKey ScanType
resetEncryptionKey_scanType = Lens.lens (\ResetEncryptionKey' {scanType} -> scanType) (\s@ResetEncryptionKey' {} a -> s {scanType = a} :: ResetEncryptionKey)

instance Core.AWSRequest ResetEncryptionKey where
  type
    AWSResponse ResetEncryptionKey =
      ResetEncryptionKeyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ResetEncryptionKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetEncryptionKey where
  hashWithSalt _salt ResetEncryptionKey' {..} =
    _salt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` scanType

instance Prelude.NFData ResetEncryptionKey where
  rnf ResetEncryptionKey' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf scanType

instance Data.ToHeaders ResetEncryptionKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResetEncryptionKey where
  toJSON ResetEncryptionKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceType" Data..= resourceType),
            Prelude.Just ("scanType" Data..= scanType)
          ]
      )

instance Data.ToPath ResetEncryptionKey where
  toPath = Prelude.const "/encryptionkey/reset"

instance Data.ToQuery ResetEncryptionKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetEncryptionKeyResponse' smart constructor.
data ResetEncryptionKeyResponse = ResetEncryptionKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetEncryptionKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'resetEncryptionKeyResponse_httpStatus' - The response's http status code.
newResetEncryptionKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetEncryptionKeyResponse
newResetEncryptionKeyResponse pHttpStatus_ =
  ResetEncryptionKeyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
resetEncryptionKeyResponse_httpStatus :: Lens.Lens' ResetEncryptionKeyResponse Prelude.Int
resetEncryptionKeyResponse_httpStatus = Lens.lens (\ResetEncryptionKeyResponse' {httpStatus} -> httpStatus) (\s@ResetEncryptionKeyResponse' {} a -> s {httpStatus = a} :: ResetEncryptionKeyResponse)

instance Prelude.NFData ResetEncryptionKeyResponse where
  rnf ResetEncryptionKeyResponse' {..} =
    Prelude.rnf httpStatus
