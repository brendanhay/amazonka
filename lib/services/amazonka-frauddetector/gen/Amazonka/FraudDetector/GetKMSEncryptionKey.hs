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
-- Module      : Amazonka.FraudDetector.GetKMSEncryptionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the encryption key if a KMS key has been specified to be used to
-- encrypt content in Amazon Fraud Detector.
module Amazonka.FraudDetector.GetKMSEncryptionKey
  ( -- * Creating a Request
    GetKMSEncryptionKey (..),
    newGetKMSEncryptionKey,

    -- * Destructuring the Response
    GetKMSEncryptionKeyResponse (..),
    newGetKMSEncryptionKeyResponse,

    -- * Response Lenses
    getKMSEncryptionKeyResponse_kmsKey,
    getKMSEncryptionKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKMSEncryptionKey' smart constructor.
data GetKMSEncryptionKey = GetKMSEncryptionKey'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKMSEncryptionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetKMSEncryptionKey ::
  GetKMSEncryptionKey
newGetKMSEncryptionKey = GetKMSEncryptionKey'

instance Core.AWSRequest GetKMSEncryptionKey where
  type
    AWSResponse GetKMSEncryptionKey =
      GetKMSEncryptionKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKMSEncryptionKeyResponse'
            Prelude.<$> (x Data..?> "kmsKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKMSEncryptionKey where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetKMSEncryptionKey where
  rnf _ = ()

instance Data.ToHeaders GetKMSEncryptionKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetKMSEncryptionKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetKMSEncryptionKey where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetKMSEncryptionKey where
  toPath = Prelude.const "/"

instance Data.ToQuery GetKMSEncryptionKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKMSEncryptionKeyResponse' smart constructor.
data GetKMSEncryptionKeyResponse = GetKMSEncryptionKeyResponse'
  { -- | The KMS encryption key.
    kmsKey :: Prelude.Maybe KMSKey,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKMSEncryptionKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'getKMSEncryptionKeyResponse_kmsKey' - The KMS encryption key.
--
-- 'httpStatus', 'getKMSEncryptionKeyResponse_httpStatus' - The response's http status code.
newGetKMSEncryptionKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKMSEncryptionKeyResponse
newGetKMSEncryptionKeyResponse pHttpStatus_ =
  GetKMSEncryptionKeyResponse'
    { kmsKey =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The KMS encryption key.
getKMSEncryptionKeyResponse_kmsKey :: Lens.Lens' GetKMSEncryptionKeyResponse (Prelude.Maybe KMSKey)
getKMSEncryptionKeyResponse_kmsKey = Lens.lens (\GetKMSEncryptionKeyResponse' {kmsKey} -> kmsKey) (\s@GetKMSEncryptionKeyResponse' {} a -> s {kmsKey = a} :: GetKMSEncryptionKeyResponse)

-- | The response's http status code.
getKMSEncryptionKeyResponse_httpStatus :: Lens.Lens' GetKMSEncryptionKeyResponse Prelude.Int
getKMSEncryptionKeyResponse_httpStatus = Lens.lens (\GetKMSEncryptionKeyResponse' {httpStatus} -> httpStatus) (\s@GetKMSEncryptionKeyResponse' {} a -> s {httpStatus = a} :: GetKMSEncryptionKeyResponse)

instance Prelude.NFData GetKMSEncryptionKeyResponse where
  rnf GetKMSEncryptionKeyResponse' {..} =
    Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf httpStatus
