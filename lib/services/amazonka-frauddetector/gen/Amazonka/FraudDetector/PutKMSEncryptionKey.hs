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
-- Module      : Amazonka.FraudDetector.PutKMSEncryptionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies the KMS key to be used to encrypt content in Amazon Fraud
-- Detector.
module Amazonka.FraudDetector.PutKMSEncryptionKey
  ( -- * Creating a Request
    PutKMSEncryptionKey (..),
    newPutKMSEncryptionKey,

    -- * Request Lenses
    putKMSEncryptionKey_kmsEncryptionKeyArn,

    -- * Destructuring the Response
    PutKMSEncryptionKeyResponse (..),
    newPutKMSEncryptionKeyResponse,

    -- * Response Lenses
    putKMSEncryptionKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutKMSEncryptionKey' smart constructor.
data PutKMSEncryptionKey = PutKMSEncryptionKey'
  { -- | The KMS encryption key ARN.
    --
    -- The KMS key must be single-Region key. Amazon Fraud Detector does not
    -- support multi-Region KMS key.
    kmsEncryptionKeyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutKMSEncryptionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsEncryptionKeyArn', 'putKMSEncryptionKey_kmsEncryptionKeyArn' - The KMS encryption key ARN.
--
-- The KMS key must be single-Region key. Amazon Fraud Detector does not
-- support multi-Region KMS key.
newPutKMSEncryptionKey ::
  -- | 'kmsEncryptionKeyArn'
  Prelude.Text ->
  PutKMSEncryptionKey
newPutKMSEncryptionKey pKmsEncryptionKeyArn_ =
  PutKMSEncryptionKey'
    { kmsEncryptionKeyArn =
        pKmsEncryptionKeyArn_
    }

-- | The KMS encryption key ARN.
--
-- The KMS key must be single-Region key. Amazon Fraud Detector does not
-- support multi-Region KMS key.
putKMSEncryptionKey_kmsEncryptionKeyArn :: Lens.Lens' PutKMSEncryptionKey Prelude.Text
putKMSEncryptionKey_kmsEncryptionKeyArn = Lens.lens (\PutKMSEncryptionKey' {kmsEncryptionKeyArn} -> kmsEncryptionKeyArn) (\s@PutKMSEncryptionKey' {} a -> s {kmsEncryptionKeyArn = a} :: PutKMSEncryptionKey)

instance Core.AWSRequest PutKMSEncryptionKey where
  type
    AWSResponse PutKMSEncryptionKey =
      PutKMSEncryptionKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutKMSEncryptionKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutKMSEncryptionKey where
  hashWithSalt _salt PutKMSEncryptionKey' {..} =
    _salt `Prelude.hashWithSalt` kmsEncryptionKeyArn

instance Prelude.NFData PutKMSEncryptionKey where
  rnf PutKMSEncryptionKey' {..} =
    Prelude.rnf kmsEncryptionKeyArn

instance Data.ToHeaders PutKMSEncryptionKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.PutKMSEncryptionKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutKMSEncryptionKey where
  toJSON PutKMSEncryptionKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("kmsEncryptionKeyArn" Data..= kmsEncryptionKeyArn)
          ]
      )

instance Data.ToPath PutKMSEncryptionKey where
  toPath = Prelude.const "/"

instance Data.ToQuery PutKMSEncryptionKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutKMSEncryptionKeyResponse' smart constructor.
data PutKMSEncryptionKeyResponse = PutKMSEncryptionKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutKMSEncryptionKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putKMSEncryptionKeyResponse_httpStatus' - The response's http status code.
newPutKMSEncryptionKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutKMSEncryptionKeyResponse
newPutKMSEncryptionKeyResponse pHttpStatus_ =
  PutKMSEncryptionKeyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putKMSEncryptionKeyResponse_httpStatus :: Lens.Lens' PutKMSEncryptionKeyResponse Prelude.Int
putKMSEncryptionKeyResponse_httpStatus = Lens.lens (\PutKMSEncryptionKeyResponse' {httpStatus} -> httpStatus) (\s@PutKMSEncryptionKeyResponse' {} a -> s {httpStatus = a} :: PutKMSEncryptionKeyResponse)

instance Prelude.NFData PutKMSEncryptionKeyResponse where
  rnf PutKMSEncryptionKeyResponse' {..} =
    Prelude.rnf httpStatus
