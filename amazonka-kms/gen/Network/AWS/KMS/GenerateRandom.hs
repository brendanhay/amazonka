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
-- Module      : Network.AWS.KMS.GenerateRandom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a random byte string that is cryptographically secure.
--
-- By default, the random byte string is generated in KMS. To generate the
-- byte string in the CloudHSM cluster that is associated with a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>,
-- specify the custom key store ID.
--
-- Applications in Amazon Web Services Nitro Enclaves can call this
-- operation by using the
-- <https://github.com/aws/aws-nitro-enclaves-sdk-c Amazon Web Services Nitro Enclaves Development Kit>.
-- For information about the supporting parameters, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves use KMS>
-- in the /Key Management Service Developer Guide/.
--
-- For more information about entropy and random number generation, see
-- <https://docs.aws.amazon.com/kms/latest/cryptographic-details/ Key Management Service Cryptographic Details>.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GenerateRandom>
-- (IAM policy)
module Network.AWS.KMS.GenerateRandom
  ( -- * Creating a Request
    GenerateRandom (..),
    newGenerateRandom,

    -- * Request Lenses
    generateRandom_customKeyStoreId,
    generateRandom_numberOfBytes,

    -- * Destructuring the Response
    GenerateRandomResponse (..),
    newGenerateRandomResponse,

    -- * Response Lenses
    generateRandomResponse_plaintext,
    generateRandomResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGenerateRandom' smart constructor.
data GenerateRandom = GenerateRandom'
  { -- | Generates the random byte string in the CloudHSM cluster that is
    -- associated with the specified
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
    -- To find the ID of a custom key store, use the DescribeCustomKeyStores
    -- operation.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | The length of the byte string.
    numberOfBytes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateRandom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStoreId', 'generateRandom_customKeyStoreId' - Generates the random byte string in the CloudHSM cluster that is
-- associated with the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- To find the ID of a custom key store, use the DescribeCustomKeyStores
-- operation.
--
-- 'numberOfBytes', 'generateRandom_numberOfBytes' - The length of the byte string.
newGenerateRandom ::
  GenerateRandom
newGenerateRandom =
  GenerateRandom'
    { customKeyStoreId = Prelude.Nothing,
      numberOfBytes = Prelude.Nothing
    }

-- | Generates the random byte string in the CloudHSM cluster that is
-- associated with the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- To find the ID of a custom key store, use the DescribeCustomKeyStores
-- operation.
generateRandom_customKeyStoreId :: Lens.Lens' GenerateRandom (Prelude.Maybe Prelude.Text)
generateRandom_customKeyStoreId = Lens.lens (\GenerateRandom' {customKeyStoreId} -> customKeyStoreId) (\s@GenerateRandom' {} a -> s {customKeyStoreId = a} :: GenerateRandom)

-- | The length of the byte string.
generateRandom_numberOfBytes :: Lens.Lens' GenerateRandom (Prelude.Maybe Prelude.Natural)
generateRandom_numberOfBytes = Lens.lens (\GenerateRandom' {numberOfBytes} -> numberOfBytes) (\s@GenerateRandom' {} a -> s {numberOfBytes = a} :: GenerateRandom)

instance Core.AWSRequest GenerateRandom where
  type
    AWSResponse GenerateRandom =
      GenerateRandomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateRandomResponse'
            Prelude.<$> (x Core..?> "Plaintext")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateRandom

instance Prelude.NFData GenerateRandom

instance Core.ToHeaders GenerateRandom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.GenerateRandom" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GenerateRandom where
  toJSON GenerateRandom' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomKeyStoreId" Core..=)
              Prelude.<$> customKeyStoreId,
            ("NumberOfBytes" Core..=) Prelude.<$> numberOfBytes
          ]
      )

instance Core.ToPath GenerateRandom where
  toPath = Prelude.const "/"

instance Core.ToQuery GenerateRandom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateRandomResponse' smart constructor.
data GenerateRandomResponse = GenerateRandomResponse'
  { -- | The random byte string. When you use the HTTP API or the Amazon Web
    -- Services CLI, the value is Base64-encoded. Otherwise, it is not
    -- Base64-encoded.
    plaintext :: Prelude.Maybe (Core.Sensitive Core.Base64),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateRandomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'plaintext', 'generateRandomResponse_plaintext' - The random byte string. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'httpStatus', 'generateRandomResponse_httpStatus' - The response's http status code.
newGenerateRandomResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateRandomResponse
newGenerateRandomResponse pHttpStatus_ =
  GenerateRandomResponse'
    { plaintext =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The random byte string. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateRandomResponse_plaintext :: Lens.Lens' GenerateRandomResponse (Prelude.Maybe Prelude.ByteString)
generateRandomResponse_plaintext = Lens.lens (\GenerateRandomResponse' {plaintext} -> plaintext) (\s@GenerateRandomResponse' {} a -> s {plaintext = a} :: GenerateRandomResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Core._Base64)

-- | The response's http status code.
generateRandomResponse_httpStatus :: Lens.Lens' GenerateRandomResponse Prelude.Int
generateRandomResponse_httpStatus = Lens.lens (\GenerateRandomResponse' {httpStatus} -> httpStatus) (\s@GenerateRandomResponse' {} a -> s {httpStatus = a} :: GenerateRandomResponse)

instance Prelude.NFData GenerateRandomResponse
