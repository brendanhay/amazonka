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
-- By default, the random byte string is generated in AWS KMS. To generate
-- the byte string in the AWS CloudHSM cluster that is associated with a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>,
-- specify the custom key store ID.
--
-- For more information about entropy and random number generation, see the
-- <https://d0.awsstatic.com/whitepapers/KMS-Cryptographic-Details.pdf AWS Key Management Service Cryptographic Details>
-- whitepaper.
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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGenerateRandom' smart constructor.
data GenerateRandom = GenerateRandom'
  { -- | Generates the random byte string in the AWS CloudHSM cluster that is
    -- associated with the specified
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
    -- To find the ID of a custom key store, use the DescribeCustomKeyStores
    -- operation.
    customKeyStoreId :: Core.Maybe Core.Text,
    -- | The length of the byte string.
    numberOfBytes :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenerateRandom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStoreId', 'generateRandom_customKeyStoreId' - Generates the random byte string in the AWS CloudHSM cluster that is
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
    { customKeyStoreId = Core.Nothing,
      numberOfBytes = Core.Nothing
    }

-- | Generates the random byte string in the AWS CloudHSM cluster that is
-- associated with the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- To find the ID of a custom key store, use the DescribeCustomKeyStores
-- operation.
generateRandom_customKeyStoreId :: Lens.Lens' GenerateRandom (Core.Maybe Core.Text)
generateRandom_customKeyStoreId = Lens.lens (\GenerateRandom' {customKeyStoreId} -> customKeyStoreId) (\s@GenerateRandom' {} a -> s {customKeyStoreId = a} :: GenerateRandom)

-- | The length of the byte string.
generateRandom_numberOfBytes :: Lens.Lens' GenerateRandom (Core.Maybe Core.Natural)
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
            Core.<$> (x Core..?> "Plaintext")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GenerateRandom

instance Core.NFData GenerateRandom

instance Core.ToHeaders GenerateRandom where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.GenerateRandom" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GenerateRandom where
  toJSON GenerateRandom' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CustomKeyStoreId" Core..=)
              Core.<$> customKeyStoreId,
            ("NumberOfBytes" Core..=) Core.<$> numberOfBytes
          ]
      )

instance Core.ToPath GenerateRandom where
  toPath = Core.const "/"

instance Core.ToQuery GenerateRandom where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGenerateRandomResponse' smart constructor.
data GenerateRandomResponse = GenerateRandomResponse'
  { -- | The random byte string. When you use the HTTP API or the AWS CLI, the
    -- value is Base64-encoded. Otherwise, it is not Base64-encoded.
    plaintext :: Core.Maybe (Core.Sensitive Core.Base64),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenerateRandomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'plaintext', 'generateRandomResponse_plaintext' - The random byte string. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'httpStatus', 'generateRandomResponse_httpStatus' - The response's http status code.
newGenerateRandomResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GenerateRandomResponse
newGenerateRandomResponse pHttpStatus_ =
  GenerateRandomResponse'
    { plaintext = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The random byte string. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateRandomResponse_plaintext :: Lens.Lens' GenerateRandomResponse (Core.Maybe Core.ByteString)
generateRandomResponse_plaintext = Lens.lens (\GenerateRandomResponse' {plaintext} -> plaintext) (\s@GenerateRandomResponse' {} a -> s {plaintext = a} :: GenerateRandomResponse) Core.. Lens.mapping (Core._Sensitive Core.. Core._Base64)

-- | The response's http status code.
generateRandomResponse_httpStatus :: Lens.Lens' GenerateRandomResponse Core.Int
generateRandomResponse_httpStatus = Lens.lens (\GenerateRandomResponse' {httpStatus} -> httpStatus) (\s@GenerateRandomResponse' {} a -> s {httpStatus = a} :: GenerateRandomResponse)

instance Core.NFData GenerateRandomResponse
