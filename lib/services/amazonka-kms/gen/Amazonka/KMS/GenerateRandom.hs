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
-- Module      : Amazonka.KMS.GenerateRandom
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a random byte string that is cryptographically secure.
--
-- You must use the @NumberOfBytes@ parameter to specify the length of the
-- random byte string. There is no default value for string length.
--
-- By default, the random byte string is generated in KMS. To generate the
-- byte string in the CloudHSM cluster associated with an CloudHSM key
-- store, use the @CustomKeyStoreId@ parameter.
--
-- @GenerateRandom@ also supports
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitro-enclave.html Amazon Web Services Nitro Enclaves>,
-- which provide an isolated compute environment in Amazon EC2. To call
-- @GenerateRandom@ for a Nitro enclave, use the
-- <https://docs.aws.amazon.com/enclaves/latest/user/developing-applications.html#sdk Amazon Web Services Nitro Enclaves SDK>
-- or any Amazon Web Services SDK. Use the @Recipient@ parameter to provide
-- the attestation document for the enclave. Instead of plaintext bytes,
-- the response includes the plaintext bytes encrypted under the public key
-- from the attestation document (@CiphertextForRecipient@).For information
-- about the interaction between KMS and Amazon Web Services Nitro
-- Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/.
--
-- For more information about entropy and random number generation, see
-- <https://docs.aws.amazon.com/kms/latest/cryptographic-details/ Key Management Service Cryptographic Details>.
--
-- __Cross-account use__: Not applicable. @GenerateRandom@ does not use any
-- account-specific resources, such as KMS keys.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GenerateRandom>
-- (IAM policy)
module Amazonka.KMS.GenerateRandom
  ( -- * Creating a Request
    GenerateRandom (..),
    newGenerateRandom,

    -- * Request Lenses
    generateRandom_customKeyStoreId,
    generateRandom_numberOfBytes,
    generateRandom_recipient,

    -- * Destructuring the Response
    GenerateRandomResponse (..),
    newGenerateRandomResponse,

    -- * Response Lenses
    generateRandomResponse_ciphertextForRecipient,
    generateRandomResponse_plaintext,
    generateRandomResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGenerateRandom' smart constructor.
data GenerateRandom = GenerateRandom'
  { -- | Generates the random byte string in the CloudHSM cluster that is
    -- associated with the specified CloudHSM key store. To find the ID of a
    -- custom key store, use the DescribeCustomKeyStores operation.
    --
    -- External key store IDs are not valid for this parameter. If you specify
    -- the ID of an external key store, @GenerateRandom@ throws an
    -- @UnsupportedOperationException@.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | The length of the random byte string. This parameter is required.
    numberOfBytes :: Prelude.Maybe Prelude.Natural,
    -- | A signed
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitro-enclave-how.html#term-attestdoc attestation document>
    -- from an Amazon Web Services Nitro enclave and the encryption algorithm
    -- to use with the enclave\'s public key. The only valid encryption
    -- algorithm is @RSAES_OAEP_SHA_256@.
    --
    -- This parameter only supports attestation documents for Amazon Web
    -- Services Nitro Enclaves. To include this parameter, use the
    -- <https://docs.aws.amazon.com/enclaves/latest/user/developing-applications.html#sdk Amazon Web Services Nitro Enclaves SDK>
    -- or any Amazon Web Services SDK.
    --
    -- When you use this parameter, instead of returning plaintext bytes, KMS
    -- encrypts the plaintext bytes under the public key in the attestation
    -- document, and returns the resulting ciphertext in the
    -- @CiphertextForRecipient@ field in the response. This ciphertext can be
    -- decrypted only with the private key in the enclave. The @Plaintext@
    -- field in the response is null or empty.
    --
    -- For information about the interaction between KMS and Amazon Web
    -- Services Nitro Enclaves, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
    -- in the /Key Management Service Developer Guide/.
    recipient :: Prelude.Maybe RecipientInfo
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
-- associated with the specified CloudHSM key store. To find the ID of a
-- custom key store, use the DescribeCustomKeyStores operation.
--
-- External key store IDs are not valid for this parameter. If you specify
-- the ID of an external key store, @GenerateRandom@ throws an
-- @UnsupportedOperationException@.
--
-- 'numberOfBytes', 'generateRandom_numberOfBytes' - The length of the random byte string. This parameter is required.
--
-- 'recipient', 'generateRandom_recipient' - A signed
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitro-enclave-how.html#term-attestdoc attestation document>
-- from an Amazon Web Services Nitro enclave and the encryption algorithm
-- to use with the enclave\'s public key. The only valid encryption
-- algorithm is @RSAES_OAEP_SHA_256@.
--
-- This parameter only supports attestation documents for Amazon Web
-- Services Nitro Enclaves. To include this parameter, use the
-- <https://docs.aws.amazon.com/enclaves/latest/user/developing-applications.html#sdk Amazon Web Services Nitro Enclaves SDK>
-- or any Amazon Web Services SDK.
--
-- When you use this parameter, instead of returning plaintext bytes, KMS
-- encrypts the plaintext bytes under the public key in the attestation
-- document, and returns the resulting ciphertext in the
-- @CiphertextForRecipient@ field in the response. This ciphertext can be
-- decrypted only with the private key in the enclave. The @Plaintext@
-- field in the response is null or empty.
--
-- For information about the interaction between KMS and Amazon Web
-- Services Nitro Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/.
newGenerateRandom ::
  GenerateRandom
newGenerateRandom =
  GenerateRandom'
    { customKeyStoreId = Prelude.Nothing,
      numberOfBytes = Prelude.Nothing,
      recipient = Prelude.Nothing
    }

-- | Generates the random byte string in the CloudHSM cluster that is
-- associated with the specified CloudHSM key store. To find the ID of a
-- custom key store, use the DescribeCustomKeyStores operation.
--
-- External key store IDs are not valid for this parameter. If you specify
-- the ID of an external key store, @GenerateRandom@ throws an
-- @UnsupportedOperationException@.
generateRandom_customKeyStoreId :: Lens.Lens' GenerateRandom (Prelude.Maybe Prelude.Text)
generateRandom_customKeyStoreId = Lens.lens (\GenerateRandom' {customKeyStoreId} -> customKeyStoreId) (\s@GenerateRandom' {} a -> s {customKeyStoreId = a} :: GenerateRandom)

-- | The length of the random byte string. This parameter is required.
generateRandom_numberOfBytes :: Lens.Lens' GenerateRandom (Prelude.Maybe Prelude.Natural)
generateRandom_numberOfBytes = Lens.lens (\GenerateRandom' {numberOfBytes} -> numberOfBytes) (\s@GenerateRandom' {} a -> s {numberOfBytes = a} :: GenerateRandom)

-- | A signed
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitro-enclave-how.html#term-attestdoc attestation document>
-- from an Amazon Web Services Nitro enclave and the encryption algorithm
-- to use with the enclave\'s public key. The only valid encryption
-- algorithm is @RSAES_OAEP_SHA_256@.
--
-- This parameter only supports attestation documents for Amazon Web
-- Services Nitro Enclaves. To include this parameter, use the
-- <https://docs.aws.amazon.com/enclaves/latest/user/developing-applications.html#sdk Amazon Web Services Nitro Enclaves SDK>
-- or any Amazon Web Services SDK.
--
-- When you use this parameter, instead of returning plaintext bytes, KMS
-- encrypts the plaintext bytes under the public key in the attestation
-- document, and returns the resulting ciphertext in the
-- @CiphertextForRecipient@ field in the response. This ciphertext can be
-- decrypted only with the private key in the enclave. The @Plaintext@
-- field in the response is null or empty.
--
-- For information about the interaction between KMS and Amazon Web
-- Services Nitro Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/.
generateRandom_recipient :: Lens.Lens' GenerateRandom (Prelude.Maybe RecipientInfo)
generateRandom_recipient = Lens.lens (\GenerateRandom' {recipient} -> recipient) (\s@GenerateRandom' {} a -> s {recipient = a} :: GenerateRandom)

instance Core.AWSRequest GenerateRandom where
  type
    AWSResponse GenerateRandom =
      GenerateRandomResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateRandomResponse'
            Prelude.<$> (x Data..?> "CiphertextForRecipient")
            Prelude.<*> (x Data..?> "Plaintext")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateRandom where
  hashWithSalt _salt GenerateRandom' {..} =
    _salt
      `Prelude.hashWithSalt` customKeyStoreId
      `Prelude.hashWithSalt` numberOfBytes
      `Prelude.hashWithSalt` recipient

instance Prelude.NFData GenerateRandom where
  rnf GenerateRandom' {..} =
    Prelude.rnf customKeyStoreId
      `Prelude.seq` Prelude.rnf numberOfBytes
      `Prelude.seq` Prelude.rnf recipient

instance Data.ToHeaders GenerateRandom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.GenerateRandom" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GenerateRandom where
  toJSON GenerateRandom' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomKeyStoreId" Data..=)
              Prelude.<$> customKeyStoreId,
            ("NumberOfBytes" Data..=) Prelude.<$> numberOfBytes,
            ("Recipient" Data..=) Prelude.<$> recipient
          ]
      )

instance Data.ToPath GenerateRandom where
  toPath = Prelude.const "/"

instance Data.ToQuery GenerateRandom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateRandomResponse' smart constructor.
data GenerateRandomResponse = GenerateRandomResponse'
  { -- | The plaintext random bytes encrypted with the public key from the Nitro
    -- enclave. This ciphertext can be decrypted only by using a private key in
    -- the Nitro enclave.
    --
    -- This field is included in the response only when the @Recipient@
    -- parameter in the request includes a valid attestation document from an
    -- Amazon Web Services Nitro enclave. For information about the interaction
    -- between KMS and Amazon Web Services Nitro Enclaves, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
    -- in the /Key Management Service Developer Guide/.
    ciphertextForRecipient :: Prelude.Maybe Data.Base64,
    -- | The random byte string. When you use the HTTP API or the Amazon Web
    -- Services CLI, the value is Base64-encoded. Otherwise, it is not
    -- Base64-encoded.
    --
    -- If the response includes the @CiphertextForRecipient@ field, the
    -- @Plaintext@ field is null or empty.
    plaintext :: Prelude.Maybe (Data.Sensitive Data.Base64),
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
-- 'ciphertextForRecipient', 'generateRandomResponse_ciphertextForRecipient' - The plaintext random bytes encrypted with the public key from the Nitro
-- enclave. This ciphertext can be decrypted only by using a private key in
-- the Nitro enclave.
--
-- This field is included in the response only when the @Recipient@
-- parameter in the request includes a valid attestation document from an
-- Amazon Web Services Nitro enclave. For information about the interaction
-- between KMS and Amazon Web Services Nitro Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'plaintext', 'generateRandomResponse_plaintext' - The random byte string. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.
--
-- If the response includes the @CiphertextForRecipient@ field, the
-- @Plaintext@ field is null or empty.--
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
    { ciphertextForRecipient =
        Prelude.Nothing,
      plaintext = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The plaintext random bytes encrypted with the public key from the Nitro
-- enclave. This ciphertext can be decrypted only by using a private key in
-- the Nitro enclave.
--
-- This field is included in the response only when the @Recipient@
-- parameter in the request includes a valid attestation document from an
-- Amazon Web Services Nitro enclave. For information about the interaction
-- between KMS and Amazon Web Services Nitro Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateRandomResponse_ciphertextForRecipient :: Lens.Lens' GenerateRandomResponse (Prelude.Maybe Prelude.ByteString)
generateRandomResponse_ciphertextForRecipient = Lens.lens (\GenerateRandomResponse' {ciphertextForRecipient} -> ciphertextForRecipient) (\s@GenerateRandomResponse' {} a -> s {ciphertextForRecipient = a} :: GenerateRandomResponse) Prelude.. Lens.mapping Data._Base64

-- | The random byte string. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.
--
-- If the response includes the @CiphertextForRecipient@ field, the
-- @Plaintext@ field is null or empty.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateRandomResponse_plaintext :: Lens.Lens' GenerateRandomResponse (Prelude.Maybe Prelude.ByteString)
generateRandomResponse_plaintext = Lens.lens (\GenerateRandomResponse' {plaintext} -> plaintext) (\s@GenerateRandomResponse' {} a -> s {plaintext = a} :: GenerateRandomResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | The response's http status code.
generateRandomResponse_httpStatus :: Lens.Lens' GenerateRandomResponse Prelude.Int
generateRandomResponse_httpStatus = Lens.lens (\GenerateRandomResponse' {httpStatus} -> httpStatus) (\s@GenerateRandomResponse' {} a -> s {httpStatus = a} :: GenerateRandomResponse)

instance Prelude.NFData GenerateRandomResponse where
  rnf GenerateRandomResponse' {..} =
    Prelude.rnf ciphertextForRecipient
      `Prelude.seq` Prelude.rnf plaintext
      `Prelude.seq` Prelude.rnf httpStatus
