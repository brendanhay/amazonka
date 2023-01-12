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
-- Module      : Amazonka.KMS.Sign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a
-- <https://en.wikipedia.org/wiki/Digital_signature digital signature> for
-- a message or message digest by using the private key in an asymmetric
-- signing KMS key. To verify the signature, use the Verify operation, or
-- use the public key in the same asymmetric KMS key outside of KMS. For
-- information about asymmetric KMS keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Asymmetric KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- Digital signatures are generated and verified by using asymmetric key
-- pair, such as an RSA or ECC pair that is represented by an asymmetric
-- KMS key. The key owner (or an authorized user) uses their private key to
-- sign a message. Anyone with the public key can verify that the message
-- was signed with that particular private key and that the message hasn\'t
-- changed since it was signed.
--
-- To use the @Sign@ operation, provide the following information:
--
-- -   Use the @KeyId@ parameter to identify an asymmetric KMS key with a
--     @KeyUsage@ value of @SIGN_VERIFY@. To get the @KeyUsage@ value of a
--     KMS key, use the DescribeKey operation. The caller must have
--     @kms:Sign@ permission on the KMS key.
--
-- -   Use the @Message@ parameter to specify the message or message digest
--     to sign. You can submit messages of up to 4096 bytes. To sign a
--     larger message, generate a hash digest of the message, and then
--     provide the hash digest in the @Message@ parameter. To indicate
--     whether the message is a full message or a digest, use the
--     @MessageType@ parameter.
--
-- -   Choose a signing algorithm that is compatible with the KMS key.
--
-- When signing a message, be sure to record the KMS key and the signing
-- algorithm. This information is required to verify the signature.
--
-- Best practices recommend that you limit the time during which any
-- signature is effective. This deters an attack where the actor uses a
-- signed message to establish validity repeatedly or long after the
-- message is superseded. Signatures do not include a timestamp, but you
-- can include a timestamp in the signed message to help you detect when
-- its time to refresh the signature.
--
-- To verify the signature that this operation generates, use the Verify
-- operation. Or use the GetPublicKey operation to download the public key
-- and then use the public key to verify the signature outside of KMS.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. To perform this operation with a KMS key in
-- a different Amazon Web Services account, specify the key ARN or alias
-- ARN in the value of the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:Sign>
-- (key policy)
--
-- __Related operations__: Verify
module Amazonka.KMS.Sign
  ( -- * Creating a Request
    Sign (..),
    newSign,

    -- * Request Lenses
    sign_grantTokens,
    sign_messageType,
    sign_keyId,
    sign_message,
    sign_signingAlgorithm,

    -- * Destructuring the Response
    SignResponse (..),
    newSignResponse,

    -- * Response Lenses
    signResponse_keyId,
    signResponse_signature,
    signResponse_signingAlgorithm,
    signResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSign' smart constructor.
data Sign = Sign'
  { -- | A list of grant tokens.
    --
    -- Use a grant token when your permission to call this operation comes from
    -- a new grant that has not yet achieved /eventual consistency/. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
    -- in the /Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Tells KMS whether the value of the @Message@ parameter is a message or
    -- message digest. The default value, RAW, indicates a message. To indicate
    -- a message digest, enter @DIGEST@.
    messageType :: Prelude.Maybe MessageType,
    -- | Identifies an asymmetric KMS key. KMS uses the private key in the
    -- asymmetric KMS key to sign the message. The @KeyUsage@ type of the KMS
    -- key must be @SIGN_VERIFY@. To find the @KeyUsage@ of a KMS key, use the
    -- DescribeKey operation.
    --
    -- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
    -- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
    -- key in a different Amazon Web Services account, you must use the key ARN
    -- or alias ARN.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey. To get the alias name and alias ARN, use ListAliases.
    keyId :: Prelude.Text,
    -- | Specifies the message or message digest to sign. Messages can be 0-4096
    -- bytes. To sign a larger message, provide the message digest.
    --
    -- If you provide a message, KMS generates a hash digest of the message and
    -- then signs it.
    message :: Data.Sensitive Data.Base64,
    -- | Specifies the signing algorithm to use when signing the message.
    --
    -- Choose an algorithm that is compatible with the type and size of the
    -- specified asymmetric KMS key.
    signingAlgorithm :: SigningAlgorithmSpec
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Sign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantTokens', 'sign_grantTokens' - A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
--
-- 'messageType', 'sign_messageType' - Tells KMS whether the value of the @Message@ parameter is a message or
-- message digest. The default value, RAW, indicates a message. To indicate
-- a message digest, enter @DIGEST@.
--
-- 'keyId', 'sign_keyId' - Identifies an asymmetric KMS key. KMS uses the private key in the
-- asymmetric KMS key to sign the message. The @KeyUsage@ type of the KMS
-- key must be @SIGN_VERIFY@. To find the @KeyUsage@ of a KMS key, use the
-- DescribeKey operation.
--
-- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
-- key in a different Amazon Web Services account, you must use the key ARN
-- or alias ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey. To get the alias name and alias ARN, use ListAliases.
--
-- 'message', 'sign_message' - Specifies the message or message digest to sign. Messages can be 0-4096
-- bytes. To sign a larger message, provide the message digest.
--
-- If you provide a message, KMS generates a hash digest of the message and
-- then signs it.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'signingAlgorithm', 'sign_signingAlgorithm' - Specifies the signing algorithm to use when signing the message.
--
-- Choose an algorithm that is compatible with the type and size of the
-- specified asymmetric KMS key.
newSign ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'message'
  Prelude.ByteString ->
  -- | 'signingAlgorithm'
  SigningAlgorithmSpec ->
  Sign
newSign pKeyId_ pMessage_ pSigningAlgorithm_ =
  Sign'
    { grantTokens = Prelude.Nothing,
      messageType = Prelude.Nothing,
      keyId = pKeyId_,
      message =
        Data._Sensitive Prelude.. Data._Base64
          Lens.# pMessage_,
      signingAlgorithm = pSigningAlgorithm_
    }

-- | A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
sign_grantTokens :: Lens.Lens' Sign (Prelude.Maybe [Prelude.Text])
sign_grantTokens = Lens.lens (\Sign' {grantTokens} -> grantTokens) (\s@Sign' {} a -> s {grantTokens = a} :: Sign) Prelude.. Lens.mapping Lens.coerced

-- | Tells KMS whether the value of the @Message@ parameter is a message or
-- message digest. The default value, RAW, indicates a message. To indicate
-- a message digest, enter @DIGEST@.
sign_messageType :: Lens.Lens' Sign (Prelude.Maybe MessageType)
sign_messageType = Lens.lens (\Sign' {messageType} -> messageType) (\s@Sign' {} a -> s {messageType = a} :: Sign)

-- | Identifies an asymmetric KMS key. KMS uses the private key in the
-- asymmetric KMS key to sign the message. The @KeyUsage@ type of the KMS
-- key must be @SIGN_VERIFY@. To find the @KeyUsage@ of a KMS key, use the
-- DescribeKey operation.
--
-- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
-- key in a different Amazon Web Services account, you must use the key ARN
-- or alias ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey. To get the alias name and alias ARN, use ListAliases.
sign_keyId :: Lens.Lens' Sign Prelude.Text
sign_keyId = Lens.lens (\Sign' {keyId} -> keyId) (\s@Sign' {} a -> s {keyId = a} :: Sign)

-- | Specifies the message or message digest to sign. Messages can be 0-4096
-- bytes. To sign a larger message, provide the message digest.
--
-- If you provide a message, KMS generates a hash digest of the message and
-- then signs it.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
sign_message :: Lens.Lens' Sign Prelude.ByteString
sign_message = Lens.lens (\Sign' {message} -> message) (\s@Sign' {} a -> s {message = a} :: Sign) Prelude.. Data._Sensitive Prelude.. Data._Base64

-- | Specifies the signing algorithm to use when signing the message.
--
-- Choose an algorithm that is compatible with the type and size of the
-- specified asymmetric KMS key.
sign_signingAlgorithm :: Lens.Lens' Sign SigningAlgorithmSpec
sign_signingAlgorithm = Lens.lens (\Sign' {signingAlgorithm} -> signingAlgorithm) (\s@Sign' {} a -> s {signingAlgorithm = a} :: Sign)

instance Core.AWSRequest Sign where
  type AWSResponse Sign = SignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SignResponse'
            Prelude.<$> (x Data..?> "KeyId")
            Prelude.<*> (x Data..?> "Signature")
            Prelude.<*> (x Data..?> "SigningAlgorithm")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Sign where
  hashWithSalt _salt Sign' {..} =
    _salt `Prelude.hashWithSalt` grantTokens
      `Prelude.hashWithSalt` messageType
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` signingAlgorithm

instance Prelude.NFData Sign where
  rnf Sign' {..} =
    Prelude.rnf grantTokens
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf signingAlgorithm

instance Data.ToHeaders Sign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.Sign" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Sign where
  toJSON Sign' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GrantTokens" Data..=) Prelude.<$> grantTokens,
            ("MessageType" Data..=) Prelude.<$> messageType,
            Prelude.Just ("KeyId" Data..= keyId),
            Prelude.Just ("Message" Data..= message),
            Prelude.Just
              ("SigningAlgorithm" Data..= signingAlgorithm)
          ]
      )

instance Data.ToPath Sign where
  toPath = Prelude.const "/"

instance Data.ToQuery Sign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSignResponse' smart constructor.
data SignResponse = SignResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the asymmetric KMS key that was used to sign the message.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The cryptographic signature that was generated for the message.
    --
    -- -   When used with the supported RSA signing algorithms, the encoding of
    --     this value is defined by
    --     <https://tools.ietf.org/html/rfc8017 PKCS #1 in RFC 8017>.
    --
    -- -   When used with the @ECDSA_SHA_256@, @ECDSA_SHA_384@, or
    --     @ECDSA_SHA_512@ signing algorithms, this value is a DER-encoded
    --     object as defined by ANS X9.62–2005 and
    --     <https://tools.ietf.org/html/rfc3279#section-2.2.3 RFC 3279 Section 2.2.3>.
    --     This is the most commonly used signature format and is appropriate
    --     for most uses.
    --
    -- When you use the HTTP API or the Amazon Web Services CLI, the value is
    -- Base64-encoded. Otherwise, it is not Base64-encoded.
    signature :: Prelude.Maybe Data.Base64,
    -- | The signing algorithm that was used to sign the message.
    signingAlgorithm :: Prelude.Maybe SigningAlgorithmSpec,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'signResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the asymmetric KMS key that was used to sign the message.
--
-- 'signature', 'signResponse_signature' - The cryptographic signature that was generated for the message.
--
-- -   When used with the supported RSA signing algorithms, the encoding of
--     this value is defined by
--     <https://tools.ietf.org/html/rfc8017 PKCS #1 in RFC 8017>.
--
-- -   When used with the @ECDSA_SHA_256@, @ECDSA_SHA_384@, or
--     @ECDSA_SHA_512@ signing algorithms, this value is a DER-encoded
--     object as defined by ANS X9.62–2005 and
--     <https://tools.ietf.org/html/rfc3279#section-2.2.3 RFC 3279 Section 2.2.3>.
--     This is the most commonly used signature format and is appropriate
--     for most uses.
--
-- When you use the HTTP API or the Amazon Web Services CLI, the value is
-- Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'signingAlgorithm', 'signResponse_signingAlgorithm' - The signing algorithm that was used to sign the message.
--
-- 'httpStatus', 'signResponse_httpStatus' - The response's http status code.
newSignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SignResponse
newSignResponse pHttpStatus_ =
  SignResponse'
    { keyId = Prelude.Nothing,
      signature = Prelude.Nothing,
      signingAlgorithm = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the asymmetric KMS key that was used to sign the message.
signResponse_keyId :: Lens.Lens' SignResponse (Prelude.Maybe Prelude.Text)
signResponse_keyId = Lens.lens (\SignResponse' {keyId} -> keyId) (\s@SignResponse' {} a -> s {keyId = a} :: SignResponse)

-- | The cryptographic signature that was generated for the message.
--
-- -   When used with the supported RSA signing algorithms, the encoding of
--     this value is defined by
--     <https://tools.ietf.org/html/rfc8017 PKCS #1 in RFC 8017>.
--
-- -   When used with the @ECDSA_SHA_256@, @ECDSA_SHA_384@, or
--     @ECDSA_SHA_512@ signing algorithms, this value is a DER-encoded
--     object as defined by ANS X9.62–2005 and
--     <https://tools.ietf.org/html/rfc3279#section-2.2.3 RFC 3279 Section 2.2.3>.
--     This is the most commonly used signature format and is appropriate
--     for most uses.
--
-- When you use the HTTP API or the Amazon Web Services CLI, the value is
-- Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
signResponse_signature :: Lens.Lens' SignResponse (Prelude.Maybe Prelude.ByteString)
signResponse_signature = Lens.lens (\SignResponse' {signature} -> signature) (\s@SignResponse' {} a -> s {signature = a} :: SignResponse) Prelude.. Lens.mapping Data._Base64

-- | The signing algorithm that was used to sign the message.
signResponse_signingAlgorithm :: Lens.Lens' SignResponse (Prelude.Maybe SigningAlgorithmSpec)
signResponse_signingAlgorithm = Lens.lens (\SignResponse' {signingAlgorithm} -> signingAlgorithm) (\s@SignResponse' {} a -> s {signingAlgorithm = a} :: SignResponse)

-- | The response's http status code.
signResponse_httpStatus :: Lens.Lens' SignResponse Prelude.Int
signResponse_httpStatus = Lens.lens (\SignResponse' {httpStatus} -> httpStatus) (\s@SignResponse' {} a -> s {httpStatus = a} :: SignResponse)

instance Prelude.NFData SignResponse where
  rnf SignResponse' {..} =
    Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf signature
      `Prelude.seq` Prelude.rnf signingAlgorithm
      `Prelude.seq` Prelude.rnf httpStatus
