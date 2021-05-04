{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KMS.Sign
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a
-- <https://en.wikipedia.org/wiki/Digital_signature digital signature> for
-- a message or message digest by using the private key in an asymmetric
-- CMK. To verify the signature, use the Verify operation, or use the
-- public key in the same asymmetric CMK outside of AWS KMS. For
-- information about symmetric and asymmetric CMKs, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs>
-- in the /AWS Key Management Service Developer Guide/.
--
-- Digital signatures are generated and verified by using asymmetric key
-- pair, such as an RSA or ECC pair that is represented by an asymmetric
-- customer master key (CMK). The key owner (or an authorized user) uses
-- their private key to sign a message. Anyone with the public key can
-- verify that the message was signed with that particular private key and
-- that the message hasn\'t changed since it was signed.
--
-- To use the @Sign@ operation, provide the following information:
--
-- -   Use the @KeyId@ parameter to identify an asymmetric CMK with a
--     @KeyUsage@ value of @SIGN_VERIFY@. To get the @KeyUsage@ value of a
--     CMK, use the DescribeKey operation. The caller must have @kms:Sign@
--     permission on the CMK.
--
-- -   Use the @Message@ parameter to specify the message or message digest
--     to sign. You can submit messages of up to 4096 bytes. To sign a
--     larger message, generate a hash digest of the message, and then
--     provide the hash digest in the @Message@ parameter. To indicate
--     whether the message is a full message or a digest, use the
--     @MessageType@ parameter.
--
-- -   Choose a signing algorithm that is compatible with the CMK.
--
-- When signing a message, be sure to record the CMK and the signing
-- algorithm. This information is required to verify the signature.
--
-- To verify the signature that this operation generates, use the Verify
-- operation. Or use the GetPublicKey operation to download the public key
-- and then use the public key to verify the signature outside of AWS KMS.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. To perform this operation with a CMK in a
-- different AWS account, specify the key ARN or alias ARN in the value of
-- the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:Sign>
-- (key policy)
--
-- __Related operations__: Verify
module Network.AWS.KMS.Sign
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
    signResponse_signingAlgorithm,
    signResponse_signature,
    signResponse_keyId,
    signResponse_httpStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSign' smart constructor.
data Sign = Sign'
  { -- | A list of grant tokens.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Tells AWS KMS whether the value of the @Message@ parameter is a message
    -- or message digest. The default value, RAW, indicates a message. To
    -- indicate a message digest, enter @DIGEST@.
    messageType :: Prelude.Maybe MessageType,
    -- | Identifies an asymmetric CMK. AWS KMS uses the private key in the
    -- asymmetric CMK to sign the message. The @KeyUsage@ type of the CMK must
    -- be @SIGN_VERIFY@. To find the @KeyUsage@ of a CMK, use the DescribeKey
    -- operation.
    --
    -- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias
    -- name, or alias ARN. When using an alias name, prefix it with
    -- @\"alias\/\"@. To specify a CMK in a different AWS account, you must use
    -- the key ARN or alias ARN.
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
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey. To
    -- get the alias name and alias ARN, use ListAliases.
    keyId :: Prelude.Text,
    -- | Specifies the message or message digest to sign. Messages can be 0-4096
    -- bytes. To sign a larger message, provide the message digest.
    --
    -- If you provide a message, AWS KMS generates a hash digest of the message
    -- and then signs it.
    message :: Prelude.Sensitive Prelude.Base64,
    -- | Specifies the signing algorithm to use when signing the message.
    --
    -- Choose an algorithm that is compatible with the type and size of the
    -- specified asymmetric CMK.
    signingAlgorithm :: SigningAlgorithmSpec
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'messageType', 'sign_messageType' - Tells AWS KMS whether the value of the @Message@ parameter is a message
-- or message digest. The default value, RAW, indicates a message. To
-- indicate a message digest, enter @DIGEST@.
--
-- 'keyId', 'sign_keyId' - Identifies an asymmetric CMK. AWS KMS uses the private key in the
-- asymmetric CMK to sign the message. The @KeyUsage@ type of the CMK must
-- be @SIGN_VERIFY@. To find the @KeyUsage@ of a CMK, use the DescribeKey
-- operation.
--
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias
-- name, or alias ARN. When using an alias name, prefix it with
-- @\"alias\/\"@. To specify a CMK in a different AWS account, you must use
-- the key ARN or alias ARN.
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
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey. To
-- get the alias name and alias ARN, use ListAliases.
--
-- 'message', 'sign_message' - Specifies the message or message digest to sign. Messages can be 0-4096
-- bytes. To sign a larger message, provide the message digest.
--
-- If you provide a message, AWS KMS generates a hash digest of the message
-- and then signs it.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'signingAlgorithm', 'sign_signingAlgorithm' - Specifies the signing algorithm to use when signing the message.
--
-- Choose an algorithm that is compatible with the type and size of the
-- specified asymmetric CMK.
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
        Prelude._Sensitive Prelude.. Prelude._Base64
          Lens.# pMessage_,
      signingAlgorithm = pSigningAlgorithm_
    }

-- | A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
sign_grantTokens :: Lens.Lens' Sign (Prelude.Maybe [Prelude.Text])
sign_grantTokens = Lens.lens (\Sign' {grantTokens} -> grantTokens) (\s@Sign' {} a -> s {grantTokens = a} :: Sign) Prelude.. Lens.mapping Prelude._Coerce

-- | Tells AWS KMS whether the value of the @Message@ parameter is a message
-- or message digest. The default value, RAW, indicates a message. To
-- indicate a message digest, enter @DIGEST@.
sign_messageType :: Lens.Lens' Sign (Prelude.Maybe MessageType)
sign_messageType = Lens.lens (\Sign' {messageType} -> messageType) (\s@Sign' {} a -> s {messageType = a} :: Sign)

-- | Identifies an asymmetric CMK. AWS KMS uses the private key in the
-- asymmetric CMK to sign the message. The @KeyUsage@ type of the CMK must
-- be @SIGN_VERIFY@. To find the @KeyUsage@ of a CMK, use the DescribeKey
-- operation.
--
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias
-- name, or alias ARN. When using an alias name, prefix it with
-- @\"alias\/\"@. To specify a CMK in a different AWS account, you must use
-- the key ARN or alias ARN.
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
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey. To
-- get the alias name and alias ARN, use ListAliases.
sign_keyId :: Lens.Lens' Sign Prelude.Text
sign_keyId = Lens.lens (\Sign' {keyId} -> keyId) (\s@Sign' {} a -> s {keyId = a} :: Sign)

-- | Specifies the message or message digest to sign. Messages can be 0-4096
-- bytes. To sign a larger message, provide the message digest.
--
-- If you provide a message, AWS KMS generates a hash digest of the message
-- and then signs it.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
sign_message :: Lens.Lens' Sign Prelude.ByteString
sign_message = Lens.lens (\Sign' {message} -> message) (\s@Sign' {} a -> s {message = a} :: Sign) Prelude.. Prelude._Sensitive Prelude.. Prelude._Base64

-- | Specifies the signing algorithm to use when signing the message.
--
-- Choose an algorithm that is compatible with the type and size of the
-- specified asymmetric CMK.
sign_signingAlgorithm :: Lens.Lens' Sign SigningAlgorithmSpec
sign_signingAlgorithm = Lens.lens (\Sign' {signingAlgorithm} -> signingAlgorithm) (\s@Sign' {} a -> s {signingAlgorithm = a} :: Sign)

instance Prelude.AWSRequest Sign where
  type Rs Sign = SignResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SignResponse'
            Prelude.<$> (x Prelude..?> "SigningAlgorithm")
            Prelude.<*> (x Prelude..?> "Signature")
            Prelude.<*> (x Prelude..?> "KeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Sign

instance Prelude.NFData Sign

instance Prelude.ToHeaders Sign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("TrentService.Sign" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON Sign where
  toJSON Sign' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GrantTokens" Prelude..=) Prelude.<$> grantTokens,
            ("MessageType" Prelude..=) Prelude.<$> messageType,
            Prelude.Just ("KeyId" Prelude..= keyId),
            Prelude.Just ("Message" Prelude..= message),
            Prelude.Just
              ("SigningAlgorithm" Prelude..= signingAlgorithm)
          ]
      )

instance Prelude.ToPath Sign where
  toPath = Prelude.const "/"

instance Prelude.ToQuery Sign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSignResponse' smart constructor.
data SignResponse = SignResponse'
  { -- | The signing algorithm that was used to sign the message.
    signingAlgorithm :: Prelude.Maybe SigningAlgorithmSpec,
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
    -- When you use the HTTP API or the AWS CLI, the value is Base64-encoded.
    -- Otherwise, it is not Base64-encoded.
    signature :: Prelude.Maybe Prelude.Base64,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the asymmetric CMK that was used to sign the message.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingAlgorithm', 'signResponse_signingAlgorithm' - The signing algorithm that was used to sign the message.
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
-- When you use the HTTP API or the AWS CLI, the value is Base64-encoded.
-- Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyId', 'signResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the asymmetric CMK that was used to sign the message.
--
-- 'httpStatus', 'signResponse_httpStatus' - The response's http status code.
newSignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SignResponse
newSignResponse pHttpStatus_ =
  SignResponse'
    { signingAlgorithm = Prelude.Nothing,
      signature = Prelude.Nothing,
      keyId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The signing algorithm that was used to sign the message.
signResponse_signingAlgorithm :: Lens.Lens' SignResponse (Prelude.Maybe SigningAlgorithmSpec)
signResponse_signingAlgorithm = Lens.lens (\SignResponse' {signingAlgorithm} -> signingAlgorithm) (\s@SignResponse' {} a -> s {signingAlgorithm = a} :: SignResponse)

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
-- When you use the HTTP API or the AWS CLI, the value is Base64-encoded.
-- Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
signResponse_signature :: Lens.Lens' SignResponse (Prelude.Maybe Prelude.ByteString)
signResponse_signature = Lens.lens (\SignResponse' {signature} -> signature) (\s@SignResponse' {} a -> s {signature = a} :: SignResponse) Prelude.. Lens.mapping Prelude._Base64

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the asymmetric CMK that was used to sign the message.
signResponse_keyId :: Lens.Lens' SignResponse (Prelude.Maybe Prelude.Text)
signResponse_keyId = Lens.lens (\SignResponse' {keyId} -> keyId) (\s@SignResponse' {} a -> s {keyId = a} :: SignResponse)

-- | The response's http status code.
signResponse_httpStatus :: Lens.Lens' SignResponse Prelude.Int
signResponse_httpStatus = Lens.lens (\SignResponse' {httpStatus} -> httpStatus) (\s@SignResponse' {} a -> s {httpStatus = a} :: SignResponse)

instance Prelude.NFData SignResponse
