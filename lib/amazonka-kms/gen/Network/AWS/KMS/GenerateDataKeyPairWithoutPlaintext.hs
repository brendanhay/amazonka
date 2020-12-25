{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKeyPairWithoutPlaintext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique asymmetric data key pair. The @GenerateDataKeyPairWithoutPlaintext@ operation returns a plaintext public key and a copy of the private key that is encrypted under the symmetric CMK you specify. Unlike 'GenerateDataKeyPair' , this operation does not return a plaintext private key.
--
-- To generate a data key pair, you must specify a symmetric customer master key (CMK) to encrypt the private key in the data key pair. You cannot use an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the @KeySpec@ field in the 'DescribeKey' response.
-- You can use the public key that @GenerateDataKeyPairWithoutPlaintext@ returns to encrypt data or verify a signature outside of AWS KMS. Then, store the encrypted private key with the data. When you are ready to decrypt data or sign a message, you can use the 'Decrypt' operation to decrypt the encrypted private key.
-- @GenerateDataKeyPairWithoutPlaintext@ returns a unique data key pair for each request. The bytes in the key are not related to the caller or CMK that is used to encrypt the private key.
-- You can use the optional encryption context to add additional security to the encryption operation. If you specify an @EncryptionContext@ , you must specify the same encryption context (a case-sensitive exact match) when decrypting the encrypted data key. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GenerateDataKeyPairWithoutPlaintext
  ( -- * Creating a request
    GenerateDataKeyPairWithoutPlaintext (..),
    mkGenerateDataKeyPairWithoutPlaintext,

    -- ** Request lenses
    gdkpwpKeyId,
    gdkpwpKeyPairSpec,
    gdkpwpEncryptionContext,
    gdkpwpGrantTokens,

    -- * Destructuring the response
    GenerateDataKeyPairWithoutPlaintextResponse (..),
    mkGenerateDataKeyPairWithoutPlaintextResponse,

    -- ** Response lenses
    gdkpwprrsKeyId,
    gdkpwprrsKeyPairSpec,
    gdkpwprrsPrivateKeyCiphertextBlob,
    gdkpwprrsPublicKey,
    gdkpwprrsResponseStatus,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGenerateDataKeyPairWithoutPlaintext' smart constructor.
data GenerateDataKeyPairWithoutPlaintext = GenerateDataKeyPairWithoutPlaintext'
  { -- | Specifies the CMK that encrypts the private key in the data key pair. You must specify a symmetric CMK. You cannot use an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
    --
    -- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ .
    -- For example:
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Alias name: @alias/ExampleAlias@
    --
    --
    --     * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@
    --
    --
    -- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
    keyId :: Types.KeyId,
    -- | Determines the type of data key pair that is generated.
    --
    -- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
    keyPairSpec :: Types.DataKeyPairSpec,
    -- | Specifies the encryption context that will be used when encrypting the private key in the data key pair.
    --
    -- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
    encryptionContext :: Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue),
    -- | A list of grant tokens.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantTokens :: Core.Maybe [Types.GrantTokenType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateDataKeyPairWithoutPlaintext' value with any optional fields omitted.
mkGenerateDataKeyPairWithoutPlaintext ::
  -- | 'keyId'
  Types.KeyId ->
  -- | 'keyPairSpec'
  Types.DataKeyPairSpec ->
  GenerateDataKeyPairWithoutPlaintext
mkGenerateDataKeyPairWithoutPlaintext keyId keyPairSpec =
  GenerateDataKeyPairWithoutPlaintext'
    { keyId,
      keyPairSpec,
      encryptionContext = Core.Nothing,
      grantTokens = Core.Nothing
    }

-- | Specifies the CMK that encrypts the private key in the data key pair. You must specify a symmetric CMK. You cannot use an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
--
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ .
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Alias name: @alias/ExampleAlias@
--
--
--     * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwpKeyId :: Lens.Lens' GenerateDataKeyPairWithoutPlaintext Types.KeyId
gdkpwpKeyId = Lens.field @"keyId"
{-# DEPRECATED gdkpwpKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Determines the type of data key pair that is generated.
--
-- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
--
-- /Note:/ Consider using 'keyPairSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwpKeyPairSpec :: Lens.Lens' GenerateDataKeyPairWithoutPlaintext Types.DataKeyPairSpec
gdkpwpKeyPairSpec = Lens.field @"keyPairSpec"
{-# DEPRECATED gdkpwpKeyPairSpec "Use generic-lens or generic-optics with 'keyPairSpec' instead." #-}

-- | Specifies the encryption context that will be used when encrypting the private key in the data key pair.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwpEncryptionContext :: Lens.Lens' GenerateDataKeyPairWithoutPlaintext (Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue))
gdkpwpEncryptionContext = Lens.field @"encryptionContext"
{-# DEPRECATED gdkpwpEncryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwpGrantTokens :: Lens.Lens' GenerateDataKeyPairWithoutPlaintext (Core.Maybe [Types.GrantTokenType])
gdkpwpGrantTokens = Lens.field @"grantTokens"
{-# DEPRECATED gdkpwpGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

instance Core.FromJSON GenerateDataKeyPairWithoutPlaintext where
  toJSON GenerateDataKeyPairWithoutPlaintext {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyId" Core..= keyId),
            Core.Just ("KeyPairSpec" Core..= keyPairSpec),
            ("EncryptionContext" Core..=) Core.<$> encryptionContext,
            ("GrantTokens" Core..=) Core.<$> grantTokens
          ]
      )

instance Core.AWSRequest GenerateDataKeyPairWithoutPlaintext where
  type
    Rs GenerateDataKeyPairWithoutPlaintext =
      GenerateDataKeyPairWithoutPlaintextResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "TrentService.GenerateDataKeyPairWithoutPlaintext"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateDataKeyPairWithoutPlaintextResponse'
            Core.<$> (x Core..:? "KeyId")
            Core.<*> (x Core..:? "KeyPairSpec")
            Core.<*> (x Core..:? "PrivateKeyCiphertextBlob")
            Core.<*> (x Core..:? "PublicKey")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGenerateDataKeyPairWithoutPlaintextResponse' smart constructor.
data GenerateDataKeyPairWithoutPlaintextResponse = GenerateDataKeyPairWithoutPlaintextResponse'
  { -- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
    keyId :: Core.Maybe Types.KeyId,
    -- | The type of data key pair that was generated.
    keyPairSpec :: Core.Maybe Types.DataKeyPairSpec,
    -- | The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
    privateKeyCiphertextBlob :: Core.Maybe Core.Base64,
    -- | The public key (in plaintext).
    publicKey :: Core.Maybe Core.Base64,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateDataKeyPairWithoutPlaintextResponse' value with any optional fields omitted.
mkGenerateDataKeyPairWithoutPlaintextResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GenerateDataKeyPairWithoutPlaintextResponse
mkGenerateDataKeyPairWithoutPlaintextResponse responseStatus =
  GenerateDataKeyPairWithoutPlaintextResponse'
    { keyId =
        Core.Nothing,
      keyPairSpec = Core.Nothing,
      privateKeyCiphertextBlob = Core.Nothing,
      publicKey = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwprrsKeyId :: Lens.Lens' GenerateDataKeyPairWithoutPlaintextResponse (Core.Maybe Types.KeyId)
gdkpwprrsKeyId = Lens.field @"keyId"
{-# DEPRECATED gdkpwprrsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The type of data key pair that was generated.
--
-- /Note:/ Consider using 'keyPairSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwprrsKeyPairSpec :: Lens.Lens' GenerateDataKeyPairWithoutPlaintextResponse (Core.Maybe Types.DataKeyPairSpec)
gdkpwprrsKeyPairSpec = Lens.field @"keyPairSpec"
{-# DEPRECATED gdkpwprrsKeyPairSpec "Use generic-lens or generic-optics with 'keyPairSpec' instead." #-}

-- | The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'privateKeyCiphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwprrsPrivateKeyCiphertextBlob :: Lens.Lens' GenerateDataKeyPairWithoutPlaintextResponse (Core.Maybe Core.Base64)
gdkpwprrsPrivateKeyCiphertextBlob = Lens.field @"privateKeyCiphertextBlob"
{-# DEPRECATED gdkpwprrsPrivateKeyCiphertextBlob "Use generic-lens or generic-optics with 'privateKeyCiphertextBlob' instead." #-}

-- | The public key (in plaintext).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwprrsPublicKey :: Lens.Lens' GenerateDataKeyPairWithoutPlaintextResponse (Core.Maybe Core.Base64)
gdkpwprrsPublicKey = Lens.field @"publicKey"
{-# DEPRECATED gdkpwprrsPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwprrsResponseStatus :: Lens.Lens' GenerateDataKeyPairWithoutPlaintextResponse Core.Int
gdkpwprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdkpwprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
