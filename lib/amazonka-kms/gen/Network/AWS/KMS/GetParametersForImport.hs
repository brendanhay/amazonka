{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GetParametersForImport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the items you need to import key material into a symmetric, customer managed customer master key (CMK). For more information about importing key material into AWS KMS, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ .
--
-- This operation returns a public key and an import token. Use the public key to encrypt the symmetric key material. Store the import token to send with a subsequent 'ImportKeyMaterial' request.
-- You must specify the key ID of the symmetric CMK into which you will import key material. This CMK's @Origin@ must be @EXTERNAL@ . You must also specify the wrapping algorithm and type of wrapping key (public key) that you will use to encrypt the key material. You cannot perform this operation on an asymmetric CMK or on any CMK in a different AWS account.
-- To import key material, you must use the public key and import token from the same response. These items are valid for 24 hours. The expiration date and time appear in the @GetParametersForImport@ response. You cannot use an expired token in an 'ImportKeyMaterial' request. If your key and token expire, send another @GetParametersForImport@ request.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GetParametersForImport
  ( -- * Creating a request
    GetParametersForImport (..),
    mkGetParametersForImport,

    -- ** Request lenses
    gpfiKeyId,
    gpfiWrappingAlgorithm,
    gpfiWrappingKeySpec,

    -- * Destructuring the response
    GetParametersForImportResponse (..),
    mkGetParametersForImportResponse,

    -- ** Response lenses
    gpfirsKeyId,
    gpfirsPublicKey,
    gpfirsParametersValidTo,
    gpfirsImportToken,
    gpfirsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetParametersForImport' smart constructor.
data GetParametersForImport = GetParametersForImport'
  { keyId ::
      Lude.Text,
    wrappingAlgorithm :: AlgorithmSpec,
    wrappingKeySpec :: WrappingKeySpec
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParametersForImport' with the minimum fields required to make a request.
--
-- * 'keyId' - The identifier of the symmetric CMK into which you will import key material. The @Origin@ of the CMK must be @EXTERNAL@ .
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
-- * 'wrappingAlgorithm' - The algorithm you will use to encrypt the key material before importing it with 'ImportKeyMaterial' . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys-encrypt-key-material.html Encrypt the Key Material> in the /AWS Key Management Service Developer Guide/ .
-- * 'wrappingKeySpec' - The type of wrapping key (public key) to return in the response. Only 2048-bit RSA public keys are supported.
mkGetParametersForImport ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'wrappingAlgorithm'
  AlgorithmSpec ->
  -- | 'wrappingKeySpec'
  WrappingKeySpec ->
  GetParametersForImport
mkGetParametersForImport
  pKeyId_
  pWrappingAlgorithm_
  pWrappingKeySpec_ =
    GetParametersForImport'
      { keyId = pKeyId_,
        wrappingAlgorithm = pWrappingAlgorithm_,
        wrappingKeySpec = pWrappingKeySpec_
      }

-- | The identifier of the symmetric CMK into which you will import key material. The @Origin@ of the CMK must be @EXTERNAL@ .
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfiKeyId :: Lens.Lens' GetParametersForImport Lude.Text
gpfiKeyId = Lens.lens (keyId :: GetParametersForImport -> Lude.Text) (\s a -> s {keyId = a} :: GetParametersForImport)
{-# DEPRECATED gpfiKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The algorithm you will use to encrypt the key material before importing it with 'ImportKeyMaterial' . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys-encrypt-key-material.html Encrypt the Key Material> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'wrappingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfiWrappingAlgorithm :: Lens.Lens' GetParametersForImport AlgorithmSpec
gpfiWrappingAlgorithm = Lens.lens (wrappingAlgorithm :: GetParametersForImport -> AlgorithmSpec) (\s a -> s {wrappingAlgorithm = a} :: GetParametersForImport)
{-# DEPRECATED gpfiWrappingAlgorithm "Use generic-lens or generic-optics with 'wrappingAlgorithm' instead." #-}

-- | The type of wrapping key (public key) to return in the response. Only 2048-bit RSA public keys are supported.
--
-- /Note:/ Consider using 'wrappingKeySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfiWrappingKeySpec :: Lens.Lens' GetParametersForImport WrappingKeySpec
gpfiWrappingKeySpec = Lens.lens (wrappingKeySpec :: GetParametersForImport -> WrappingKeySpec) (\s a -> s {wrappingKeySpec = a} :: GetParametersForImport)
{-# DEPRECATED gpfiWrappingKeySpec "Use generic-lens or generic-optics with 'wrappingKeySpec' instead." #-}

instance Lude.AWSRequest GetParametersForImport where
  type Rs GetParametersForImport = GetParametersForImportResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetParametersForImportResponse'
            Lude.<$> (x Lude..?> "KeyId")
            Lude.<*> (x Lude..?> "PublicKey")
            Lude.<*> (x Lude..?> "ParametersValidTo")
            Lude.<*> (x Lude..?> "ImportToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetParametersForImport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.GetParametersForImport" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetParametersForImport where
  toJSON GetParametersForImport' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyId" Lude..= keyId),
            Lude.Just ("WrappingAlgorithm" Lude..= wrappingAlgorithm),
            Lude.Just ("WrappingKeySpec" Lude..= wrappingKeySpec)
          ]
      )

instance Lude.ToPath GetParametersForImport where
  toPath = Lude.const "/"

instance Lude.ToQuery GetParametersForImport where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetParametersForImportResponse' smart constructor.
data GetParametersForImportResponse = GetParametersForImportResponse'
  { keyId ::
      Lude.Maybe Lude.Text,
    publicKey ::
      Lude.Maybe
        ( Lude.Sensitive
            Lude.Base64
        ),
    parametersValidTo ::
      Lude.Maybe Lude.Timestamp,
    importToken ::
      Lude.Maybe Lude.Base64,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParametersForImportResponse' with the minimum fields required to make a request.
--
-- * 'importToken' - The import token to send in a subsequent 'ImportKeyMaterial' request.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK to use in a subsequent 'ImportKeyMaterial' request. This is the same CMK specified in the @GetParametersForImport@ request.
-- * 'parametersValidTo' - The time at which the import token and public key are no longer valid. After this time, you cannot use them to make an 'ImportKeyMaterial' request and you must send another @GetParametersForImport@ request to get new ones.
-- * 'publicKey' - The public key to use to encrypt the key material before importing it with 'ImportKeyMaterial' .--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'responseStatus' - The response status code.
mkGetParametersForImportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetParametersForImportResponse
mkGetParametersForImportResponse pResponseStatus_ =
  GetParametersForImportResponse'
    { keyId = Lude.Nothing,
      publicKey = Lude.Nothing,
      parametersValidTo = Lude.Nothing,
      importToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK to use in a subsequent 'ImportKeyMaterial' request. This is the same CMK specified in the @GetParametersForImport@ request.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfirsKeyId :: Lens.Lens' GetParametersForImportResponse (Lude.Maybe Lude.Text)
gpfirsKeyId = Lens.lens (keyId :: GetParametersForImportResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: GetParametersForImportResponse)
{-# DEPRECATED gpfirsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The public key to use to encrypt the key material before importing it with 'ImportKeyMaterial' .--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfirsPublicKey :: Lens.Lens' GetParametersForImportResponse (Lude.Maybe (Lude.Sensitive Lude.Base64))
gpfirsPublicKey = Lens.lens (publicKey :: GetParametersForImportResponse -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {publicKey = a} :: GetParametersForImportResponse)
{-# DEPRECATED gpfirsPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The time at which the import token and public key are no longer valid. After this time, you cannot use them to make an 'ImportKeyMaterial' request and you must send another @GetParametersForImport@ request to get new ones.
--
-- /Note:/ Consider using 'parametersValidTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfirsParametersValidTo :: Lens.Lens' GetParametersForImportResponse (Lude.Maybe Lude.Timestamp)
gpfirsParametersValidTo = Lens.lens (parametersValidTo :: GetParametersForImportResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {parametersValidTo = a} :: GetParametersForImportResponse)
{-# DEPRECATED gpfirsParametersValidTo "Use generic-lens or generic-optics with 'parametersValidTo' instead." #-}

-- | The import token to send in a subsequent 'ImportKeyMaterial' request.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'importToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfirsImportToken :: Lens.Lens' GetParametersForImportResponse (Lude.Maybe Lude.Base64)
gpfirsImportToken = Lens.lens (importToken :: GetParametersForImportResponse -> Lude.Maybe Lude.Base64) (\s a -> s {importToken = a} :: GetParametersForImportResponse)
{-# DEPRECATED gpfirsImportToken "Use generic-lens or generic-optics with 'importToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfirsResponseStatus :: Lens.Lens' GetParametersForImportResponse Lude.Int
gpfirsResponseStatus = Lens.lens (responseStatus :: GetParametersForImportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetParametersForImportResponse)
{-# DEPRECATED gpfirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
