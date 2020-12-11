{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateRandom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a random byte string that is cryptographically secure.
--
-- By default, the random byte string is generated in AWS KMS. To generate the byte string in the AWS CloudHSM cluster that is associated with a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> , specify the custom key store ID.
-- For more information about entropy and random number generation, see the <https://d0.awsstatic.com/whitepapers/KMS-Cryptographic-Details.pdf AWS Key Management Service Cryptographic Details> whitepaper.
module Network.AWS.KMS.GenerateRandom
  ( -- * Creating a request
    GenerateRandom (..),
    mkGenerateRandom,

    -- ** Request lenses
    grNumberOfBytes,
    grCustomKeyStoreId,

    -- * Destructuring the response
    GenerateRandomResponse (..),
    mkGenerateRandomResponse,

    -- ** Response lenses
    grrsPlaintext,
    grrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGenerateRandom' smart constructor.
data GenerateRandom = GenerateRandom'
  { numberOfBytes ::
      Lude.Maybe Lude.Natural,
    customKeyStoreId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateRandom' with the minimum fields required to make a request.
--
-- * 'customKeyStoreId' - Generates the random byte string in the AWS CloudHSM cluster that is associated with the specified <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
-- * 'numberOfBytes' - The length of the byte string.
mkGenerateRandom ::
  GenerateRandom
mkGenerateRandom =
  GenerateRandom'
    { numberOfBytes = Lude.Nothing,
      customKeyStoreId = Lude.Nothing
    }

-- | The length of the byte string.
--
-- /Note:/ Consider using 'numberOfBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grNumberOfBytes :: Lens.Lens' GenerateRandom (Lude.Maybe Lude.Natural)
grNumberOfBytes = Lens.lens (numberOfBytes :: GenerateRandom -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfBytes = a} :: GenerateRandom)
{-# DEPRECATED grNumberOfBytes "Use generic-lens or generic-optics with 'numberOfBytes' instead." #-}

-- | Generates the random byte string in the AWS CloudHSM cluster that is associated with the specified <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grCustomKeyStoreId :: Lens.Lens' GenerateRandom (Lude.Maybe Lude.Text)
grCustomKeyStoreId = Lens.lens (customKeyStoreId :: GenerateRandom -> Lude.Maybe Lude.Text) (\s a -> s {customKeyStoreId = a} :: GenerateRandom)
{-# DEPRECATED grCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

instance Lude.AWSRequest GenerateRandom where
  type Rs GenerateRandom = GenerateRandomResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GenerateRandomResponse'
            Lude.<$> (x Lude..?> "Plaintext") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GenerateRandom where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.GenerateRandom" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GenerateRandom where
  toJSON GenerateRandom' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NumberOfBytes" Lude..=) Lude.<$> numberOfBytes,
            ("CustomKeyStoreId" Lude..=) Lude.<$> customKeyStoreId
          ]
      )

instance Lude.ToPath GenerateRandom where
  toPath = Lude.const "/"

instance Lude.ToQuery GenerateRandom where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGenerateRandomResponse' smart constructor.
data GenerateRandomResponse = GenerateRandomResponse'
  { plaintext ::
      Lude.Maybe (Lude.Sensitive Lude.Base64),
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateRandomResponse' with the minimum fields required to make a request.
--
-- * 'plaintext' - The random byte string. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'responseStatus' - The response status code.
mkGenerateRandomResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GenerateRandomResponse
mkGenerateRandomResponse pResponseStatus_ =
  GenerateRandomResponse'
    { plaintext = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The random byte string. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'plaintext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsPlaintext :: Lens.Lens' GenerateRandomResponse (Lude.Maybe (Lude.Sensitive Lude.Base64))
grrsPlaintext = Lens.lens (plaintext :: GenerateRandomResponse -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {plaintext = a} :: GenerateRandomResponse)
{-# DEPRECATED grrsPlaintext "Use generic-lens or generic-optics with 'plaintext' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GenerateRandomResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GenerateRandomResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GenerateRandomResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
