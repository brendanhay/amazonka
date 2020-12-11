{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DescribeKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides detailed information about a customer master key (CMK). You can run @DescribeKey@ on a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed CMK> or an <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMK> .
--
-- This detailed information includes the key ARN, creation date (and deletion date, if applicable), the key state, and the origin and expiration date (if any) of the key material. For CMKs in custom key stores, it includes information about the custom key store, such as the key store ID and the AWS CloudHSM cluster ID. It includes fields, like @KeySpec@ , that help you distinguish symmetric from asymmetric CMKs. It also provides information that is particularly important to asymmetric CMKs, such as the key usage (encryption or signing) and the encryption algorithms or signing algorithms that the CMK supports.
-- @DescribeKey@ does not return the following information:
--
--     * Aliases associated with the CMK. To get this information, use 'ListAliases' .
--
--
--     * Whether automatic key rotation is enabled on the CMK. To get this information, use 'GetKeyRotationStatus' . Also, some key states prevent a CMK from being automatically rotated. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html#rotate-keys-how-it-works How Automatic Key Rotation Works> in /AWS Key Management Service Developer Guide/ .
--
--
--     * Tags on the CMK. To get this information, use 'ListResourceTags' .
--
--
--     * Key policies and grants on the CMK. To get this information, use 'GetKeyPolicy' and 'ListGrants' .
--
--
-- If you call the @DescribeKey@ operation on a /predefined AWS alias/ , that is, an AWS alias with no key ID, AWS KMS creates an <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys AWS managed CMK> . Then, it associates the alias with the new CMK, and returns the @KeyId@ and @Arn@ of the new CMK in the response.
-- To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.
module Network.AWS.KMS.DescribeKey
  ( -- * Creating a request
    DescribeKey (..),
    mkDescribeKey,

    -- ** Request lenses
    dGrantTokens,
    dKeyId,

    -- * Destructuring the response
    DescribeKeyResponse (..),
    mkDescribeKeyResponse,

    -- ** Response lenses
    dkrsKeyMetadata,
    dkrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeKey' smart constructor.
data DescribeKey = DescribeKey'
  { grantTokens ::
      Lude.Maybe [Lude.Text],
    keyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeKey' with the minimum fields required to make a request.
--
-- * 'grantTokens' - A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
-- * 'keyId' - Describes the specified customer master key (CMK).
--
-- If you specify a predefined AWS alias (an AWS alias with no key ID), KMS associates the alias with an <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys AWS managed CMK> and returns its @KeyId@ and @Arn@ in the response.
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN.
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
mkDescribeKey ::
  -- | 'keyId'
  Lude.Text ->
  DescribeKey
mkDescribeKey pKeyId_ =
  DescribeKey' {grantTokens = Lude.Nothing, keyId = pKeyId_}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGrantTokens :: Lens.Lens' DescribeKey (Lude.Maybe [Lude.Text])
dGrantTokens = Lens.lens (grantTokens :: DescribeKey -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: DescribeKey)
{-# DEPRECATED dGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | Describes the specified customer master key (CMK).
--
-- If you specify a predefined AWS alias (an AWS alias with no key ID), KMS associates the alias with an <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys AWS managed CMK> and returns its @KeyId@ and @Arn@ in the response.
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN.
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
dKeyId :: Lens.Lens' DescribeKey Lude.Text
dKeyId = Lens.lens (keyId :: DescribeKey -> Lude.Text) (\s a -> s {keyId = a} :: DescribeKey)
{-# DEPRECATED dKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.AWSRequest DescribeKey where
  type Rs DescribeKey = DescribeKeyResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeKeyResponse'
            Lude.<$> (x Lude..?> "KeyMetadata") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.DescribeKey" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeKey where
  toJSON DescribeKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GrantTokens" Lude..=) Lude.<$> grantTokens,
            Lude.Just ("KeyId" Lude..= keyId)
          ]
      )

instance Lude.ToPath DescribeKey where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeKeyResponse' smart constructor.
data DescribeKeyResponse = DescribeKeyResponse'
  { keyMetadata ::
      Lude.Maybe KeyMetadata,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeKeyResponse' with the minimum fields required to make a request.
--
-- * 'keyMetadata' - Metadata associated with the key.
-- * 'responseStatus' - The response status code.
mkDescribeKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeKeyResponse
mkDescribeKeyResponse pResponseStatus_ =
  DescribeKeyResponse'
    { keyMetadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Metadata associated with the key.
--
-- /Note:/ Consider using 'keyMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkrsKeyMetadata :: Lens.Lens' DescribeKeyResponse (Lude.Maybe KeyMetadata)
dkrsKeyMetadata = Lens.lens (keyMetadata :: DescribeKeyResponse -> Lude.Maybe KeyMetadata) (\s a -> s {keyMetadata = a} :: DescribeKeyResponse)
{-# DEPRECATED dkrsKeyMetadata "Use generic-lens or generic-optics with 'keyMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkrsResponseStatus :: Lens.Lens' DescribeKeyResponse Lude.Int
dkrsResponseStatus = Lens.lens (responseStatus :: DescribeKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeKeyResponse)
{-# DEPRECATED dkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
