{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dKeyId,
    dGrantTokens,

    -- * Destructuring the response
    DescribeKeyResponse (..),
    mkDescribeKeyResponse,

    -- ** Response lenses
    dkrrsKeyMetadata,
    dkrrsResponseStatus,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeKey' smart constructor.
data DescribeKey = DescribeKey'
  { -- | Describes the specified customer master key (CMK).
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
    keyId :: Types.KeyId,
    -- | A list of grant tokens.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantTokens :: Core.Maybe [Types.GrantTokenType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeKey' value with any optional fields omitted.
mkDescribeKey ::
  -- | 'keyId'
  Types.KeyId ->
  DescribeKey
mkDescribeKey keyId =
  DescribeKey' {keyId, grantTokens = Core.Nothing}

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
dKeyId :: Lens.Lens' DescribeKey Types.KeyId
dKeyId = Lens.field @"keyId"
{-# DEPRECATED dKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGrantTokens :: Lens.Lens' DescribeKey (Core.Maybe [Types.GrantTokenType])
dGrantTokens = Lens.field @"grantTokens"
{-# DEPRECATED dGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

instance Core.FromJSON DescribeKey where
  toJSON DescribeKey {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyId" Core..= keyId),
            ("GrantTokens" Core..=) Core.<$> grantTokens
          ]
      )

instance Core.AWSRequest DescribeKey where
  type Rs DescribeKey = DescribeKeyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.DescribeKey")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeKeyResponse'
            Core.<$> (x Core..:? "KeyMetadata") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeKeyResponse' smart constructor.
data DescribeKeyResponse = DescribeKeyResponse'
  { -- | Metadata associated with the key.
    keyMetadata :: Core.Maybe Types.KeyMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeKeyResponse' value with any optional fields omitted.
mkDescribeKeyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeKeyResponse
mkDescribeKeyResponse responseStatus =
  DescribeKeyResponse' {keyMetadata = Core.Nothing, responseStatus}

-- | Metadata associated with the key.
--
-- /Note:/ Consider using 'keyMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkrrsKeyMetadata :: Lens.Lens' DescribeKeyResponse (Core.Maybe Types.KeyMetadata)
dkrrsKeyMetadata = Lens.field @"keyMetadata"
{-# DEPRECATED dkrrsKeyMetadata "Use generic-lens or generic-optics with 'keyMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkrrsResponseStatus :: Lens.Lens' DescribeKeyResponse Core.Int
dkrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dkrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
