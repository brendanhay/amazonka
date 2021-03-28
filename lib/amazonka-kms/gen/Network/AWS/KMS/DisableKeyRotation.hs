{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DisableKeyRotation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material> for the specified symmetric customer master key (CMK).
--
-- You cannot enable automatic rotation of asymmetric CMKs, CMKs with imported key material, or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . You cannot perform this operation on a CMK in a different AWS account.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.DisableKeyRotation
    (
    -- * Creating a request
      DisableKeyRotation (..)
    , mkDisableKeyRotation
    -- ** Request lenses
    , dkrKeyId

    -- * Destructuring the response
    , DisableKeyRotationResponse (..)
    , mkDisableKeyRotationResponse
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableKeyRotation' smart constructor.
newtype DisableKeyRotation = DisableKeyRotation'
  { keyId :: Types.KeyIdType
    -- ^ Identifies a symmetric customer master key (CMK). You cannot enable automatic rotation of <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric CMKs> , CMKs with <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material> , or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> .
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableKeyRotation' value with any optional fields omitted.
mkDisableKeyRotation
    :: Types.KeyIdType -- ^ 'keyId'
    -> DisableKeyRotation
mkDisableKeyRotation keyId = DisableKeyRotation'{keyId}

-- | Identifies a symmetric customer master key (CMK). You cannot enable automatic rotation of <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric CMKs> , CMKs with <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material> , or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> .
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
dkrKeyId :: Lens.Lens' DisableKeyRotation Types.KeyIdType
dkrKeyId = Lens.field @"keyId"
{-# INLINEABLE dkrKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.ToQuery DisableKeyRotation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableKeyRotation where
        toHeaders DisableKeyRotation{..}
          = Core.pure ("X-Amz-Target", "TrentService.DisableKeyRotation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisableKeyRotation where
        toJSON DisableKeyRotation{..}
          = Core.object (Core.catMaybes [Core.Just ("KeyId" Core..= keyId)])

instance Core.AWSRequest DisableKeyRotation where
        type Rs DisableKeyRotation = DisableKeyRotationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DisableKeyRotationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableKeyRotationResponse' smart constructor.
data DisableKeyRotationResponse = DisableKeyRotationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableKeyRotationResponse' value with any optional fields omitted.
mkDisableKeyRotationResponse
    :: DisableKeyRotationResponse
mkDisableKeyRotationResponse = DisableKeyRotationResponse'
