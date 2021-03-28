{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.EnableKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the key state of a customer master key (CMK) to enabled. This allows you to use the CMK for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> . You cannot perform this operation on a CMK in a different AWS account.
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.EnableKey
    (
    -- * Creating a request
      EnableKey (..)
    , mkEnableKey
    -- ** Request lenses
    , ekKeyId

    -- * Destructuring the response
    , EnableKeyResponse (..)
    , mkEnableKeyResponse
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableKey' smart constructor.
newtype EnableKey = EnableKey'
  { keyId :: Types.KeyId
    -- ^ A unique identifier for the customer master key (CMK).
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

-- | Creates a 'EnableKey' value with any optional fields omitted.
mkEnableKey
    :: Types.KeyId -- ^ 'keyId'
    -> EnableKey
mkEnableKey keyId = EnableKey'{keyId}

-- | A unique identifier for the customer master key (CMK).
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
ekKeyId :: Lens.Lens' EnableKey Types.KeyId
ekKeyId = Lens.field @"keyId"
{-# INLINEABLE ekKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.ToQuery EnableKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableKey where
        toHeaders EnableKey{..}
          = Core.pure ("X-Amz-Target", "TrentService.EnableKey") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnableKey where
        toJSON EnableKey{..}
          = Core.object (Core.catMaybes [Core.Just ("KeyId" Core..= keyId)])

instance Core.AWSRequest EnableKey where
        type Rs EnableKey = EnableKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull EnableKeyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableKeyResponse' smart constructor.
data EnableKeyResponse = EnableKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableKeyResponse' value with any optional fields omitted.
mkEnableKeyResponse
    :: EnableKeyResponse
mkEnableKeyResponse = EnableKeyResponse'
