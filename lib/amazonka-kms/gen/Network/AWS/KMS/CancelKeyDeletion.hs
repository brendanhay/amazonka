{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.CancelKeyDeletion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the deletion of a customer master key (CMK). When this operation succeeds, the key state of the CMK is @Disabled@ . To enable the CMK, use 'EnableKey' . You cannot perform this operation on a CMK in a different AWS account.
--
-- For more information about scheduling and canceling deletion of a CMK, see <https://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html Deleting Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.CancelKeyDeletion
    (
    -- * Creating a request
      CancelKeyDeletion (..)
    , mkCancelKeyDeletion
    -- ** Request lenses
    , ckdKeyId

    -- * Destructuring the response
    , CancelKeyDeletionResponse (..)
    , mkCancelKeyDeletionResponse
    -- ** Response lenses
    , ckdrrsKeyId
    , ckdrrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelKeyDeletion' smart constructor.
newtype CancelKeyDeletion = CancelKeyDeletion'
  { keyId :: Types.KeyId
    -- ^ The unique identifier for the customer master key (CMK) for which to cancel deletion.
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

-- | Creates a 'CancelKeyDeletion' value with any optional fields omitted.
mkCancelKeyDeletion
    :: Types.KeyId -- ^ 'keyId'
    -> CancelKeyDeletion
mkCancelKeyDeletion keyId = CancelKeyDeletion'{keyId}

-- | The unique identifier for the customer master key (CMK) for which to cancel deletion.
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
ckdKeyId :: Lens.Lens' CancelKeyDeletion Types.KeyId
ckdKeyId = Lens.field @"keyId"
{-# INLINEABLE ckdKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.ToQuery CancelKeyDeletion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelKeyDeletion where
        toHeaders CancelKeyDeletion{..}
          = Core.pure ("X-Amz-Target", "TrentService.CancelKeyDeletion")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelKeyDeletion where
        toJSON CancelKeyDeletion{..}
          = Core.object (Core.catMaybes [Core.Just ("KeyId" Core..= keyId)])

instance Core.AWSRequest CancelKeyDeletion where
        type Rs CancelKeyDeletion = CancelKeyDeletionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CancelKeyDeletionResponse' Core.<$>
                   (x Core..:? "KeyId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelKeyDeletionResponse' smart constructor.
data CancelKeyDeletionResponse = CancelKeyDeletionResponse'
  { keyId :: Core.Maybe Types.KeyId
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK whose deletion is canceled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelKeyDeletionResponse' value with any optional fields omitted.
mkCancelKeyDeletionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelKeyDeletionResponse
mkCancelKeyDeletionResponse responseStatus
  = CancelKeyDeletionResponse'{keyId = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK whose deletion is canceled.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckdrrsKeyId :: Lens.Lens' CancelKeyDeletionResponse (Core.Maybe Types.KeyId)
ckdrrsKeyId = Lens.field @"keyId"
{-# INLINEABLE ckdrrsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckdrrsResponseStatus :: Lens.Lens' CancelKeyDeletionResponse Core.Int
ckdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ckdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
