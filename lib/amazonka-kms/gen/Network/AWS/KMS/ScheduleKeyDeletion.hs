{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ScheduleKeyDeletion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules the deletion of a customer master key (CMK). You may provide a waiting period, specified in days, before deletion occurs. If you do not provide a waiting period, the default period of 30 days is used. When this operation is successful, the key state of the CMK changes to @PendingDeletion@ . Before the waiting period ends, you can use 'CancelKeyDeletion' to cancel the deletion of the CMK. After the waiting period ends, AWS KMS deletes the CMK and all AWS KMS data associated with it, including all aliases that refer to it.
--
-- /Important:/ Deleting a CMK is a destructive and potentially dangerous operation. When a CMK is deleted, all data that was encrypted under the CMK is unrecoverable. To prevent the use of a CMK without deleting it, use 'DisableKey' .
-- If you schedule deletion of a CMK from a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> , when the waiting period expires, @ScheduleKeyDeletion@ deletes the CMK from AWS KMS. Then AWS KMS makes a best effort to delete the key material from the associated AWS CloudHSM cluster. However, you might need to manually <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-orphaned-key delete the orphaned key material> from the cluster and its backups.
-- You cannot perform this operation on a CMK in a different AWS account.
-- For more information about scheduling a CMK for deletion, see <https://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html Deleting Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.ScheduleKeyDeletion
    (
    -- * Creating a request
      ScheduleKeyDeletion (..)
    , mkScheduleKeyDeletion
    -- ** Request lenses
    , skdKeyId
    , skdPendingWindowInDays

    -- * Destructuring the response
    , ScheduleKeyDeletionResponse (..)
    , mkScheduleKeyDeletionResponse
    -- ** Response lenses
    , skdrrsDeletionDate
    , skdrrsKeyId
    , skdrrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkScheduleKeyDeletion' smart constructor.
data ScheduleKeyDeletion = ScheduleKeyDeletion'
  { keyId :: Types.KeyId
    -- ^ The unique identifier of the customer master key (CMK) to delete.
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
  , pendingWindowInDays :: Core.Maybe Core.Natural
    -- ^ The waiting period, specified in number of days. After the waiting period ends, AWS KMS deletes the customer master key (CMK).
--
-- This value is optional. If you include a value, it must be between 7 and 30, inclusive. If you do not include a value, it defaults to 30.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleKeyDeletion' value with any optional fields omitted.
mkScheduleKeyDeletion
    :: Types.KeyId -- ^ 'keyId'
    -> ScheduleKeyDeletion
mkScheduleKeyDeletion keyId
  = ScheduleKeyDeletion'{keyId, pendingWindowInDays = Core.Nothing}

-- | The unique identifier of the customer master key (CMK) to delete.
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
skdKeyId :: Lens.Lens' ScheduleKeyDeletion Types.KeyId
skdKeyId = Lens.field @"keyId"
{-# INLINEABLE skdKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The waiting period, specified in number of days. After the waiting period ends, AWS KMS deletes the customer master key (CMK).
--
-- This value is optional. If you include a value, it must be between 7 and 30, inclusive. If you do not include a value, it defaults to 30.
--
-- /Note:/ Consider using 'pendingWindowInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skdPendingWindowInDays :: Lens.Lens' ScheduleKeyDeletion (Core.Maybe Core.Natural)
skdPendingWindowInDays = Lens.field @"pendingWindowInDays"
{-# INLINEABLE skdPendingWindowInDays #-}
{-# DEPRECATED pendingWindowInDays "Use generic-lens or generic-optics with 'pendingWindowInDays' instead"  #-}

instance Core.ToQuery ScheduleKeyDeletion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ScheduleKeyDeletion where
        toHeaders ScheduleKeyDeletion{..}
          = Core.pure ("X-Amz-Target", "TrentService.ScheduleKeyDeletion")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ScheduleKeyDeletion where
        toJSON ScheduleKeyDeletion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  ("PendingWindowInDays" Core..=) Core.<$> pendingWindowInDays])

instance Core.AWSRequest ScheduleKeyDeletion where
        type Rs ScheduleKeyDeletion = ScheduleKeyDeletionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ScheduleKeyDeletionResponse' Core.<$>
                   (x Core..:? "DeletionDate") Core.<*> x Core..:? "KeyId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkScheduleKeyDeletionResponse' smart constructor.
data ScheduleKeyDeletionResponse = ScheduleKeyDeletionResponse'
  { deletionDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time after which AWS KMS deletes the customer master key (CMK).
  , keyId :: Core.Maybe Types.KeyId
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK whose deletion is scheduled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ScheduleKeyDeletionResponse' value with any optional fields omitted.
mkScheduleKeyDeletionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ScheduleKeyDeletionResponse
mkScheduleKeyDeletionResponse responseStatus
  = ScheduleKeyDeletionResponse'{deletionDate = Core.Nothing,
                                 keyId = Core.Nothing, responseStatus}

-- | The date and time after which AWS KMS deletes the customer master key (CMK).
--
-- /Note:/ Consider using 'deletionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skdrrsDeletionDate :: Lens.Lens' ScheduleKeyDeletionResponse (Core.Maybe Core.NominalDiffTime)
skdrrsDeletionDate = Lens.field @"deletionDate"
{-# INLINEABLE skdrrsDeletionDate #-}
{-# DEPRECATED deletionDate "Use generic-lens or generic-optics with 'deletionDate' instead"  #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK whose deletion is scheduled.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skdrrsKeyId :: Lens.Lens' ScheduleKeyDeletionResponse (Core.Maybe Types.KeyId)
skdrrsKeyId = Lens.field @"keyId"
{-# INLINEABLE skdrrsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skdrrsResponseStatus :: Lens.Lens' ScheduleKeyDeletionResponse Core.Int
skdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE skdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
