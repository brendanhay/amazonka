{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.SendBonus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @SendBonus@ operation issues a payment of money from your account to a Worker. This payment happens separately from the reward you pay to the Worker when you approve the Worker's assignment. The SendBonus operation requires the Worker's ID and the assignment ID as parameters to initiate payment of the bonus. You must include a message that explains the reason for the bonus payment, as the Worker may not be expecting the payment. Amazon Mechanical Turk collects a fee for bonus payments, similar to the HIT listing fee. This operation fails if your account does not have enough funds to pay for both the bonus and the fees. 
module Network.AWS.MechanicalTurk.SendBonus
    (
    -- * Creating a request
      SendBonus (..)
    , mkSendBonus
    -- ** Request lenses
    , sbWorkerId
    , sbBonusAmount
    , sbAssignmentId
    , sbReason
    , sbUniqueRequestToken

    -- * Destructuring the response
    , SendBonusResponse (..)
    , mkSendBonusResponse
    -- ** Response lenses
    , sbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSendBonus' smart constructor.
data SendBonus = SendBonus'
  { workerId :: Types.WorkerId
    -- ^ The ID of the Worker being paid the bonus.
  , bonusAmount :: Types.BonusAmount
    -- ^ The Bonus amount is a US Dollar amount specified using a string (for example, "5" represents $5.00 USD and "101.42" represents $101.42 USD). Do not include currency symbols or currency codes. 
  , assignmentId :: Types.AssignmentId
    -- ^ The ID of the assignment for which this bonus is paid.
  , reason :: Core.Text
    -- ^ A message that explains the reason for the bonus payment. The Worker receiving the bonus can see this message.
  , uniqueRequestToken :: Core.Maybe Types.IdempotencyToken
    -- ^ A unique identifier for this request, which allows you to retry the call on error without granting multiple bonuses. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the bonus already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return an error with a message containing the request ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendBonus' value with any optional fields omitted.
mkSendBonus
    :: Types.WorkerId -- ^ 'workerId'
    -> Types.BonusAmount -- ^ 'bonusAmount'
    -> Types.AssignmentId -- ^ 'assignmentId'
    -> Core.Text -- ^ 'reason'
    -> SendBonus
mkSendBonus workerId bonusAmount assignmentId reason
  = SendBonus'{workerId, bonusAmount, assignmentId, reason,
               uniqueRequestToken = Core.Nothing}

-- | The ID of the Worker being paid the bonus.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbWorkerId :: Lens.Lens' SendBonus Types.WorkerId
sbWorkerId = Lens.field @"workerId"
{-# INLINEABLE sbWorkerId #-}
{-# DEPRECATED workerId "Use generic-lens or generic-optics with 'workerId' instead"  #-}

-- | The Bonus amount is a US Dollar amount specified using a string (for example, "5" represents $5.00 USD and "101.42" represents $101.42 USD). Do not include currency symbols or currency codes. 
--
-- /Note:/ Consider using 'bonusAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBonusAmount :: Lens.Lens' SendBonus Types.BonusAmount
sbBonusAmount = Lens.field @"bonusAmount"
{-# INLINEABLE sbBonusAmount #-}
{-# DEPRECATED bonusAmount "Use generic-lens or generic-optics with 'bonusAmount' instead"  #-}

-- | The ID of the assignment for which this bonus is paid.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbAssignmentId :: Lens.Lens' SendBonus Types.AssignmentId
sbAssignmentId = Lens.field @"assignmentId"
{-# INLINEABLE sbAssignmentId #-}
{-# DEPRECATED assignmentId "Use generic-lens or generic-optics with 'assignmentId' instead"  #-}

-- | A message that explains the reason for the bonus payment. The Worker receiving the bonus can see this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbReason :: Lens.Lens' SendBonus Core.Text
sbReason = Lens.field @"reason"
{-# INLINEABLE sbReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

-- | A unique identifier for this request, which allows you to retry the call on error without granting multiple bonuses. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the bonus already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return an error with a message containing the request ID.
--
-- /Note:/ Consider using 'uniqueRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbUniqueRequestToken :: Lens.Lens' SendBonus (Core.Maybe Types.IdempotencyToken)
sbUniqueRequestToken = Lens.field @"uniqueRequestToken"
{-# INLINEABLE sbUniqueRequestToken #-}
{-# DEPRECATED uniqueRequestToken "Use generic-lens or generic-optics with 'uniqueRequestToken' instead"  #-}

instance Core.ToQuery SendBonus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SendBonus where
        toHeaders SendBonus{..}
          = Core.pure
              ("X-Amz-Target", "MTurkRequesterServiceV20170117.SendBonus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SendBonus where
        toJSON SendBonus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WorkerId" Core..= workerId),
                  Core.Just ("BonusAmount" Core..= bonusAmount),
                  Core.Just ("AssignmentId" Core..= assignmentId),
                  Core.Just ("Reason" Core..= reason),
                  ("UniqueRequestToken" Core..=) Core.<$> uniqueRequestToken])

instance Core.AWSRequest SendBonus where
        type Rs SendBonus = SendBonusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 SendBonusResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSendBonusResponse' smart constructor.
newtype SendBonusResponse = SendBonusResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SendBonusResponse' value with any optional fields omitted.
mkSendBonusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SendBonusResponse
mkSendBonusResponse responseStatus
  = SendBonusResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrrsResponseStatus :: Lens.Lens' SendBonusResponse Core.Int
sbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
