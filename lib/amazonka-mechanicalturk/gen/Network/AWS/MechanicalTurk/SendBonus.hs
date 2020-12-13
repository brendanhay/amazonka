{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SendBonus (..),
    mkSendBonus,

    -- ** Request lenses
    sbUniqueRequestToken,
    sbReason,
    sbWorkerId,
    sbAssignmentId,
    sbBonusAmount,

    -- * Destructuring the response
    SendBonusResponse (..),
    mkSendBonusResponse,

    -- ** Response lenses
    sbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSendBonus' smart constructor.
data SendBonus = SendBonus'
  { -- | A unique identifier for this request, which allows you to retry the call on error without granting multiple bonuses. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the bonus already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return an error with a message containing the request ID.
    uniqueRequestToken :: Lude.Maybe Lude.Text,
    -- | A message that explains the reason for the bonus payment. The Worker receiving the bonus can see this message.
    reason :: Lude.Text,
    -- | The ID of the Worker being paid the bonus.
    workerId :: Lude.Text,
    -- | The ID of the assignment for which this bonus is paid.
    assignmentId :: Lude.Text,
    -- | The Bonus amount is a US Dollar amount specified using a string (for example, "5" represents $5.00 USD and "101.42" represents $101.42 USD). Do not include currency symbols or currency codes.
    bonusAmount :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendBonus' with the minimum fields required to make a request.
--
-- * 'uniqueRequestToken' - A unique identifier for this request, which allows you to retry the call on error without granting multiple bonuses. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the bonus already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return an error with a message containing the request ID.
-- * 'reason' - A message that explains the reason for the bonus payment. The Worker receiving the bonus can see this message.
-- * 'workerId' - The ID of the Worker being paid the bonus.
-- * 'assignmentId' - The ID of the assignment for which this bonus is paid.
-- * 'bonusAmount' - The Bonus amount is a US Dollar amount specified using a string (for example, "5" represents $5.00 USD and "101.42" represents $101.42 USD). Do not include currency symbols or currency codes.
mkSendBonus ::
  -- | 'reason'
  Lude.Text ->
  -- | 'workerId'
  Lude.Text ->
  -- | 'assignmentId'
  Lude.Text ->
  -- | 'bonusAmount'
  Lude.Text ->
  SendBonus
mkSendBonus pReason_ pWorkerId_ pAssignmentId_ pBonusAmount_ =
  SendBonus'
    { uniqueRequestToken = Lude.Nothing,
      reason = pReason_,
      workerId = pWorkerId_,
      assignmentId = pAssignmentId_,
      bonusAmount = pBonusAmount_
    }

-- | A unique identifier for this request, which allows you to retry the call on error without granting multiple bonuses. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the bonus already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return an error with a message containing the request ID.
--
-- /Note:/ Consider using 'uniqueRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbUniqueRequestToken :: Lens.Lens' SendBonus (Lude.Maybe Lude.Text)
sbUniqueRequestToken = Lens.lens (uniqueRequestToken :: SendBonus -> Lude.Maybe Lude.Text) (\s a -> s {uniqueRequestToken = a} :: SendBonus)
{-# DEPRECATED sbUniqueRequestToken "Use generic-lens or generic-optics with 'uniqueRequestToken' instead." #-}

-- | A message that explains the reason for the bonus payment. The Worker receiving the bonus can see this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbReason :: Lens.Lens' SendBonus Lude.Text
sbReason = Lens.lens (reason :: SendBonus -> Lude.Text) (\s a -> s {reason = a} :: SendBonus)
{-# DEPRECATED sbReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The ID of the Worker being paid the bonus.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbWorkerId :: Lens.Lens' SendBonus Lude.Text
sbWorkerId = Lens.lens (workerId :: SendBonus -> Lude.Text) (\s a -> s {workerId = a} :: SendBonus)
{-# DEPRECATED sbWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

-- | The ID of the assignment for which this bonus is paid.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbAssignmentId :: Lens.Lens' SendBonus Lude.Text
sbAssignmentId = Lens.lens (assignmentId :: SendBonus -> Lude.Text) (\s a -> s {assignmentId = a} :: SendBonus)
{-# DEPRECATED sbAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

-- | The Bonus amount is a US Dollar amount specified using a string (for example, "5" represents $5.00 USD and "101.42" represents $101.42 USD). Do not include currency symbols or currency codes.
--
-- /Note:/ Consider using 'bonusAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBonusAmount :: Lens.Lens' SendBonus Lude.Text
sbBonusAmount = Lens.lens (bonusAmount :: SendBonus -> Lude.Text) (\s a -> s {bonusAmount = a} :: SendBonus)
{-# DEPRECATED sbBonusAmount "Use generic-lens or generic-optics with 'bonusAmount' instead." #-}

instance Lude.AWSRequest SendBonus where
  type Rs SendBonus = SendBonusResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          SendBonusResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendBonus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MTurkRequesterServiceV20170117.SendBonus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SendBonus where
  toJSON SendBonus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UniqueRequestToken" Lude..=) Lude.<$> uniqueRequestToken,
            Lude.Just ("Reason" Lude..= reason),
            Lude.Just ("WorkerId" Lude..= workerId),
            Lude.Just ("AssignmentId" Lude..= assignmentId),
            Lude.Just ("BonusAmount" Lude..= bonusAmount)
          ]
      )

instance Lude.ToPath SendBonus where
  toPath = Lude.const "/"

instance Lude.ToQuery SendBonus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSendBonusResponse' smart constructor.
newtype SendBonusResponse = SendBonusResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendBonusResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSendBonusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendBonusResponse
mkSendBonusResponse pResponseStatus_ =
  SendBonusResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrsResponseStatus :: Lens.Lens' SendBonusResponse Lude.Int
sbrsResponseStatus = Lens.lens (responseStatus :: SendBonusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendBonusResponse)
{-# DEPRECATED sbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
