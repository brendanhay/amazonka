{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MechanicalTurk.SendBonus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @SendBonus@ operation issues a payment of money from your account to
-- a Worker. This payment happens separately from the reward you pay to the
-- Worker when you approve the Worker\'s assignment. The SendBonus
-- operation requires the Worker\'s ID and the assignment ID as parameters
-- to initiate payment of the bonus. You must include a message that
-- explains the reason for the bonus payment, as the Worker may not be
-- expecting the payment. Amazon Mechanical Turk collects a fee for bonus
-- payments, similar to the HIT listing fee. This operation fails if your
-- account does not have enough funds to pay for both the bonus and the
-- fees.
module Amazonka.MechanicalTurk.SendBonus
  ( -- * Creating a Request
    SendBonus (..),
    newSendBonus,

    -- * Request Lenses
    sendBonus_uniqueRequestToken,
    sendBonus_workerId,
    sendBonus_bonusAmount,
    sendBonus_assignmentId,
    sendBonus_reason,

    -- * Destructuring the Response
    SendBonusResponse (..),
    newSendBonusResponse,

    -- * Response Lenses
    sendBonusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendBonus' smart constructor.
data SendBonus = SendBonus'
  { -- | A unique identifier for this request, which allows you to retry the call
    -- on error without granting multiple bonuses. This is useful in cases such
    -- as network timeouts where it is unclear whether or not the call
    -- succeeded on the server. If the bonus already exists in the system from
    -- a previous call using the same UniqueRequestToken, subsequent calls will
    -- return an error with a message containing the request ID.
    uniqueRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Worker being paid the bonus.
    workerId :: Prelude.Text,
    -- | The Bonus amount is a US Dollar amount specified using a string (for
    -- example, \"5\" represents $5.00 USD and \"101.42\" represents $101.42
    -- USD). Do not include currency symbols or currency codes.
    bonusAmount :: Prelude.Text,
    -- | The ID of the assignment for which this bonus is paid.
    assignmentId :: Prelude.Text,
    -- | A message that explains the reason for the bonus payment. The Worker
    -- receiving the bonus can see this message.
    reason :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendBonus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uniqueRequestToken', 'sendBonus_uniqueRequestToken' - A unique identifier for this request, which allows you to retry the call
-- on error without granting multiple bonuses. This is useful in cases such
-- as network timeouts where it is unclear whether or not the call
-- succeeded on the server. If the bonus already exists in the system from
-- a previous call using the same UniqueRequestToken, subsequent calls will
-- return an error with a message containing the request ID.
--
-- 'workerId', 'sendBonus_workerId' - The ID of the Worker being paid the bonus.
--
-- 'bonusAmount', 'sendBonus_bonusAmount' - The Bonus amount is a US Dollar amount specified using a string (for
-- example, \"5\" represents $5.00 USD and \"101.42\" represents $101.42
-- USD). Do not include currency symbols or currency codes.
--
-- 'assignmentId', 'sendBonus_assignmentId' - The ID of the assignment for which this bonus is paid.
--
-- 'reason', 'sendBonus_reason' - A message that explains the reason for the bonus payment. The Worker
-- receiving the bonus can see this message.
newSendBonus ::
  -- | 'workerId'
  Prelude.Text ->
  -- | 'bonusAmount'
  Prelude.Text ->
  -- | 'assignmentId'
  Prelude.Text ->
  -- | 'reason'
  Prelude.Text ->
  SendBonus
newSendBonus
  pWorkerId_
  pBonusAmount_
  pAssignmentId_
  pReason_ =
    SendBonus'
      { uniqueRequestToken = Prelude.Nothing,
        workerId = pWorkerId_,
        bonusAmount = pBonusAmount_,
        assignmentId = pAssignmentId_,
        reason = pReason_
      }

-- | A unique identifier for this request, which allows you to retry the call
-- on error without granting multiple bonuses. This is useful in cases such
-- as network timeouts where it is unclear whether or not the call
-- succeeded on the server. If the bonus already exists in the system from
-- a previous call using the same UniqueRequestToken, subsequent calls will
-- return an error with a message containing the request ID.
sendBonus_uniqueRequestToken :: Lens.Lens' SendBonus (Prelude.Maybe Prelude.Text)
sendBonus_uniqueRequestToken = Lens.lens (\SendBonus' {uniqueRequestToken} -> uniqueRequestToken) (\s@SendBonus' {} a -> s {uniqueRequestToken = a} :: SendBonus)

-- | The ID of the Worker being paid the bonus.
sendBonus_workerId :: Lens.Lens' SendBonus Prelude.Text
sendBonus_workerId = Lens.lens (\SendBonus' {workerId} -> workerId) (\s@SendBonus' {} a -> s {workerId = a} :: SendBonus)

-- | The Bonus amount is a US Dollar amount specified using a string (for
-- example, \"5\" represents $5.00 USD and \"101.42\" represents $101.42
-- USD). Do not include currency symbols or currency codes.
sendBonus_bonusAmount :: Lens.Lens' SendBonus Prelude.Text
sendBonus_bonusAmount = Lens.lens (\SendBonus' {bonusAmount} -> bonusAmount) (\s@SendBonus' {} a -> s {bonusAmount = a} :: SendBonus)

-- | The ID of the assignment for which this bonus is paid.
sendBonus_assignmentId :: Lens.Lens' SendBonus Prelude.Text
sendBonus_assignmentId = Lens.lens (\SendBonus' {assignmentId} -> assignmentId) (\s@SendBonus' {} a -> s {assignmentId = a} :: SendBonus)

-- | A message that explains the reason for the bonus payment. The Worker
-- receiving the bonus can see this message.
sendBonus_reason :: Lens.Lens' SendBonus Prelude.Text
sendBonus_reason = Lens.lens (\SendBonus' {reason} -> reason) (\s@SendBonus' {} a -> s {reason = a} :: SendBonus)

instance Core.AWSRequest SendBonus where
  type AWSResponse SendBonus = SendBonusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SendBonusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendBonus where
  hashWithSalt _salt SendBonus' {..} =
    _salt
      `Prelude.hashWithSalt` uniqueRequestToken
      `Prelude.hashWithSalt` workerId
      `Prelude.hashWithSalt` bonusAmount
      `Prelude.hashWithSalt` assignmentId
      `Prelude.hashWithSalt` reason

instance Prelude.NFData SendBonus where
  rnf SendBonus' {..} =
    Prelude.rnf uniqueRequestToken `Prelude.seq`
      Prelude.rnf workerId `Prelude.seq`
        Prelude.rnf bonusAmount `Prelude.seq`
          Prelude.rnf assignmentId `Prelude.seq`
            Prelude.rnf reason

instance Data.ToHeaders SendBonus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.SendBonus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendBonus where
  toJSON SendBonus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UniqueRequestToken" Data..=)
              Prelude.<$> uniqueRequestToken,
            Prelude.Just ("WorkerId" Data..= workerId),
            Prelude.Just ("BonusAmount" Data..= bonusAmount),
            Prelude.Just ("AssignmentId" Data..= assignmentId),
            Prelude.Just ("Reason" Data..= reason)
          ]
      )

instance Data.ToPath SendBonus where
  toPath = Prelude.const "/"

instance Data.ToQuery SendBonus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendBonusResponse' smart constructor.
data SendBonusResponse = SendBonusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendBonusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendBonusResponse_httpStatus' - The response's http status code.
newSendBonusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendBonusResponse
newSendBonusResponse pHttpStatus_ =
  SendBonusResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
sendBonusResponse_httpStatus :: Lens.Lens' SendBonusResponse Prelude.Int
sendBonusResponse_httpStatus = Lens.lens (\SendBonusResponse' {httpStatus} -> httpStatus) (\s@SendBonusResponse' {} a -> s {httpStatus = a} :: SendBonusResponse)

instance Prelude.NFData SendBonusResponse where
  rnf SendBonusResponse' {..} = Prelude.rnf httpStatus
