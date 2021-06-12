{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SES.Types.AddHeaderAction
import Network.AWS.SES.Types.BounceAction
import Network.AWS.SES.Types.LambdaAction
import Network.AWS.SES.Types.S3Action
import Network.AWS.SES.Types.SNSAction
import Network.AWS.SES.Types.StopAction
import Network.AWS.SES.Types.WorkmailAction

-- | An action that Amazon SES can take when it receives an email on behalf
-- of one or more email addresses or domains that you own. An instance of
-- this data type can represent only one action.
--
-- For information about setting up receipt rules, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide>.
--
-- /See:/ 'newReceiptAction' smart constructor.
data ReceiptAction = ReceiptAction'
  { -- | Calls an AWS Lambda function, and optionally, publishes a notification
    -- to Amazon SNS.
    lambdaAction :: Core.Maybe LambdaAction,
    -- | Terminates the evaluation of the receipt rule set and optionally
    -- publishes a notification to Amazon SNS.
    stopAction :: Core.Maybe StopAction,
    -- | Saves the received message to an Amazon Simple Storage Service (Amazon
    -- S3) bucket and, optionally, publishes a notification to Amazon SNS.
    s3Action :: Core.Maybe S3Action,
    -- | Rejects the received email by returning a bounce response to the sender
    -- and, optionally, publishes a notification to Amazon Simple Notification
    -- Service (Amazon SNS).
    bounceAction :: Core.Maybe BounceAction,
    -- | Calls Amazon WorkMail and, optionally, publishes a notification to
    -- Amazon Amazon SNS.
    workmailAction :: Core.Maybe WorkmailAction,
    -- | Adds a header to the received email.
    addHeaderAction :: Core.Maybe AddHeaderAction,
    -- | Publishes the email content within a notification to Amazon SNS.
    sNSAction :: Core.Maybe SNSAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReceiptAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaAction', 'receiptAction_lambdaAction' - Calls an AWS Lambda function, and optionally, publishes a notification
-- to Amazon SNS.
--
-- 'stopAction', 'receiptAction_stopAction' - Terminates the evaluation of the receipt rule set and optionally
-- publishes a notification to Amazon SNS.
--
-- 's3Action', 'receiptAction_s3Action' - Saves the received message to an Amazon Simple Storage Service (Amazon
-- S3) bucket and, optionally, publishes a notification to Amazon SNS.
--
-- 'bounceAction', 'receiptAction_bounceAction' - Rejects the received email by returning a bounce response to the sender
-- and, optionally, publishes a notification to Amazon Simple Notification
-- Service (Amazon SNS).
--
-- 'workmailAction', 'receiptAction_workmailAction' - Calls Amazon WorkMail and, optionally, publishes a notification to
-- Amazon Amazon SNS.
--
-- 'addHeaderAction', 'receiptAction_addHeaderAction' - Adds a header to the received email.
--
-- 'sNSAction', 'receiptAction_sNSAction' - Publishes the email content within a notification to Amazon SNS.
newReceiptAction ::
  ReceiptAction
newReceiptAction =
  ReceiptAction'
    { lambdaAction = Core.Nothing,
      stopAction = Core.Nothing,
      s3Action = Core.Nothing,
      bounceAction = Core.Nothing,
      workmailAction = Core.Nothing,
      addHeaderAction = Core.Nothing,
      sNSAction = Core.Nothing
    }

-- | Calls an AWS Lambda function, and optionally, publishes a notification
-- to Amazon SNS.
receiptAction_lambdaAction :: Lens.Lens' ReceiptAction (Core.Maybe LambdaAction)
receiptAction_lambdaAction = Lens.lens (\ReceiptAction' {lambdaAction} -> lambdaAction) (\s@ReceiptAction' {} a -> s {lambdaAction = a} :: ReceiptAction)

-- | Terminates the evaluation of the receipt rule set and optionally
-- publishes a notification to Amazon SNS.
receiptAction_stopAction :: Lens.Lens' ReceiptAction (Core.Maybe StopAction)
receiptAction_stopAction = Lens.lens (\ReceiptAction' {stopAction} -> stopAction) (\s@ReceiptAction' {} a -> s {stopAction = a} :: ReceiptAction)

-- | Saves the received message to an Amazon Simple Storage Service (Amazon
-- S3) bucket and, optionally, publishes a notification to Amazon SNS.
receiptAction_s3Action :: Lens.Lens' ReceiptAction (Core.Maybe S3Action)
receiptAction_s3Action = Lens.lens (\ReceiptAction' {s3Action} -> s3Action) (\s@ReceiptAction' {} a -> s {s3Action = a} :: ReceiptAction)

-- | Rejects the received email by returning a bounce response to the sender
-- and, optionally, publishes a notification to Amazon Simple Notification
-- Service (Amazon SNS).
receiptAction_bounceAction :: Lens.Lens' ReceiptAction (Core.Maybe BounceAction)
receiptAction_bounceAction = Lens.lens (\ReceiptAction' {bounceAction} -> bounceAction) (\s@ReceiptAction' {} a -> s {bounceAction = a} :: ReceiptAction)

-- | Calls Amazon WorkMail and, optionally, publishes a notification to
-- Amazon Amazon SNS.
receiptAction_workmailAction :: Lens.Lens' ReceiptAction (Core.Maybe WorkmailAction)
receiptAction_workmailAction = Lens.lens (\ReceiptAction' {workmailAction} -> workmailAction) (\s@ReceiptAction' {} a -> s {workmailAction = a} :: ReceiptAction)

-- | Adds a header to the received email.
receiptAction_addHeaderAction :: Lens.Lens' ReceiptAction (Core.Maybe AddHeaderAction)
receiptAction_addHeaderAction = Lens.lens (\ReceiptAction' {addHeaderAction} -> addHeaderAction) (\s@ReceiptAction' {} a -> s {addHeaderAction = a} :: ReceiptAction)

-- | Publishes the email content within a notification to Amazon SNS.
receiptAction_sNSAction :: Lens.Lens' ReceiptAction (Core.Maybe SNSAction)
receiptAction_sNSAction = Lens.lens (\ReceiptAction' {sNSAction} -> sNSAction) (\s@ReceiptAction' {} a -> s {sNSAction = a} :: ReceiptAction)

instance Core.FromXML ReceiptAction where
  parseXML x =
    ReceiptAction'
      Core.<$> (x Core..@? "LambdaAction")
      Core.<*> (x Core..@? "StopAction")
      Core.<*> (x Core..@? "S3Action")
      Core.<*> (x Core..@? "BounceAction")
      Core.<*> (x Core..@? "WorkmailAction")
      Core.<*> (x Core..@? "AddHeaderAction")
      Core.<*> (x Core..@? "SNSAction")

instance Core.Hashable ReceiptAction

instance Core.NFData ReceiptAction

instance Core.ToQuery ReceiptAction where
  toQuery ReceiptAction' {..} =
    Core.mconcat
      [ "LambdaAction" Core.=: lambdaAction,
        "StopAction" Core.=: stopAction,
        "S3Action" Core.=: s3Action,
        "BounceAction" Core.=: bounceAction,
        "WorkmailAction" Core.=: workmailAction,
        "AddHeaderAction" Core.=: addHeaderAction,
        "SNSAction" Core.=: sNSAction
      ]
