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
-- Module      : Amazonka.SES.Types.ReceiptAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.ReceiptAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.AddHeaderAction
import Amazonka.SES.Types.BounceAction
import Amazonka.SES.Types.LambdaAction
import Amazonka.SES.Types.S3Action
import Amazonka.SES.Types.SNSAction
import Amazonka.SES.Types.StopAction
import Amazonka.SES.Types.WorkmailAction

-- | An action that Amazon SES can take when it receives an email on behalf
-- of one or more email addresses or domains that you own. An instance of
-- this data type can represent only one action.
--
-- For information about setting up receipt rules, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide>.
--
-- /See:/ 'newReceiptAction' smart constructor.
data ReceiptAction = ReceiptAction'
  { -- | Adds a header to the received email.
    addHeaderAction :: Prelude.Maybe AddHeaderAction,
    -- | Publishes the email content within a notification to Amazon SNS.
    sNSAction :: Prelude.Maybe SNSAction,
    -- | Calls Amazon WorkMail and, optionally, publishes a notification to
    -- Amazon Amazon SNS.
    workmailAction :: Prelude.Maybe WorkmailAction,
    -- | Rejects the received email by returning a bounce response to the sender
    -- and, optionally, publishes a notification to Amazon Simple Notification
    -- Service (Amazon SNS).
    bounceAction :: Prelude.Maybe BounceAction,
    -- | Calls an AWS Lambda function, and optionally, publishes a notification
    -- to Amazon SNS.
    lambdaAction :: Prelude.Maybe LambdaAction,
    -- | Terminates the evaluation of the receipt rule set and optionally
    -- publishes a notification to Amazon SNS.
    stopAction :: Prelude.Maybe StopAction,
    -- | Saves the received message to an Amazon Simple Storage Service (Amazon
    -- S3) bucket and, optionally, publishes a notification to Amazon SNS.
    s3Action :: Prelude.Maybe S3Action
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReceiptAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addHeaderAction', 'receiptAction_addHeaderAction' - Adds a header to the received email.
--
-- 'sNSAction', 'receiptAction_sNSAction' - Publishes the email content within a notification to Amazon SNS.
--
-- 'workmailAction', 'receiptAction_workmailAction' - Calls Amazon WorkMail and, optionally, publishes a notification to
-- Amazon Amazon SNS.
--
-- 'bounceAction', 'receiptAction_bounceAction' - Rejects the received email by returning a bounce response to the sender
-- and, optionally, publishes a notification to Amazon Simple Notification
-- Service (Amazon SNS).
--
-- 'lambdaAction', 'receiptAction_lambdaAction' - Calls an AWS Lambda function, and optionally, publishes a notification
-- to Amazon SNS.
--
-- 'stopAction', 'receiptAction_stopAction' - Terminates the evaluation of the receipt rule set and optionally
-- publishes a notification to Amazon SNS.
--
-- 's3Action', 'receiptAction_s3Action' - Saves the received message to an Amazon Simple Storage Service (Amazon
-- S3) bucket and, optionally, publishes a notification to Amazon SNS.
newReceiptAction ::
  ReceiptAction
newReceiptAction =
  ReceiptAction'
    { addHeaderAction = Prelude.Nothing,
      sNSAction = Prelude.Nothing,
      workmailAction = Prelude.Nothing,
      bounceAction = Prelude.Nothing,
      lambdaAction = Prelude.Nothing,
      stopAction = Prelude.Nothing,
      s3Action = Prelude.Nothing
    }

-- | Adds a header to the received email.
receiptAction_addHeaderAction :: Lens.Lens' ReceiptAction (Prelude.Maybe AddHeaderAction)
receiptAction_addHeaderAction = Lens.lens (\ReceiptAction' {addHeaderAction} -> addHeaderAction) (\s@ReceiptAction' {} a -> s {addHeaderAction = a} :: ReceiptAction)

-- | Publishes the email content within a notification to Amazon SNS.
receiptAction_sNSAction :: Lens.Lens' ReceiptAction (Prelude.Maybe SNSAction)
receiptAction_sNSAction = Lens.lens (\ReceiptAction' {sNSAction} -> sNSAction) (\s@ReceiptAction' {} a -> s {sNSAction = a} :: ReceiptAction)

-- | Calls Amazon WorkMail and, optionally, publishes a notification to
-- Amazon Amazon SNS.
receiptAction_workmailAction :: Lens.Lens' ReceiptAction (Prelude.Maybe WorkmailAction)
receiptAction_workmailAction = Lens.lens (\ReceiptAction' {workmailAction} -> workmailAction) (\s@ReceiptAction' {} a -> s {workmailAction = a} :: ReceiptAction)

-- | Rejects the received email by returning a bounce response to the sender
-- and, optionally, publishes a notification to Amazon Simple Notification
-- Service (Amazon SNS).
receiptAction_bounceAction :: Lens.Lens' ReceiptAction (Prelude.Maybe BounceAction)
receiptAction_bounceAction = Lens.lens (\ReceiptAction' {bounceAction} -> bounceAction) (\s@ReceiptAction' {} a -> s {bounceAction = a} :: ReceiptAction)

-- | Calls an AWS Lambda function, and optionally, publishes a notification
-- to Amazon SNS.
receiptAction_lambdaAction :: Lens.Lens' ReceiptAction (Prelude.Maybe LambdaAction)
receiptAction_lambdaAction = Lens.lens (\ReceiptAction' {lambdaAction} -> lambdaAction) (\s@ReceiptAction' {} a -> s {lambdaAction = a} :: ReceiptAction)

-- | Terminates the evaluation of the receipt rule set and optionally
-- publishes a notification to Amazon SNS.
receiptAction_stopAction :: Lens.Lens' ReceiptAction (Prelude.Maybe StopAction)
receiptAction_stopAction = Lens.lens (\ReceiptAction' {stopAction} -> stopAction) (\s@ReceiptAction' {} a -> s {stopAction = a} :: ReceiptAction)

-- | Saves the received message to an Amazon Simple Storage Service (Amazon
-- S3) bucket and, optionally, publishes a notification to Amazon SNS.
receiptAction_s3Action :: Lens.Lens' ReceiptAction (Prelude.Maybe S3Action)
receiptAction_s3Action = Lens.lens (\ReceiptAction' {s3Action} -> s3Action) (\s@ReceiptAction' {} a -> s {s3Action = a} :: ReceiptAction)

instance Core.FromXML ReceiptAction where
  parseXML x =
    ReceiptAction'
      Prelude.<$> (x Core..@? "AddHeaderAction")
      Prelude.<*> (x Core..@? "SNSAction")
      Prelude.<*> (x Core..@? "WorkmailAction")
      Prelude.<*> (x Core..@? "BounceAction")
      Prelude.<*> (x Core..@? "LambdaAction")
      Prelude.<*> (x Core..@? "StopAction")
      Prelude.<*> (x Core..@? "S3Action")

instance Prelude.Hashable ReceiptAction where
  hashWithSalt _salt ReceiptAction' {..} =
    _salt `Prelude.hashWithSalt` addHeaderAction
      `Prelude.hashWithSalt` sNSAction
      `Prelude.hashWithSalt` workmailAction
      `Prelude.hashWithSalt` bounceAction
      `Prelude.hashWithSalt` lambdaAction
      `Prelude.hashWithSalt` stopAction
      `Prelude.hashWithSalt` s3Action

instance Prelude.NFData ReceiptAction where
  rnf ReceiptAction' {..} =
    Prelude.rnf addHeaderAction
      `Prelude.seq` Prelude.rnf sNSAction
      `Prelude.seq` Prelude.rnf workmailAction
      `Prelude.seq` Prelude.rnf bounceAction
      `Prelude.seq` Prelude.rnf lambdaAction
      `Prelude.seq` Prelude.rnf stopAction
      `Prelude.seq` Prelude.rnf s3Action

instance Core.ToQuery ReceiptAction where
  toQuery ReceiptAction' {..} =
    Prelude.mconcat
      [ "AddHeaderAction" Core.=: addHeaderAction,
        "SNSAction" Core.=: sNSAction,
        "WorkmailAction" Core.=: workmailAction,
        "BounceAction" Core.=: bounceAction,
        "LambdaAction" Core.=: lambdaAction,
        "StopAction" Core.=: stopAction,
        "S3Action" Core.=: s3Action
      ]
