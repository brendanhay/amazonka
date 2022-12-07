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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.ReceiptAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | Calls an AWS Lambda function, and optionally, publishes a notification
    -- to Amazon SNS.
    lambdaAction :: Prelude.Maybe LambdaAction,
    -- | Saves the received message to an Amazon Simple Storage Service (Amazon
    -- S3) bucket and, optionally, publishes a notification to Amazon SNS.
    s3Action :: Prelude.Maybe S3Action,
    -- | Terminates the evaluation of the receipt rule set and optionally
    -- publishes a notification to Amazon SNS.
    stopAction :: Prelude.Maybe StopAction,
    -- | Publishes the email content within a notification to Amazon SNS.
    sNSAction :: Prelude.Maybe SNSAction,
    -- | Rejects the received email by returning a bounce response to the sender
    -- and, optionally, publishes a notification to Amazon Simple Notification
    -- Service (Amazon SNS).
    bounceAction :: Prelude.Maybe BounceAction,
    -- | Calls Amazon WorkMail and, optionally, publishes a notification to
    -- Amazon Amazon SNS.
    workmailAction :: Prelude.Maybe WorkmailAction
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
-- 'lambdaAction', 'receiptAction_lambdaAction' - Calls an AWS Lambda function, and optionally, publishes a notification
-- to Amazon SNS.
--
-- 's3Action', 'receiptAction_s3Action' - Saves the received message to an Amazon Simple Storage Service (Amazon
-- S3) bucket and, optionally, publishes a notification to Amazon SNS.
--
-- 'stopAction', 'receiptAction_stopAction' - Terminates the evaluation of the receipt rule set and optionally
-- publishes a notification to Amazon SNS.
--
-- 'sNSAction', 'receiptAction_sNSAction' - Publishes the email content within a notification to Amazon SNS.
--
-- 'bounceAction', 'receiptAction_bounceAction' - Rejects the received email by returning a bounce response to the sender
-- and, optionally, publishes a notification to Amazon Simple Notification
-- Service (Amazon SNS).
--
-- 'workmailAction', 'receiptAction_workmailAction' - Calls Amazon WorkMail and, optionally, publishes a notification to
-- Amazon Amazon SNS.
newReceiptAction ::
  ReceiptAction
newReceiptAction =
  ReceiptAction'
    { addHeaderAction = Prelude.Nothing,
      lambdaAction = Prelude.Nothing,
      s3Action = Prelude.Nothing,
      stopAction = Prelude.Nothing,
      sNSAction = Prelude.Nothing,
      bounceAction = Prelude.Nothing,
      workmailAction = Prelude.Nothing
    }

-- | Adds a header to the received email.
receiptAction_addHeaderAction :: Lens.Lens' ReceiptAction (Prelude.Maybe AddHeaderAction)
receiptAction_addHeaderAction = Lens.lens (\ReceiptAction' {addHeaderAction} -> addHeaderAction) (\s@ReceiptAction' {} a -> s {addHeaderAction = a} :: ReceiptAction)

-- | Calls an AWS Lambda function, and optionally, publishes a notification
-- to Amazon SNS.
receiptAction_lambdaAction :: Lens.Lens' ReceiptAction (Prelude.Maybe LambdaAction)
receiptAction_lambdaAction = Lens.lens (\ReceiptAction' {lambdaAction} -> lambdaAction) (\s@ReceiptAction' {} a -> s {lambdaAction = a} :: ReceiptAction)

-- | Saves the received message to an Amazon Simple Storage Service (Amazon
-- S3) bucket and, optionally, publishes a notification to Amazon SNS.
receiptAction_s3Action :: Lens.Lens' ReceiptAction (Prelude.Maybe S3Action)
receiptAction_s3Action = Lens.lens (\ReceiptAction' {s3Action} -> s3Action) (\s@ReceiptAction' {} a -> s {s3Action = a} :: ReceiptAction)

-- | Terminates the evaluation of the receipt rule set and optionally
-- publishes a notification to Amazon SNS.
receiptAction_stopAction :: Lens.Lens' ReceiptAction (Prelude.Maybe StopAction)
receiptAction_stopAction = Lens.lens (\ReceiptAction' {stopAction} -> stopAction) (\s@ReceiptAction' {} a -> s {stopAction = a} :: ReceiptAction)

-- | Publishes the email content within a notification to Amazon SNS.
receiptAction_sNSAction :: Lens.Lens' ReceiptAction (Prelude.Maybe SNSAction)
receiptAction_sNSAction = Lens.lens (\ReceiptAction' {sNSAction} -> sNSAction) (\s@ReceiptAction' {} a -> s {sNSAction = a} :: ReceiptAction)

-- | Rejects the received email by returning a bounce response to the sender
-- and, optionally, publishes a notification to Amazon Simple Notification
-- Service (Amazon SNS).
receiptAction_bounceAction :: Lens.Lens' ReceiptAction (Prelude.Maybe BounceAction)
receiptAction_bounceAction = Lens.lens (\ReceiptAction' {bounceAction} -> bounceAction) (\s@ReceiptAction' {} a -> s {bounceAction = a} :: ReceiptAction)

-- | Calls Amazon WorkMail and, optionally, publishes a notification to
-- Amazon Amazon SNS.
receiptAction_workmailAction :: Lens.Lens' ReceiptAction (Prelude.Maybe WorkmailAction)
receiptAction_workmailAction = Lens.lens (\ReceiptAction' {workmailAction} -> workmailAction) (\s@ReceiptAction' {} a -> s {workmailAction = a} :: ReceiptAction)

instance Data.FromXML ReceiptAction where
  parseXML x =
    ReceiptAction'
      Prelude.<$> (x Data..@? "AddHeaderAction")
      Prelude.<*> (x Data..@? "LambdaAction")
      Prelude.<*> (x Data..@? "S3Action")
      Prelude.<*> (x Data..@? "StopAction")
      Prelude.<*> (x Data..@? "SNSAction")
      Prelude.<*> (x Data..@? "BounceAction")
      Prelude.<*> (x Data..@? "WorkmailAction")

instance Prelude.Hashable ReceiptAction where
  hashWithSalt _salt ReceiptAction' {..} =
    _salt `Prelude.hashWithSalt` addHeaderAction
      `Prelude.hashWithSalt` lambdaAction
      `Prelude.hashWithSalt` s3Action
      `Prelude.hashWithSalt` stopAction
      `Prelude.hashWithSalt` sNSAction
      `Prelude.hashWithSalt` bounceAction
      `Prelude.hashWithSalt` workmailAction

instance Prelude.NFData ReceiptAction where
  rnf ReceiptAction' {..} =
    Prelude.rnf addHeaderAction
      `Prelude.seq` Prelude.rnf lambdaAction
      `Prelude.seq` Prelude.rnf s3Action
      `Prelude.seq` Prelude.rnf stopAction
      `Prelude.seq` Prelude.rnf sNSAction
      `Prelude.seq` Prelude.rnf bounceAction
      `Prelude.seq` Prelude.rnf workmailAction

instance Data.ToQuery ReceiptAction where
  toQuery ReceiptAction' {..} =
    Prelude.mconcat
      [ "AddHeaderAction" Data.=: addHeaderAction,
        "LambdaAction" Data.=: lambdaAction,
        "S3Action" Data.=: s3Action,
        "StopAction" Data.=: stopAction,
        "SNSAction" Data.=: sNSAction,
        "BounceAction" Data.=: bounceAction,
        "WorkmailAction" Data.=: workmailAction
      ]
