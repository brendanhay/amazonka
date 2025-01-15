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
-- Module      : Amazonka.SES.Types.BounceAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.BounceAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When included in a receipt rule, this action rejects the received email
-- by returning a bounce response to the sender and, optionally, publishes
-- a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- For information about sending a bounce message in response to a received
-- email, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-bounce.html Amazon SES Developer Guide>.
--
-- /See:/ 'newBounceAction' smart constructor.
data BounceAction = BounceAction'
  { -- | The SMTP enhanced status code, as defined by
    -- <https://tools.ietf.org/html/rfc3463 RFC 3463>.
    statusCode :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
    -- the bounce action is taken. An example of an Amazon SNS topic ARN is
    -- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
    -- Amazon SNS topics, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The SMTP reply code, as defined by
    -- <https://tools.ietf.org/html/rfc5321 RFC 5321>.
    smtpReplyCode :: Prelude.Text,
    -- | Human-readable text to include in the bounce message.
    message :: Prelude.Text,
    -- | The email address of the sender of the bounced email. This is the
    -- address from which the bounce message will be sent.
    sender :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BounceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCode', 'bounceAction_statusCode' - The SMTP enhanced status code, as defined by
-- <https://tools.ietf.org/html/rfc3463 RFC 3463>.
--
-- 'topicArn', 'bounceAction_topicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
-- the bounce action is taken. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
--
-- 'smtpReplyCode', 'bounceAction_smtpReplyCode' - The SMTP reply code, as defined by
-- <https://tools.ietf.org/html/rfc5321 RFC 5321>.
--
-- 'message', 'bounceAction_message' - Human-readable text to include in the bounce message.
--
-- 'sender', 'bounceAction_sender' - The email address of the sender of the bounced email. This is the
-- address from which the bounce message will be sent.
newBounceAction ::
  -- | 'smtpReplyCode'
  Prelude.Text ->
  -- | 'message'
  Prelude.Text ->
  -- | 'sender'
  Prelude.Text ->
  BounceAction
newBounceAction pSmtpReplyCode_ pMessage_ pSender_ =
  BounceAction'
    { statusCode = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      smtpReplyCode = pSmtpReplyCode_,
      message = pMessage_,
      sender = pSender_
    }

-- | The SMTP enhanced status code, as defined by
-- <https://tools.ietf.org/html/rfc3463 RFC 3463>.
bounceAction_statusCode :: Lens.Lens' BounceAction (Prelude.Maybe Prelude.Text)
bounceAction_statusCode = Lens.lens (\BounceAction' {statusCode} -> statusCode) (\s@BounceAction' {} a -> s {statusCode = a} :: BounceAction)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
-- the bounce action is taken. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
bounceAction_topicArn :: Lens.Lens' BounceAction (Prelude.Maybe Prelude.Text)
bounceAction_topicArn = Lens.lens (\BounceAction' {topicArn} -> topicArn) (\s@BounceAction' {} a -> s {topicArn = a} :: BounceAction)

-- | The SMTP reply code, as defined by
-- <https://tools.ietf.org/html/rfc5321 RFC 5321>.
bounceAction_smtpReplyCode :: Lens.Lens' BounceAction Prelude.Text
bounceAction_smtpReplyCode = Lens.lens (\BounceAction' {smtpReplyCode} -> smtpReplyCode) (\s@BounceAction' {} a -> s {smtpReplyCode = a} :: BounceAction)

-- | Human-readable text to include in the bounce message.
bounceAction_message :: Lens.Lens' BounceAction Prelude.Text
bounceAction_message = Lens.lens (\BounceAction' {message} -> message) (\s@BounceAction' {} a -> s {message = a} :: BounceAction)

-- | The email address of the sender of the bounced email. This is the
-- address from which the bounce message will be sent.
bounceAction_sender :: Lens.Lens' BounceAction Prelude.Text
bounceAction_sender = Lens.lens (\BounceAction' {sender} -> sender) (\s@BounceAction' {} a -> s {sender = a} :: BounceAction)

instance Data.FromXML BounceAction where
  parseXML x =
    BounceAction'
      Prelude.<$> (x Data..@? "StatusCode")
      Prelude.<*> (x Data..@? "TopicArn")
      Prelude.<*> (x Data..@ "SmtpReplyCode")
      Prelude.<*> (x Data..@ "Message")
      Prelude.<*> (x Data..@ "Sender")

instance Prelude.Hashable BounceAction where
  hashWithSalt _salt BounceAction' {..} =
    _salt
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` smtpReplyCode
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` sender

instance Prelude.NFData BounceAction where
  rnf BounceAction' {..} =
    Prelude.rnf statusCode `Prelude.seq`
      Prelude.rnf topicArn `Prelude.seq`
        Prelude.rnf smtpReplyCode `Prelude.seq`
          Prelude.rnf message `Prelude.seq`
            Prelude.rnf sender

instance Data.ToQuery BounceAction where
  toQuery BounceAction' {..} =
    Prelude.mconcat
      [ "StatusCode" Data.=: statusCode,
        "TopicArn" Data.=: topicArn,
        "SmtpReplyCode" Data.=: smtpReplyCode,
        "Message" Data.=: message,
        "Sender" Data.=: sender
      ]
