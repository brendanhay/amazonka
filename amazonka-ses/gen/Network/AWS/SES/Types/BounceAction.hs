{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SES.Types.BounceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BounceAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
    -- the bounce action is taken. An example of an Amazon SNS topic ARN is
    -- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
    -- Amazon SNS topics, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The SMTP enhanced status code, as defined by
    -- <https://tools.ietf.org/html/rfc3463 RFC 3463>.
    statusCode :: Prelude.Maybe Prelude.Text,
    -- | The SMTP reply code, as defined by
    -- <https://tools.ietf.org/html/rfc5321 RFC 5321>.
    smtpReplyCode :: Prelude.Text,
    -- | Human-readable text to include in the bounce message.
    message :: Prelude.Text,
    -- | The email address of the sender of the bounced email. This is the
    -- address from which the bounce message will be sent.
    sender :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BounceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'bounceAction_topicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
-- the bounce action is taken. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
--
-- 'statusCode', 'bounceAction_statusCode' - The SMTP enhanced status code, as defined by
-- <https://tools.ietf.org/html/rfc3463 RFC 3463>.
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
    { topicArn = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      smtpReplyCode = pSmtpReplyCode_,
      message = pMessage_,
      sender = pSender_
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
-- the bounce action is taken. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
bounceAction_topicArn :: Lens.Lens' BounceAction (Prelude.Maybe Prelude.Text)
bounceAction_topicArn = Lens.lens (\BounceAction' {topicArn} -> topicArn) (\s@BounceAction' {} a -> s {topicArn = a} :: BounceAction)

-- | The SMTP enhanced status code, as defined by
-- <https://tools.ietf.org/html/rfc3463 RFC 3463>.
bounceAction_statusCode :: Lens.Lens' BounceAction (Prelude.Maybe Prelude.Text)
bounceAction_statusCode = Lens.lens (\BounceAction' {statusCode} -> statusCode) (\s@BounceAction' {} a -> s {statusCode = a} :: BounceAction)

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

instance Prelude.FromXML BounceAction where
  parseXML x =
    BounceAction'
      Prelude.<$> (x Prelude..@? "TopicArn")
      Prelude.<*> (x Prelude..@? "StatusCode")
      Prelude.<*> (x Prelude..@ "SmtpReplyCode")
      Prelude.<*> (x Prelude..@ "Message")
      Prelude.<*> (x Prelude..@ "Sender")

instance Prelude.Hashable BounceAction

instance Prelude.NFData BounceAction

instance Prelude.ToQuery BounceAction where
  toQuery BounceAction' {..} =
    Prelude.mconcat
      [ "TopicArn" Prelude.=: topicArn,
        "StatusCode" Prelude.=: statusCode,
        "SmtpReplyCode" Prelude.=: smtpReplyCode,
        "Message" Prelude.=: message,
        "Sender" Prelude.=: sender
      ]
