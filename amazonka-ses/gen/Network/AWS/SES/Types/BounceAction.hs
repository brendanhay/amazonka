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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    topicArn :: Core.Maybe Core.Text,
    -- | The SMTP enhanced status code, as defined by
    -- <https://tools.ietf.org/html/rfc3463 RFC 3463>.
    statusCode :: Core.Maybe Core.Text,
    -- | The SMTP reply code, as defined by
    -- <https://tools.ietf.org/html/rfc5321 RFC 5321>.
    smtpReplyCode :: Core.Text,
    -- | Human-readable text to include in the bounce message.
    message :: Core.Text,
    -- | The email address of the sender of the bounced email. This is the
    -- address from which the bounce message will be sent.
    sender :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'message'
  Core.Text ->
  -- | 'sender'
  Core.Text ->
  BounceAction
newBounceAction pSmtpReplyCode_ pMessage_ pSender_ =
  BounceAction'
    { topicArn = Core.Nothing,
      statusCode = Core.Nothing,
      smtpReplyCode = pSmtpReplyCode_,
      message = pMessage_,
      sender = pSender_
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
-- the bounce action is taken. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
bounceAction_topicArn :: Lens.Lens' BounceAction (Core.Maybe Core.Text)
bounceAction_topicArn = Lens.lens (\BounceAction' {topicArn} -> topicArn) (\s@BounceAction' {} a -> s {topicArn = a} :: BounceAction)

-- | The SMTP enhanced status code, as defined by
-- <https://tools.ietf.org/html/rfc3463 RFC 3463>.
bounceAction_statusCode :: Lens.Lens' BounceAction (Core.Maybe Core.Text)
bounceAction_statusCode = Lens.lens (\BounceAction' {statusCode} -> statusCode) (\s@BounceAction' {} a -> s {statusCode = a} :: BounceAction)

-- | The SMTP reply code, as defined by
-- <https://tools.ietf.org/html/rfc5321 RFC 5321>.
bounceAction_smtpReplyCode :: Lens.Lens' BounceAction Core.Text
bounceAction_smtpReplyCode = Lens.lens (\BounceAction' {smtpReplyCode} -> smtpReplyCode) (\s@BounceAction' {} a -> s {smtpReplyCode = a} :: BounceAction)

-- | Human-readable text to include in the bounce message.
bounceAction_message :: Lens.Lens' BounceAction Core.Text
bounceAction_message = Lens.lens (\BounceAction' {message} -> message) (\s@BounceAction' {} a -> s {message = a} :: BounceAction)

-- | The email address of the sender of the bounced email. This is the
-- address from which the bounce message will be sent.
bounceAction_sender :: Lens.Lens' BounceAction Core.Text
bounceAction_sender = Lens.lens (\BounceAction' {sender} -> sender) (\s@BounceAction' {} a -> s {sender = a} :: BounceAction)

instance Core.FromXML BounceAction where
  parseXML x =
    BounceAction'
      Core.<$> (x Core..@? "TopicArn")
      Core.<*> (x Core..@? "StatusCode")
      Core.<*> (x Core..@ "SmtpReplyCode")
      Core.<*> (x Core..@ "Message")
      Core.<*> (x Core..@ "Sender")

instance Core.Hashable BounceAction

instance Core.NFData BounceAction

instance Core.ToQuery BounceAction where
  toQuery BounceAction' {..} =
    Core.mconcat
      [ "TopicArn" Core.=: topicArn,
        "StatusCode" Core.=: statusCode,
        "SmtpReplyCode" Core.=: smtpReplyCode,
        "Message" Core.=: message,
        "Sender" Core.=: sender
      ]
