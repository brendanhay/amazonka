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
-- Module      : Network.AWS.SES.Types.SNSAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SNSAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SES.Types.SNSActionEncoding

-- | When included in a receipt rule, this action publishes a notification to
-- Amazon Simple Notification Service (Amazon SNS). This action includes a
-- complete copy of the email content in the Amazon SNS notifications.
-- Amazon SNS notifications for all other actions simply provide
-- information about the email. They do not include the email content
-- itself.
--
-- If you own the Amazon SNS topic, you don\'t need to do anything to give
-- Amazon SES permission to publish emails to it. However, if you don\'t
-- own the Amazon SNS topic, you need to attach a policy to the topic to
-- give Amazon SES permissions to access it. For information about giving
-- permissions, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
--
-- You can only publish emails that are 150 KB or less (including the
-- header) to Amazon SNS. Larger emails will bounce. If you anticipate
-- emails larger than 150 KB, use the S3 action instead.
--
-- For information about using a receipt rule to publish an Amazon SNS
-- notification, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-sns.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSNSAction' smart constructor.
data SNSAction = SNSAction'
  { -- | The encoding to use for the email within the Amazon SNS notification.
    -- UTF-8 is easier to use, but may not preserve all special characters when
    -- a message was encoded with a different encoding format. Base64 preserves
    -- all special characters. The default value is UTF-8.
    encoding :: Core.Maybe SNSActionEncoding,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify. An
    -- example of an Amazon SNS topic ARN is
    -- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
    -- Amazon SNS topics, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
    topicArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SNSAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoding', 'sNSAction_encoding' - The encoding to use for the email within the Amazon SNS notification.
-- UTF-8 is easier to use, but may not preserve all special characters when
-- a message was encoded with a different encoding format. Base64 preserves
-- all special characters. The default value is UTF-8.
--
-- 'topicArn', 'sNSAction_topicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify. An
-- example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
newSNSAction ::
  -- | 'topicArn'
  Core.Text ->
  SNSAction
newSNSAction pTopicArn_ =
  SNSAction'
    { encoding = Core.Nothing,
      topicArn = pTopicArn_
    }

-- | The encoding to use for the email within the Amazon SNS notification.
-- UTF-8 is easier to use, but may not preserve all special characters when
-- a message was encoded with a different encoding format. Base64 preserves
-- all special characters. The default value is UTF-8.
sNSAction_encoding :: Lens.Lens' SNSAction (Core.Maybe SNSActionEncoding)
sNSAction_encoding = Lens.lens (\SNSAction' {encoding} -> encoding) (\s@SNSAction' {} a -> s {encoding = a} :: SNSAction)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify. An
-- example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
sNSAction_topicArn :: Lens.Lens' SNSAction Core.Text
sNSAction_topicArn = Lens.lens (\SNSAction' {topicArn} -> topicArn) (\s@SNSAction' {} a -> s {topicArn = a} :: SNSAction)

instance Core.FromXML SNSAction where
  parseXML x =
    SNSAction'
      Core.<$> (x Core..@? "Encoding")
      Core.<*> (x Core..@ "TopicArn")

instance Core.Hashable SNSAction

instance Core.NFData SNSAction

instance Core.ToQuery SNSAction where
  toQuery SNSAction' {..} =
    Core.mconcat
      [ "Encoding" Core.=: encoding,
        "TopicArn" Core.=: topicArn
      ]
