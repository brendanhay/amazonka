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
-- Module      : Network.AWS.SES.Types.SNSAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SNSAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    encoding :: Prelude.Maybe SNSActionEncoding,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify. An
    -- example of an Amazon SNS topic ARN is
    -- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
    -- Amazon SNS topics, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
    topicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  SNSAction
newSNSAction pTopicArn_ =
  SNSAction'
    { encoding = Prelude.Nothing,
      topicArn = pTopicArn_
    }

-- | The encoding to use for the email within the Amazon SNS notification.
-- UTF-8 is easier to use, but may not preserve all special characters when
-- a message was encoded with a different encoding format. Base64 preserves
-- all special characters. The default value is UTF-8.
sNSAction_encoding :: Lens.Lens' SNSAction (Prelude.Maybe SNSActionEncoding)
sNSAction_encoding = Lens.lens (\SNSAction' {encoding} -> encoding) (\s@SNSAction' {} a -> s {encoding = a} :: SNSAction)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify. An
-- example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
sNSAction_topicArn :: Lens.Lens' SNSAction Prelude.Text
sNSAction_topicArn = Lens.lens (\SNSAction' {topicArn} -> topicArn) (\s@SNSAction' {} a -> s {topicArn = a} :: SNSAction)

instance Prelude.FromXML SNSAction where
  parseXML x =
    SNSAction'
      Prelude.<$> (x Prelude..@? "Encoding")
      Prelude.<*> (x Prelude..@ "TopicArn")

instance Prelude.Hashable SNSAction

instance Prelude.NFData SNSAction

instance Prelude.ToQuery SNSAction where
  toQuery SNSAction' {..} =
    Prelude.mconcat
      [ "Encoding" Prelude.=: encoding,
        "TopicArn" Prelude.=: topicArn
      ]
