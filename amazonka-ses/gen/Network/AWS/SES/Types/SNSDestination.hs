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
-- Module      : Network.AWS.SES.Types.SNSDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SNSDestination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the topic ARN associated with an Amazon Simple Notification
-- Service (Amazon SNS) event destination.
--
-- Event destinations, such as Amazon SNS, are associated with
-- configuration sets, which enable you to publish email sending events.
-- For information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSNSDestination' smart constructor.
data SNSDestination = SNSDestination'
  { -- | The ARN of the Amazon SNS topic that email sending events will be
    -- published to. An example of an Amazon SNS topic ARN is
    -- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
    -- Amazon SNS topics, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
    topicARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SNSDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicARN', 'sNSDestination_topicARN' - The ARN of the Amazon SNS topic that email sending events will be
-- published to. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
newSNSDestination ::
  -- | 'topicARN'
  Prelude.Text ->
  SNSDestination
newSNSDestination pTopicARN_ =
  SNSDestination' {topicARN = pTopicARN_}

-- | The ARN of the Amazon SNS topic that email sending events will be
-- published to. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
sNSDestination_topicARN :: Lens.Lens' SNSDestination Prelude.Text
sNSDestination_topicARN = Lens.lens (\SNSDestination' {topicARN} -> topicARN) (\s@SNSDestination' {} a -> s {topicARN = a} :: SNSDestination)

instance Prelude.FromXML SNSDestination where
  parseXML x =
    SNSDestination'
      Prelude.<$> (x Prelude..@ "TopicARN")

instance Prelude.Hashable SNSDestination

instance Prelude.NFData SNSDestination

instance Prelude.ToQuery SNSDestination where
  toQuery SNSDestination' {..} =
    Prelude.mconcat ["TopicARN" Prelude.=: topicARN]
