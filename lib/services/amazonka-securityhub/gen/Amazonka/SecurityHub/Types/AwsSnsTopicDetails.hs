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
-- Module      : Amazonka.SecurityHub.Types.AwsSnsTopicDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSnsTopicDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsSnsTopicSubscription

-- | Provides information about an Amazon SNS topic to which notifications
-- can be published.
--
-- /See:/ 'newAwsSnsTopicDetails' smart constructor.
data AwsSnsTopicDetails = AwsSnsTopicDetails'
  { -- | Indicates failed message delivery status for an Amazon SNS topic that is
    -- subscribed to a platform application endpoint.
    applicationSuccessFeedbackRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates failed message delivery status for an Amazon SNS topic that is
    -- subscribed to an Amazon Kinesis Data Firehose endpoint.
    firehoseFailureFeedbackRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates successful message delivery status for an Amazon SNS topic
    -- that is subscribed to an Amazon Kinesis Data Firehose endpoint.
    firehoseSuccessFeedbackRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates failed message delivery status for an Amazon SNS topic that is
    -- subscribed to an HTTP endpoint.
    httpFailureFeedbackRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates successful message delivery status for an Amazon SNS topic
    -- that is subscribed to an HTTP endpoint.
    httpSuccessFeedbackRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of an Amazon Web Services managed key for Amazon SNS or a
    -- customer managed key.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | The subscription\'s owner.
    owner :: Prelude.Maybe Prelude.Text,
    -- | Indicates failed message delivery status for an Amazon SNS topic that is
    -- subscribed to an Amazon SQS endpoint.
    sqsFailureFeedbackRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates successful message delivery status for an Amazon SNS topic
    -- that is subscribed to an Amazon SQS endpoint.
    sqsSuccessFeedbackRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Subscription is an embedded property that describes the subscription
    -- endpoints of an Amazon SNS topic.
    subscription :: Prelude.Maybe [AwsSnsTopicSubscription],
    -- | The name of the Amazon SNS topic.
    topicName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSnsTopicDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationSuccessFeedbackRoleArn', 'awsSnsTopicDetails_applicationSuccessFeedbackRoleArn' - Indicates failed message delivery status for an Amazon SNS topic that is
-- subscribed to a platform application endpoint.
--
-- 'firehoseFailureFeedbackRoleArn', 'awsSnsTopicDetails_firehoseFailureFeedbackRoleArn' - Indicates failed message delivery status for an Amazon SNS topic that is
-- subscribed to an Amazon Kinesis Data Firehose endpoint.
--
-- 'firehoseSuccessFeedbackRoleArn', 'awsSnsTopicDetails_firehoseSuccessFeedbackRoleArn' - Indicates successful message delivery status for an Amazon SNS topic
-- that is subscribed to an Amazon Kinesis Data Firehose endpoint.
--
-- 'httpFailureFeedbackRoleArn', 'awsSnsTopicDetails_httpFailureFeedbackRoleArn' - Indicates failed message delivery status for an Amazon SNS topic that is
-- subscribed to an HTTP endpoint.
--
-- 'httpSuccessFeedbackRoleArn', 'awsSnsTopicDetails_httpSuccessFeedbackRoleArn' - Indicates successful message delivery status for an Amazon SNS topic
-- that is subscribed to an HTTP endpoint.
--
-- 'kmsMasterKeyId', 'awsSnsTopicDetails_kmsMasterKeyId' - The ID of an Amazon Web Services managed key for Amazon SNS or a
-- customer managed key.
--
-- 'owner', 'awsSnsTopicDetails_owner' - The subscription\'s owner.
--
-- 'sqsFailureFeedbackRoleArn', 'awsSnsTopicDetails_sqsFailureFeedbackRoleArn' - Indicates failed message delivery status for an Amazon SNS topic that is
-- subscribed to an Amazon SQS endpoint.
--
-- 'sqsSuccessFeedbackRoleArn', 'awsSnsTopicDetails_sqsSuccessFeedbackRoleArn' - Indicates successful message delivery status for an Amazon SNS topic
-- that is subscribed to an Amazon SQS endpoint.
--
-- 'subscription', 'awsSnsTopicDetails_subscription' - Subscription is an embedded property that describes the subscription
-- endpoints of an Amazon SNS topic.
--
-- 'topicName', 'awsSnsTopicDetails_topicName' - The name of the Amazon SNS topic.
newAwsSnsTopicDetails ::
  AwsSnsTopicDetails
newAwsSnsTopicDetails =
  AwsSnsTopicDetails'
    { applicationSuccessFeedbackRoleArn =
        Prelude.Nothing,
      firehoseFailureFeedbackRoleArn = Prelude.Nothing,
      firehoseSuccessFeedbackRoleArn = Prelude.Nothing,
      httpFailureFeedbackRoleArn = Prelude.Nothing,
      httpSuccessFeedbackRoleArn = Prelude.Nothing,
      kmsMasterKeyId = Prelude.Nothing,
      owner = Prelude.Nothing,
      sqsFailureFeedbackRoleArn = Prelude.Nothing,
      sqsSuccessFeedbackRoleArn = Prelude.Nothing,
      subscription = Prelude.Nothing,
      topicName = Prelude.Nothing
    }

-- | Indicates failed message delivery status for an Amazon SNS topic that is
-- subscribed to a platform application endpoint.
awsSnsTopicDetails_applicationSuccessFeedbackRoleArn :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_applicationSuccessFeedbackRoleArn = Lens.lens (\AwsSnsTopicDetails' {applicationSuccessFeedbackRoleArn} -> applicationSuccessFeedbackRoleArn) (\s@AwsSnsTopicDetails' {} a -> s {applicationSuccessFeedbackRoleArn = a} :: AwsSnsTopicDetails)

-- | Indicates failed message delivery status for an Amazon SNS topic that is
-- subscribed to an Amazon Kinesis Data Firehose endpoint.
awsSnsTopicDetails_firehoseFailureFeedbackRoleArn :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_firehoseFailureFeedbackRoleArn = Lens.lens (\AwsSnsTopicDetails' {firehoseFailureFeedbackRoleArn} -> firehoseFailureFeedbackRoleArn) (\s@AwsSnsTopicDetails' {} a -> s {firehoseFailureFeedbackRoleArn = a} :: AwsSnsTopicDetails)

-- | Indicates successful message delivery status for an Amazon SNS topic
-- that is subscribed to an Amazon Kinesis Data Firehose endpoint.
awsSnsTopicDetails_firehoseSuccessFeedbackRoleArn :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_firehoseSuccessFeedbackRoleArn = Lens.lens (\AwsSnsTopicDetails' {firehoseSuccessFeedbackRoleArn} -> firehoseSuccessFeedbackRoleArn) (\s@AwsSnsTopicDetails' {} a -> s {firehoseSuccessFeedbackRoleArn = a} :: AwsSnsTopicDetails)

-- | Indicates failed message delivery status for an Amazon SNS topic that is
-- subscribed to an HTTP endpoint.
awsSnsTopicDetails_httpFailureFeedbackRoleArn :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_httpFailureFeedbackRoleArn = Lens.lens (\AwsSnsTopicDetails' {httpFailureFeedbackRoleArn} -> httpFailureFeedbackRoleArn) (\s@AwsSnsTopicDetails' {} a -> s {httpFailureFeedbackRoleArn = a} :: AwsSnsTopicDetails)

-- | Indicates successful message delivery status for an Amazon SNS topic
-- that is subscribed to an HTTP endpoint.
awsSnsTopicDetails_httpSuccessFeedbackRoleArn :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_httpSuccessFeedbackRoleArn = Lens.lens (\AwsSnsTopicDetails' {httpSuccessFeedbackRoleArn} -> httpSuccessFeedbackRoleArn) (\s@AwsSnsTopicDetails' {} a -> s {httpSuccessFeedbackRoleArn = a} :: AwsSnsTopicDetails)

-- | The ID of an Amazon Web Services managed key for Amazon SNS or a
-- customer managed key.
awsSnsTopicDetails_kmsMasterKeyId :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_kmsMasterKeyId = Lens.lens (\AwsSnsTopicDetails' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@AwsSnsTopicDetails' {} a -> s {kmsMasterKeyId = a} :: AwsSnsTopicDetails)

-- | The subscription\'s owner.
awsSnsTopicDetails_owner :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_owner = Lens.lens (\AwsSnsTopicDetails' {owner} -> owner) (\s@AwsSnsTopicDetails' {} a -> s {owner = a} :: AwsSnsTopicDetails)

-- | Indicates failed message delivery status for an Amazon SNS topic that is
-- subscribed to an Amazon SQS endpoint.
awsSnsTopicDetails_sqsFailureFeedbackRoleArn :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_sqsFailureFeedbackRoleArn = Lens.lens (\AwsSnsTopicDetails' {sqsFailureFeedbackRoleArn} -> sqsFailureFeedbackRoleArn) (\s@AwsSnsTopicDetails' {} a -> s {sqsFailureFeedbackRoleArn = a} :: AwsSnsTopicDetails)

-- | Indicates successful message delivery status for an Amazon SNS topic
-- that is subscribed to an Amazon SQS endpoint.
awsSnsTopicDetails_sqsSuccessFeedbackRoleArn :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_sqsSuccessFeedbackRoleArn = Lens.lens (\AwsSnsTopicDetails' {sqsSuccessFeedbackRoleArn} -> sqsSuccessFeedbackRoleArn) (\s@AwsSnsTopicDetails' {} a -> s {sqsSuccessFeedbackRoleArn = a} :: AwsSnsTopicDetails)

-- | Subscription is an embedded property that describes the subscription
-- endpoints of an Amazon SNS topic.
awsSnsTopicDetails_subscription :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe [AwsSnsTopicSubscription])
awsSnsTopicDetails_subscription = Lens.lens (\AwsSnsTopicDetails' {subscription} -> subscription) (\s@AwsSnsTopicDetails' {} a -> s {subscription = a} :: AwsSnsTopicDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Amazon SNS topic.
awsSnsTopicDetails_topicName :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_topicName = Lens.lens (\AwsSnsTopicDetails' {topicName} -> topicName) (\s@AwsSnsTopicDetails' {} a -> s {topicName = a} :: AwsSnsTopicDetails)

instance Data.FromJSON AwsSnsTopicDetails where
  parseJSON =
    Data.withObject
      "AwsSnsTopicDetails"
      ( \x ->
          AwsSnsTopicDetails'
            Prelude.<$> (x Data..:? "ApplicationSuccessFeedbackRoleArn")
            Prelude.<*> (x Data..:? "FirehoseFailureFeedbackRoleArn")
            Prelude.<*> (x Data..:? "FirehoseSuccessFeedbackRoleArn")
            Prelude.<*> (x Data..:? "HttpFailureFeedbackRoleArn")
            Prelude.<*> (x Data..:? "HttpSuccessFeedbackRoleArn")
            Prelude.<*> (x Data..:? "KmsMasterKeyId")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "SqsFailureFeedbackRoleArn")
            Prelude.<*> (x Data..:? "SqsSuccessFeedbackRoleArn")
            Prelude.<*> (x Data..:? "Subscription" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TopicName")
      )

instance Prelude.Hashable AwsSnsTopicDetails where
  hashWithSalt _salt AwsSnsTopicDetails' {..} =
    _salt
      `Prelude.hashWithSalt` applicationSuccessFeedbackRoleArn
      `Prelude.hashWithSalt` firehoseFailureFeedbackRoleArn
      `Prelude.hashWithSalt` firehoseSuccessFeedbackRoleArn
      `Prelude.hashWithSalt` httpFailureFeedbackRoleArn
      `Prelude.hashWithSalt` httpSuccessFeedbackRoleArn
      `Prelude.hashWithSalt` kmsMasterKeyId
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` sqsFailureFeedbackRoleArn
      `Prelude.hashWithSalt` sqsSuccessFeedbackRoleArn
      `Prelude.hashWithSalt` subscription
      `Prelude.hashWithSalt` topicName

instance Prelude.NFData AwsSnsTopicDetails where
  rnf AwsSnsTopicDetails' {..} =
    Prelude.rnf applicationSuccessFeedbackRoleArn
      `Prelude.seq` Prelude.rnf firehoseFailureFeedbackRoleArn
      `Prelude.seq` Prelude.rnf firehoseSuccessFeedbackRoleArn
      `Prelude.seq` Prelude.rnf httpFailureFeedbackRoleArn
      `Prelude.seq` Prelude.rnf httpSuccessFeedbackRoleArn
      `Prelude.seq` Prelude.rnf kmsMasterKeyId
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf sqsFailureFeedbackRoleArn
      `Prelude.seq` Prelude.rnf sqsSuccessFeedbackRoleArn
      `Prelude.seq` Prelude.rnf subscription
      `Prelude.seq` Prelude.rnf topicName

instance Data.ToJSON AwsSnsTopicDetails where
  toJSON AwsSnsTopicDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationSuccessFeedbackRoleArn" Data..=)
              Prelude.<$> applicationSuccessFeedbackRoleArn,
            ("FirehoseFailureFeedbackRoleArn" Data..=)
              Prelude.<$> firehoseFailureFeedbackRoleArn,
            ("FirehoseSuccessFeedbackRoleArn" Data..=)
              Prelude.<$> firehoseSuccessFeedbackRoleArn,
            ("HttpFailureFeedbackRoleArn" Data..=)
              Prelude.<$> httpFailureFeedbackRoleArn,
            ("HttpSuccessFeedbackRoleArn" Data..=)
              Prelude.<$> httpSuccessFeedbackRoleArn,
            ("KmsMasterKeyId" Data..=)
              Prelude.<$> kmsMasterKeyId,
            ("Owner" Data..=) Prelude.<$> owner,
            ("SqsFailureFeedbackRoleArn" Data..=)
              Prelude.<$> sqsFailureFeedbackRoleArn,
            ("SqsSuccessFeedbackRoleArn" Data..=)
              Prelude.<$> sqsSuccessFeedbackRoleArn,
            ("Subscription" Data..=) Prelude.<$> subscription,
            ("TopicName" Data..=) Prelude.<$> topicName
          ]
      )
