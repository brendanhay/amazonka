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
-- Module      : Network.AWS.SecurityHub.Types.AwsSnsTopicDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsSnsTopicDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsSnsTopicSubscription

-- | A wrapper type for the topic\'s ARN.
--
-- /See:/ 'newAwsSnsTopicDetails' smart constructor.
data AwsSnsTopicDetails = AwsSnsTopicDetails'
  { -- | The ID of an Amazon Web Services managed key for Amazon SNS or a
    -- customer managed key.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the topic.
    topicName :: Prelude.Maybe Prelude.Text,
    -- | The subscription\'s owner.
    owner :: Prelude.Maybe Prelude.Text,
    -- | Subscription is an embedded property that describes the subscription
    -- endpoints of an SNS topic.
    subscription :: Prelude.Maybe [AwsSnsTopicSubscription]
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
-- 'kmsMasterKeyId', 'awsSnsTopicDetails_kmsMasterKeyId' - The ID of an Amazon Web Services managed key for Amazon SNS or a
-- customer managed key.
--
-- 'topicName', 'awsSnsTopicDetails_topicName' - The name of the topic.
--
-- 'owner', 'awsSnsTopicDetails_owner' - The subscription\'s owner.
--
-- 'subscription', 'awsSnsTopicDetails_subscription' - Subscription is an embedded property that describes the subscription
-- endpoints of an SNS topic.
newAwsSnsTopicDetails ::
  AwsSnsTopicDetails
newAwsSnsTopicDetails =
  AwsSnsTopicDetails'
    { kmsMasterKeyId =
        Prelude.Nothing,
      topicName = Prelude.Nothing,
      owner = Prelude.Nothing,
      subscription = Prelude.Nothing
    }

-- | The ID of an Amazon Web Services managed key for Amazon SNS or a
-- customer managed key.
awsSnsTopicDetails_kmsMasterKeyId :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_kmsMasterKeyId = Lens.lens (\AwsSnsTopicDetails' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@AwsSnsTopicDetails' {} a -> s {kmsMasterKeyId = a} :: AwsSnsTopicDetails)

-- | The name of the topic.
awsSnsTopicDetails_topicName :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_topicName = Lens.lens (\AwsSnsTopicDetails' {topicName} -> topicName) (\s@AwsSnsTopicDetails' {} a -> s {topicName = a} :: AwsSnsTopicDetails)

-- | The subscription\'s owner.
awsSnsTopicDetails_owner :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe Prelude.Text)
awsSnsTopicDetails_owner = Lens.lens (\AwsSnsTopicDetails' {owner} -> owner) (\s@AwsSnsTopicDetails' {} a -> s {owner = a} :: AwsSnsTopicDetails)

-- | Subscription is an embedded property that describes the subscription
-- endpoints of an SNS topic.
awsSnsTopicDetails_subscription :: Lens.Lens' AwsSnsTopicDetails (Prelude.Maybe [AwsSnsTopicSubscription])
awsSnsTopicDetails_subscription = Lens.lens (\AwsSnsTopicDetails' {subscription} -> subscription) (\s@AwsSnsTopicDetails' {} a -> s {subscription = a} :: AwsSnsTopicDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsSnsTopicDetails where
  parseJSON =
    Core.withObject
      "AwsSnsTopicDetails"
      ( \x ->
          AwsSnsTopicDetails'
            Prelude.<$> (x Core..:? "KmsMasterKeyId")
            Prelude.<*> (x Core..:? "TopicName")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "Subscription" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsSnsTopicDetails

instance Prelude.NFData AwsSnsTopicDetails

instance Core.ToJSON AwsSnsTopicDetails where
  toJSON AwsSnsTopicDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsMasterKeyId" Core..=)
              Prelude.<$> kmsMasterKeyId,
            ("TopicName" Core..=) Prelude.<$> topicName,
            ("Owner" Core..=) Prelude.<$> owner,
            ("Subscription" Core..=) Prelude.<$> subscription
          ]
      )
