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
-- Module      : Network.AWS.SESv2.Types.Topic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.Topic where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.SubscriptionStatus

-- | An interest group, theme, or label within a list. Lists can have
-- multiple topics.
--
-- /See:/ 'newTopic' smart constructor.
data Topic = Topic'
  { -- | A description of what the topic is about, which the contact will see.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the topic.
    topicName :: Prelude.Text,
    -- | The name of the topic the contact will see.
    displayName :: Prelude.Text,
    -- | The default subscription status to be applied to a contact if the
    -- contact has not noted their preference for subscribing to a topic.
    defaultSubscriptionStatus :: SubscriptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Topic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'topic_description' - A description of what the topic is about, which the contact will see.
--
-- 'topicName', 'topic_topicName' - The name of the topic.
--
-- 'displayName', 'topic_displayName' - The name of the topic the contact will see.
--
-- 'defaultSubscriptionStatus', 'topic_defaultSubscriptionStatus' - The default subscription status to be applied to a contact if the
-- contact has not noted their preference for subscribing to a topic.
newTopic ::
  -- | 'topicName'
  Prelude.Text ->
  -- | 'displayName'
  Prelude.Text ->
  -- | 'defaultSubscriptionStatus'
  SubscriptionStatus ->
  Topic
newTopic
  pTopicName_
  pDisplayName_
  pDefaultSubscriptionStatus_ =
    Topic'
      { description = Prelude.Nothing,
        topicName = pTopicName_,
        displayName = pDisplayName_,
        defaultSubscriptionStatus =
          pDefaultSubscriptionStatus_
      }

-- | A description of what the topic is about, which the contact will see.
topic_description :: Lens.Lens' Topic (Prelude.Maybe Prelude.Text)
topic_description = Lens.lens (\Topic' {description} -> description) (\s@Topic' {} a -> s {description = a} :: Topic)

-- | The name of the topic.
topic_topicName :: Lens.Lens' Topic Prelude.Text
topic_topicName = Lens.lens (\Topic' {topicName} -> topicName) (\s@Topic' {} a -> s {topicName = a} :: Topic)

-- | The name of the topic the contact will see.
topic_displayName :: Lens.Lens' Topic Prelude.Text
topic_displayName = Lens.lens (\Topic' {displayName} -> displayName) (\s@Topic' {} a -> s {displayName = a} :: Topic)

-- | The default subscription status to be applied to a contact if the
-- contact has not noted their preference for subscribing to a topic.
topic_defaultSubscriptionStatus :: Lens.Lens' Topic SubscriptionStatus
topic_defaultSubscriptionStatus = Lens.lens (\Topic' {defaultSubscriptionStatus} -> defaultSubscriptionStatus) (\s@Topic' {} a -> s {defaultSubscriptionStatus = a} :: Topic)

instance Core.FromJSON Topic where
  parseJSON =
    Core.withObject
      "Topic"
      ( \x ->
          Topic'
            Prelude.<$> (x Core..:? "Description")
            Prelude.<*> (x Core..: "TopicName")
            Prelude.<*> (x Core..: "DisplayName")
            Prelude.<*> (x Core..: "DefaultSubscriptionStatus")
      )

instance Prelude.Hashable Topic

instance Prelude.NFData Topic

instance Core.ToJSON Topic where
  toJSON Topic' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("TopicName" Core..= topicName),
            Prelude.Just ("DisplayName" Core..= displayName),
            Prelude.Just
              ( "DefaultSubscriptionStatus"
                  Core..= defaultSubscriptionStatus
              )
          ]
      )
