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
-- Module      : Amazonka.SESV2.Types.Topic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.Topic where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.SubscriptionStatus

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

instance Data.FromJSON Topic where
  parseJSON =
    Data.withObject
      "Topic"
      ( \x ->
          Topic'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..: "TopicName")
            Prelude.<*> (x Data..: "DisplayName")
            Prelude.<*> (x Data..: "DefaultSubscriptionStatus")
      )

instance Prelude.Hashable Topic where
  hashWithSalt _salt Topic' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` topicName
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` defaultSubscriptionStatus

instance Prelude.NFData Topic where
  rnf Topic' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf topicName `Prelude.seq`
        Prelude.rnf displayName `Prelude.seq`
          Prelude.rnf defaultSubscriptionStatus

instance Data.ToJSON Topic where
  toJSON Topic' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("TopicName" Data..= topicName),
            Prelude.Just ("DisplayName" Data..= displayName),
            Prelude.Just
              ( "DefaultSubscriptionStatus"
                  Data..= defaultSubscriptionStatus
              )
          ]
      )
