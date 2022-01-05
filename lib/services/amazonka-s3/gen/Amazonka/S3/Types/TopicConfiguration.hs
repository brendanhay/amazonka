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
-- Module      : Amazonka.S3.Types.TopicConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.TopicConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Event
import Amazonka.S3.Types.NotificationConfigurationFilter

-- | A container for specifying the configuration for publication of messages
-- to an Amazon Simple Notification Service (Amazon SNS) topic when Amazon
-- S3 detects specified events.
--
-- /See:/ 'newTopicConfiguration' smart constructor.
data TopicConfiguration = TopicConfiguration'
  { id :: Prelude.Maybe Prelude.Text,
    filter' :: Prelude.Maybe NotificationConfigurationFilter,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon
    -- S3 publishes a message when it detects events of the specified type.
    topicArn :: Prelude.Text,
    -- | The Amazon S3 bucket event about which to send notifications. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types>
    -- in the /Amazon S3 User Guide/.
    events :: [Event]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'topicConfiguration_id' - Undocumented member.
--
-- 'filter'', 'topicConfiguration_filter' - Undocumented member.
--
-- 'topicArn', 'topicConfiguration_topicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon
-- S3 publishes a message when it detects events of the specified type.
--
-- 'events', 'topicConfiguration_events' - The Amazon S3 bucket event about which to send notifications. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types>
-- in the /Amazon S3 User Guide/.
newTopicConfiguration ::
  -- | 'topicArn'
  Prelude.Text ->
  TopicConfiguration
newTopicConfiguration pTopicArn_ =
  TopicConfiguration'
    { id = Prelude.Nothing,
      filter' = Prelude.Nothing,
      topicArn = pTopicArn_,
      events = Prelude.mempty
    }

-- | Undocumented member.
topicConfiguration_id :: Lens.Lens' TopicConfiguration (Prelude.Maybe Prelude.Text)
topicConfiguration_id = Lens.lens (\TopicConfiguration' {id} -> id) (\s@TopicConfiguration' {} a -> s {id = a} :: TopicConfiguration)

-- | Undocumented member.
topicConfiguration_filter :: Lens.Lens' TopicConfiguration (Prelude.Maybe NotificationConfigurationFilter)
topicConfiguration_filter = Lens.lens (\TopicConfiguration' {filter'} -> filter') (\s@TopicConfiguration' {} a -> s {filter' = a} :: TopicConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon
-- S3 publishes a message when it detects events of the specified type.
topicConfiguration_topicArn :: Lens.Lens' TopicConfiguration Prelude.Text
topicConfiguration_topicArn = Lens.lens (\TopicConfiguration' {topicArn} -> topicArn) (\s@TopicConfiguration' {} a -> s {topicArn = a} :: TopicConfiguration)

-- | The Amazon S3 bucket event about which to send notifications. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types>
-- in the /Amazon S3 User Guide/.
topicConfiguration_events :: Lens.Lens' TopicConfiguration [Event]
topicConfiguration_events = Lens.lens (\TopicConfiguration' {events} -> events) (\s@TopicConfiguration' {} a -> s {events = a} :: TopicConfiguration) Prelude.. Lens.coerced

instance Core.FromXML TopicConfiguration where
  parseXML x =
    TopicConfiguration'
      Prelude.<$> (x Core..@? "Id")
      Prelude.<*> (x Core..@? "Filter")
      Prelude.<*> (x Core..@ "Topic")
      Prelude.<*> (Core.parseXMLList "Event" x)

instance Prelude.Hashable TopicConfiguration where
  hashWithSalt _salt TopicConfiguration' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` events

instance Prelude.NFData TopicConfiguration where
  rnf TopicConfiguration' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf events

instance Core.ToXML TopicConfiguration where
  toXML TopicConfiguration' {..} =
    Prelude.mconcat
      [ "Id" Core.@= id,
        "Filter" Core.@= filter',
        "Topic" Core.@= topicArn,
        Core.toXMLList "Event" events
      ]
