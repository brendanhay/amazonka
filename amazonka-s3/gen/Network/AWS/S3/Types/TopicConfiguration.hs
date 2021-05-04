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
-- Module      : Network.AWS.S3.Types.TopicConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.TopicConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.NotificationConfigurationFilter

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
    -- in the /Amazon Simple Storage Service Developer Guide/.
    events :: [Event]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- in the /Amazon Simple Storage Service Developer Guide/.
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
-- in the /Amazon Simple Storage Service Developer Guide/.
topicConfiguration_events :: Lens.Lens' TopicConfiguration [Event]
topicConfiguration_events = Lens.lens (\TopicConfiguration' {events} -> events) (\s@TopicConfiguration' {} a -> s {events = a} :: TopicConfiguration) Prelude.. Prelude._Coerce

instance Prelude.FromXML TopicConfiguration where
  parseXML x =
    TopicConfiguration'
      Prelude.<$> (x Prelude..@? "Id")
      Prelude.<*> (x Prelude..@? "Filter")
      Prelude.<*> (x Prelude..@ "Topic")
      Prelude.<*> (Prelude.parseXMLList "Event" x)

instance Prelude.Hashable TopicConfiguration

instance Prelude.NFData TopicConfiguration

instance Prelude.ToXML TopicConfiguration where
  toXML TopicConfiguration' {..} =
    Prelude.mconcat
      [ "Id" Prelude.@= id,
        "Filter" Prelude.@= filter',
        "Topic" Prelude.@= topicArn,
        Prelude.toXMLList "Event" events
      ]
