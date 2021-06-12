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
-- Module      : Network.AWS.S3.Types.QueueConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.QueueConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.NotificationConfigurationFilter

-- | Specifies the configuration for publishing messages to an Amazon Simple
-- Queue Service (Amazon SQS) queue when Amazon S3 detects specified
-- events.
--
-- /See:/ 'newQueueConfiguration' smart constructor.
data QueueConfiguration = QueueConfiguration'
  { id :: Core.Maybe Core.Text,
    filter' :: Core.Maybe NotificationConfigurationFilter,
    -- | The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon
    -- S3 publishes a message when it detects events of the specified type.
    queueArn :: Core.Text,
    -- | A collection of bucket events for which to send notifications
    events :: [Event]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueueConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'queueConfiguration_id' - Undocumented member.
--
-- 'filter'', 'queueConfiguration_filter' - Undocumented member.
--
-- 'queueArn', 'queueConfiguration_queueArn' - The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon
-- S3 publishes a message when it detects events of the specified type.
--
-- 'events', 'queueConfiguration_events' - A collection of bucket events for which to send notifications
newQueueConfiguration ::
  -- | 'queueArn'
  Core.Text ->
  QueueConfiguration
newQueueConfiguration pQueueArn_ =
  QueueConfiguration'
    { id = Core.Nothing,
      filter' = Core.Nothing,
      queueArn = pQueueArn_,
      events = Core.mempty
    }

-- | Undocumented member.
queueConfiguration_id :: Lens.Lens' QueueConfiguration (Core.Maybe Core.Text)
queueConfiguration_id = Lens.lens (\QueueConfiguration' {id} -> id) (\s@QueueConfiguration' {} a -> s {id = a} :: QueueConfiguration)

-- | Undocumented member.
queueConfiguration_filter :: Lens.Lens' QueueConfiguration (Core.Maybe NotificationConfigurationFilter)
queueConfiguration_filter = Lens.lens (\QueueConfiguration' {filter'} -> filter') (\s@QueueConfiguration' {} a -> s {filter' = a} :: QueueConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon
-- S3 publishes a message when it detects events of the specified type.
queueConfiguration_queueArn :: Lens.Lens' QueueConfiguration Core.Text
queueConfiguration_queueArn = Lens.lens (\QueueConfiguration' {queueArn} -> queueArn) (\s@QueueConfiguration' {} a -> s {queueArn = a} :: QueueConfiguration)

-- | A collection of bucket events for which to send notifications
queueConfiguration_events :: Lens.Lens' QueueConfiguration [Event]
queueConfiguration_events = Lens.lens (\QueueConfiguration' {events} -> events) (\s@QueueConfiguration' {} a -> s {events = a} :: QueueConfiguration) Core.. Lens._Coerce

instance Core.FromXML QueueConfiguration where
  parseXML x =
    QueueConfiguration'
      Core.<$> (x Core..@? "Id")
      Core.<*> (x Core..@? "Filter")
      Core.<*> (x Core..@ "Queue")
      Core.<*> (Core.parseXMLList "Event" x)

instance Core.Hashable QueueConfiguration

instance Core.NFData QueueConfiguration

instance Core.ToXML QueueConfiguration where
  toXML QueueConfiguration' {..} =
    Core.mconcat
      [ "Id" Core.@= id,
        "Filter" Core.@= filter',
        "Queue" Core.@= queueArn,
        Core.toXMLList "Event" events
      ]
