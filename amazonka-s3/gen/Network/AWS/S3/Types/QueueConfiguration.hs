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
-- Module      : Network.AWS.S3.Types.QueueConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.QueueConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.NotificationConfigurationFilter

-- | Specifies the configuration for publishing messages to an Amazon Simple
-- Queue Service (Amazon SQS) queue when Amazon S3 detects specified
-- events.
--
-- /See:/ 'newQueueConfiguration' smart constructor.
data QueueConfiguration = QueueConfiguration'
  { id :: Prelude.Maybe Prelude.Text,
    filter' :: Prelude.Maybe NotificationConfigurationFilter,
    -- | The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon
    -- S3 publishes a message when it detects events of the specified type.
    queueArn :: Prelude.Text,
    -- | A collection of bucket events for which to send notifications
    events :: [Event]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  QueueConfiguration
newQueueConfiguration pQueueArn_ =
  QueueConfiguration'
    { id = Prelude.Nothing,
      filter' = Prelude.Nothing,
      queueArn = pQueueArn_,
      events = Prelude.mempty
    }

-- | Undocumented member.
queueConfiguration_id :: Lens.Lens' QueueConfiguration (Prelude.Maybe Prelude.Text)
queueConfiguration_id = Lens.lens (\QueueConfiguration' {id} -> id) (\s@QueueConfiguration' {} a -> s {id = a} :: QueueConfiguration)

-- | Undocumented member.
queueConfiguration_filter :: Lens.Lens' QueueConfiguration (Prelude.Maybe NotificationConfigurationFilter)
queueConfiguration_filter = Lens.lens (\QueueConfiguration' {filter'} -> filter') (\s@QueueConfiguration' {} a -> s {filter' = a} :: QueueConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon
-- S3 publishes a message when it detects events of the specified type.
queueConfiguration_queueArn :: Lens.Lens' QueueConfiguration Prelude.Text
queueConfiguration_queueArn = Lens.lens (\QueueConfiguration' {queueArn} -> queueArn) (\s@QueueConfiguration' {} a -> s {queueArn = a} :: QueueConfiguration)

-- | A collection of bucket events for which to send notifications
queueConfiguration_events :: Lens.Lens' QueueConfiguration [Event]
queueConfiguration_events = Lens.lens (\QueueConfiguration' {events} -> events) (\s@QueueConfiguration' {} a -> s {events = a} :: QueueConfiguration) Prelude.. Prelude._Coerce

instance Prelude.FromXML QueueConfiguration where
  parseXML x =
    QueueConfiguration'
      Prelude.<$> (x Prelude..@? "Id")
      Prelude.<*> (x Prelude..@? "Filter")
      Prelude.<*> (x Prelude..@ "Queue")
      Prelude.<*> (Prelude.parseXMLList "Event" x)

instance Prelude.Hashable QueueConfiguration

instance Prelude.NFData QueueConfiguration

instance Prelude.ToXML QueueConfiguration where
  toXML QueueConfiguration' {..} =
    Prelude.mconcat
      [ "Id" Prelude.@= id,
        "Filter" Prelude.@= filter',
        "Queue" Prelude.@= queueArn,
        Prelude.toXMLList "Event" events
      ]
