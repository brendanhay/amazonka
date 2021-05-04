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
-- Module      : Network.AWS.DirectoryService.Types.EventTopic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.EventTopic where

import Network.AWS.DirectoryService.Types.TopicStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about SNS topic and AWS Directory Service directory
-- associations.
--
-- /See:/ 'newEventTopic' smart constructor.
data EventTopic = EventTopic'
  { -- | The topic registration status.
    status :: Prelude.Maybe TopicStatus,
    -- | The date and time of when you associated your directory with the SNS
    -- topic.
    createdDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of an AWS SNS topic the receives status messages from the
    -- directory.
    topicName :: Prelude.Maybe Prelude.Text,
    -- | The SNS topic ARN (Amazon Resource Name).
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The Directory ID of an AWS Directory Service directory that will publish
    -- status messages to an SNS topic.
    directoryId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'eventTopic_status' - The topic registration status.
--
-- 'createdDateTime', 'eventTopic_createdDateTime' - The date and time of when you associated your directory with the SNS
-- topic.
--
-- 'topicName', 'eventTopic_topicName' - The name of an AWS SNS topic the receives status messages from the
-- directory.
--
-- 'topicArn', 'eventTopic_topicArn' - The SNS topic ARN (Amazon Resource Name).
--
-- 'directoryId', 'eventTopic_directoryId' - The Directory ID of an AWS Directory Service directory that will publish
-- status messages to an SNS topic.
newEventTopic ::
  EventTopic
newEventTopic =
  EventTopic'
    { status = Prelude.Nothing,
      createdDateTime = Prelude.Nothing,
      topicName = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      directoryId = Prelude.Nothing
    }

-- | The topic registration status.
eventTopic_status :: Lens.Lens' EventTopic (Prelude.Maybe TopicStatus)
eventTopic_status = Lens.lens (\EventTopic' {status} -> status) (\s@EventTopic' {} a -> s {status = a} :: EventTopic)

-- | The date and time of when you associated your directory with the SNS
-- topic.
eventTopic_createdDateTime :: Lens.Lens' EventTopic (Prelude.Maybe Prelude.UTCTime)
eventTopic_createdDateTime = Lens.lens (\EventTopic' {createdDateTime} -> createdDateTime) (\s@EventTopic' {} a -> s {createdDateTime = a} :: EventTopic) Prelude.. Lens.mapping Prelude._Time

-- | The name of an AWS SNS topic the receives status messages from the
-- directory.
eventTopic_topicName :: Lens.Lens' EventTopic (Prelude.Maybe Prelude.Text)
eventTopic_topicName = Lens.lens (\EventTopic' {topicName} -> topicName) (\s@EventTopic' {} a -> s {topicName = a} :: EventTopic)

-- | The SNS topic ARN (Amazon Resource Name).
eventTopic_topicArn :: Lens.Lens' EventTopic (Prelude.Maybe Prelude.Text)
eventTopic_topicArn = Lens.lens (\EventTopic' {topicArn} -> topicArn) (\s@EventTopic' {} a -> s {topicArn = a} :: EventTopic)

-- | The Directory ID of an AWS Directory Service directory that will publish
-- status messages to an SNS topic.
eventTopic_directoryId :: Lens.Lens' EventTopic (Prelude.Maybe Prelude.Text)
eventTopic_directoryId = Lens.lens (\EventTopic' {directoryId} -> directoryId) (\s@EventTopic' {} a -> s {directoryId = a} :: EventTopic)

instance Prelude.FromJSON EventTopic where
  parseJSON =
    Prelude.withObject
      "EventTopic"
      ( \x ->
          EventTopic'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "CreatedDateTime")
            Prelude.<*> (x Prelude..:? "TopicName")
            Prelude.<*> (x Prelude..:? "TopicArn")
            Prelude.<*> (x Prelude..:? "DirectoryId")
      )

instance Prelude.Hashable EventTopic

instance Prelude.NFData EventTopic
