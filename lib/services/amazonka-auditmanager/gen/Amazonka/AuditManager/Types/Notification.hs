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
-- Module      : Amazonka.AuditManager.Types.Notification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Notification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The notification that informs a user of an update in Audit Manager. For
-- example, this includes the notification that\'s sent when a control set
-- is delegated for review.
--
-- /See:/ 'newNotification' smart constructor.
data Notification = Notification'
  { -- | The identifier for the assessment.
    assessmentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the related assessment.
    assessmentName :: Prelude.Maybe Prelude.Text,
    -- | The description of the notification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the notification.
    id :: Prelude.Maybe Prelude.Text,
    -- | The sender of the notification.
    source :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the control set that the notification is about.
    controlSetName :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the control set.
    controlSetId :: Prelude.Maybe Prelude.Text,
    -- | The time when the notification was sent.
    eventTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Notification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'notification_assessmentId' - The identifier for the assessment.
--
-- 'assessmentName', 'notification_assessmentName' - The name of the related assessment.
--
-- 'description', 'notification_description' - The description of the notification.
--
-- 'id', 'notification_id' - The unique identifier for the notification.
--
-- 'source', 'notification_source' - The sender of the notification.
--
-- 'controlSetName', 'notification_controlSetName' - Specifies the name of the control set that the notification is about.
--
-- 'controlSetId', 'notification_controlSetId' - The identifier for the control set.
--
-- 'eventTime', 'notification_eventTime' - The time when the notification was sent.
newNotification ::
  Notification
newNotification =
  Notification'
    { assessmentId = Prelude.Nothing,
      assessmentName = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      source = Prelude.Nothing,
      controlSetName = Prelude.Nothing,
      controlSetId = Prelude.Nothing,
      eventTime = Prelude.Nothing
    }

-- | The identifier for the assessment.
notification_assessmentId :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_assessmentId = Lens.lens (\Notification' {assessmentId} -> assessmentId) (\s@Notification' {} a -> s {assessmentId = a} :: Notification)

-- | The name of the related assessment.
notification_assessmentName :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_assessmentName = Lens.lens (\Notification' {assessmentName} -> assessmentName) (\s@Notification' {} a -> s {assessmentName = a} :: Notification)

-- | The description of the notification.
notification_description :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_description = Lens.lens (\Notification' {description} -> description) (\s@Notification' {} a -> s {description = a} :: Notification)

-- | The unique identifier for the notification.
notification_id :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_id = Lens.lens (\Notification' {id} -> id) (\s@Notification' {} a -> s {id = a} :: Notification)

-- | The sender of the notification.
notification_source :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_source = Lens.lens (\Notification' {source} -> source) (\s@Notification' {} a -> s {source = a} :: Notification)

-- | Specifies the name of the control set that the notification is about.
notification_controlSetName :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_controlSetName = Lens.lens (\Notification' {controlSetName} -> controlSetName) (\s@Notification' {} a -> s {controlSetName = a} :: Notification)

-- | The identifier for the control set.
notification_controlSetId :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_controlSetId = Lens.lens (\Notification' {controlSetId} -> controlSetId) (\s@Notification' {} a -> s {controlSetId = a} :: Notification)

-- | The time when the notification was sent.
notification_eventTime :: Lens.Lens' Notification (Prelude.Maybe Prelude.UTCTime)
notification_eventTime = Lens.lens (\Notification' {eventTime} -> eventTime) (\s@Notification' {} a -> s {eventTime = a} :: Notification) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Notification where
  parseJSON =
    Data.withObject
      "Notification"
      ( \x ->
          Notification'
            Prelude.<$> (x Data..:? "assessmentId")
            Prelude.<*> (x Data..:? "assessmentName")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "source")
            Prelude.<*> (x Data..:? "controlSetName")
            Prelude.<*> (x Data..:? "controlSetId")
            Prelude.<*> (x Data..:? "eventTime")
      )

instance Prelude.Hashable Notification where
  hashWithSalt _salt Notification' {..} =
    _salt `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` assessmentName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` controlSetName
      `Prelude.hashWithSalt` controlSetId
      `Prelude.hashWithSalt` eventTime

instance Prelude.NFData Notification where
  rnf Notification' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf assessmentName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf controlSetName
      `Prelude.seq` Prelude.rnf controlSetId
      `Prelude.seq` Prelude.rnf eventTime
