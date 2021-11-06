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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Notification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The notification used to inform a user of an update in Audit Manager.
-- For example, this includes the notification that is sent when a control
-- set is delegated for review.
--
-- /See:/ 'newNotification' smart constructor.
data Notification = Notification'
  { -- | Specifies the name of the control set that the notification is about.
    controlSetName :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specified control set.
    controlSetId :: Prelude.Maybe Prelude.Text,
    -- | The time when the notification was sent.
    eventTime :: Prelude.Maybe Core.POSIX,
    -- | The sender of the notification.
    source :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the notification.
    id :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Maybe Prelude.Text,
    -- | The description of the notification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the related assessment.
    assessmentName :: Prelude.Maybe Prelude.Text
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
-- 'controlSetName', 'notification_controlSetName' - Specifies the name of the control set that the notification is about.
--
-- 'controlSetId', 'notification_controlSetId' - The identifier for the specified control set.
--
-- 'eventTime', 'notification_eventTime' - The time when the notification was sent.
--
-- 'source', 'notification_source' - The sender of the notification.
--
-- 'id', 'notification_id' - The unique identifier for the notification.
--
-- 'assessmentId', 'notification_assessmentId' - The identifier for the specified assessment.
--
-- 'description', 'notification_description' - The description of the notification.
--
-- 'assessmentName', 'notification_assessmentName' - The name of the related assessment.
newNotification ::
  Notification
newNotification =
  Notification'
    { controlSetName = Prelude.Nothing,
      controlSetId = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      source = Prelude.Nothing,
      id = Prelude.Nothing,
      assessmentId = Prelude.Nothing,
      description = Prelude.Nothing,
      assessmentName = Prelude.Nothing
    }

-- | Specifies the name of the control set that the notification is about.
notification_controlSetName :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_controlSetName = Lens.lens (\Notification' {controlSetName} -> controlSetName) (\s@Notification' {} a -> s {controlSetName = a} :: Notification)

-- | The identifier for the specified control set.
notification_controlSetId :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_controlSetId = Lens.lens (\Notification' {controlSetId} -> controlSetId) (\s@Notification' {} a -> s {controlSetId = a} :: Notification)

-- | The time when the notification was sent.
notification_eventTime :: Lens.Lens' Notification (Prelude.Maybe Prelude.UTCTime)
notification_eventTime = Lens.lens (\Notification' {eventTime} -> eventTime) (\s@Notification' {} a -> s {eventTime = a} :: Notification) Prelude.. Lens.mapping Core._Time

-- | The sender of the notification.
notification_source :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_source = Lens.lens (\Notification' {source} -> source) (\s@Notification' {} a -> s {source = a} :: Notification)

-- | The unique identifier for the notification.
notification_id :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_id = Lens.lens (\Notification' {id} -> id) (\s@Notification' {} a -> s {id = a} :: Notification)

-- | The identifier for the specified assessment.
notification_assessmentId :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_assessmentId = Lens.lens (\Notification' {assessmentId} -> assessmentId) (\s@Notification' {} a -> s {assessmentId = a} :: Notification)

-- | The description of the notification.
notification_description :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_description = Lens.lens (\Notification' {description} -> description) (\s@Notification' {} a -> s {description = a} :: Notification)

-- | The name of the related assessment.
notification_assessmentName :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_assessmentName = Lens.lens (\Notification' {assessmentName} -> assessmentName) (\s@Notification' {} a -> s {assessmentName = a} :: Notification)

instance Core.FromJSON Notification where
  parseJSON =
    Core.withObject
      "Notification"
      ( \x ->
          Notification'
            Prelude.<$> (x Core..:? "controlSetName")
            Prelude.<*> (x Core..:? "controlSetId")
            Prelude.<*> (x Core..:? "eventTime")
            Prelude.<*> (x Core..:? "source")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "assessmentId")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "assessmentName")
      )

instance Prelude.Hashable Notification

instance Prelude.NFData Notification
