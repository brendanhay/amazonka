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
-- Module      : Network.AWS.ElasticTranscoder.Types.Notifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Notifications where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Amazon Simple Notification Service (Amazon SNS) topic or topics to
-- notify in order to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in
-- the Amazon SNS console.
--
-- /See:/ 'newNotifications' smart constructor.
data Notifications = Notifications'
  { -- | The Amazon SNS topic that you want to notify when Elastic Transcoder
    -- encounters a warning condition.
    warning :: Core.Maybe Core.Text,
    -- | The Amazon SNS topic that you want to notify when Elastic Transcoder
    -- encounters an error condition.
    error :: Core.Maybe Core.Text,
    -- | The Amazon Simple Notification Service (Amazon SNS) topic that you want
    -- to notify when Elastic Transcoder has started to process the job.
    progressing :: Core.Maybe Core.Text,
    -- | The Amazon SNS topic that you want to notify when Elastic Transcoder has
    -- finished processing the job.
    completed :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Notifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warning', 'notifications_warning' - The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters a warning condition.
--
-- 'error', 'notifications_error' - The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters an error condition.
--
-- 'progressing', 'notifications_progressing' - The Amazon Simple Notification Service (Amazon SNS) topic that you want
-- to notify when Elastic Transcoder has started to process the job.
--
-- 'completed', 'notifications_completed' - The Amazon SNS topic that you want to notify when Elastic Transcoder has
-- finished processing the job.
newNotifications ::
  Notifications
newNotifications =
  Notifications'
    { warning = Core.Nothing,
      error = Core.Nothing,
      progressing = Core.Nothing,
      completed = Core.Nothing
    }

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters a warning condition.
notifications_warning :: Lens.Lens' Notifications (Core.Maybe Core.Text)
notifications_warning = Lens.lens (\Notifications' {warning} -> warning) (\s@Notifications' {} a -> s {warning = a} :: Notifications)

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters an error condition.
notifications_error :: Lens.Lens' Notifications (Core.Maybe Core.Text)
notifications_error = Lens.lens (\Notifications' {error} -> error) (\s@Notifications' {} a -> s {error = a} :: Notifications)

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want
-- to notify when Elastic Transcoder has started to process the job.
notifications_progressing :: Lens.Lens' Notifications (Core.Maybe Core.Text)
notifications_progressing = Lens.lens (\Notifications' {progressing} -> progressing) (\s@Notifications' {} a -> s {progressing = a} :: Notifications)

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder has
-- finished processing the job.
notifications_completed :: Lens.Lens' Notifications (Core.Maybe Core.Text)
notifications_completed = Lens.lens (\Notifications' {completed} -> completed) (\s@Notifications' {} a -> s {completed = a} :: Notifications)

instance Core.FromJSON Notifications where
  parseJSON =
    Core.withObject
      "Notifications"
      ( \x ->
          Notifications'
            Core.<$> (x Core..:? "Warning")
            Core.<*> (x Core..:? "Error")
            Core.<*> (x Core..:? "Progressing")
            Core.<*> (x Core..:? "Completed")
      )

instance Core.Hashable Notifications

instance Core.NFData Notifications

instance Core.ToJSON Notifications where
  toJSON Notifications' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Warning" Core..=) Core.<$> warning,
            ("Error" Core..=) Core.<$> error,
            ("Progressing" Core..=) Core.<$> progressing,
            ("Completed" Core..=) Core.<$> completed
          ]
      )
