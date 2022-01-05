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
-- Module      : Amazonka.ElasticTranscoder.Types.Notifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Notifications where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Simple Notification Service (Amazon SNS) topic or topics to
-- notify in order to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in
-- the Amazon SNS console.
--
-- /See:/ 'newNotifications' smart constructor.
data Notifications = Notifications'
  { -- | The Amazon SNS topic that you want to notify when Elastic Transcoder
    -- encounters an error condition.
    error :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SNS topic that you want to notify when Elastic Transcoder
    -- encounters a warning condition.
    warning :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Notification Service (Amazon SNS) topic that you want
    -- to notify when Elastic Transcoder has started to process the job.
    progressing :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SNS topic that you want to notify when Elastic Transcoder has
    -- finished processing the job.
    completed :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Notifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'notifications_error' - The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters an error condition.
--
-- 'warning', 'notifications_warning' - The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters a warning condition.
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
    { error = Prelude.Nothing,
      warning = Prelude.Nothing,
      progressing = Prelude.Nothing,
      completed = Prelude.Nothing
    }

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters an error condition.
notifications_error :: Lens.Lens' Notifications (Prelude.Maybe Prelude.Text)
notifications_error = Lens.lens (\Notifications' {error} -> error) (\s@Notifications' {} a -> s {error = a} :: Notifications)

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters a warning condition.
notifications_warning :: Lens.Lens' Notifications (Prelude.Maybe Prelude.Text)
notifications_warning = Lens.lens (\Notifications' {warning} -> warning) (\s@Notifications' {} a -> s {warning = a} :: Notifications)

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want
-- to notify when Elastic Transcoder has started to process the job.
notifications_progressing :: Lens.Lens' Notifications (Prelude.Maybe Prelude.Text)
notifications_progressing = Lens.lens (\Notifications' {progressing} -> progressing) (\s@Notifications' {} a -> s {progressing = a} :: Notifications)

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder has
-- finished processing the job.
notifications_completed :: Lens.Lens' Notifications (Prelude.Maybe Prelude.Text)
notifications_completed = Lens.lens (\Notifications' {completed} -> completed) (\s@Notifications' {} a -> s {completed = a} :: Notifications)

instance Core.FromJSON Notifications where
  parseJSON =
    Core.withObject
      "Notifications"
      ( \x ->
          Notifications'
            Prelude.<$> (x Core..:? "Error")
            Prelude.<*> (x Core..:? "Warning")
            Prelude.<*> (x Core..:? "Progressing")
            Prelude.<*> (x Core..:? "Completed")
      )

instance Prelude.Hashable Notifications where
  hashWithSalt _salt Notifications' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` warning
      `Prelude.hashWithSalt` progressing
      `Prelude.hashWithSalt` completed

instance Prelude.NFData Notifications where
  rnf Notifications' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf warning
      `Prelude.seq` Prelude.rnf progressing
      `Prelude.seq` Prelude.rnf completed

instance Core.ToJSON Notifications where
  toJSON Notifications' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Error" Core..=) Prelude.<$> error,
            ("Warning" Core..=) Prelude.<$> warning,
            ("Progressing" Core..=) Prelude.<$> progressing,
            ("Completed" Core..=) Prelude.<$> completed
          ]
      )
