{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Notifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Notifications
  ( Notifications (..),

    -- * Smart constructor
    mkNotifications,

    -- * Lenses
    nError,
    nWarning,
    nProgressing,
    nCompleted,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon Simple Notification Service (Amazon SNS) topic or topics to notify in order to report job status.
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
-- /See:/ 'mkNotifications' smart constructor.
data Notifications = Notifications'
  { -- | The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
    error :: Lude.Maybe Lude.Text,
    -- | The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
    warning :: Lude.Maybe Lude.Text,
    -- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
    progressing :: Lude.Maybe Lude.Text,
    -- | The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
    completed :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Notifications' with the minimum fields required to make a request.
--
-- * 'error' - The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
-- * 'warning' - The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
-- * 'progressing' - The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
-- * 'completed' - The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
mkNotifications ::
  Notifications
mkNotifications =
  Notifications'
    { error = Lude.Nothing,
      warning = Lude.Nothing,
      progressing = Lude.Nothing,
      completed = Lude.Nothing
    }

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nError :: Lens.Lens' Notifications (Lude.Maybe Lude.Text)
nError = Lens.lens (error :: Notifications -> Lude.Maybe Lude.Text) (\s a -> s {error = a} :: Notifications)
{-# DEPRECATED nError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nWarning :: Lens.Lens' Notifications (Lude.Maybe Lude.Text)
nWarning = Lens.lens (warning :: Notifications -> Lude.Maybe Lude.Text) (\s a -> s {warning = a} :: Notifications)
{-# DEPRECATED nWarning "Use generic-lens or generic-optics with 'warning' instead." #-}

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
--
-- /Note:/ Consider using 'progressing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nProgressing :: Lens.Lens' Notifications (Lude.Maybe Lude.Text)
nProgressing = Lens.lens (progressing :: Notifications -> Lude.Maybe Lude.Text) (\s a -> s {progressing = a} :: Notifications)
{-# DEPRECATED nProgressing "Use generic-lens or generic-optics with 'progressing' instead." #-}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
--
-- /Note:/ Consider using 'completed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCompleted :: Lens.Lens' Notifications (Lude.Maybe Lude.Text)
nCompleted = Lens.lens (completed :: Notifications -> Lude.Maybe Lude.Text) (\s a -> s {completed = a} :: Notifications)
{-# DEPRECATED nCompleted "Use generic-lens or generic-optics with 'completed' instead." #-}

instance Lude.FromJSON Notifications where
  parseJSON =
    Lude.withObject
      "Notifications"
      ( \x ->
          Notifications'
            Lude.<$> (x Lude..:? "Error")
            Lude.<*> (x Lude..:? "Warning")
            Lude.<*> (x Lude..:? "Progressing")
            Lude.<*> (x Lude..:? "Completed")
      )

instance Lude.ToJSON Notifications where
  toJSON Notifications' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Error" Lude..=) Lude.<$> error,
            ("Warning" Lude..=) Lude.<$> warning,
            ("Progressing" Lude..=) Lude.<$> progressing,
            ("Completed" Lude..=) Lude.<$> completed
          ]
      )
