{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobMessages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobMessages
  ( JobMessages (..),

    -- * Smart constructor
    mkJobMessages,

    -- * Lenses
    jmWarning,
    jmInfo,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides messages from the service about jobs that you have already successfully submitted.
--
-- /See:/ 'mkJobMessages' smart constructor.
data JobMessages = JobMessages'
  { -- | List of messages that warn about conditions that might cause your job not to run or to fail.
    warning :: Lude.Maybe [Lude.Text],
    -- | List of messages that are informational only and don't indicate a problem with your job.
    info :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobMessages' with the minimum fields required to make a request.
--
-- * 'warning' - List of messages that warn about conditions that might cause your job not to run or to fail.
-- * 'info' - List of messages that are informational only and don't indicate a problem with your job.
mkJobMessages ::
  JobMessages
mkJobMessages =
  JobMessages' {warning = Lude.Nothing, info = Lude.Nothing}

-- | List of messages that warn about conditions that might cause your job not to run or to fail.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmWarning :: Lens.Lens' JobMessages (Lude.Maybe [Lude.Text])
jmWarning = Lens.lens (warning :: JobMessages -> Lude.Maybe [Lude.Text]) (\s a -> s {warning = a} :: JobMessages)
{-# DEPRECATED jmWarning "Use generic-lens or generic-optics with 'warning' instead." #-}

-- | List of messages that are informational only and don't indicate a problem with your job.
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmInfo :: Lens.Lens' JobMessages (Lude.Maybe [Lude.Text])
jmInfo = Lens.lens (info :: JobMessages -> Lude.Maybe [Lude.Text]) (\s a -> s {info = a} :: JobMessages)
{-# DEPRECATED jmInfo "Use generic-lens or generic-optics with 'info' instead." #-}

instance Lude.FromJSON JobMessages where
  parseJSON =
    Lude.withObject
      "JobMessages"
      ( \x ->
          JobMessages'
            Lude.<$> (x Lude..:? "warning" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "info" Lude..!= Lude.mempty)
      )
