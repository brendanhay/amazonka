{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ContainerSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ContainerSummary
  ( ContainerSummary (..),

    -- * Smart constructor
    mkContainerSummary,

    -- * Lenses
    csReason,
    csExitCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing summary details of a container within a job.
--
-- /See:/ 'mkContainerSummary' smart constructor.
data ContainerSummary = ContainerSummary'
  { -- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
    reason :: Lude.Maybe Lude.Text,
    -- | The exit code to return upon completion.
    exitCode :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerSummary' with the minimum fields required to make a request.
--
-- * 'reason' - A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
-- * 'exitCode' - The exit code to return upon completion.
mkContainerSummary ::
  ContainerSummary
mkContainerSummary =
  ContainerSummary' {reason = Lude.Nothing, exitCode = Lude.Nothing}

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csReason :: Lens.Lens' ContainerSummary (Lude.Maybe Lude.Text)
csReason = Lens.lens (reason :: ContainerSummary -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: ContainerSummary)
{-# DEPRECATED csReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The exit code to return upon completion.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csExitCode :: Lens.Lens' ContainerSummary (Lude.Maybe Lude.Int)
csExitCode = Lens.lens (exitCode :: ContainerSummary -> Lude.Maybe Lude.Int) (\s a -> s {exitCode = a} :: ContainerSummary)
{-# DEPRECATED csExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

instance Lude.FromJSON ContainerSummary where
  parseJSON =
    Lude.withObject
      "ContainerSummary"
      ( \x ->
          ContainerSummary'
            Lude.<$> (x Lude..:? "reason") Lude.<*> (x Lude..:? "exitCode")
      )
