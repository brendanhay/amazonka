{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionType
  ( ScheduledActionType (..),

    -- * Smart constructor
    mkScheduledActionType,

    -- * Lenses
    satResizeCluster,
    satResumeCluster,
    satPauseCluster,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.PauseClusterMessage
import Network.AWS.Redshift.Types.ResizeClusterMessage
import Network.AWS.Redshift.Types.ResumeClusterMessage

-- | The action type that specifies an Amazon Redshift API operation that is supported by the Amazon Redshift scheduler.
--
-- /See:/ 'mkScheduledActionType' smart constructor.
data ScheduledActionType = ScheduledActionType'
  { -- | An action that runs a @ResizeCluster@ API operation.
    resizeCluster :: Lude.Maybe ResizeClusterMessage,
    -- | An action that runs a @ResumeCluster@ API operation.
    resumeCluster :: Lude.Maybe ResumeClusterMessage,
    -- | An action that runs a @PauseCluster@ API operation.
    pauseCluster :: Lude.Maybe PauseClusterMessage
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledActionType' with the minimum fields required to make a request.
--
-- * 'resizeCluster' - An action that runs a @ResizeCluster@ API operation.
-- * 'resumeCluster' - An action that runs a @ResumeCluster@ API operation.
-- * 'pauseCluster' - An action that runs a @PauseCluster@ API operation.
mkScheduledActionType ::
  ScheduledActionType
mkScheduledActionType =
  ScheduledActionType'
    { resizeCluster = Lude.Nothing,
      resumeCluster = Lude.Nothing,
      pauseCluster = Lude.Nothing
    }

-- | An action that runs a @ResizeCluster@ API operation.
--
-- /Note:/ Consider using 'resizeCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satResizeCluster :: Lens.Lens' ScheduledActionType (Lude.Maybe ResizeClusterMessage)
satResizeCluster = Lens.lens (resizeCluster :: ScheduledActionType -> Lude.Maybe ResizeClusterMessage) (\s a -> s {resizeCluster = a} :: ScheduledActionType)
{-# DEPRECATED satResizeCluster "Use generic-lens or generic-optics with 'resizeCluster' instead." #-}

-- | An action that runs a @ResumeCluster@ API operation.
--
-- /Note:/ Consider using 'resumeCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satResumeCluster :: Lens.Lens' ScheduledActionType (Lude.Maybe ResumeClusterMessage)
satResumeCluster = Lens.lens (resumeCluster :: ScheduledActionType -> Lude.Maybe ResumeClusterMessage) (\s a -> s {resumeCluster = a} :: ScheduledActionType)
{-# DEPRECATED satResumeCluster "Use generic-lens or generic-optics with 'resumeCluster' instead." #-}

-- | An action that runs a @PauseCluster@ API operation.
--
-- /Note:/ Consider using 'pauseCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satPauseCluster :: Lens.Lens' ScheduledActionType (Lude.Maybe PauseClusterMessage)
satPauseCluster = Lens.lens (pauseCluster :: ScheduledActionType -> Lude.Maybe PauseClusterMessage) (\s a -> s {pauseCluster = a} :: ScheduledActionType)
{-# DEPRECATED satPauseCluster "Use generic-lens or generic-optics with 'pauseCluster' instead." #-}

instance Lude.FromXML ScheduledActionType where
  parseXML x =
    ScheduledActionType'
      Lude.<$> (x Lude..@? "ResizeCluster")
      Lude.<*> (x Lude..@? "ResumeCluster")
      Lude.<*> (x Lude..@? "PauseCluster")

instance Lude.ToQuery ScheduledActionType where
  toQuery ScheduledActionType' {..} =
    Lude.mconcat
      [ "ResizeCluster" Lude.=: resizeCluster,
        "ResumeCluster" Lude.=: resumeCluster,
        "PauseCluster" Lude.=: pauseCluster
      ]
