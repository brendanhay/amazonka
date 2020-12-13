{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LifecycleEvent
  ( LifecycleEvent (..),

    -- * Smart constructor
    mkLifecycleEvent,

    -- * Lenses
    leStatus,
    leLifecycleEventName,
    leStartTime,
    leDiagnostics,
    leEndTime,
  )
where

import Network.AWS.CodeDeploy.Types.Diagnostics
import Network.AWS.CodeDeploy.Types.LifecycleEventStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a deployment lifecycle event.
--
-- /See:/ 'mkLifecycleEvent' smart constructor.
data LifecycleEvent = LifecycleEvent'
  { -- | The deployment lifecycle event status:
    --
    --
    --     * Pending: The deployment lifecycle event is pending.
    --
    --
    --     * InProgress: The deployment lifecycle event is in progress.
    --
    --
    --     * Succeeded: The deployment lifecycle event ran successfully.
    --
    --
    --     * Failed: The deployment lifecycle event has failed.
    --
    --
    --     * Skipped: The deployment lifecycle event has been skipped.
    --
    --
    --     * Unknown: The deployment lifecycle event is unknown.
    status :: Lude.Maybe LifecycleEventStatus,
    -- | The deployment lifecycle event name, such as @ApplicationStop@ , @BeforeInstall@ , @AfterInstall@ , @ApplicationStart@ , or @ValidateService@ .
    lifecycleEventName :: Lude.Maybe Lude.Text,
    -- | A timestamp that indicates when the deployment lifecycle event started.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | Diagnostic information about the deployment lifecycle event.
    diagnostics :: Lude.Maybe Diagnostics,
    -- | A timestamp that indicates when the deployment lifecycle event ended.
    endTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecycleEvent' with the minimum fields required to make a request.
--
-- * 'status' - The deployment lifecycle event status:
--
--
--     * Pending: The deployment lifecycle event is pending.
--
--
--     * InProgress: The deployment lifecycle event is in progress.
--
--
--     * Succeeded: The deployment lifecycle event ran successfully.
--
--
--     * Failed: The deployment lifecycle event has failed.
--
--
--     * Skipped: The deployment lifecycle event has been skipped.
--
--
--     * Unknown: The deployment lifecycle event is unknown.
--
--
-- * 'lifecycleEventName' - The deployment lifecycle event name, such as @ApplicationStop@ , @BeforeInstall@ , @AfterInstall@ , @ApplicationStart@ , or @ValidateService@ .
-- * 'startTime' - A timestamp that indicates when the deployment lifecycle event started.
-- * 'diagnostics' - Diagnostic information about the deployment lifecycle event.
-- * 'endTime' - A timestamp that indicates when the deployment lifecycle event ended.
mkLifecycleEvent ::
  LifecycleEvent
mkLifecycleEvent =
  LifecycleEvent'
    { status = Lude.Nothing,
      lifecycleEventName = Lude.Nothing,
      startTime = Lude.Nothing,
      diagnostics = Lude.Nothing,
      endTime = Lude.Nothing
    }

-- | The deployment lifecycle event status:
--
--
--     * Pending: The deployment lifecycle event is pending.
--
--
--     * InProgress: The deployment lifecycle event is in progress.
--
--
--     * Succeeded: The deployment lifecycle event ran successfully.
--
--
--     * Failed: The deployment lifecycle event has failed.
--
--
--     * Skipped: The deployment lifecycle event has been skipped.
--
--
--     * Unknown: The deployment lifecycle event is unknown.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStatus :: Lens.Lens' LifecycleEvent (Lude.Maybe LifecycleEventStatus)
leStatus = Lens.lens (status :: LifecycleEvent -> Lude.Maybe LifecycleEventStatus) (\s a -> s {status = a} :: LifecycleEvent)
{-# DEPRECATED leStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The deployment lifecycle event name, such as @ApplicationStop@ , @BeforeInstall@ , @AfterInstall@ , @ApplicationStart@ , or @ValidateService@ .
--
-- /Note:/ Consider using 'lifecycleEventName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leLifecycleEventName :: Lens.Lens' LifecycleEvent (Lude.Maybe Lude.Text)
leLifecycleEventName = Lens.lens (lifecycleEventName :: LifecycleEvent -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleEventName = a} :: LifecycleEvent)
{-# DEPRECATED leLifecycleEventName "Use generic-lens or generic-optics with 'lifecycleEventName' instead." #-}

-- | A timestamp that indicates when the deployment lifecycle event started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStartTime :: Lens.Lens' LifecycleEvent (Lude.Maybe Lude.Timestamp)
leStartTime = Lens.lens (startTime :: LifecycleEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: LifecycleEvent)
{-# DEPRECATED leStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Diagnostic information about the deployment lifecycle event.
--
-- /Note:/ Consider using 'diagnostics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leDiagnostics :: Lens.Lens' LifecycleEvent (Lude.Maybe Diagnostics)
leDiagnostics = Lens.lens (diagnostics :: LifecycleEvent -> Lude.Maybe Diagnostics) (\s a -> s {diagnostics = a} :: LifecycleEvent)
{-# DEPRECATED leDiagnostics "Use generic-lens or generic-optics with 'diagnostics' instead." #-}

-- | A timestamp that indicates when the deployment lifecycle event ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leEndTime :: Lens.Lens' LifecycleEvent (Lude.Maybe Lude.Timestamp)
leEndTime = Lens.lens (endTime :: LifecycleEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: LifecycleEvent)
{-# DEPRECATED leEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.FromJSON LifecycleEvent where
  parseJSON =
    Lude.withObject
      "LifecycleEvent"
      ( \x ->
          LifecycleEvent'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "lifecycleEventName")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "diagnostics")
            Lude.<*> (x Lude..:? "endTime")
      )
