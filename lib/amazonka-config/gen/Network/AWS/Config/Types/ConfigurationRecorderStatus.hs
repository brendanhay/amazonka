{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationRecorderStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationRecorderStatus
  ( ConfigurationRecorderStatus (..),

    -- * Smart constructor
    mkConfigurationRecorderStatus,

    -- * Lenses
    crsLastErrorCode,
    crsLastErrorMessage,
    crsLastStartTime,
    crsLastStatus,
    crsLastStatusChangeTime,
    crsLastStopTime,
    crsName,
    crsRecording,
  )
where

import qualified Network.AWS.Config.Types.RecorderStatus as Types
import qualified Network.AWS.Config.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The current status of the configuration recorder.
--
-- /See:/ 'mkConfigurationRecorderStatus' smart constructor.
data ConfigurationRecorderStatus = ConfigurationRecorderStatus'
  { -- | The error code indicating that the recording failed.
    lastErrorCode :: Core.Maybe Types.String,
    -- | The message indicating that the recording failed due to an error.
    lastErrorMessage :: Core.Maybe Types.String,
    -- | The time the recorder was last started.
    lastStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The last (previous) status of the recorder.
    lastStatus :: Core.Maybe Types.RecorderStatus,
    -- | The time when the status was last changed.
    lastStatusChangeTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time the recorder was last stopped.
    lastStopTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the configuration recorder.
    name :: Core.Maybe Types.String,
    -- | Specifies whether or not the recorder is currently recording.
    recording :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ConfigurationRecorderStatus' value with any optional fields omitted.
mkConfigurationRecorderStatus ::
  ConfigurationRecorderStatus
mkConfigurationRecorderStatus =
  ConfigurationRecorderStatus'
    { lastErrorCode = Core.Nothing,
      lastErrorMessage = Core.Nothing,
      lastStartTime = Core.Nothing,
      lastStatus = Core.Nothing,
      lastStatusChangeTime = Core.Nothing,
      lastStopTime = Core.Nothing,
      name = Core.Nothing,
      recording = Core.Nothing
    }

-- | The error code indicating that the recording failed.
--
-- /Note:/ Consider using 'lastErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastErrorCode :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Types.String)
crsLastErrorCode = Lens.field @"lastErrorCode"
{-# DEPRECATED crsLastErrorCode "Use generic-lens or generic-optics with 'lastErrorCode' instead." #-}

-- | The message indicating that the recording failed due to an error.
--
-- /Note:/ Consider using 'lastErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastErrorMessage :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Types.String)
crsLastErrorMessage = Lens.field @"lastErrorMessage"
{-# DEPRECATED crsLastErrorMessage "Use generic-lens or generic-optics with 'lastErrorMessage' instead." #-}

-- | The time the recorder was last started.
--
-- /Note:/ Consider using 'lastStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStartTime :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.NominalDiffTime)
crsLastStartTime = Lens.field @"lastStartTime"
{-# DEPRECATED crsLastStartTime "Use generic-lens or generic-optics with 'lastStartTime' instead." #-}

-- | The last (previous) status of the recorder.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStatus :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Types.RecorderStatus)
crsLastStatus = Lens.field @"lastStatus"
{-# DEPRECATED crsLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

-- | The time when the status was last changed.
--
-- /Note:/ Consider using 'lastStatusChangeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStatusChangeTime :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.NominalDiffTime)
crsLastStatusChangeTime = Lens.field @"lastStatusChangeTime"
{-# DEPRECATED crsLastStatusChangeTime "Use generic-lens or generic-optics with 'lastStatusChangeTime' instead." #-}

-- | The time the recorder was last stopped.
--
-- /Note:/ Consider using 'lastStopTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStopTime :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.NominalDiffTime)
crsLastStopTime = Lens.field @"lastStopTime"
{-# DEPRECATED crsLastStopTime "Use generic-lens or generic-optics with 'lastStopTime' instead." #-}

-- | The name of the configuration recorder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsName :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Types.String)
crsName = Lens.field @"name"
{-# DEPRECATED crsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies whether or not the recorder is currently recording.
--
-- /Note:/ Consider using 'recording' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsRecording :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.Bool)
crsRecording = Lens.field @"recording"
{-# DEPRECATED crsRecording "Use generic-lens or generic-optics with 'recording' instead." #-}

instance Core.FromJSON ConfigurationRecorderStatus where
  parseJSON =
    Core.withObject "ConfigurationRecorderStatus" Core.$
      \x ->
        ConfigurationRecorderStatus'
          Core.<$> (x Core..:? "lastErrorCode")
          Core.<*> (x Core..:? "lastErrorMessage")
          Core.<*> (x Core..:? "lastStartTime")
          Core.<*> (x Core..:? "lastStatus")
          Core.<*> (x Core..:? "lastStatusChangeTime")
          Core.<*> (x Core..:? "lastStopTime")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "recording")
