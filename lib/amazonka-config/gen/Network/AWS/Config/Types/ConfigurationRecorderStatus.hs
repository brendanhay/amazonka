{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationRecorderStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConfigurationRecorderStatus
  ( ConfigurationRecorderStatus (..)
  -- * Smart constructor
  , mkConfigurationRecorderStatus
  -- * Lenses
  , crsLastErrorCode
  , crsLastErrorMessage
  , crsLastStartTime
  , crsLastStatus
  , crsLastStatusChangeTime
  , crsLastStopTime
  , crsName
  , crsRecording
  ) where

import qualified Network.AWS.Config.Types.RecorderStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The current status of the configuration recorder.
--
-- /See:/ 'mkConfigurationRecorderStatus' smart constructor.
data ConfigurationRecorderStatus = ConfigurationRecorderStatus'
  { lastErrorCode :: Core.Maybe Core.Text
    -- ^ The error code indicating that the recording failed.
  , lastErrorMessage :: Core.Maybe Core.Text
    -- ^ The message indicating that the recording failed due to an error.
  , lastStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the recorder was last started.
  , lastStatus :: Core.Maybe Types.RecorderStatus
    -- ^ The last (previous) status of the recorder.
  , lastStatusChangeTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the status was last changed.
  , lastStopTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the recorder was last stopped.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the configuration recorder.
  , recording :: Core.Maybe Core.Bool
    -- ^ Specifies whether or not the recorder is currently recording.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ConfigurationRecorderStatus' value with any optional fields omitted.
mkConfigurationRecorderStatus
    :: ConfigurationRecorderStatus
mkConfigurationRecorderStatus
  = ConfigurationRecorderStatus'{lastErrorCode = Core.Nothing,
                                 lastErrorMessage = Core.Nothing, lastStartTime = Core.Nothing,
                                 lastStatus = Core.Nothing, lastStatusChangeTime = Core.Nothing,
                                 lastStopTime = Core.Nothing, name = Core.Nothing,
                                 recording = Core.Nothing}

-- | The error code indicating that the recording failed.
--
-- /Note:/ Consider using 'lastErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastErrorCode :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.Text)
crsLastErrorCode = Lens.field @"lastErrorCode"
{-# INLINEABLE crsLastErrorCode #-}
{-# DEPRECATED lastErrorCode "Use generic-lens or generic-optics with 'lastErrorCode' instead"  #-}

-- | The message indicating that the recording failed due to an error.
--
-- /Note:/ Consider using 'lastErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastErrorMessage :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.Text)
crsLastErrorMessage = Lens.field @"lastErrorMessage"
{-# INLINEABLE crsLastErrorMessage #-}
{-# DEPRECATED lastErrorMessage "Use generic-lens or generic-optics with 'lastErrorMessage' instead"  #-}

-- | The time the recorder was last started.
--
-- /Note:/ Consider using 'lastStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStartTime :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.NominalDiffTime)
crsLastStartTime = Lens.field @"lastStartTime"
{-# INLINEABLE crsLastStartTime #-}
{-# DEPRECATED lastStartTime "Use generic-lens or generic-optics with 'lastStartTime' instead"  #-}

-- | The last (previous) status of the recorder.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStatus :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Types.RecorderStatus)
crsLastStatus = Lens.field @"lastStatus"
{-# INLINEABLE crsLastStatus #-}
{-# DEPRECATED lastStatus "Use generic-lens or generic-optics with 'lastStatus' instead"  #-}

-- | The time when the status was last changed.
--
-- /Note:/ Consider using 'lastStatusChangeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStatusChangeTime :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.NominalDiffTime)
crsLastStatusChangeTime = Lens.field @"lastStatusChangeTime"
{-# INLINEABLE crsLastStatusChangeTime #-}
{-# DEPRECATED lastStatusChangeTime "Use generic-lens or generic-optics with 'lastStatusChangeTime' instead"  #-}

-- | The time the recorder was last stopped.
--
-- /Note:/ Consider using 'lastStopTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStopTime :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.NominalDiffTime)
crsLastStopTime = Lens.field @"lastStopTime"
{-# INLINEABLE crsLastStopTime #-}
{-# DEPRECATED lastStopTime "Use generic-lens or generic-optics with 'lastStopTime' instead"  #-}

-- | The name of the configuration recorder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsName :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.Text)
crsName = Lens.field @"name"
{-# INLINEABLE crsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies whether or not the recorder is currently recording.
--
-- /Note:/ Consider using 'recording' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsRecording :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.Bool)
crsRecording = Lens.field @"recording"
{-# INLINEABLE crsRecording #-}
{-# DEPRECATED recording "Use generic-lens or generic-optics with 'recording' instead"  #-}

instance Core.FromJSON ConfigurationRecorderStatus where
        parseJSON
          = Core.withObject "ConfigurationRecorderStatus" Core.$
              \ x ->
                ConfigurationRecorderStatus' Core.<$>
                  (x Core..:? "lastErrorCode") Core.<*> x Core..:? "lastErrorMessage"
                    Core.<*> x Core..:? "lastStartTime"
                    Core.<*> x Core..:? "lastStatus"
                    Core.<*> x Core..:? "lastStatusChangeTime"
                    Core.<*> x Core..:? "lastStopTime"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "recording"
