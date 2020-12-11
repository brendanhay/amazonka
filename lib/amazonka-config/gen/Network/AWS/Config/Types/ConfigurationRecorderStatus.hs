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
    crsLastStopTime,
    crsLastStatusChangeTime,
    crsRecording,
    crsLastStatus,
    crsLastErrorMessage,
    crsName,
    crsLastStartTime,
  )
where

import Network.AWS.Config.Types.RecorderStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The current status of the configuration recorder.
--
-- /See:/ 'mkConfigurationRecorderStatus' smart constructor.
data ConfigurationRecorderStatus = ConfigurationRecorderStatus'
  { lastErrorCode ::
      Lude.Maybe Lude.Text,
    lastStopTime ::
      Lude.Maybe Lude.Timestamp,
    lastStatusChangeTime ::
      Lude.Maybe Lude.Timestamp,
    recording :: Lude.Maybe Lude.Bool,
    lastStatus ::
      Lude.Maybe RecorderStatus,
    lastErrorMessage ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    lastStartTime ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationRecorderStatus' with the minimum fields required to make a request.
--
-- * 'lastErrorCode' - The error code indicating that the recording failed.
-- * 'lastErrorMessage' - The message indicating that the recording failed due to an error.
-- * 'lastStartTime' - The time the recorder was last started.
-- * 'lastStatus' - The last (previous) status of the recorder.
-- * 'lastStatusChangeTime' - The time when the status was last changed.
-- * 'lastStopTime' - The time the recorder was last stopped.
-- * 'name' - The name of the configuration recorder.
-- * 'recording' - Specifies whether or not the recorder is currently recording.
mkConfigurationRecorderStatus ::
  ConfigurationRecorderStatus
mkConfigurationRecorderStatus =
  ConfigurationRecorderStatus'
    { lastErrorCode = Lude.Nothing,
      lastStopTime = Lude.Nothing,
      lastStatusChangeTime = Lude.Nothing,
      recording = Lude.Nothing,
      lastStatus = Lude.Nothing,
      lastErrorMessage = Lude.Nothing,
      name = Lude.Nothing,
      lastStartTime = Lude.Nothing
    }

-- | The error code indicating that the recording failed.
--
-- /Note:/ Consider using 'lastErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastErrorCode :: Lens.Lens' ConfigurationRecorderStatus (Lude.Maybe Lude.Text)
crsLastErrorCode = Lens.lens (lastErrorCode :: ConfigurationRecorderStatus -> Lude.Maybe Lude.Text) (\s a -> s {lastErrorCode = a} :: ConfigurationRecorderStatus)
{-# DEPRECATED crsLastErrorCode "Use generic-lens or generic-optics with 'lastErrorCode' instead." #-}

-- | The time the recorder was last stopped.
--
-- /Note:/ Consider using 'lastStopTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStopTime :: Lens.Lens' ConfigurationRecorderStatus (Lude.Maybe Lude.Timestamp)
crsLastStopTime = Lens.lens (lastStopTime :: ConfigurationRecorderStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastStopTime = a} :: ConfigurationRecorderStatus)
{-# DEPRECATED crsLastStopTime "Use generic-lens or generic-optics with 'lastStopTime' instead." #-}

-- | The time when the status was last changed.
--
-- /Note:/ Consider using 'lastStatusChangeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStatusChangeTime :: Lens.Lens' ConfigurationRecorderStatus (Lude.Maybe Lude.Timestamp)
crsLastStatusChangeTime = Lens.lens (lastStatusChangeTime :: ConfigurationRecorderStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastStatusChangeTime = a} :: ConfigurationRecorderStatus)
{-# DEPRECATED crsLastStatusChangeTime "Use generic-lens or generic-optics with 'lastStatusChangeTime' instead." #-}

-- | Specifies whether or not the recorder is currently recording.
--
-- /Note:/ Consider using 'recording' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsRecording :: Lens.Lens' ConfigurationRecorderStatus (Lude.Maybe Lude.Bool)
crsRecording = Lens.lens (recording :: ConfigurationRecorderStatus -> Lude.Maybe Lude.Bool) (\s a -> s {recording = a} :: ConfigurationRecorderStatus)
{-# DEPRECATED crsRecording "Use generic-lens or generic-optics with 'recording' instead." #-}

-- | The last (previous) status of the recorder.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStatus :: Lens.Lens' ConfigurationRecorderStatus (Lude.Maybe RecorderStatus)
crsLastStatus = Lens.lens (lastStatus :: ConfigurationRecorderStatus -> Lude.Maybe RecorderStatus) (\s a -> s {lastStatus = a} :: ConfigurationRecorderStatus)
{-# DEPRECATED crsLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

-- | The message indicating that the recording failed due to an error.
--
-- /Note:/ Consider using 'lastErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastErrorMessage :: Lens.Lens' ConfigurationRecorderStatus (Lude.Maybe Lude.Text)
crsLastErrorMessage = Lens.lens (lastErrorMessage :: ConfigurationRecorderStatus -> Lude.Maybe Lude.Text) (\s a -> s {lastErrorMessage = a} :: ConfigurationRecorderStatus)
{-# DEPRECATED crsLastErrorMessage "Use generic-lens or generic-optics with 'lastErrorMessage' instead." #-}

-- | The name of the configuration recorder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsName :: Lens.Lens' ConfigurationRecorderStatus (Lude.Maybe Lude.Text)
crsName = Lens.lens (name :: ConfigurationRecorderStatus -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ConfigurationRecorderStatus)
{-# DEPRECATED crsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time the recorder was last started.
--
-- /Note:/ Consider using 'lastStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastStartTime :: Lens.Lens' ConfigurationRecorderStatus (Lude.Maybe Lude.Timestamp)
crsLastStartTime = Lens.lens (lastStartTime :: ConfigurationRecorderStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastStartTime = a} :: ConfigurationRecorderStatus)
{-# DEPRECATED crsLastStartTime "Use generic-lens or generic-optics with 'lastStartTime' instead." #-}

instance Lude.FromJSON ConfigurationRecorderStatus where
  parseJSON =
    Lude.withObject
      "ConfigurationRecorderStatus"
      ( \x ->
          ConfigurationRecorderStatus'
            Lude.<$> (x Lude..:? "lastErrorCode")
            Lude.<*> (x Lude..:? "lastStopTime")
            Lude.<*> (x Lude..:? "lastStatusChangeTime")
            Lude.<*> (x Lude..:? "recording")
            Lude.<*> (x Lude..:? "lastStatus")
            Lude.<*> (x Lude..:? "lastErrorMessage")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "lastStartTime")
      )
