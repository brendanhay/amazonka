-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte27SourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte27SourceSettings
  ( Scte27SourceSettings (..),

    -- * Smart constructor
    mkScte27SourceSettings,

    -- * Lenses
    sssPid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Scte27 Source Settings
--
-- /See:/ 'mkScte27SourceSettings' smart constructor.
newtype Scte27SourceSettings = Scte27SourceSettings'
  { pid ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte27SourceSettings' with the minimum fields required to make a request.
--
-- * 'pid' - The pid field is used in conjunction with the caption selector languageCode field as follows:
--
--   - Specify PID and Language: Extracts captions from that PID; the language is "informational".
--   - Specify PID and omit Language: Extracts the specified PID.
--   - Omit PID and specify Language: Extracts the specified language, whichever PID that happens to be.
--   - Omit PID and omit Language: Valid only if source is DVB-Sub that is being passed through; all languages will be passed through.
mkScte27SourceSettings ::
  Scte27SourceSettings
mkScte27SourceSettings = Scte27SourceSettings' {pid = Lude.Nothing}

-- | The pid field is used in conjunction with the caption selector languageCode field as follows:
--
--   - Specify PID and Language: Extracts captions from that PID; the language is "informational".
--   - Specify PID and omit Language: Extracts the specified PID.
--   - Omit PID and specify Language: Extracts the specified language, whichever PID that happens to be.
--   - Omit PID and omit Language: Valid only if source is DVB-Sub that is being passed through; all languages will be passed through.
--
-- /Note:/ Consider using 'pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssPid :: Lens.Lens' Scte27SourceSettings (Lude.Maybe Lude.Natural)
sssPid = Lens.lens (pid :: Scte27SourceSettings -> Lude.Maybe Lude.Natural) (\s a -> s {pid = a} :: Scte27SourceSettings)
{-# DEPRECATED sssPid "Use generic-lens or generic-optics with 'pid' instead." #-}

instance Lude.FromJSON Scte27SourceSettings where
  parseJSON =
    Lude.withObject
      "Scte27SourceSettings"
      (\x -> Scte27SourceSettings' Lude.<$> (x Lude..:? "pid"))

instance Lude.ToJSON Scte27SourceSettings where
  toJSON Scte27SourceSettings' {..} =
    Lude.object (Lude.catMaybes [("pid" Lude..=) Lude.<$> pid])
