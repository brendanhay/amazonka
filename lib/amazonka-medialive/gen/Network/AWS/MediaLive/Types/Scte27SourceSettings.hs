{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte27SourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte27SourceSettings
  ( Scte27SourceSettings (..)
  -- * Smart constructor
  , mkScte27SourceSettings
  -- * Lenses
  , sssPid
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Scte27 Source Settings
--
-- /See:/ 'mkScte27SourceSettings' smart constructor.
newtype Scte27SourceSettings = Scte27SourceSettings'
  { pid :: Core.Maybe Core.Natural
    -- ^ The pid field is used in conjunction with the caption selector languageCode field as follows:
--
--   - Specify PID and Language: Extracts captions from that PID; the language is "informational".
--   - Specify PID and omit Language: Extracts the specified PID.
--   - Omit PID and specify Language: Extracts the specified language, whichever PID that happens to be.
--   - Omit PID and omit Language: Valid only if source is DVB-Sub that is being passed through; all languages will be passed through.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Scte27SourceSettings' value with any optional fields omitted.
mkScte27SourceSettings
    :: Scte27SourceSettings
mkScte27SourceSettings = Scte27SourceSettings'{pid = Core.Nothing}

-- | The pid field is used in conjunction with the caption selector languageCode field as follows:
--
--   - Specify PID and Language: Extracts captions from that PID; the language is "informational".
--   - Specify PID and omit Language: Extracts the specified PID.
--   - Omit PID and specify Language: Extracts the specified language, whichever PID that happens to be.
--   - Omit PID and omit Language: Valid only if source is DVB-Sub that is being passed through; all languages will be passed through.
--
-- /Note:/ Consider using 'pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssPid :: Lens.Lens' Scte27SourceSettings (Core.Maybe Core.Natural)
sssPid = Lens.field @"pid"
{-# INLINEABLE sssPid #-}
{-# DEPRECATED pid "Use generic-lens or generic-optics with 'pid' instead"  #-}

instance Core.FromJSON Scte27SourceSettings where
        toJSON Scte27SourceSettings{..}
          = Core.object (Core.catMaybes [("pid" Core..=) Core.<$> pid])

instance Core.FromJSON Scte27SourceSettings where
        parseJSON
          = Core.withObject "Scte27SourceSettings" Core.$
              \ x -> Scte27SourceSettings' Core.<$> (x Core..:? "pid")
