{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.OutputDestinationSettings
  ( OutputDestinationSettings (..)
  -- * Smart constructor
  , mkOutputDestinationSettings
  -- * Lenses
  , odsPasswordParam
  , odsStreamName
  , odsUrl
  , odsUsername
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for OutputDestinationSettings
--
-- /See:/ 'mkOutputDestinationSettings' smart constructor.
data OutputDestinationSettings = OutputDestinationSettings'
  { passwordParam :: Core.Maybe Core.Text
    -- ^ key used to extract the password from EC2 Parameter store
  , streamName :: Core.Maybe Core.Text
    -- ^ Stream name for RTMP destinations (URLs of type rtmp://)
  , url :: Core.Maybe Core.Text
    -- ^ A URL specifying a destination
  , username :: Core.Maybe Core.Text
    -- ^ username for destination
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputDestinationSettings' value with any optional fields omitted.
mkOutputDestinationSettings
    :: OutputDestinationSettings
mkOutputDestinationSettings
  = OutputDestinationSettings'{passwordParam = Core.Nothing,
                               streamName = Core.Nothing, url = Core.Nothing,
                               username = Core.Nothing}

-- | key used to extract the password from EC2 Parameter store
--
-- /Note:/ Consider using 'passwordParam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odsPasswordParam :: Lens.Lens' OutputDestinationSettings (Core.Maybe Core.Text)
odsPasswordParam = Lens.field @"passwordParam"
{-# INLINEABLE odsPasswordParam #-}
{-# DEPRECATED passwordParam "Use generic-lens or generic-optics with 'passwordParam' instead"  #-}

-- | Stream name for RTMP destinations (URLs of type rtmp://)
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odsStreamName :: Lens.Lens' OutputDestinationSettings (Core.Maybe Core.Text)
odsStreamName = Lens.field @"streamName"
{-# INLINEABLE odsStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | A URL specifying a destination
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odsUrl :: Lens.Lens' OutputDestinationSettings (Core.Maybe Core.Text)
odsUrl = Lens.field @"url"
{-# INLINEABLE odsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | username for destination
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odsUsername :: Lens.Lens' OutputDestinationSettings (Core.Maybe Core.Text)
odsUsername = Lens.field @"username"
{-# INLINEABLE odsUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON OutputDestinationSettings where
        toJSON OutputDestinationSettings{..}
          = Core.object
              (Core.catMaybes
                 [("passwordParam" Core..=) Core.<$> passwordParam,
                  ("streamName" Core..=) Core.<$> streamName,
                  ("url" Core..=) Core.<$> url,
                  ("username" Core..=) Core.<$> username])

instance Core.FromJSON OutputDestinationSettings where
        parseJSON
          = Core.withObject "OutputDestinationSettings" Core.$
              \ x ->
                OutputDestinationSettings' Core.<$>
                  (x Core..:? "passwordParam") Core.<*> x Core..:? "streamName"
                    Core.<*> x Core..:? "url"
                    Core.<*> x Core..:? "username"
