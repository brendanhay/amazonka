{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.WebvttDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WebvttDestinationSettings
  ( WebvttDestinationSettings (..),

    -- * Smart constructor
    mkWebvttDestinationSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Webvtt Destination Settings
--
-- /See:/ 'mkWebvttDestinationSettings' smart constructor.
data WebvttDestinationSettings = WebvttDestinationSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WebvttDestinationSettings' value with any optional fields omitted.
mkWebvttDestinationSettings ::
  WebvttDestinationSettings
mkWebvttDestinationSettings = WebvttDestinationSettings'

instance Core.FromJSON WebvttDestinationSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON WebvttDestinationSettings where
  parseJSON =
    Core.withObject "WebvttDestinationSettings" Core.$
      \x -> Core.pure WebvttDestinationSettings'
