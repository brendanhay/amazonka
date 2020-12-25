{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
  ( ColorSpacePassthroughSettings (..),

    -- * Smart constructor
    mkColorSpacePassthroughSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Passthrough applies no color space conversion to the output
--
-- /See:/ 'mkColorSpacePassthroughSettings' smart constructor.
data ColorSpacePassthroughSettings = ColorSpacePassthroughSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ColorSpacePassthroughSettings' value with any optional fields omitted.
mkColorSpacePassthroughSettings ::
  ColorSpacePassthroughSettings
mkColorSpacePassthroughSettings = ColorSpacePassthroughSettings'

instance Core.FromJSON ColorSpacePassthroughSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON ColorSpacePassthroughSettings where
  parseJSON =
    Core.withObject "ColorSpacePassthroughSettings" Core.$
      \x -> Core.pure ColorSpacePassthroughSettings'
