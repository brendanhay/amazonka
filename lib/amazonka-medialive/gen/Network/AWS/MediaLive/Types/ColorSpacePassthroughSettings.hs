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
import qualified Network.AWS.Prelude as Lude

-- | Passthrough applies no color space conversion to the output
--
-- /See:/ 'mkColorSpacePassthroughSettings' smart constructor.
data ColorSpacePassthroughSettings = ColorSpacePassthroughSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ColorSpacePassthroughSettings' with the minimum fields required to make a request.
mkColorSpacePassthroughSettings ::
  ColorSpacePassthroughSettings
mkColorSpacePassthroughSettings = ColorSpacePassthroughSettings'

instance Lude.FromJSON ColorSpacePassthroughSettings where
  parseJSON =
    Lude.withObject
      "ColorSpacePassthroughSettings"
      (\x -> Lude.pure ColorSpacePassthroughSettings')

instance Lude.ToJSON ColorSpacePassthroughSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
