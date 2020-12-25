{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Ssml
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Ssml
  ( Ssml (..),

    -- * Smart constructor
    mkSsml,

    -- * Lenses
    sLocale,
    sValue,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Locale as Types
import qualified Network.AWS.AlexaBusiness.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The SSML message. For more information, see <https://developer.amazon.com/docs/custom-skills/speech-synthesis-markup-language-ssml-reference.html SSML Reference> .
--
-- /See:/ 'mkSsml' smart constructor.
data Ssml = Ssml'
  { -- | The locale of the SSML message. Currently, en-US is supported.
    locale :: Types.Locale,
    -- | The value of the SSML message in the correct SSML format. The audio tag is not supported.
    value :: Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ssml' value with any optional fields omitted.
mkSsml ::
  -- | 'locale'
  Types.Locale ->
  -- | 'value'
  Types.Value ->
  Ssml
mkSsml locale value = Ssml' {locale, value}

-- | The locale of the SSML message. Currently, en-US is supported.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLocale :: Lens.Lens' Ssml Types.Locale
sLocale = Lens.field @"locale"
{-# DEPRECATED sLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The value of the SSML message in the correct SSML format. The audio tag is not supported.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValue :: Lens.Lens' Ssml Types.Value
sValue = Lens.field @"value"
{-# DEPRECATED sValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Ssml where
  toJSON Ssml {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Locale" Core..= locale),
            Core.Just ("Value" Core..= value)
          ]
      )
