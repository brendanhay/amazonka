{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSource
  ( InputSource (..),

    -- * Smart constructor
    mkInputSource,

    -- * Lenses
    isPasswordParam,
    isUrl,
    isUsername,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The settings for a PULL type input.
--
-- /See:/ 'mkInputSource' smart constructor.
data InputSource = InputSource'
  { -- | The key used to extract the password from EC2 Parameter store.
    passwordParam :: Core.Maybe Core.Text,
    -- | This represents the customer's source URL where stream is
    --
    -- pulled from.
    url :: Core.Maybe Core.Text,
    -- | The username for the input source.
    username :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputSource' value with any optional fields omitted.
mkInputSource ::
  InputSource
mkInputSource =
  InputSource'
    { passwordParam = Core.Nothing,
      url = Core.Nothing,
      username = Core.Nothing
    }

-- | The key used to extract the password from EC2 Parameter store.
--
-- /Note:/ Consider using 'passwordParam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isPasswordParam :: Lens.Lens' InputSource (Core.Maybe Core.Text)
isPasswordParam = Lens.field @"passwordParam"
{-# DEPRECATED isPasswordParam "Use generic-lens or generic-optics with 'passwordParam' instead." #-}

-- | This represents the customer's source URL where stream is
--
-- pulled from.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isUrl :: Lens.Lens' InputSource (Core.Maybe Core.Text)
isUrl = Lens.field @"url"
{-# DEPRECATED isUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The username for the input source.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isUsername :: Lens.Lens' InputSource (Core.Maybe Core.Text)
isUsername = Lens.field @"username"
{-# DEPRECATED isUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON InputSource where
  parseJSON =
    Core.withObject "InputSource" Core.$
      \x ->
        InputSource'
          Core.<$> (x Core..:? "passwordParam")
          Core.<*> (x Core..:? "url")
          Core.<*> (x Core..:? "username")
