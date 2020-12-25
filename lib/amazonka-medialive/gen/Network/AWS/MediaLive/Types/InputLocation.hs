{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLocation
  ( InputLocation (..),

    -- * Smart constructor
    mkInputLocation,

    -- * Lenses
    ilUri,
    ilPasswordParam,
    ilUsername,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Input Location
--
-- /See:/ 'mkInputLocation' smart constructor.
data InputLocation = InputLocation'
  { -- | Uniform Resource Identifier - This should be a path to a file accessible to the Live system (eg. a http:// URI) depending on the output type. For example, a RTMP destination should have a uri simliar to: "rtmp://fmsserver/live".
    uri :: Core.Text,
    -- | key used to extract the password from EC2 Parameter store
    passwordParam :: Core.Maybe Core.Text,
    -- | Documentation update needed
    username :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputLocation' value with any optional fields omitted.
mkInputLocation ::
  -- | 'uri'
  Core.Text ->
  InputLocation
mkInputLocation uri =
  InputLocation'
    { uri,
      passwordParam = Core.Nothing,
      username = Core.Nothing
    }

-- | Uniform Resource Identifier - This should be a path to a file accessible to the Live system (eg. a http:// URI) depending on the output type. For example, a RTMP destination should have a uri simliar to: "rtmp://fmsserver/live".
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilUri :: Lens.Lens' InputLocation Core.Text
ilUri = Lens.field @"uri"
{-# DEPRECATED ilUri "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | key used to extract the password from EC2 Parameter store
--
-- /Note:/ Consider using 'passwordParam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilPasswordParam :: Lens.Lens' InputLocation (Core.Maybe Core.Text)
ilPasswordParam = Lens.field @"passwordParam"
{-# DEPRECATED ilPasswordParam "Use generic-lens or generic-optics with 'passwordParam' instead." #-}

-- | Documentation update needed
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilUsername :: Lens.Lens' InputLocation (Core.Maybe Core.Text)
ilUsername = Lens.field @"username"
{-# DEPRECATED ilUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON InputLocation where
  toJSON InputLocation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("uri" Core..= uri),
            ("passwordParam" Core..=) Core.<$> passwordParam,
            ("username" Core..=) Core.<$> username
          ]
      )

instance Core.FromJSON InputLocation where
  parseJSON =
    Core.withObject "InputLocation" Core.$
      \x ->
        InputLocation'
          Core.<$> (x Core..: "uri")
          Core.<*> (x Core..:? "passwordParam")
          Core.<*> (x Core..:? "username")
