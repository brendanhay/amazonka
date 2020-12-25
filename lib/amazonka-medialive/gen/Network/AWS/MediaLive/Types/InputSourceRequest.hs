{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSourceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSourceRequest
  ( InputSourceRequest (..),

    -- * Smart constructor
    mkInputSourceRequest,

    -- * Lenses
    isrPasswordParam,
    isrUrl,
    isrUsername,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for for a PULL type input.
--
-- /See:/ 'mkInputSourceRequest' smart constructor.
data InputSourceRequest = InputSourceRequest'
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

-- | Creates a 'InputSourceRequest' value with any optional fields omitted.
mkInputSourceRequest ::
  InputSourceRequest
mkInputSourceRequest =
  InputSourceRequest'
    { passwordParam = Core.Nothing,
      url = Core.Nothing,
      username = Core.Nothing
    }

-- | The key used to extract the password from EC2 Parameter store.
--
-- /Note:/ Consider using 'passwordParam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrPasswordParam :: Lens.Lens' InputSourceRequest (Core.Maybe Core.Text)
isrPasswordParam = Lens.field @"passwordParam"
{-# DEPRECATED isrPasswordParam "Use generic-lens or generic-optics with 'passwordParam' instead." #-}

-- | This represents the customer's source URL where stream is
--
-- pulled from.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrUrl :: Lens.Lens' InputSourceRequest (Core.Maybe Core.Text)
isrUrl = Lens.field @"url"
{-# DEPRECATED isrUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The username for the input source.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrUsername :: Lens.Lens' InputSourceRequest (Core.Maybe Core.Text)
isrUsername = Lens.field @"username"
{-# DEPRECATED isrUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON InputSourceRequest where
  toJSON InputSourceRequest {..} =
    Core.object
      ( Core.catMaybes
          [ ("passwordParam" Core..=) Core.<$> passwordParam,
            ("url" Core..=) Core.<$> url,
            ("username" Core..=) Core.<$> username
          ]
      )
