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
    isrURL,
    isrUsername,
    isrPasswordParam,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for for a PULL type input.
--
-- /See:/ 'mkInputSourceRequest' smart constructor.
data InputSourceRequest = InputSourceRequest'
  { -- | This represents the customer's source URL where stream is
    --
    -- pulled from.
    url :: Lude.Maybe Lude.Text,
    -- | The username for the input source.
    username :: Lude.Maybe Lude.Text,
    -- | The key used to extract the password from EC2 Parameter store.
    passwordParam :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputSourceRequest' with the minimum fields required to make a request.
--
-- * 'url' - This represents the customer's source URL where stream is
--
-- pulled from.
-- * 'username' - The username for the input source.
-- * 'passwordParam' - The key used to extract the password from EC2 Parameter store.
mkInputSourceRequest ::
  InputSourceRequest
mkInputSourceRequest =
  InputSourceRequest'
    { url = Lude.Nothing,
      username = Lude.Nothing,
      passwordParam = Lude.Nothing
    }

-- | This represents the customer's source URL where stream is
--
-- pulled from.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrURL :: Lens.Lens' InputSourceRequest (Lude.Maybe Lude.Text)
isrURL = Lens.lens (url :: InputSourceRequest -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: InputSourceRequest)
{-# DEPRECATED isrURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The username for the input source.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrUsername :: Lens.Lens' InputSourceRequest (Lude.Maybe Lude.Text)
isrUsername = Lens.lens (username :: InputSourceRequest -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: InputSourceRequest)
{-# DEPRECATED isrUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The key used to extract the password from EC2 Parameter store.
--
-- /Note:/ Consider using 'passwordParam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrPasswordParam :: Lens.Lens' InputSourceRequest (Lude.Maybe Lude.Text)
isrPasswordParam = Lens.lens (passwordParam :: InputSourceRequest -> Lude.Maybe Lude.Text) (\s a -> s {passwordParam = a} :: InputSourceRequest)
{-# DEPRECATED isrPasswordParam "Use generic-lens or generic-optics with 'passwordParam' instead." #-}

instance Lude.ToJSON InputSourceRequest where
  toJSON InputSourceRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("url" Lude..=) Lude.<$> url,
            ("username" Lude..=) Lude.<$> username,
            ("passwordParam" Lude..=) Lude.<$> passwordParam
          ]
      )
