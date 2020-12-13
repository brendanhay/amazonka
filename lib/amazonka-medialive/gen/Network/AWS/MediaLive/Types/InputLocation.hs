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
    ilUsername,
    ilURI,
    ilPasswordParam,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Input Location
--
-- /See:/ 'mkInputLocation' smart constructor.
data InputLocation = InputLocation'
  { -- | Documentation update needed
    username :: Lude.Maybe Lude.Text,
    -- | Uniform Resource Identifier - This should be a path to a file accessible to the Live system (eg. a http:// URI) depending on the output type. For example, a RTMP destination should have a uri simliar to: "rtmp://fmsserver/live".
    uri :: Lude.Text,
    -- | key used to extract the password from EC2 Parameter store
    passwordParam :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputLocation' with the minimum fields required to make a request.
--
-- * 'username' - Documentation update needed
-- * 'uri' - Uniform Resource Identifier - This should be a path to a file accessible to the Live system (eg. a http:// URI) depending on the output type. For example, a RTMP destination should have a uri simliar to: "rtmp://fmsserver/live".
-- * 'passwordParam' - key used to extract the password from EC2 Parameter store
mkInputLocation ::
  -- | 'uri'
  Lude.Text ->
  InputLocation
mkInputLocation pURI_ =
  InputLocation'
    { username = Lude.Nothing,
      uri = pURI_,
      passwordParam = Lude.Nothing
    }

-- | Documentation update needed
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilUsername :: Lens.Lens' InputLocation (Lude.Maybe Lude.Text)
ilUsername = Lens.lens (username :: InputLocation -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: InputLocation)
{-# DEPRECATED ilUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Uniform Resource Identifier - This should be a path to a file accessible to the Live system (eg. a http:// URI) depending on the output type. For example, a RTMP destination should have a uri simliar to: "rtmp://fmsserver/live".
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilURI :: Lens.Lens' InputLocation Lude.Text
ilURI = Lens.lens (uri :: InputLocation -> Lude.Text) (\s a -> s {uri = a} :: InputLocation)
{-# DEPRECATED ilURI "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | key used to extract the password from EC2 Parameter store
--
-- /Note:/ Consider using 'passwordParam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilPasswordParam :: Lens.Lens' InputLocation (Lude.Maybe Lude.Text)
ilPasswordParam = Lens.lens (passwordParam :: InputLocation -> Lude.Maybe Lude.Text) (\s a -> s {passwordParam = a} :: InputLocation)
{-# DEPRECATED ilPasswordParam "Use generic-lens or generic-optics with 'passwordParam' instead." #-}

instance Lude.FromJSON InputLocation where
  parseJSON =
    Lude.withObject
      "InputLocation"
      ( \x ->
          InputLocation'
            Lude.<$> (x Lude..:? "username")
            Lude.<*> (x Lude..: "uri")
            Lude.<*> (x Lude..:? "passwordParam")
      )

instance Lude.ToJSON InputLocation where
  toJSON InputLocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("username" Lude..=) Lude.<$> username,
            Lude.Just ("uri" Lude..= uri),
            ("passwordParam" Lude..=) Lude.<$> passwordParam
          ]
      )
