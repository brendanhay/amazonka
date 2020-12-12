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
    isURL,
    isUsername,
    isPasswordParam,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The settings for a PULL type input.
--
-- /See:/ 'mkInputSource' smart constructor.
data InputSource = InputSource'
  { url :: Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    passwordParam :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputSource' with the minimum fields required to make a request.
--
-- * 'passwordParam' - The key used to extract the password from EC2 Parameter store.
-- * 'url' - This represents the customer's source URL where stream is
--
-- pulled from.
-- * 'username' - The username for the input source.
mkInputSource ::
  InputSource
mkInputSource =
  InputSource'
    { url = Lude.Nothing,
      username = Lude.Nothing,
      passwordParam = Lude.Nothing
    }

-- | This represents the customer's source URL where stream is
--
-- pulled from.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isURL :: Lens.Lens' InputSource (Lude.Maybe Lude.Text)
isURL = Lens.lens (url :: InputSource -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: InputSource)
{-# DEPRECATED isURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The username for the input source.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isUsername :: Lens.Lens' InputSource (Lude.Maybe Lude.Text)
isUsername = Lens.lens (username :: InputSource -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: InputSource)
{-# DEPRECATED isUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The key used to extract the password from EC2 Parameter store.
--
-- /Note:/ Consider using 'passwordParam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isPasswordParam :: Lens.Lens' InputSource (Lude.Maybe Lude.Text)
isPasswordParam = Lens.lens (passwordParam :: InputSource -> Lude.Maybe Lude.Text) (\s a -> s {passwordParam = a} :: InputSource)
{-# DEPRECATED isPasswordParam "Use generic-lens or generic-optics with 'passwordParam' instead." #-}

instance Lude.FromJSON InputSource where
  parseJSON =
    Lude.withObject
      "InputSource"
      ( \x ->
          InputSource'
            Lude.<$> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "username")
            Lude.<*> (x Lude..:? "passwordParam")
      )
