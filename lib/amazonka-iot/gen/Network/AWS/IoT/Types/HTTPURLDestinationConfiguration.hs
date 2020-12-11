-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPURLDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPURLDestinationConfiguration
  ( HTTPURLDestinationConfiguration (..),

    -- * Smart constructor
    mkHTTPURLDestinationConfiguration,

    -- * Lenses
    httpudcConfirmationURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | HTTP URL destination configuration used by the topic rule's HTTP action.
--
-- /See:/ 'mkHTTPURLDestinationConfiguration' smart constructor.
newtype HTTPURLDestinationConfiguration = HTTPURLDestinationConfiguration'
  { confirmationURL ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPURLDestinationConfiguration' with the minimum fields required to make a request.
--
-- * 'confirmationURL' - The URL AWS IoT uses to confirm ownership of or access to the topic rule destination URL.
mkHTTPURLDestinationConfiguration ::
  -- | 'confirmationURL'
  Lude.Text ->
  HTTPURLDestinationConfiguration
mkHTTPURLDestinationConfiguration pConfirmationURL_ =
  HTTPURLDestinationConfiguration'
    { confirmationURL =
        pConfirmationURL_
    }

-- | The URL AWS IoT uses to confirm ownership of or access to the topic rule destination URL.
--
-- /Note:/ Consider using 'confirmationURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpudcConfirmationURL :: Lens.Lens' HTTPURLDestinationConfiguration Lude.Text
httpudcConfirmationURL = Lens.lens (confirmationURL :: HTTPURLDestinationConfiguration -> Lude.Text) (\s a -> s {confirmationURL = a} :: HTTPURLDestinationConfiguration)
{-# DEPRECATED httpudcConfirmationURL "Use generic-lens or generic-optics with 'confirmationURL' instead." #-}

instance Lude.ToJSON HTTPURLDestinationConfiguration where
  toJSON HTTPURLDestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("confirmationUrl" Lude..= confirmationURL)]
      )
