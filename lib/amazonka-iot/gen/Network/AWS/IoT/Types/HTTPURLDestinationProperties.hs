{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPURLDestinationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPURLDestinationProperties
  ( HTTPURLDestinationProperties (..),

    -- * Smart constructor
    mkHTTPURLDestinationProperties,

    -- * Lenses
    httpudpConfirmationURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | HTTP URL destination properties.
--
-- /See:/ 'mkHTTPURLDestinationProperties' smart constructor.
newtype HTTPURLDestinationProperties = HTTPURLDestinationProperties'
  { -- | The URL used to confirm the HTTP topic rule destination URL.
    confirmationURL :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPURLDestinationProperties' with the minimum fields required to make a request.
--
-- * 'confirmationURL' - The URL used to confirm the HTTP topic rule destination URL.
mkHTTPURLDestinationProperties ::
  HTTPURLDestinationProperties
mkHTTPURLDestinationProperties =
  HTTPURLDestinationProperties' {confirmationURL = Lude.Nothing}

-- | The URL used to confirm the HTTP topic rule destination URL.
--
-- /Note:/ Consider using 'confirmationURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpudpConfirmationURL :: Lens.Lens' HTTPURLDestinationProperties (Lude.Maybe Lude.Text)
httpudpConfirmationURL = Lens.lens (confirmationURL :: HTTPURLDestinationProperties -> Lude.Maybe Lude.Text) (\s a -> s {confirmationURL = a} :: HTTPURLDestinationProperties)
{-# DEPRECATED httpudpConfirmationURL "Use generic-lens or generic-optics with 'confirmationURL' instead." #-}

instance Lude.FromJSON HTTPURLDestinationProperties where
  parseJSON =
    Lude.withObject
      "HTTPURLDestinationProperties"
      ( \x ->
          HTTPURLDestinationProperties'
            Lude.<$> (x Lude..:? "confirmationUrl")
      )
