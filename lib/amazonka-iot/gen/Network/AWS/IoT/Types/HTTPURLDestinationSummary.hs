-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPURLDestinationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPURLDestinationSummary
  ( HTTPURLDestinationSummary (..),

    -- * Smart constructor
    mkHTTPURLDestinationSummary,

    -- * Lenses
    httpudsConfirmationURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an HTTP URL destination.
--
-- /See:/ 'mkHTTPURLDestinationSummary' smart constructor.
newtype HTTPURLDestinationSummary = HTTPURLDestinationSummary'
  { confirmationURL ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPURLDestinationSummary' with the minimum fields required to make a request.
--
-- * 'confirmationURL' - The URL used to confirm ownership of or access to the HTTP topic rule destination URL.
mkHTTPURLDestinationSummary ::
  HTTPURLDestinationSummary
mkHTTPURLDestinationSummary =
  HTTPURLDestinationSummary' {confirmationURL = Lude.Nothing}

-- | The URL used to confirm ownership of or access to the HTTP topic rule destination URL.
--
-- /Note:/ Consider using 'confirmationURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpudsConfirmationURL :: Lens.Lens' HTTPURLDestinationSummary (Lude.Maybe Lude.Text)
httpudsConfirmationURL = Lens.lens (confirmationURL :: HTTPURLDestinationSummary -> Lude.Maybe Lude.Text) (\s a -> s {confirmationURL = a} :: HTTPURLDestinationSummary)
{-# DEPRECATED httpudsConfirmationURL "Use generic-lens or generic-optics with 'confirmationURL' instead." #-}

instance Lude.FromJSON HTTPURLDestinationSummary where
  parseJSON =
    Lude.withObject
      "HTTPURLDestinationSummary"
      ( \x ->
          HTTPURLDestinationSummary' Lude.<$> (x Lude..:? "confirmationUrl")
      )
