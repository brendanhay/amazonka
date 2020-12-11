-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.HTTPHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.HTTPHeader
  ( HTTPHeader (..),

    -- * Smart constructor
    mkHTTPHeader,

    -- * Lenses
    httphValue,
    httphName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The response from a 'GetSampledRequests' request includes an @HTTPHeader@ complex type that appears as @Headers@ in the response syntax. @HTTPHeader@ contains the names and values of all of the headers that appear in one of the web requests that were returned by @GetSampledRequests@ .
--
-- /See:/ 'mkHTTPHeader' smart constructor.
data HTTPHeader = HTTPHeader'
  { value :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPHeader' with the minimum fields required to make a request.
--
-- * 'name' - The name of one of the headers in the sampled web request.
-- * 'value' - The value of one of the headers in the sampled web request.
mkHTTPHeader ::
  HTTPHeader
mkHTTPHeader =
  HTTPHeader' {value = Lude.Nothing, name = Lude.Nothing}

-- | The value of one of the headers in the sampled web request.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httphValue :: Lens.Lens' HTTPHeader (Lude.Maybe Lude.Text)
httphValue = Lens.lens (value :: HTTPHeader -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: HTTPHeader)
{-# DEPRECATED httphValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of one of the headers in the sampled web request.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httphName :: Lens.Lens' HTTPHeader (Lude.Maybe Lude.Text)
httphName = Lens.lens (name :: HTTPHeader -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: HTTPHeader)
{-# DEPRECATED httphName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON HTTPHeader where
  parseJSON =
    Lude.withObject
      "HTTPHeader"
      ( \x ->
          HTTPHeader'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Name")
      )
