-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.KeyRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.KeyRange
  ( KeyRange (..),

    -- * Smart constructor
    mkKeyRange,

    -- * Lenses
    krEndMarker,
    krBeginMarker,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a key range. For export jobs, a @S3Resource@ object can have an optional @KeyRange@ value. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
-- /See:/ 'mkKeyRange' smart constructor.
data KeyRange = KeyRange'
  { endMarker :: Lude.Maybe Lude.Text,
    beginMarker :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyRange' with the minimum fields required to make a request.
--
-- * 'beginMarker' - The key that starts an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
-- * 'endMarker' - The key that ends an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
mkKeyRange ::
  KeyRange
mkKeyRange =
  KeyRange' {endMarker = Lude.Nothing, beginMarker = Lude.Nothing}

-- | The key that ends an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
--
-- /Note:/ Consider using 'endMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
krEndMarker :: Lens.Lens' KeyRange (Lude.Maybe Lude.Text)
krEndMarker = Lens.lens (endMarker :: KeyRange -> Lude.Maybe Lude.Text) (\s a -> s {endMarker = a} :: KeyRange)
{-# DEPRECATED krEndMarker "Use generic-lens or generic-optics with 'endMarker' instead." #-}

-- | The key that starts an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
--
-- /Note:/ Consider using 'beginMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
krBeginMarker :: Lens.Lens' KeyRange (Lude.Maybe Lude.Text)
krBeginMarker = Lens.lens (beginMarker :: KeyRange -> Lude.Maybe Lude.Text) (\s a -> s {beginMarker = a} :: KeyRange)
{-# DEPRECATED krBeginMarker "Use generic-lens or generic-optics with 'beginMarker' instead." #-}

instance Lude.FromJSON KeyRange where
  parseJSON =
    Lude.withObject
      "KeyRange"
      ( \x ->
          KeyRange'
            Lude.<$> (x Lude..:? "EndMarker") Lude.<*> (x Lude..:? "BeginMarker")
      )

instance Lude.ToJSON KeyRange where
  toJSON KeyRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EndMarker" Lude..=) Lude.<$> endMarker,
            ("BeginMarker" Lude..=) Lude.<$> beginMarker
          ]
      )
