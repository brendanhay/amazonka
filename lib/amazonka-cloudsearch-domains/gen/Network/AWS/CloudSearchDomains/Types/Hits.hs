-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Hits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Hits
  ( Hits (..),

    -- * Smart constructor
    mkHits,

    -- * Lenses
    hCursor,
    hHit,
    hStart,
    hFound,
  )
where

import Network.AWS.CloudSearchDomains.Types.Hit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The collection of documents that match the search request.
--
-- /See:/ 'mkHits' smart constructor.
data Hits = Hits'
  { cursor :: Lude.Maybe Lude.Text,
    hit :: Lude.Maybe [Hit],
    start :: Lude.Maybe Lude.Integer,
    found :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Hits' with the minimum fields required to make a request.
--
-- * 'cursor' - A cursor that can be used to retrieve the next set of matching documents when you want to page through a large result set.
-- * 'found' - The total number of documents that match the search request.
-- * 'hit' - A document that matches the search request.
-- * 'start' - The index of the first matching document.
mkHits ::
  Hits
mkHits =
  Hits'
    { cursor = Lude.Nothing,
      hit = Lude.Nothing,
      start = Lude.Nothing,
      found = Lude.Nothing
    }

-- | A cursor that can be used to retrieve the next set of matching documents when you want to page through a large result set.
--
-- /Note:/ Consider using 'cursor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hCursor :: Lens.Lens' Hits (Lude.Maybe Lude.Text)
hCursor = Lens.lens (cursor :: Hits -> Lude.Maybe Lude.Text) (\s a -> s {cursor = a} :: Hits)
{-# DEPRECATED hCursor "Use generic-lens or generic-optics with 'cursor' instead." #-}

-- | A document that matches the search request.
--
-- /Note:/ Consider using 'hit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHit :: Lens.Lens' Hits (Lude.Maybe [Hit])
hHit = Lens.lens (hit :: Hits -> Lude.Maybe [Hit]) (\s a -> s {hit = a} :: Hits)
{-# DEPRECATED hHit "Use generic-lens or generic-optics with 'hit' instead." #-}

-- | The index of the first matching document.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hStart :: Lens.Lens' Hits (Lude.Maybe Lude.Integer)
hStart = Lens.lens (start :: Hits -> Lude.Maybe Lude.Integer) (\s a -> s {start = a} :: Hits)
{-# DEPRECATED hStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | The total number of documents that match the search request.
--
-- /Note:/ Consider using 'found' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFound :: Lens.Lens' Hits (Lude.Maybe Lude.Integer)
hFound = Lens.lens (found :: Hits -> Lude.Maybe Lude.Integer) (\s a -> s {found = a} :: Hits)
{-# DEPRECATED hFound "Use generic-lens or generic-optics with 'found' instead." #-}

instance Lude.FromJSON Hits where
  parseJSON =
    Lude.withObject
      "Hits"
      ( \x ->
          Hits'
            Lude.<$> (x Lude..:? "cursor")
            Lude.<*> (x Lude..:? "hit" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "start")
            Lude.<*> (x Lude..:? "found")
      )
