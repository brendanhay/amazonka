-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Hit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Hit
  ( Hit (..),

    -- * Smart constructor
    mkHit,

    -- * Lenses
    hExprs,
    hId,
    hHighlights,
    hFields,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a document that matches the search request.
--
-- /See:/ 'mkHit' smart constructor.
data Hit = Hit'
  { exprs ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    id :: Lude.Maybe Lude.Text,
    highlights :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    fields :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Hit' with the minimum fields required to make a request.
--
-- * 'exprs' - The expressions returned from a document that matches the search request.
-- * 'fields' - The fields returned from a document that matches the search request.
-- * 'highlights' - The highlights returned from a document that matches the search request.
-- * 'id' - The document ID of a document that matches the search request.
mkHit ::
  Hit
mkHit =
  Hit'
    { exprs = Lude.Nothing,
      id = Lude.Nothing,
      highlights = Lude.Nothing,
      fields = Lude.Nothing
    }

-- | The expressions returned from a document that matches the search request.
--
-- /Note:/ Consider using 'exprs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hExprs :: Lens.Lens' Hit (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
hExprs = Lens.lens (exprs :: Hit -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {exprs = a} :: Hit)
{-# DEPRECATED hExprs "Use generic-lens or generic-optics with 'exprs' instead." #-}

-- | The document ID of a document that matches the search request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hId :: Lens.Lens' Hit (Lude.Maybe Lude.Text)
hId = Lens.lens (id :: Hit -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Hit)
{-# DEPRECATED hId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The highlights returned from a document that matches the search request.
--
-- /Note:/ Consider using 'highlights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHighlights :: Lens.Lens' Hit (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
hHighlights = Lens.lens (highlights :: Hit -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {highlights = a} :: Hit)
{-# DEPRECATED hHighlights "Use generic-lens or generic-optics with 'highlights' instead." #-}

-- | The fields returned from a document that matches the search request.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFields :: Lens.Lens' Hit (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
hFields = Lens.lens (fields :: Hit -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {fields = a} :: Hit)
{-# DEPRECATED hFields "Use generic-lens or generic-optics with 'fields' instead." #-}

instance Lude.FromJSON Hit where
  parseJSON =
    Lude.withObject
      "Hit"
      ( \x ->
          Hit'
            Lude.<$> (x Lude..:? "exprs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "highlights" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "fields" Lude..!= Lude.mempty)
      )
