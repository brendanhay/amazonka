{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TagValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TagValues
  ( TagValues (..),

    -- * Smart constructor
    mkTagValues,

    -- * Lenses
    tvValues,
    tvKey,
    tvMatchOptions,
  )
where

import Network.AWS.CostExplorer.Types.MatchOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The values that are available for a tag.
--
-- /See:/ 'mkTagValues' smart constructor.
data TagValues = TagValues'
  { values :: Lude.Maybe [Lude.Text],
    key :: Lude.Maybe Lude.Text,
    matchOptions :: Lude.Maybe [MatchOption]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagValues' with the minimum fields required to make a request.
--
-- * 'key' - The key for the tag.
-- * 'matchOptions' - The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
-- * 'values' - The specific value of the tag.
mkTagValues ::
  TagValues
mkTagValues =
  TagValues'
    { values = Lude.Nothing,
      key = Lude.Nothing,
      matchOptions = Lude.Nothing
    }

-- | The specific value of the tag.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvValues :: Lens.Lens' TagValues (Lude.Maybe [Lude.Text])
tvValues = Lens.lens (values :: TagValues -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: TagValues)
{-# DEPRECATED tvValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The key for the tag.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvKey :: Lens.Lens' TagValues (Lude.Maybe Lude.Text)
tvKey = Lens.lens (key :: TagValues -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: TagValues)
{-# DEPRECATED tvKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
--
-- /Note:/ Consider using 'matchOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvMatchOptions :: Lens.Lens' TagValues (Lude.Maybe [MatchOption])
tvMatchOptions = Lens.lens (matchOptions :: TagValues -> Lude.Maybe [MatchOption]) (\s a -> s {matchOptions = a} :: TagValues)
{-# DEPRECATED tvMatchOptions "Use generic-lens or generic-optics with 'matchOptions' instead." #-}

instance Lude.FromJSON TagValues where
  parseJSON =
    Lude.withObject
      "TagValues"
      ( \x ->
          TagValues'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "MatchOptions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON TagValues where
  toJSON TagValues' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Values" Lude..=) Lude.<$> values,
            ("Key" Lude..=) Lude.<$> key,
            ("MatchOptions" Lude..=) Lude.<$> matchOptions
          ]
      )
