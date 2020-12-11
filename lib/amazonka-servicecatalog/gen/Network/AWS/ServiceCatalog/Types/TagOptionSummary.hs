-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.TagOptionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.TagOptionSummary
  ( TagOptionSummary (..),

    -- * Smart constructor
    mkTagOptionSummary,

    -- * Lenses
    tosValues,
    tosKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information about a TagOption.
--
-- /See:/ 'mkTagOptionSummary' smart constructor.
data TagOptionSummary = TagOptionSummary'
  { values ::
      Lude.Maybe [Lude.Text],
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagOptionSummary' with the minimum fields required to make a request.
--
-- * 'key' - The TagOption key.
-- * 'values' - The TagOption value.
mkTagOptionSummary ::
  TagOptionSummary
mkTagOptionSummary =
  TagOptionSummary' {values = Lude.Nothing, key = Lude.Nothing}

-- | The TagOption value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tosValues :: Lens.Lens' TagOptionSummary (Lude.Maybe [Lude.Text])
tosValues = Lens.lens (values :: TagOptionSummary -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: TagOptionSummary)
{-# DEPRECATED tosValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The TagOption key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tosKey :: Lens.Lens' TagOptionSummary (Lude.Maybe Lude.Text)
tosKey = Lens.lens (key :: TagOptionSummary -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: TagOptionSummary)
{-# DEPRECATED tosKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON TagOptionSummary where
  parseJSON =
    Lude.withObject
      "TagOptionSummary"
      ( \x ->
          TagOptionSummary'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Key")
      )
