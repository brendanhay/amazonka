-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.AdvancedFieldSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.AdvancedFieldSelector
  ( AdvancedFieldSelector (..),

    -- * Smart constructor
    mkAdvancedFieldSelector,

    -- * Lenses
    afsEndsWith,
    afsNotStartsWith,
    afsEquals,
    afsNotEquals,
    afsNotEndsWith,
    afsStartsWith,
    afsField,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkAdvancedFieldSelector' smart constructor.
data AdvancedFieldSelector = AdvancedFieldSelector'
  { endsWith ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    notStartsWith ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    equals :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    notEquals ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    notEndsWith ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    startsWith ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    field :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdvancedFieldSelector' with the minimum fields required to make a request.
--
-- * 'endsWith' - Undocumented field.
-- * 'equals' - Undocumented field.
-- * 'field' - Undocumented field.
-- * 'notEndsWith' - Undocumented field.
-- * 'notEquals' - Undocumented field.
-- * 'notStartsWith' - Undocumented field.
-- * 'startsWith' - Undocumented field.
mkAdvancedFieldSelector ::
  -- | 'field'
  Lude.Text ->
  AdvancedFieldSelector
mkAdvancedFieldSelector pField_ =
  AdvancedFieldSelector'
    { endsWith = Lude.Nothing,
      notStartsWith = Lude.Nothing,
      equals = Lude.Nothing,
      notEquals = Lude.Nothing,
      notEndsWith = Lude.Nothing,
      startsWith = Lude.Nothing,
      field = pField_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'endsWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsEndsWith :: Lens.Lens' AdvancedFieldSelector (Lude.Maybe (Lude.NonEmpty Lude.Text))
afsEndsWith = Lens.lens (endsWith :: AdvancedFieldSelector -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {endsWith = a} :: AdvancedFieldSelector)
{-# DEPRECATED afsEndsWith "Use generic-lens or generic-optics with 'endsWith' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notStartsWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsNotStartsWith :: Lens.Lens' AdvancedFieldSelector (Lude.Maybe (Lude.NonEmpty Lude.Text))
afsNotStartsWith = Lens.lens (notStartsWith :: AdvancedFieldSelector -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {notStartsWith = a} :: AdvancedFieldSelector)
{-# DEPRECATED afsNotStartsWith "Use generic-lens or generic-optics with 'notStartsWith' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'equals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsEquals :: Lens.Lens' AdvancedFieldSelector (Lude.Maybe (Lude.NonEmpty Lude.Text))
afsEquals = Lens.lens (equals :: AdvancedFieldSelector -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {equals = a} :: AdvancedFieldSelector)
{-# DEPRECATED afsEquals "Use generic-lens or generic-optics with 'equals' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsNotEquals :: Lens.Lens' AdvancedFieldSelector (Lude.Maybe (Lude.NonEmpty Lude.Text))
afsNotEquals = Lens.lens (notEquals :: AdvancedFieldSelector -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {notEquals = a} :: AdvancedFieldSelector)
{-# DEPRECATED afsNotEquals "Use generic-lens or generic-optics with 'notEquals' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notEndsWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsNotEndsWith :: Lens.Lens' AdvancedFieldSelector (Lude.Maybe (Lude.NonEmpty Lude.Text))
afsNotEndsWith = Lens.lens (notEndsWith :: AdvancedFieldSelector -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {notEndsWith = a} :: AdvancedFieldSelector)
{-# DEPRECATED afsNotEndsWith "Use generic-lens or generic-optics with 'notEndsWith' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startsWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsStartsWith :: Lens.Lens' AdvancedFieldSelector (Lude.Maybe (Lude.NonEmpty Lude.Text))
afsStartsWith = Lens.lens (startsWith :: AdvancedFieldSelector -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {startsWith = a} :: AdvancedFieldSelector)
{-# DEPRECATED afsStartsWith "Use generic-lens or generic-optics with 'startsWith' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'field' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsField :: Lens.Lens' AdvancedFieldSelector Lude.Text
afsField = Lens.lens (field :: AdvancedFieldSelector -> Lude.Text) (\s a -> s {field = a} :: AdvancedFieldSelector)
{-# DEPRECATED afsField "Use generic-lens or generic-optics with 'field' instead." #-}

instance Lude.FromJSON AdvancedFieldSelector where
  parseJSON =
    Lude.withObject
      "AdvancedFieldSelector"
      ( \x ->
          AdvancedFieldSelector'
            Lude.<$> (x Lude..:? "EndsWith")
            Lude.<*> (x Lude..:? "NotStartsWith")
            Lude.<*> (x Lude..:? "Equals")
            Lude.<*> (x Lude..:? "NotEquals")
            Lude.<*> (x Lude..:? "NotEndsWith")
            Lude.<*> (x Lude..:? "StartsWith")
            Lude.<*> (x Lude..: "Field")
      )

instance Lude.ToJSON AdvancedFieldSelector where
  toJSON AdvancedFieldSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EndsWith" Lude..=) Lude.<$> endsWith,
            ("NotStartsWith" Lude..=) Lude.<$> notStartsWith,
            ("Equals" Lude..=) Lude.<$> equals,
            ("NotEquals" Lude..=) Lude.<$> notEquals,
            ("NotEndsWith" Lude..=) Lude.<$> notEndsWith,
            ("StartsWith" Lude..=) Lude.<$> startsWith,
            Lude.Just ("Field" Lude..= field)
          ]
      )
