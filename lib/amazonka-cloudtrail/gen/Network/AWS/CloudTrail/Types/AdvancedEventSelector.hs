-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.AdvancedEventSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.AdvancedEventSelector
  ( AdvancedEventSelector (..),

    -- * Smart constructor
    mkAdvancedEventSelector,

    -- * Lenses
    aesName,
    aesFieldSelectors,
  )
where

import Network.AWS.CloudTrail.Types.AdvancedFieldSelector
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkAdvancedEventSelector' smart constructor.
data AdvancedEventSelector = AdvancedEventSelector'
  { name ::
      Lude.Text,
    fieldSelectors ::
      Lude.NonEmpty AdvancedFieldSelector
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdvancedEventSelector' with the minimum fields required to make a request.
--
-- * 'fieldSelectors' - Undocumented field.
-- * 'name' - Undocumented field.
mkAdvancedEventSelector ::
  -- | 'name'
  Lude.Text ->
  -- | 'fieldSelectors'
  Lude.NonEmpty AdvancedFieldSelector ->
  AdvancedEventSelector
mkAdvancedEventSelector pName_ pFieldSelectors_ =
  AdvancedEventSelector'
    { name = pName_,
      fieldSelectors = pFieldSelectors_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aesName :: Lens.Lens' AdvancedEventSelector Lude.Text
aesName = Lens.lens (name :: AdvancedEventSelector -> Lude.Text) (\s a -> s {name = a} :: AdvancedEventSelector)
{-# DEPRECATED aesName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fieldSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aesFieldSelectors :: Lens.Lens' AdvancedEventSelector (Lude.NonEmpty AdvancedFieldSelector)
aesFieldSelectors = Lens.lens (fieldSelectors :: AdvancedEventSelector -> Lude.NonEmpty AdvancedFieldSelector) (\s a -> s {fieldSelectors = a} :: AdvancedEventSelector)
{-# DEPRECATED aesFieldSelectors "Use generic-lens or generic-optics with 'fieldSelectors' instead." #-}

instance Lude.FromJSON AdvancedEventSelector where
  parseJSON =
    Lude.withObject
      "AdvancedEventSelector"
      ( \x ->
          AdvancedEventSelector'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..: "FieldSelectors")
      )

instance Lude.ToJSON AdvancedEventSelector where
  toJSON AdvancedEventSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("FieldSelectors" Lude..= fieldSelectors)
          ]
      )
