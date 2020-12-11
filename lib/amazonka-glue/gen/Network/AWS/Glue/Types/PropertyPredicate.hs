-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PropertyPredicate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PropertyPredicate
  ( PropertyPredicate (..),

    -- * Smart constructor
    mkPropertyPredicate,

    -- * Lenses
    ppValue,
    ppKey,
    ppComparator,
  )
where

import Network.AWS.Glue.Types.Comparator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines a property predicate.
--
-- /See:/ 'mkPropertyPredicate' smart constructor.
data PropertyPredicate = PropertyPredicate'
  { value ::
      Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text,
    comparator :: Lude.Maybe Comparator
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PropertyPredicate' with the minimum fields required to make a request.
--
-- * 'comparator' - The comparator used to compare this property to others.
-- * 'key' - The key of the property.
-- * 'value' - The value of the property.
mkPropertyPredicate ::
  PropertyPredicate
mkPropertyPredicate =
  PropertyPredicate'
    { value = Lude.Nothing,
      key = Lude.Nothing,
      comparator = Lude.Nothing
    }

-- | The value of the property.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppValue :: Lens.Lens' PropertyPredicate (Lude.Maybe Lude.Text)
ppValue = Lens.lens (value :: PropertyPredicate -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: PropertyPredicate)
{-# DEPRECATED ppValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The key of the property.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppKey :: Lens.Lens' PropertyPredicate (Lude.Maybe Lude.Text)
ppKey = Lens.lens (key :: PropertyPredicate -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: PropertyPredicate)
{-# DEPRECATED ppKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The comparator used to compare this property to others.
--
-- /Note:/ Consider using 'comparator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppComparator :: Lens.Lens' PropertyPredicate (Lude.Maybe Comparator)
ppComparator = Lens.lens (comparator :: PropertyPredicate -> Lude.Maybe Comparator) (\s a -> s {comparator = a} :: PropertyPredicate)
{-# DEPRECATED ppComparator "Use generic-lens or generic-optics with 'comparator' instead." #-}

instance Lude.ToJSON PropertyPredicate where
  toJSON PropertyPredicate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Value" Lude..=) Lude.<$> value,
            ("Key" Lude..=) Lude.<$> key,
            ("Comparator" Lude..=) Lude.<$> comparator
          ]
      )
