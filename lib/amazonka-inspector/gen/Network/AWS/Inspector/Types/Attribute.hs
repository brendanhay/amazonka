-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Attribute
  ( Attribute (..),

    -- * Smart constructor
    mkAttribute,

    -- * Lenses
    aValue,
    aKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used as a request parameter in the 'AddAttributesToFindings' and 'CreateAssessmentTemplate' actions.
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { value :: Lude.Maybe Lude.Text,
    key :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- * 'key' - The attribute key.
-- * 'value' - The value assigned to the attribute key.
mkAttribute ::
  -- | 'key'
  Lude.Text ->
  Attribute
mkAttribute pKey_ = Attribute' {value = Lude.Nothing, key = pKey_}

-- | The value assigned to the attribute key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute (Lude.Maybe Lude.Text)
aValue = Lens.lens (value :: Attribute -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Attribute)
{-# DEPRECATED aValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The attribute key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aKey :: Lens.Lens' Attribute Lude.Text
aKey = Lens.lens (key :: Attribute -> Lude.Text) (\s a -> s {key = a} :: Attribute)
{-# DEPRECATED aKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON Attribute where
  parseJSON =
    Lude.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..: "key")
      )

instance Lude.ToJSON Attribute where
  toJSON Attribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [("value" Lude..=) Lude.<$> value, Lude.Just ("key" Lude..= key)]
      )
