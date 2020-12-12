{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.Attribute
  ( Attribute (..),

    -- * Smart constructor
    mkAttribute,

    -- * Lenses
    aAlternateValueEncoding,
    aAlternateNameEncoding,
    aName,
    aValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- |
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { alternateValueEncoding ::
      Lude.Maybe Lude.Text,
    alternateNameEncoding :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    value :: Lude.Text
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
-- * 'alternateNameEncoding' -
-- * 'alternateValueEncoding' -
-- * 'name' - The name of the attribute.
-- * 'value' - The value of the attribute.
mkAttribute ::
  -- | 'name'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  Attribute
mkAttribute pName_ pValue_ =
  Attribute'
    { alternateValueEncoding = Lude.Nothing,
      alternateNameEncoding = Lude.Nothing,
      name = pName_,
      value = pValue_
    }

-- |
--
-- /Note:/ Consider using 'alternateValueEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlternateValueEncoding :: Lens.Lens' Attribute (Lude.Maybe Lude.Text)
aAlternateValueEncoding = Lens.lens (alternateValueEncoding :: Attribute -> Lude.Maybe Lude.Text) (\s a -> s {alternateValueEncoding = a} :: Attribute)
{-# DEPRECATED aAlternateValueEncoding "Use generic-lens or generic-optics with 'alternateValueEncoding' instead." #-}

-- |
--
-- /Note:/ Consider using 'alternateNameEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlternateNameEncoding :: Lens.Lens' Attribute (Lude.Maybe Lude.Text)
aAlternateNameEncoding = Lens.lens (alternateNameEncoding :: Attribute -> Lude.Maybe Lude.Text) (\s a -> s {alternateNameEncoding = a} :: Attribute)
{-# DEPRECATED aAlternateNameEncoding "Use generic-lens or generic-optics with 'alternateNameEncoding' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Attribute Lude.Text
aName = Lens.lens (name :: Attribute -> Lude.Text) (\s a -> s {name = a} :: Attribute)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute Lude.Text
aValue = Lens.lens (value :: Attribute -> Lude.Text) (\s a -> s {value = a} :: Attribute)
{-# DEPRECATED aValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML Attribute where
  parseXML x =
    Attribute'
      Lude.<$> (x Lude..@? "AlternateValueEncoding")
      Lude.<*> (x Lude..@? "AlternateNameEncoding")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "Value")

instance Lude.ToQuery Attribute where
  toQuery Attribute' {..} =
    Lude.mconcat
      [ "AlternateValueEncoding" Lude.=: alternateValueEncoding,
        "AlternateNameEncoding" Lude.=: alternateNameEncoding,
        "Name" Lude.=: name,
        "Value" Lude.=: value
      ]
