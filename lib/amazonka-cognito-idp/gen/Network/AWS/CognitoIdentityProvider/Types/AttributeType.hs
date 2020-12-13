{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AttributeType
  ( AttributeType (..),

    -- * Smart constructor
    mkAttributeType,

    -- * Lenses
    atValue,
    atName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies whether the attribute is standard or custom.
--
-- /See:/ 'mkAttributeType' smart constructor.
data AttributeType = AttributeType'
  { -- | The value of the attribute.
    value :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The name of the attribute.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributeType' with the minimum fields required to make a request.
--
-- * 'value' - The value of the attribute.
-- * 'name' - The name of the attribute.
mkAttributeType ::
  -- | 'name'
  Lude.Text ->
  AttributeType
mkAttributeType pName_ =
  AttributeType' {value = Lude.Nothing, name = pName_}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atValue :: Lens.Lens' AttributeType (Lude.Maybe (Lude.Sensitive Lude.Text))
atValue = Lens.lens (value :: AttributeType -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {value = a} :: AttributeType)
{-# DEPRECATED atValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atName :: Lens.Lens' AttributeType Lude.Text
atName = Lens.lens (name :: AttributeType -> Lude.Text) (\s a -> s {name = a} :: AttributeType)
{-# DEPRECATED atName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AttributeType where
  parseJSON =
    Lude.withObject
      "AttributeType"
      ( \x ->
          AttributeType'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON AttributeType where
  toJSON AttributeType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Value" Lude..=) Lude.<$> value,
            Lude.Just ("Name" Lude..= name)
          ]
      )
