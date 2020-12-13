{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
  ( StringAttributeConstraintsType (..),

    -- * Smart constructor
    mkStringAttributeConstraintsType,

    -- * Lenses
    sactMaxLength,
    sactMinLength,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The constraints associated with a string attribute.
--
-- /See:/ 'mkStringAttributeConstraintsType' smart constructor.
data StringAttributeConstraintsType = StringAttributeConstraintsType'
  { -- | The maximum length.
    maxLength :: Lude.Maybe Lude.Text,
    -- | The minimum length.
    minLength :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StringAttributeConstraintsType' with the minimum fields required to make a request.
--
-- * 'maxLength' - The maximum length.
-- * 'minLength' - The minimum length.
mkStringAttributeConstraintsType ::
  StringAttributeConstraintsType
mkStringAttributeConstraintsType =
  StringAttributeConstraintsType'
    { maxLength = Lude.Nothing,
      minLength = Lude.Nothing
    }

-- | The maximum length.
--
-- /Note:/ Consider using 'maxLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sactMaxLength :: Lens.Lens' StringAttributeConstraintsType (Lude.Maybe Lude.Text)
sactMaxLength = Lens.lens (maxLength :: StringAttributeConstraintsType -> Lude.Maybe Lude.Text) (\s a -> s {maxLength = a} :: StringAttributeConstraintsType)
{-# DEPRECATED sactMaxLength "Use generic-lens or generic-optics with 'maxLength' instead." #-}

-- | The minimum length.
--
-- /Note:/ Consider using 'minLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sactMinLength :: Lens.Lens' StringAttributeConstraintsType (Lude.Maybe Lude.Text)
sactMinLength = Lens.lens (minLength :: StringAttributeConstraintsType -> Lude.Maybe Lude.Text) (\s a -> s {minLength = a} :: StringAttributeConstraintsType)
{-# DEPRECATED sactMinLength "Use generic-lens or generic-optics with 'minLength' instead." #-}

instance Lude.FromJSON StringAttributeConstraintsType where
  parseJSON =
    Lude.withObject
      "StringAttributeConstraintsType"
      ( \x ->
          StringAttributeConstraintsType'
            Lude.<$> (x Lude..:? "MaxLength") Lude.<*> (x Lude..:? "MinLength")
      )

instance Lude.ToJSON StringAttributeConstraintsType where
  toJSON StringAttributeConstraintsType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxLength" Lude..=) Lude.<$> maxLength,
            ("MinLength" Lude..=) Lude.<$> minLength
          ]
      )
