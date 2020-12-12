{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AccountAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AccountAttributeValue
  ( AccountAttributeValue (..),

    -- * Smart constructor
    mkAccountAttributeValue,

    -- * Lenses
    aavAttributeValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a value of an account attribute.
--
-- /See:/ 'mkAccountAttributeValue' smart constructor.
newtype AccountAttributeValue = AccountAttributeValue'
  { attributeValue ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountAttributeValue' with the minimum fields required to make a request.
--
-- * 'attributeValue' - The value of the attribute.
mkAccountAttributeValue ::
  AccountAttributeValue
mkAccountAttributeValue =
  AccountAttributeValue' {attributeValue = Lude.Nothing}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aavAttributeValue :: Lens.Lens' AccountAttributeValue (Lude.Maybe Lude.Text)
aavAttributeValue = Lens.lens (attributeValue :: AccountAttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {attributeValue = a} :: AccountAttributeValue)
{-# DEPRECATED aavAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

instance Lude.FromXML AccountAttributeValue where
  parseXML x =
    AccountAttributeValue' Lude.<$> (x Lude..@? "attributeValue")
