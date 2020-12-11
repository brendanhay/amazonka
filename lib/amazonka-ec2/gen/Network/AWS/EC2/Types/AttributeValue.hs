-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AttributeValue
  ( AttributeValue (..),

    -- * Smart constructor
    mkAttributeValue,

    -- * Lenses
    avValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a value for a resource attribute that is a String.
--
-- /See:/ 'mkAttributeValue' smart constructor.
newtype AttributeValue = AttributeValue'
  { value ::
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

-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- * 'value' - The attribute value. The value is case-sensitive.
mkAttributeValue ::
  AttributeValue
mkAttributeValue = AttributeValue' {value = Lude.Nothing}

-- | The attribute value. The value is case-sensitive.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avValue :: Lens.Lens' AttributeValue (Lude.Maybe Lude.Text)
avValue = Lens.lens (value :: AttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: AttributeValue)
{-# DEPRECATED avValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML AttributeValue where
  parseXML x = AttributeValue' Lude.<$> (x Lude..@? "value")

instance Lude.ToQuery AttributeValue where
  toQuery AttributeValue' {..} = Lude.mconcat ["Value" Lude.=: value]
