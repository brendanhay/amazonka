-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AttributeBooleanValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AttributeBooleanValue
  ( AttributeBooleanValue (..),

    -- * Smart constructor
    mkAttributeBooleanValue,

    -- * Lenses
    abvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a value for a resource attribute that is a Boolean value.
--
-- /See:/ 'mkAttributeBooleanValue' smart constructor.
newtype AttributeBooleanValue = AttributeBooleanValue'
  { value ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributeBooleanValue' with the minimum fields required to make a request.
--
-- * 'value' - The attribute value. The valid values are @true@ or @false@ .
mkAttributeBooleanValue ::
  AttributeBooleanValue
mkAttributeBooleanValue =
  AttributeBooleanValue' {value = Lude.Nothing}

-- | The attribute value. The valid values are @true@ or @false@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abvValue :: Lens.Lens' AttributeBooleanValue (Lude.Maybe Lude.Bool)
abvValue = Lens.lens (value :: AttributeBooleanValue -> Lude.Maybe Lude.Bool) (\s a -> s {value = a} :: AttributeBooleanValue)
{-# DEPRECATED abvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML AttributeBooleanValue where
  parseXML x = AttributeBooleanValue' Lude.<$> (x Lude..@? "value")

instance Lude.ToQuery AttributeBooleanValue where
  toQuery AttributeBooleanValue' {..} =
    Lude.mconcat ["Value" Lude.=: value]
