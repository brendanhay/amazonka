-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AttributeValueTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AttributeValueTarget
  ( AttributeValueTarget (..),

    -- * Smart constructor
    mkAttributeValueTarget,

    -- * Lenses
    avtAttributeValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes an attribute value.
--
-- /See:/ 'mkAttributeValueTarget' smart constructor.
newtype AttributeValueTarget = AttributeValueTarget'
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

-- | Creates a value of 'AttributeValueTarget' with the minimum fields required to make a request.
--
-- * 'attributeValue' - The value of the attribute.
mkAttributeValueTarget ::
  AttributeValueTarget
mkAttributeValueTarget =
  AttributeValueTarget' {attributeValue = Lude.Nothing}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avtAttributeValue :: Lens.Lens' AttributeValueTarget (Lude.Maybe Lude.Text)
avtAttributeValue = Lens.lens (attributeValue :: AttributeValueTarget -> Lude.Maybe Lude.Text) (\s a -> s {attributeValue = a} :: AttributeValueTarget)
{-# DEPRECATED avtAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

instance Lude.FromXML AttributeValueTarget where
  parseXML x =
    AttributeValueTarget' Lude.<$> (x Lude..@? "AttributeValue")
